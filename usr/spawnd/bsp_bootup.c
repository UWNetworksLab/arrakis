/**
 * \file
 * \brief Bootstrap logic for spawn daemon.
 *
 * \bug The monitor explicitly requires an indication
 * when it will not be asked to boot more cores.
 * So that it can setup its routing tables, etc.
 * To accomodate the bug, the domain waits for pci to finish iterating
 * the ACPI tables before informing the monitors of which cores to boot.
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/cpu_arch.h>

#include <spawndomain/spawndomain.h>

#include <vfs/vfs.h>
#include <skb/skb.h>

#include <dist/barrier.h>

#include <if/monitor_defs.h>


#include "internal.h"


#define MIN(a,b) ((a) < (b) ? (a) : (b))

struct coreid_mapping {
    int arch_id;
    bool present;
};

static struct coreid_mapping coreid_mappings[MAX_COREID + 1];
static coreid_t coreid_offset;

static const char *kernel_cmdline;

static void state_machine(void);

static void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "Msg err in boot_core_reply");
        printf("msgerr in boot_core_reply, exiting\n");
        abort();
    }

    state_machine();
}

/**
 * \brief Msg sent by monitor when it has booted all requested cores
 * and initialized
 */
static void boot_initialize_reply(struct monitor_binding *st)
{
    state_machine();
}

static errval_t get_kernel_cmdline(const char *bootmods)
{
    // find something in the kernel string
    char *cmdline_pos = strstr(bootmods, "/sbin/cpu");
    if (cmdline_pos == NULL) {
        DEBUG_ERR(SPAWN_ERR_GET_CMDLINE_ARGS, "didn't find kernel command line");
        return SPAWN_ERR_GET_CMDLINE_ARGS;
    }

    // find the line containing the something
    const char *start = bootmods, *end = NULL;
    do {
        if (end != NULL) {
            start = end + 1;
        }
        end = strchr(start, '\n');
        if (end == NULL) {
            return SPAWN_ERR_GET_CMDLINE_ARGS;
        }
    } while (end < cmdline_pos);

    // copy it out
    char *buf = malloc(end - start + 1);
    if (buf == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    memcpy(buf, start, end - start);
    buf[end - start] = '\0';
    kernel_cmdline = buf;
    return SYS_ERR_OK;
}

static void state_machine(void)
{
    errval_t err;
    static int state_id = 0;
    struct monitor_binding *mb = get_monitor_binding();

    switch (state_id) {
    case 0: // Set reply handlers
        mb->rx_vtbl.boot_core_reply = boot_core_reply;
        mb->rx_vtbl.boot_initialize_reply = boot_initialize_reply;
        state_id++;

        // Fall through

    case 1: { // Boot all present cores
        static int coreid = 0;

        // find next core to boot
        while (coreid <= MAX_COREID
               && (coreid == my_core_id || !coreid_mappings[coreid].present)) {
            coreid++;
        }

        if (coreid <= MAX_COREID) {
            err = mb->tx_vtbl.boot_core_request(mb, NOP_CONT, coreid,
                                                coreid_mappings[coreid].arch_id,
                                                CURRENT_CPU_TYPE, kernel_cmdline);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending boot core request");
            }

            coreid++;
        } else { // Send boot_initialize_request message
            err = mb->tx_vtbl.boot_initialize_request(mb, NOP_CONT);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "sending boot initialize request");
            }

            state_id++;
        }

        break;
    }

    case 2: { // wait for all spawnd's to come up

        for (uintptr_t c = 0; c <= MAX_COREID; c++) {
            if (coreid_mappings[c].present && c != my_core_id) {
                err = nsb_wait_n(c, SERVICE_BASENAME);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "nameservice barrier wait for %s.%d",
                                   SERVICE_BASENAME, c);
                }                
            }
        }

        err = nsb_register(ALL_SPAWNDS_UP);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice barrier register for %s", 
                           ALL_SPAWNDS_UP);
        }                

        // offer the spawn service
        err = start_service();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "starting spawnd service");
        }
        break;
    }

    default:
        assert(!"Should not get here");
        printf("Should not get here, exiting\n");
        abort();
    }
}

/* -------------------------- MAIN ------------------------------- */

static void mappings_from_skb(uintptr_t my_arch_id, char *str)
{
    // insert mapping for self
    coreid_mappings[my_core_id].arch_id = my_arch_id;
    coreid_mappings[my_core_id].present = true;

    coreid_t n = 0;
    if (n == my_core_id) {
        n++;
    }

    while (*str != '\0') {
        if (!isdigit((int)*str)) {
            str++;
            continue;
        }
        uintptr_t arch_id = strtol(str, &str, 10);
        if (arch_id != my_arch_id) {
            coreid_mappings[n].arch_id = arch_id;
            coreid_mappings[n].present = true;
            if (++n == my_core_id) {
                n++;
            }
        }
    }
}

static void mappings_from_cmdline(uintptr_t my_arch_id, const char *str)
{
    // insert mapping for self
    coreid_mappings[my_core_id].arch_id = my_arch_id;
    coreid_mappings[my_core_id].present = true;

    coreid_t next = coreid_offset;
    if (next == my_core_id) {
        next++;
    }

    char *p = strchr(str, '=');
    assert(p != NULL);
    p++;
    while(*p != '\0') {
        int id_from = strtol(p, (char **)&p, 10), id_to = id_from;
        if(*p == '-') {
            p++;
            id_to = strtol(p, (char **)&p, 10);
        }
        assert(*p == ',' || *p == '\0');
        if(*p != '\0') {
            p++;
        }
        for(int i = id_from; i <= id_to; i++) {
            if (i != my_arch_id) {
                assert(next <= MAX_COREID);
                coreid_mappings[next].arch_id = i;
                coreid_mappings[next].present = true;
                if (++next == my_core_id) {
                    next++;
                }
            }
        }
    }
}

void bsp_bootup(const char *bootmodules, int argc, const char *argv[])
{
    const char *cmdline_mappings = NULL;
    uintptr_t my_arch_id = 0;
    errval_t err;

    // Find kernel cmdline
    err = get_kernel_cmdline(bootmodules);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "invalid kernel cmdline");
    }

    /* This is all a bit silly. We do not yet support heterogeneous systems so
       only one cmdline arg is expected, but in anticipation of that, we
       support passing different names for that argument. */
    for (int i = 1; i < argc; i++) {
        if(!strncmp(argv[i],"bootapic-x86_64=",strlen("bootapic-x86_64="))) {
            cmdline_mappings = argv[i];
        } else if(!strncmp(argv[i],"bootapic-x86_32=",strlen("bootapic-x86_32="))) {
            cmdline_mappings = argv[i];
        } else if(!strncmp(argv[i],"bootscc=",strlen("bootscc="))) {
            cmdline_mappings = argv[i];
        } else if(!strncmp(argv[i],"bootarm=",strlen("bootarm="))) {
        	cmdline_mappings = argv[i];
        // this is for compatibility with the armv5 ports which doesn't support multi-core yet
        } else if(!strcmp(argv[i],"bootarm")) {
        	cmdline_mappings = "dummy=";
        } else if (strncmp(argv[i], "coreid_offset=", sizeof("coreid_offset")) == 0) {
            coreid_offset = strtol(argv[i] + sizeof("coreid_offset"), NULL, 10);
        } else if (strncmp(argv[i], "apicid=", sizeof("apicid")) == 0) {
            my_arch_id = strtol(argv[i] + sizeof("apicid"), NULL, 10);
        } else if(!strcmp(argv[i],"boot")) {
            // ignored
        } else {
            debug_printf("Invalid arg (%s) or architecture not supported.\n",
                         argv[i]);
            abort();
        }
    }

    if (cmdline_mappings != NULL) {
        mappings_from_cmdline(my_arch_id, cmdline_mappings);
    } else {
        // Use SKB to boot all cores
        err = skb_client_connect();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "skb_client_connect failed");
        }

        // debug_printf("Waiting for pci to finish\n");
        /* Wait for pci to finish ACPI enumeration.
           This uses the nameserver as a lock server.
           When pci is done enumeration, it will add this to the server.
        */
        iref_t iref;
        err = nameservice_blocking_lookup("pci_discovery_done", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }

        /* Get the list of APIC IDs */
        char *result, *str_err;
        int32_t int_err;
        err = skb_evaluate("get_apic_id_list(L),write(L).",
                           &result, &str_err, &int_err);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "skb_evaluate failed");
        }
        mappings_from_skb(my_arch_id, result);
        free(result);
        free(str_err);

        /* Add the mappings to the skb */
        for (int i = 0; i <= MAX_COREID; i++) {
            if (!coreid_mappings[i].present) {
                continue;
            }

            err = skb_add_fact("corename(%d, x86_64, apic(%d)).",
                               i, coreid_mappings[i].arch_id);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to add core mapping to skb");
            }
        }

#ifdef BARRELFISH_MULTIHOP_CHAN_H
        // wait until routing tables are set up (for multi-hop routing)
        err = nameservice_blocking_lookup("rts_done", &iref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
        }
#endif // BARRELFISH_MULTIHOP_CHAN_H

    }

    for (int i = 0; i <= MAX_COREID; i++) {
        if (coreid_mappings[i].present) {
            debug_printf("coreid %d is arch id %d\n", i,
                         coreid_mappings[i].arch_id);
        }
    }

    state_machine();
}
