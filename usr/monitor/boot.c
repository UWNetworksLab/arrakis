/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <barrelfish_kpi/cpu.h> // for cpu_type_to_archstr()

/* Use to figure out when all monitors initialized. */
int seen_connections = 0;
int num_monitors = 1;

/**
 * \brief Based on number of monitors in the system,
 * returns number of connections created.
 */
static int get_num_connections(int num)
{
    if (num == 1 || num == 2) {
        return 0;
    }
    if (num == 3) {
        return 1;
    }

    return (num - 2) + get_num_connections(num - 1);
}

/**
 * \brief Msg handler for booting a given core
 *
 * \param id     id of the core to boot
 * \param hwid   hardware specific id of the core to boot
 * \param cpu_type  Type of cpu to boot
 * \param cmdline command-line args for kernel
 *
 * \bug Verify that cpu_type matches the elf image
 */
void boot_core_request(struct monitor_binding *b, coreid_t id, int32_t hwid,
                       int32_t int_cpu_type, char *cmdline)
{
    errval_t err;
    enum cpu_type cpu_type = (enum cpu_type)int_cpu_type;
    struct intermon_binding *new_binding = NULL;

    if (id == my_core_id) {
        err = MON_ERR_SAME_CORE;
        goto out;
    }

    if (cpu_type >= CPU_TYPE_NUM) {
        err = SPAWN_ERR_UNKNOWN_TARGET_ARCH;
        goto out;
    }

    printf("Monitor %d: booting %s core %d as '%s'\n", my_core_id,
           cpu_type_to_archstr(cpu_type), id, cmdline);

    /* Assure memory server and chips have initialized */
    assert(mem_serv_iref != 0);
    assert(ramfs_serv_iref != 0);
    assert(name_serv_iref != 0);
    assert(monitor_mem_iref != 0);

    err = spawn_xcore_monitor(id, hwid, cpu_type, cmdline, &new_binding);
    if(err_is_fail(err)) {
        err = err_push(err, MON_ERR_SPAWN_XCORE_MONITOR);
        goto out;
    }

    // setup new binding
    assert(new_binding != NULL);
    intermon_init(new_binding, id);

    // store client that requested the boot, so we can tell them when it completes
    struct intermon_state *st = new_binding->st;
    st->originating_client = b;

 out:
    free(cmdline);

    if (err_is_ok(err)) {
        num_monitors++;
    } else {
        errval_t err2 = b->tx_vtbl.boot_core_reply(b, NOP_CONT, err);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "sending boot_core_reply failed");
        }
    }
}

/**
 * \brief XXX: This is a hack. Currently, we must know when all cores
 * are booted so that the monitors can initialize with each other,
 * setup routing tables and synchronize clocks.
 */
void boot_initialize_request(struct monitor_binding *st)
{
    errval_t err;

    /* Wait for all monitors to initialize. */
    int num_connections = get_num_connections(num_monitors);
    while(num_connections > seen_connections) {
        // This waiting is fine, boot_manager will not send another msg
        // till it gets a reply from this.
        messages_wait_and_handle_next();
    }

    printf("all %d monitors up\n", num_monitors);

#ifndef __scc__
    if(num_monitors > 1) {
        printf("monitor: synchronizing clocks\n");
        err = timing_sync_timer();
        assert(err_is_ok(err) || err_no(err) == SYS_ERR_SYNC_MISS);
        if(err_no(err) == SYS_ERR_SYNC_MISS) {
            printf("monitor: failed to sync clocks. Bad reference clock?\n");
        }
    }
#endif

    err = st->tx_vtbl.boot_initialize_reply(st, NOP_CONT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "boot_initialize_reply failed");
    }
}
