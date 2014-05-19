/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <elf/elf.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <barrelfish_kpi/shared_mem_arch.h>
#include <notify_ipi.h>

struct xcore_bind_handler {
    coreid_t                    coreid;
    enum cpu_type               cputype;
    struct monitor_binding      *binding;
};

errval_t spawn_xcore_monitor(coreid_t id, int hwid, enum cpu_type cpu_type,
                             const char *cmdline /* XXX: currently ignored */,
                             struct intermon_binding **ret_binding)
{
    errval_t err;

    switch(cpu_type) {
    case CPU_SCC:
        break;

    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    // Setup new inter-monitor connection
    struct intermon_ump_ipi_binding *binding = malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(binding != NULL);

    // compute size of frame needed and allocate it
    struct capref frame;
    size_t framesize = MON_URPC_CHANNEL_LEN * 2;
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * my_core_id),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (my_core_id + 1)));
    err = frame_alloc(&frame, framesize, &framesize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    ram_set_affinity(0, 0);

    // Mark it remote
    bool has_descendants;
    err = monitor_cap_remote(frame, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame_attr(&buf, framesize, frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    memset(buf, 0, framesize);

    // Bootee's notify channel ID is always 0
    struct capref notify_cap;
    err = notification_create_cap(0, hwid, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // init our end of the binding and channel
    err = intermon_ump_ipi_init(binding, get_default_waitset(),
                                buf, MON_URPC_CHANNEL_LEN,
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_UMP_CHAN_BIND);
    }

    *ret_binding = &binding->b;

    // Identify UMP frame for tracing
    struct frame_identity umpid;
    err = invoke_frame_identify(frame, &umpid);
    assert(err_is_ok(err));
    binding->ump_state.chan.recvid = (uintptr_t)umpid.base;
    binding->ump_state.chan.sendid =
        (uintptr_t)(umpid.base + MON_URPC_CHANNEL_LEN);

    /* Look up information on the urpc_frame cap */
    struct frame_identity urpc_frame_id  = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &urpc_frame_id);
    assert(err_is_ok(err));

    /* Invoke kernel capability to boot new core */
    err = invoke_monitor_spawn_scc_core(hwid, urpc_frame_id.base,
                                        urpc_frame_id.bits, chanid);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

    return SYS_ERR_OK;
}

errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding)
{
    errval_t err;
    int argn = 1;

    assert(argc == 5);

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[argn++], NULL, 10);

    // core_id of the core that booted this core
    coreid_t core_id = strtol(argv[argn++], NULL, 10);
    *ret_parent_coreid = core_id;

    // other monitor's channel id
    assert(strncmp("chanid", argv[argn], strlen("chanid")) == 0);
    int chan_id = strtol(strchr(argv[argn++], '=') + 1, NULL, 10);

    // other monitor's frame base
    assert(strncmp("frame", argv[argn], strlen("frame")) == 0);
    uint64_t chanbase = strtoul(strchr(argv[argn++], '=') + 1, NULL, 10);

    err = monitor_client_setup_mem_serv();
    assert(err_is_ok(err));

    /* Wait for mem_serv to advertise its iref to us */
    while (mem_serv_iref == 0) {
        messages_wait_and_handle_next();
    }

    /* Can now connect to and use mem_serv */
    err = ram_alloc_set(NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    printf("frame base at 0x%llx -- 0x%llx\n", chanbase, chanbase + BASE_PAGE_SIZE);

    assert(MON_URPC_CHANNEL_LEN * 2 <= BASE_PAGE_SIZE);
    ram_set_affinity(chanbase, chanbase + BASE_PAGE_SIZE);
    struct capref frame;
    err = frame_alloc(&frame, MON_URPC_CHANNEL_LEN * 2, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_alloc failed");
        return err; // FIXME: cleanup
    }
    ram_set_affinity(0, 0);     // Reset affinity

    struct frame_identity frameid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &frameid);
    assert(err == SYS_ERR_OK);

    printf("URPC physical frame at 0x%llx\n", frameid.base);

    // Map frame locally
    // XXX: Remove this and use URPC_BASE when spawning from other core
    void *buf;
    err = vspace_map_one_frame_attr(&buf, MON_URPC_CHANNEL_LEN * 2, frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
                                    NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Create notify cap to other monitor
    struct capref notify_cap;
    err = notification_create_cap(chan_id, core_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    assert(chanid == 0);        // Make sure it's channel 0
    err = notification_create_cap(chanid, my_core_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // setup our side of the binding
    struct intermon_ump_ipi_binding *rckb;
    rckb = malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(rckb != NULL);
    err = intermon_ump_ipi_init(rckb, get_default_waitset(),
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN,
                                buf, MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_UMP_CHAN_BIND);
        return err;
    }

    // Identify UMP frame for tracing
    rckb->ump_state.chan.sendid = (uintptr_t)frameid.base;
    rckb->ump_state.chan.recvid =
        (uintptr_t)(frameid.base + MON_URPC_CHANNEL_LEN);

    *ret_binding = &rckb->b;

    return SYS_ERR_OK;
}
