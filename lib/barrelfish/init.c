/**
 * \file
 * \brief Barrelfish library initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/idc.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish/terminal.h>
#include <barrelfish/morecore.h>
#include <barrelfish/monitor_client.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish_kpi/domain_params.h>
#include <if/monitor_defs.h>
#include <trace/trace.h>
#include <octopus/init.h>
#include "threads_priv.h"
#include "init.h"

/// Are we the init domain (and thus need to take some special paths)?
static bool init_domain;

extern size_t (*_libc_terminal_read_func)(char *, size_t);
extern size_t (*_libc_terminal_write_func)(const char *, size_t);
extern void (*_libc_exit_func)(int);
extern void (*_libc_assert_func)(const char *, const char *, const char *, int);

void libc_exit(int);

void libc_exit(int status)
{
    errval_t err;

    if (!init_domain) {
        terminal_exit();
    }

    // Use spawnd if spawned through spawnd
    if(disp_get_domain_id() == 0) {
        errval_t err = cap_revoke(cap_dispatcher);
        if (err_is_fail(err)) {
	    sys_print("revoking dispatcher failed in _Exit, spinning!", 100);
	    while (1) {}
        }
        err = cap_delete(cap_dispatcher);
        sys_print("deleting dispatcher failed in _Exit, spinning!", 100);

        // XXX: Leak all other domain allocations
    } else {
        err = spawn_exit(status);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "spawn_exit");
        }
    }

    // If we're not dead by now, we wait
    while (1) {}
}

static void libc_assert(const char *expression, const char *file,
                        const char *function, int line)
{
    char buf[512];
    size_t len;

    /* Formatting as per suggestion in C99 spec 7.2.1.1 */
    len = snprintf(buf, sizeof(buf), "Assertion failed on core %d in %.*s: %s,"
                   " function %s, file %s, line %d.\n",
                   disp_get_core_id(), DISP_NAME_LEN,
                   disp_name(), expression, function, file, line);
    sys_print(buf, len < sizeof(buf) ? len : sizeof(buf));
}

/* Set libc function pointers */
void barrelfish_libc_glue_init(void)
{
    _libc_terminal_read_func = terminal_read;
    _libc_terminal_write_func = terminal_write;
    _libc_exit_func = libc_exit;
    _libc_assert_func = libc_assert;
    /* morecore func is setup by morecore_init() */

    // XXX: set a static buffer for stdout
    // this avoids an implicit call to malloc() on the first printf
    static char buf[BUFSIZ];
    setvbuf(stdout, buf, _IOLBF, sizeof(buf));
}

static void monitor_bind_cont(void *st, errval_t err, struct monitor_binding *b);

#ifdef CONFIG_TRACE
errval_t trace_my_setup(void)
{
#ifndef TRACING_EXISTS
    return SYS_ERR_OK;
#else
    errval_t err;

    struct capref cap = {
        .cnode  = cnode_task,
        .slot   = TASKCN_SLOT_TRACEBUF
    };

    if (disp_get_core_id() >= TRACE_COREID_LIMIT) {
        // can't support tracing on this core. sorry :(
        return SYS_ERR_OK;
    }

    err = vspace_map_one_frame((void**)&trace_buffer_master, TRACE_ALLOC_SIZE,
                               cap, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }

    trace_buffer_va = trace_buffer_master +
        (disp_get_core_id() * TRACE_PERCORE_BUF_SIZE);

    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    // Update pointer to trace buffer in child's dispatcher
    disp->trace_buf = (struct trace_buffer *)trace_buffer_va;

    return SYS_ERR_OK;
#endif
}
#endif

static bool request_done = false;

/** \brief Initialise libbarrelfish.
 *
 * This runs on a thread in every domain, after the dispatcher is setup but
 * before main() runs.
 */
errval_t barrelfish_init_onthread(struct spawn_domain_params *params)
{
    errval_t err;

    // do we have an environment?
    if (params != NULL && params->envp[0] != NULL) {
        extern char **environ;
        environ = params->envp;
    }

    // Init default waitset for this dispatcher
    struct waitset *default_ws = get_default_waitset();
    waitset_init(default_ws);

    // Initialize ram_alloc state
    ram_alloc_init();
    /* All domains use smallcn to initialize */
    err = ram_alloc_set(ram_alloc_fixed);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    err = vspace_current_init(init_domain);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_INIT);
    }

    err = slot_alloc_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    // reconstruct our pmap from data passed to us by our parent
    if (params != NULL && params->vspace_buf != NULL) {
        struct pmap *pmap = get_current_pmap();
        err = pmap->f.deserialise(pmap, params->vspace_buf,
                                  params->vspace_buf_len);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VSPACE_INIT);
        }
    } else if (init_domain) {
        // TODO: the kernel boots us with a deterministic pmap structure: use it
    }

    err = morecore_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MORECORE_INIT);
    }

    lmp_endpoint_init();

    // init domains only get partial init
    if (init_domain) {
        return SYS_ERR_OK;
    }

    /* bind to monitor */
    struct monitor_lmp_binding *mcb =
        malloc(sizeof(struct monitor_lmp_binding));
    assert(mcb != NULL);
    set_monitor_binding(&mcb->b);

    errval_t init_complete_err;

    request_done = false;
    err = monitor_client_lmp_bind(mcb, monitor_bind_cont, &init_complete_err,
                                  default_ws, DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MONITOR_CLIENT_BIND);
    }

    // dispatch events on the waitset until monitor binding completes
    while (!request_done) {
        err = event_dispatch(default_ws);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_EVENT_DISPATCH);
        }
    }

    if(err_is_fail(init_complete_err)) {
        USER_PANIC_ERR(err, "during initialization");
    }

    idc_init();

    /* Bind with monitor's blocking rpc channel */
    err = monitor_client_blocking_rpc_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MONITOR_RPC_BIND);
    }

    /* XXX: Setup the channel with mem_serv and use the channel instead */
    err = ram_alloc_set(NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

#ifdef CONFIG_TRACE
    err = trace_my_setup();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "trace_my_setup failed");
        return err;
    }
#endif

    // try to connect to name service (may fail if we are the skb or ramfsd!)
    err = nameservice_client_blocking_bind();
    if (err_is_fail(err)) {
        if (err_no(err) == LIB_ERR_GET_NAME_IREF) {
            // skip everything else if we don't have a nameservice
            return SYS_ERR_OK;
        } else {
            return err_push(err, LIB_ERR_NAMESERVICE_CLIENT_INIT);
        }
    }

    // init terminal
    err = terminal_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_TERMINAL_INIT);
    }

    // Init domain spanning code
    err = domain_init();
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_DOMAIN_INIT);
    }

    // XXX: Record text/data mappings from environment
    char *p = getenv("ARRAKIS_PMAP");
    if(p != NULL) {
        struct morecore_state *mcstate = get_morecore_state();
        for(mcstate->v2p_entries = 0; *p != '\0'; mcstate->v2p_entries++) {
            assert(mcstate->v2p_entries < MAX_V2P_MAPPINGS);
            struct v2pmap *e = &mcstate->v2p_mappings[mcstate->v2p_entries];
            int r = sscanf(p, "%" PRIxGENVADDR ":%" PRIxGENPADDR ":%zx ", &e->va, &e->pa, &e->size);
            assert(r == 3);
            p = strchr(p, ' ') + 1;
        }
    }

    return err;
}

static void monitor_bind_cont(void *st, errval_t err, struct monitor_binding *b)
{
    // hacky errval_t state pointer used to signal completion
    errval_t *init_complete_err = st;

    assert(!init_domain);
    *init_complete_err = err;

    // signal completion
    request_done = true;
}

/**
 *  \brief Initialise libbarrelfish, while disabled.
 *
 * This runs on the dispatcher's stack, while disabled, before the dispatcher is
 * setup. We can't call anything that needs to be enabled (ie. cap invocations)
 * or uses threads. This is called from crt0.
 */
void barrelfish_init_disabled(dispatcher_handle_t handle, bool init_dom_arg);
void barrelfish_init_disabled(dispatcher_handle_t handle, bool init_dom_arg)
{
    init_domain = init_dom_arg;
    disp_init_disabled(handle);
    thread_init_disabled(handle, init_dom_arg);
}
