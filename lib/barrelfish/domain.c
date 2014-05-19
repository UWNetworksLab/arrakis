/**
 * \file
 * \brief Manage domain spanning cores
 *
 * \bug Need to specify how big the default thread on the spanned dispatcher
 * should be because we cannot dynamically grow our stacks
 *
 * \bug Can only do domain_new_dispatcher() when no other dispatchers have
 * threads (except for the internal interdisp-thread).
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/waitset_chan.h>
#include <arch/registers.h>
#include <barrelfish/dispatch.h>
#include <if/interdisp_defs.h>
#include "arch/threads.h"
#include "init.h"
#include <if/monitor_defs.h>
#include "threads_priv.h"
#include "waitset_chan_priv.h"

///< Struct to maintain per dispatcher domain library state
struct domain_state {
    iref_t iref;  ///< Iref for the interdisp service
    struct interdisp_binding *b[MAX_CPUS];
    struct waitset interdisp_ws;
    struct thread *default_waitset_handler;
    struct thread *remote_wakeup_queue;
    struct waitset_chanstate remote_wakeup_event;
    bool conditional;
};

///< Struct to send init information to the dispatcher that was spanned
struct remote_core_state {
    iref_t iref;                  ///< Iref of the interdisp service to connect to
    uint8_t core_id;              ///< Core id of the domain that spanned this dispatcher
    struct span_domain_state *span_domain_state; ///< Reference to the span_domain_state of the "server"
    bool initialized;             ///< true if remote core is fully initialized
    int cnt;                      ///< Used to count dispatcher connected
};

///< Struct for spanning domains state machine
struct span_domain_state {
    struct thread *thread;              ///< Thread to run on remote core
    uint8_t core_id;                    ///< Id of the remote core
    errval_t err;                       ///< To propagate error value
    domain_spanned_callback_t callback; ///< Callback for when domain has spanned
    void *callback_arg;                 ///< Optional argument to pass with callback
    struct capref frame;                ///< Dispatcher frame
    struct capref vroot;                ///< VRoot cap
    struct event_queue_node event_qnode;       ///< Event queue node
    struct waitset_chanstate initev;    ///< Dispatcher initialized event
    bool initialized;                   ///< True if remote initialized
};

///< Array of all interdisp IREFs in the domain
static iref_t allirefs[MAX_CPUS];

static void dispatcher_initialized_handler(void *arg)
{
    struct span_domain_state *span_domain_state = arg;
#if 0
    struct domain_state *domain_state = get_domain_state();

    // XXX: Tell currently active interdisp-threads to handle default waitset
    for(int i = 0; i < MAX_CPUS; i++) {
        struct interdisp_binding *b = domain_state->b[i];

        if(disp_get_core_id() != i &&
           span_domain_state->core_id != i && b != NULL) {
            errval_t err = b->tx_vtbl.span_slave_done(b, NOP_CONT);
            assert(err_is_ok(err));
        }
    }
#endif

    /* Upcall into the domain_new_dispatcher callback if registered */
    if (span_domain_state->callback) {
        span_domain_state->callback(span_domain_state->callback_arg, SYS_ERR_OK);
    }

    free(span_domain_state);
}

/**
 * \brief Handled for dispatcher_initialized msg type
 *
 * Called when a recently spanned dispatcher has initialized.
 * Store it's connection object, and upcall into the registered callback
 */
static void dispatcher_initialized(struct interdisp_binding *st, genvaddr_t id)
{
    struct span_domain_state *span_domain_state = (struct span_domain_state*)(uintptr_t)id;

    // Signal the default waitset of this event
    struct event_closure closure = {
        .handler = dispatcher_initialized_handler,
        .arg = span_domain_state,
    };
    waitset_chanstate_init(&span_domain_state->initev, CHANTYPE_EVENT_QUEUE);
    errval_t err = waitset_chan_trigger_closure(get_default_waitset(),
                                                &span_domain_state->initev,
                                                closure);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "Triggering default waitset");
    }
}

static void send_cap_request(struct interdisp_binding *st,
                             struct capref cap, genvaddr_t info)
{
    errval_t err = SYS_ERR_OK, err2;
    struct capref *dest = (struct capref *)(uintptr_t)info;

    err = cap_copy(*dest, cap);
    if(err_is_fail(err)) {
        err_push(err, LIB_ERR_CAP_COPY_FAIL);
        DEBUG_ERR(err, "cap_copy");
        abort();
        goto send_reply;
    }
    err = cap_destroy(cap);
    if(err_is_fail(err)) {
        err_push(err, LIB_ERR_CAP_DELETE_FAIL);
        DEBUG_ERR(err, "cap_destroy default");
        abort();
        goto send_reply;
    }

 send_reply:
    err2 = st->tx_vtbl.send_cap_reply(st, NOP_CONT, err);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "Failed to send send_cap_reply");
    }
}

static errval_t send_cap_err = SYS_ERR_OK;
static bool cap_received = false;

static void send_cap_reply(struct interdisp_binding *st, errval_t err)
{
    send_cap_err = err;
    cap_received = true;
}

static void create_thread_request(struct interdisp_binding *b,
                                  genvaddr_t funcaddr, genvaddr_t argaddr,
                                  uint64_t stacksize)
{
    thread_func_t start_func = (thread_func_t)(uintptr_t)funcaddr;
    void *arg = (void *)(uintptr_t)argaddr;
    struct thread *newthread;

    // XXX: Probably want to return pointer to thread struct to caller
    if(stacksize > 0) {
        newthread = thread_create_varstack(start_func, arg, stacksize);
    } else {
        newthread = thread_create(start_func, arg);
    }
    assert(newthread != NULL);
}

static void wakeup_thread_request(struct interdisp_binding *b,
                                  genvaddr_t taddr)
{
    coreid_t core_id = disp_get_core_id();
    struct thread *wakeup = (struct thread *)(uintptr_t)taddr;
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    /* assert_disabled(wakeup->disp == handle); */
    assert_disabled(wakeup->coreid == core_id);
    wakeup->disp = handle;
    thread_enqueue(wakeup, &disp_gen->runq);
    disp_enable(handle);
}

/*
 * XXX: The whole span_slave*() thing is a hack to allow all
 * dispatchers to wait on both the monitor and interdisp waitsets
 * while we bind to all.
 */

static int span_slave_thread(void *arg)
{
    errval_t err = thread_detach(thread_self());
    assert(err_is_ok(err));

    for(;;) {
        event_dispatch(get_default_waitset());
    }

    return 0;
}

static void span_slave_request(struct interdisp_binding *b)
{
    USER_PANIC("shouldn't be called");
    thread_create(span_slave_thread, NULL);
}

static void span_slave_done_handler(void *cs)
{
    USER_PANIC("shouldn't be called");
    free(cs);
    thread_exit();
}

static void span_slave_done_request(struct interdisp_binding *b)
{
    USER_PANIC("shouldn't be called");
    struct waitset_chanstate *cs = malloc(sizeof(struct waitset_chanstate));

    // Signal the default waitset of this event
    struct event_closure closure = {
        .handler = span_slave_done_handler,
        .arg = cs,
    };
    waitset_chanstate_init(cs, CHANTYPE_EVENT_QUEUE);
    errval_t err = waitset_chan_trigger_closure(get_default_waitset(), cs,
                                                closure);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "Triggering default waitset");
    }
}

static void span_eager_connect_request(struct interdisp_binding *b,
                                       coreid_t core_id)
{
    struct domain_state *domain_state = get_domain_state();

    /* Store the sending core's connection */
    domain_state->b[core_id] = b;
}

static struct interdisp_rx_vtbl interdisp_vtbl = {
    .dispatcher_initialized = dispatcher_initialized,

    .send_cap_request = send_cap_request,
    .send_cap_reply = send_cap_reply,

    .wakeup_thread    = wakeup_thread_request,
    .create_thread    = create_thread_request,

    // XXX: Hack to allow domain_new_dispatcher() to proceed when not all
    // default waitsets are serviced
    .span_slave       = span_slave_request,
    .span_slave_done  = span_slave_done_request,
    .span_eager_connect = span_eager_connect_request,
};

/**
 * \brief Called when the "client" connects to "server"
 *
 * Make the connection a "server" connection, free unnecessary state.
 * Send init msg to the dispatcher that spanned this dispatcher.
 */
static void client_connected(void *st, errval_t err,
                             struct interdisp_binding *b)
{
    struct remote_core_state *state = (struct remote_core_state*)st;
    struct domain_state *domain_state = get_domain_state();

    if(err_is_fail(err)) {
        DEBUG_ERR(err, "binding to interdisp service");
        abort();
    }

    /* Set it on the domain library state */
    b->rx_vtbl = interdisp_vtbl;
    domain_state->b[state->cnt] = b;

    // Send it our core id
    err = b->tx_vtbl.span_eager_connect(b, NOP_CONT, disp_get_core_id());
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending span_eager_connect");
    }

    // Connect to next active dispatcher
    do {
        state->cnt++;
        if(state->cnt == disp_get_core_id()) {
            state->cnt++;
        }
    } while(allirefs[state->cnt] == NULL_IREF && state->cnt < MAX_CPUS);

    if(state->cnt < MAX_CPUS) {
        err = interdisp_bind(allirefs[state->cnt], client_connected,
                             state, &domain_state->interdisp_ws,
                             IDC_BIND_FLAGS_DEFAULT);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "Binding to inter-dispatcher service");
        }
    } else {
        struct interdisp_binding *sb = domain_state->b[state->core_id];
        /* Send initialized msg to the dispatcher that spanned us */
        errval_t err2 = sb->tx_vtbl.
            dispatcher_initialized(sb, NOP_CONT,
                                   (uintptr_t)state->span_domain_state);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err, "failed to send initalized msg");
            abort();
        }

        state->initialized = true;
    }
}

static errval_t server_connected(void *st, struct interdisp_binding *b)
{
    b->rx_vtbl = interdisp_vtbl;
    return SYS_ERR_OK;
}

/**
 * \brief Called when domain gets a interdisp service.
 * It will set it on the domain_state.
 */
static void server_listening(void *st, errval_t err, iref_t iref)
{
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "interdisp service export");
        abort();
    }

    struct domain_state *domain_state = get_domain_state();
    domain_state->iref = iref;

    // Also set in the global array
    allirefs[disp_get_core_id()] = iref;
    domain_state->conditional = true;
}

/**
 * \brief Called on the inter-disp handler thread, when another thread
 * on this dispatcher wants to wakeup a thread on a foreign dispatcher.
 */
static void handle_wakeup_on(void *arg)
{
    struct domain_state *domain_state = get_domain_state();
    errval_t err;

    assert(domain_state != NULL);

    // Dequeue all (disable to ensure mutual exclusion -- per dispatcher)
    for(;;) {
        struct thread *thread = NULL;

        dispatcher_handle_t disp = disp_disable();
        if(domain_state->remote_wakeup_queue != NULL) {
            thread = thread_dequeue(&domain_state->remote_wakeup_queue);
        }
        disp_enable(disp);

        // Break if queue empty
        if(thread == NULL) {
            break;
        }

        // XXX: Hack
        /* coreid_t core_id = disp_handle_get_core_id(thread->disp); */
        coreid_t core_id = thread->coreid;

        assert(domain_state->b[core_id] != NULL);

        struct interdisp_binding *b = domain_state->b[core_id];
        err = b->tx_vtbl.wakeup_thread(b, NOP_CONT, (genvaddr_t)(uintptr_t)thread);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "wakeup_thread");
        }
    }
}

/**
 * \brief Handler thread for inter-dispatcher messages
 * \param arg   Pointer to inter-dispatcher waitset
 * \return 0 on successful exit
 */
static int interdisp_msg_handler(void *arg)
{
    struct waitset *ws = arg;
    assert(ws != NULL);

    for(;;) {
        errval_t err = event_dispatch(ws);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "error on event dispatch");
        }
    }

    return 0;
}

/**
 * \brief Runs enabled on the remote core to initialize the dispatcher
 */
static int remote_core_init_enabled(void *arg)
{
    errval_t err;
    struct remote_core_state *remote_core_state =
        (struct remote_core_state*)arg;

    /* Initialize the barrelfish library */
    err = barrelfish_init_onthread(NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "barrelfish_init_onthread failed");
        abort();
        return -1;
    }

    // Connect to all dispatchers eagerly
    remote_core_state->cnt = 0;
    while(allirefs[remote_core_state->cnt] == NULL_IREF && remote_core_state->cnt < MAX_CPUS) {
        remote_core_state->cnt++;
        if(remote_core_state->cnt == disp_get_core_id()) {
            remote_core_state->cnt++;
        }
    }
    // Don't move before barrelfish_init_onthread()
    struct domain_state *st = get_domain_state();
    if(remote_core_state->cnt != MAX_CPUS) {
        err = interdisp_bind(allirefs[remote_core_state->cnt], client_connected,
                             remote_core_state, &st->interdisp_ws,
                             IDC_BIND_FLAGS_DEFAULT);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "Failure binding to inter-dispatcher service");
        }
    }

    while(!remote_core_state->initialized) {
        event_dispatch(get_default_waitset());
    }

    /* Free unnecessary state */
    free(remote_core_state);

    /* XXX: create a thread that will handle the default waitset */
    st->default_waitset_handler = thread_create(span_slave_thread, NULL);
    assert(st->default_waitset_handler != NULL);

    return interdisp_msg_handler(&st->interdisp_ws);
}

/**
 * \brief Runs disabled on the remote core to initialize
 */
static void remote_core_init_disabled(struct thread *thread)
{
    dispatcher_handle_t disp = thread->disp;

    /* Initialize the dispatcher */
    disp_init_disabled(disp);

    /* Initialize the threads library, and call remote_core_init_enabled */
    thread_init_remote(disp, thread);
}

/**
 * \brief Initialize the domain library
 *
 * Registers a iref with the monitor to offer the interdisp service on this core
 * Does not block for completion.
 */
errval_t domain_init(void)
{
    errval_t err;
    struct domain_state *domain_state = malloc(sizeof(struct domain_state));
    if (!domain_state) {
        return LIB_ERR_MALLOC_FAIL;
    }
    set_domain_state(domain_state);

    domain_state->iref = 0;
    domain_state->default_waitset_handler = NULL;
    domain_state->remote_wakeup_queue = NULL;
    waitset_chanstate_init(&domain_state->remote_wakeup_event,
                           CHANTYPE_EVENT_QUEUE);
    for (int i = 0; i < MAX_CPUS; i++) {
        domain_state->b[i] = NULL;
    }

    waitset_init(&domain_state->interdisp_ws);
    domain_state->conditional = false;
    err = interdisp_export(NULL, server_listening, server_connected,
                           &domain_state->interdisp_ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    // XXX: Wait for the export to finish before returning
    while(!domain_state->conditional) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/**
 * \brief Handler to continue spanning domain state machine
 */
static void span_domain_reply(struct monitor_binding *mb,
                              errval_t msgerr, uintptr_t domain_id)
{
    /* On success, no further action needed */
    if (err_is_ok(msgerr)) {
        return;
    }

    /* On failure, release resources and notify the caller */
    struct span_domain_state *span_domain_state =
        (struct span_domain_state*)domain_id;
    errval_t err = cap_destroy(span_domain_state->frame);
    if (err_is_fail(err)) {
        err_push(msgerr, LIB_ERR_CAP_DESTROY);
    }

    if (span_domain_state->callback) { /* Use the callback to return error */
        span_domain_state->callback(span_domain_state->callback_arg, msgerr);
    } else { /* Use debug_err if no callback registered */
        DEBUG_ERR(msgerr, "Failure in span_domain_reply");
    }
    free(span_domain_state);
}

static void span_domain_request_sender(void *arg)
{
    struct monitor_binding *mb = arg;
    struct span_domain_state *st = mb->st;

    errval_t err = mb->tx_vtbl.
        span_domain_request(mb, NOP_CONT, (uintptr_t)st, st->core_id, st->vroot,
                            st->frame);
    if (err_is_ok(err)) {
        event_mutex_unlock(&mb->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        /* Wait to use the monitor binding */
        err = mb->register_send(mb, mb->waitset,
                                MKCONT(span_domain_request_sender,mb));
        if(err_is_fail(err)) { // shouldn't fail, as we have the mutex
            USER_PANIC_ERR(err, "register_send");
        }
    } else { // permanent error
        event_mutex_unlock(&mb->mutex);
        err = err_push(err, MON_CLIENT_ERR_SPAN_DOMAIN_REQUEST);
        DEBUG_ERR(err, "span_domain_request");
    }
}

static void span_domain_request_sender_wrapper(void *st)
{
    struct monitor_binding *mb = get_monitor_binding();
    mb->st = st;
    span_domain_request_sender(mb);
}

/**
 * \brief Since we cannot dynamically grow our stack yet, we need a
 * verion that will create threads on remote core with variable stack size
 *
 * \bug this is a hack
 */
static errval_t domain_new_dispatcher_varstack(coreid_t core_id,
                                               domain_spanned_callback_t callback,
                                               void *callback_arg, size_t stack_size)
{
    assert(core_id != disp_get_core_id());

    errval_t err;
    struct domain_state *domain_state = get_domain_state();
    struct monitor_binding *mb = get_monitor_binding();
    assert(domain_state != NULL);

    /* Set reply handler */
    mb->rx_vtbl.span_domain_reply = span_domain_reply;

    while(domain_state->iref == 0) { /* If not initialized, wait */
        messages_wait_and_handle_next();
    }

    /* Create the remote_core_state passed to the new dispatcher */
    struct remote_core_state *remote_core_state =
        calloc(1, sizeof(struct remote_core_state));
    if (!remote_core_state) {
        return LIB_ERR_MALLOC_FAIL;
    }
    remote_core_state->core_id = disp_get_core_id();
    remote_core_state->iref    = domain_state->iref;

    /* Create the thread for the new dispatcher to init on */
    struct thread *newthread =
        thread_create_unrunnable(remote_core_init_enabled,
                                 (void*)remote_core_state, stack_size);
    if (newthread == NULL) {
        return LIB_ERR_THREAD_CREATE;
    }

    /* Save the state for later steps of the spanning state machine */
    struct span_domain_state *span_domain_state =
        malloc(sizeof(struct span_domain_state));
    if (!span_domain_state) {
        return LIB_ERR_MALLOC_FAIL;
    }
    span_domain_state->thread       = newthread;
    span_domain_state->core_id      = core_id;
    span_domain_state->callback     = callback;
    span_domain_state->callback_arg = callback_arg;

    /* Give remote_core_state pointer to span_domain_state */
    remote_core_state->span_domain_state = span_domain_state;

    /* Start spanning domain state machine by sending vroot to the monitor */
    struct capref vroot = {
        .cnode = cnode_page,
        .slot = 0
    };

    /* Create new dispatcher frame */
    struct capref frame;
    size_t dispsize = ((size_t)1) << DISPATCHER_FRAME_BITS;
    err = frame_alloc(&frame, dispsize, &dispsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }
    lvaddr_t dispaddr;

    err = vspace_map_one_frame((void **)&dispaddr, dispsize, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    dispatcher_handle_t handle = dispaddr;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    arch_registers_state_t *disabled_area =
        dispatcher_get_disabled_save_area(handle);

    /* Set dispatcher on the newthread */
    span_domain_state->thread->disp = handle;
    span_domain_state->frame = frame;
    span_domain_state->vroot = vroot;

    /* Setup dispatcher */
    disp->udisp = (lvaddr_t)handle;
    disp->disabled = true;
    disp->fpu_trap = 1;
    disp_gen->core_id = span_domain_state->core_id;
    // Setup the dispatcher to run remote_core_init_disabled
    // and pass the created thread as an argument
    registers_set_initial(disabled_area, span_domain_state->thread,
                          (lvaddr_t)remote_core_init_disabled,
                          (lvaddr_t)&disp_gen->stack[DISPATCHER_STACK_WORDS],
                          (uintptr_t)span_domain_state->thread, 0, 0, 0);
    // Give dispatcher a unique name for debugging
    snprintf(disp->name, DISP_NAME_LEN, "%s%d", disp_name(),
             span_domain_state->core_id);

#ifdef __x86_64__
    // XXX: share LDT state between all dispatchers
    // this needs to happen before the remote core starts, otherwise the segment
    // selectors in the new thread state are invalid
    struct dispatcher_shared_x86_64 *disp_x64
        = get_dispatcher_shared_x86_64(handle);
    struct dispatcher_shared_x86_64 *mydisp_x64
        = get_dispatcher_shared_x86_64(curdispatcher());

    disp_x64->ldt_base = mydisp_x64->ldt_base;
    disp_x64->ldt_npages = mydisp_x64->ldt_npages;
#endif

    threads_prepare_to_span(handle);

    // Setup new local thread for inter-dispatcher messages, if not already done
    static struct thread *interdisp_thread = NULL;
    if(interdisp_thread == NULL) {
        interdisp_thread = thread_create(interdisp_msg_handler,
                                         &domain_state->interdisp_ws);
        err = thread_detach(interdisp_thread);
        assert(err_is_ok(err));
    }

#if 0
    // XXX: Tell currently active interdisp-threads to handle default waitset
    for(int i = 0; i < MAX_CPUS; i++) {
        struct interdisp_binding *b = domain_state->b[i];

        if(disp_get_core_id() != i && b != NULL) {
            err = b->tx_vtbl.span_slave(b, NOP_CONT);
            assert(err_is_ok(err));
        }
    }
#endif

    /* XXX: create a thread that will handle the default waitset */
    if (domain_state->default_waitset_handler == NULL) {
        domain_state->default_waitset_handler
            = thread_create(span_slave_thread, NULL);
        assert(domain_state->default_waitset_handler != NULL);
    }

    /* Wait to use the monitor binding */
    struct monitor_binding *mcb = get_monitor_binding();
    event_mutex_enqueue_lock(&mcb->mutex, &span_domain_state->event_qnode,
                             (struct event_closure) {
                                 .handler = span_domain_request_sender_wrapper,
                                     .arg = span_domain_state });

#if 0
    while(!span_domain_state->initialized) {
        event_dispatch(get_default_waitset());
    }

    /* Free state */
    free(span_domain_state);
#endif

    return SYS_ERR_OK;
}

/**
 * \brief Creates a dispatcher on a remote core
 *
 * \param core_id   Id of the core to create the dispatcher on
 * \param callback  Callback to use when new dispatcher is created
 *
 * The new dispatcher is created with the same vroot, sharing the same vspace.
 * The new dispatcher also has a urpc connection to the core that created it.
 */
errval_t domain_new_dispatcher(coreid_t core_id,
                               domain_spanned_callback_t callback,
                               void *callback_arg)
{
    return domain_new_dispatcher_varstack(core_id, callback, callback_arg,
                                          THREADS_DEFAULT_STACK_BYTES);
}

errval_t domain_send_cap(coreid_t core_id, struct capref cap)
{
    errval_t err;
    struct domain_state *domain_state = get_domain_state();
    if (!domain_state->b[core_id]) {
        return LIB_ERR_NO_SPANNED_DISP;
    }

    send_cap_err = SYS_ERR_OK;
    cap_received = false;

    struct interdisp_binding *b = domain_state->b[core_id];
    err = b->tx_vtbl.send_cap_request(b, NOP_CONT, cap, (uintptr_t)&cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SEND_CAP_REQUEST);
    }

    assert(!"NYI");
    // TODO: Handled on different thread
    /* while(!cap_received) { */
    /*     messages_wait_and_handle_next(); */
    /* } */

    return send_cap_err;
}

/**
 * \brief Wakeup a thread on a foreign dispatcher while disabled.
 *
 * \param core_id       Core ID to wakeup on
 * \param thread        Pointer to thread to wakeup
 * \param mydisp        Dispatcher this function is running on
 *
 * \return SYS_ERR_OK on success.
 */
static errval_t domain_wakeup_on_coreid_disabled(coreid_t core_id,
                                                 struct thread *thread,
                                                 dispatcher_handle_t mydisp)
{
    struct domain_state *ds = get_domain_state();

    // XXX: Ugly hack to allow waking up on a core id we don't have a
    // dispatcher handler for
    thread->coreid = core_id;

    // Catch this early
    assert_disabled(ds != NULL);
    if (ds->b[core_id] == NULL) {
        return LIB_ERR_NO_SPANNED_DISP;
    }

    thread_enqueue(thread, &ds->remote_wakeup_queue);

    // Signal the inter-disp waitset of this event
    struct event_closure closure = {
        .handler = handle_wakeup_on
    };
    errval_t err =
        waitset_chan_trigger_closure_disabled(&ds->interdisp_ws,
                                              &ds->remote_wakeup_event,
                                              closure,
                                              mydisp);
    assert_disabled(err_is_ok(err) ||
                    err_no(err) == LIB_ERR_CHAN_ALREADY_REGISTERED);

    return SYS_ERR_OK;
}

errval_t domain_wakeup_on_disabled(dispatcher_handle_t disp,
                                   struct thread *thread,
                                   dispatcher_handle_t mydisp)
{
    coreid_t core_id = disp_handle_get_core_id(disp);

    // TODO: Can't wakeup on anyone else than the owning dispatcher yet
    assert_disabled(disp == thread->disp);

    return domain_wakeup_on_coreid_disabled(core_id, thread, mydisp);
}

errval_t domain_wakeup_on(dispatcher_handle_t disp,
                          struct thread *thread)
{
    dispatcher_handle_t mydisp = disp_disable();
    errval_t err = domain_wakeup_on_disabled(disp, thread, mydisp);
    disp_enable(mydisp);
    return err;
}

errval_t domain_thread_move_to(struct thread *thread, coreid_t core_id)
{
    assert(thread == thread_self());
    dispatcher_handle_t mydisp = disp_disable();
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(mydisp);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(mydisp);

    struct thread *next = thread->next;
    thread_remove_from_queue(&disp_gen->runq, thread);

    errval_t err = domain_wakeup_on_coreid_disabled(core_id, thread, mydisp);
    if(err_is_fail(err)) {
        thread_enqueue(thread, &disp_gen->runq);
        disp_enable(mydisp);
        return err;
    }

    // run the next thread, if any
    if (next != thread) {
        disp_gen->current = next;
        disp_resume(mydisp, &next->regs);
    } else {
        disp_gen->current = NULL;
        disp->haswork = havework_disabled(mydisp);
        disp_yield_disabled(mydisp);
    }

    USER_PANIC("should never be reached");
}

errval_t domain_thread_create_on_varstack(coreid_t core_id,
                                          thread_func_t start_func,
                                          void *arg, size_t stacksize)
{
    if (disp_get_core_id() == core_id) {
        struct thread *th = NULL;
        if (stacksize == 0) {
            th = thread_create(start_func, arg);
        } else {
            th = thread_create_varstack(start_func, arg, stacksize);
        }
        if (th != NULL) {
            return SYS_ERR_OK;
        } else {
            return LIB_ERR_THREAD_CREATE;
        }
    } else {
        struct domain_state *domain_state = get_domain_state();
        errval_t err;

        if (domain_state->b[core_id] == NULL) {
            return LIB_ERR_NO_SPANNED_DISP;
        }

        struct interdisp_binding *b = domain_state->b[core_id];
        err = b->tx_vtbl.create_thread(b, NOP_CONT,
                                       (genvaddr_t)(uintptr_t)start_func,
                                       (genvaddr_t)(uintptr_t)arg,
                                       stacksize);
        if (err_is_fail(err)) {
            return err;
        }

        return SYS_ERR_OK;
    }
}

errval_t domain_thread_create_on(coreid_t core_id, thread_func_t start_func,
                                 void *arg)
{
    return domain_thread_create_on_varstack(core_id, start_func, arg, 0);
}

/**
 * \brief set the core_id.
 *
 * Code using this should do a kernel_cap invocation to get the core_id first.
 */
void disp_set_core_id(coreid_t core_id)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_id = core_id;
}


/**
 * \brief returns the core_id stored in disp_priv struct
 */
coreid_t disp_get_core_id(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_id;
}

/**
 * \brief returns the domain_id stored in disp_priv struct
 */
domainid_t disp_get_domain_id(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->domain_id;
}

/**
 * \brief returns the core_id stored in disp_priv struct
 */
coreid_t disp_handle_get_core_id(dispatcher_handle_t handle)
{
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_id;
}

struct waitset *get_default_waitset(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.c.default_waitset;
}

/**
 * \brief set the monitor client binding on the dispatcher priv
 */
void set_monitor_binding(struct monitor_binding *b)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.monitor_binding = b;
}

/**
 * \brief Returns the monitor client binding on the dispatcher priv
 */
struct monitor_binding *get_monitor_binding(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.monitor_binding;
}


/**
 * \brief set the  blocking rpc monitor client binding on the dispatcher priv
 */
void set_monitor_blocking_rpc_client(struct monitor_blocking_rpc_client *st)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.monitor_blocking_rpc_client = st;
}

/**
 * \brief Returns the blocking rpc monitor client binding on the
 * dispatcher priv
 */
struct monitor_blocking_rpc_client *get_monitor_blocking_rpc_client(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.monitor_blocking_rpc_client;
}

/**
 * \brief set the mem client on the dispatcher priv
 */
void set_mem_client(struct mem_rpc_client *st)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.mem_st = st;
}

/**
 * \brief Returns the mem client on the dispatcher priv
 */
struct mem_rpc_client *get_mem_client(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.mem_st;
}

/**
 * \brief Returns a pointer to the current vspace on the dispatcher priv
 */
struct vspace *get_current_vspace(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.vspace_state.vspace;
}

/**
 * \brief Returns a pointer to the current pinned state on the dispatcher priv
 */
struct pinned_state *get_current_pinned_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.pinned_state;
}

/**
 * \brief Returns a pointer to the current pmap on the dispatcher priv
 */
struct pmap *get_current_pmap(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return (struct pmap*)&disp->core_state.vspace_state.pmap;
}

/**
 * \brief Returns a pointer to the morecore state on the dispatcher priv
 */
struct morecore_state *get_morecore_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.c.morecore_state;
}

/**
 * \brief Returns a pointer to the ram_alloc state on the dispatcher priv
 */
struct ram_alloc_state *get_ram_alloc_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.c.ram_alloc_state;
}

/**
 * \brief Returns a pointer to the ram_alloc state on the dispatcher priv
 */
struct skb_state *get_skb_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.c.skb_state;
}

/**
 * \brief Returns a pointer to the octopus rpc client on the dispatcher priv
 */
struct octopus_rpc_client *get_octopus_rpc_client(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.octopus_rpc_client;
}

/**
 * \brief Sets the octopus rpc client on the dispatcher priv
 */
void set_octopus_rpc_client(struct octopus_rpc_client *c)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.octopus_rpc_client = c;
}

/**
 * \brief Returns a pointer to the chips_context state on the dispatcher priv
 */
struct spawn_rpc_client *get_spawn_rpc_client(coreid_t core)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    assert(core < MAX_CPUS);
    return disp->core_state.c.spawn_rpc_clients[core];
}

/**
 * \brief set the chips_context state on the dispatcher priv
 */
void set_spawn_rpc_client(coreid_t core, struct spawn_rpc_client *c)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    assert(core < MAX_CPUS);
    disp->core_state.c.spawn_rpc_clients[core] = c;
}

struct arrakis_rpc_client *get_arrakis_rpc_client(coreid_t core)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    assert(core < MAX_CPUS);
    return disp->core_state.c.arrakis_rpc_clients[core];
}

/**
 * \brief set the chips_context state on the dispatcher priv
 */
void set_arrakis_rpc_client(coreid_t core, struct arrakis_rpc_client *c)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    assert(core < MAX_CPUS);
    disp->core_state.c.arrakis_rpc_clients[core] = c;
}

/**
 * \brief Returns a pointer to the terminal state on the dispatcher priv
 */
struct terminal_state *get_terminal_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.terminal_state;
}

/**
 * \brief set the terminal state on the dispatcher priv
 */
void set_terminal_state(struct terminal_state *st)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.terminal_state = st;
}

/**
 * \brief Returns a pointer to the domain state on the dispatcher priv
 */
struct domain_state *get_domain_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.domain_state;
}

/**
 * \brief set the domain state on the dispatcher priv
 */
void set_domain_state(struct domain_state *st)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.domain_state = st;
}

/**
 * \brief Returns a pointer to the spawn state on the dispatcher priv
 */
struct spawn_state *get_spawn_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return disp->core_state.c.spawn_state;
}

/**
 * \brief set the spawn state on the dispatcher priv
 */
void set_spawn_state(struct spawn_state *st)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    disp->core_state.c.spawn_state = st;
}

/**
 * \brief Returns a pointer to the spawn state on the dispatcher priv
 */
struct slot_alloc_state *get_slot_alloc_state(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic* disp = get_dispatcher_generic(handle);
    return &disp->core_state.c.slot_alloc_state;
}
