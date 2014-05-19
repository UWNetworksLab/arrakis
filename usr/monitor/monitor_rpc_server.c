/** \file
 *  \brief Monitor's connection with the dispatchers on the same core for
 *  blocking rpc calls.
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include "monitor.h"
#include <barrelfish/monitor_client.h>

// workaround inlining bug with gcc 4.4.1 shipped with ubuntu 9.10 and 4.4.3 in Debian
#if defined(__i386__) && defined(__GNUC__) \
    && __GNUC__ == 4 && __GNUC_MINOR__ == 4 && __GNUC_PATCHLEVEL__ <= 3
#define SAFEINLINE __attribute__((noinline))
#else
#define SAFEINLINE
#endif

/*-------------------------- Internal structures ----------------------------*/

struct retype_st {
    struct rcap_st rcap_st;             // must always be first
    struct monitor_blocking_binding *b;
    struct capref croot;
    capaddr_t src;
    uint64_t new_type;
    uint8_t size_bits;
    capaddr_t to;
    capaddr_t slot;
    int dcn_vbits;
};

struct delete_st {
    struct rcap_st rcap_st;             // must always be first
    struct monitor_blocking_binding *b;
    struct capref croot;
    capaddr_t src;
    uint8_t vbits;
};

struct revoke_st {
    struct rcap_st rcap_st;             // must always be first
    struct monitor_blocking_binding *b;
    struct capref croot;
    capaddr_t src;
    uint8_t vbits;
};

/*------------------------ Static global variables -------------------------*/

static struct retype_st static_retype_state;
static bool static_retype_state_used = false;

static struct delete_st static_delete_state;
static bool static_delete_state_used = false;

static struct revoke_st static_revoke_state;
static bool static_revoke_state_used = false;

/*-------------------------- Helper Functions ------------------------------*/

static void remote_cap_retype_phase_2(void * st_arg);
static void remote_cap_delete_phase_2(void * st_arg);
static void remote_cap_revoke_phase_2(void * st_arg);

static SAFEINLINE struct retype_st *
alloc_retype_st(struct monitor_blocking_binding *b, struct capref croot,
                capaddr_t src, uint64_t new_type, uint8_t size_bits,
                capaddr_t to, capaddr_t slot, int dcn_vbits)
{
    struct retype_st * st;
    if (static_retype_state_used) {
        st = malloc (sizeof(struct retype_st));
        assert(st);
    } else {
        st = &static_retype_state;
        static_retype_state_used = true;
    }

    st->rcap_st.free_at_ccast = false;
    st->rcap_st.cb            = remote_cap_retype_phase_2;
    st->b        = b;
    st->croot    = croot;
    st->src      = src;
    st->new_type = new_type;
    st->size_bits= size_bits;
    st->to       = to;
    st->slot     = slot;
    st->dcn_vbits= dcn_vbits;

    return st;
}

static void free_retype_st(struct retype_st * st)
{
    cap_destroy(st->croot);
    if (st == &static_retype_state) {
        static_retype_state_used = false;
    } else {
        free(st);
    }
}

static SAFEINLINE struct delete_st *
alloc_delete_st(struct monitor_blocking_binding *b, struct capref croot,
                capaddr_t src, uint8_t vbits)
{
    struct delete_st * st;
    if (static_delete_state_used) {
        st = malloc (sizeof(struct delete_st));
        assert(st);
    } else {
        st = &static_delete_state;
        static_delete_state_used = true;
    }

    st->rcap_st.free_at_ccast = false;
    st->rcap_st.cb            = remote_cap_delete_phase_2;
    st->b        = b;
    st->croot    = croot;
    st->src      = src;
    st->vbits    = vbits;

    return st;
}

static void free_delete_st(struct delete_st * st)
{
    cap_destroy(st->croot);
    if (st == &static_delete_state) {
        static_delete_state_used = false;
    } else {
        free(st);
    }
}

// workaround inlining bug with gcc 4.4.1 shipped with ubuntu 9.10 and 4.4.3 in Debian
static SAFEINLINE struct revoke_st *
alloc_revoke_st(struct monitor_blocking_binding *b, struct capref croot,
                capaddr_t src, uint8_t vbits)
{
    struct revoke_st * st;
    if (static_revoke_state_used) {
        st = malloc (sizeof(struct revoke_st));
        assert(st);
    } else {
        st = &static_revoke_state;
        static_revoke_state_used = true;
    }

    st->rcap_st.free_at_ccast = false;
    st->rcap_st.cb            = remote_cap_revoke_phase_2;
    st->b        = b;
    st->croot    = croot;
    st->src      = src;
    st->vbits    = vbits;

    return st;
}

static void free_revoke_st(struct revoke_st * st)
{
    cap_destroy(st->croot);
    if (st == &static_revoke_state) {
        static_revoke_state_used = false;
    } else {
        free(st);
    }
}


/*---------------------------- Handler functions ----------------------------*/



static void remote_cap_retype(struct monitor_blocking_binding *b,
                              struct capref croot, capaddr_t src,
                              uint64_t new_type, uint8_t size_bits,
                              capaddr_t to, capaddr_t slot, int32_t dcn_vbits)
{
    errval_t err;
    bool has_descendants;
    coremask_t on_cores;

    /* Save state for stackripped reply */
    struct retype_st * st = alloc_retype_st(b, croot, src, new_type, size_bits,
                                            to, slot, dcn_vbits);


    /* Get the raw cap from the kernel */
    err = monitor_domains_cap_identify(croot, src, CPTR_BITS,
                                       &(st->rcap_st.capability));
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_REMOTE);
        goto reply;
    }

    /* Check if cap is retyped, if it is there is no point continuing,
       This will be checked again once we succeed in locking cap */
    err = rcap_db_get_info(&st->rcap_st.capability, &has_descendants, &on_cores);
    assert(err_is_ok(err));
    if (has_descendants) {
        err = MON_ERR_REMOTE_CAP_NEED_REVOKE;
        goto reply;
    }

    /* request lock */
    err = rcap_db_acquire_lock(&st->rcap_st.capability, (struct rcap_st*)st);
    if (err_is_fail(err)) {
        goto reply;
    }
    return;  // continues in remote_cap_retype_phase_2

reply:
    free_retype_st(st);
    err = b->tx_vtbl.remote_cap_retype_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}


static void remote_cap_retype_phase_2(void * st_arg)
{
    errval_t err, reply_err;
    bool has_descendants;
    coremask_t on_cores;
    struct retype_st * st = (struct retype_st *) st_arg;
    struct monitor_blocking_binding *b = st->b;


    reply_err = st->rcap_st.err;

    err = rcap_db_get_info(&st->rcap_st.capability, &has_descendants, &on_cores);

    assert(err_is_ok(err));
    if (has_descendants) {
        reply_err = MON_ERR_REMOTE_CAP_NEED_REVOKE;
    }

    if (err_is_fail(reply_err)) {
        // lock failed or cap already retyped, unlock any cores we locked
        err = rcap_db_release_lock(&(st->rcap_st.capability), st->rcap_st.cores_locked);
        assert (err_is_ok(err));
    } else {
        // all good, do retype on domains behalf
        reply_err = monitor_retype_remote_cap(st->croot, st->src, st->new_type,
                                              st->size_bits, st->to, st->slot,
                                              st->dcn_vbits);

        // signal if retype was a success to remote cores
        err = rcap_db_retype(&(st->rcap_st.capability), err_is_ok(reply_err));
        assert (err_is_ok(err));
    }

    free_retype_st(st);
    err = b->tx_vtbl.remote_cap_retype_response(b, NOP_CONT, reply_err);
    assert (err_is_ok(err));
}


static void remote_cap_delete(struct monitor_blocking_binding *b,
                              struct capref croot, capaddr_t src, uint8_t vbits)
{
    errval_t err;

    /* Save state for stackripped reply */
    struct delete_st * st = alloc_delete_st(b, croot, src, vbits);

    /* Get the raw cap from the kernel */
    err = monitor_domains_cap_identify(croot, src, vbits,
                                       &(st->rcap_st.capability));
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_REMOTE);
        goto reply;
    }

    /* request lock */
    err = rcap_db_acquire_lock(&(st->rcap_st.capability), (struct rcap_st*)st);
    if (err_is_fail(err)) {
        goto reply;
    }
    return;  // continues in remote_cap_retype_phase_2

reply:
    free_delete_st(st);
    err = b->tx_vtbl.remote_cap_delete_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void remote_cap_delete_phase_2(void * st_arg)
{
    errval_t err, reply_err;
    struct delete_st * st = (struct delete_st *) st_arg;
    struct monitor_blocking_binding *b = st->b;

    reply_err = st->rcap_st.err;
    if (err_is_fail(reply_err)) {
        // lock failed, unlock any cores we locked
        err = rcap_db_release_lock(&(st->rcap_st.capability),
                                   st->rcap_st.cores_locked);
        assert (err_is_ok(err));
    } else {
        // all good, do delete on domains behalf
        reply_err = monitor_delete_remote_cap(st->croot, st->src, st->vbits);
        if (err_is_fail(reply_err)) {
            DEBUG_ERR(reply_err, "delete cap error");
        }

        if (err_is_ok(reply_err)) {
            // signal delete to other cores
            err = rcap_db_delete(&st->rcap_st.capability);
            assert(err_is_ok(err));
        }
    }

    free_delete_st(st);
    err = b->tx_vtbl.remote_cap_delete_response(b, NOP_CONT, reply_err);
    assert (err_is_ok(err));
}


static void remote_cap_revoke(struct monitor_blocking_binding *b,
                              struct capref croot, capaddr_t src, uint8_t vbits)
{
    errval_t err;
    /* Save state for stackripped reply */
    struct revoke_st * st = alloc_revoke_st(b, croot, src, vbits);

    /* Get the raw cap from the kernel */
    err = monitor_domains_cap_identify(croot, src, vbits,
                                       &(st->rcap_st.capability));
    if (err_is_fail(err)) {
        err_push(err, MON_ERR_CAP_REMOTE);
        goto reply;
    }

    /* request recursive lock on the cap and all of its descendants */
    err = rcap_db_acquire_recursive_lock(&(st->rcap_st.capability),
                                         (struct rcap_st*)st);
    if (err_is_fail(err)) {
        goto reply;
    }
    return;  // continues in remote_cap_retype_phase_2

reply:
    free_revoke_st(st);
    err = b->tx_vtbl.remote_cap_revoke_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void remote_cap_revoke_phase_2(void * st_arg)
{
    errval_t err, reply_err;
    struct revoke_st * st = (struct revoke_st *) st_arg;
    struct monitor_blocking_binding *b = st->b;

    reply_err = st->rcap_st.err;
    if (err_is_fail(reply_err)) {
        // recursive lock failed, unlock any cores we locked
        err = rcap_db_release_recursive_lock(&(st->rcap_st.capability),
                                             st->rcap_st.cores_locked);
        assert (err_is_ok(err));
    } else {
        // all good, do revole on domains behalf
        reply_err = monitor_revoke_remote_cap(st->croot, st->src, st->vbits);
        if (err_is_fail(reply_err)) {
            DEBUG_ERR(reply_err, "revoke cap error");
        }

        if (err_is_ok(reply_err)) {
            // signal revoke to other cores
            err = rcap_db_revoke(&st->rcap_st.capability);
            assert(err_is_ok(err));
        }
    }

    free_revoke_st(st);
    err = b->tx_vtbl.remote_cap_revoke_response(b, NOP_CONT, reply_err);
    assert (err_is_ok(err));
}

static void rsrc_manifest(struct monitor_blocking_binding *b,
                          struct capref dispcap, char *str)
{
    errval_t err, err2;
    rsrcid_t id;

    err = rsrc_new(&id);
    if(err_is_fail(err)) {
        goto out;
    }
    err = rsrc_join(id, dispcap, b);
    if(err_is_fail(err)) {
        // TODO: Cleanup
        goto out;
    }
    err = rsrc_submit_manifest(id, str);

 out:
    err2 = b->tx_vtbl.rsrc_manifest_response(b, NOP_CONT, id, err);
    assert(err_is_ok(err2));
}

static void rsrc_phase(struct monitor_blocking_binding *b,
                       rsrcid_t id, uint32_t phase)
{
    errval_t err;

    err = rsrc_set_phase(id, phase);
    assert(err_is_ok(err));

    err = b->tx_vtbl.rsrc_phase_response(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void rpc_rsrc_join(struct monitor_blocking_binding *b,
                          rsrcid_t id, struct capref dispcap)
{
    errval_t err, err2;

    err = rsrc_join(id, dispcap, b);

    if(err_is_fail(err)) {
        err2 = b->tx_vtbl.rsrc_join_response(b, NOP_CONT, err);
        assert(err_is_ok(err2));
    }
}

static void alloc_monitor_ep(struct monitor_blocking_binding *b)
{
    struct capref retcap = NULL_CAP;
    errval_t err, reterr = SYS_ERR_OK;

    struct monitor_lmp_binding *lmpb =
        malloc(sizeof(struct monitor_lmp_binding));
    assert(lmpb != NULL);

    // setup our end of the binding
    err = monitor_client_lmp_accept(lmpb, get_default_waitset(),
                                    DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(lmpb);
        reterr = err_push(err, LIB_ERR_MONITOR_CLIENT_ACCEPT);
        goto out;
    }

    retcap = lmpb->chan.local_cap;
    monitor_server_init(&lmpb->b);

out:
    err = b->tx_vtbl.alloc_monitor_ep_response(b, NOP_CONT, reterr, retcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send alloc_monitor_ep_reply");
    }
}

static void cap_identify(struct monitor_blocking_binding *b,
                         struct capref cap)
{
    errval_t err, reterr;

    union capability_caprep_u u;
    reterr = monitor_cap_identify(cap, &u.cap);

    /* XXX: shouldn't we skip this if we're being called from the monitor?
     * apparently not: we make a copy of the cap on LMP to self?!?! */
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = b->tx_vtbl.cap_identify_response(b, NOP_CONT, reterr, u.caprepb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reply failed");
    }
}

#define ARM_IRQ_MAX 256

static void arm_irq_handle_call(struct monitor_blocking_binding *b,
        struct capref ep, uint32_t irq)
{

    errval_t err = 1;

    if (irq <= ARM_IRQ_MAX) {
        err = invoke_irqtable_set(cap_irq, irq, ep);
    }

    errval_t err2 = b->tx_vtbl.arm_irq_handle_response(b, NOP_CONT, err);
    assert(err_is_ok(err2));
}

static void irq_handle_call(struct monitor_blocking_binding *b, struct capref ep)
{
    /* allocate a new slot in the IRQ table */
    // XXX: probably want to be able to reuse vectors! :)
    static int nextvec = 0;
    int vec = nextvec++;

    /* set it and reply */
    errval_t err = invoke_irqtable_set(cap_irq, vec, ep);
    errval_t err2 = b->tx_vtbl.irq_handle_response(b, NOP_CONT, err, vec);
    assert(err_is_ok(err2));
}

static void get_arch_core_id(struct monitor_blocking_binding *b)
{
    static uintptr_t arch_id = -1;
    errval_t err;

    if (arch_id == -1) {
        err = invoke_monitor_get_arch_id(&arch_id);
        assert(err_is_ok(err));
        assert(arch_id != -1);
    }

    err = b->tx_vtbl.get_arch_core_id_response(b, NOP_CONT, arch_id);
    assert(err_is_ok(err));
}

static void cap_set_remote_done(void *arg)
{
    struct capref *tmpcap = arg;

    errval_t err = cap_destroy(*tmpcap);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "cap_destroy");
    }

    free(tmpcap);
}

struct pending_reply {
    struct monitor_blocking_binding *b;
    errval_t err;
    struct capref *cap;
};

static void retry_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct monitor_blocking_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.cap_set_remote_response(b, MKCONT(cap_set_remote_done, r->cap),
                                             r->err);
    if (err_is_ok(err)) {
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply, r));
        assert(err_is_ok(err));
    } else {
        DEBUG_ERR(err, "failed to reply to memory request");
        cap_set_remote_done(r->cap);
    }
}

static void cap_set_remote(struct monitor_blocking_binding *b,
                           struct capref cap, bool remote)
{
    bool has_descendants;
    struct capref *tmpcap = malloc(sizeof(struct capref));
    errval_t err, reterr;

    *tmpcap = cap;
    reterr = monitor_cap_remote(cap, remote, &has_descendants);
    err = b->tx_vtbl.cap_set_remote_response(b, MKCONT(cap_set_remote_done, tmpcap),
                                             reterr);
    if(err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = reterr;
            r->cap = tmpcap;
            err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply, r));
            assert(err_is_ok(err));
        } else {
            USER_PANIC_ERR(err, "cap_set_remote_response");
        }
    }
}

/* ----------------------- BOOTINFO REQUEST CODE START ---------------------- */

static void get_phyaddr_cap(struct monitor_blocking_binding *b)
{
    // XXX: We should not just hand out this cap to everyone
    // who requests it. There is currently no way to determine
    // if the client is a valid recipient
    errval_t err;

    struct capref src = {
        .cnode = cnode_root,
        .slot  = ROOTCN_SLOT_PACN
    };

    err = b->tx_vtbl.get_phyaddr_cap_response(b, NOP_CONT, src,
            SYS_ERR_OK);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_phyaddr_cap, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_phyaddr_cap_response failed");
    }
}

static void get_io_cap(struct monitor_blocking_binding *b)
{
    // XXX: We should not just hand out this cap to everyone
    // who requests it. There is currently no way to determine
    // if the client is a valid recipient
    errval_t err;
    struct capref src = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };

    err = b->tx_vtbl.get_io_cap_response(b, NOP_CONT, src,
            SYS_ERR_OK);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_io_cap, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_io_cap_response failed");
    }
}


static void get_bootinfo(struct monitor_blocking_binding *b)
{
    errval_t err;

    struct capref frame = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_BOOTINFO
    };

    struct frame_identity id = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    err = b->tx_vtbl.get_bootinfo_response(b, NOP_CONT, SYS_ERR_OK, frame,
                                           (size_t)1 << id.bits);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT((void (*)(void *))get_bootinfo, b));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send failed");
            }
        }

        USER_PANIC_ERR(err, "sending get_bootinfo_response failed");
    }
}

/* ----------------------- BOOTINFO REQUEST CODE END ----------------------- */

/*------------------------- Initialization functions -------------------------*/

static struct monitor_blocking_rx_vtbl rx_vtbl = {
    .get_bootinfo_call = get_bootinfo,
    .get_phyaddr_cap_call = get_phyaddr_cap,
    .get_io_cap_call = get_io_cap,

    .remote_cap_retype_call  = remote_cap_retype,
    .remote_cap_delete_call  = remote_cap_delete,
    .remote_cap_revoke_call  = remote_cap_revoke,

    .rsrc_manifest_call      = rsrc_manifest,
    .rsrc_join_call          = rpc_rsrc_join,
    .rsrc_phase_call         = rsrc_phase,

    .alloc_monitor_ep_call   = alloc_monitor_ep,
    .cap_identify_call       = cap_identify,
    .irq_handle_call         = irq_handle_call,
    .arm_irq_handle_call     = arm_irq_handle_call,
    .get_arch_core_id_call   = get_arch_core_id,

    .cap_set_remote_call     = cap_set_remote,
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    set_monitor_rpc_iref(iref);
}

static errval_t connect_callback(void *st, struct monitor_blocking_binding *b)
{
    b->rx_vtbl = rx_vtbl;

    // TODO: set error handler
    return SYS_ERR_OK;
}

errval_t monitor_rpc_init(void)
{
    static struct monitor_blocking_export e = {
        .connect_cb = connect_callback,
        .common = {
            .export_callback = export_callback,
            .flags = IDC_EXPORT_FLAGS_DEFAULT,
            .connect_cb_st = &e,
            .lmp_connect_callback = monitor_blocking_lmp_connect_handler,
        }
    };

    e.waitset = get_default_waitset();

    return idc_export_service(&e.common);
}
