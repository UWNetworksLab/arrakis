/**
 * \file
 * \brief Kernel management of dispatchers (implementation).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h> /* XXX wait_for_interrupt, resume, execute */
#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <wakeup.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/lmp.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <barrelfish_kpi/cpu_arch.h>
#include <barrelfish_kpi/registers_arch.h>

#if defined(__x86_64__) || defined(__i386__)
#  include <arch/x86/apic.h>
#endif

#ifdef __x86_64__
#  include <vmkit.h>
#endif

#ifdef FPU_LAZY_CONTEXT_SWITCH
#  include <fpu.h>
#endif

#define MIN(a,b)        ((a) < (b) ? (a) : (b))

/**
 * \brief The kernel timeslice given in milliseconds.
 */
int kernel_timeslice = CONFIG_TIMESLICE;

/// Counter for number of context switches
uint64_t context_switch_counter = 0;

/// Current execution dispatcher (when in system call or exception)
struct dcb *dcb_current = NULL;

/// Remembered FPU-using DCB (NULL if none)
struct dcb *fpu_dcb = NULL;

/**
 * \brief Switch context to 'dcb'.
 *
 * This is a wrapper function to call the real, hardware-dependent
 * context-switch function to switch to the dispatcher, pointed to by
 * 'dcb'. It also sets 'dcb_current'.
 *
 * \param dcb        Pointer to dispatcher to which to switch context.
 */
static inline void context_switch(struct dcb *dcb)
{
//    printf("Executing the context switch\n");
    assert(dcb != NULL);
    assert(dcb->vspace != 0);

    // VM guests do not have a user space dispatcher
    if (!dcb->is_vm_guest) {
        assert(dcb->disp != 0);
    }

#ifdef FPU_LAZY_CONTEXT_SWITCH
    // XXX: It should be possible to merge this code fragment with the
    // other FPU restore fragment below
    if(fpu_dcb != NULL && !dcb->is_vm_guest) {
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(dcb->disp);

        // Switch FPU trap on if we switch away from FPU DCB and target is enabled
        // If target disabled, we eagerly restore the FPU
        if(fpu_dcb != dcb && !dcb->disabled) {
            disp->fpu_trap = 1;
        }

        // Restore FPU trap state
        if(disp->fpu_trap) {
            fpu_trap_on();
        } else {
            fpu_trap_off();
        }
    }
#endif

    paging_context_switch(dcb->vspace);
    context_switch_counter++;

    if (!dcb->is_vm_guest) {
        assert(dcb->disp_cte.cap.type == ObjType_Frame);

        /* FIXME: incomplete clean-up of "thread_register" in progress here.
         * Complain vigorously to AB if he checks this mess in
         */
#ifdef __x86_64__  /* Setup new LDT */
        maybe_reload_ldt(dcb, false);
#else
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(dcb->disp);

#ifdef FPU_LAZY_CONTEXT_SWITCH
        // Eagerly restore FPU if it was used disabled and set FPU trap accordingly
        if(disp->fpu_used && dcb->disabled) {
            // Context switch if FPU state is stale
            if(fpu_dcb != dcb) {
                // XXX: Need to reset fpu_dcb when that DCB is deleted
                struct dispatcher_shared_generic *dst =
                    get_dispatcher_shared_generic(fpu_dcb->disp);

                fpu_trap_off();

                // Store old FPU state if it was used
                if(fpu_dcb->disabled) {
                    fpu_save(dispatcher_get_disabled_fpu_save_area(fpu_dcb->disp));
		    dst->fpu_used = 1;
                } else {
                    assert(!fpu_dcb->disabled);
                    fpu_save(dispatcher_get_enabled_fpu_save_area(fpu_dcb->disp));
		    dst->fpu_used = 2;
                }

		if(disp->fpu_used == 1) {
		  fpu_restore(dispatcher_get_disabled_fpu_save_area(dcb->disp));
		} else {
		  assert(disp->fpu_used == 2);
		  fpu_restore(dispatcher_get_enabled_fpu_save_area(dcb->disp));
		}

                // Restore trap state once more, since we modified it
                if(disp->fpu_trap) {
                    fpu_trap_on();
                } else {
                    fpu_trap_off();
                }
            }
            fpu_dcb = dcb;
        }
#endif /* FPU_LAZY_CONTEXT_SWITCH */

	/*
	 * The name of the function is somewhat misleading. we need an unused
	 * user register that always stores the pointer to the current
	 * dispatcher. most ABIs define a register for thread-local storage,
	 * and we have been abusing that on x64 for the dispatcher pointer
	 * --arch_set_thread_ register sets this pointer.  Obviously this
	 * needs to change to support thread-local storage using a standard
	 * ABI, so we will have to figure out how to get to the dispatcher
	 * from something like a thread-local variable.  The reason that this
	 * is in the switch path and not in resume/execute is that on x86_64
	 * loading the thread register (fs) is stupidly expensive, so we avoid
	 * doing it unless we switch contexts -- presumably that could be a
	 * local optimisation in the x86_64 dispatch paths rather than the
	 * generic context_switch path/
	 */
        arch_set_thread_register(disp->udisp);
#endif
    }
}

#ifdef __scc__
struct dcb *run_next = NULL;
#endif

#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define TRACE_N_BM 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK


void __attribute__ ((noreturn)) dispatch(struct dcb *dcb)
{
#ifdef FPU_LAZY_CONTEXT_SWITCH
    // Save state of FPU trap for this domain (treat it like normal context switched state)
    if(dcb_current != NULL && !dcb_current->is_vm_guest) {
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(dcb_current->disp);
        disp->fpu_trap = fpu_trap_get();
    }
#endif

    // XXX FIXME: Why is this null pointer check on the fast path ?
    // If we have nothing to do we should call something other than dispatch
    if (dcb == NULL) {
        dcb_current = NULL;
#if defined(__x86_64__) || defined(__i386__)
        // Can this be moved into wait_for_interrupt?
        // Or wait_for_nonscheduling_interrupt()?
        if (!wakeup_is_pending()) {
            apic_mask_timer();
        }
#endif
        wait_for_interrupt();
    }

    // XXX: run_next scheduling hack
#ifdef __scc__
    if(run_next != NULL) {
        dcb = run_next;
        run_next = NULL;
    }
#endif

    // Don't context switch if we are current already
    if (dcb_current != dcb) {

#ifdef TRACE_CSWITCH
//#if TRACE_N_BM

//#else
        trace_event(TRACE_SUBSYS_KERNEL,
                    TRACE_EVENT_KERNEL_CSWITCH,
                    (uint32_t)(lvaddr_t)dcb & 0xFFFFFFFF);
//#endif // TRACE_N_BM
#endif // TRACE_CSWITCH

        context_switch(dcb);
        dcb_current = dcb;
    }

    assert(dcb != NULL);

    dispatcher_handle_t handle = dcb->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    arch_registers_state_t *disabled_area =
        dispatcher_get_disabled_save_area(handle);

    assert(disp != NULL);
    disp->systime = kernel_now;
    if (dcb->disabled) {
        debug(SUBSYS_DISPATCH, "resume %.*s at 0x%" PRIx64 "\n", DISP_NAME_LEN,
              disp->name, (uint64_t)registers_get_ip(disabled_area));
        assert(dispatcher_is_disabled_ip(handle,
                                         registers_get_ip(disabled_area)));
	if(!dcb->is_vm_guest) {
	  resume(disabled_area);
#ifdef __x86_64__
	} else {
	  vmkit_vmenter(dcb);
#endif
	}
    } else {
        debug(SUBSYS_DISPATCH, "dispatch %.*s\n", DISP_NAME_LEN, disp->name);
        assert(disp->dispatcher_run != 0);
        disp->disabled = 1;
	if(!dcb->is_vm_guest) {
	  execute(disp->dispatcher_run);
#ifdef __x86_64__
	} else {
	  vmkit_vmexec(dcb, disp->dispatcher_run);
#endif
	}
    }
} // end function: dispatch

/**
 * \brief Transfer cap from 'send' to 'ep', according to 'msg'.
 *
 * Reads the cap transfer spec in the LMP message 'msg' and transfers
 * the cap from CSpace in DCB 'send' accordingly.
 *
 * \param ep    Endpoint capability of destination
 * \param send  Pointer to sending DCB.
 * \param send_cptr Address of capability in sender's cspace
 * \param send_bits Valid bits in #send_cptr
 *
 * \return      Error code
 */
static errval_t lmp_transfer_cap(struct capability *ep, struct dcb *send,
                                 capaddr_t send_cptr, uint8_t send_bits)
{
    errval_t err;
    /* Parameter checking */
    assert(send_cptr != CPTR_NULL);
    assert(send != NULL);
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPoint);
    struct dcb *recv = ep->u.endpoint.listener;
    assert(recv != NULL);
    assert(ep->u.endpoint.epoffset != 0);

    /* Look up the slot receiver can receive caps in */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpoint.epoffset);

    // The cnode
    struct capability *recv_cnode_cap;
    err = caps_lookup_cap(&recv->cspace.cap, recv_ep->recv_cptr,
                          recv_ep->recv_bits, &recv_cnode_cap,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP);
    }
    // Check for cnode type
    if (recv_cnode_cap->type != ObjType_CNode) {
        return SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
    }
    // The slot within the cnode
    struct cte *recv_cte;
    recv_cte = caps_locate_slot(recv_cnode_cap->u.cnode.cnode,
                                recv_ep->recv_slot);

    /* Look up source slot in sender */
    struct cte *send_cte;
    err = caps_lookup_slot(&send->cspace.cap, send_cptr, send_bits, &send_cte,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP);
    }

    /* Is destination empty */
    if (recv_cte->cap.type != ObjType_Null) {
        return SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED;
    }

    /* Insert send cap into recv cap */
    err = caps_copy_to_cte(recv_cte, send_cte, false, 0, 0);
    assert(err_is_ok(err)); // Cannot fail after checking that slot is empty

    return SYS_ERR_OK;
}

/**
 * \brief Check if it would be possible to deliver LMP payload, but do not deliver it
 *
 * \param ep     Endpoint capability to send to
 * \param payload_len Length (in number of words) of payload
 */
static errval_t lmp_can_deliver_payload(struct capability *ep,
                                        size_t payload_len)
{
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPoint);
    struct dcb *recv = ep->u.endpoint.listener;
    assert(recv != NULL);

    /* check that receiver exists and has specified an endpoint buffer */
    if (recv->disp == 0 || ep->u.endpoint.epoffset == 0) {
        return SYS_ERR_LMP_NO_TARGET;
    }

    /* locate receiver's endpoint buffer */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpoint.epoffset);

    /* check delivered/consumed state */
    uint32_t epbuflen = ep->u.endpoint.epbuflen;
    uint32_t pos = recv_ep->delivered;
    uint32_t consumed = recv_ep->consumed;
    if (pos >= epbuflen || consumed >= epbuflen) {
        return SYS_ERR_LMP_EP_STATE_INVALID;
    }

    /* compute space available in endpoint */
    uint32_t epspace;
    if (pos >= consumed) {
        epspace = epbuflen - (pos - consumed);
    } else {
        epspace = consumed - pos;
    }

    /* Check if there's enough space for another msg.
     * We always keep one word free, to avoid having the special case where
     * delivered == consumed may mean the buffer is both completely full and
     * completely empty */
    if (epspace <= payload_len + LMP_RECV_HEADER_LENGTH) {
        return SYS_ERR_LMP_BUF_OVERFLOW;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Deliver the payload of an LMP message to a dispatcher.
 *
 * \param ep     Endpoint capability to send to
 * \param send   DCB of the sender. Can be NULL for kernel-originated messages
 * \param payload     Message payload
 * \param payload_len Length (in number of words) of payload
 * \param captransfer True iff a cap has also been delivered
 *
 * \return Error code
 */
errval_t lmp_deliver_payload(struct capability *ep, struct dcb *send,
                             uintptr_t *payload, size_t payload_len,
                             bool captransfer)
{
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPoint);
    struct dcb *recv = ep->u.endpoint.listener;
    assert(recv != NULL);
    assert(payload != NULL || payload_len == 0);

    errval_t err;

    err = lmp_can_deliver_payload(ep, payload_len);
    if (err_is_fail(err)) {
        return err;
    }

    /* locate receiver's endpoint buffer */
    struct lmp_endpoint_kern *recv_ep
        = (void *)((uint8_t *)recv->disp + ep->u.endpoint.epoffset);

    /* read current pos and buflen */
    uint32_t epbuflen = ep->u.endpoint.epbuflen;
    uint32_t pos = recv_ep->delivered;

    struct dispatcher_shared_generic *send_disp =
        send ? get_dispatcher_shared_generic(send->disp) : NULL;
    struct dispatcher_shared_generic *recv_disp =
        get_dispatcher_shared_generic(recv->disp);
    debug(SUBSYS_DISPATCH, "LMP %.*s -> %.*s\n",
          DISP_NAME_LEN, send ? send_disp->name : "kernel",
          DISP_NAME_LEN, recv_disp->name);

    // Setup receiver's message flags
    union lmp_recv_header recvheader = { .raw = 0 };
    recvheader.x.flags.captransfer = captransfer;
    recvheader.x.length = payload_len;

    /* Deliver header */
    recv_ep->buf[pos] = recvheader.raw;
    if (++pos == epbuflen) {
        pos = 0;
    }

    /* Transfer the msg */
    for(int i = 0; i < payload_len; i++) {
        recv_ep->buf[pos] = payload[i];
        if (++pos == epbuflen) {
            pos = 0;
        }
    }

    // update the delivered pos
    recv_ep->delivered = pos;

    // tell the dispatcher that it has an outstanding message in one of its EPs
    recv_disp->lmp_delivered += payload_len + LMP_RECV_HEADER_LENGTH;

    // ... and give it a hint which one to look at
    recv_disp->lmp_hint = ep->u.endpoint.epoffset;

    // Make target runnable
    make_runnable(recv);

    return SYS_ERR_OK;
}

/**
 * \brief Deliver an LMP message to a dispatcher.
 *
 * \param ep     Endpoint capability to send to
 * \param send   DCB of the sender. Can be NULL for kernel-originated messages
 * \param payload Buffer containing message payload
 * \param len    Length of message payload, as number of words
 * \param send_cptr Capability to be transferred with LMP
 * \param send_bits Valid bits in #send_cptr
 */
errval_t lmp_deliver(struct capability *ep, struct dcb *send,
                     uintptr_t *payload, size_t len,
                     capaddr_t send_cptr, uint8_t send_bits)
{
    bool captransfer;
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPoint);
    struct dcb *recv = ep->u.endpoint.listener;
    assert(recv != NULL);
    assert(payload != NULL);

    errval_t err;

    /* Is the sender trying to send a cap? */
    if (send_cptr != CPTR_NULL) {
        /* Don't attempt to transfer the cap if we can't send the payload */
        err = lmp_can_deliver_payload(ep, len);
        if (err_is_fail(err)) {
            return err;
        }

        err = lmp_transfer_cap(ep, send, send_cptr, send_bits);
        if (err_is_fail(err)) {
            return err;
        }

        captransfer = true;
    } else {
        captransfer = false;
    }

    /* Send msg */
    err = lmp_deliver_payload(ep, send, payload, len, captransfer);
    // shouldn't fail, if we delivered the cap successfully
    assert(!(captransfer && err_is_fail(err)));
    return err;
}
