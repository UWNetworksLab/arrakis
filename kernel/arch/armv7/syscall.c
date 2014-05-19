/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/sys_debug.h>

#include <arm_hal.h>
#include <irq.h>

#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <exec.h>
#include <stdio.h>
#include <syscall.h>
#include <armv7_syscall.h>
#include <start_aps.h>
#include <useraccess.h>

__attribute__((noreturn)) void sys_syscall_kernel(void);
__attribute__((noreturn)) void sys_syscall(arch_registers_state_t* context);

__attribute__((noreturn))
void sys_syscall_kernel(void)
{
    panic("Why is the kernel making a system call?");
}

/**
 * \brief Spawn a new core
 */
struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry)
{
#ifdef __ARM_ARCH_7M__
printf("armv7-m can not spawn new cores yet");
#else
	int r;
	switch(cpu_type) {
	case CPU_ARM7:
		r = start_aps_arm_start(core_id, (lvaddr_t)entry);
		if(r != 0)
		{
			return SYSRET(SYS_ERR_CORE_NOT_FOUND);
		}
		break;
	default:
        assert(!"Architecture not supported");
        return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        break;
	}
#endif //defined(__ARM_ARCH_7M__)
    return SYSRET(SYS_ERR_OK);
}

static struct sysret
handle_dispatcher_setup(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(7 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t   odptr    = sa->arg2;
    capaddr_t   cptr     = sa->arg3;
    uintptr_t rundepth = sa->arg4;
    int       depth    = rundepth & 0xff;
    int       run      = rundepth >> 8;
    capaddr_t   vptr     = sa->arg5;
    capaddr_t   dptr     = sa->arg6;

    return sys_dispatcher_setup(to, cptr, depth, vptr, dptr, run, odptr);
}

static struct sysret
handle_dispatcher_properties(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(8 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    enum task_type type = (enum task_type)(sa->arg3 >> 16);
    uint16_t weight = sa->arg3 & 0xffff;

    return sys_dispatcher_properties(to, type, sa->arg4,
                                     sa->arg5, sa->arg6, sa->arg7, weight);
}

static struct sysret
handle_dispatcher_perfmon(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);
}

static struct sysret
handle_frame_identify(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);

    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    assert(to->u.frame.bits < BASE_PAGE_SIZE);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = to->u.frame.base | to->u.frame.bits,
    };
}

static struct sysret
handle_frame_modify_flags(
        struct capability *to,
        arch_registers_state_t *context,
        int argc
        )
{
    // Modify flags of (part of) mapped region of frame
    assert (5 == argc);

    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);

    // unpack arguments
    struct registers_arm_syscall_args* sa = &context->syscall_args;
    size_t offset = sa->arg2; // in pages; of first page to modify from first
                              // page in mapped region
    size_t pages  = sa->arg3; // #pages to modify
    size_t flags  = sa->arg4; // new flags

    paging_modify_flags(to, offset, pages, flags);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = 0,
    };
}

static struct sysret
handle_mint(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(7 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t destcn_cptr  = sa->arg2;
    capaddr_t source_cptr  = sa->arg3;
    capaddr_t dest_slot    = sa->arg4 >> 16;
    int     destcn_vbits = (sa->arg4 >> 8) & 0xff;
    int     source_vbits = sa->arg4 & 0xff;

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, sa->arg5, sa->arg6, true);
}

static struct sysret
handle_copy(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(5 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t destcn_cptr  = sa->arg2;
    capaddr_t source_cptr  = sa->arg3;
    capaddr_t dest_slot    = sa->arg4 >> 16;
    int     destcn_vbits = (sa->arg4 >> 8) & 0xff;
    int     source_vbits = sa->arg4 & 0xff;

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, 0, 0, false);
}

static struct sysret
handle_retype_common(
    struct capability* root,
    bool from_monitor,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(6 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    // Source capability cptr
    capaddr_t source_cptr      = sa->arg2;
    uintptr_t word           = sa->arg3;
    // Type to retype to
    enum objtype type        = word >> 16;
    // Object bits for variable-sized types
    uint8_t objbits          = (word >> 8) & 0xff;
    // Destination cnode cptr
    capaddr_t  dest_cnode_cptr = sa->arg4;
    // Destination slot number
    capaddr_t dest_slot        = sa->arg5;
    // Valid bits in destination cnode cptr
    uint8_t dest_vbits       = (word & 0xff);

    return sys_retype(root, source_cptr, type, objbits, dest_cnode_cptr,
                      dest_slot, dest_vbits, from_monitor);
}

static struct sysret
handle_retype(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    return handle_retype_common(root, false, context, argc);
}

static struct sysret
handle_delete(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = (capaddr_t)sa->arg2;
    int     bits = (int)sa->arg3;

    return sys_delete(root, cptr, bits, false);
}

static struct sysret
handle_create(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(5 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    enum objtype type      = (sa->arg2 >> 16) & 0xffff;
    uint8_t      objbits   = (sa->arg2 >> 8) & 0xff;
    capaddr_t    dest_cptr = sa->arg3;
    cslot_t      dest_slot = sa->arg4;
    int          bits      = sa->arg2 & 0xff;
    printk(LOG_NOTE, "type = %d, bits = %d\n", type, bits);

    return sys_create(root, type, objbits, dest_cptr, dest_slot, bits);
}

static struct sysret
handle_revoke(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = (capaddr_t)sa->arg2;
    int     bits = (int)sa->arg3;

    return sys_revoke(root, cptr, bits, false);
}

static struct sysret
handle_map(
    struct capability *ptable,
    arch_registers_state_t *context,
    int argc
    )
{
    assert(7 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    /* Retrieve arguments */
    capaddr_t  source_cptr   = (capaddr_t)sa->arg2;
    capaddr_t dest_slot      = ((capaddr_t)sa->arg3) >> 16;
    int      source_vbits  = ((int)sa->arg3) & 0xff;
    uintptr_t flags, offset,pte_count;
    flags = (uintptr_t)sa->arg4;
    offset = (uintptr_t)sa->arg5;
    pte_count = (uintptr_t)sa->arg6;

    return sys_map(ptable, dest_slot, source_cptr, source_vbits,
                   flags, offset, pte_count);
}

static struct sysret
handle_unmap(
    struct capability* ptable,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    /* Retrieve arguments */
    capaddr_t  mapping_cptr  = (capaddr_t)sa->arg2;
    int mapping_bits         = (((int)sa->arg3) >> 20) & 0xff;
    size_t pte_count         = (((size_t)sa->arg3) >> 10) & 0x3ff;
    pte_count               += 1;
    size_t entry             = ((size_t)sa->arg3) & 0x3ff;

    errval_t err;
    struct cte *mapping = NULL;
    err = caps_lookup_slot(&dcb_current->cspace.cap, mapping_cptr, mapping_bits,
                           &mapping, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_CAP_NOT_FOUND));
    }

    err = page_mappings_unmap(ptable, mapping, entry, pte_count);
    return SYSRET(err);
}

static struct sysret
monitor_get_core_id(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);

    return (struct sysret) { .error = SYS_ERR_OK, .value = my_core_id };
}

static struct sysret
monitor_get_arch_id(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);

    // TODO: ARM doesn't support multicore yet...
    return (struct sysret) { .error = SYS_ERR_OK, .value = my_core_id };
}

static struct sysret
monitor_handle_register(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(3 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    capaddr_t ep_caddr = (capaddr_t)sa->arg2;

    return sys_monitor_register(ep_caddr);
}

static struct sysret
monitor_remote_cap(
	struct capability *kernel_cap,
	arch_registers_state_t* context,
	int argc)
{
	//assert(3 == argc);

	struct registers_arm_syscall_args* sa = &context->syscall_args;

	struct capability *root = &dcb_current->cspace.cap;
    capaddr_t cptr = sa->arg2;
    int bits = sa->arg3;
    bool remote = (bool)sa->arg4;

    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, bits, &cte, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    set_cap_remote(cte, remote);
    bool has_desc = has_descendants(cte);

    return (struct sysret){ .error = SYS_ERR_OK, .value = has_desc };
}

static struct sysret
monitor_create_cap(
    struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(6 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    //printf("%d: %"PRIu32", %"PRIu32", %"PRIu32", %"PRIu32", %"PRIu32", %"PRIu32"\n",
    //        argc, sa->arg0, sa->arg1, sa->arg2, sa->arg3, sa->arg4, sa->arg5);

    /* Create the cap in the destination */
    capaddr_t cnode_cptr = sa->arg2;
    int cnode_vbits    = sa->arg3;
    size_t slot        = sa->arg4;
    struct capability *src =
        (struct capability*)sa->arg5;

    //printf("type = %d\n", src->type);

    /* Certain types cannot be created here */
    if ((src->type == ObjType_Null) || (src->type == ObjType_EndPoint)
        || (src->type == ObjType_Dispatcher) || (src->type == ObjType_Kernel)
        || (src->type == ObjType_IRQTable)) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, src));
}

/**
 * \brief Spawn a new core and create a kernel cap for it.
 */
static struct sysret
monitor_spawn_core(
	struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc)
{
	//assert(3 == argc);

	struct registers_arm_syscall_args* sa = &context->syscall_args;

	coreid_t core_id       = sa->arg2;
    enum cpu_type cpu_type = sa->arg3;
    genvaddr_t entry       = sa->arg5;

    return sys_monitor_spawn_core(core_id, cpu_type, entry);
}

static struct sysret
monitor_identify_cap(
	struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc)
{
	struct registers_arm_syscall_args* sa = &context->syscall_args;

	capaddr_t cptr = sa->arg2;
	int bits = sa->arg3;
	struct capability *retbuf = (void *)sa->arg4;

    return sys_monitor_identify_cap(&dcb_current->cspace.cap, cptr, bits, retbuf);
}

static struct sysret handle_irq_table_set( struct capability* to,
        arch_registers_state_t* context,
        int argc
        )
{
#ifdef __ARM_ARCH_7M__
    printf("armv7-m can not handle userspace IRQs yet\n");
    return SYSRET(SYS_ERR_IRQ_INVALID);
#else
    struct registers_arm_syscall_args* sa = &context->syscall_args;

    return SYSRET(irq_table_set(sa->arg2, sa->arg3));
#endif
}


static struct sysret handle_irq_table_delete( struct capability* to,
        arch_registers_state_t* context,
        int argc
        )
{
#ifdef __ARM_ARCH_7M__
    printf("armv7-m can not handle userspace IRQs yet\n");
    return SYSRET(SYS_ERR_IRQ_INVALID);
#else
    struct registers_arm_syscall_args* sa = &context->syscall_args;

    return SYSRET(irq_table_delete(sa->arg2));
#endif
}


static struct sysret dispatcher_dump_ptables(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(to->type == ObjType_Dispatcher);
    assert(2 == argc);

    printf("kernel_dump_ptables\n");

    struct dcb *dispatcher = to->u.dispatcher.dcb;

    paging_dump_tables(dispatcher);

    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_idcap_identify(struct capability *to,
                                           arch_registers_state_t *context,
                                           int argc)
{
    assert(to->type == ObjType_ID);
    assert(3 == argc);

    struct registers_arm_syscall_args* sa = &context->syscall_args;
    idcap_id_t *idp = (idcap_id_t *) sa->arg2;

    // Check validity of user space pointer
    if (!access_ok(ACCESS_WRITE, (lvaddr_t) idp, sizeof(*idp)))  {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    return sys_idcap_identify(to, idp);
}


typedef struct sysret (*invocation_t)(struct capability*, arch_registers_state_t*, int);

static invocation_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup]       = handle_dispatcher_setup,
        [DispatcherCmd_Properties]  = handle_dispatcher_properties,
        [DispatcherCmd_PerfMon]     = handle_dispatcher_perfmon,
        [DispatcherCmd_DumpPTables] = dispatcher_dump_ptables,
    },
    [ObjType_Frame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
    },
    [ObjType_DevFrame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
    },
    [ObjType_CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
        [CNodeCmd_Create] = handle_create,
    },
    [ObjType_VNode_ARM_l1] = {
    	[VNodeCmd_Map]   = handle_map,
    	[VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_ARM_l2] = {
    	[VNodeCmd_Map]   = handle_map,
    	[VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_IRQTable] = {
            [IRQTableCmd_Set] = handle_irq_table_set,
            [IRQTableCmd_Delete] = handle_irq_table_delete,
        },
    [ObjType_Kernel] = {
        [KernelCmd_Get_core_id]  = monitor_get_core_id,
        [KernelCmd_Get_arch_id]  = monitor_get_arch_id,
        [KernelCmd_Register]     = monitor_handle_register,
        [KernelCmd_Create_cap]   = monitor_create_cap,
        [KernelCmd_Remote_cap]   = monitor_remote_cap,
        [KernelCmd_Spawn_core]   = monitor_spawn_core,
        [KernelCmd_Identify_cap] = monitor_identify_cap,
    },
    [ObjType_ID] = {
        [IDCmd_Identify] = handle_idcap_identify
    }
};

static struct sysret
handle_invoke(arch_registers_state_t *context, int argc)
{
    struct registers_arm_syscall_args* sa = &context->syscall_args;

    //
    // Must match lib/barrelfish/include/arch/arm/arch/invocations.h
    //
    uint8_t  flags       = (sa->arg0 >> 24) & 0xf;
    uint8_t  invoke_bits = (sa->arg0 >> 16) & 0xff;
    capaddr_t  invoke_cptr = sa->arg1;

    debug(SUBSYS_SYSCALL, "sys_invoke(0x%"PRIxCADDR"(%d))\n",
                invoke_cptr, invoke_bits);

    struct sysret r = { .error = SYS_ERR_OK, .value = 0 };

    struct capability* to;
    r.error = caps_lookup_cap(&dcb_current->cspace.cap,
                              invoke_cptr, invoke_bits,
                              &to, CAPRIGHTS_READ);
    if (err_is_ok(r.error))
    {
        assert(to != NULL);
        assert(to->type < ObjType_Num);

        if (ObjType_EndPoint == to->type)
        {
            struct dcb *listener = to->u.endpoint.listener;
            assert(listener != NULL);

            if (listener->disp) {
                uint8_t length_words = (sa->arg0 >> 28) & 0xff;
                uint8_t send_bits = (sa->arg0 >> 8) & 0xff;
                capaddr_t send_cptr = sa->arg2;
                /* limit length of message from buggy/malicious sender */
                length_words = min(length_words, LMP_MSG_LENGTH);

                // does the sender want to yield their timeslice on success?
                bool sync = flags & LMP_FLAG_SYNC;
                // does the sender want to yield to the target
                // if undeliverable?
                bool yield = flags & LMP_FLAG_YIELD;

                // Message registers in context are
                // discontinguous for now so copy message words
                // to temporary container. This is fixable, but
                // not in this pass.
                uintptr_t msg_words[LMP_MSG_LENGTH];
                msg_words[0] = sa->arg3;
                msg_words[1] = sa->arg4;
                msg_words[2] = sa->arg5;
                msg_words[3] = sa->arg6;
                msg_words[4] = sa->arg7;
                msg_words[5] = sa->arg8;
                msg_words[6] = sa->arg9;
                msg_words[7] = sa->arg10;
                msg_words[8] = sa->arg11;
                STATIC_ASSERT(LMP_MSG_LENGTH == 9, "Oops");

                // try to deliver message
                r.error = lmp_deliver(to, dcb_current, msg_words,
                                      length_words, send_cptr, send_bits);

                /* Switch to reciever upon successful delivery
                 * with sync flag, or (some cases of)
                 * unsuccessful delivery with yield flag */
                enum err_code err_code = err_no(r.error);
                if ((sync && err_is_ok(r.error)) ||
                    (yield && (err_code == SYS_ERR_LMP_BUF_OVERFLOW
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED))
                   ) {
                    if (err_is_fail(r.error)) {
                        struct dispatcher_shared_generic *current_disp =
                            get_dispatcher_shared_generic(dcb_current->disp);
                        struct dispatcher_shared_generic *listener_disp =
                            get_dispatcher_shared_generic(listener->disp);
                        debug(SUBSYS_DISPATCH, "LMP failed; %.*s yields to %.*s: %u\n",
                              DISP_NAME_LEN, current_disp->name,
                              DISP_NAME_LEN, listener_disp->name, err_code);
                    }

                    // special-case context switch: ensure correct state in current DCB
                    dispatcher_handle_t handle = dcb_current->disp;
                    struct dispatcher_shared_arm *disp =
                        get_dispatcher_shared_arm(handle);
                    dcb_current->disabled = dispatcher_is_disabled_ip(handle, context->named.pc);
                    if (dcb_current->disabled) {
                        assert(context == &disp->disabled_save_area);
                        context->named.r0 = r.error;
                    }
                    else {
                        assert(context == &disp->enabled_save_area);
                        context->named.r0 = r.error;
                    }
                    dispatch(listener);
                }
            }
            else {
                r.error = SYS_ERR_LMP_NO_TARGET;
            }
        }
        else
        {
            uint8_t cmd = (sa->arg0 >> 8)  & 0xff;
            if (cmd < CAP_MAX_CMD)
            {
                invocation_t invocation = invocations[to->type][cmd];
                if (invocation)
                {
                    r = invocation(to, context, argc);
                    if (!dcb_current)
                    {
                        // dcb_current was removed, dispatch someone else
                        assert(err_is_ok(r.error));
                        dispatch(schedule());
                    }
                    return r;
                }
            }
            printk(LOG_ERR, "Bad invocation type %d cmd %d\n", to->type, cmd);
            r.error = SYS_ERR_ILLEGAL_INVOCATION;
        }
    }

    return r;
}

static struct sysret handle_debug_syscall(int msg)
{
    struct sysret retval = { .error = SYS_ERR_OK };
    switch (msg) {
        case DEBUG_CONTEXT_COUNTER_RESET:
            dispatch_csc_reset();
            break;

        case DEBUG_CONTEXT_COUNTER_READ:
            retval.value = dispatch_get_csc();
            break;

        case DEBUG_TIMESLICE_COUNTER_READ:
            retval.value = kernel_now;
            break;

        case DEBUG_HARDWARE_TIMER_READ:
            retval.value = tsc_read();
            break;

        case DEBUG_HARDWARE_TIMER_HERTZ_READ:
            retval.value = tsc_get_hz();
            break;

        default:
            printk(LOG_ERR, "invalid sys_debug msg type %d\n", msg);
            retval.error = err_push(retval.error, SYS_ERR_ILLEGAL_SYSCALL);
    }
    return retval;
}

/**
 * System call dispatch routine.
 *
 * @return struct sysret for all calls except yield / invoke.
 */
//__attribute__((noreturn))
void sys_syscall(arch_registers_state_t* context)
{
    STATIC_ASSERT_OFFSETOF(struct sysret, error, 0);

    struct registers_arm_syscall_args* sa = &context->syscall_args;

    uintptr_t   syscall = sa->arg0 & 0xf;
    uintptr_t   argc    = (sa->arg0 >> 4) & 0xf;

    struct sysret r = { .error = SYS_ERR_INVARGS_SYSCALL, .value = 0 };

    switch (syscall)
    {
        case SYSCALL_INVOKE:
            r = handle_invoke(context, argc);
            break;

        case SYSCALL_YIELD:
            if (argc == 2)
            {
                r = sys_yield((capaddr_t)sa->arg1);
            }
            break;

        case SYSCALL_NOP:
            break;

        case SYSCALL_PRINT:
            if (argc == 3)
            {
                r.error = sys_print((const char*)sa->arg1, (size_t)sa->arg2);
            }
            break;

        case SYSCALL_DEBUG:
            if (argc == 2) {
                r = handle_debug_syscall(sa->arg1);
            }
            break;
            
#ifdef  __ARM_ARCH_7M__
    //help the dispatcher resume a context that can not be restored whithout a mode change
        case SYSCALL_RESUME_CONTEXT:
            if (argc == 2)
                r.error = sys_resume_context((arch_registers_state_t*) sa->arg1);
            break;
#endif  //__ARM_ARCH_7M__

        default:
            panic("Illegal syscall");
            r.error = SYS_ERR_ILLEGAL_SYSCALL;
            break;
    }

    if (r.error) {
        debug(SUBSYS_SYSCALL, "syscall failed %08"PRIx32" => %08"PRIxERRV"\n",
              sa->arg0, r.error);
    }

    context->named.r0 = r.error;
    context->named.r1 = r.value;

    resume(context);
}

#ifdef __ARM_ARCH_7M__    //armv7-m: cortex-m3 on pandaboard
/*
    needed because to resume an interrupted IT block, there literally is only one way:
    exiting handler mode, restoring the context
    if the dispatcher has to restore a context with IT-bits set, it can only do so with help
    from the kernel. 
*/
errval_t __attribute__ ((noreturn)) sys_resume_context(arch_registers_state_t* registers){
    debug(SUBSYS_SYSCALL, "restoring context for dispatcher\n");
    //because we come from a syscall, r9 does not yet point to current dispatcher.
    //the context we restore probably has the right one, exept if it is in systemcall
    //related code (e.g. restoring registers after a syscall)
    
    //we want the correct dispatcher, because we want to set disabled = 0
    //we can not do that in the calling code, because then the act of calling us
    //would set the enabled area (i.e. it could be restored which we don't want)
    struct dispatcher_shared_generic *disp_gen
        = get_dispatcher_shared_generic(dcb_current->disp);//find the correct current dispatcher
    
    
    //resume looks at our value of the rtls register, so we need to set it
    arch_set_thread_register(disp_gen->udisp);
    
    //set dispatcher->disabled = 0, because the resume code would also do that
    disp_gen->disabled = 0;
    
    //((struct dispatcher_shared_generic*) registers->named.rtls)->disabled = 0;

    resume(registers);
}
#endif //__ARM_ARCH_7M__

