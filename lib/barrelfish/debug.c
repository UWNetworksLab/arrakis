/**
 * \file
 * \brief Debugging functions
 */

/*
 * Copyright (c) 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish_kpi/dispatcher_shared.h>

#define DISP_MEMORY_SIZE            1024 // size of memory dump in bytes

/**
 * \brief Print a message and abort.
 *
 * Something irrecoverably bad happened. Print a panic message, then abort.
 */
void user_panic_fn(const char *file, const char *func, int line,
                   const char *msg, ...)
{
    va_list ap;
    char msg_str[128];
    //int msg_str_cc;
    va_start(ap, msg);
    //msg_str_cc =
        vsnprintf(msg_str, sizeof(msg_str), msg, ap);
    va_end(ap);

    char str[256];
    //int strcc =
        snprintf(str, sizeof(str), "%.*s.%u in %s() %s:%d\n%s\n",
                     DISP_NAME_LEN, disp_name(), disp_get_core_id(),
                     func, file, line, msg_str);
    sys_print(str, sizeof(str));

    abort();
}

errval_t debug_cap_identify(struct capref cap, struct capability *ret)
{
    errval_t err, msgerr;

    if (get_cap_addr(cap) == 0) {
        return SYS_ERR_CAP_NOT_FOUND;
    }

    union {
        monitor_blocking_caprep_t caprep;
        struct capability capability;
    } u;

    struct monitor_blocking_rpc_client *r = get_monitor_blocking_rpc_client();
    err = r->vtbl.cap_identify(r, cap, &msgerr, &u.caprep);
    if (err_is_fail(err)){
        return err;
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }

    assert(ret != NULL);
    *ret = u.capability;

    return msgerr;
}

/**
 * \brief Dump own hw page tables
 */
errval_t debug_dump_hw_ptables(void)
{
    return invoke_dispatcher_dump_ptables(cap_dispatcher);
}

void debug_printf(const char *fmt, ...)
{
    va_list argptr;
    char str[256];
    size_t len;

    /* len = snprintf(str, sizeof(str), "\033[34m%.*s.\033[31m%u\033[0m: ", DISP_NAME_LEN, disp_name(), */
    /*                disp_get_core_id()); */
    len = snprintf(str, sizeof(str), "%.*s.%u: ", DISP_NAME_LEN, disp_name(),
                   disp_get_core_id());
    if (len < sizeof(str)) {
        va_start(argptr, fmt);
        vsnprintf(str + len, sizeof(str) - len, fmt, argptr);
        va_end(argptr);
    }
    sys_print(str, sizeof(str));
}

/**
 * \brief Function to do the actual printing based on the type of capability
 */
int debug_print_cap(char *buf, size_t len, struct capability *cap)
{
    switch (cap->type) {
    case ObjType_PhysAddr:
        return snprintf(buf, len,
                        "physical address range cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.physaddr.base, cap->u.physaddr.bits);

    case ObjType_RAM:
        return snprintf(buf, len, "RAM cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.ram.base, cap->u.ram.bits);

    case ObjType_CNode: {
        int ret = snprintf(buf, len, "CNode cap "
                           "(bits %u, rights mask 0x%" PRIxCAPRIGHTS ")",
                           cap->u.cnode.bits, cap->u.cnode.rightsmask);
        if (cap->u.cnode.guard_size != 0 && ret < len) {
            ret += snprintf(&buf[ret], len - ret, " (guard 0x%" PRIxCADDR ":%u)",
                            cap->u.cnode.guard, cap->u.cnode.guard_size);
        }
        return ret;
    }

    case ObjType_Dispatcher:
        return snprintf(buf, len, "Dispatcher cap %p", cap->u.dispatcher.dcb);

    case ObjType_Frame:
        return snprintf(buf, len, "Frame cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.frame.base, cap->u.frame.bits);

    case ObjType_DevFrame:
        return snprintf(buf, len, "Device Frame cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.frame.base, cap->u.devframe.bits);

    case ObjType_VNode_ARM_l1:
        return snprintf(buf, len, "ARM L1 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l1.base);

    case ObjType_VNode_ARM_l2:
        return snprintf(buf, len, "ARM L2 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l2.base);

    case ObjType_VNode_x86_32_ptable:
        return snprintf(buf, len, "x86_32 Page table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_ptable.base);

    case ObjType_VNode_x86_32_pdir:
        return snprintf(buf, len, "x86_32 Page directory at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_pdir.base);

    case ObjType_VNode_x86_32_pdpt:
        return snprintf(buf, len, "x86_32 PDPT at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_pdpt.base);

    case ObjType_VNode_x86_64_ptable:
        return snprintf(buf, len, "x86_64 Page table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ptable.base);

    case ObjType_VNode_x86_64_pdir:
        return snprintf(buf, len, "x86_64 Page directory at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pdir.base);

    case ObjType_VNode_x86_64_pdpt:
        return snprintf(buf, len, "x86_64 PDPT at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pdpt.base);

    case ObjType_VNode_x86_64_pml4:
        return snprintf(buf, len, "x86_64 PML4 at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pml4.base);

    case ObjType_IRQTable:
        return snprintf(buf, len, "IRQTable cap");

    case ObjType_EndPoint:
        return snprintf(buf, len, "EndPoint cap (disp %p offset 0x%" PRIxLVADDR ")",
                        cap->u.endpoint.listener, cap->u.endpoint.epoffset);

    case ObjType_IO:
        return snprintf(buf, len, "IO cap (0x%hx-0x%hx)",
                        cap->u.io.start, cap->u.io.end);

    case ObjType_Kernel:
        return snprintf(buf, len, "Kernel cap");

    case ObjType_ID:
        return snprintf(buf, len, "ID capability (coreid 0x%" PRIxCOREID
                        " core_local_id 0x%" PRIx32 ")", cap->u.id.coreid,
                        cap->u.id.core_local_id);

    default:
        return snprintf(buf, len, "UNKNOWN TYPE! (%d)", cap->type);
    }
}

int debug_print_cap_at_capref(char *buf, size_t len, struct capref cap)
{
    struct capability capability;
    errval_t err;

    if (capref_is_null(cap)) {
        return snprintf(buf, len, "(null cap)");
    }

    err = debug_cap_identify(cap, &capability);
    if (err_is_fail(err)) {
        return snprintf(buf, len, "(ERROR identifying cap!)");
    } else {
        return debug_print_cap(buf, len, &capability);
    }
}

/**
 * \brief Walk the cspace printing all non-null capabilities
 *
 * \param cnode         cnode to walk
 * \param level         depth in the cspace
 *
 * \bug assumes guards are always zero
 */
static void walk_cspace(struct cnoderef cnode, uint8_t level)
{
    struct capability cap;
    errval_t err;

    struct capref pos = {
        .cnode = cnode, .slot = 0
    };

    // If too many bits resolved, return
    if (pos.cnode.address_bits + pos.cnode.guard_size + pos.cnode.size_bits
        > CPTR_BITS) {
        return;
    }

    // Walk through all the slots in the CNode
    for (pos.slot = 0; pos.slot < (((capaddr_t)1) << cnode.size_bits); pos.slot++) {
        // Get cap data
        err = debug_cap_identify(pos, &cap);

        // If cap type was Null, kernel returns error
        if (err_no(err) == SYS_ERR_IDENTIFY_LOOKUP ||
            err_no(err) == SYS_ERR_CAP_NOT_FOUND ||
            err_no(err) == SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP) {
            continue;
        } else if (err_is_fail(err)) {
            DEBUG_ERR(err, "debug_cap_identify failed");
            return;
        }

        char buf[256];
        size_t prpos = 0;

        // Print the stats for the child slot
        for(int i = 0; i < level; i++) {
            prpos += snprintf(&buf[prpos], sizeof(buf) - prpos, "  ");
            assert(prpos < sizeof(buf));
        }
        prpos += snprintf(&buf[prpos], sizeof(buf) - prpos,
                          "slot %" PRIuCADDR " caddr 0x%" PRIxCADDR " (%u bits) is a ",
                          pos.slot, get_cap_addr(pos), get_cap_valid_bits(pos));
        assert(prpos < sizeof(buf));
        prpos += debug_print_cap(&buf[prpos], sizeof(buf) - prpos, &cap);
        assert(prpos < sizeof(buf));
        debug_printf("%s\n", buf);

        // If CNode type, descend into it
        if (cap.type == ObjType_CNode) {
            struct cnoderef childcn = {
                .address = get_cap_addr(pos),
                .address_bits = get_cap_valid_bits(pos),
                .size_bits = cap.u.cnode.bits,
                .guard_size = cap.u.cnode.guard_size,
            };
            walk_cspace(childcn, level + 1);
        }
    }
}

/**
 * \brief Dump an arbitrary cspace, given the root
 */
void debug_cspace(struct capref root)
{
    struct capability cap;

    /* find out size of root cnode */
    errval_t err = debug_cap_identify(root, &cap);
    assert(err_is_ok(err));

    struct cnoderef cnode = {
        .address = get_cap_addr(root),
        .address_bits = get_cap_valid_bits(root),
        .size_bits = cap.u.cnode.bits,
        .guard_size = cap.u.cnode.guard_size,
    };

    walk_cspace(cnode, 0);
}

void debug_my_cspace(void)
{
    // XXX: Assume my root CNode has a size of #DEFAULT_CNODE_BITS
    struct cnoderef cnode = {
        .address = 0,
        .address_bits = 0,
        .size_bits = DEFAULT_CNODE_BITS,
        .guard_size = 0,
    };

    walk_cspace(cnode, 0);
}

int debug_print_capref(char *buf, size_t len, struct capref cap)
{
    return snprintf(buf, len, "CNode addr 0x%" PRIxCADDR
                              ", vbits = %d, slot %" PRIuCADDR ", vbits = %d",
                    get_cnode_addr(cap),  get_cnode_valid_bits(cap), cap.slot,
                    get_cap_valid_bits(cap));
}

void debug_dump_mem(lvaddr_t start_addr, lvaddr_t end_addr, lvaddr_t point)
{
    debug_printf("Dumping memory in range 0x%" PRIxLVADDR
                 " to 0x%" PRIxLVADDR ":\n",
                 start_addr, end_addr);

    for (uintptr_t *p = (void *)start_addr; (uintptr_t)p < end_addr; p++) {
        uint8_t *bytes = (void *)p;
        char buf[32];
        size_t bufpos = 0;
        for (int i = 0; i < sizeof(uintptr_t); i++) {
            bufpos += snprintf(&buf[bufpos], sizeof(buf) - bufpos, "%02x ", bytes[i]);
            assert(bufpos < sizeof(buf));
        }
        debug_printf("%p: %.*s %*" PRIxPTR "%s\n", p, (int)sizeof(buf), buf,
                     (int)sizeof(uintptr_t) * 2, *p,
                     p == (uintptr_t *)point ? " <== We are here" : "");
    }
}

void debug_dump_mem_around_addr(lvaddr_t addr)
{
    /* lvaddr_t page_aligned_addr = ROUND_DOWN(addr, BASE_PAGE_SIZE); */
    lvaddr_t start_addr = ROUND_DOWN(addr - DISP_MEMORY_SIZE/2, sizeof(uintptr_t));
    lvaddr_t end_addr = ROUND_UP(addr + DISP_MEMORY_SIZE/2, sizeof(uintptr_t));

    /* if (start_addr < page_aligned_addr) { */
    /*     start_addr = page_aligned_addr; */
    /* } */
    /* if (end_addr > page_aligned_addr + BASE_PAGE_SIZE) { */
    /*     end_addr = page_aligned_addr + BASE_PAGE_SIZE; */
    /* } */

    debug_dump_mem(start_addr, end_addr, addr);
}

void debug_err(const char *file, const char *func, int line, errval_t err,
               const char *msg, ...)
{
    va_list ap;

    char str[256];
    char *leader = (err == 0) ? "SUCCESS" : "ERROR";
    //int strcc =
        snprintf(str, sizeof(str), "%s: %.*s.%u in %s() %s:%d\n%s: ",
                     leader, DISP_NAME_LEN, disp_name(), disp_get_core_id(),
                     func, file, line, leader);
    sys_print(str, sizeof(str));

    if (msg != NULL) {
        va_start(ap, msg);
        //int strcc2 =
            vsnprintf(str, sizeof(str), msg, ap);
        va_end(ap);
        sys_print(str, sizeof(str));
    }
    sys_print("\n", 1);

    if (err != 0) {
        err_print_calltrace(err);
    }
}

bool debug_notify_syscall = false;

void debug_control_plane_forbidden(void);
void debug_control_plane_forbidden(void)
{
    debug_notify_syscall = true;
}
