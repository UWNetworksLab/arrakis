/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INVOCATIONS_H
#define INVOCATIONS_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>

/**
 * \brief Spawn a new core.
 *
 * \param cur_kern   Cap of the current kernel
 * \param core_id    APIC ID of the core to try booting
 * \param sp_mem     Cap to Ram type memory to relocate the new kernel
 * \param dcb        Cap to the dcb of the user program to run on the new kernel
 * \param root_vbits Number of valid bits in root_cptr
 * \param root_cptr  Cap to the root of cspace of the new user program
 * \param vtree      Cap to the vtree root of the new user program
 * \param dispatcher Cap to the dispatcher of the new user program
 * \param entry      Kernel entry point in physical memory
 */
//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                          forvaddr_t entry)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall6((invoke_bits << 16) | (KernelCmd_Spawn_core << 8)
                    | SYSCALL_INVOKE, invoke_cptr, core_id, cpu_type,
                    (uintptr_t)(entry >> 32), (uintptr_t) entry).error;
}

static inline errval_t
invoke_monitor_identify_cap(capaddr_t cap, int bits, struct capability *out)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall5((invoke_bits << 16) | (KernelCmd_Identify_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, cap, bits,
                    (uintptr_t)out).error;
}

static inline errval_t
invoke_monitor_identify_domains_cap(capaddr_t root_cap, int root_bits,
                                    capaddr_t cap, int bits,
                                    struct capability *out)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall7((invoke_bits << 16) | (KernelCmd_Identify_domains_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, root_cap, root_bits,
                    cap, bits, (uintptr_t)out).error;
}

static inline errval_t
invoke_monitor_nullify_cap(capaddr_t cap, int bits)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_Nullify_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, cap, bits).error;
}

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int bits, capaddr_t slot)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall6((invoke_bits << 16) | (KernelCmd_Create_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, caddr, bits, slot,
                    (uintptr_t)raw).error;
}

static inline errval_t
invoke_monitor_cap_remote(capaddr_t cap, int bits, bool is_remote,
                          bool * has_descendents)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    struct sysret r;
    r = syscall5((invoke_bits << 16) | (KernelCmd_Remote_cap << 8)
                 | SYSCALL_INVOKE, invoke_cptr, cap, bits, is_remote);
    if (err_is_ok(r.error)) {
        *has_descendents = r.value;
    }
    return r.error;
}

static inline errval_t
invoke_monitor_register(struct capref ep)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_Register << 8)
                    | SYSCALL_INVOKE, invoke_cptr, get_cap_addr(ep)).error;
}

static inline errval_t
invoke_monitor_identify_cnode_get_cap(uint64_t *cnode_raw, capaddr_t slot,
                                      struct capability *out)
{
    USER_PANIC("NYI");
    assert(cnode_raw != NULL);
    assert(out != NULL);
    return LIB_ERR_NOT_IMPLEMENTED;

#if 0
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Iden_cnode_get_cap);
    for (int i = 0; i < sizeof(struct capability) / sizeof(uint64_t); i++) {
        idc_msg_encode_word(&msg, cnode_raw[i]);
    }
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, (uintptr_t)out);

    return cap_invoke(cap_kernel, &msg);
#endif
}

static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, enum objtype newtype,
                                 int objbits, capaddr_t to, capaddr_t slot,
                                 int bits)
{
    assert(src != CPTR_NULL);
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t
invoke_monitor_remote_cap_delete(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, int bits) {
    assert(src != CPTR_NULL);

    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall6(invoke_bits << 16 | (MonitorCmd_Delete << 8)
                    | SYSCALL_INVOKE, invoke_cptr, rootcap_addr,
                    rootcap_vbits, src, bits).error;
}

static inline errval_t
invoke_monitor_remote_cap_revoke(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, int bits) {
    assert(src != CPTR_NULL);

    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall6(invoke_bits << 16 | (MonitorCmd_Revoke << 8)
                    | SYSCALL_INVOKE, invoke_cptr, rootcap_addr,
                    rootcap_vbits, src, bits).error;
}

/**
 * \brief Set up tracing in the kernel
 *
 */
static inline errval_t
invoke_trace_setup(struct capref cap)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t
invoke_domain_id(struct capref cap, uint64_t domain_id)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t
invoke_monitor_rck_register(struct capref kern_cap, struct capref ep,
                            int chanid)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t
invoke_monitor_rck_delete(struct capref kern_cap, int chanid)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t invoke_monitor_sync_timer(uint64_t synctime)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_Sync_timer << 8)
                    | SYSCALL_INVOKE, invoke_cptr, synctime >> 32,
                    synctime & 0xffffffff).error;
}

static inline errval_t
invoke_monitor_get_arch_id(uintptr_t *arch_id)
{
    assert(arch_id != NULL);

    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    struct sysret sysret;
    sysret = syscall2((invoke_bits << 16) | (KernelCmd_Get_arch_id << 8)
                      | SYSCALL_INVOKE, invoke_cptr);
    if (err_is_ok(sysret.error)) {
        *arch_id = sysret.value;
    }
    return sysret.error;
}

static inline errval_t
invoke_monitor_ipi_register(struct capref ep, int chanid)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_IPI_Register << 8)
                    | SYSCALL_INVOKE, invoke_cptr,
                    get_cap_addr(ep),
                    chanid).error;
}

static inline errval_t
invoke_monitor_ipi_delete(int chanid)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_IPI_Delete << 8)
                    | SYSCALL_INVOKE, invoke_cptr,
                    chanid).error;
}

#endif
