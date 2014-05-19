/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef GUEST_H
#define GUEST_H

#include <barrelfish/barrelfish.h>
#include "amd_vmcb_dev.h"

struct guest {
    // indicates whether the guest is runnable atm or waiting
    bool                    runnable;
    // Monitor endpoint for this guest
    struct lmp_endpoint     *monitor_ep;
    // The allocator for the slot this guests uses
    struct multi_slot_allocator slot_alloc;
    // VMCB data
    struct capref           vmcb_cap;
    lpaddr_t                 vmcb_pa;
    lvaddr_t                 vmcb_va;
    // guest control data
    struct capref           ctrl_cap;
    struct guest_control    *ctrl;
    // IOPM data (IO port access)
    struct capref           iopm_cap;
    lpaddr_t                 iopm_pa;
    lvaddr_t                 iopm_va;
    // MSRPM data (MSR access)
    struct capref           msrpm_cap;
    lpaddr_t                 msrpm_pa;
    lvaddr_t                 msrpm_va;
    // Mackerel data structure to VMCV
    amd_vmcb_t              vmcb;
    // Guest dispatcher
    struct capref           dcb_cap;
    // Guests physical address space (virtual to the domain)
    struct vspace           vspace;
    // Guest physical memory
    lvaddr_t                 mem_low_va;
    lvaddr_t                 mem_high_va;
    // indicates whether the guest was in emulation before the exit
    bool                    emulated_before_exit;
    // virtual hardware
    struct hdd              *hdds[8];
    size_t                  hdd_count;
    struct console          *console;
    struct pc16550d         *serial_ports[4];
    size_t                  serial_port_count;
    struct apic             *apic;
    struct lpc              *lpc;
    struct pci              *pci;
    // some settings which belong to an upcomming CPU abstraction
    bool                    a20_gate_enabled;
};

/**
 * \brief This enum is used to indicate the size of operands to some operations.
 */
enum opsize {
    OPSIZE_8,
    OPSIZE_16,
    OPSIZE_32,
    OPSIZE_64
};

extern lvaddr_t guest_offset;
static inline lvaddr_t
        host_to_guest (lvaddr_t addr)
{
    return addr - guest_offset;
}

static inline lvaddr_t
        guest_to_host (lvaddr_t addr)
{
    return addr + guest_offset;
}

// REGISTER ACCESS HELPERS

// RAX

static inline uint64_t
guest_get_rax (struct guest *g)
{
    return amd_vmcb_rax_rd(&g->vmcb);
}

static inline void
guest_set_rax (struct guest *g, uint64_t val)
{
    amd_vmcb_rax_wr(&g->vmcb, val);
}

static inline uint32_t
guest_get_eax (struct guest *g)
{
    return amd_vmcb_rax_rd(&g->vmcb) & 0xffffffff;
}

static inline void
guest_set_eax (struct guest *g, uint32_t val)
{
    uint64_t buf = amd_vmcb_rax_rd(&g->vmcb);
    buf = (buf & ~0xffffffff) | val;
    amd_vmcb_rax_wr(&g->vmcb, buf);
}

static inline uint16_t
guest_get_ax (struct guest *g)
{
    return amd_vmcb_rax_rd(&g->vmcb) & 0xffff;
}

static inline void
guest_set_ax (struct guest *g, uint16_t val)
{
    uint64_t buf = amd_vmcb_rax_rd(&g->vmcb);
    buf = (buf & ~0xffff) | val;
    amd_vmcb_rax_wr(&g->vmcb, buf);
}

static inline uint8_t
guest_get_ah (struct guest *g)
{
    return amd_vmcb_rax_rd(&g->vmcb) >> 8;
}

static inline void
guest_set_ah (struct guest *g, uint8_t val)
{
    uint64_t buf = amd_vmcb_rax_rd(&g->vmcb);
    buf = (buf & ~0xff00) | ((uint64_t)val) << 8;
    amd_vmcb_rax_wr(&g->vmcb, buf);
}

static inline uint8_t
guest_get_al (struct guest *g)
{
    return amd_vmcb_rax_rd(&g->vmcb);
}

static inline void
guest_set_al (struct guest *g, uint8_t val)
{
    uint64_t buf = amd_vmcb_rax_rd(&g->vmcb);
    buf = (buf & ~0xff) | val;
    amd_vmcb_rax_wr(&g->vmcb, buf);
}


// RBX

static inline uint64_t
guest_get_rbx (struct guest *g)
{
    return g->ctrl->regs.rbx;
}

static inline void
guest_set_rbx (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rbx = val;
}

static inline uint32_t
guest_get_ebx (struct guest *g) {
    return g->ctrl->regs.rbx;
}

static inline void
guest_set_ebx (struct guest *g, uint32_t val) {
    uint64_t buf = g->ctrl->regs.rbx;
    g->ctrl->regs.rbx = (buf & ~0xffffffff) | val;
}

static inline uint16_t
guest_get_bx (struct guest *g)
{
    return g->ctrl->regs.rbx & 0xffff;
}

static inline void
guest_set_bx (struct guest *g, uint16_t val)
{
    uint64_t buf = g->ctrl->regs.rbx;
    g->ctrl->regs.rbx = (buf & ~0xffff) | val;
}

static inline uint8_t
guest_get_bl (struct guest *g)
{
    return g->ctrl->regs.rbx & 0xff;
}

static inline void
guest_set_bl (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rbx;
    g->ctrl->regs.rbx = (buf & ~0xff) | val;
}

static inline uint8_t
guest_get_bh (struct guest *g)
{
    return g->ctrl->regs.rbx >> 8;
}

static inline void
guest_set_bh (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rbx;
    g->ctrl->regs.rbx = (buf & ~0xff00) | ((uint64_t)val) << 8;
}


// RCX

static inline uint64_t
guest_get_rcx (struct guest *g)
{
    return g->ctrl->regs.rcx;
}

static inline void
guest_set_rcx (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rcx = val;
}

static inline uint32_t
guest_get_ecx (struct guest *g) {
    return g->ctrl->regs.rcx;
}

static inline void
guest_set_ecx (struct guest *g, uint32_t val) {
    uint64_t buf = g->ctrl->regs.rcx;
    g->ctrl->regs.rcx = (buf & ~0xffffffff) | val;
}

static inline uint16_t
guest_get_cx (struct guest *g)
{
    return g->ctrl->regs.rcx & 0xffff;
}

static inline void
guest_set_cx (struct guest *g, uint16_t val)
{
    uint64_t buf = g->ctrl->regs.rcx;
    g->ctrl->regs.rcx = (buf & ~0xffff) | val;
}

static inline uint8_t
guest_get_ch (struct guest *g)
{
    return g->ctrl->regs.rcx >> 8;
}

static inline void
guest_set_ch (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rcx;
    g->ctrl->regs.rcx = (buf & ~0xff00) | ((uint64_t)val) << 8;
}

static inline uint8_t
guest_get_cl (struct guest *g)
{
    return g->ctrl->regs.rcx;
}

static inline void
guest_set_cl (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rcx;
    buf = (buf & ~0xff) | val;
    g->ctrl->regs.rcx = val;
}


// RDX

static inline uint64_t
guest_get_rdx (struct guest *g)
{
    return g->ctrl->regs.rdx;
}

static inline void
guest_set_rdx (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rdx = val;
}

static inline uint32_t
guest_get_edx (struct guest *g) {
    return g->ctrl->regs.rdx;
}

static inline void
guest_set_edx (struct guest *g, uint32_t val) {
    uint64_t buf = g->ctrl->regs.rdx;
    g->ctrl->regs.rdx = (buf & ~0xffffffff) | val;
}

static inline uint8_t
guest_get_dh (struct guest *g)
{
    return g->ctrl->regs.rdx >> 8;
}

static inline void
guest_set_dh (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rdx;
    g->ctrl->regs.rdx = (buf & ~0xff00) | ((uint64_t)val) << 8;
}

static inline uint8_t
guest_get_dl (struct guest *g)
{
    return g->ctrl->regs.rdx & 0xff;
}

static inline void
guest_set_dl (struct guest *g, uint8_t val)
{
    uint64_t buf = g->ctrl->regs.rdx;
    g->ctrl->regs.rdx = (buf & ~0xff) | val;
}


// RDI

static inline uint64_t
guest_get_rdi (struct guest *g)
{
    return g->ctrl->regs.rdi;
}

static inline void
guest_set_rdi (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rdi = val;
}

static inline uint16_t
guest_get_di (struct guest *g)
{
    return g->ctrl->regs.rdi & 0xffff;
}

static inline void
guest_set_di (struct guest *g, uint16_t val)
{
    uint64_t buf = g->ctrl->regs.rdi;
    g->ctrl->regs.rdi = (buf & ~0xffff) | val;
}


// RSI

static inline uint64_t
guest_get_rsi (struct guest *g)
{
    return g->ctrl->regs.rsi;
}

static inline void
guest_set_rsi (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rsi = val;
}

static inline uint16_t
guest_get_si (struct guest *g)
{
    return g->ctrl->regs.rsi & 0xffff;
}

static inline void
guest_set_si (struct guest *g, uint16_t val)
{
    uint64_t buf = g->ctrl->regs.rsi;
    g->ctrl->regs.rsi = (buf & ~0xffff) | val;
}


// RSP

static inline uint64_t
guest_get_rsp (struct guest *g)
{
    return amd_vmcb_rsp_rd(&g->vmcb);
}

static inline void
guest_set_rsp (struct guest *g, uint64_t val)
{
    amd_vmcb_rsp_wr(&g->vmcb, val);
}


// RBP

static inline uint64_t
guest_get_rbp (struct guest *g)
{
    return g->ctrl->regs.rbp;
}

static inline void
guest_set_rbp (struct guest *g, uint64_t val)
{
    g->ctrl->regs.rbp = val;
}


struct guest *guest_create (void);
errval_t guest_make_runnable (struct guest *g, bool run);
void guest_handle_vmexit (struct guest *g);

errval_t guest_vspace_map_wrapper(struct vspace *vspace, lvaddr_t vaddr,
                                  struct capref frame,  size_t size);

errval_t
alloc_guest_mem(struct guest *g, lvaddr_t guest_paddr, size_t bytes);

#endif // GUEST_H
