/**
 * \file
 * \brief Local descriptor table (LDT) management
 */

/*
 * Copyright (c) 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/syscalls.h>
#include <barrelfish/ldt.h>
#include <arch/ldt.h>
#include <target/x86_64/barrelfish_kpi/cpu_target.h> // segment_descriptor
#include <barrelfish_kpi/cpu_arch.h> // segment_descriptor
#include <stdio.h>
#include <string.h>

#define LDT_NENTRIES    512     ///< Number of entries in the statically-sized LDT

/// Local segment descriptor table. Shared by all dispatchers in an address space.
// (XXX: coherence assumption)
static union segment_descriptor ldt[LDT_NENTRIES];

/// Spinlock protecting LDT in spanned domains
// (XXX: coherence assumption)
static spinlock_t ldt_spinlock;

#ifdef ARRAKIS

/*
 * AMD64 Segmentation Data Structures and definitions
 */

/*
 * Selectors
 */

#define SEL_RPL_MASK    3       /* requester priv level */
#define ISPL(s) ((s)&3)         /* what is the priority level of a selector */
#define SEL_KPL 0               /* kernel priority level */
#define SEL_UPL 3               /* user priority level */
#define ISLDT(s)        ((s)&SEL_LDT)   /* is it local or global */
#define SEL_LDT 4               /* local descriptor table */
#define IDXSEL(s)       (((s)>>3) & 0x1fff)             /* index of selector */
#define LSEL(s,r)       (((s)<<3) | SEL_LDT | r)        /* a local selector */
#define GSEL(s,r)       (((s)<<3) | r)                  /* a global selector */

/**
 * Gate descriptors (e.g. indirect descriptors, trap, interrupt etc. 128 bit)
 * Only interrupt and trap gates have gd_ist.
 */
struct  gate_descriptor {
    uint64_t gd_looffset:16;       /* gate offset (lsb) */
    uint64_t gd_selector:16;       /* gate segment selector */
    uint64_t gd_ist:3;             /* IST table index */
    uint64_t gd_xx:5;              /* unused */
    uint64_t gd_type:5;            /* segment type */
    uint64_t gd_dpl:2;             /* segment descriptor priority level */
    uint64_t gd_p:1;               /* segment descriptor present */
    uint64_t gd_hioffset:48;       /* gate offset (msb) */
    uint64_t sd_xx1:32;
} __attribute__((packed));

/* system segments and gate types */
#define SDT_SYSNULL      0      /* system null */
#define SDT_SYSLDT       2      /* system 64 bit local descriptor table */
#define SDT_SYSTSS       9      /* system available 64 bit TSS */
#define SDT_SYSBSY      11      /* system busy 64 bit TSS */
#define SDT_SYSCGT      12      /* system 64 bit call gate */
#define SDT_SYSIGT      14      /* system 64 bit interrupt gate */
#define SDT_SYSTGT      15      /* system 64 bit trap gate */

/* memory segment types */
#define SDT_MEMRO       16      /* memory read only */
#define SDT_MEMROA      17      /* memory read only accessed */
#define SDT_MEMRW       18      /* memory read write */
#define SDT_MEMRWA      19      /* memory read write accessed */
#define SDT_MEMROD      20      /* memory read only expand dwn limit */
#define SDT_MEMRODA     21      /* memory read only expand dwn limit accessed */
#define SDT_MEMRWD      22      /* memory read write expand dwn limit */
#define SDT_MEMRWDA     23      /* memory read write expand dwn limit accessed */
#define SDT_MEME        24      /* memory execute only */
#define SDT_MEMEA       25      /* memory execute only accessed */
#define SDT_MEMER       26      /* memory execute read */
#define SDT_MEMERA      27      /* memory execute read accessed */
#define SDT_MEMEC       28      /* memory execute only conforming */
#define SDT_MEMEAC      29      /* memory execute only accessed conforming */
#define SDT_MEMERC      30      /* memory execute read conforming */
#define SDT_MEMERAC     31      /* memory execute read accessed conforming */

/*
 * Size of IDT table
 */
#define NIDT    256             /* 32 reserved, 16 h/w, 0 s/w, linux's 0x80 */

/*
 * Entries in the Global Descriptor Table (GDT)
 */
#define NULL_SEL        0       /**< Null descriptor */
#define KCODE_SEL       1       /**< Kernel code descriptor */
#define KSTACK_SEL      2       /**< Shared user/kernel stack descriptor */
#define USTACK_SEL      3       /**< User stack descriptor */
#define UCODE_SEL       4       /**< User code descriptor */
#define TSS_LO_SEL      5       /**< Task State Segment (TSS) -- low 64bit */
#define TSS_HI_SEL      6       /**< Task State Segment (TSS) -- high 64bit */
#define LDT_LO_SEL      7       /**< Local descriptor table (LDT) -- low */
#define LDT_HI_SEL      8       /**< Local descriptor table (LDT) -- high */
#define NGDT_MEM        9       /**< Number of descriptors */

/**
 * region descriptors, used to load gdt/idt tables before segments yet exist.
 */
struct region_descriptor {
    uint16_t rd_limit;          /**< segment extent */
    uint64_t rd_base;           /**< base address  */
} __attribute__((packed));

struct task_state_segment {
    uint32_t    reserved;
    uint64_t    rsp[3];
    uint64_t    reserved2;
    uint64_t    ist[7];
    uint64_t    reserved3;
    uint16_t    reserved4;
    uint16_t    iomap_base;
} __attribute__ ((packed));

/**
 * \brief Global Task State Segment (TSS).
 *
 * This is the global, static and only Task State Segment (TSS). It is used
 * for interrupt and exception handling (stack setup) while in user-space.
 */
static struct task_state_segment tss __attribute__ ((aligned (4)));

union segment_descriptor gdt[] __attribute__ ((aligned (4))) = {
    [NULL_SEL] = {   // Null segment
        .raw = 0
    },
    [KCODE_SEL] = {   // Kernel code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [KSTACK_SEL] = {   // Kernel stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [USTACK_SEL] = {   // User stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [UCODE_SEL] = {   // User code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 1,
            .operation_size = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [TSS_LO_SEL] = {   // Global Task State Segment (TSS), lower 8 bytes
        .sys_lo = {
            .lo_limit = sizeof(tss) & 0xffff,
            .type = SDT_SYSTSS,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = (sizeof(tss) >> 16) & 0xf,
            .available = 0,
            .granularity = 0,
        }
    },
    [TSS_HI_SEL] = {   // Global Task State Segment (TSS), upper 8 bytes
        .sys_hi = {
            .base = 0
        }
    },
    [LDT_LO_SEL] = {    // Local descriptor table (LDT), lower 8 bytes
        .sys_lo = {
            .lo_limit = 0, // # 4k pages (since granularity = 1)
            .lo_base = 0, // changed by context switch path when doing lldt
            .type = 2, // LDT
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0,
            .available = 0,
            .granularity = 1,
            .hi_base = 0
        }
    },
    [LDT_HI_SEL] = {    // Local descriptor table (LDT), upper 8 bytes
        .sys_hi = {
            .base = 0 // changed by context switch path when doing lldt
        }
    },
};

static union segment_descriptor *ldt_descriptor = &gdt[LDT_LO_SEL];

/// Remember current LDT pointer, so we can avoid reloading it
static lvaddr_t current_ldt_base = -1;
static size_t current_ldt_npages;

static void maybe_reload_ldt(struct dispatcher_shared_x86_64 *disp, bool force_reload)
{
    /* Read fields from user dispatcher once for consistency */
    lvaddr_t ldt_base = disp->ldt_base;
    size_t ldt_npages = disp->ldt_npages;

    /* optimize out if this is the same as the previous LDT */
    if (!force_reload && ldt_base == current_ldt_base
        && ldt_npages == current_ldt_npages) {
        return;
    }

    uint16_t selector = 0;

    if (ldt_base != 0 && ldt_npages != 0) {
        ldt_descriptor[0].sys_lo.lo_base = ldt_base & ((1ul << 24) - 1);
        ldt_descriptor[0].sys_lo.hi_base = (ldt_base >> 24) & 0xff;
        ldt_descriptor[1].sys_hi.base = ldt_base >> 32;
        assert(ldt_descriptor[0].sys_lo.granularity != 0);
        ldt_descriptor[0].sys_lo.lo_limit = ldt_npages;

        selector = GSEL(LDT_LO_SEL, SEL_UPL);
    }

    __asm volatile("lldt %%ax"
                   : /* No output */
                   : "a" (selector));

    current_ldt_base = ldt_base;
    current_ldt_npages = ldt_npages;
}

/**
 * \brief Setup default GDT.
 *
 * Loads the GDT register with the default GDT and reloads CS and SS
 * to point to the new entries. Resets all other segment registers to null.
 * Finally, completes setup of GDT to include TSS base address mapping and
 * loads TSS into task register.
 */
static void gdt_reset(struct dispatcher_generic *disp)
{
    lvaddr_t                     ptss = (lvaddr_t)&tss;
    struct region_descriptor    region = {
        .rd_limit = sizeof(gdt),
        .rd_base = (uint64_t)&gdt
    };

    // Load default GDT
    __asm volatile("lgdt %[region]" :: [region] "m" (region));

    // Reload segments
    __asm volatile("mov %[null], %%ds      \n\t"
                   "mov %[null], %%es      \n\t"
                   "mov %[ss], %%ss        \n\t"
                   "mov %[null], %%gs      \n\t"
                   "mov %[null], %%fs      \n\t"
                   "pushq %[cs]            \n\t"          // new CS
                   "lea 1f(%%rip), %%rax   \n\t"          // jumps to after lret
                   "pushq %%rax            \n\t"          // new IP
                   "lretq                  \n\t"          // fake return
                   "1:                     \n\t"          // we'll continue here
                   : /* No Output */
                   :
                   [null] "r" (0),
                   [ss] "r" (GSEL(KSTACK_SEL, SEL_KPL)),
                   [cs] "i" (GSEL(KCODE_SEL, SEL_KPL))
                   : "rax"
                   );

    // Complete setup of TSS descriptor (by inserting base address of TSS)
    gdt[TSS_LO_SEL].sys_lo.lo_base = ptss & 0xffffff;
    gdt[TSS_LO_SEL].sys_lo.hi_base = (ptss >> 24) & 0xff;
    gdt[TSS_HI_SEL].sys_hi.base = ptss >> 32;

    // Complete setup of TSS
    tss.rsp[0] = (lvaddr_t)&disp->stack[DISPATCHER_STACK_WORDS];

    // Load task state register
    __asm volatile("ltr %%ax" :: "a" (GSEL(TSS_LO_SEL, SEL_KPL)));
}

/* Utility function for code below; initialises a gate_descriptor */
static void setgd(struct gate_descriptor *gd, void (* handler)(void),
                  int ist, int type, int dpl, int selector)
{
    memset(gd, 0, sizeof(struct gate_descriptor));
    gd->gd_looffset = (uintptr_t)handler & ((1UL << 16) - 1);
    gd->gd_hioffset = (uintptr_t)handler >> 16;
    gd->gd_selector = selector;
    gd->gd_ist = ist;
    gd->gd_type = type;
    gd->gd_dpl = dpl;
    gd->gd_p = 1;
}

/**
 * \brief Define IRQ handler number 'num'.
 *
 * This defines an interrupt handler for vector #num. The way this is done is
 * quite tricky: A block of assembly is emitted, with a label pointing to
 * the beginning of that block. The label is made known as a symbol by
 * having a C function _declaration_ directly in front of the block. The
 * symbol has to be defined extern, so it is global, but its ELF visibility
 * is set "hidden", so that the symbol does not end up in the GOT. This is
 * very important for keeping the code position-independent.
 *
 * The NOERR/ERR variants depend on whether the hardware delivers an error code.
 */
#define HW_EXCEPTION_NOERR(num)                                         \
    void __attribute__ ((visibility ("hidden"))) hwexc_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwexc_"#num",@function                 \n\t"        \
           "hwexc_"#num":                                  \n\t"        \
           "pushq $0                /* dummy error code */ \n\t"        \
           "pushq $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

#define HW_EXCEPTION_ERR(num)                                           \
    void __attribute__ ((visibility ("hidden"))) hwexc_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwexc_"#num",@function                 \n\t"        \
           "hwexc_"#num":                                  \n\t"        \
           "pushq $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

__asm (
    ".text                                              \n\t"
    "   .type hwexc_common ,@function                   \n\t"
    /* a kernel fault means something bad happened, so we stack
     * everything for the debugger to use, in the GDB frame format */
    "hwexc_common:                                      \n\t"
    "pushq 6*8(%rsp) /* SS */                           \n\t"
    "pushq 4*8(%rsp) /* CS */                           \n\t"
    "pushq 7*8(%rsp) /* EFLAGS */                       \n\t"
    "pushq 5*8(%rsp) /* RIP */                          \n\t"
    /* TODO: extend frame size and save FS/GS so we can resume afterwards */
    "pushq %r15                                         \n\t"
    "pushq %r14                                         \n\t"
    "pushq %r13                                         \n\t"
    "pushq %r12                                         \n\t"
    "pushq %r11                                         \n\t"
    "pushq %r10                                         \n\t"
    "pushq %r9                                          \n\t"
    "pushq %r8                                          \n\t"
    "pushq 17*8(%rsp) /* RSP */                         \n\t"
    "pushq %rbp                                         \n\t"
    "pushq %rdi                                         \n\t"
    "pushq %rsi                                         \n\t"
    "pushq %rdx                                         \n\t"
    "pushq %rcx                                         \n\t"
    "pushq %rbx                                         \n\t"
    "pushq %rax                                         \n\t"
    "movq 20*8(%rsp), %rdi  /* vector number */         \n\t"
    "movq 21*8(%rsp), %rsi  /* error code   */          \n\t"
    "movq %rsp, %rdx       /* save area ptr*/           \n\t"
    "jmp generic_handle_exception                       \n\t"
);

// CPU exceptions
HW_EXCEPTION_NOERR(0);
HW_EXCEPTION_NOERR(1);
HW_EXCEPTION_NOERR(2);
HW_EXCEPTION_NOERR(3);
HW_EXCEPTION_NOERR(4);
HW_EXCEPTION_NOERR(5);
HW_EXCEPTION_NOERR(6);
HW_EXCEPTION_NOERR(7);
HW_EXCEPTION_ERR(8);
HW_EXCEPTION_NOERR(9);
HW_EXCEPTION_ERR(10);
HW_EXCEPTION_ERR(11);
HW_EXCEPTION_ERR(12);
HW_EXCEPTION_ERR(13);
HW_EXCEPTION_ERR(14);
HW_EXCEPTION_NOERR(16);
HW_EXCEPTION_ERR(17);
HW_EXCEPTION_NOERR(18);
HW_EXCEPTION_NOERR(19);

// Reserved as "unhandled exception" handler
HW_EXCEPTION_NOERR(666);

/**
 * \brief X86_64 register set
 *
 * As defined by GDB.
 */
enum gdb_x86_64_register_nums {
    GDB_X86_64_RAX_REG, GDB_X86_64_RBX_REG, GDB_X86_64_RCX_REG, GDB_X86_64_RDX_REG,
    GDB_X86_64_RSI_REG, GDB_X86_64_RDI_REG, GDB_X86_64_RBP_REG, GDB_X86_64_RSP_REG,
    GDB_X86_64_R8_REG, GDB_X86_64_R9_REG, GDB_X86_64_R10_REG, GDB_X86_64_R11_REG,
    GDB_X86_64_R12_REG, GDB_X86_64_R13_REG, GDB_X86_64_R14_REG, GDB_X86_64_R15_REG,
    GDB_X86_64_RIP_REG, GDB_X86_64_EFLAGS_REG, GDB_X86_64_CS_REG, GDB_X86_64_SS_REG,

/* these are not saved/used in 64-bit mode, and currently avoided
    DS_REG, ES_REG, FS_REG, GS_REG,
*/

/* these are not used yet:
    ST0_REG, ST1_REG, ST2_REG, ST3_REG, ST4_REG, ST5_REG, ST6_REG, ST7_REG,

    FCTRL_REG, FSTAT_REG, FTAG_REG, FISEG_REG,
    FIOFF_REG, FOSEG_REG, FOOFF_REG, FOP_REG,

    XMM0_REG, XMM1_REG, XMM2_REG, XMM3_REG, XMM4_REG, XMM5_REG,
    XMM6_REG, XMM7_REG, XMM8_REG, XMM9_REG, XMM10_REG, XMM11_REG,
    XMM12_REG, XMM13_REG, XMM14_REG, XMM15_REG,
    MXCSR_REG
*/

    GDB_X86_64_NUM_REGS /* not a real register; must be last! */
};

void disp_pagefault(dispatcher_handle_t handle, lvaddr_t fault_address,
                    uintptr_t error, lvaddr_t ip);

/**
 * \brief Handles kernel exceptions
 *
 * \param vec   Vector number of exception
 * \param error Error code from CPU, or 0 for an exception without an error code
 * \param gdb_save_frame Pointer to save area for registers stacked by trap handler
 */
static __attribute__ ((used))
void generic_handle_exception(uint64_t vec, uint64_t error,
                              uintptr_t *gdb_save_frame)
{
    // XXX: This assumes we're enabled. That's not always the case...

    switch(vec) {
    case IDT_PF:
        {
            // For now, disable the dispatcher and call classic exception handler code
            dispatcher_handle_t handle = disp_disable();
            lvaddr_t fault_address;
            arch_registers_state_t *regs = dispatcher_get_enabled_save_area(handle);
            __asm volatile("mov %%cr2, %[fault_address]"
                           : [fault_address] "=r" (fault_address));

            // Write registers to dispatcher save area
            regs->rsp = gdb_save_frame[GDB_X86_64_RSP_REG];
            regs->eflags = gdb_save_frame[GDB_X86_64_EFLAGS_REG];
            regs->rip = gdb_save_frame[GDB_X86_64_RIP_REG];

            disp_pagefault(handle, fault_address, error, regs->rip);
        }
        break;

    default:
        debug_printf("Unhandled exception %d at 0x%" PRIxPTR " (error code 0x%lx)\n",
                     (int)vec, gdb_save_frame[GDB_X86_64_RIP_REG], error);
        abort();
        break;
    }
}

/**
 * \brief Interrupt Descriptor Table (IDT) for processor this kernel is running
 * on.
 */
static struct gate_descriptor idt[NIDT] __attribute__ ((aligned (16)));

/**
 * \brief Sets up the default IDT for current CPU.
 */
static void setup_default_idt(void)
{
    struct region_descriptor region = {         // set default IDT
        .rd_limit = NIDT * sizeof(idt[0]) - 1,
        .rd_base = (uint64_t)&idt
    };
    int i;

    // reset IDT
    memset((void *)&idt, 0, NIDT * sizeof(idt[0]));

    // initialize IDT with default generic handlers
    for (i = 0; i < NIDT; i++)
        setgd(&idt[i], hwexc_666, 0, SDT_SYSIGT, SEL_KPL,
              GSEL(KCODE_SEL, SEL_KPL));

    /* Setup exception handlers */
    setgd(&idt[0], hwexc_0, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[1], hwexc_1, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[2], hwexc_2, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[3], hwexc_3, 0, SDT_SYSTGT, SEL_UPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[4], hwexc_4, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[5], hwexc_5, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[6], hwexc_6, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[7], hwexc_7, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[8], hwexc_8, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[9], hwexc_9, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[10], hwexc_10, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[11], hwexc_11, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[12], hwexc_12, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[13], hwexc_13, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[14], hwexc_14, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupt 15 is undefined
    setgd(&idt[16], hwexc_16, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[17], hwexc_17, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[18], hwexc_18, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[19], hwexc_19, 0, SDT_SYSTGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupts 20 - 31 are reserved

    /* Load IDT register */
    __asm volatile("lidt %0" :: "m" (region));
}
#endif

/** \brief Initialise private (per-dispatcher) LDT */
void ldt_init_disabled(dispatcher_handle_t handle)
{
    errval_t err;

    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    struct dispatcher_x86_64 *disp_priv = get_dispatcher_x86_64(handle);

    /* setup private (static) LDT, and get kernel to load it */
    disp->ldt_base = (lvaddr_t) ldt;
    // XXX: size may not be multiple of page size, but does it really matter?
    disp->ldt_npages = DIVIDE_ROUND_UP(sizeof(ldt), BASE_PAGE_SIZE);
#ifdef ARRAKIS
    gdt_reset(get_dispatcher_generic(handle));
    maybe_reload_ldt(disp, true);
    setup_default_idt();
#else
    sys_x86_reload_ldt();
#endif

    /* XXX: kludge to maintain backwards compatibility.
     * Setup a single segment descriptor that we can use to locate the
     * current dispatcher (i.e. curdispatcher() always works). This
     * will be replaced once we switch onto a thread with a real FS segment.
     */
    disp_priv->dummyseg[0] = 0;
    disp_priv->dummyseg[1] = handle;
    err = ldt_alloc_segment_disabled(handle, disp_priv->dummyseg,
                                     &disp_priv->disp_seg_selector);
    if (err_is_fail(err)) {
        // XXX: can't call usual debug/panic code, as curdispatcher() won't work
        char buf[128];
        snprintf(buf, sizeof(buf),
                 "%.*s.%u: fatal error in ldt_init_disabled(). Aborted.\n",
                 DISP_NAME_LEN, disp->d.name, disp_priv->generic.core_id);
        sys_print(buf, sizeof(buf));
        while (1) {disp_yield_disabled(handle);}
    }

    /* load this segment to FS */
    __asm volatile("mov %%ax, %%fs"
                   : /* No outputs */
                   : "a" (disp_priv->disp_seg_selector));
}

/**
 * \brief Allocate and fill a segment descriptor in the LDT
 *
 * \param handle Dispatcher handle
 * \param segbase Base of segment
 * \param ret_selector On success, used to return selector for new segment
 */
errval_t ldt_alloc_segment_disabled(dispatcher_handle_t handle, void *segbase,
                                    uint16_t *ret_selector)
{
    // segment descriptors are limited to a 32-bit base address
    if ((lvaddr_t)segbase >= (1ul << 32)) {
        return LIB_ERR_SEGBASE_OVER_4G_LIMIT;
    }

    // construct descriptor
    union segment_descriptor desc = {
        .d = {
            .lo_base = ((lvaddr_t) segbase) & 0xffffff,
            .hi_base = (((lvaddr_t) segbase) >> 24) & 0xff,
            .type = 3, /* read/write data, accessed */
            .system_desc = 1, /* data */
            .privilege_level = 3, /* user mode */
            .present = 1,
            .long_mode = 0,
            .operation_size = 1,
        }
    };

    // find free LDT entry
    acquire_spinlock(&ldt_spinlock);
    for (int i = 0; i < LDT_NENTRIES; i++) {
        if (!ldt[i].d.present) {
            ldt[i] = desc;
            release_spinlock(&ldt_spinlock);
            assert_disabled(ret_selector != NULL);
            *ret_selector = X86_64_LDT_SELECTOR(i);
            return SYS_ERR_OK;
        }
    }
    release_spinlock(&ldt_spinlock);

    return LIB_ERR_LDT_FULL;
}

/**
 * \brief enabled version of ldt_alloc_segment_disabled()
 * 
 * Exposed for calls by special-case software that needs to play with segments.
 */
errval_t ldt_alloc_segment(void *segbase, uint16_t *ret_selector)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t ret = ldt_alloc_segment_disabled(handle, segbase, ret_selector);
    disp_enable(handle);
    return ret;
}

/**
 * \brief Free a previously-allocated segment on a specific dispatcher
 *
 * \param handle Dispatcher handle
 * \param selector Segment selector
 */
errval_t ldt_free_segment_ondisp(dispatcher_handle_t handle, uint16_t selector)
{
    if ((selector & 0x7) != 7) { // XXX: user-priv LDT selector
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    int idx = X86_64_SELECTOR_IDX(selector);

    // check that this entry is occupied
    if (idx >= LDT_NENTRIES || !ldt[idx].d.present) {
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    // mark entry as free
    ldt[idx].raw = 0;
    return SYS_ERR_OK;
}

/**
 * \brief Free a previously-allocated segment on the current dispatcher
 *
 * \param selector Segment selector
 */
errval_t ldt_free_segment(uint16_t selector)
{
    // strictly speaking, we probably don't need to disable here
    dispatcher_handle_t handle = disp_disable();
    errval_t ret = ldt_free_segment_ondisp(handle, selector);
    disp_enable(handle);
    return ret;
}

/**
 * \brief Update the base address of a previously-allocated segment
 *
 * \param selector Segment selector
 * \param segbase New base of segment
 */
errval_t ldt_update_segment(uint16_t selector, void *segbase)
{
    if ((selector & 0x7) != 7) { // XXX: user-priv LDT selector
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    int idx = X86_64_SELECTOR_IDX(selector);

    // check that this entry is occupied
    if (idx >= LDT_NENTRIES || !ldt[idx].d.present) {
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    // segment descriptors are limited to a 32-bit base address
    if ((lvaddr_t)segbase >= (1ul << 32)) {
        return LIB_ERR_SEGBASE_OVER_4G_LIMIT;
    }

    // update base address
    ldt[idx].d.lo_base = ((lvaddr_t) segbase) & 0xffffff;
    ldt[idx].d.hi_base = (((lvaddr_t) segbase) >> 24) & 0xff;

    return SYS_ERR_OK;
}
