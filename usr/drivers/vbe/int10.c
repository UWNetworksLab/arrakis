/**
 * \file
 * \brief Video BIOS int 10h interface.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdarg.h>
#include <barrelfish/barrelfish.h>
#include <x86emu.h>
#include <mackerel/mackerel.h>

#include "int10.h"

#define M _X86EMU_env

#define X86_EAX M.x86.R_EAX
#define X86_EBX M.x86.R_EBX
#define X86_ECX M.x86.R_ECX
#define X86_EDX M.x86.R_EDX
#define X86_ESI M.x86.R_ESI
#define X86_EDI M.x86.R_EDI
#define X86_EBP M.x86.R_EBP
#define X86_EIP M.x86.R_EIP
#define X86_ESP M.x86.R_ESP
#define X86_EFLAGS M.x86.R_EFLG

#define X86_FLAGS M.x86.R_FLG
#define X86_AX M.x86.R_AX
#define X86_BX M.x86.R_BX
#define X86_CX M.x86.R_CX
#define X86_DX M.x86.R_DX
#define X86_SI M.x86.R_SI
#define X86_DI M.x86.R_DI
#define X86_BP M.x86.R_BP
#define X86_IP M.x86.R_IP
#define X86_SP M.x86.R_SP
#define X86_CS M.x86.R_CS
#define X86_DS M.x86.R_DS
#define X86_ES M.x86.R_ES
#define X86_SS M.x86.R_SS
#define X86_FS M.x86.R_FS
#define X86_GS M.x86.R_GS

#define X86_AL M.x86.R_AL
#define X86_BL M.x86.R_BL
#define X86_CL M.x86.R_CL
#define X86_DL M.x86.R_DL

#define X86_AH M.x86.R_AH
#define X86_BH M.x86.R_BH
#define X86_CH M.x86.R_CH
#define X86_DH M.x86.R_DH

#define X86_IF_MASK             0x00000200
#define X86_IOPL_MASK           0x00003000

static void *mymem = NULL;

/// XXX: Dunno if this is a save address
#define STACKSEG        0x30000

/**
 * \brief Dump all registers of the emulated x86.
 */
static void dump_registers(void)
{
    printf("EAX=0x%8.8lx, EBX=0x%8.8lx, ECX=0x%8.8lx, EDX=0x%8.8lx\n",
           (unsigned long)X86_EAX, (unsigned long)X86_EBX,
           (unsigned long)X86_ECX, (unsigned long)X86_EDX);
    printf("ESP=0x%8.8lx, EBP=0x%8.8lx, ESI=0x%8.8lx, EDI=0x%8.8lx\n",
           (unsigned long)X86_ESP, (unsigned long)X86_EBP,
           (unsigned long)X86_ESI, (unsigned long)X86_EDI);
    printf("CS=0x%4.4x, SS=0x%4.4x,"
           " DS=0x%4.4x, ES=0x%4.4x, FS=0x%4.4x, GS=0x%4.4x\n",
           X86_CS, X86_SS, X86_DS, X86_ES, X86_FS, X86_GS);
    printf("EIP=0x%8.8lx, EFLAGS=0x%8.8lx\n",
           (unsigned long)X86_EIP, (unsigned long)X86_EFLAGS);
}

static uint8_t mem_rb(u32 addr)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    return *(uint8_t *)(mymem + addr);
}

static uint16_t mem_rw(u32 addr)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    return *(uint16_t *)(mymem + addr);
}

static u32 mem_rl(u32 addr)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    return *(uint32_t *)(mymem + addr);
}

static void mem_wb(u32 addr, uint8_t val)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    *(uint8_t *)(mymem + addr) = val;
}

static void mem_ww(u32 addr, uint16_t val)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    *(uint16_t *)(mymem + addr) = val;
}

static void mem_wl(u32 addr, u32 val)
{
    assert(addr < REALMODE_MEM_SIZE);
    assert(mymem != NULL);
    *(uint32_t *)(mymem + addr) = val;
}

/**
 * \brief Pushes one 16-bit word onto the emulated x86's stack.
 *
 * \param val   Word to push
 */
static void pushw(uint16_t val)
{
    X86_ESP -= 2;
    mem_ww(((uint32_t) X86_SS << 4) + X86_SP, val);
}

/**
 * \brief Emulates an x86 software interrupt.
 *
 * \param int   Interrupt number to call.
 *
 * \return 1 on success, 0 on error.
 */
static int run_bios_int(int num)
{
    uint32_t eflags;
/*     printf("calling video BIOS (int %x) at: ", num); */
    eflags = X86_EFLAGS;
    pushw(eflags);
    pushw(X86_CS);
    pushw(X86_IP);
    X86_CS = mem_rw((num << 2) + 2);
    X86_IP = mem_rw(num << 2);
/*     printf("0x%x:%x\n", X86_CS, X86_EIP); */

    return 1;
}

static int int1A_handler(void)
{
    USER_PANIC("NYI");
    return 0;
}

static int intE6_handler(void)
{
    USER_PANIC("NYI");
    return 0;
}

static void int_handler(int num)
{
    int ret = 0;

    switch (num) {
    case 0x1a:
        // Int 0x1a deals with PCI config space
        ret = int1A_handler();
        break;

    case 0xe6:
        // Int 0xe6 deals with PCI config space
        ret = intE6_handler();
        break;

    default:
        break;
    }

    if (!ret) {
        ret = run_bios_int(num);
    }

    if (!ret) {
        printf("Halting on int 0x%2.2x!\n", num);
        dump_registers();
        X86EMU_halt_sys();
    }
}

static uint8_t xinb(uint16_t port)
{
    uint8_t val = mackerel_read_io_8(port, 0);
    //printf("inb %x %x\n", port, val);
    return val;
}

static uint16_t xinw(uint16_t port)
{
    uint16_t val = mackerel_read_io_16(port, 0);
    //printf("inw %x %x\n", port, val);
    return val;
}

static u32 xinl(uint16_t port)
{
    uint32_t val = mackerel_read_io_32(port, 0);
    //printf("inl %x %x\n", port, val);
    return val;
}

static void xoutb(uint16_t port, uint8_t val)
{
    //printf("outb %x %x\n", port, val);
    mackerel_write_io_8(port, 0, val);
}

static void xoutw(uint16_t port, uint16_t val)
{
    //printf("outw %x %x\n", port, val);
    mackerel_write_io_16(port, 0, val);
}

static void xoutl(uint16_t port, u32 val)
{
    //printf("outl %x %x\n", port, val);
    mackerel_write_io_32(port, 0, val);
}

static void set_return_trap(void)
{
    /*
     * Here we set the exit condition:  We return when we encounter
     * 'hlt' (=0xf4), which we locate at address 0x600 in x86 memory.
     */
    mem_wb(0x0600, 0xf4);
}

void int10_init(void *mem)
{
    int i;
    X86EMU_intrFuncs intFuncs[256];
    X86EMU_pioFuncs pioFuncs = {
        (&xinb),
        (&xinw),
        (&xinl),
        (&xoutb),
        (&xoutw),
        (&xoutl)
    };
    X86EMU_memFuncs memFuncs = {
        (&mem_rb),
        (&mem_rw),
        (&mem_rl),
        (&mem_wb),
        (&mem_ww),
        (&mem_wl)
    };

    X86EMU_setupMemFuncs(&memFuncs);

    _X86EMU_env.mem_base = 0;
    _X86EMU_env.mem_size = 1024 * 1024 + 1024;
    X86EMU_setupPioFuncs(&pioFuncs);

    for(i = 0; i < 256; i++) {
        intFuncs[i] = int_handler;
    }

    X86EMU_setupIntrFuncs(intFuncs);

    mymem = mem;

    set_return_trap();
}

#if 0
static void
SetResetBIOSVars(xf86Int10InfoPtr pInt, Bool set)
{
    int pagesize = getpagesize();
    unsigned char* base = xf86MapVidMem(pInt->scrnIndex,
                                        VIDMEM_MMIO, 0, pagesize);
    int i;

    if (set) {
        for (i = BIOS_SCRATCH_OFF; i < BIOS_SCRATCH_END; i++)
            mem_ww(pInt, i, *(base + i));
    } else {
        for (i = BIOS_SCRATCH_OFF; i < BIOS_SCRATCH_END; i++)
            *(base + i) = MEM_RW(pInt, i);
    }

    xf86UnMapVidMem(pInt->scrnIndex,base,pagesize);
}
#endif

void int10(struct int10_regs *regs)
{
    X86_EAX = regs->eax;
    X86_EBX = regs->ebx;
    X86_ECX = regs->ecx;
    X86_EDX = regs->edx;
    X86_ESI = regs->esi;
    X86_EDI = regs->edi;
    X86_EBP = regs->ebp;
    X86_ESP = 0x1000; X86_SS = STACKSEG >> 4;
    X86_EIP = 0x0600; X86_CS = 0x0;     /* address of 'hlt' */
    X86_DS = 0x40;                      /* standard pc ds */
    X86_ES = regs->es;
    X86_FS = 0;
    X86_GS = 0;
    X86_EFLAGS = X86_IF_MASK | X86_IOPL_MASK;

    int_handler(0x10);
    X86EMU_exec();

    regs->eax = X86_EAX;
    regs->ebx = X86_EBX;
    regs->ecx = X86_ECX;
    regs->edx = X86_EDX;
    regs->esi = X86_ESI;
    regs->edi = X86_EDI;
    regs->es = X86_ES;
    regs->ebp = X86_EBP;
    regs->eflags = X86_EFLAGS;
}

void printk(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
}
