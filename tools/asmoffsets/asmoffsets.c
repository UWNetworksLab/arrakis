/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * "Code" to calculate offsets of fields from structures for assembler.
 *
 * This is pretty ugly. GCC gives us the nice offsetof() function, for computing
 * the byte offset of some member (eg. within a struct), however there is no
 * easy way to get that value out to standalone assembly code, hence this.
 *
 * This file is compiled but never assembled. Instead, we trick GCC (using the
 * inline asm syntax) into emitting a bunch of #defines for the offsets we need.
 * Then, a postprocessing script (grep) extracts the actual #defines, and
 * creates a header file suitable for inclusion in to standalone assembly code.
 *
 * As an added bonus, we can use this to do compile-time assertion checking on
 * statically-known values (eg. struct sizes).
 */

/* GCC builtin offsetof */
// #define offsetof(TYPE, MEMBER)  __builtin_offsetof(TYPE, MEMBER)

/* macro to generate the fake assembly output that results in a #define */
#define EMITX(NAMESTR, VALUE) \
    __asm("\n#define " NAMESTR "\t%[val]\n" \
          :: [val] "n" (VALUE));

/* macro to emit a "constant" value */
#define EMIT(NAME, VALUE) EMITX(#NAME, VALUE)

/* macro to emit an offset */
#define DECL(NAME, TYPE, MEMBER) \
    EMITX("OFFSETOF_" #NAME, offsetof(TYPE, MEMBER))

/* macro to emit an offset limit (e.g. stack limit) */
#define DECL_LIMIT(NAME, TYPE, MEMBER) \
    EMITX("OFFSETOF_" #NAME, (offsetof(TYPE, MEMBER) + sizeof(((TYPE*)0)->MEMBER)))

#if 0
// XXX This is nonsense code: if its determinable statically then use
// a static assert.  If it isnt then we want to know here since there
// is going to be no dynamic test with this code.

/* macro to generate the fake assembly output that results in a compile-time
 * error if any of our structures are too big */
#define XXASSERT(EXP, EXPSTR, LINE)                             \
    if (!(EXP)) {                                               \
        __asm("\n#error Compile-time assertion failure: "       \
              EXPSTR ", " __FILE__ ":" #LINE "\n");             \
    }
#define XASSERT(EXP,EXPSTR,LINE) XXASSERT(EXP, EXPSTR, LINE)
#define ASSERT(EXP) XASSERT(EXP, #EXP, __LINE__)
#endif

#include <barrelfish/static_assert.h>
#define ASSERT(EXP) STATIC_ASSERT(EXP, #EXP)

/* XXX: kernel and user includes together */
#include <deputy/nodeputy.h>
#include <kernel.h>
#include <dispatch.h> // XXX: from kernel include dir
#include <barrelfish/barrelfish.h>
#include <barrelfish/lmp_endpoints.h>

#include <barrelfish/dispatcher_arch.h>

/* wrap everything inside a dummy function, to keep the compiler happy */
#ifdef __ICC
int main(void)
#else
void dummy(void);
void dummy(void)
#endif
{
    /* preamble */
    __asm("\n#ifndef ASMOFFSETS_H\n#define ASMOFFSETS_H\n");
    DECL(DCB_DISP, struct dcb, disp);
    DECL(DCB_DISABLED, struct dcb, disabled);
    // XXX: Assumes cap is first member of struct cte
    DECL(DCB_CSPACE_CAP, struct dcb, cspace.cap);
    DECL(DCB_VSPACE, struct dcb, vspace);
#ifdef CONFIG_SCHEDULER_RR
    DECL(DCB_RR_PREV, struct dcb, prev);
#elif defined(CONFIG_SCHEDULER_RBED)
    DECL(DCB_RBED_NEXT, struct dcb, next);
#else
# error Must define a kernel scheduling policy!
#endif

    DECL(CAP_CNODE_BITS, struct capability, u.cnode.bits);
    DECL(CAP_CNODE_CNODE, struct capability, u.cnode.cnode);
    DECL(CAP_TYPE, struct capability, type);
    DECL(CAP_ENDPOINT_EPOFFSET, struct capability, u.endpoint.epoffset);
    DECL(CAP_ENDPOINT_EPBUFLEN, struct capability, u.endpoint.epbuflen);
    DECL(CAP_ENDPOINT_LISTENER, struct capability, u.endpoint.listener);

    DECL(DISP_DISABLED, struct dispatcher_shared_generic, disabled);
    DECL(DISP_RUN, struct dispatcher_shared_generic, dispatcher_run);
    DECL(DISP_LRPC, struct dispatcher_shared_generic, dispatcher_lrpc);
    DECL(DISP_UDISP, struct dispatcher_shared_generic, udisp);
    DECL(DISP_LMP_DELIVERED, struct dispatcher_shared_generic, lmp_delivered);
    DECL(DISP_SYSTIME, struct dispatcher_shared_generic, systime);

    DECL_LIMIT(DISP_PRIV_STACK_LIMIT, struct dispatcher_generic, stack);
    DECL_LIMIT(DISP_PRIV_TRAP_STACK_LIMIT, struct dispatcher_generic, trap_stack);

#if defined __x86_64__
    DECL(DISP_X86_64_CRIT_PC_LOW, struct dispatcher_shared_x86_64, crit_pc_low);
    DECL(DISP_X86_64_CRIT_PC_HIGH, struct dispatcher_shared_x86_64, crit_pc_high);
    DECL(DISP_X86_64_LDT_BASE, struct dispatcher_shared_x86_64, ldt_base);
    DECL(DISP_X86_64_LDT_NPAGES, struct dispatcher_shared_x86_64, ldt_npages);
    EMIT(LDT_LO_SEL, LDT_LO_SEL);
    EMIT(LDT_HI_SEL, LDT_HI_SEL);
    EMIT(LDT_SELECTOR, GSEL(LDT_LO_SEL, SEL_UPL));
    DECL(DISP_X86_64_ENABLED_AREA, struct dispatcher_shared_x86_64, enabled_save_area);
    DECL(DISP_X86_64_DISABLED_AREA, struct dispatcher_shared_x86_64, disabled_save_area);
    DECL(DISP_X86_64_TRAP_AREA, struct dispatcher_shared_x86_64, trap_save_area);
#endif

#if defined __i386__
    DECL(DISP_X86_32_CRIT_PC_LOW, struct dispatcher_shared_x86_32, crit_pc_low);
    DECL(DISP_X86_32_CRIT_PC_HIGH, struct dispatcher_shared_x86_32, crit_pc_high);
    DECL(DISP_X86_32_ENABLED_AREA, struct dispatcher_shared_x86_32, enabled_save_area);
    DECL(DISP_X86_32_DISABLED_AREA, struct dispatcher_shared_x86_32, disabled_save_area);
    DECL(DISP_X86_32_TRAP_AREA, struct dispatcher_shared_x86_32, trap_save_area);
#endif

#if defined(__arm__)
    DECL(DISP_CRIT_PC_LOW, struct dispatcher_shared_arm, crit_pc_low);
    DECL(DISP_CRIT_PC_HIGH, struct dispatcher_shared_arm, crit_pc_high);
    DECL(DISP_ENABLED_AREA, struct dispatcher_shared_arm, enabled_save_area);
    DECL(DISP_DISABLED_AREA, struct dispatcher_shared_arm, disabled_save_area);
    DECL(DISP_TRAP_AREA, struct dispatcher_shared_arm, trap_save_area);
    DECL(DISP_GENERIC, struct dispatcher_arm, generic);
#endif // __arm__

    DECL(LMP_ENDPOINT_DELIVERED, struct lmp_endpoint_kern, delivered);
    DECL(LMP_ENDPOINT_CONSUMED, struct lmp_endpoint_kern, consumed);
    DECL(LMP_ENDPOINT_KERNPART, struct lmp_endpoint, k);

    EMIT(OBJTYPE_ENDPOINT, ObjType_EndPoint);

    // register offsets in save areas
#if __x86_64__
    DECL(RAX_REG, struct registers_x86_64, rax);
    DECL(RSP_REG, struct registers_x86_64, rsp);
    DECL(RIP_REG, struct registers_x86_64, rip);
    DECL(EFLAGS_REG, struct registers_x86_64, eflags);
    DECL(FS_REG, struct registers_x86_64, fs);
    DECL(GS_REG, struct registers_x86_64, gs);
#elif __i386__
    DECL(FS_REG, struct registers_x86_32, fs);
    DECL(GS_REG, struct registers_x86_32, gs);
#endif /* __x86_64__ */

    // error codes needed in LRPC path
    EMIT(SYS_ERR_OK, SYS_ERR_OK);
    EMIT(SYS_ERR_LMP_TARGET_DISABLED, SYS_ERR_LMP_TARGET_DISABLED);
    EMIT(SYS_ERR_LMP_BUF_OVERFLOW, SYS_ERR_LMP_BUF_OVERFLOW);
    EMIT(SYS_ERR_LRPC_SLOT_INVALID, SYS_ERR_LRPC_SLOT_INVALID);
    EMIT(SYS_ERR_LRPC_NOT_ENDPOINT, SYS_ERR_LRPC_NOT_ENDPOINT);

    /* sanity check size of various structures, so we break the build if they
     * don't match */
#if   defined __x86_64__
    ASSERT(sizeof(struct dispatcher_x86_64) <= (1 << DISPATCHER_FRAME_BITS));
#elif defined __i386__
    ASSERT(sizeof(struct dispatcher_x86_32) <= (1 << DISPATCHER_FRAME_BITS));
#elif defined __arm__
    ASSERT(sizeof(struct dispatcher_arm) <= (1 << DISPATCHER_FRAME_BITS));
#else
#error "Define architecture"
#endif
    ASSERT(sizeof(struct cte) <= (1 << OBJBITS_CTE));
    ASSERT(sizeof(struct dcb) <= (1 << OBJBITS_DISPATCHER));

    union lmp_recv_header rcvheader;
    EMIT(SIZEOF_LMP_RECV_HEADER, sizeof(rcvheader));
    EMIT(SIZEOF_LMP_RECV_HEADER_RAW, sizeof(rcvheader.raw));
    ASSERT(sizeof(rcvheader) == sizeof(rcvheader.raw));

    EMIT(SIZEOF_STRUCT_SYSRET, sizeof(struct sysret));

    /* footer */
    __asm("\n#endif /* ASMOFFSETS_H */\n");
}
