/**
 * \file
 * \brief System call entry point to the kernel and LRPC fast-path
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/lmp.h>
#include <target/x86_64/offsets_target.h>
#include <x86.h>
#include <asmoffsets.h>

    .text
    .globl syscall_entry

syscall_entry:
        /* is this an LRPC or a normal syscall? */
        cmp $SYSCALL_LRPC, %rdi
        jne  syscall_path   /* normal syscall, branch off */

	/* Load pointer to current DCB */
	mov	dcb_current(%rip), %rdi

	/* TODO: Check that caller is not disabled */

        /* dcb_current->disabled=false */
        movb $0, OFFSETOF_DCB_DISABLED(%rdi)

        /* Save caller's registers */
	mov	OFFSETOF_DCB_DISP(%rdi), %rdi
	lea	OFFSETOF_DISP_X86_64_ENABLED_AREA(%rdi), %rdi
	movq	$SYS_ERR_OK, OFFSETOF_RAX_REG(%rdi)
	mov	%rcx, OFFSETOF_RIP_REG(%rdi)
	mov	%r11, OFFSETOF_EFLAGS_REG(%rdi)
	mov	%rsp, OFFSETOF_RSP_REG(%rdi)
	mov     %fs, OFFSETOF_FS_REG(%rdi)
	mov	%gs, OFFSETOF_GS_REG(%rdi)

	/* Load pointer to root CNode cap */
	mov	dcb_current(%rip), %rdi
	lea	OFFSETOF_DCB_CSPACE_CAP(%rdi), %rdi

	/* Check that slot number is within CNode */
        movb OFFSETOF_CAP_CNODE_BITS(%rdi), %cl
        mov $1, %r15
        shl %cl, %r15
	cmp	%r15, %rsi
	jae	err_slot

	/* Load pointer to endpoint cap */
	shl	$OBJBITS_CTE, %rsi
	mov	OFFSETOF_CAP_CNODE_CNODE(%rdi), %rcx
	mov	$0xffffff8000000000, %rdi	// phys_to_mem()
	add	%rdi, %rcx
	add	%rsi, %rcx

	/* Check that it's an endpoint */
	cmpl	$OBJTYPE_ENDPOINT, OFFSETOF_CAP_TYPE(%rcx)
	jne	err_endpoint

	/* TODO: Check rights on the endpoint */

	/* Set epoffset for receiver, load epbuflen */
	mov	OFFSETOF_CAP_ENDPOINT_EPOFFSET(%rcx), %rdi
        mov     OFFSETOF_CAP_ENDPOINT_EPBUFLEN(%rcx), %r13d /* r13d = epbuflen */

	/* Load pointer to listener's DCB */
	mov	OFFSETOF_CAP_ENDPOINT_LISTENER(%rcx), %rsi

	/* Check whether listener is runnable */
#if defined(CONFIG_SCHEDULER_RR)
	cmpl	$0, OFFSETOF_DCB_RR_PREV(%rsi)
	je	lrpc_make_runnable
#elif defined(CONFIG_SCHEDULER_RBED)
	cmpl	$0, OFFSETOF_DCB_RBED_NEXT(%rsi)
	je	lrpc_rbed_check_runnable
#else
# error Must define a kernel scheduling policy!
#endif

lrpc_check_runnable_continue:
	/* Check whether listener is disabled */
	cmpb	$0, OFFSETOF_DCB_DISABLED(%rsi)
	jne	err_disabled

        /* RCX = target dispatcher */
        mov OFFSETOF_DCB_DISP(%rsi), %rcx

        /* Remember LRPC entry point on target (R15) */
        mov OFFSETOF_DISP_LRPC(%rcx), %r15

        /* check that the receiver has space in their buffer */
        add %rdi, %rcx /* add epoffset to dispatcher: rcx = endpoint pointer */
        mov OFFSETOF_LMP_ENDPOINT_DELIVERED(%rcx), %r11d /* r11d = delivered */
        mov %r11d, %r12d /* r12d = delivered */
        mov OFFSETOF_LMP_ENDPOINT_CONSUMED(%rcx), %r14d /* r14d = consumed */

        /*
         *  newpos = delivered + len;
         *  if (newpos >= consumed && consumed > delivered)
         *    goto err_buflen;
         *  if (newpos >= epbuflen) {
         *    newpos -= epbuflen;
         *    if (newpos >= consumed)
         *      goto err_buflen;
         *  }
         *  delivered = newpos
         */

        add $(LRPC_MSG_LENGTH + LMP_RECV_HEADER_LENGTH), %r11d /* r11d (newpos) = delivered + len */

        cmp %r14d, %r11d
        jb 1f /* if newpos < consumed */
        cmp %r12d, %r14d
        ja err_buflen /* if consumed > delivered */

1:
        cmp %r13d, %r11d
        jb 2f /* if newpos < epbuflen */

        /* newpos >= epbuflen */
        sub %r13d, %r11d /* newpos (r11d) -= epbuflen (r13d) */
        cmp %r14d, %r11d /* if newpos >= consumed */
        jae err_buflen

2:      /* there's enough space, reserve it by updating delivered = newpos */
        mov %r11d, OFFSETOF_LMP_ENDPOINT_DELIVERED(%rcx)

	/* Set current domain to receiver */
	mov	%rsi, dcb_current(%rip)

	/* Switch to listener address space */
	mov	OFFSETOF_DCB_VSPACE(%rsi), %rax
	mov	%rax, %cr3

        /* Zero registers to avoid the receiver getting hold of them
         * FIXME: should zero all non-payload registers */
        xor     %eax, %eax
        mov     %eax, %fs
        mov     %eax, %gs
        
	/* Get new dispatcher pointer */
	mov	OFFSETOF_DCB_DISP(%rsi), %rax
	/* Disable target dispatcher -- gotta do it here for TLB hit reasons */
	movl	$1, OFFSETOF_DISP_DISABLED(%rax)
        /* update dispatcher's global delivered count */
        addl    $(LRPC_MSG_LENGTH + LMP_RECV_HEADER_LENGTH), OFFSETOF_DISP_LMP_DELIVERED(%rax)
        /* update systime field in dispatcher from kernel_now variable */
        movq    kernel_now(%rip), %r11
        movq    %r11, OFFSETOF_DISP_SYSTIME(%rax)

        /* Check if it's necessary to load a new LDT */
	mov	OFFSETOF_DISP_X86_64_LDT_BASE(%rax), %r11
        mov     OFFSETOF_DISP_X86_64_LDT_NPAGES(%rax), %r14
        cmp     current_ldt_base(%rip), %r11
        jne load_ldt

        cmp     current_ldt_npages(%rip), %r14
        jne load_ldt

load_ldt_continue:
	/* Enter at LRPC entry point */
        mov     %r12d, %esi            /* bufpos of reserved space in EP buffer */
	mov	%r15, %rcx             /* saved LRPC EP */
        movq    OFFSETOF_DISP_UDISP(%rax), %rax /* user-level dispatcher pointer */
	mov	$USER_RFLAGS, %r11  /* eflags */
	sysretq

load_ldt: /* Load a new LDT: r11 = base, r14 = npages, rcx = temp for descriptor */

        /* If either base or npages is zero, load an invalid LDT */
        cmpq    $0, %r11
        je load_ldt_invalid
        cmpq    $0, %r14
        je load_ldt_invalid
        
        /* Update segment descriptor for LDT */

        movq    %r11, current_ldt_base(%rip)
        movq    %r14, current_ldt_npages(%rip)

        /* Format of high word of descriptor is:
         * 32 bits of zero/reserved
         * Base bits 63-32 */
        mov %r11, %rcx
        shr $32, %rcx
        shl $32, %rcx

        // Store new descriptor (high half) to GDT
        mov %rcx, (gdt + 8*LDT_HI_SEL)(%rip)

        /* Format of low word of descriptor is:
         * Base bits 31-24 (top 8 bits of 32-bit addr)
         * 16 bits of flags/miscellany: 0x80e2
         *   granularity = 1
         *   operation_size = irrelevant
         *   long_mode = irrelevant
         *   available = irrelevant
         *   4 high bits of limit address = 0 (assuming LDT is < 2**16 * 4k)
         *   present = 1
         *   privilege_level (2 bits wide) = 3 (user privilege)
         *   system descriptor = 0
         *   type (4 bits wide) = 2
         * low 24 bits of base addr
         * low 16 bits of limit
         */

        // bits 24:31 of base
        mov %r11, %rcx
        shr $24, %rcx

        // flags/misc
        shl $16, %rcx
        or  $0x80e2, %rcx

        // low 24 bits of base
        shl $24, %rcx
        shl $40, %r11
        shr $40, %r11
        or  %r11, %rcx

        // low 16 bits of limit
        shl $16, %rcx
        shl $48, %r14
        shr $48, %r14
        or  %r14, %rcx

        // Store new descriptor (low half) to GDT
        mov %rcx, (gdt + 8*LDT_LO_SEL)(%rip)
        
        // Construct segment selector and load it
        mov     $LDT_SELECTOR, %cx        
        lldt    %cx
	jmp	load_ldt_continue

load_ldt_invalid:  /* Load an invalid LDT */
        mov     $0, %cx
        lldt    %cx
        movq    $0, current_ldt_base(%rip)
        movq    $0, current_ldt_npages(%rip)
        jmp     load_ldt_continue

err_slot:	// Wrong slot
	mov	$SYS_ERR_LRPC_SLOT_INVALID, %rax
	jmp	err

err_endpoint:	// Not an endpoint
	mov	$SYS_ERR_LRPC_NOT_ENDPOINT, %rax
	/* jmp	err  - fall through */

    /* An error occured */
err:
    /* Restore user's state */
    mov dcb_current(%rip), %rdi
    mov OFFSETOF_DCB_DISP(%rdi), %rdi
    lea OFFSETOF_DISP_X86_64_ENABLED_AREA(%rdi), %rdi
    mov OFFSETOF_RIP_REG(%rdi), %rcx
    mov OFFSETOF_EFLAGS_REG(%rdi), %r11
    mov OFFSETOF_RSP_REG(%rdi), %rsp
    sysretq

err_disabled:	// Target disabled
    /* Return error to caller in their enabled save area */
    mov dcb_current(%rip), %rdi
    mov OFFSETOF_DCB_DISP(%rdi), %rdi
    lea OFFSETOF_DISP_X86_64_ENABLED_AREA(%rdi), %rdi
    movq $SYS_ERR_LMP_TARGET_DISABLED, OFFSETOF_RAX_REG(%rdi)

    /* Yield to target (call dispatch(target) in C) */
    mov %rsi, %rdi /* rdi = target DCB */
    lea (x86_64_kernel_stack + X86_64_KERNEL_STACK_SIZE)(%rip), %rsp
    jmp dispatch /* no return */

err_buflen:     /* Target's endpoint buffer is full */
    /* Return error to caller in their enabled save area */
    mov dcb_current(%rip), %rdi
    mov OFFSETOF_DCB_DISP(%rdi), %rdi
    lea OFFSETOF_DISP_X86_64_ENABLED_AREA(%rdi), %rdi
    movq $SYS_ERR_LMP_BUF_OVERFLOW, OFFSETOF_RAX_REG(%rdi)

    /* Yield to target (call dispatch(target) in C) */
    mov %rsi, %rdi /* rdi = target DCB */
    lea (x86_64_kernel_stack + X86_64_KERNEL_STACK_SIZE)(%rip), %rsp
    jmp dispatch /* no return */

#ifdef CONFIG_SCHEDULER_RBED
lrpc_rbed_check_runnable:
	cmp	queue_tail(%rip), %rsi
	jne	lrpc_make_runnable
	jmp	lrpc_check_runnable_continue
#endif

lrpc_make_runnable:
	/* Save user stack */
	movq	%rsp, user_stack_save(%rip)

	/* Get kernel stack */
	lea	(x86_64_kernel_stack + X86_64_KERNEL_STACK_SIZE)(%rip), %rsp

	// Save complete register state
	pushq	%rdx
	pushq	%rcx
	pushq	%rbx
	pushq	%rax
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi

	// Call make runnable in C
	movq	%rsi, %rdi
	callq	make_runnable

	// Restore complete register state
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r8
	popq	%r9
	popq	%r10
	popq	%r11
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rax
	popq	%rbx
	popq	%rcx
	popq	%rdx

	/* Restore user stack */
	movq	user_stack_save(%rip), %rsp

	// Jump back
	jmp	lrpc_check_runnable_continue


/* regular syscall path */
syscall_path:
        /* Save user stack */
        movq    %rsp, user_stack_save(%rip)

        /* Get kernel stack */
        lea (x86_64_kernel_stack + X86_64_KERNEL_STACK_SIZE)(%rip), %rsp

        pushq   %rcx            /* Save user-space RIP */
        pushq   %r11            /* Save user-space RFLAGS */

        pushq   %rbx            /* arg11 */
        pushq   %rbp            /* arg10 */
        pushq   %rax            /* arg9 */
        pushq   %r15            /* arg8 */
        pushq   %r14            /* arg7 */
        pushq   %r13            /* arg6 */
        pushq   %r12            /* arg5 */
        pushq   %r9             /* arg4 */
        pushq   %r8             /* arg3 */
        pushq   %r10            /* arg2 in r10, NOT rcx from syscall */

        /* syscall number is in rdi (1st function argument) */
        /* arg0 is in rsi (2nd function argument) */
        /* arg1 is in rdx (3rd function argument) */
        movq    %r11, %r8   /* 5th function argument is user's flags */
        movq    %rcx, %r9   /* 6th function argument is user's IP */
        movq    %rsp, %rcx  /* 4th function argument is pointer to arg buffer */

        callq   sys_syscall     /* Process system call in C */

        addq    $0x50, %rsp     /* Remove buffer from stack */
        popq    %r11            /* Restore RFLAGS */
        popq    %rcx            /* Restore RIP */
        movq    user_stack_save(%rip), %rsp /* Restore user stack */
        sysretq             /* Return to user-space */

	.bss
	.comm	user_stack_save, 8
