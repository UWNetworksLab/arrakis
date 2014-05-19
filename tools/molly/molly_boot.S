	.text
	.globl	molly_start, molly_to_kernel_transition

        // ........................................
        //
        // Entry point.  Initialize a stack and branch to the
        // C entry point in molly_init.c
molly_start:
	lea	(molly_kernel_stack + 4096)(%rip), %rsp
	call	molly_init
        int3

        // Never reached
halt:	hlt
	jmp	halt

        // ........................................
        //
        // Transfer control to the proper kernel once it has
        // been loaded and relocated.  The caller supplies the
        // RAX RBX values through which the kernel expects
        // the multiboot magic and info.
molly_to_kernel_transition:     
        mov     %rsi, %rax
        mov     %rdx, %rbx
        call    *%rdi

        // ........................................
        //
        // Stack for use by the C molly kernel.
        
        .data
molly_kernel_stack:
        .skip   4096
        
        
