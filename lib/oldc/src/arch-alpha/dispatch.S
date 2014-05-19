#include <l4/alpha/asm.h>
#include <l4/alpha/pal.h>
		
BEGIN_PROC(console_dispatch)
	.prologue 0
	subq	$30,80,$30
	stq	$26,0($30)
	stq	$29,8($30)

	stq	$8,16($30)	/* OpenVMS save regs */
	stq	$9,24($30)
	stq	$10,32($30)
	stq	$11,40($30)
	stq	$12,48($30)
	stq	$13,56($30)
	stq	$14,64($30)
	stq	$15,72($30)

	mov	$20, $1		/* HWRPB in a4 */	
	ldq	$2,0xc0($1)	/* crb offset */
	addq	$2,$1,$2	/* crb */
	ldq	$27,0($2)	/* dispatch procedure value */
	
	ldq	$2,8($27)	/* dispatch call address */
	jsr	$26,($2)	/* call it (weird VMS call seq) */

	ldq	$26,0($30)
	ldq	$29,8($30)

	ldq	$8,16($30)
	ldq	$9,24($30)
	ldq	$10,32($30)
	ldq	$11,40($30)
	ldq	$12,48($30)
	ldq	$13,56($30)
	ldq	$14,64($30)
	ldq	$15,72($30)

	addq	$30,80,$30
	ret	$31,($26)
END_PROC(console_dispatch)