/* awiggins (2004-12-17): This code should really be moved into a generic
 *   Alpha PAL code library when someone can be bothered.
 */

#include <l4/alpha/asm.h>
#include <l4/alpha/pal.h>

BEGIN_PROC(pal_halt)
	call_pal PAL_halt
END_PROC(pal_halt)