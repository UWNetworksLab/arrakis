#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

int main(int argc, char *argv[])
{
  debug_printf("Hello world (debug_printf)\n");
  printf("Hello world (normal printf)\n");
  for (int i = 0;i < argc; i ++) {
    printf("arg[%d] = %s\n", i, argv[i]);
  }

  // Check that we're in privileged mode
  uint16_t cs;
  __asm volatile("mov %%cs, %[reg]"
		 : [reg] "=r" (cs));

  if((cs & 3) == 0) {
    printf("We're in privileged mode!\n");

    printf("Trying privileged operation...\n");
    uintptr_t cr0;
    __asm volatile("mov %%cr0, %[reg]"
		   : [reg] "=r" (cr0));

    printf("Succeeded! CR0 is %" PRIxPTR "\n", cr0);
  } else {
    printf("NO privileged mode enabled\n");
  }

  printf("Causing pagefault...\n");

  /* __asm volatile ("ud2"); */

  uintptr_t *zero = (uintptr_t *)0;
  *zero = 0;

  printf("After pagefault\n");

  return EXIT_SUCCESS;
}
