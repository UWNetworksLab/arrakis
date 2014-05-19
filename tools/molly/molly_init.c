// Simple boot-loader.  
//
// This code is only intended for use on M5 where it is started via
// molly_boot.S which runs on Core 0.  The simulator starts Core 0
// in 64-bit mode already, so typical initialization code is not 
// needed.

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>
#include "../../kernel/include/multiboot.h"

// Prototypes for functions from molly_boot.S:
extern void molly_to_kernel_transition(void *entry_addr,
                                       uint64_t RAX,
                                       uint64_t RBX);

// Prototypes for functions from m5_kernel.c: 
extern struct multiboot_info *molly_get_mbi(void);
//struct multiboot_info *molly_get_mbi(void) { return NULL; }

// Prototypes for symbols declared via linker script:
extern void *_start_img;
extern void *_end_img;

//......................................................................
//
// Basic debugging output functions.  These assume M5 with the serial 
// port already initialized, and with flow control unnecessary.

static inline void outb(int port, char data)
{
    __asm __volatile("outb %0,%%dx" : : "a" (data), "d" (port));
}

static int serial_portbase = 0x3f8; // COM1

static void serial_putchar(char c)
{
  outb(serial_portbase, c);
}

static void putstr(const char *cp) {
  char c;
  while ((c=*(cp++))) {
    serial_putchar(c);
  }
}

static const char hexdigits[] = "0123456789abcdef";

static void puthex64(unsigned long long x) {
  putstr("0x");
  unsigned long long mask = 0xf000000000000000;
  int s = 60;
  do {
    unsigned int idx = (x & mask) >> s;
    serial_putchar(hexdigits[idx]);
    s -= 4;
    mask >>= 4;
  } while (s >= 0);  
}

static void puthex32(uint32_t x) {
  putstr("0x");
  uint32_t mask = 0xf0000000;
  int s = 28;
  do {
    unsigned int idx = (x & mask) >> s;
    serial_putchar(hexdigits[idx]);
    s -= 4;
    mask >>= 4;
  } while (s >= 0);  
}

static void puthex8(uint32_t x) {
  uint32_t mask = 0xf0;
  int s = 4;
  do {
    unsigned int idx = (x & mask) >> s;
    serial_putchar(hexdigits[idx]);
    s -= 4;
    mask >>= 4;
  } while (s >= 0);  
}

static void putptr64(void *ptr) {
  puthex64((unsigned long long)ptr);
}

static void putstrptr64(char *str, void *ptr) {
  putstr(str);
  putptr64(ptr);
  putstr("\n");
}


//......................................................................
//
// Basic libc functionality needed 

void __assert(const char *exp, const char *file, const char *func, int line)
{
  putstr("Assertion failure: ");
  putstr(exp);
  putstr(" ");
  putstr(file);
  putstr(" ");
  putstr(func);
  putstr(" ");
  puthex32(line);
  putstr("\n");
  while(1){}
}

int printf(const char *fmt, ...)
{
  putstr(fmt);
}

void *
memset (void *s, int c, size_t n)
{
    uint8_t *p = (uint8_t *)s;
    for (size_t m = 0; m < n; m++) {
        *p++ = c;
    }
    return s;
}

void *
memcpy(void *dst, const void *src, size_t len)
{
    char *d = dst;
    const char *s = src;

    /* check that we don't overlap (should use memmove()) */
    assert((src < dst && src + len <= dst) || (dst < src && dst + len <= src));

    while (len--)
        *d++ = *s++;

    return dst;
}

char *
strrchr(const char *s, int c)
{
    unsigned int i;

    if(strlen(s) == 0)
        return NULL;

    for(i = strlen(s) - 1; i >= 0; i--) {
        if(s[i] == c)
            return (char *)&s[i];
    }

    return NULL;
}

int
strncmp(const char *s1, const char *s2, size_t n)
{
    int result;

    for(unsigned int i = 0; i < n; i++) {
        if((result = s2[i] - s1[i]) != 0) {
            return result;
        }

        if(s1[i] == '\0' || s2[i] == '\0') {
            break;
        }
    }

    return 0;
}

size_t
strlen(const char *s)
{
    size_t i = 0;

    while (*s != '\0') {
        i++;
        s++;
    }

    return i;
}

int
strcmp(const char* a, const char* b)
{
    while (*a == *b && *a != '\0')
    {
        a++;
        b++;
    }

    return *a - *b;
}

//......................................................................
//
// Allocation function used from the ELF loader

#define BASE_PAGE_SIZE 4096

// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

static uint64_t next_addr;

static errval_t linear_alloc(void *s, 
                             genvaddr_t base, 
                             size_t size, 
                             uint32_t flags,
                             void **ret)
{
  // round to base page size
  uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;
  
  /* *ret = (void *)(uintptr_t)base; */
  *ret = (void *)next_addr;
  
  next_addr += npages * BASE_PAGE_SIZE;
  return SYS_ERR_OK;
}

// Helper function to check that addresses assumed to be 32-bit do not
// have high bits set.

static uint32_t ptr_to_uint32(void *ptr) {
  uint64_t temp = (uint64_t)ptr;
  assert(temp < 0x7fffffff); 
  return (uint32_t) temp;
}

// C code entered from molly_boot.S:

void molly_init(void) {
  putstr("......................................................................\n");
  putstr("molly_init:\n");
  putstrptr64("  boot image start : ", &_start_img);
  putstrptr64("  boot image end   : ", &_end_img);

  struct multiboot_info *mbi = molly_get_mbi();
  putstrptr64("  multiboot info   : ", mbi);

  // Start allocating from one page beyond the boot image:
  next_addr = (ROUND_UP((uint64_t) &_end_img, BASE_PAGE_SIZE) + 
               BASE_PAGE_SIZE);
  putstrptr64("  allocating from  : ", (void*)next_addr);

  // Load the kernel out from the boot image:
  struct multiboot_modinfo *mbi_mods;
  mbi_mods = (struct multiboot_modinfo*)(uint64_t)(mbi->mods_addr);
  void *kernel = (void*)(uint64_t)(mbi_mods[0].mod_start);
  uint32_t kernel_bytes = mbi_mods[0].mod_end - mbi_mods[0].mod_start;
  putstrptr64("  kernel start     : ", kernel);
  putstrptr64("  kernel bytes     : ", (void*)(uint64_t)kernel_bytes);

  void *kernel_entry = NULL;
  lpaddr_t kernel_start = next_addr;
  genvaddr_t tls_base = 0;
  size_t tls_init_len = 0;
  size_t tls_total_len = 0;
  errval_t err = elf64_load(EM_X86_64, 
                            linear_alloc, NULL, 
                            (uint64_t) kernel, kernel_bytes,
                            (genvaddr_t *) &kernel_entry,
                            &tls_base,
                            &tls_init_len,
                            &tls_total_len);
  if (err_is_fail(err)) {
    putstr("Kernel loading failed\n");
    return;
  }
  putstrptr64("  kernel entry pt  : ", kernel_entry);

  // Relocate kernel image
  struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)kernel;
  struct Elf64_Shdr *rela, *symtab, *symhead =
    (struct Elf64_Shdr *)(kernel + (uintptr_t)cpu_head->e_shoff);
  genvaddr_t elfbase = elf_virtual_base64(cpu_head);
  rela = elf64_find_section_header_type(symhead, 
                                        cpu_head->e_shnum, 
                                        SHT_RELA);
  symtab = elf64_find_section_header_type(symhead, 
                                          cpu_head->e_shnum, 
                                          SHT_DYNSYM);
  elf64_relocate(kernel_start, elfbase,
                 (struct Elf64_Rela *)(uintptr_t)(kernel + rela->sh_offset),
                 rela->sh_size,
                 (struct Elf64_Sym *)(uintptr_t)(kernel + symtab->sh_offset),
                 symtab->sh_size,
                 elfbase, (void *)kernel_start);
  kernel_entry = kernel_entry - elfbase + kernel_start;
  putstrptr64("  ...relocated to  : ", kernel_entry);

  // Initialize multiboot symbol information for the 
  // relocated kernel:
  mbi->syms.elf.num = cpu_head->e_shnum;
  mbi->syms.elf.size = cpu_head->e_shentsize;
  mbi->syms.elf.addr = ptr_to_uint32(kernel) + cpu_head->e_shoff;
  mbi->syms.elf.shndx = cpu_head->e_shstrndx;
 
  putstr("......................................................................\n");

  molly_to_kernel_transition(kernel_entry,
                             MULTIBOOT_INFO_MAGIC,
                             ptr_to_uint32(mbi)
                             );
}

