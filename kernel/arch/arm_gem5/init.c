/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <init.h>
#include <exceptions.h>
#include <exec.h>
#include <offsets.h>
#include <paging_kernel_arch.h>
#include <phys_mmap.h>
#include <serial.h>
#include <stdio.h>
#include <arm_hal.h>
#include <cpiobin.h>
#include <getopt/getopt.h>
#include <cp15.h>
#include <elf/elf.h>
#include <arm_core_data.h>
#include <startup_arch.h>
#include <kernel_multiboot.h>
#include <global.h>
#include <start_aps.h>

#define GEM5_RAM_SIZE               (256UL*1024*1024)

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

/**
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
//static uint32_t addr_global;

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
__attribute__ ((aligned(8)));

/**
 * Boot-up L1 page table for addresses up to 2GB (translated by TTBR0)
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 16K after relocation
static union arm_l1_entry boot_l1_low[2*ARM_L1_MAX_ENTRIES]
__attribute__ ((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry * aligned_boot_l1_low;
/**
 * Boot-up L1 page table for addresses >=2GB (translated by TTBR1)
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 16K after relocation
static union arm_l1_entry boot_l1_high[2*ARM_L1_MAX_ENTRIES]
__attribute__ ((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry * aligned_boot_l1_high;

//
// ATAG boot header declarations
//
// See: http://www.simtec.co.uk/products/SWLINUX/files/booting_article.html
//

static const uint32_t ATAG_NONE      = 0;
static const uint32_t ATAG_CORE      = 0x54410001;
static const uint32_t ATAG_MEM       = 0x54410002;
static const uint32_t ATAG_VIDEOTEXT = 0x54410003;
static const uint32_t ATAG_RAMDISK   = 0x54410004;
static const uint32_t ATAG_INITRD2   = 0x54420005;
static const uint32_t ATAG_SERIAL    = 0x54410006;
static const uint32_t ATAG_REVISION  = 0x54410007;
static const uint32_t ATAG_VIDEOLFB  = 0x54410008;
static const uint32_t ATAG_CMDLINE   = 0x54410009;

struct atag_header {
    uint32_t size;              // Size of header plus payload in 32-bit words
    uint32_t tag;               // Payload identifier
};

struct atag_core {
    uint32_t flags;             // bit 0 = r/o
    uint32_t page_bytes;
    uint32_t root_device;
};

struct atag_mem {
    uint32_t bytes;
    uint32_t start;
};

struct atag_videotext {
    uint8_t  width;
    uint8_t  height;
    uint16_t video_page;
    uint8_t  video_mode;
    uint8_t  video_cols;
    uint16_t video_ega_bx;
    uint8_t  video_lines;
    uint8_t  video_isvga;
    uint16_t video_points;
};

struct atag_ramdisk {
    uint32_t flags;             // Bit 0 = load, bit 1 = prompt
    uint32_t bytes;             // Decompressed size
    uint32_t start;               // Starting block of RAM disk image
};

struct atag_initrd2 {
    uint32_t start;             // Physical start address
    uint32_t bytes;             // Copmressed disk image in bytes
};

struct atag_serial {
    uint32_t low;               // Lower order bits of board serial number
    uint32_t high;              // Upper order bits of board serial number
};

struct atag_revision {
    uint32_t board_revision;
};

struct atag_videolfb
{
    uint16_t lfb_width;
    uint16_t lfb_height;
    uint16_t lfb_depth;
    uint16_t lfb_linelength;
    uint32_t lfb_base;
    uint32_t lfb_size;
    uint8_t  red_size;
    uint8_t  red_pos;
    uint8_t  green_size;
    uint8_t  green_pos;
    uint8_t  bluint_te_size;
    uint8_t  bluint_te_pos;
    uint8_t  rsvd_size;
    uint8_t  rsvd_pos;
};

struct atag_cmdline
{
    char cmdline[1];
};

struct atag {
    struct atag_header header;
    union {
        struct atag_core         core;
        struct atag_mem          mem;
        struct atag_videotext    videotext;
        struct atag_ramdisk      ramdisk;
        struct atag_initrd2      initrd2;
        struct atag_serial       serial;
        struct atag_revision     revision;
        struct atag_videolfb     videolfb;
        struct atag_cmdline      cmdline;
    } u;
};


#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//

static int timeslice = 5;        //interval in ms in which the scheduler gets called

static struct cmdarg cmdargs[] = {
    { "consolePort",    ArgType_UInt, { .uinteger = &serial_console_port}},
    { "debugPort",      ArgType_UInt, { .uinteger = &serial_debug_port}},
    { "loglevel",       ArgType_Int, { .integer = &kernel_loglevel }},
    { "logmask",        ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    { "timeslice",      ArgType_Int, { .integer = &timeslice }},
    {NULL, 0, {NULL}}
};

static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
    __asm volatile (
        "add sp, sp, %[offset]\n\t" :: [offset] "r" (offset)
    );
}

static inline void __attribute__ ((always_inline))
relocate_got_base(lvaddr_t offset)
{
    __asm volatile (
        "add r10, r10, %[offset]\n\t" :: [offset] "r" (offset)
    );
}

#ifndef __gem5__
static void enable_cycle_counter_user_access(void)
{
    /* enable user-mode access to the performance counter*/
    __asm volatile ("mcr p15, 0, %0, C9, C14, 0\n\t" :: "r"(1));

    /* disable counter overflow interrupts (just in case)*/
    __asm volatile ("mcr p15, 0, %0, C9, C14, 2\n\t" :: "r"(0x8000000f));
}
#endif

static void paging_init(void)
{
    // configure system to use TTBR1 for VAs >= 2GB
    uint32_t ttbcr;
    ttbcr = cp15_read_ttbcr();
    ttbcr |= 1;
    cp15_write_ttbcr(ttbcr);

    // make sure pagetables are aligned to 16K
    aligned_boot_l1_low = (union arm_l1_entry *)ROUND_UP((uintptr_t)boot_l1_low, ARM_L1_ALIGN);
    aligned_boot_l1_high = (union arm_l1_entry *)ROUND_UP((uintptr_t)boot_l1_high, ARM_L1_ALIGN);

    lvaddr_t vbase = MEMORY_OFFSET, base = 0;

    for(size_t i=0; i < ARM_L1_MAX_ENTRIES/2; i++,
        base += ARM_L1_SECTION_BYTES, vbase += ARM_L1_SECTION_BYTES)
    {
        // create 1:1 mapping
        paging_map_kernel_section((uintptr_t)aligned_boot_l1_low, base, base);

        // Alias the same region at MEMORY_OFFSET
        paging_map_kernel_section((uintptr_t)aligned_boot_l1_high, vbase, base);
    }

    // Activate new page tables
    cp15_write_ttbr1((lpaddr_t)aligned_boot_l1_high);
    //cp15_write_ttbr0((lpaddr_t)&boot_l1_high[0]);
    cp15_write_ttbr0((lpaddr_t)aligned_boot_l1_low);
}

void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
    timeslice = CONSTRAIN(timeslice, 1, 20);
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It sets up exception handling,
 * initializes devices and enables interrupts. After that it
 * calls arm_kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void  __attribute__ ((noinline,noreturn)) text_init(void)
{
    errval_t errval;
    // Relocate glbl_core_data to "memory"
    glbl_core_data = (struct arm_core_data *)
        local_phys_to_mem((lpaddr_t)glbl_core_data);

    // Relocate global to "memory"
    global = (struct global*)local_phys_to_mem((lpaddr_t)global);

    // Map-out low memory
    if(glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP) {
        struct arm_coredata_mmap *mmap = (struct arm_coredata_mmap *)
                local_phys_to_mem(glbl_core_data->mmap_addr);
        paging_arm_reset(mmap->base_addr, mmap->length);
    } else {
        paging_arm_reset(PHYS_MEMORY_START, GEM5_RAM_SIZE);
    }

    exceptions_init();

    kernel_startup_early();

    //initialize console
     serial_console_init();

     // do not remove/change this printf: needed by regression harness
     printf("Barrelfish CPU driver starting on ARMv7 Board id 0x%08"PRIx32"\n", hal_get_board_id());
     printf("The address of paging_map_kernel_section is %p\n", paging_map_kernel_section);

     errval = serial_debug_init();
     if (err_is_fail(errval)) {
         printf("Failed to initialize debug port: %d", serial_debug_port);
     }

     my_core_id = hal_get_cpu_id();

     gic_init();

     if(hal_cpu_is_bsp()) {
         // init SCU if more than one core present
         if(scu_get_core_count() > 4) {
             panic("ARM SCU doesn't support more than 4 cores!");
         }
         if(scu_get_core_count() > 1) {
             scu_enable();
         }
     }

     pit_init(timeslice, 0);
     pit_init(timeslice, 1);
     tsc_init();

#ifndef __gem5__
     enable_cycle_counter_user_access();
     reset_cycle_counter();
#endif

     // tell BSP that we are started up
     uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
     *ap_wait = AP_STARTED;

     arm_kernel_startup();
}

/**
 * Entry point called from boot.S for bootstrap processor.
 * if is_bsp == true, then pointer points to multiboot_info
 * else pointer points to a global struct
 */

void arch_init(void *pointer)
{
    void __attribute__ ((noreturn)) (*reloc_text_init)(void) =
        (void *)local_phys_to_mem((lpaddr_t)text_init);

    struct Elf32_Shdr *rela, *symtab;
    struct arm_coredata_elf *elf = NULL;

    serial_early_init(serial_console_port);

    if(hal_cpu_is_bsp()) {
        struct multiboot_info *mb = (struct multiboot_info *)pointer;
        elf = (struct arm_coredata_elf *)&mb->syms.elf;
        memset(glbl_core_data, 0, sizeof(struct arm_core_data));
        glbl_core_data->start_free_ram =
                        ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte),
                                 BASE_PAGE_SIZE);

        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;

        // Construct the global structure
        memset(&global->locks, 0, sizeof(global->locks));
    } else {
        global = (struct global *)GLOBAL_VBASE;
        memset(&global->locks, 0, sizeof(global->locks));
        struct arm_core_data *core_data =
                (struct arm_core_data*)((lvaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
        glbl_core_data = core_data;
        glbl_core_data->cmdline = (lpaddr_t)&core_data->kernel_cmdline;
        my_core_id = core_data->dst_core_id;
        elf = &core_data->elf;
    }

    // XXX: print kernel address for debugging with gdb
    printf("Kernel starting at address 0x%"PRIxLVADDR"\n",
            local_phys_to_mem((uint32_t)&kernel_first_byte));

    // Find relocation section
    rela = elf32_find_section_header_type((struct Elf32_Shdr *)
                ((uintptr_t)elf->addr),
                elf->num, SHT_REL);

    if (rela == NULL) {
        panic("Kernel image does not include relocation section!");
    }

    // Find symtab section
    symtab = elf32_find_section_header_type((struct Elf32_Shdr *)(lpaddr_t)elf->addr,
                elf->num, SHT_DYNSYM);

    if (symtab == NULL) {
        panic("Kernel image does not include symbol table!");
    }

    paging_init();

    cp15_enable_mmu();

    // Relocate kernel image for top of memory
    elf32_relocate(MEMORY_OFFSET + (lvaddr_t)&kernel_first_byte,
            (lvaddr_t)&kernel_first_byte,
            (struct Elf32_Rel *)(rela->sh_addr - START_KERNEL_PHYS + &kernel_first_byte),
            rela->sh_size,
            (struct Elf32_Sym *)(symtab->sh_addr - START_KERNEL_PHYS + &kernel_first_byte),
            symtab->sh_size,
            START_KERNEL_PHYS, &kernel_first_byte);
    /*** Aliased kernel available now -- low memory still mapped ***/

    // Relocate stack to aliased location
    relocate_stack(MEMORY_OFFSET);

    //relocate got_base register to aliased location
    relocate_got_base(MEMORY_OFFSET);

    // Call aliased text_init() function and continue initialization
    reloc_text_init();
}
