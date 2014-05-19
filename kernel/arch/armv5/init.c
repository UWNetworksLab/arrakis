/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
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
    uint32_t start;             // Starting block of RAM disk image
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

static struct atag * atag_find(struct atag *a, uint32_t tag)
{
    while (a->header.tag != ATAG_NONE) {
        if (a->header.tag == tag) {
            return a;
        }
        a = (struct atag*)(a->header.size + (uint32_t*)a);
    }
    return NULL;
}

//
// Macros used in command-line processing
//

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//

static int tick_hz                   = 100;

static struct cmdarg cmdargs[] = {
    { "consolePort",    ArgType_UInt, { .uinteger = &serial_console_port}},
    { "debugPort",      ArgType_UInt, { .uinteger = &serial_debug_port}},
    { "loglevel",       ArgType_Int, { .integer = &kernel_loglevel }},
    { "logmask",        ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    { "tickHz",         ArgType_Int, { .integer = &tick_hz }},
    {NULL, 0, {NULL}}
};

/**
 * Entry point called from boot.S for bootstrap processor.
 */
void arch_init(uint32_t     board_id,
               struct atag *atag_base,
               lvaddr_t     elf_file,
               lvaddr_t     alloc_top)
{
    //
    // Assumptions:
    //
    // - MMU and caches are enabled. No lockdowns in caches or TLB.
    // - Kernel has own section starting at KERNEL_OFFSET.
    // - Kernel section includes the highmem relocated exception vector table.
    //


    struct atag * ae = NULL;

    exceptions_init();

    ae = atag_find(atag_base, ATAG_MEM);
    paging_map_memory(0, ae->u.mem.start, ae->u.mem.bytes);

    ae = atag_find(atag_base, ATAG_CMDLINE);
    if (ae != NULL)
    {
        parse_commandline(ae->u.cmdline.cmdline, cmdargs);
        tick_hz = CONSTRAIN(tick_hz, 10, 1000);
    }

    if (board_id == hal_get_board_id())
    {
        errval_t errval;

        serial_console_init();

        // do not remove/change this printf: needed by regression harness
        printf("Barrelfish CPU driver starting on ARMv5 Board id 0x%08"PRIx32"\n",
               board_id);
        printf("The address of paging_map_kernel_section is %p\n", 
               paging_map_kernel_section);
        errval = serial_debug_init();
        if (err_is_fail(errval))
        {
            printf("Failed to initialize debug port: %d", serial_debug_port);
        }

        debug(SUBSYS_STARTUP, "alloc_top %08"PRIxLVADDR" %08"PRIxLVADDR"\n",
               alloc_top, alloc_top - KERNEL_OFFSET);
        debug(SUBSYS_STARTUP, "elf_file %08"PRIxLVADDR"\n", elf_file);

        my_core_id = hal_get_cpu_id();
        
        pic_init();
        pit_init(tick_hz);
        tsc_init();

        ae = atag_find(atag_base, ATAG_MEM);
                
        // Add unused physical memory to memory map

        phys_mmap_t phys_mmap;

        // Kernel effectively consumes [0...alloc_top]
        // Add region above alloc_top with care to skip exception vector
        // page.
        phys_mmap_add(&phys_mmap,
                      alloc_top - KERNEL_OFFSET,
                      ETABLE_ADDR - KERNEL_OFFSET);

        phys_mmap_add(&phys_mmap,
                      ETABLE_ADDR - KERNEL_OFFSET + BASE_PAGE_SIZE,
                      ae->u.mem.start + ae->u.mem.bytes);

        ae = atag_find(atag_base, ATAG_VIDEOLFB);
        if (NULL != ae)
        {
            // Remove frame buffer (if present).
            phys_mmap_remove(&phys_mmap,
                             ae->u.videolfb.lfb_base,
                             ae->u.videolfb.lfb_base + ae->u.videolfb.lfb_size);
            assert(!"Not supported");
        }

        ae = atag_find(atag_base, ATAG_INITRD2);
        if (NULL != ae)
        {
            phys_mmap_remove(&phys_mmap,
                             ae->u.initrd2.start,
                             ae->u.initrd2.start + ae->u.initrd2.bytes);

            arm_kernel_startup(&phys_mmap,
                               ae->u.initrd2.start,
                               ae->u.initrd2.bytes);
        }
        else {
            panic("initrd not found\n");
        }
    }
    else {
        panic("Mis-matched board id: [current %"PRIu32", kernel %"PRIu32"]",
              board_id, hal_get_board_id());
    }
}
