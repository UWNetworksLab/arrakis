/*
 * Copyright (c) 2009-2013, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/**
 * \file
 * \brief cortex-m3 CPU driver init code for the OMAP44xx series SoCs.
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
#include <spinlock.h>
#include <stdio.h>
#include <arm_hal.h>
#include <getopt/getopt.h>
#include <elf/elf.h>
#include <arm_core_data.h>
#include <startup_arch.h>
#include <kernel_multiboot.h>
#include <global.h>
#include <start_aps.h> // AP_WAIT_*, AUX_CORE_BOOT_*  and friends

#include <omap44xx_map.h>
#include <dev/omap/omap44xx_id_dev.h>
#include <dev/omap/omap44xx_gpio_dev.h>

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

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//
//XXX: SysTick actually counts cycles, so time can at most be estimated
static int timeslice  = 5; //interval in ms in which the scheduler gets called

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
		    "add	sp, sp, %[offset]\n\t" ::[offset] "r" (offset)
		    );
}

static inline void __attribute__ ((always_inline))
relocate_got_base(lvaddr_t offset)
{
    __asm volatile (
		    "add	r10, r10, %[offset]\n\t" ::[offset] "r" (offset)
		    );
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
 * initializes devices and transitions into handler mode.
 */
static void  __attribute__ ((noinline,noreturn)) text_init(void)
{
    errval_t errval;

    if ((glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP)) {
        // BSP core: set final page tables
        struct arm_coredata_mmap *mmap = (struct arm_coredata_mmap *)
            local_phys_to_mem(glbl_core_data->mmap_addr);
        paging_arm_reset(mmap->base_addr, mmap->length);
        //printf("paging_arm_reset: base: 0x%"PRIx64", length: 0x%"PRIx64".\n", mmap->base_addr, mmap->length);
    } else {
        // AP core
        //  FIXME: Not sure what to do, so map the whole memory for now
        paging_arm_reset(PHYS_MEMORY_START, 0x40000000);
    }


    //printf("startup_early\n");
    kernel_startup_early();
    //printf("kernel_startup_early done!\n");

    //initialize console
    serial_init(serial_console_port);
    spinlock_init();

    printf("Barrelfish CPU driver starting on ARMv7-M OMAP44xx"
           " Board id 0x%08"PRIx32"\n", hal_get_board_id());
    printf("The address of paging_map_kernel_section is %p\n",
           paging_map_kernel_section);

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
            printf("Failed to initialize debug port: %d", serial_debug_port);
    }

    if (my_core_id != hal_get_cpu_id()) {
        printf("** setting my_core_id (="PRIuCOREID") to match hal_get_cpu_id() (=%u)\n");
        my_core_id = hal_get_cpu_id();
    }

    // Test MMU by remapping the device identifier and reading it using a
    // virtual address
    lpaddr_t id_code_section = OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE & ~ARM_L1_SECTION_MASK;
    lvaddr_t id_code_remapped = paging_map_device(id_code_section,
                                                  ARM_L1_SECTION_BYTES);
    omap44xx_id_t id;
    omap44xx_id_initialize(&id, (mackerel_addr_t)(id_code_remapped +
	     (OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE & ARM_L1_SECTION_MASK)));

    char buf[200];
    omap44xx_id_code_pr(buf,200,&id);
    printf("Using MMU, %s", buf);

    
    nvic_init();
    printf("nvic_init done\n");
    //XXX: cachemarker: these counts are intended for no caching, adjust if we have caching
    //systick_init(0xFFFFFF);//maximum value
    //systick_init(0x0AC000);//absolute minimum for -O2: very little progress and much thrashing
    systick_init(0x0C0000);//reasonable: many interrupts, no thrashing (with -O2)
    printf("systick_init done\n");
    

    //transition into handler mode, will call text_init_continued
    exceptions_early_init();
    panic("exceptions_early_init has returned. this should never happen!");
}

/*
 * \brief last bit of initialization before calling arm_kernel_startup()
 *  called by a special exception handler -> we are now in handler mode
 *  properly set up exceptions here
 */
void  __attribute__ ((noinline,noreturn)) text_init_continued(void){
    printf("entered text_init_continued - we are now in handler mode\n");
    
    //now all devices should have been mapped -> release the lock on their TLB entries
    set_tlb_lock_basevalue(0);
    
    //now is probably the time to set up the vectortable?
    printf("overwriting TLB mapping for page 0 (vectortable).\n");
    printf("physical address of vectortable: 0x%x\n", (uint32_t) &vectortable);
    add_tlb_mapping(0, (lpaddr_t) &vectortable, 1, 2);
    printf("mapped vectortable page to address 0 in TLB.\n");
    set_tlb_lock_basevalue(2);//XXX: hack, to make sure the already preserved entry in there stays preserved
    //(entry 1 is a preserved mapping of the section containing the TLB flush code)
    //XXX: cachemarker: as soon as caching is up, we can flush the TLB safely
    // -> remove the preserved-bit for the entry with the flushing code on
    
    
    exceptions_init();//set up proper exception handlers in the relocated vectortable
    
    printf("starting SysTick timer\n");
    systick_start();
    arm_kernel_startup();
}


/*
 * Doesn't work yet on the second LED for some reason...
 */
static void set_leds(void)
{
    uint32_t r, nr;
    omap44xx_gpio_t g;
    //char buf[8001];

    omap44xx_gpio_initialize(&g, (mackerel_addr_t)OMAP44XX_MAP_L4_WKUP_GPIO1);
    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1<<8));
    omap44xx_gpio_oe_wr(&g,r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g);
    nr = r  |(1<<8); 
    for(int i = 0; i < 5; i++) {
	omap44xx_gpio_dataout_wr(&g,r);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
	omap44xx_gpio_dataout_wr(&g,nr);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
    }
    return;

    omap44xx_gpio_initialize(&g, (mackerel_addr_t)OMAP44XX_MAP_L4_PER_GPIO4);

    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1<<14));
    omap44xx_gpio_oe_wr(&g,r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g);
    nr = r  |(1<<14); 
    for(int i = 0; i < 100; i++) {
	omap44xx_gpio_dataout_wr(&g,r);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
	omap44xx_gpio_dataout_wr(&g,nr);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
    }
}

/**
 * Entry point called from boot.S for bootstrap processor.
 * if is_bsp == true, then pointer points to multiboot_info
 * else pointer points to a global struct
 */
void arch_init(void *pointer)
{

    serial_early_init(serial_console_port);
    spinlock_early_init();//from here on we can safely use printf

#if 0 //XXX: HACK: we currently are seperate from the other cores, so we can not use
        // either of the "normal" cases
    if (hal_cpu_is_bsp()) {
        struct multiboot_info *mb = pointer;

        memset(glbl_core_data, 0, sizeof(struct arm_core_data));

        size_t max_addr = max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte);
        glbl_core_data->start_free_ram = ROUND_UP(max_addr, BASE_PAGE_SIZE);
        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;

        memset(&global->locks, 0, sizeof(global->locks));
    } else {
        global = (struct global *)GLOBAL_VBASE;
        // zeroing locks for the app core seems bogus to me --AKK
        //memset(&global->locks, 0, sizeof(global->locks));

        // our core data (struct arm_core_data) is placed one page before the
        // first byte of the kernel image
        glbl_core_data = (struct arm_core_data *)
                            ((lpaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
        glbl_core_data->cmdline = (lpaddr_t)&glbl_core_data->kernel_cmdline;
        my_core_id = glbl_core_data->dst_core_id;

        // tell BSP that we are started up
        // See Section 27.4.4 in the OMAP44xx manual for how this should work.
        // we do this early, to avoid having to map the registers
        lpaddr_t aux_core_boot_0 = AUX_CORE_BOOT_0;
        lpaddr_t ap_wait = AP_WAIT_PHYS;

        *((volatile lvaddr_t *)aux_core_boot_0) = 2<<2;
        //__sync_synchronize();
        *((volatile lvaddr_t *)ap_wait) = AP_STARTED;

    }
#else    
    //XXX: HACK
    //since the M3 currently thinks it is a bsp core, we do most of the bsp stuff
    //exept for truly global variables
    
    //not sure what address to use for our core_data
    glbl_core_data = (struct arm_core_data*)((lvaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
    
    memset(glbl_core_data, 0, sizeof(struct arm_core_data));
       
    struct multiboot_info *mb = (struct multiboot_info *)pointer;
    
	glbl_core_data->start_free_ram =
	                ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte),
	                         BASE_PAGE_SIZE);

    glbl_core_data->mods_addr = mb->mods_addr;
    glbl_core_data->mods_count = mb->mods_count;
    glbl_core_data->cmdline = mb->cmdline;
    glbl_core_data->mmap_length = mb->mmap_length;
    glbl_core_data->mmap_addr = mb->mmap_addr;
    glbl_core_data->multiboot_flags = mb->flags;
    
//    global = (struct global *)GLOBAL_VBASE;//we currently do not need global
#endif //0


    // XXX: print kernel address for debugging with gdb
    printf("Barrelfish OMAP44xx cortex-m3 CPU driver starting at addr 0x%"PRIxLVADDR"\n", 
	   local_phys_to_mem((uint32_t)&kernel_first_byte));


    if (1) {
        set_leds();
    }

    //we already are in a virtual address space, so we do not have to do MMU stuff already
    text_init();
}
