/*
 * Copyright (c) 2009,2013, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/**
 * \file
 * \brief heteropanda-specific setup code
 * assumes that there is a heteropanda_slave image,
 * sets up the cortex-m3 processor by:
 *      loading and relocating the heteropanda_slave image into memory
 *      setting the clock signals for the MMU and processor
 *      setting up the L2 MMU TLB to contain the mappings necessary to bootstrap
 *      writing the necessary entries in the vectortable
 *      actually starting the processor
 *
 *
 * XXX: please note that the current implementation is very heteropanda-specific.
 *      still, a more generic cortex-m3 initializer could follow this example to 
 *      set up the signals and TLB
 */

#include <kernel.h>
#include <string.h>
#include <exceptions.h>
#include <stdio.h>
#include <elf/elf.h>

#include <cortexm3_heteropanda.h>

#include <omap44xx_map.h>
#include <dev/omap/omap44xx_mmu_dev.h>

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

//only do something if heteropanda config option has been selected
#ifdef HETEROPANDA

omap44xx_mmu_t m3mmu;


//since in our current implementation the table will only be used twice,
//this can in principle be anywhere we want (but preferably page-aligned)
uint32_t vectortable_base;

genvaddr_t image_entry;//where the image entry point will end up loaded into


//initialize mackerel devices assuming direct access to physical addresses
void cortex_m3_early_init(void){
    omap44xx_mmu_initialize(&m3mmu, (mackerel_addr_t) OMAP44XX_MAP_M3_L2MMU);
}




//allocate memory in molly fashion
static lpaddr_t phys_alloc_next;//keeps changing
static lpaddr_t phys_alloc_base;//takes initial value
#define BASE_PAGE_SIZE 4096
static errval_t linear_alloc(void *s, genvaddr_t base, size_t size, uint32_t flags,
                             void **ret)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    /* *ret = (void *)(uintptr_t)base; */
    *ret = (void *)phys_alloc_next;

    phys_alloc_next += npages * BASE_PAGE_SIZE;
    return SYS_ERR_OK;
}




/*
 * \brief find, load, and relocate the elf image
 * the loading and relocation follows the example of arm_molly
 */
errval_t load_slave_image(void* start_image);
errval_t load_slave_image(void* start_image){
    printf("start address of slave image: 0x%x\n", start_image);
    printf("elf_virtual_size: 0x%x\n", elf_virtual_size((lvaddr_t)start_image));
    //I would expect the image to be somewhere around 24MB big
    
    // taken from arm_molly. load ELF image into memory
    
    phys_alloc_next = ROUND_UP(SLAVE_LOADED_START, 16*1024*1024);
    //currently aligning to 16MB, so we can simply map a supersection, facilitating TLB entries


    phys_alloc_base = phys_alloc_next;
    
    
    errval_t err = elf32_load(EM_ARM, linear_alloc, NULL, (uint32_t)start_image,
                    elf_virtual_size((lvaddr_t)start_image), &image_entry, NULL, NULL, NULL);
    if (err_is_fail(err)) {
        printf("failed to load heteropanda_slave image\n");
        return err;
    }
    
    // Relocate image, taken from molly_init

    struct Elf32_Ehdr *slave_head = (struct Elf32_Ehdr *)start_image;
    struct Elf32_Shdr *rela, *symtab, *symhead =
        (struct Elf32_Shdr *)(start_image + (uintptr_t)slave_head->e_shoff);
    genvaddr_t elfbase = elf_virtual_base32(slave_head);

    rela = elf32_find_section_header_type(symhead, slave_head->e_shnum, SHT_REL);
    symtab = elf32_find_section_header_type(symhead, slave_head->e_shnum, SHT_DYNSYM);
    //relocate to dest that the virtual(!) address will be
    //we want to map 1:1, so relocate to physical address 
    //(which will end up being the virtual one as well)
    //(this way the bootstrap code will not have to first figure out the mapping itself)
    elf32_relocate(phys_alloc_base, elfbase,
                   (struct Elf32_Rel *)(uintptr_t)(start_image + rela->sh_offset),
                   rela->sh_size,
                   (struct Elf32_Sym *)(uintptr_t)(start_image + symtab->sh_offset),
                   symtab->sh_size,
                   elfbase, (void *)phys_alloc_base);

    image_entry = image_entry - elfbase;
    
    printf("entry point address of relocated slave image: 0x%llx\n",image_entry);
    return err;
}


/*
 * \brief set up the control signals for the cortex-m3 and its MMU
 */
void set_signals(void);
void set_signals(void){
    //setting from auto to nosleep (which might not actually be necessary)
    *((uint32_t *) CM_MPU_M3_CLKSTCTRL) = 0x00;

    //setting mandatory clocks from disabled to auto
    *((uint32_t *) CM_MPU_M3_MPU_M3_CLKCTRL) |= 0x01;
    
    *((uint32_t *) RM_MPU_M3_RSTCTRL) &= ~0x4;
    
    while (omap44xx_mmu_sysstatus_rd(&m3mmu) == 0){
        printf("waiting for M3 MMU status to become 1: 0x%x\n");
    }
    
    //disable idle mode, probably not necessary
    omap44xx_mmu_sysconfig_idlemode_wrf(&m3mmu, (omap44xx_mmu_idle_mode_t) 1);
    
    //enable MMU
    omap44xx_mmu_cntl_mmu_wrf(&m3mmu, (omap44xx_mmu_mmu_feature_status_t) 1);
}
 

/*
 * \brief set up some mappings in the TLB, so that the bootstrap code can run
 * (very heteropanda-specific)
 */
void set_tlb_mappings(void);
void set_tlb_mappings(void){
    printf("setting TLB mappings\n");
/*
    trying to set TLB entry 0 does not work as expected.
    it seems to actually write entry 1 instead, so for the moment we just
    ignore entry 0 and start our mapping on entry 1
    (the lock register seems a bit strange in general, with reads giving values that
    obviously are not what is actually in there)
    */
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu, 1);
    
    //first mapping: vectortable page (need virtual address 0)
    //virtual address to be translated
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, 0);//map virtual address 0 (vectortable)
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    //only map a small page -- we only need the first few bytes anyway
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 2);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    //corresponding physical address; NOTE: need to cut last 12 bits
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, vectortable_base>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    //no size conversion
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB

    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,2);//lock first few entries
    //all other entries will have a 1:1 mapping, so the kernel does not run on virtual memory
    
    //second mapping: 
    //map second supersection with the code in it
    //XXX: HACK: as experimentally determined, this is where the code to flush the TLB
    //will end up in, so until we have caching, this entry needs to be preserved
    //(which is also why we put it as the second entry here)
    
    //virtual address to be translated
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu,  (phys_alloc_base+0x1000000)>>12);
    //XXX hack: mark it as preserved, so we can use it to flush other pages
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 1);
    //map a supersection
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 3);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    //corresponding physical address NOTE: need to cut last 12 bits
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, (phys_alloc_base+0x1000000)>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_mixed_wrf(&m3mmu, 1);//use CPU size 
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,3);//lock first few entries
    //from here on, the order of entries does not matter anymore
    
    //third entry: slave image
    //virtual address to be translated
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, phys_alloc_base>>12);
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    //map a supersection
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 3);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    //corresponding physical address
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, phys_alloc_base>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,4);//lock first few entries
    
    //fourth entry: used for LED blinking
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, OMAP44XX_MAP_L4_WKUP_GPIO1>>12);
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    //small page is enough
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 2);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, OMAP44XX_MAP_L4_WKUP_GPIO1>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,5);//lock first few entries
    
    //fifth entry: map the page with all the UART3 stuff, needed for serial output
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, OMAP44XX_MAP_L4_PER_UART3>>12);
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    //small page is enough
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 2);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, OMAP44XX_MAP_L4_PER_UART3>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,6);//lock first few entries
    
    //sixth entry: page with the spinlock module, so the M3 can use it from the start
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, OMAP44XX_MAP_L4_CFG_SPINLOCK>>12);
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 2);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu,OMAP44XX_MAP_L4_CFG_SPINLOCK>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,7);//lock first few entries
    
    //seventh entry: map another supersection for the kernel, just so we can be sure we 
    //don't run out of space prematurely ()
    omap44xx_mmu_cam_virtual_tag_wrf(&m3mmu, (phys_alloc_base+0x2000000)>>12);
    omap44xx_mmu_cam_preserved_wrf(&m3mmu, 0);
    omap44xx_mmu_cam_page_size_wrf(&m3mmu, (omap44xx_mmu_page_size_t) 3);
    omap44xx_mmu_cam_valid_wrf(&m3mmu, 1);
    omap44xx_mmu_ram_physical_address_wrf(&m3mmu, (phys_alloc_base+0x2000000)>>12);
    omap44xx_mmu_ram_endianness_wrf(&m3mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&m3mmu, (omap44xx_mmu_page_element_size_t) 3);
    omap44xx_mmu_ld_tlb_wr(&m3mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    
    omap44xx_mmu_lock_basevalue_wrf(&m3mmu,8);//lock first few entries
}
 
/*
 * \brief set up and start the cortex-m3
 * to run the heteropanda_slave image
 */
void prepare_and_start_m3(void* start_image){
    printf("setting up cortex-m3\n");
    
    errval_t err = load_slave_image(start_image);
    if (err_is_fail(err)){
        printf("failed to load slave image into memory\n");
        return;
    }
    
    set_signals();
    
    //put vectortable one page before image.
    //(this memory will only be accessed twice in the kernel bootstrap phase, so it does 
    //not really matter if it is out of the m3's mmap range)
    vectortable_base = phys_alloc_base - BASE_PAGE_SIZE;
    
    set_tlb_mappings();//set up the TLB entries.
    
    
    //write initial vectortable entries. lsb of all handler entries MUST be 1 (thumb mode)
    
    //initial stack pointer (same page as vectortable, will probably never be accessed)
    *((uint32_t*) vectortable_base) = 1024;
    //reset vector: virtual address of first instruction in image
    *(((uint32_t*) vectortable_base)+1) = phys_alloc_base + image_entry + 1;
    //hardfault vector -- should not actually be necessary
    //give unmapped address, so we get a MMU fault for it
    *(((uint32_t*) vectortable_base)+3) = 0xFACE +1;

    
    printf("finished setting up cortex-m3. starting processor.\n");
    //get M3 itself out of reset
    *((uint32_t *) RM_MPU_M3_RSTCTRL) &= ~1;
}
#endif //defined (HETEROPANDA)
