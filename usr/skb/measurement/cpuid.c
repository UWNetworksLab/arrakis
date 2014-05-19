/**
 * \file
 * \brief Adds information from CPUID for the core it is running on to the SKB
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include "cpuid_dev.h"
#include <cpuid/cpuid.h>
#include <skb/skb.h>
#include "datagatherer.h"



/******************************************************************************/
// Helpers
/******************************************************************************/

static uint8_t nr_bits_needed(uint8_t count)
{
    uint8_t mask = 0x80;
    uint8_t cnt = 8;

    while ((cnt > 0) && ((mask & count) != mask)) {
        mask >>= 1;
        cnt--;
    }
    return (cnt);
}


/******************************************************************************/
// AMD
/******************************************************************************/

static void add_amd_cache_info(struct cpuid_t id, coreid_t coreid)
{
// information for AMD's L1 TLB and AMD's L1 cache

    // tlb(coreid, level, data | instruction, associativitycode, nrentries,
    //     pagesize).
    cpuid_amd_tlbinfo1_t tlb1 = cpuid_l1_24m_rd(&id);
    skb_add_fact("tlb(%hhu, 1, data, %hhu, %hhu, %u).",
                 coreid, tlb1.dtlb_asc, tlb1.dtlb_sz, 2 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 1, data, %hhu, %hhu, %u).",
                 coreid, tlb1.dtlb_asc, tlb1.dtlb_sz >> 1, 4 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 1, instruction, %hhu, %hhu, %u).",
                 coreid, tlb1.itlb_asc, tlb1.itlb_sz, 2 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 1, instruction, %hhu, %hhu, %u).",
                 coreid, tlb1.itlb_asc, tlb1.itlb_sz >> 1, 4 * 1024 * 1024);

    tlb1 = cpuid_l1_4k_rd(&id);
    skb_add_fact("tlb(%hhu, 1, data, %hhu, %hhu, %u).",
                 coreid, tlb1.dtlb_asc, tlb1.dtlb_sz, 4 * 1024);
    skb_add_fact("tlb(%hhu, 1, instruction, %hhu, %hhu, %u).",
                 coreid, tlb1.itlb_asc, tlb1.itlb_sz, 4 * 1024);

    //cache(name, coreid, level, data | instruction, size, associativitycode,
    //      linesize, linespertag).
    cpuid_amd_cacheinfo1_t cache1 = cpuid_l1_dci_rd(&id);
    skb_add_fact("cache(l1d, %hhu, 1, data, %u, %hhu, %hhu, %hhu).",
                 coreid, cache1.size * 1024, cache1.assoc,
                 cache1.linesize, cache1.lpt);

    cache1 = cpuid_l1_ici_rd(&id);
    skb_add_fact("cache(l1i, %hhu, 1, instruction, %u, %hhu, %hhu, %hhu).",
                 coreid, cache1.size * 1024, cache1.assoc,
                 cache1.linesize, cache1.lpt);

    //associativity_encoding(vendor,level,associativitycode,
    //                       reserved | disabled | direct | full | associativity).
    skb_add_fact("associativity_encoding(amd, 1, 0, reserved).");
    skb_add_fact("associativity_encoding(amd, 1, 1, direct).");
    skb_add_fact("associativity_encoding(amd, 1, 255, full).");
    for (int i = 2; i <= 0xfe; i++) {
        skb_add_fact("associativity_encoding(amd, 1, %d, %d).", i, i);
    }



// information for AMD's L2 TLB and AMD's L2 and L3 cache
    cpuid_amd_tlbinfo23_t tlb23 = cpuid_l2_24m_rd(&id);
    skb_add_fact("tlb(%hhu, 2, data, %hhu, %hhu, %u).",
                 coreid, tlb23.dtlb_asc, tlb23.dtlb_sz, 2 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 2, data, %hhu, %hhu, %u).",
                 coreid, tlb23.dtlb_asc, tlb23.dtlb_sz >> 1, 4 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 2, instruction, %hhu, %hhu, %u).",
                 coreid, tlb23.itlb_asc, tlb23.itlb_sz, 2 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 2, instruction, %hhu, %hhu, %u).",
                 coreid, tlb23.itlb_asc, tlb23.itlb_sz >> 1, 4 * 1024 * 1024);

    tlb23 = cpuid_l2_4k_rd(&id);
    skb_add_fact("tlb(%hhu, 2, data, %hhu, %hhu, %u).",
                 coreid, tlb23.dtlb_asc, tlb23.dtlb_sz, 4 * 1024);
    skb_add_fact("tlb(%hhu, 2, instruction, %hhu, %hhu, %u).",
                 coreid, tlb23.itlb_asc, tlb23.itlb_sz, 4 * 1024);


    cpuid_amd_cacheinfo23_t cache23 = cpuid_l2_ci_rd(&id);
    skb_add_fact("cache(l2, %hhu, 2, instruction, %u, %hhu, %hhu, %hhu).",
                 coreid, cache23.size * 1024, cache23.assoc,
                 cache23.linesize, cache23.lpt);
    skb_add_fact("cache(l2, %hhu, 2, data, %u, %hhu, %hhu, %hhu).",
                 coreid, cache23.size * 1024, cache23.assoc,
                 cache23.linesize, cache23.lpt);

    cache23 = cpuid_l3_ci_rd(&id);
    skb_add_fact("cache(l3, %hhu, 3, instruction, %u, %hhu, %hhu, %hhu).",
                 coreid, cache23.size * 512 * 1024, cache23.assoc,
                 cache23.linesize, cache23.lpt);
    skb_add_fact("cache(l3, %hhu, 3, data, %u, %hhu, %hhu, %hhu).",
                 coreid, cache23.size * 512 * 1024, cache23.assoc,
                 cache23.linesize, cache23.lpt);

//1G-page TLB
    tlb23 = cpuid_l1_1G_rd(&id);
    skb_add_fact("tlb(%hhu, 1, data, %hhu, %hhu, %u).",
                 coreid, tlb23.dtlb_asc, tlb23.dtlb_sz, 1 * 1024 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 1, instruction, %hhu, %hhu, %u).",
                 coreid, tlb23.itlb_asc, tlb23.itlb_sz, 1 * 1024 * 1024 * 1024);

    tlb23 = cpuid_l2_1G_rd(&id);
    skb_add_fact("tlb(%hhu, 2, data, %hhu, %hhu, %u).",
                 coreid, tlb23.dtlb_asc, tlb23.dtlb_sz, 1 * 1024 * 1024 * 1024);
    skb_add_fact("tlb(%hhu, 2, instruction, %hhu, %hhu, %u).",
                 coreid, tlb23.itlb_asc, tlb23.itlb_sz, 1 * 1024 * 1024 * 1024);


    skb_add_fact("associativity_encoding(amd, 2, 0, disabled).");
    skb_add_fact("associativity_encoding(amd, 2, 1, direct).");
    skb_add_fact("associativity_encoding(amd, 2, 2, 2).");
    skb_add_fact("associativity_encoding(amd, 2, 4, 4).");
    skb_add_fact("associativity_encoding(amd, 2, 6, 8).");
    skb_add_fact("associativity_encoding(amd, 2, 8, 16).");
    skb_add_fact("associativity_encoding(amd, 2, 10, 32).");
    skb_add_fact("associativity_encoding(amd, 2, 11, 48).");
    skb_add_fact("associativity_encoding(amd, 2, 12, 64).");
    skb_add_fact("associativity_encoding(amd, 2, 13, 96).");
    skb_add_fact("associativity_encoding(amd, 2, 14, 128).");
    skb_add_fact("associativity_encoding(amd, 2, 15, full).");

    skb_add_fact("associativity_encoding(amd, 3, 0, disabled).");
    skb_add_fact("associativity_encoding(amd, 3, 1, direct).");
    skb_add_fact("associativity_encoding(amd, 3, 2, 2).");
    skb_add_fact("associativity_encoding(amd, 3, 4, 4).");
    skb_add_fact("associativity_encoding(amd, 3, 6, 8).");
    skb_add_fact("associativity_encoding(amd, 3, 8, 16).");
    skb_add_fact("associativity_encoding(amd, 3, 10, 32).");
    skb_add_fact("associativity_encoding(amd, 3, 11, 48).");
    skb_add_fact("associativity_encoding(amd, 3, 12, 64).");
    skb_add_fact("associativity_encoding(amd, 3, 13, 96).");
    skb_add_fact("associativity_encoding(amd, 3, 14, 128).");
    skb_add_fact("associativity_encoding(amd, 3, 15, full).");

}

static void add_amd_nrcore_info(coreid_t core_id)
{
    uint32_t eax, ebx, ecx, edx;

    cpuid(1, &eax, &ebx, &ecx, &edx);
    uint8_t local_apic_id = ebx >> 24;
    uint8_t logical_processors = (ebx >> 16) & 0xff;
    bool htt = (edx >> 28) & 0x1;

    if (!htt) {
        //the processor only has one core
        //cpu_thread(APIC_ID, packageID, coreID, threadID).
        skb_add_fact("cpu_thread(%u, %u, %u, %u).", core_id, local_apic_id, 0, 0);
    } else {
        cpuid(0x80000008, &eax, &ebx, &ecx, &edx);
        uint8_t mnc;
        uint8_t ApicIdCoreIdSize = (ecx >> 12) & 0xf;
        uint8_t nc = ecx & 0xff;
        if (ApicIdCoreIdSize == 0) {
            //legacy method to compute the number of cores on AMD
            mnc = nc + 1;
        } else {
            //extended method to compute the number of cores on AMD
            mnc = 1 << ApicIdCoreIdSize;
        }
//XXX: not sure about these calculations.
//        uint8_t hyperthreads = logical_processors / nc;
        uint8_t hyperthreads = logical_processors / mnc;
        uint8_t hyperthread_shift = nr_bits_needed(hyperthreads - 1);
        uint8_t hyperthread_mask = (1 << hyperthread_shift) - 1;
        uint8_t core_shift = nr_bits_needed(mnc - 1);
        uint8_t core_mask = (1 << core_shift) - 1;

        //cpu_thread(APIC_ID, packageID, coreID, threadID).
        skb_add_fact("cpu_thread(%u, %u, %u, %u).", core_id,
            local_apic_id >> (hyperthread_shift + core_shift),
            (local_apic_id >> (hyperthread_shift)) & core_mask,
            local_apic_id & hyperthread_mask);
    }
}

/******************************************************************************/


/******************************************************************************/
//Intel
/******************************************************************************/

static void add_intel_nrcore_info(coreid_t core_id)
{
    uint32_t eax, ebx, ecx, edx;
    uint8_t logical_cores, cores_per_package, hyperthreads, initial_apic_id;
    cpuid(1, &eax, &ebx, &ecx, &edx);
    logical_cores = (ebx >> 16) & 0xff;
    initial_apic_id = (ebx >> 24) & 0xff;
    //XXX: FIXME: cpuid() should allow to initialize also other registers,
    //            because some cpuid() calls have two input parameters (like here
    //            ecx has to be zero, but also supports different values to get
    //            different data)
    __asm__ __volatile__("xor %%ecx,%%ecx"::);
    cpuid(4, &eax, &ebx, &ecx, &edx);
    cores_per_package = (eax >> 26) + 1;
    hyperthreads = logical_cores / cores_per_package;

    uint8_t hyperthread_shift = nr_bits_needed(hyperthreads - 1);
    uint8_t hyperthread_mask = (1 << hyperthread_shift) - 1;
    uint8_t core_shift = nr_bits_needed(cores_per_package - 1);
    uint8_t core_mask = (1 << core_shift) - 1;

    //cpu_thread(APIC_ID, packageID, coreID, threadID).
    skb_add_fact("cpu_thread(%u, %u, %u, %u).", core_id,
                initial_apic_id >> (hyperthread_shift + core_shift),
                (initial_apic_id >> (hyperthread_shift)) & core_mask,
                initial_apic_id & hyperthread_mask);
}








/******************************************************************************/
//main function to be called
/******************************************************************************/
void gather_cpuid_data(coreid_t core_id)
{
    uint32_t eax, ebx, ecx, edx;

    struct cpuid_t id;
    cpuid_initialize(&id);

    //read the maximum number of standard functions
    uint32_t max_std_fn = cpuid_max_biv_rd(&id);
//    skb_add_fact("maxstdcpuid(%hhu, %u).", core_id, max_std_fn);
    skb_add_fact("maxstdcpuid(%u, %u).", core_id, max_std_fn);
/*
the mackerel file seemsnot to work
    if ((cpuid_vendor0_rd(&id) == 0x68747541) &&
        (cpuid_vendor1_rd(&id) == 0x444d4163) &&
        (cpuid_vendor2_rd(&id) == 0x69746e65)) {
*/
    cpuid(0,&eax,&ebx,&ecx,&edx);
    if ((ebx == 0x68747541) && (ecx == 0x444d4163) && (edx == 0x69746e65)) {
        //AuthenticAMD
        skb_add_fact("vendor(%hhu,amd).", core_id);
        add_amd_cache_info(id, core_id);
        add_amd_nrcore_info(core_id);
    } else if ((ebx == 0x756e6547) && (ecx == 0x6c65746e) && (edx == 0x49656e69)) {
        //GenuineIntel
        add_intel_nrcore_info(core_id);
    } else {
        printf("skb_cpuid: unknown CPU type...\n");
        printf("vendor numbers: %x %x %x",
            cpuid_vendor0_rd(&id),
            cpuid_vendor1_rd(&id),
            cpuid_vendor2_rd(&id));

        printf("\nok, trying by hand...\n");
        cpuid(0,&eax,&ebx,&ecx,&edx);
        printf("vendor numbers: %x %x %x",
            ebx,ecx,edx);
        printf("\nXXX: added _AMD_ cache info anyway...\n");
        add_amd_cache_info(id, core_id);
        add_amd_nrcore_info(core_id);
    }

}
