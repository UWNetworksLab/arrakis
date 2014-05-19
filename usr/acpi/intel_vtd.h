/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef INTEL_VTD_H
#define INTEL_VTD_H

#include <pci/confspace/pci_confspace.h>

#include <dev/vtd_dev.h>
#include <dev/vtd_iotlb_dev.h>

#include "vtd_domains.h"
#include "vtd_sl_paging.h"

#define ROOT_TABLE_TYPE         vtd_rt
#define NUM_ROOT_ENTRIES	256

#define VTD_FOR_EACH(var, head) for (var = (head); var; var = var->next)
#define VTD_ADD_UNIT(x, head) do {(x)->next = (head); (head) = (x);} while(0)

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

// A structure representing a remapping hardware unit. We currently assume
// that there is only one such unit per segment.
struct vtd_unit {
    vtd_t *                     regset;     // Remapping register set
    vtd_iotlb_t *               iotlb_regs; // IOTLB registers

    uint16_t                    pci_seg;
    // capability for the root table frame
    struct capref               rt_frame;
    vtd_root_entry_array_t *    root_table;
  
    // array of capabilities of context table frames
    struct capref               ct_frame_caps[NUM_ROOT_ENTRIES]; 
    vtd_context_entry_array_t * context_tables[NUM_ROOT_ENTRIES];

    struct vtd_unit *  next;
};

errval_t vtd_create_domain(struct capref pml4);
errval_t vtd_remove_domain(struct capref pml4);

errval_t vtd_domain_remove_device(int seg, int bus, int dev, int func, struct capref pml4);
errval_t vtd_domain_add_device(int seg, int bus, int dev, int func, struct capref pml4);

void vtd_identity_domain_add_devices(void);

int vtd_init(void);

// This will need to be changed when (and if) Arrakis supports PCI
// segments.
static inline int valid_device(int bus, int dev, int func)
{
    return ((bus >= 0 && bus < PCI_NBUSES)   && 
	    (dev >= 0 && dev < PCI_NDEVICES) &&
	    (func >= 0 && func < PCI_NFUNCTIONS));
}

// Returns true if units, a list of remapping hardware units, is empty.
static inline int vtd_no_units(struct vtd_unit *units) {
    return (units == NULL);
}

// Returns 1 if the remapping unit snoops the processor caches for both
// paging-entries and pages, else 0.
static inline int vtd_coherency(struct vtd_unit *unit)
{
    int pwc = vtd_ECAP_pwc_rdf(unit->regset);
    int sc  = vtd_ECAP_sc_rdf(unit->regset);
    return (pwc & sc);
}

// Perform command to set the root table and then wait until hardware
// sets the bit to indicate completion.
static inline void GSTS_srtp_wait(struct vtd_unit *unit)
{
    vtd_GCMD_srtp_wrf(unit->regset, 1);
    while(vtd_GSTS_rtps_rdf(unit->regset) == 0);
}

// Perform command to flush the write buffer and then wait until hardware
// clears the bit to indicate completion.
static inline void GSTS_wbf_wait(struct vtd_unit *unit)
{
    vtd_GCMD_wbf_wrf(unit->regset, 1);
    while(vtd_GSTS_wbfs_rdf(unit->regset));
}

// Perform command to enable/diable DMA-remapping and then wait until hardware 
// sets/clears the bit to indicate completion.
static inline void GSTS_te_wait(struct vtd_unit *unit, int val)
{
    assert(val == 1 || val == 0);
    vtd_GCMD_te_wrf(unit->regset, val);
    while(vtd_GSTS_tes_rdf(unit->regset) == !val);
}

// Perform command to invalidate the context-cache and then wait until 
// the hardware clears the bit to indicate completion.
static inline void CCMD_icc_wait(struct vtd_unit *unit)
{
    vtd_CCMD_icc_wrf(unit->regset, 1);
    while(vtd_CCMD_icc_rdf(unit->regset));
}

// Perform command to invalidate the IOTLB and then wait until hardware 
// clears the bit to indicate completion.
static inline void IOTLB_ivt_wait(struct vtd_unit *unit)
{ 
    vtd_iotlb_iotlb_reg_ivt_wrf(unit->iotlb_regs, 1);
    while (vtd_iotlb_iotlb_reg_ivt_rdf(unit->iotlb_regs));
}

// Flush the internal write buffers.
static inline void vtd_flush_write_buffer(struct vtd_unit *unit)
{
    if (vtd_CAP_rwbf_rdf(unit->regset)) {
        GSTS_wbf_wait(unit);
    }
}

// Perform ax global invalidation of context-cache for unit.
// vtd_iotlb_glob_inval must be called afterwards.
static inline void vtd_context_cache_glob_inval(struct vtd_unit *unit)
{
    // Flush the Root-Complex internal write buffers
    vtd_flush_write_buffer(unit);
    // We want a global invalidation
    vtd_CCMD_cirg_wrf(unit->regset, vtd_gir);
    // Perform invalidation
    CCMD_icc_wait(unit);
}

// Perform a domain-selective invalidation of context-cache for dom.
// vtd_iotlb_dom_inval must be called afterwards
static inline void vtd_context_cache_dom_inval(struct vtd_domain *dom)
{
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        vtd_flush_write_buffer(u);
        vtd_CCMD_cirg_wrf(u->regset, vtd_domir);
	vtd_CCMD_did_wrf(u->regset, dom->did);
	CCMD_icc_wait(u);
    }
}

// Perform a device-selective invalidation of context-cache for 
// the device with the source-id sid contained in the domain dom.
// vtd_iotlb_dom_inval must be called afterwards
static inline void vtd_context_cache_dev_inval(struct vtd_domain *dom, int sid, int func_mask)
{
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        vtd_flush_write_buffer(u);
	vtd_CCMD_cirg_wrf(u->regset, vtd_devir);
	vtd_CCMD_did_wrf(u->regset, dom->did);
	vtd_CCMD_sid_wrf(u->regset, sid);
	vtd_CCMD_fm_wrf(u->regset, func_mask);
	CCMD_icc_wait(u);
    }
}

// Perform a global IOTLB invalidation for unit.
static inline void vtd_iotlb_glob_inval(struct vtd_unit *unit)
{
    // Flush the Root-Complex internal write buffers
    vtd_flush_write_buffer(unit);
    // We want a global invalidation
    vtd_iotlb_iotlb_reg_iirg_wrf(unit->iotlb_regs, vtd_iotlb_gir);
    // Drain writes and reads (if not required, will be ignored by hardware)
    vtd_iotlb_iotlb_reg_dw_wrf(unit->iotlb_regs, 1);
    vtd_iotlb_iotlb_reg_dr_wrf(unit->iotlb_regs, 1);
    // Invalidate IOTLB
    IOTLB_ivt_wait(unit);
}

// Perform a domain-selective IOTLB invalidation for dom.
static inline void vtd_iotlb_dom_inval(struct vtd_domain *dom)
{
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        vtd_flush_write_buffer(u);
        vtd_iotlb_iotlb_reg_iirg_wrf(u->iotlb_regs, vtd_iotlb_domir);
	vtd_iotlb_iotlb_reg_did_wrf(u->iotlb_regs, dom->did);
	vtd_iotlb_iotlb_reg_dw_wrf(u->iotlb_regs, 1);
	vtd_iotlb_iotlb_reg_dr_wrf(u->iotlb_regs, 1);
	IOTLB_ivt_wait(u);
    }
}

// Perform a page-selective-within-domain IOTLB invalidation for dom.
//
// addr - the sl-input-address that needs to be invalidated
//
// addr_mask - specifies the number of low bits of addr to be masked. 
// Used for the invalidation of multiple contiguous pages which may 
// comprise a large page. 
//
// inval_hint - a value of 0 indicates that both leaf and non-leaf sl 
// entries may have been modified. 1 indicates that no sl non-leaf entry
// has been modified.
static inline void vtd_iotlb_page_inval(struct vtd_domain *dom, int ival_hint, int addr, int addr_mask)
{ 
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        vtd_flush_write_buffer(u);
        vtd_iotlb_iotlb_reg_iirg_wrf(u->iotlb_regs, vtd_iotlb_pir);
	vtd_iotlb_iotlb_reg_did_wrf(u->iotlb_regs, dom->did);
	vtd_iotlb_iotlb_reg_dw_wrf(u->iotlb_regs, 1);
	vtd_iotlb_iotlb_reg_dr_wrf(u->iotlb_regs, 1);
	vtd_iotlb_iva_reg_ih_wrf(u->iotlb_regs, ival_hint);
	vtd_iotlb_iva_reg_addr_wrf(u->iotlb_regs, (addr >> 12));
	vtd_iotlb_iva_reg_am_wrf(u->iotlb_regs, addr_mask);
	IOTLB_ivt_wait(u);
    }
}

// Enable DMA-remapping for unit.
static inline void vtd_trnsl_enable(struct vtd_unit *unit)
{
    GSTS_te_wait(unit, 1);
}

// Disable DMA-remapping for unit.
static inline void vtd_trnsl_disable(struct vtd_unit *unit)
{
    GSTS_te_wait(unit, 0);
}

// Returns the number of domains supported. A simple linear relationship
// exists between the value of the nd field in the Capability register 
// and the number of domains.
static inline int vtd_number_domains_supported(struct vtd_unit *unit)
{
    int nd_bits;
    assert(unit != NULL);
    nd_bits = (2 * vtd_CAP_nd_rdf(unit->regset)) + 4;
    return (1 << nd_bits);
}

// Returns the number of bits per page for a pagetable with the specified
// number of levels.
static inline int vtd_levels_to_page_bits(int levels) {
    assert(levels >= 2 && levels <= 4);
    return (48 - SL_PTABLE_MASK_BITS * levels);
}

#endif //INTEL_VTD_H
