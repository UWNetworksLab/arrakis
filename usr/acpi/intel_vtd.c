/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <acpi.h>
#include <mm/mm.h>
#include <skb/skb.h>
#include <pci/confspace/pci_confspace.h>

#include "intel_vtd.h"
#include "vtd_debug.h"

extern struct mm pci_mm_physaddr;

// cache coherency for frame mappings
static bool cache_coherence = false;
static vregion_flags_t vtd_map_attr;

static struct vtd_unit *vtd_units = NULL;
static struct vtd_domain_list *domains = NULL;

static struct vtd_domain *identity_domain = NULL;
static lvaddr_t *id_pagetable_vbase = NULL;

#ifdef VTD_DEBUG_
static void vtd_dump_registers(struct vtd_unit *unit)
{
    size_t size = 1 << 13;
    char *buffer = malloc(size);
    assert(buffer != NULL);
    vtd_iotlb_pr(buffer, size, unit->iotlb_regs);
    vtd_pr(buffer, size, unit->regset);
    puts(buffer);
    free(buffer);
}
#endif

#ifdef VTD_DEBUG_
static void dump_domains(struct vtd_domain_list *lst)
{
    struct vtd_domain *d;
    VTD_DEBUG("Printing the list of domains:\n");
  
    if (domain_list_empty(lst)) {
        printf("No domains\n");
	return;
    }
    VTD_FOR_EACH(d, lst->head) {
        if (d == lst->head) printf("H");
	printf("->[%d]", d->did);
	if (d == lst->tail) printf("<-T");
    }
    printf("\n");
}
#endif

// Creates the root-table and context-tables for unit. 
static void vtd_create_tables(struct vtd_unit *unit)
{
    assert(unit != NULL);
    errval_t err;
    err = frame_alloc(&(unit->rt_frame), BASE_PAGE_SIZE, NULL);
    assert(err_is_ok(err));

    err = vspace_map_one_frame_attr((void **)&(unit->root_table), BASE_PAGE_BITS, 
				    unit->rt_frame, vtd_map_attr, NULL, NULL);
    assert(err_is_ok(err));

    for (int i = 0; i < NUM_ROOT_ENTRIES; i++) {
        err = frame_alloc(&(unit->ct_frame_caps[i]), BASE_PAGE_SIZE, NULL);
	assert(err_is_ok(err));

	err = vspace_map_one_frame_attr((void **)&(unit->context_tables[i]), BASE_PAGE_BITS,
					unit->ct_frame_caps[i], vtd_map_attr, NULL, NULL);
	assert(err_is_ok(err));
    }
}

// Maps the remapping hardware memory-mapped registers of a unit to vspace.
static void *vtd_map_registers(genpaddr_t regset_base)
{
    errval_t err;
    void *regset_vbase;
    struct capref regset_frame, regset_devframe;
    struct frame_identity regset_frame_id;
  
    err = mm_alloc_range(&pci_mm_physaddr, BASE_PAGE_BITS, regset_base, 
			 regset_base + BASE_PAGE_SIZE, &regset_frame, NULL);     
    assert(err_is_ok(err));
    
    err = invoke_frame_identify(regset_frame, &regset_frame_id);
    assert(err_is_ok(err));

    err = devframe_type(&regset_devframe, regset_frame, BASE_PAGE_BITS);
    assert(err_is_ok(err));
    assert(regset_base == regset_frame_id.base);

    err = vspace_map_one_frame_attr(&regset_vbase, BASE_PAGE_SIZE, regset_devframe, 
				    vtd_map_attr, NULL, NULL);
    assert(err_is_ok(err));
    return regset_vbase;
}

static void vtd_set_root_table(struct vtd_unit *unit) 
{
    assert(unit != NULL);
    errval_t err;
    genpaddr_t rt_base;
    struct frame_identity rt_frame_id;
    err = invoke_frame_identify(unit->rt_frame, &rt_frame_id);
    assert(err_is_ok(err));
    rt_base = rt_frame_id.base;
    assert((rt_base & BASE_PAGE_MASK) == 0);
    // Set the root-table type
    vtd_RTADDR_rtt_wrf(unit->regset, ROOT_TABLE_TYPE);
    // Set the physical address of the root-table
    vtd_RTADDR_rta_wrf(unit->regset, (rt_base >> BASE_PAGE_BITS));
    // Update the root-table pointer
    GSTS_srtp_wait(unit);
    // Globally invalidate the context-cache and then globally invalidate
    // the IOTLB (only in this order). 
    vtd_context_cache_glob_inval(unit);
    vtd_iotlb_glob_inval(unit);
}

// Creates and initializes a structure for a remapping hardware unit
// with its register set at regset_vbase.
// 
// We will have to change this as it is possible for multiple hardware
// units may reside on the same segment.
static struct vtd_unit *vtd_create_unit(void *regset_vbase, uint16_t segment)
{
    int iro;
    struct vtd_unit *new_unit;
    void *iotlb_regs_vbase;

    new_unit = (struct vtd_unit *)malloc(sizeof(struct vtd_unit));
    assert(new_unit != NULL);

    new_unit->regset = malloc(sizeof(*(new_unit->regset)));
    assert(new_unit->regset != NULL);
    vtd_initialize(new_unit->regset, regset_vbase);

    iro = vtd_ECAP_iro_rdf(new_unit->regset); 
    iotlb_regs_vbase = (void *)((uint8_t *)regset_vbase + iro);

    new_unit->iotlb_regs = malloc(sizeof(*(new_unit->iotlb_regs)));
    assert(new_unit->iotlb_regs != NULL);
    vtd_iotlb_initialize(new_unit->iotlb_regs, iotlb_regs_vbase);

    new_unit->pci_seg = segment;
    new_unit->next = NULL;
    
    // Create root table and context tables for the new remapping unit
    vtd_create_tables(new_unit); 
    return new_unit;
}

static void vtd_insert_context_tables(struct vtd_unit *unit)
{
    assert(unit != NULL);
    assert(vtd_CAP_cm_rdf(unit->regset) == 0);
 
    errval_t err;
    struct frame_identity ct_id;
    for (int i = 0; i < NUM_ROOT_ENTRIES; i++) {
        err = invoke_frame_identify(unit->ct_frame_caps[i], &ct_id);
        assert(err_is_ok(err));
	assert((ct_id.base & BASE_PAGE_MASK) == 0);
	vtd_root_entry_ctp_insert(unit->root_table[i], (ct_id.base >> BASE_PAGE_BITS));
	vtd_root_entry_p_insert(unit->root_table[i], 1);
    }
}

// If the cap is a capability for an x86-64 PML4 VNode we return
// the base physical address of it. If not, we return 0.
static inline genpaddr_t pml4_base(struct capref cap)
{
    errval_t err;
    struct vnode_identity pml4_id;
    err = invoke_vnode_identify(cap, &pml4_id);
    assert(err_is_ok(err));
    genpaddr_t pt = pml4_id.base;
    if (pml4_id.type != ObjType_VNode_x86_64_pml4) {
        return 0; 
    }
  
    // If the cap is for the identity domain, we return the physical
    // address of the identity pagetable, which has a PML4 table created 
    // from a frame capability, instead of the base address of the empty
    // PML4 VNode.
    pml4_id.base = 0;
    pml4_id.type = 0;
    err = invoke_vnode_identify(identity_domain->pml4, &pml4_id);
    assert(err_is_ok(err));
    if (pml4_id.base == pt) {
        pt = identity_domain->pt_gp;
    }
    return pt;
}

// Removes a device from the domain specified by pml4.
errval_t vtd_domain_remove_device(int seg, int bus, int dev, int func, struct capref pml4)
{
    if (vtd_no_units(vtd_units)) return VTD_ERR_NO_UNITS;
    if (!valid_device(bus, dev, func)) return VTD_ERR_DEV_NOT_FOUND;
  
    genpaddr_t pt = pml4_base(pml4);
    if (pt == 0) return VTD_ERR_INVALID_CAP;
  
    // Find the domain in the list of domains
    struct vtd_domain *dom = NULL;
    VTD_FOR_EACH(dom, domains->head) {
        if (dom->pt_gp == pt) break;
    }

    if (dom == NULL) return VTD_ERR_DOM_NOT_FOUND;
  
    // Find the unit containing the device under its scope
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        if (u->pci_seg == seg) break;
    }
    if (u == NULL) return VTD_ERR_DEV_NOT_FOUND;

    vtd_context_entry_array_t *context_table = u->context_tables[bus];
    uint8_t id = (dev << 3) | func;

    // The device doesn't belong to this domain
    if (!vtd_context_entry_p_extract(context_table[id])) {
        return VTD_ERR_DEV_NOT_FOUND;
    }

    vtd_context_entry_p_insert(context_table[id], 0);
    vtd_context_entry_t_insert(context_table[id], 0);
    vtd_context_entry_slptptr_insert(context_table[id], 0);
    vtd_context_entry_did_insert(context_table[id], 0);
    vtd_context_entry_aw_insert(context_table[id], 0);

    // After removing the devices, we perform a context-cache device-selective 
    // invalidation followed by an IOTLB domain-selective invalidation.
    int sid = (bus << 16) | id;
    vtd_context_cache_dev_inval(dom, sid, vtd_nomask);
    vtd_iotlb_dom_inval(dom);
    return SYS_ERR_OK;
}

// Adds a device to the domain specified by pml4.
errval_t vtd_domain_add_device(int seg, int bus, int dev, int func, struct capref pml4)
{
    errval_t err;
    if (vtd_no_units(vtd_units)) return VTD_ERR_NO_UNITS;
    if (!valid_device(bus, dev, func)) return VTD_ERR_DEV_NOT_FOUND;
    
    genpaddr_t pt = pml4_base(pml4);
    if (pt == 0) return VTD_ERR_INVALID_CAP;

    // Find the domain with the provided pml4 capability
    struct vtd_domain *dom = NULL;
    VTD_FOR_EACH(dom, domains->head) {
        if (dom->pt_gp == pt) break;
    }

    if (dom == NULL) return VTD_ERR_DOM_NOT_FOUND;

    // Find the unit containing the device under its scope
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        if (u->pci_seg == seg) break;
    }
    if (u == NULL) return VTD_ERR_DEV_NOT_FOUND;

    vtd_context_entry_array_t *context_table = u->context_tables[bus];
    uint8_t id = (dev << 3) | func;

    // When a request is made for a device, if it belongs to the identity domain,
    // we remove it before adding it to the domain specified by pml4
    if (vtd_context_entry_p_extract(context_table[id])) {
        int did = vtd_context_entry_did_extract(context_table[id]);
	if (did == identity_domain->did && (pt != identity_domain->pt_gp)) {
	    err = vtd_domain_remove_device(seg, bus, dev, func, identity_domain->pml4);
	    assert(err_is_ok(err));
	} else {
	    return VTD_ERR_DEV_USED;
	}
    }
    
    // If device-TLBs are supported, allow translated and translation requests
    if (vtd_ECAP_dt_rdf(u->regset)) {
        vtd_context_entry_t_insert(context_table[id], vtd_hme);
    }    
    vtd_context_entry_aw_insert(context_table[id], vtd_agaw48);
    vtd_context_entry_did_insert(context_table[id], dom->did);
   
    sys_debug_flush_cache();
    
    vtd_context_entry_slptptr_insert(context_table[id], (pt >> 12));
    vtd_context_entry_p_insert(context_table[id], 1);
    return SYS_ERR_OK;
}

// Determines the minimum and maximum domain-ids among all
// hardware units.
static void vtd_create_did_bounds(struct vtd_unit *head, struct vtd_domain_list *doms)
{
    assert(head != NULL);
    struct vtd_unit *u = head;
    doms->min_did = vtd_CAP_cm_rdf(u->regset);
    doms->max_did = vtd_number_domains_supported(u)-1;
    VTD_FOR_EACH(u, head) {
        doms->min_did = MAX(doms->min_did, vtd_CAP_cm_rdf(u->regset));
        doms->max_did = MIN(doms->max_did, vtd_number_domains_supported(u)-1);
    }
}

// Creates a new domain for an application using a capability to its root PML4.
errval_t vtd_create_domain(struct capref pml4)
{
    if (vtd_no_units(vtd_units)) return VTD_ERR_NO_UNITS;
 
    // Check that pml4 is a capability for a x86-64 PML4 VNode
    errval_t err;
    struct vnode_identity pml4_id;
    err = invoke_vnode_identify(pml4, &pml4_id);
    assert(err_is_ok(err));
    genpaddr_t pt = pml4_id.base;
    if (pml4_id.type != ObjType_VNode_x86_64_pml4) return VTD_ERR_INVALID_CAP;
    int did = domains->min_did;

    // Find a domain-id for the new domain
    struct vtd_domain *d = NULL;
    VTD_FOR_EACH(d, domains->head) {
        if (did < d->did) break;
	did++;
    }

    // All domain-ids have been exausted. Return an error.
    if (did > domains->max_did) return VTD_ERR_FULL;

    VTD_DEBUG("Creating domain with pt = %"PRIu64", did = %d\n", pt, did);
#ifdef VTD_DEBUG_
    dump_domains(domains);
#endif

    struct vtd_domain *new_domain = vtd_new_domain(did, pt, pml4, vtd_units);
    vtd_insert_domain(new_domain, d, domains);
  
    return SYS_ERR_OK;
}

// Removes a domain for an application with the specified root PML4.
errval_t vtd_remove_domain(struct capref pml4)
{
    if (vtd_no_units(vtd_units)) return VTD_ERR_NO_UNITS;

    // Check that pml4 is a capability for a x86-64 PML4 VNode
    errval_t err;
    struct vnode_identity pml4_id;
    err = invoke_vnode_identify(pml4, &pml4_id);
    assert(err_is_ok(err));
    genpaddr_t pt = pml4_id.base;
    if (pml4_id.type != ObjType_VNode_x86_64_pml4) return VTD_ERR_INVALID_CAP;

    // empty
    if (domain_list_empty(domains)) return VTD_ERR_DOM_NOT_FOUND;

    struct vtd_domain *d = NULL;
    VTD_FOR_EACH(d, domains->head) {
        if (d->pt_gp == pt) {
	    vtd_delete_domain(d, domains);
	    return SYS_ERR_OK;
	}
    }
    return VTD_ERR_DOM_NOT_FOUND;
}

// Establishes a mapping from va to pa in pt, a second-level 
// pagetable structure.
static inline uint64_t vtd_map(uint64_t va, uint64_t pa, lvaddr_t *pt, int levels)
{
    struct capref pe_frame;
    struct frame_identity pe_id;
    uint64_t *vtp = pt;

    int e = 0;
    for (int current_level = 1; current_level <= levels; current_level++) {
        switch (current_level) {
	case 1: 
	    e = X86_64_PML4_BASE(va);
	    break;
	case 2: 
	    e = X86_64_PDPT_BASE(va);
	    break; 
	case 3: 
	    e = X86_64_PDIR_BASE(va);
	    break;
	case 4:
	    e = X86_64_PTABLE_BASE(va);
	    break;
	}
	if (current_level == levels) break;
	if (vtp[e + X86_64_PTABLE_SIZE] == 0) {
	    int bytes = 2 * BASE_PAGE_SIZE;
	    bytes = (current_level == (levels-1)) ?  bytes : 2 * bytes;
 
	    errval_t err = frame_alloc(&pe_frame, bytes, NULL);
	    assert(err_is_ok(err));
    
	    void *vbase;
	    err = vspace_map_one_frame_attr(&vbase, bytes, pe_frame, 
					    vtd_map_attr, NULL, NULL);
	    assert(err_is_ok(err));
	    assert(((uint64_t)vbase & BASE_PAGE_MASK) == 0);
    
	    err = invoke_frame_identify(pe_frame, &pe_id);
	    assert(err_is_ok(err));
	    assert((pe_id.base & BASE_PAGE_MASK) == 0);

	    union sl_pdir_entry *entry = (union sl_pdir_entry *)vtp + e;
	    sl_map_table(entry, pe_id.base);
	    vtp[e + X86_64_PTABLE_SIZE] = (uint64_t)vbase;
	}
	vtp = (uint64_t *)vtp[e + X86_64_PTABLE_SIZE]; 
    }

    union sl_ptable_entry *entry = (union sl_ptable_entry *)vtp + e;
    paging_sl_flags_t flags = SL_PTABLE_READ | SL_PTABLE_WRITE;
   
    switch (levels) {
    case 2:
        sl_map_large30(entry, pa, flags);
	break;
    case 3:
        sl_map_large21(entry, pa, flags);
	break;
    case 4:
        sl_map(entry, pa, flags);
	break;
    }

    return (1UL << vtd_levels_to_page_bits(levels));
}

// Returns the minimum number of supported page bits among all hardware 
// units.
static int vtd_page_bits(struct vtd_unit *head) 
{
    assert(head != NULL);
    int num_page_bits = 30;

    struct vtd_unit *u = head;
    VTD_FOR_EACH(u, head) {
      int unit_page_bits;
      if (vtd_CAP_sllps30_rdf(u->regset)) {
          unit_page_bits = 30;
      } else if (vtd_CAP_sllps21_rdf(u->regset)) {
	  unit_page_bits = 21;
      } else {
          unit_page_bits = 12;
      }
      num_page_bits = MIN(num_page_bits, unit_page_bits);
    }
    return num_page_bits;
}

// Create the identity domain along with its identity pagetable if there
// is at least one remapping unit present.
static void vtd_create_identity_domain(void)
{   
    int levels = 4;
    int page_bits = vtd_page_bits(vtd_units);
    levels -= (page_bits - BASE_PAGE_BITS) / SL_PTABLE_MASK_BITS;

    // Map only the first 1024 GB of physical memory. Attempting to map
    // the entire address space with this current implementation is 
    // infeasible.
    uint64_t max_addr = 1UL << 40;
    errval_t err;
    struct frame_identity pe_frame_id;
    struct capref pe_frame;
    void *pe_vaddr;
    err = frame_alloc(&pe_frame, 2 * X86_64_BASE_PAGE_SIZE, NULL);
    assert(err_is_ok(err));
    err = invoke_frame_identify(pe_frame, &pe_frame_id);
    assert(err_is_ok(err));
    err = vspace_map_one_frame_attr(&pe_vaddr, 1 << pe_frame_id.bits, pe_frame,
				    vtd_map_attr, NULL, NULL);
    assert(err_is_ok(err));
    assert((pe_frame_id.base & X86_64_BASE_PAGE_MASK) == 0 &&
	   ((uint64_t)pe_vaddr & X86_64_BASE_PAGE_MASK) == 0);
    
    struct capref empty_pml4;
    err = slot_alloc(&empty_pml4);
    assert(err_is_ok(err));
    err = vnode_create(empty_pml4, ObjType_VNode_x86_64_pml4);
    assert(err_is_ok(err));
    
    id_pagetable_vbase = (lvaddr_t *)pe_vaddr;
    
    identity_domain = vtd_new_domain(domains->min_did, pe_frame_id.base, empty_pml4, vtd_units);
    assert(domains != NULL);
    assert(domain_list_empty(domains));
    domains->head = identity_domain;
    domains->tail = identity_domain;

    uint64_t mapped, remaining, vaddr, paddr;
    vaddr = 0, paddr = 0;
    remaining = max_addr;
    while (remaining > 0) {
        mapped = vtd_map(vaddr, paddr, id_pagetable_vbase, levels);
	vaddr += mapped;
	paddr += mapped;
	remaining -= mapped;
    }
}

// Called to add the devices specified in the translation structures to 
// the identity domain.
static void vtd_add_dmar_devices(void)
{
    errval_t err;
    err = skb_client_connect();
    assert(err_is_ok(err));
  
    err = skb_execute_query("dmar_devices(L),""length(L,Len),writeln(L)");
    assert(err_is_ok(err));

    struct list_parser_status status;
    skb_read_list_init(&status);
    
    int seg, bus, dev, func;
    while(skb_read_list(&status, "address(%"PRIu32",%"PRIu32",%"PRIu32",%"PRIu32")", 
			&seg, &bus, &dev, &func)) {
        err = vtd_domain_add_device(seg, bus, dev, func, identity_domain->pml4);
	assert(err == VTD_ERR_DEV_USED || err == SYS_ERR_OK);
    }
}

// Given the address of a PCIe bridge, return the bus downstream of it.
static int vtd_find_secondary_bus(int bus, int dev, int func)
{
    errval_t err;
    err = skb_execute_query("bridge(PCIE,addr(%d,%d,%d),_,_,_,_,_,secondary(BUS)),"
			    "write(secondary_bus(BUS)).", bus, dev, func);
    assert(err_is_ok(err));
    
    int next_bus;
    err = skb_read_output("secondary_bus(%d)", &next_bus);
    assert(err_is_ok(err));

    return next_bus;
}

// Parses a Path structure, comprised of (Device number, Function number) pairs, 
// representing the hierarchical path of a device. The address of the device
// is returned in bus, dev, and func.
static void vtd_parse_dev_path(int begin_bus, int *bus, int *dev, int *func, char *begin, char *end)
{
    assert((bus != NULL) && (dev != NULL) && (func != NULL));
    int curr_bus, curr_dev, curr_func;
    ACPI_DMAR_PCI_PATH *path_entry;

    path_entry = (ACPI_DMAR_PCI_PATH *)begin;
  
    curr_bus  = begin_bus;  
    curr_dev  = path_entry->Device;
    curr_func = path_entry->Function;
    
    path_entry = (ACPI_DMAR_PCI_PATH *)((char *)path_entry + sizeof(ACPI_DMAR_PCI_PATH));
    while ((char *)path_entry != end) {
        curr_bus  = vtd_find_secondary_bus(curr_bus, curr_dev, curr_func);
	curr_dev  = path_entry->Device;
	curr_func = path_entry->Function;
    
	path_entry = (ACPI_DMAR_PCI_PATH *)((char *)path_entry + sizeof(ACPI_DMAR_PCI_PATH));  
    }
    *bus  = curr_bus;
    *dev  = curr_dev;
    *func = curr_func;
}

// Parses a Device Scope Structure belonging to a remapping structure.
static void vtd_parse_dev_scope_structure(int segment, char *begin, char *end, enum AcpiDmarType type) 
{
    errval_t err;
    int path_length;
    ACPI_DMAR_DEVICE_SCOPE *entry;
    ACPI_DMAR_PCI_PATH *path_begin, *path_end;

    entry = (ACPI_DMAR_DEVICE_SCOPE *)begin;
    while ((char *)entry != end) {
        path_length = entry->Length - sizeof(ACPI_DMAR_DEVICE_SCOPE);
	assert(path_length == 2);

	path_begin = (ACPI_DMAR_PCI_PATH *)((char *)entry + sizeof(ACPI_DMAR_DEVICE_SCOPE));
	path_end   = (ACPI_DMAR_PCI_PATH *)((char *)path_begin + path_length);
 
	int bus, dev, func;
	vtd_parse_dev_path(entry->Bus, &bus, &dev, &func, (char *)path_begin, (char *)path_end);

	err = skb_execute_query("dmar_device(%"PRIu8",%"PRIu8","
				"addr(%"PRIu32",%"PRIu32",%"PRIu32",%"PRIu32"),%"PRIu8").",
				type, entry->EntryType, segment, bus, dev, func, entry->EnumerationId);

	// A device may have already been reported to the SKB for an earlier
	// translation structure.
	if (err_is_fail(err)) {
	    skb_add_fact("dmar_device(%"PRIu8",%"PRIu8","
			 "addr(%"PRIu32",%"PRIu32",%"PRIu32",%"PRIu32"),%"PRIu8").",
			 type, entry->EntryType, segment, bus, dev, func, entry->EnumerationId);
	    VTD_DEBUG("Adding device %d:%d:%d:%d\n", segment, bus, dev, func);
	}

	entry = (ACPI_DMAR_DEVICE_SCOPE *)((char *)entry + entry->Length);
    }
}

// Parses a DMA Remapping Hardware Unit (DRHD) structure. There is at least one
// such structure for each PCI segment.
static void vtd_parse_drhd_structure(char *begin, char *end)
{
    ACPI_DMAR_HARDWARE_UNIT *drhd;
    struct vtd_unit *new_unit;
  
    drhd = (ACPI_DMAR_HARDWARE_UNIT *)begin;
    
    skb_add_fact("dmar_hardware_unit(%"PRIu8", %"PRIu16", %"PRIu64").",
		 drhd->Flags, drhd->Segment, drhd->Address);
  
    new_unit = vtd_create_unit(vtd_map_registers(drhd->Address), drhd->Segment);
    vtd_insert_context_tables(new_unit);
    VTD_ADD_UNIT(new_unit, vtd_units); 

    vtd_parse_dev_scope_structure(drhd->Segment, begin + sizeof(ACPI_DMAR_HARDWARE_UNIT), 
				  end, ACPI_DMAR_TYPE_HARDWARE_UNIT);
 
#ifdef VTD_DEBUG_
    vtd_dump_registers(new_unit);
#endif
}
 
// Parses a Reserved Memory Region Reporting (RMRR) remapping structure.
// Reserved Memory Region Reporting structures report reserved memory regions for 
// devices that are each under the scope of some remapping hardware unit.
static void vtd_parse_rmrr_structure(char *begin, char *end) 
{
    ACPI_DMAR_RESERVED_MEMORY *rmrr;
    rmrr = (ACPI_DMAR_RESERVED_MEMORY *)begin;
    skb_add_fact("dmar_reserved_memory(%"PRIu16", %"PRIu64", %"PRIu64").",
		 rmrr->Segment, rmrr->BaseAddress, rmrr->EndAddress);
    vtd_parse_dev_scope_structure(rmrr->Segment, begin + sizeof(ACPI_DMAR_RESERVED_MEMORY), 
				  end, ACPI_DMAR_TYPE_RESERVED_MEMORY);
} 

// Parses a Root Port ATS Capability Reporting (ATSR) structure.
// An ATSR structure is provided for each PCI segment supporting Device-TLBs. Currently, 
// we only report the PCI segments supporting Device-TLBs and the associated PCIe 
// Root-Ports to the SKB.
static void vtd_parse_atsr_structure(char *begin, char *end) 
{
    ACPI_DMAR_ATSR *atsr;
    atsr = (ACPI_DMAR_ATSR *)begin;
    skb_add_fact("dmar_atsr(%"PRIu8", %"PRIu16").", atsr->Flags, atsr->Segment);
    if (atsr->Flags == ACPI_DMAR_ALL_PORTS) {
        return;
    }
    vtd_parse_dev_scope_structure(atsr->Segment, begin + sizeof(ACPI_DMAR_ATSR),
				  end, ACPI_DMAR_TYPE_ATSR);
}

// Parses a Remapping Hardware Static Affinity (RHSA) structure.
// RHSA structures are optional and are for platforms supporting non-uniform memory. 
// Currently, we only report the proximity domain each hardware unit belongs to(identified 
// by the base address of its register set) to the SKB. 
static void vtd_parse_rhsa_structure(char *begin, char *end) 
{
    ACPI_DMAR_RHSA *rhsa;
    rhsa = (ACPI_DMAR_RHSA *)begin;
    skb_add_fact("dmar_rhsa(%"PRIu64", %"PRIu32").", rhsa->BaseAddress, rhsa->ProximityDomain);
}

// Parses an ACPI Name-space Device Declaration structure (ANDD).
// Currently, we only add the information about each ACPI name-space enumerated device 
// to the SKB.
static void vtd_parse_andd_structure(char *begin, char *end) 
{
    ACPI_DMAR_ANDD *andd;
    andd = (ACPI_DMAR_ANDD *)begin;
    skb_add_fact("dmar_andd(%"PRIu8", %s).", andd->DeviceNumber, andd->ObjectName);
} 

// Parses the DMA Remapping Reporting (DMAR) ACPI table.
static ACPI_STATUS vtd_parse_dmar_table(void)
{ 
    ACPI_STATUS status;
    ACPI_TABLE_DMAR *dmar;
    ACPI_DMAR_HEADER *header;
    char *structure, *structure_end;

    status = AcpiGetTable(ACPI_SIG_DMAR, 0, (ACPI_TABLE_HEADER **)&dmar);
    if (ACPI_FAILURE(status)) {
        VTD_DEBUG("Failure in retrieving DMAR table.\n");
	return status;
    }
    structure = (char *)dmar + sizeof(ACPI_TABLE_DMAR);
    while (structure != ((char *)dmar + dmar->Header.Length)) {
        header = (ACPI_DMAR_HEADER *)structure;
        structure_end = structure + header->Length;
   
	switch (header->Type) {
	case ACPI_DMAR_TYPE_HARDWARE_UNIT:
	    vtd_parse_drhd_structure(structure, structure_end);
	    break;
	case ACPI_DMAR_TYPE_RESERVED_MEMORY: 
	    vtd_parse_rmrr_structure(structure, structure_end);
	    break;
	case ACPI_DMAR_TYPE_ATSR:
	    vtd_parse_atsr_structure(structure, structure_end);
	    break;
	case ACPI_DMAR_HARDWARE_AFFINITY:
	    vtd_parse_rhsa_structure(structure, structure_end);
	    break;
	case ACPI_DMAR_TYPE_ANDD:
	    vtd_parse_andd_structure(structure, structure_end);
	    break;
	default: assert(!"Reserved for future use!\n");
	}

	structure = structure_end;
    }
    return AE_OK;
}

// Add devices on this platform to the identity domain
void vtd_identity_domain_add_devices(void)
{
    if (vtd_no_units(vtd_units)) return;

    errval_t err;
    err = skb_client_connect();
    assert(err_is_ok(err));

    // Add PCIe-to-PCIe bridges to the identity domain.  
    err = skb_execute_query("pcie_bridges(L),""length(L,Len),writeln(L)");
    assert(err_is_ok(err));

    struct list_parser_status status;
    skb_read_list_init(&status);

    int bus, dev, func;
    while(skb_read_list(&status, "address(%"PRIu32",%"PRIu32",%"PRIu32")", &bus, &dev, &func)) {
        VTD_DEBUG("adding device (bridge) %d:%d:%d to the identity domain.\n", bus, dev, func); 
	err = vtd_domain_add_device(0, bus, dev, func, identity_domain->pml4);
	assert(err == VTD_ERR_DEV_USED || err == SYS_ERR_OK);
    }
 
    err = skb_execute_query("find_devices(L),""length(L,Len),writeln(L)");
    assert(err_is_ok(err));

    skb_read_list_init(&status);

    // Add all PCIe devices present on the platform to the identity domain. Since PCI 
    // devices behind PCIe-to-PCI/PCI-X bridges and conventional PCI bridges have the 
    // same source-id on their transactions, only add endpoint PCI devices on the root 
    // bus.  
    char s_type[5];
    while(skb_read_list(&status, "address(%[a-z],%"PRIu32",%"PRIu32",%"PRIu32")", s_type, &bus, &dev, &func)) {
        VTD_DEBUG("adding %s device %d:%d:%d to the identity domain.\n", s_type, bus, dev, func);
	if (!strncmp(s_type, "pcie", strlen("pcie"))) {
	    err = vtd_domain_add_device(0, bus, dev, func, identity_domain->pml4);
	    assert(err == VTD_ERR_DEV_USED || err == SYS_ERR_OK);
	} else if (!strncmp(s_type, "pci", strlen("pci"))) {
	    if (bus == 0) {
	        err = vtd_domain_add_device(0, bus, dev, func, identity_domain->pml4);
		assert(err == VTD_ERR_DEV_USED || err == SYS_ERR_OK);
	    }
	}
    }
}

int vtd_init(void)
{
    ACPI_STATUS as;
    vtd_map_attr = (cache_coherence ? VREGION_FLAGS_READ_WRITE :
			              VREGION_FLAGS_READ_WRITE_NOCACHE);
    as = vtd_parse_dmar_table();
    if (ACPI_FAILURE(as)) {
        return 1;
    }
   
    // When we have finished parsing the DMAR table, we create the identity 
    // domain and determine the domain-id bounds that can be used on all 
    // hardware units.
    domains = vtd_new_domain_list();
    vtd_create_did_bounds(vtd_units, domains);
    vtd_create_identity_domain();
    vtd_add_dmar_devices();

    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, vtd_units) {
        vtd_set_root_table(u);
	vtd_trnsl_enable(u);
	skb_add_fact("vtd_enabled(%"PRIu16",%"PRIu8").", u->pci_seg, vtd_coherency(u));
    }

    VTD_DEBUG("Enabling DMA remapping succeeded\n");

    return 0;
}
