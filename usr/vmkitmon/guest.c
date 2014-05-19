
/**
 * \file
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include "vmkitmon.h"
#include <barrelfish/lmp_endpoints.h>
#include "x86.h"
#include "svm.h"
#include "realmode.h"
#include "hdd.h"
#include "console.h"
#include "pc16550d.h"
#include "apic.h"
#include "lpc.h"
#include "pci.h"
#include "pci_host.h"
#include "pci_devices.h"
#include "pci_ethernet.h"

#define VMCB_SIZE       0x1000      // 4KB
#define IOPM_SIZE       0x3000      // 12KB
#define MSRPM_SIZE      0x2000      // 8KB
#define RM_MEM_SIZE     (0x100000 + BASE_PAGE_SIZE)    // 1MB + A20 gate space

#define APIC_BASE       0xfee00000

#define SERIAL_DRIVER   "serial0.raw"

#define MIN(x,y) ((x)<(y)?(x):(y))

lvaddr_t guest_offset = 0;
static struct guest __guest;
static struct guest *__guestp = NULL;
/// stores the last used guest ASID
static uint32_t last_guest_asid = 0;

// FIXME: this is somewhat broken by design... we should emit proper exceptions
//        to the guest opposed to just halt the VM
#define guest_assert(g, e) \
    ((e) ? (void)0 : (handle_vmexit_unhandeled(g), assert(e)))

static errval_t
guest_slot_alloc(struct guest *g, struct capref *ret)
{
    return g->slot_alloc.a.alloc(&g->slot_alloc.a, ret);
}

errval_t guest_vspace_map_wrapper(struct vspace *vspace, lvaddr_t vaddr,
                                         struct capref frame,  size_t size)
{
    errval_t err;
    struct vregion *vregion = NULL;
    struct memobj_one_frame *memobj = NULL;

    // Allocate space
    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    memobj = malloc(sizeof(struct memobj_one_frame));
    if (!memobj) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create the objects
    err = memobj_create_one_frame(memobj, size, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
        goto error;
    }
    err = memobj->m.f.fill(&memobj->m, 0, frame, size);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_FILL);
        goto error;
    }
    err = vregion_map_fixed(vregion, vspace, &memobj->m, 0, size, vaddr,
                            VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_EXECUTE);
    if (err_is_fail(err)) {
        err = LIB_ERR_VSPACE_MAP;
        goto error;
    }
    err = memobj->m.f.pagefault(&memobj->m, vregion, 0, 0);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        goto error;
    }

    return SYS_ERR_OK;

 error: // XXX: proper cleanup
    if (vregion) {
        free(vregion);
    }
    if (memobj) {
        free(memobj);
    }
    return err;
}

#define GUEST_VSPACE_SIZE (1ul<<32) // GB

static errval_t vspace_map_wrapper(lvaddr_t vaddr, struct capref frame,
                                   size_t size)
{
    errval_t err;
    static struct memobj_anon *memobj = NULL;
    static struct vregion *vregion = NULL;
    static bool initialized = false;

    if (!initialized) {
        // Allocate space
        memobj = malloc(sizeof(struct memobj_anon));
        if (!memobj) {
            return LIB_ERR_MALLOC_FAIL;
        }
        vregion = malloc(sizeof(struct vregion));
        if (!vregion) {
            return LIB_ERR_MALLOC_FAIL;
        }

        // Create a memobj and vregion
        err = memobj_create_anon(memobj, GUEST_VSPACE_SIZE, 0);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
        }
        err = vregion_map(vregion, get_current_vspace(), &memobj->m, 0,
                          GUEST_VSPACE_SIZE, VREGION_FLAGS_READ_WRITE);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VREGION_MAP);
        }

        guest_offset = vregion_get_base_addr(vregion);
        initialized = true;
    }

    // Create mapping
    err = memobj->m.f.fill(&memobj->m, vaddr, frame, size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_FILL);
    }
    err = memobj->m.f.pagefault(&memobj->m, vregion, vaddr, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
    }

    return SYS_ERR_OK;
}
// allocates some bytes of memory for the guest starting at a specific addr
// also performs the mapping into the vspace of the monitor
errval_t
alloc_guest_mem(struct guest *g, lvaddr_t guest_paddr, size_t bytes)
{
    errval_t err;

    // only allow multiple of page sizes to be allocated
    assert(bytes > 0 && (bytes & BASE_PAGE_MASK) == 0);
    // do not allow allocation outside of the guests physical memory
    assert(guest_paddr + bytes <= g->mem_high_va);

    // Allocate frame
    struct capref cap;
    err = guest_slot_alloc(g, &cap);




    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }
    err = frame_create(cap, bytes, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_CREATE);
    }

    // Map into the guest vspace
    err = guest_vspace_map_wrapper(&g->vspace, guest_paddr, cap, bytes);
    if (err_is_fail(err)) {
        return err;
    }

    // Create a copy of the capability to map in our vspace
    struct capref host_cap;
    err = slot_alloc(&host_cap);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_copy(host_cap, cap);
    if (err_is_fail(err)) {
        return err;
    }

    // Map into my vspace
    err = vspace_map_wrapper(guest_to_host(guest_paddr), host_cap, bytes);
    if (err_is_fail(err)) {
        return err;
    }

	struct frame_identity frameid = { .base = 0, .bits = 0 };
	errval_t r = invoke_frame_identify(cap, &frameid);
	assert(err_is_ok(r));
	VMKIT_PCI_DEBUG("alloc_guest_mem: frameid.base: 0x%lx, frameid.bits: %d, g->mem_low_va: 0x%lx, g->mem_high_va: 0x%lx\n",frameid.base, frameid.bits, g->mem_low_va, g->mem_high_va);

    return SYS_ERR_OK;
}

static void
initialize_iopm (struct guest *self) {
    // intercept all IO port accesses (for now)
    memset((void*)self->iopm_va, 0xFF, IOPM_SIZE);
}

// access_mode: 0 all access, 1 read intercept, 2 write intercept, 3 all interc.
static inline void
set_msr_access (struct guest *g, uint32_t msr, int access_mode)
{
    assert(access_mode >= 0 && access_mode <= 3);

    // a region a 2K bytes represents the access bits of 8K MSRs, therefore each
    // MSR takes two bits (one for rdmsr and one for wrmsr)
    uintptr_t byte_offset = (msr & 0xffff) / 4;
    int bit_offset = ((msr & 0xffff) % 4) * 2;

    if (msr < 0x2000) {
        // do nothing
    } else if (msr >= 0xc0000000 && msr < 0xc0002000) {
        byte_offset += 0x800;
    } else if (msr >= 0xc0010000 && msr < 0xc0012000) {
        byte_offset += 0x1000;
    } else {
        assert(!"not reached");
    }

    assert(byte_offset < MSRPM_SIZE);

    // read the byte holding the relevant bits
    uint8_t val = *(uint8_t *)(g->msrpm_va + byte_offset);
    // set the access params according to the arguments
    val = (val & ~(0x3 << bit_offset)) | (access_mode << bit_offset);
    // store the modified value back in the map
    *(uint8_t *)(g->msrpm_va + byte_offset) = val;

    //printf("MSR: msr %x, byte_offset %lx, bit_offset %x, val %x\n", msr, byte_offset, bit_offset, val);
}

static void
initialize_msrpm (struct guest *g) {
    // intercept all MSR accesses (for now)
    memset((void*)g->msrpm_va, 0xff, MSRPM_SIZE);

#if 0
    // allow performance counters and evnets MSR accesses
    set_msr_access (g, 0xc0010000, 0);
    set_msr_access (g, 0xc0010001, 0);
    set_msr_access (g, 0xc0010002, 0);
    set_msr_access (g, 0xc0010003, 0);
    set_msr_access (g, 0xc0010004, 0);
    set_msr_access (g, 0xc0010005, 0);
    set_msr_access (g, 0xc0010006, 0);
    set_msr_access (g, 0xc0010007, 0);
#endif
}

#define INIT_DATA_SEGREG(vmcb,x)                 \
do {                                             \
    amd_vmcb_seg_attrib_t __sa = {               \
        .segtype = 3,                            \
        .p = 1,                                  \
        .s = 1                                   \
    };                                           \
    amd_vmcb_##x## _attrib_wr((vmcb), __sa);     \
    amd_vmcb_##x## _selector_wr((vmcb), 0x0);    \
    amd_vmcb_##x## _base_wr((vmcb), 0x0);        \
    amd_vmcb_##x## _limit_wr((vmcb), 0xffff);    \
} while (0)

#define INIT_CODE_SEGREG(vmcb,x)                 \
do {                                             \
    amd_vmcb_seg_attrib_t __sa = {               \
        .segtype = 11,                           \
        .p = 1,                                  \
        .s = 1                                   \
    };                                           \
    amd_vmcb_##x## _attrib_wr((vmcb), __sa);     \
    amd_vmcb_##x## _selector_wr((vmcb), 0xf000); \
    amd_vmcb_##x## _base_wr((vmcb), 0xffff0000); \
    amd_vmcb_##x## _limit_wr((vmcb), 0xffff);    \
} while (0)

#define INIT_SYS_SEGREG(vmcb,x)                  \
do {                                             \
    amd_vmcb_seg_attrib_t __sa = {               \
        .segtype = 2,                            \
        .p = 1                                   \
    };                                           \
    amd_vmcb_##x## _attrib_wr((vmcb), __sa);     \
    amd_vmcb_##x## _selector_wr((vmcb), 0x0);    \
    amd_vmcb_##x## _base_wr((vmcb), 0x0);        \
    amd_vmcb_##x## _limit_wr((vmcb), 0xffff);    \
} while (0)

/* This method initializes a new VMCB memory regsion and sets the initial
 * machine state as defined by the AMD64 architecture specification */
static void
initialize_vmcb (struct guest *self) {
    amd_vmcb_initialize(&self->vmcb, (mackerel_addr_t)self->vmcb_va);

    // 1. Initialize intercepts

    /* For now we intercept just everything */

    amd_vmcb_cr_access_wr_raw(&self->vmcb, ~0u);
    amd_vmcb_cr_access_rdcr2_wrf(&self->vmcb, 0);
    amd_vmcb_cr_access_wrcr2_wrf(&self->vmcb, 0);
    amd_vmcb_cr_access_rdcr4_wrf(&self->vmcb, 0);
    amd_vmcb_cr_access_wrcr4_wrf(&self->vmcb, 0);

    // FIXME: ignoring DR accesses may be insecure
    //amd_vmcb_dr_access_wr_raw(&self->vmcb, ~0u);
    amd_vmcb_exceptions_wr_raw(&self->vmcb, ~0u);
    amd_vmcb_exceptions_vector7_wrf(&self->vmcb, 0);
    amd_vmcb_exceptions_vector14_wrf(&self->vmcb, 0);

    amd_vmcb_intercepts_wr_raw(&self->vmcb, 0x1fffffffffff);
    amd_vmcb_intercepts_pushf_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_popf_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_invlpg_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_rdtsc_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_rdtscp_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_iret_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_wbinvd_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_pause_wrf(&self->vmcb, 0);
    amd_vmcb_intercepts_vintr_wrf(&self->vmcb, 0);

    // 2. Setup some config fields

    // physical addresses of IOPM and MSRPM_SIZE
    amd_vmcb_iopm_base_pa_wr(&self->vmcb, self->iopm_pa);
    amd_vmcb_msrpm_base_pa_wr(&self->vmcb, self->msrpm_pa);
    // assign guest ASID
    // FIXME: use real asid allocator. BF does not know about tagged TLBs atm
    amd_vmcb_tlb_guest_asid_wrf(&self->vmcb, ++last_guest_asid);
    // enable virtual intr masking
    amd_vmcb_vintr_vintr_masking_wrf(&self->vmcb, 1);
    // enable nested paging
    amd_vmcb_np_enable_wrf(&self->vmcb, 1);

    /* 3. Guest state initialization
     * according to Intels Manual 3A: Table 9-1. */

    // The second bit of rflags needs to be 1, also indicate that we support the
    // CPUID instruction.
    amd_vmcb_rflags_wr_raw(&self->vmcb, 0x00200002);
    amd_vmcb_rip_wr(&self->vmcb, 0x0000fff0);
    amd_vmcb_cr0_wr_raw(&self->vmcb, 0x60000010);

    INIT_CODE_SEGREG(&self->vmcb, cs);
    INIT_DATA_SEGREG(&self->vmcb, ss);
    INIT_DATA_SEGREG(&self->vmcb, ds);
    INIT_DATA_SEGREG(&self->vmcb, es);
    INIT_DATA_SEGREG(&self->vmcb, fs);
    INIT_DATA_SEGREG(&self->vmcb, gs);

    INIT_SYS_SEGREG(&self->vmcb, gdtr);
    INIT_SYS_SEGREG(&self->vmcb, idtr);
    INIT_SYS_SEGREG(&self->vmcb, ldtr);
    INIT_SYS_SEGREG(&self->vmcb, tr);

    amd_vmcb_dr6_wr(&self->vmcb, 0xffff0ff0);
    amd_vmcb_dr7_wr(&self->vmcb, 0x00000400);

    // taken from the linux SVM source
    amd_vmcb_gpat_wr(&self->vmcb, 0x0007040600070406ul);

    // svm requires guest EFER.SVME to be set
    amd_vmcb_efer_svme_wrf(&self->vmcb, 1);
}

static void
idc_handler(void *arg)
{
    struct guest *g = arg;
    errval_t err;

    // consume message
    struct lmp_recv_buf buf = { .buflen = 0 };
    err = lmp_endpoint_recv(g->monitor_ep, &buf, NULL);
    assert(err_is_ok(err));

    // run real handler
    guest_handle_vmexit(g);

    // re-register
    struct event_closure cl = {
        .handler = idc_handler,
        .arg = arg,
    };
    err = lmp_endpoint_register(g->monitor_ep, get_default_waitset(), cl);
    assert(err_is_ok(err));
}

/* This method duplicates some code from spawndomain since we need to spawn very
 * special domains */
static void
spawn_guest_domain (struct guest *self) {
    errval_t err;

    // create the guest virtual address space
    struct capref vnode_cap;
    err = guest_slot_alloc(self, &vnode_cap);
    assert(err_is_ok(err));
    err = vnode_create(vnode_cap, ObjType_VNode_x86_64_pml4);
    assert(err_is_ok(err));

    struct pmap *pmap = malloc(sizeof(struct pmap_x86));
    assert(pmap);
    err = pmap_x86_64_init(pmap, &self->vspace, vnode_cap, NULL);
    assert(err_is_ok(err));
    err = vspace_init(&self->vspace, pmap);
    assert(err_is_ok(err));

    // create DCB
    err = guest_slot_alloc(self, &self->dcb_cap);
    assert(err_is_ok(err));
    err = dispatcher_create(self->dcb_cap);
    assert(err_is_ok(err));

    // create end point
    struct capref ep_cap;

    // use minimum-sized endpoint, because we don't need to buffer >1 vmexit
    err = endpoint_create(LMP_RECV_LENGTH, &ep_cap, &self->monitor_ep);
    assert(err_is_ok(err));

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = idc_handler,
        .arg = self,
    };
    err = lmp_endpoint_register(self->monitor_ep, get_default_waitset(), cl);
    assert(err_is_ok(err));

    // setup the DCB
    err = invoke_dispatcher_setup_guest(self->dcb_cap, ep_cap, vnode_cap,
                                        self->vmcb_cap, self->ctrl_cap);
    assert(err_is_ok(err));

    // set up the guests physical address space
    self->mem_low_va = 0;
    // FIXME: Hardcoded guest memory size
    self->mem_high_va = 0x80000000;   // 2 GiB
    // allocate the memory used for real mode
    // This is not 100% necessary since one could also catch the pagefaults.
    // If we allocate the whole memory at once we use less caps and reduce
    // the risk run out of CSpace.
    err = alloc_guest_mem(self, 0x0, 0x80000000);
    assert_err(err, "alloc_guest_mem");
}

static void
install_grub_stage2 (struct guest *g, void *img, size_t img_size)
{
    assert(img != NULL);

    /* the grub image goes to 0x8000 according to
     * http://www.gnu.org/software/grub/manual/html_node/Memory-map.html */
    memcpy((void *)(guest_to_host(g->mem_low_va + 0x8000)), img, img_size);
    // according to grub stage2 source its entry point is at 0x8200
    amd_vmcb_rip_wr(&g->vmcb, 0x8200);
    // switch to the first segment
    amd_vmcb_cs_selector_wr(&g->vmcb, 0x0);
    amd_vmcb_cs_base_wr(&g->vmcb, 0x0);
    amd_vmcb_cs_limit_wr(&g->vmcb, 0xffff);
}

#if 0
static void
install_debug_app (struct guest *g)
{
    //static uint8_t app[] = { 0xcd, 0x20 };
    static uint8_t app[] = { 0xcd, 0x20, 0x90, 0x90, 0x90, 0x90, 0x90 };
    memcpy((void *)g->rm_mem_va, app, sizeof(app));
    amd_vmcb_rip_wr(&g->vmcb, 0x0);
    // disable nested pageing in real mode
    amd_vmcb_np_enable_wrf(&g->vmcb, 0);
    // enable paged real mode
    //amd_vmcb_cr0_pg_wrf(&g->vmcb, 0x1);
    //g->save_area->cr0 |= X86_CR0_PE_MASK;
    amd_vmcb_rsp_wr(&g->vmcb, 0x1000);
    amd_vmcb_cs_selector_wr(&g->vmcb, 0x0);
    amd_vmcb_cs_base_wr(&g->vmcb, 0x0);
    amd_vmcb_cs_limit_wr(&g->vmcb, 0xffff);
    //g->save_area->cs.selector = 0x1000;
    //g->save_area->cs.base = 0x10000;
    //g->save_area->cs.base = 0x1ffff;
}
#endif

static bool
virq_pending (void *ud, uint8_t *irq, uint8_t *irq_prio)
{
    assert(ud != NULL);

    struct guest *g = ud;

    if (amd_vmcb_vintr_rd(&g->vmcb).virq == 1) {
        if (irq != NULL) {
            *irq = amd_vmcb_vintr_rd(&g->vmcb).vintr_vector;
        }
        if (irq_prio != NULL) {
            *irq_prio = amd_vmcb_vintr_rd(&g->vmcb).vintr_prio;
        }
        return true;
    } else {
        return false;
    }
}

static void
virq_handler (void *ud, uint8_t irq, uint8_t irq_prio)
{
    assert(ud != NULL);

    struct guest *g = ud;

    // tell the hw extensions that there is a virtual IRQ pending
    amd_vmcb_vintr_virq_wrf(&g->vmcb, 1);
    amd_vmcb_vintr_vintr_prio_wrf(&g->vmcb, irq_prio);
    amd_vmcb_vintr_vintr_vector_wrf(&g->vmcb, irq);
    amd_vmcb_vintr_v_ign_tpr_wrf(&g->vmcb, 1);

    // if the guest is currently waiting then we have to restart it to make
    // forward progress
    if (!g->runnable) {
        g->runnable = true;
        guest_make_runnable(g, true);
    }
}

static void
guest_setup (struct guest *g)
{
    errval_t err;

    // initialize the guests slot_allocator
    err = multi_slot_alloc_init(&g->slot_alloc, DEFAULT_CNODE_SLOTS, NULL);
    assert_err(err, "multi_cspace_alloc_init_raw");

    struct frame_identity fi;

    // allocate memory for the vmcb
    err = guest_slot_alloc(g, &g->vmcb_cap);
    assert_err(err, "guest_cspace_alloc");
    err = frame_create(g->vmcb_cap, VMCB_SIZE, NULL);
    assert_err(err, "frame_create");
    err = invoke_frame_identify(g->vmcb_cap, &fi);
    assert_err(err, "frame_identify");
    g->vmcb_pa = fi.base;
    err = vspace_map_one_frame_attr((void**)&g->vmcb_va, VMCB_SIZE, g->vmcb_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed");
    }

    // guest control
    err = frame_alloc(&g->ctrl_cap, sizeof(struct guest_control), NULL);
    assert_err(err, "frame_alloc");
    size_t size = ROUND_UP(sizeof(struct guest_control), BASE_PAGE_SIZE);
    err = vspace_map_one_frame_attr((void**)&g->ctrl, size, g->ctrl_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed");
    }
    g->ctrl->num_vm_exits_with_monitor_invocation = 0;
    g->ctrl->num_vm_exits_without_monitor_invocation = 0;

    // allocate memory for the iopm
    err = frame_alloc(&g->iopm_cap, IOPM_SIZE, NULL);
    assert_err(err, "frame_alloc");
    err = invoke_frame_identify(g->iopm_cap, &fi);
    assert_err(err, "frame_identify");
    g->iopm_pa = fi.base;
    err = vspace_map_one_frame_attr((void**)&g->iopm_va, IOPM_SIZE, g->iopm_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed");
    }

    // allocate memory for the msrpm
    err = frame_alloc(&g->msrpm_cap, MSRPM_SIZE, NULL);
    assert_err(err, "frame_alloc");
    err = invoke_frame_identify(g->msrpm_cap, &fi);
    assert_err(err, "frame_identify");
    g->msrpm_pa = fi.base;
    err = vspace_map_one_frame_attr((void**)&g->msrpm_va, MSRPM_SIZE,
                                    g->msrpm_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame_attr failed");
    }

    // initialize the allocated structures
    initialize_iopm(g);
    initialize_msrpm(g);
    initialize_vmcb(g);

    // spawn the guest domain
    spawn_guest_domain(g);
    assert (grub_image != NULL);
    install_grub_stage2(g, grub_image, grub_image_size);
    //install_debug_app(g);

    // add virtual hardware
    g->apic = apic_new(APIC_BASE);
    g->lpc = lpc_new(virq_handler, virq_pending, g, g->apic);
    if (hdd0_image != NULL) {
        g->hdds[0] = hdd_new_from_memory(hdd0_image, hdd0_image_size);
        g->hdd_count++;
    }
    g->console = console_new();
    g->serial_ports[0] = pc16550d_new(0x3f8, 4, g->lpc);

    // FIXME: Which virtual uart port is connected to which host port
    //        should be adjustable from the command line or a configuration
    //        file.
    pc16550d_attach_to_host_uart(g->serial_ports[0], SERIAL_DRIVER);
    g->serial_ports[1] = pc16550d_new(0x2f8, 3, g->lpc);
    g->serial_ports[2] = pc16550d_new(0x3e8, 4, g->lpc);
    g->serial_ports[3] = pc16550d_new(0x2e8, 3, g->lpc);
    g->serial_port_count = 4;

    g->pci = pci_new();
    init_host_devices(g->pci);
    
    struct pci_device *ethernet = pci_ethernet_new(g->lpc, g);
    int r = pci_attach_device(g->pci, 0, 2, ethernet);
	assert(r == 0);

	struct pci_device *vmkitmon_eth = pci_vmkitmon_eth_new(g->lpc, g);
	r = pci_attach_device(g->pci, 0, 3, vmkitmon_eth);
	assert(r==0);

    // set up bios memory
    // FIXME: find a modular way to do this
    *(uint16_t *)guest_to_host(g->mem_low_va + 0x400) = 0x3f8;  // COM1
    *(uint16_t *)guest_to_host(g->mem_low_va + 0x402) = 0x2f8;  // COM2

    g->runnable = true;
}

/**
 * \brief Create a new guest.
 *
 * This function creates a new guest. It will do everything necessary to make
 * the guest accept images to run. It will create a new domain and assign some
 * memory to that domain. Afterwards it will load a bios into the memory and
 * set the guest initial IP to the POST entry of the bios.
 *
 * \return The pointer to the newly created structure describing the guest.
 */
struct guest *
guest_create (void)
{
    // support the allocation of one guest for now
    assert(__guestp == NULL);
    __guestp = &__guest;
    memset(__guestp, 0, sizeof(struct guest));
    guest_setup(__guestp);
    return __guestp;
}

static int
run_realmode (struct guest *g)
{
    int r;

    realmode_switch_to(g);
    r = realmode_exec();
    assert(r == REALMODE_ERR_OK);
    realmode_switch_from(g);

    guest_handle_vmexit(g);

    return 0;
};

/**
 * \brief Marks a guest as runnable.
 *
 * A call to this method will update the guest's runnable state and, if made
 * runnable, yield the remaining time slice to the guest domain.
 *
 * \return Zero on success, non-zero on error
 */
errval_t
guest_make_runnable (struct guest *g, bool run)
{
    assert(g->runnable);

    errval_t err;

    /* If the guest is currently in real mode (CR0.PE flag clear) then we do not
     * schedule the domain to run the virtualization but run the real-mode
     * emulation */
    if (UNLIKELY(run && amd_vmcb_cr0_rd(&g->vmcb).pe == 0)) {
        if (!g->emulated_before_exit) {
            // do the inverse of the code below
            amd_vmcb_intercepts_rdgdtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_wrgdtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_rdldtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_wrldtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_rdidtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_wridtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_rdtr_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_wrtr_wrf(&g->vmcb, 1);
            amd_vmcb_cr_access_rdcr0_wrf(&g->vmcb, 1);
            amd_vmcb_cr_access_wrcr0_wrf(&g->vmcb, 1);
            amd_vmcb_cr_access_rdcr3_wrf(&g->vmcb, 1);
            amd_vmcb_cr_access_wrcr3_wrf(&g->vmcb, 1);
            amd_vmcb_intercepts_intn_wrf(&g->vmcb, 1);

            // mark guest as emulated
            g->emulated_before_exit = true;
        }
#if 0 /* why create a thread for this? it seems fine without! -AB */
        struct thread *t = thread_create((thread_func_t)run_realmode, g);
        assert(t != NULL);
        err = thread_detach(t);
        assert(err_is_ok(err));
#else
        run_realmode(g);
#endif
        return SYS_ERR_OK;
    }

    /* every time we move the machine from the emulated to virtualized we need
     * to adjust some intercepts */
    if (UNLIKELY(run && g->emulated_before_exit)) {
        // we enforce NP to be enabled (no shadow paging support)
        assert(amd_vmcb_np_rd(&g->vmcb).enable == 1);

        // disable GDTR intercept
        amd_vmcb_intercepts_rdgdtr_wrf(&g->vmcb, 0);
        amd_vmcb_intercepts_wrgdtr_wrf(&g->vmcb, 0);
        // disable GDTR intercept
        amd_vmcb_intercepts_rdldtr_wrf(&g->vmcb, 0);
        amd_vmcb_intercepts_wrldtr_wrf(&g->vmcb, 0);
        // disable IDTR intercept
        amd_vmcb_intercepts_rdidtr_wrf(&g->vmcb, 0);
        amd_vmcb_intercepts_wridtr_wrf(&g->vmcb, 0);
        // disable TR intercept
        amd_vmcb_intercepts_rdtr_wrf(&g->vmcb, 0);
        amd_vmcb_intercepts_wrtr_wrf(&g->vmcb, 0);
        // disable non essential CR0 access intercepts_t
        amd_vmcb_cr_access_rdcr0_wrf(&g->vmcb, 0);
        amd_vmcb_cr_access_wrcr0_wrf(&g->vmcb, 0);
        // disable CR3 access intercepts
        assert(amd_vmcb_np_rd(&g->vmcb).enable != 0);
        amd_vmcb_cr_access_rdcr3_wrf(&g->vmcb, 0);
        amd_vmcb_cr_access_wrcr3_wrf(&g->vmcb, 0);
        // disable INTn intercept
        // we have to be outside of real mode for this to work
        assert(amd_vmcb_cr0_rd(&g->vmcb).pe != 0);
        amd_vmcb_intercepts_intn_wrf(&g->vmcb, 0);

        // mark guest as not emulated
        g->emulated_before_exit = false;
    }

    // update the guets domain's runnable state
    err = invoke_dispatcher(g->dcb_cap, NULL_CAP, NULL_CAP, NULL_CAP, NULL_CAP, run);
    assert_err(err, "dispatcher_make_runnable");
    // yield the dispatcher
    if (run) {
        thread_yield_dispatcher(NULL_CAP);
    }

    return SYS_ERR_OK;
}

/* VMEXIT hanlders */

#define HANDLER_ERR_OK          (0)
#define HANDLER_ERR_FATAL       (-1)

static int
handle_vmexit_unhandeled (struct guest *g)
{
    printf("Unhandeled guest vmexit:\n");
    printf(" code:\t  %lx\n", amd_vmcb_exitcode_rd(&g->vmcb));
    printf(" info1:\t  %lx\n", amd_vmcb_exitinfo1_rd(&g->vmcb));
    printf(" info2:\t  %lx\n", amd_vmcb_exitinfo2_rd(&g->vmcb));
    printf(" intinfo: %lx\n", amd_vmcb_exitintinfo_rd(&g->vmcb));

    printf("VMCB save area:\n");
    printf(" cr0:\t%lx\n", amd_vmcb_cr0_rd_raw(&g->vmcb));
    printf(" cr2:\t%lx\n", amd_vmcb_cr2_rd_raw(&g->vmcb));
    printf(" cr3:\t%lx\n", amd_vmcb_cr3_rd_raw(&g->vmcb));
    printf(" cr4:\t%lx\n", amd_vmcb_cr4_rd_raw(&g->vmcb));
    printf(" efer:\t%lx\n", amd_vmcb_efer_rd_raw(&g->vmcb));
    printf(" rip:\t%lx\n", amd_vmcb_rip_rd_raw(&g->vmcb));
    printf(" cs:\tselector %x, base %lx, limit %x, attrib %x\n",
           amd_vmcb_cs_selector_rd(&g->vmcb), amd_vmcb_cs_base_rd(&g->vmcb),
           amd_vmcb_cs_limit_rd(&g->vmcb), amd_vmcb_cs_attrib_rd_raw(&g->vmcb));
    printf(" ds:\tselector %x, base %lx, limit %x, attrib %x\n",
           amd_vmcb_ds_selector_rd(&g->vmcb), amd_vmcb_ds_base_rd(&g->vmcb),
           amd_vmcb_ds_limit_rd(&g->vmcb), amd_vmcb_ds_attrib_rd_raw(&g->vmcb));
    printf(" es:\tselector %x, base %lx, limit %x, attrib %x\n",
           amd_vmcb_es_selector_rd(&g->vmcb), amd_vmcb_es_base_rd(&g->vmcb),
           amd_vmcb_es_limit_rd(&g->vmcb), amd_vmcb_es_attrib_rd_raw(&g->vmcb));
    printf(" ss:\tselector %x, base %lx, limit %x, attrib %x\n",
           amd_vmcb_ss_selector_rd(&g->vmcb), amd_vmcb_ss_base_rd(&g->vmcb),
           amd_vmcb_ss_limit_rd(&g->vmcb), amd_vmcb_ss_attrib_rd_raw(&g->vmcb));
    printf(" rax:\t%lx\n", amd_vmcb_rax_rd_raw(&g->vmcb));
    printf(" rbx:\t%lx\n", g->ctrl->regs.rbx);
    printf(" rcx:\t%lx\n", g->ctrl->regs.rcx);
    printf(" rdx:\t%lx\n", g->ctrl->regs.rdx);
    printf(" rsi:\t%lx\n", g->ctrl->regs.rsi);
    printf(" rdi:\t%lx\n", g->ctrl->regs.rdi);

    return HANDLER_ERR_FATAL;
}

static inline uint64_t
lookup_paddr_long_mode (struct guest *g, uint64_t vaddr)
{
    union x86_lm_va va = { .raw = vaddr };
    uint64_t *page_table;

    // get a pointer to the pml4 table
    page_table = (uint64_t *)guest_to_host(amd_vmcb_cr3_rd(&g->vmcb));
    // get pml4 entry
    union x86_lm_pml4_entry pml4e = { .raw = page_table[va.u.pml4_idx] };
    assert (pml4e.u.p == 1);

    // get a pointer to the pdp table
    page_table = (uint64_t *)guest_to_host(pml4e.u.pdp_base_pa << 12);
    // get pdp entry
    union x86_lm_pdp_entry pdpe = { .raw = page_table[va.u.pdp_idx] };
    assert(pdpe.u.p == 1);
    // check for 1GB page (PS bit set)
    if (pdpe.u.ps == 1) {
        return (pdpe.u1gb.base_pa << 30) | va.u1gb.pa_offset;
    }

    // get a pointer to the pd table
    page_table = (uint64_t *)guest_to_host(pdpe.u.pd_base_pa << 12);
    // get pd entry
    union x86_lm_pd_entry pde = { .raw = page_table[va.u.pd_idx] };
    if (pde.u.p == 0) {
        printf("g2h %lx, pml4e %p %lx, pdpe %p %lx, pde %p %lx\n", guest_to_host(0), &pml4e, pml4e.raw, &pdpe, pdpe.raw, &pde, pde.raw);
    }
    assert(pde.u.p == 1);
    // check for 2MB page (PS bit set)
    if (pde.u.ps == 1) {
        return (pde.u2mb.base_pa << 21) | va.u2mb.pa_offset;
    }

    // get a pointer to the page table
    page_table = (uint64_t *)guest_to_host(pde.u.pt_base_pa << 12);
    // get the page table entry
    union x86_lm_pt_entry pte = { .raw = page_table[va.u.pt_idx] };
    assert(pte.u.p == 1);

    return (pte.u.base_pa << 12) | va.u.pa_offset;
}

static inline uint32_t
lookup_paddr_legacy_mode (struct guest *g, uint32_t vaddr)
{
//	printf("lookup_paddr_legacy_mode enter\n");
    // PAE not supported
    guest_assert(g, amd_vmcb_cr4_rd(&g->vmcb).pae == 0);

    union x86_legm_va va = { .raw = vaddr };
    uint32_t *page_table;

    // get a pointer to the pd table
    page_table = (uint32_t *)guest_to_host(amd_vmcb_cr3_rd(&g->vmcb));
    // get pd entry
    union x86_legm_pd_entry pde = { .raw = page_table[va.u.pd_idx] };
    assert (pde.u.p == 1);
    // check for 4MB page (PS bit set)
    if (pde.u.ps == 1) {
        return (pde.u4mb.base_pa << 22) | va.u4mb.pa_offset;
    }

    // get a pointer to the page table
    page_table = (uint32_t *)guest_to_host(pde.u.pt_base_pa << 12);
    // get the page table entry
    union x86_legm_pt_entry pte = { .raw = page_table[va.u.pt_idx] };
    assert(pte.u.p == 1);

    return (pte.u.base_pa << 12) | va.u.pa_offset;
}

// retunrs a pointer to a byte array starting at the current instruction
static inline int
get_instr_arr (struct guest *g, uint8_t **arr)
{
    if (UNLIKELY(amd_vmcb_cr0_rd(&g->vmcb).pg == 0)) {
    	//printf("Segmentation active!\n");
        // without paging
        // take segmentation into account
        *arr = (uint8_t *)(guest_to_host(g->mem_low_va) +
               amd_vmcb_cs_base_rd(&g->vmcb) +
               amd_vmcb_rip_rd(&g->vmcb));
    } else {
        // with paging
        if (amd_vmcb_efer_rd(&g->vmcb).lma == 1) {
            // long mode
            if (amd_vmcb_cs_attrib_rd(&g->vmcb).l == 1) {
                // 64-bit mode
                *arr = (uint8_t *)guest_to_host(lookup_paddr_long_mode(g,
                                                amd_vmcb_rip_rd(&g->vmcb)));
            } else {
                // cmpatibility mode
                guest_assert(g, !"compatiblity mode not supported yet");
            }
        } else {
            // Legacy (aka. Paged Protected) Mode
            assert(amd_vmcb_cr0_rd(&g->vmcb).pe == 1);

            *arr = (uint8_t *)guest_to_host(lookup_paddr_legacy_mode(g,
                                            amd_vmcb_rip_rd(&g->vmcb)));
        }
    }

    return HANDLER_ERR_OK;
}

static inline uint64_t
get_reg_val_by_reg_num (struct guest *g, uint8_t regnum) {
    switch (regnum) {
    case 0:
        return guest_get_rax(g);
    case 1:
        return guest_get_rcx(g);
    case 2:
        return guest_get_rdx(g);
    case 3:
        return guest_get_rbx(g);
    case 4:
        return guest_get_rsp(g);
    case 5:
        return guest_get_rbp(g);
    case 6:
        return guest_get_rsi(g);
    case 7:
        return guest_get_rdi(g);
    default:
        assert(!"not reached");
        return 0;
    }
}

static inline void
set_reg_val_by_reg_num (struct guest *g, uint8_t regnum, uint64_t val) {
    switch (regnum) {
    case 0:
        guest_set_rax(g, val);
        break;
    case 1:
        guest_set_rcx(g, val);
        break;
    case 2:
        guest_set_rdx(g, val);
        break;
    case 3:
        guest_set_rbx(g, val);
        break;
    case 4:
        guest_set_rsp(g, val);
        break;
    case 5:
        guest_set_rbp(g, val);
        break;
    case 6:
        guest_set_rsi(g, val);
        break;
    case 7:
        guest_set_rdi(g, val);
        break;
    default:
        assert(!"not reached");
        break;
    }
}

static int
handle_vmexit_cr_access (struct guest *g)
{
    int r;
    uint8_t *code = NULL;

    // fetch the location to the code
    r = get_instr_arr(g, &code);
    if (r != HANDLER_ERR_OK) {
        return r;
    }
    assert(code != NULL);

    assert(code[0] == 0x0f && (code[1] == 0x20 || code[1] == 0x22));

    uint64_t val;
    bool read = (code[1] == 0x20);
    union x86_modrm mod;
    mod.raw = code[2];

    // FIXME: use proper exception
    assert(mod.u.mod == 3);

    // source
    if (read) {
        // read from CR
        switch (mod.u.regop) {
        case 0:
            val = amd_vmcb_cr0_rd_raw(&g->vmcb);
            break;
        default:
            printf("CR access: unknown CR source register\n");
            return handle_vmexit_unhandeled(g);
        }
    } else {
        // read from GPR
        val = get_reg_val_by_reg_num(g, mod.u.rm);
    }

    // destination
    if (read) {
        // write to GPR
        switch (mod.u.rm) {
        case 0:
            guest_set_rax(g, val);
            break;
        case 1:
            guest_set_rcx(g, val);
            break;
        case 2:
            guest_set_rdx(g, val);
            break;
        case 3:
            guest_set_rbx(g, val);
            break;
        default:
            printf("CR access: unknown GPR destination register\n");
            return handle_vmexit_unhandeled(g);
        }
    } else {
        // write to CR
        switch (mod.u.regop) {
        case 0:
            amd_vmcb_cr0_wr_raw(&g->vmcb, val);
            break;
        default:
            printf("CR access: unknown CR destination register\n");
            return handle_vmexit_unhandeled(g);
        }
    }

    // advance the rip beyond the instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 3);

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_ldt (struct guest *g)
{
    int r;
    uint8_t *code = NULL;
    uint8_t *mem;

    // this handler supports only real-mode
    assert(amd_vmcb_cr0_rd(&g->vmcb).pe == 0);

    // fetch the location to the code
    r = get_instr_arr(g, &code);
    if (r != HANDLER_ERR_OK) {
        return r;
    }
    mem = (uint8_t *)guest_to_host(g->mem_low_va);
    assert(code != NULL);

    assert (code[0] == 0x0f && code[1] == 0x01);

    // check for relevant instruction prefixes
    bool addr32 = code[-2] == 0x67 || code[-1] == 0x67;
    bool op32 = code[-2] == 0x66 || code[-1] == 0x66;
    // fetch modrm
    union x86_modrm modrm = { .raw = code[2] };

    assert(modrm.u.regop == 2 || modrm.u.regop == 3);
    guest_assert(g, op32);

    uint32_t addr;
    if (addr32) {
        // byte 3-6 hold a 32 bit address to a mem location where the first word
        // holds the limit and the following dword holds the base
        addr = *(uint32_t *)&code[3];
    } else {
        // byte 3-4 hold a 16 bit address to a mem location where the first word
        // holds the limit and the following dword holds the base
        // this address is relative to DS base
        addr = *(uint16_t *)&code[3] + amd_vmcb_ds_base_rd(&g->vmcb);
    }

    // santity check on the addr
    // FIXME: raise a proper exception
    if (addr > g->mem_high_va) {
        printf("Memory access beyond physical address space\n");
        return HANDLER_ERR_FATAL;
    }

    // load the actual register
    if (modrm.u.regop == 2) {
        // LGDT
        amd_vmcb_gdtr_limit_wr(&g->vmcb, *(uint16_t*)(mem + addr));
        amd_vmcb_gdtr_base_wr(&g->vmcb, *(uint32_t*)(mem + addr + 2));
    } else if (modrm.u.regop == 3) {
        // LIDT
        amd_vmcb_idtr_limit_wr(&g->vmcb, *(uint16_t*)(mem + addr));
        amd_vmcb_idtr_base_wr(&g->vmcb, *(uint32_t*)(mem + addr + 2));
    } else {
        assert(!"not reached");
    }

    // advance the rip beyond the instruction
    if (addr32) {
        amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 7);
    } else {
        amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 5);
    }

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_swint (struct guest *g)
{
    int r;
    uint8_t *code = NULL;

    r = get_instr_arr(g, &code);
    if (r != HANDLER_ERR_OK) {
        return r;
    }
    assert (code != NULL);

    // check for correct instruciton
    assert(code[0] == 0xcd);

    // the number of the interrupt is followed by the INT (0xcd) opcode
    uint8_t int_num = code[1];

    // check whether the guest is in real mode
    if (amd_vmcb_cr0_rd(&g->vmcb).pe == 0) {
        // in real mode the interrupts starting at 10 have different meaning
        // examine the sw interrupt
        switch (int_num) {
            case 0x10:
                r = console_handle_int10(g->console, g);
                if (r != HANDLER_ERR_OK) {
                    printf("Unhandeled method on INT 0x10\n");
                    return handle_vmexit_unhandeled(g);
                }
                break;
            case 0x12:
                switch (guest_get_ax(g)) {
                    case 0: // GET MEMORY SIZE
                        // our VM always has 1MB of base memory
                        // AX holds the amount of 1KB memory blocks starting at
                        // addr 0 which is 640 (640 KiB)
                        guest_set_ax(g, 640);
                        break;
                    default:
                        printf("Unhandeled method on INT 0x12\n");
                        return handle_vmexit_unhandeled(g);
                }
                break;
            case 0x13:
                // Bootable CD-ROM - GET STATUS
                if (guest_get_ax(g) == 0x4b01) {
                    // no cdrom support
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                }
                // DISK RESET
                else if (guest_get_ah(g) == 0) {
                    for (int i = 0; i < g->hdd_count; i++) {
                        hdd_reset(g->hdds[i]);
                    }
                }
                // DISK - GET DRIVE PARAMETERS (PC,XT286,CONV,PS,ESDI,SCSI)
                else if (guest_get_ah(g) == 0x08) {
                    uint8_t dl = guest_get_dl(g);

                    // only respond to installed hard disks
                    if ((dl >> 7) && ((dl & 0x7f) < g->hdd_count)) {
                        uint16_t c;
                        uint8_t h, s;

                        r = hdd_get_geometry_chs(g->hdds[dl & 0x7f], &c, &h, &s);
                        assert(r == 0);

                        // set some return values for success
                        guest_set_ah(g, 0);
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                        guest_set_bl(g, 0);
                        // store the geometry into the correct registers
                        guest_set_cx(g, c << 6 | (s & 0x3f));
                        guest_set_dh(g, h);
                        guest_set_dl(g, g->hdd_count);
                    } else {
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                        // it is not really clear to me what ah should contain
                        // when the drive is not present, so set it to FF
                        guest_set_ah(g, 1);
                    }
                }
                // INT 13 Extensions - INSTALLATION CHECK
                else if (guest_get_ah(g) == 0x41 && guest_get_bx(g) == 0x55aa) {
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                    guest_set_bx(g, 0xaa55);
                    guest_set_ah(g, 0x01); // Drive extensions 1.x
                    guest_set_al(g, 0);
                    guest_set_cx(g, 0x5);
                }
                // IBM/MS INT 13 Extensions - EXTENDED READ
                else if (guest_get_ah(g) == 0x42) {
                    uint8_t dl = guest_get_dl(g);

                    // only respond to installed hard disks
                    if ((dl >> 7) && ((dl & 0x7f) < g->hdd_count)) {
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                        guest_set_ah(g, 0);

                        struct disk_access_block {
                            uint8_t     size;
                            uint8_t     reserved;
                            uint16_t    count;
                            // pointer to the data buffer formated like
                            // SEGMENT:ADDRESS
                            uint32_t    transfer_buffer;
                            uint64_t    abs_block_number;
                        } __attribute__ ((packed));

                        // memory location of the disk access block
                        uintptr_t mem = guest_to_host(g->mem_low_va) +
                                        amd_vmcb_ds_base_rd(&g->vmcb) +
                                        guest_get_si(g);
                        struct disk_access_block *dap = (void *)mem;

                        if (dap->size < 0x10) {
                            amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                            guest_set_ah(g, 1);
                        } else {
                            // dap->transfer buffer points to a real-mode segment
                            // resolve it according to that rules
                            mem = guest_to_host(g->mem_low_va) +
                                  ((dap->transfer_buffer >> 16) << 4) +
                                  (dap->transfer_buffer & 0xffff);

                            size_t count = dap->count;
                            r = hdd_read_blocks(g->hdds[dl & 0x7f],
                                                dap->abs_block_number,
                                                &count, mem);
                            dap->count = count;

                            if (r != HANDLER_ERR_OK) {
                                amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                                guest_set_ah(g, 1);
                            }
                        }
                    } else {
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                        // it is not really clear to me what ah should contain
                        // when the drive is not present, so set it to FF
                        guest_set_ah(g, 1);
                    }
                }
                // IBM/MS INT 13 Extensions - GET DRIVE PARAMETERS
                else if (guest_get_ah(g) == 0x48) {
                    uint8_t dl = guest_get_dl(g);

                    // only respond to installed hard disks
                    if ((dl >> 7) && ((dl & 0x7f) < g->hdd_count)) {
                        // structure to hold drive info
                        struct drive_params {
                            uint16_t size;
                            uint16_t flags;
                            uint32_t cylinders;
                            uint32_t heads;
                            uint32_t sectors;
                            uint64_t total_sectors;
                            uint16_t bytes_per_sector;
                        } __attribute__ ((packed));

                        // memory where the drive info shall be stored
                        uintptr_t mem = guest_to_host(g->mem_low_va) +
                                        amd_vmcb_ds_base_rd(&g->vmcb) +
                                        guest_get_si(g);
                        struct drive_params *drp = (void *)mem;

                        // sanity check
                        if (drp->size < sizeof(struct drive_params)) {
                            amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                        } else {
                            amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                            guest_set_ah(g, 0);

                            drp->size = sizeof(struct drive_params);
                            // CHS invalid, no removable drive, etc
                            drp->flags = 0;
                            drp->cylinders = 0;
                            drp->heads = 0;
                            drp->sectors = 0;
                            drp->total_sectors = hdd_get_blocks_count(
                                                    g->hdds[dl & 0x7f]);
                            drp->bytes_per_sector = 512; // FIXME: Hardcoded
                        }
                    } else {
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                        // it is not really clear to me what ah should contain
                        // when the drive is not present, so set it to FF
                        guest_set_ah(g, 0x1);
                    }
                } else {
                    printf("Unhandeled method on INT 0x13\n");
                    return handle_vmexit_unhandeled(g);
                }
                break;
            case 0x15:
                // ENABLE A20 GATE
                if (guest_get_ax(g) == 0x2401) {
                    g->a20_gate_enabled = true;
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                    guest_set_ah(g, 0);
                }
                // APM INSTALLATION CHECK
                else if (guest_get_ax(g) == 0x5300) {
                    // we do not support APM - set carry flag to indicate error
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                }
                // APM DISCONNECT
                else if (guest_get_ax(g) == 0x5304) {
                    // we do not support APM - set carry flag to indicate error
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                }
                // GET MEMORY SIZE FOR >64M CONFIGURATIONS
                else if (guest_get_ax(g) == 0xe801) {
                    // we do not support this BIOS call
                    // both grub and linux may also use the 0xe820 call
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                }
                // GET SYSTEM MEMORY MAP
                // EDX has to contain 0x534d4150 (== 'SMAP')
                else if (guest_get_ax(g) == 0xe820 &&
                         guest_get_edx(g) == 0x534d4150) {
                    // for now we return only one entry containing the real mem
                    if (guest_get_ebx(g) > 1 || guest_get_ecx(g) < 20) {
                        // wrong input params -> report error
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                    } else {
                        // taken from http://www.ctyme.com/intr/rb-1741.htm
                        uintptr_t addr = guest_to_host(g->mem_low_va) +
                                         amd_vmcb_es_base_rd(&g->vmcb) +
                                         guest_get_di(g);

                        // set EAX to 'SMAP'
                        guest_set_eax(g, 0x534D4150);
                        // returned bytes (always 20)
                        guest_set_ecx(g, 20);

                        switch (guest_get_ebx(g)) {
                        case 0x0:
                            // base memory
                            assert(g->mem_low_va == 0);
                            // base address
                            *(uint64_t *)addr = 0;
                            // size of the memory block
                            *(uint64_t *)(addr + 8) = 0xa0000; // 640 KiB
                            // mem type, 1 == "memory, available to the OS"
                            *(uint32_t *)(addr + 16) = 1;
                            // indicate that there is more data
                            guest_set_ebx(g, 1);
                            break;
                        case 0x1:
                            // extended memory
                            assert(g->mem_high_va > 0x100000);
                            // base address
                            *(uint64_t *)addr = 0x100000;   // 1 MiB
                            // size of the memory block
                            *(uint64_t *)(addr + 8) = g->mem_high_va - 0x100000;
                            // mem type, 1 == "memory, available to the OS"
                            *(uint32_t *)(addr + 16) = 1;
                            // indicate that there is no more data
                            guest_set_ebx(g, 0);
                            break;
                        default:
                            assert(!"not reached");
                            break;
                        }

                        // mark success
                        amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                    }
                }
                // SYSTEM - Get Intel SpeedStep (IST) information
                else if (guest_get_ax(g) == 0xe980) {
                    // not supportet yet
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                }
                // SYSTEM - GET CONFIGURATION (XT >1986/1/10,AT mdl 3x9,
                // CONV,XT286,PS)
                // GRUB BUG: it puts 0xc0 into AX instead of AH
                else if (guest_get_ax(g) == 0xc0) {
                    // we do not support this
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                    guest_set_ah(g, 0x80);
                }
                // GET EXTENDED MEMORY SIZE
                else if (guest_get_ah(g) == 0x88) {
                    // calculate number of 1KB chunks starting from 1MB but not
                    // beyond 16MB
                    assert(((g->mem_high_va - g->mem_low_va) & 0x3ff) == 0);
                    guest_set_ax(g, MIN(0x3c00 /* 16MB */,
                                 (g->mem_high_va - g->mem_low_va) / 1024));
                    // indicate no error occured
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                }
                // SYSTEM - GET CONFIGURATION (XT >1986/1/10,AT mdl 3x9,
                // CONV,XT286,PS)
                else if (guest_get_ah(g) == 0xc0) {
                    // we do not support this
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 1);
                    guest_set_ah(g, 0x80);
                // SYSTEM - SET BIOS MODE
                } else if (guest_get_ah(g) == 0xec) {
                    // I do no really know the use of this bios call and linux
                    // expects no action what so ever
                } else {
                    printf("Unhandeled method on INT 0x15\n");
                    return handle_vmexit_unhandeled(g);
                }
                break;
            case 0x16:
                // KEYBOARD - SET TYPEMATIC RATE AND DELAY
                if (guest_get_ah(g) == 0x3) {
                    // ignore this
                } else {
                    printf("Unhandeled method on INT 0x16\n");
                    return handle_vmexit_unhandeled(g);
                }
                break;
            case 0x1a:
                // TIME - GET REAL-TIME CLOCK TIME (AT,XT286,PS)
                if (guest_get_ah(g) == 0x2) {
                    uint8_t h, m, s;
                    lpc_rtc_get_time_bcd(g->lpc, &h, &m, &s);
                    guest_set_ch(g, h);
                    guest_set_cl(g, m);
                    guest_set_dh(g, s);
                    guest_set_dl(g, 0);
                    // mark success
                    amd_vmcb_rflags_cf_wrf(&g->vmcb, 0);
                } else {
                    printf("Unhandeled method on INT 0x1a\n");
                    return handle_vmexit_unhandeled(g);
                }
                break;
            default:
                printf("handle_vmexit_swint: Unhandeled real-mode interrupt "
                       "0x%x (%d).\n", int_num, int_num);
                return handle_vmexit_unhandeled(g);
        }
    } else {
        printf("vmkitmon: encountered INT instruction outside real mode\n");
        return handle_vmexit_unhandeled(g);
    }

    // advance the rip beyond the instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 2);

    return HANDLER_ERR_OK;
}

static inline enum opsize
io_access_size_to_opsize (enum x86_io_access io)
{
    if (io & X86_IO_ACCESS_SZ8) {
        return OPSIZE_8;
    } else if (io & X86_IO_ACCESS_SZ16) {
        return OPSIZE_16;
    } else if (io & X86_IO_ACCESS_SZ32) {
        return OPSIZE_32;
    } else {
        assert(!"NYI");
        return 0;
    }
}

static int
handle_vmexit_ioio (struct guest *g)
{
    int r;
    uint64_t info1 = amd_vmcb_exitinfo1_rd(&g->vmcb);
    enum x86_io_access io;
    uint16_t port = info1 >> 16;
    bool write;
    enum opsize size;
    uint32_t val;
    bool newapi = false; // needed as a transition

    // copy the access flags
    // FIXME: this severely exploits the way the x86_io_access flags are set up
    io = (info1 >> 1);
    io |= info1 & SVM_IOIO_TYPE_MASK;

    // gather some params for the io access
    write = (io & X86_IO_ACCESS_TYPE) == 0;
    size = OPSIZE_8; // make gcc happy
    if (io & X86_IO_ACCESS_SZ8) {
        size = OPSIZE_8;
    } else if (io & X86_IO_ACCESS_SZ16) {
        size = OPSIZE_16;
    } else if (io & X86_IO_ACCESS_SZ32) {
        size = OPSIZE_32;
    }

    // fetch the source val if neccessary
    if (write) {
        switch (size) {
        case OPSIZE_8:
            val = guest_get_al(g);
            break;
        case OPSIZE_16:
            val = guest_get_ax(g);
            break;
        case OPSIZE_32:
            val = guest_get_eax(g);
            break;
        default:
            assert(!"not reached");
            break;
        }
    }

    // assign the request to the corresponding subsystem
    switch (port) {
        // LPC devices
        case 0x20:  // primary PIC
        case 0x21:  // primary PIC
        case 0x40:  // Timer
        case 0x41:  // Timer
        case 0x42:  // Timer
        case 0x43:  // Timer
        case 0x61:  // NMI Controller
        case 0x70:  // RTC
        case 0x71:  // RTC
        case 0x72:  // RTC
        case 0x73:  // RTC
        case 0x74:  // RTC
        case 0x75:  // RTC
        case 0x76:  // RTC
        case 0x77:  // RTC
        case 0xa0:  // secondary PIC
        case 0xa1:  // secondary PIC
            if (write) {
                r = lpc_handle_pio_write(g->lpc, port, size, val);
                guest_assert(g, r == 0);
            } else {
                r = lpc_handle_pio_read(g->lpc, port, size, &val);
                assert(r == 0);
            }
            newapi = true;
            break;
        // Keyboard
        case 0x60:
        case 0x64:
            // we currently do not support a keyboard
            if (!write) {
                val = ~0;
            }
            newapi = true;
            break;
        case 0x80:
            // some apps use writing to this port as a method to delay execution
            // so we just do noting
            break;
        // Coprocessor
        case 0xf0:
        case 0xf1:
            // coprocessor IGNNE# - do nothing for now
            break;

        // serial COM1 port
        // FIXME: this should not be hardcoded !
        case 0x3f8:
        case 0x3f9:
        case 0x3fa:
        case 0x3fb:
        case 0x3fc:
        case 0x3fd:
        case 0x3fe:
        case 0x3ff:
        // COM2
        case 0x2f8:
        case 0x2f9:
        case 0x2fa:
        case 0x2fb:
        case 0x2fc:
        case 0x2fd:
        case 0x2fe:
        case 0x2ff:
        // COM3
        case 0x3e8:
        case 0x3e9:
        case 0x3ea:
        case 0x3eb:
        case 0x3ec:
        case 0x3ed:
        case 0x3ee:
        case 0x3ef:
        // COM4
        case 0x2e8:
        case 0x2e9:
        case 0x2ea:
        case 0x2eb:
        case 0x2ec:
        case 0x2ed:
        case 0x2ee:
        case 0x2ef: {
            int com;

            com = (port & 0xf0) == 0xf0 ? !(port & 0x100) : !(port & 0x100) + 2;
            assert(com >= 0 && com < 4);
            if (write) {
                r = pc16550d_handle_pio_write(g->serial_ports[com], port,
                                              size, val);
                assert(r == 0);
            } else {
                r = pc16550d_handle_pio_read(g->serial_ports[com], port,
                                             size, &val);
                assert(r == 0);
            }
            newapi = true;
            break;
        }

            // PCI config space (address)
    case 0xcf8:
    case 0xcf9:
    case 0xcfa:
    case 0xcfb:
            // PCI config space (data)
    case 0xcfc:
    case 0xcfd:
    case 0xcfe:
    case 0xcff:
        if(write) {
            r = pci_handle_pio_write(g->pci, port, size, val);
        } else {
            r = pci_handle_pio_read(g->pci, port, size, &val);
        }
        assert(r == 0);
        newapi = true;
        break;

        default:
            // the default is to return 0xff and to ignore writes
            if (!write) {
                val = 0xffffffff;
            }
            newapi = true;
    };

    // set the destination when neccessary
    if (newapi && !write) {
        switch (size) {
        case OPSIZE_8:
            guest_set_al(g, val);
            break;
        case OPSIZE_16:
            guest_set_ax(g, val);
            break;
        case OPSIZE_32:
            guest_set_eax(g, val);
            break;
        default:
            assert(!"not reached");
            break;
        }
    }

    // the following IP is stored in the exitinfo2 field
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_exitinfo2_rd(&g->vmcb));

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_msr (struct guest *g) {
    bool write = amd_vmcb_exitinfo1_rd(&g->vmcb) == 1;
    uint32_t msr = guest_get_ecx(g);
    uint64_t val;

    // there may be writes or reads to MSRs
    if (write) {
        // fetch the value to write from EDX:EAX
        val = ((uint64_t)guest_get_edx(g) << 32) | guest_get_eax(g);

        // store the read value into the corresponding location
        switch (msr) {
        case X86_MSR_SYSENTER_CS:
            amd_vmcb_sysenter_cs_wr(&g->vmcb, val);
            break;
        case X86_MSR_SYSENTER_ESP:
            amd_vmcb_sysenter_esp_wr(&g->vmcb, val);
            break;
        case X86_MSR_SYSENTER_EIP:
            amd_vmcb_sysenter_eip_wr(&g->vmcb, val);
            break;
        case X86_MSR_EFER:
            amd_vmcb_efer_wr_raw(&g->vmcb, val);
            break;
        case X86_MSR_FS_BASE:
            amd_vmcb_fs_base_wr(&g->vmcb, val);
            break;
        case X86_MSR_GS_BASE:
            amd_vmcb_gs_base_wr(&g->vmcb, val);
            break;
        case X86_MSR_KERNEL_GS_BASE:
            amd_vmcb_kernel_gs_base_wr(&g->vmcb, val);
            break;
        case X86_MSR_STAR:
            amd_vmcb_star_wr(&g->vmcb, val);
            break;
        case X86_MSR_LSTAR:
            amd_vmcb_lstar_wr(&g->vmcb, val);
            break;
        case X86_MSR_CSTAR:
            amd_vmcb_cstar_wr(&g->vmcb, val);
            break;
        case X86_MSR_SFMASK:
            amd_vmcb_sfmask_wr(&g->vmcb, val);
            break;
        default:
            printf("MSR: unhandeled MSR write access to %x\n", msr);
            return handle_vmexit_unhandeled(g);
        }
    } else {
        // read the value from the corresponding location
        switch (msr) {
        case X86_MSR_SYSENTER_CS:
            val = amd_vmcb_sysenter_cs_rd(&g->vmcb);
            break;
        case X86_MSR_SYSENTER_ESP:
            val = amd_vmcb_sysenter_esp_rd(&g->vmcb);
            break;
        case X86_MSR_SYSENTER_EIP:
            val = amd_vmcb_sysenter_eip_rd(&g->vmcb);
            break;
        case X86_MSR_EFER:
            val = amd_vmcb_efer_rd_raw(&g->vmcb);
            break;
        case X86_MSR_FS_BASE:
            val = amd_vmcb_fs_base_rd(&g->vmcb);
            break;
        case X86_MSR_GS_BASE:
            val = amd_vmcb_gs_base_rd(&g->vmcb);
            break;
        case X86_MSR_KERNEL_GS_BASE:
            val = amd_vmcb_kernel_gs_base_rd(&g->vmcb);
            break;
        case X86_MSR_STAR:
            val = amd_vmcb_star_rd(&g->vmcb);
            break;
        case X86_MSR_LSTAR:
            val = amd_vmcb_lstar_rd(&g->vmcb);
            break;
        case X86_MSR_CSTAR:
            val = amd_vmcb_cstar_rd(&g->vmcb);
            break;
        case X86_MSR_SFMASK:
            val = amd_vmcb_sfmask_rd(&g->vmcb);
            break;
        default:
            printf("MSR: unhandeled MSR read access to %x\n", msr);
            return handle_vmexit_unhandeled(g);
        }

        // store the value in EDX:EAX
        guest_set_eax(g, val);
        guest_set_edx(g, val >> 32);
    }

    // advance the rip beyond the current instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 2);

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_cpuid (struct guest *g) {
    uint32_t eax, ebx, ecx, edx;
    uint32_t func = guest_get_eax(g);

    switch (func) {
    // Processor Vendor and Largest Standard Function Number
    case 0:
    case 0x80000000:
        // max standard function offset
        eax = func == 0 ? 0x1 : 0x80000000;
        // string "AuthenticAMD"
        ebx = 0x68747541;
        ecx = 0x444d4163;
        edx = 0x69746e65;
    break;

    // Family, Model, Stepping Identifiers
    case 1:
        // we simulate a AMD K6-3D
        // Family 5, Model 8, Stepping 12
        eax = 0x58c;
        // no brand, clflush size 16, no mulitprocessing, no local apic
        ebx = 0x0f00;
        // support the popcnt instr
        ecx = 0x800000;
        // support some basic features
        edx = 0x89a91b;
    break;

    default:
        // use the answer of the host if there is any other request
        // FIXME: this is probably not a good idea ;)
        cpuid(func, &eax, &ebx, &ecx, &edx);
        printf("handle_vmexit_cpuid: CPUID: func %x, host reports: eax %x, "
                "ebx %x, ecx %x, edx %x\n", func, eax, ebx, ecx, edx);
        break;
    }

    guest_set_eax(g, eax);
    guest_set_ebx(g, ebx);
    guest_set_ecx(g, ecx);
    guest_set_edx(g, edx);

    // advance the rip beyond the instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 2);

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_vmmcall (struct guest *g) {
    /*printf("VMMCALL: tsc %lu, exits with mon invocation %lu, exits w/o mon "
           "invocation %lu\n", rdtsc(),
           g->ctrl->num_vm_exits_with_monitor_invocation,
           g->ctrl->num_vm_exits_without_monitor_invocation);*/

    // advance the rip beyond the instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 3);

    return HANDLER_ERR_OK;
}

static int
handle_vmexit_hlt (struct guest *g) {
    // the guest has nothing to do - poll out irq sources for pending IRQs
    // if they do not assert a virtual IRQ then we will do nothing
    lpc_pic_process_irqs(g->lpc);

    // advance the rip beyond the instruction
    amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) + 1);

    // running HLT with IRQs masked does not make any sense
    // FIXME: this assert silly, shutting down the VM would be the right way
    guest_assert(g, amd_vmcb_rflags_rd(&g->vmcb).intrf == 1);
    if (virq_pending(g, NULL, NULL)) {
        // there is an IRQ pending, proceed as normal, the CPU will take it
    } else {
        // there is really nothing to do - stop the VM and wait
        g->runnable = false;
    }

    return HANDLER_ERR_OK;
}

static inline int
decode_mov_instr_length (struct guest *g, uint8_t *code)
{
    int len;

    // we only support long mode for now
    //assert(amd_vmcb_efer_rd(&g->vmcb).lma == 1);

    // all non special MOV instructions use one byte as opcode and at least a
    // ModR/M byte
    len = 2;
    // check for the REX prefix
    if ((code[0] >> 4) == 0x4) {
        len++;
        code++;
    }
    // precaution because I did no check all variants of MOV, at least these two
    // variants are supported
    assert(code[0] == 0x89 || code[0] == 0x8b);

    union x86_modrm modrm = { .raw = code[1] };
    // check for displacements
    if (modrm.u.mod == 0x1) {
        // 1B displacement
        len++;
    } else if (modrm.u.mod == 0x2) {
        // 4B displacement
        len += 4;
    }

    // check for SIB byte
    if (modrm.u.rm == 0x4 && modrm.u.mod != 0x3) {
        len++;
    }

    return len;
}

// finds out whether a move instruction is a read or a write with respect to
// memory
static inline bool
decode_mov_is_write (struct guest *g, uint8_t *code)
{
    // check for the REX prefix
    if ((code[0] >> 4) == 0x4) {
        code++;
    }

    // we only support one move variant (in each direction) for now
    assert(code[0] == 0x89 || code[0] == 0x8b);

    union x86_modrm modrm = { .raw = code[1] };
    // not defined for reg to reg moves
    assert(modrm.u.mod != 3);

    return code[0] == 0x89; // 0x89 ==> MOV reg -> mem
}

static inline enum opsize
decode_mov_op_size (struct guest *g, uint8_t *code)
{
    /*
	printf("EFER: 0x%lx\n", amd_vmcb_efer_rd_raw(&g->vmcb));
	printf("Code: 0x%lx\n", *((uint64_t *)code));
	printf("Code[0]: 0x%x, Code[1]: 0x%x, Code[2]: 0x%x, Code[3]: 0x%x\n", code[0],code[1],code[2],code[3]);
	printf("Guest EAX: 0x%x\n", guest_get_eax(g));
	printf("Guest EBX: 0x%x\n", guest_get_ebx(g));
	printf("Guest ECX: 0x%x\n", guest_get_ecx(g));

	printf("Guest EDX: 0x%x\n", guest_get_edx(g));
	printf("Guest RDI: 0x%lx\n", guest_get_rdi(g));
	printf("Guest RSI: 0x%lx\n", guest_get_rsi(g));
	printf("Guest RSP: 0x%lx\n", guest_get_rsp(g));
	printf("Guest RBP: 0x%lx\n", guest_get_rbp(g));
    */

    // we only support long mode for now
    //assert(amd_vmcb_efer_rd(&g->vmcb).lma == 1);

    // check for the REX prefix
    if ((code[0] >> 4) == 0x4 && code[0] & 0x48) {
        return OPSIZE_64;
    }
    return OPSIZE_32;
}


static inline uint64_t
decode_mov_src_val (struct guest *g, uint8_t *code) {
    
    // we only support long mode for now
    //assert(amd_vmcb_efer_rd(&g->vmcb).lma == 1);

    // check for the REX prefix
    if ((code[0] >> 4) == 0x4) {
        code++;
    }

    // we only support one variant for now
    assert(code[0] == 0x89);

    union x86_modrm modrm = { .raw = code[1] };
    return get_reg_val_by_reg_num(g, modrm.u.regop);
}


static inline void
decode_mov_dest_val (struct guest *g, uint8_t *code, uint64_t val)
{
    // we only support long mode for now
    //assert(amd_vmcb_efer_rd(&g->vmcb).lma == 1);

    // check for the REX prefix
    if ((code[0] >> 4) == 0x4) {
        code++;
    }

    // we only support one variant for now
    assert(code[0] == 0x8b);

    union x86_modrm modrm = { .raw = code[1] };
    set_reg_val_by_reg_num(g, modrm.u.regop, val);
}

/**** e1000
#define TDBAL_OFFSET 0x3800
#define TDBAH_OFFSET 0x3804
#define RDBAL_OFFSET 0x2800
#define RDBAH_OFFSET 0x2804
#define TDT_OFFSET 0x3818 //Transmit descriptor tail. Writes to this toggle transmission
#define TCTL_OFFSET 0x400 //Transmission Control

#define IMS_OFFSET 0xd0 // Interrupt Mask Set/Read Register
#define ICS_OFFSET 0xc8 // Interrupt Cause Set Register

static int register_needs_translation(uint64_t addr){
	return (
		addr == TDBAL_OFFSET ||
		addr == TDBAH_OFFSET ||
		addr == RDBAL_OFFSET ||
		addr == RDBAH_OFFSET
	);

}

**** e1000 */




#define MMIO_MASK(bytes) (~(~(bytes) + 1)) // I think ~(-bytes) is also correct

static int
handle_vmexit_npf (struct guest *g) {
    int r;
    uint64_t fault_addr = amd_vmcb_exitinfo2_rd(&g->vmcb);
    uint8_t *code = NULL;

    // check for fault inside the guest physical memory region
    if (fault_addr >= g->mem_low_va && fault_addr < g->mem_high_va) {
        // allocate the missing memory
        alloc_guest_mem(g, fault_addr & ~BASE_PAGE_MASK, BASE_PAGE_SIZE);
        // do not advance the RIP, it is safe (and neccessary) to
        // replay the faulting instruction
        return HANDLER_ERR_OK;
    }

    // fetch the location to the code
    r = get_instr_arr(g, &code);
    assert (r == 0);

    // virtual devices
    switch (fault_addr & ~BASE_PAGE_MASK) {
    case APIC_BASE: {
        uint64_t val;
        enum opsize size;

        assert(g->apic != NULL);
        size = decode_mov_op_size(g, code);
        if (decode_mov_is_write(g, code)) {
            val = decode_mov_src_val(g, code);
            r = apic_handle_mmio_write(g->apic, fault_addr, size, val);
            assert(r == 0);
        } else {
            r = apic_handle_mmio_read(g->apic, fault_addr, size, &val);
            assert(r == 0);
            decode_mov_dest_val(g, code, val);
        }

        // advance the rip beyond the instruction
        amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) +
                        decode_mov_instr_length(g, code));

        return HANDLER_ERR_OK;
    }
    }

    //Check if this is a access to a pci device memory

    for(int bus_i = 0; bus_i<256; bus_i++){
    	for(int dev_i = 0; dev_i < 32; dev_i++){
    		struct pci_bus *bus = g->pci->bus[bus_i];
			if(bus) {
				struct pci_device* dev = bus->device[dev_i];
				if(dev){
					for(int bar_i=0; bar_i<5; bar_i++){
						struct bar_info *curbar = &dev->bars[bar_i];
						if(curbar->paddr <= fault_addr && fault_addr < curbar->paddr + curbar->bytes){
							if(decode_mov_is_write(g, code)){
								uint64_t val = decode_mov_src_val(g, code);
								if(dev->mem_write) {
									dev->mem_write(dev, MMIO_MASK(curbar->bytes) & fault_addr, bar_i, val );
								} else {
									goto error;
								}
							} else {
								uint64_t val;
								if(dev->mem_read){
									dev->mem_read(dev, MMIO_MASK(curbar->bytes) & fault_addr, bar_i, (uint32_t*)&val);
									decode_mov_dest_val(g, code, val);
								} else {
									goto error;
								}
							}
							amd_vmcb_rip_wr(&g->vmcb, amd_vmcb_rip_rd(&g->vmcb) +
							                        decode_mov_instr_length(g, code));
							return HANDLER_ERR_OK;
						}
					}
				}
			}
    	}
    }

    error:
    printf("vmkitmon: access to an unknown memory location: %lx", fault_addr);
    return handle_vmexit_unhandeled(g);
}

typedef int (*vmexit_handler)(struct guest *g);

static vmexit_handler vmexit_handlers[0x8c] = {
    [SVM_VMEXIT_CR0_READ] = handle_vmexit_cr_access,
    [SVM_VMEXIT_CR0_WRITE] = handle_vmexit_cr_access,
    [SVM_VMEXIT_CR0_SEL_WRITE] = handle_vmexit_cr_access,
    [SVM_VMEXIT_SWINT] = handle_vmexit_swint,
    [SVM_VMEXIT_IDTR_WRITE] = handle_vmexit_ldt,
    [SVM_VMEXIT_GDTR_WRITE] = handle_vmexit_ldt,
    [SVM_VMEXIT_IOIO] = handle_vmexit_ioio,
    [SVM_VMEXIT_MSR] = handle_vmexit_msr,
    [SVM_VMEXIT_CPUID] = handle_vmexit_cpuid,
    [SVM_VMEXIT_VMMCALL] = handle_vmexit_vmmcall,
    [SVM_VMEXIT_HLT] = handle_vmexit_hlt
};

void
guest_handle_vmexit (struct guest *g) {
	//struct pci_ethernet * eth = (struct pci_ethernet * ) g->pci->bus[0]->device[2]->state;//
	//printf("guest_handle_vmexit\n");
    uint64_t exitcode = amd_vmcb_exitcode_rd(&g->vmcb);

    vmexit_handler handler;

    if (exitcode == SVM_VMEXIT_NPF) {
        handler = handle_vmexit_npf;
    } else if (LIKELY(vmexit_handlers[exitcode] != NULL)) {
        handler = vmexit_handlers[exitcode];
    } else {
        handle_vmexit_unhandeled(g);
        return;
    }

    int r = handler(g);
    if (LIKELY(r == HANDLER_ERR_OK)) {
        if (g->runnable) {
            guest_make_runnable(g, true);
        }
    }
}
