/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <elf/elf.h>
#include <barrelfish_kpi/paging_arch.h>
#include <target/arm/barrelfish_kpi/arm_core_data.h>

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

struct monitor_allocate_state {
    void          *vbase;
    genvaddr_t     elfbase;
};

static errval_t monitor_elfload_allocate(void *state, genvaddr_t base,
                                         size_t size, uint32_t flags,
                                         void **retbase)
{
    struct monitor_allocate_state *s = state;

    *retbase = (char *)s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

struct xcore_bind_handler {
    coreid_t                    coreid;
    enum cpu_type               cputype;
    struct monitor_binding      *binding;
};

#if defined(CONFIG_FLOUNDER_BACKEND_UMP_IPI)
static errval_t
setup_intermon_connection_ump_ipi(int local_hwid, int remote_hwid,
                                  struct intermon_ump_ipi_binding *ump_binding,
                                  void *buf)
{
    // Bootee's notify channel ID is always 1
    struct capref notify_cap;
    err = notification_create_cap(1, remote_hwid, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, local_hwid, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // init our end of the binding and channel
    err = intermon_ump_ipi_init(ump_binding, get_default_waitset(),
                                buf, MON_URPC_CHANNEL_LEN,
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);

    return err;
}
#endif

static errval_t
setup_intermon_connection(int local_hwid,
                          int remote_hwid,
                          struct intermon_binding **ret_binding,
                          struct frame_identity *urpc_frame_id)
{
    // compute size of frame needed and allocate it
    struct capref frame;
    size_t framesize;
    errval_t err;

    framesize = MON_URPC_CHANNEL_LEN * 2;
    err = frame_alloc(&frame, framesize, &framesize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    // Mark it remote
    bool has_descendants;
    err = monitor_cap_remote(frame, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame(&buf, framesize, frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    #if defined(CONFIG_FLOUNDER_BACKEND_UMP_IPI)
    struct intermon_ump_ipi_binding *ump_binding;
    #else
    struct intermon_ump_binding *ump_binding;
    #endif

    ump_binding = malloc(sizeof(*ump_binding));
    assert(ump_binding != NULL);

    #if defined(CONFIG_FLOUNDER_BACKEND_UMP_IPI)
    err = setup_intermon_connection_ump_ipi(local_hwid, remote_hwid,
                                            ump_binding, buf);
    #else
    err = intermon_ump_init(ump_binding, get_default_waitset(),
                            buf, MON_URPC_CHANNEL_LEN,
                            (char *)buf + MON_URPC_CHANNEL_LEN,
                            MON_URPC_CHANNEL_LEN);
    #endif

    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err_push(err, LIB_ERR_UMP_CHAN_BIND);
    }


    // Identify UMP frame for tracing
    struct frame_identity umpid;
    err = invoke_frame_identify(frame, &umpid);
    assert(err_is_ok(err));
    ump_binding->ump_state.chan.recvid = (uintptr_t)umpid.base;
    ump_binding->ump_state.chan.sendid =
        (uintptr_t)(umpid.base + MON_URPC_CHANNEL_LEN);

    /* Look up information on the urpc_frame cap */
    err = invoke_frame_identify(frame, urpc_frame_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_identify failed");
    }

    *ret_binding = &ump_binding->b;
    return err;
}

struct module_blob {
    size_t             size;
    lvaddr_t           vaddr;
    genpaddr_t         paddr;
    struct mem_region *mem_region;
};

static errval_t
module_blob_map(const char *name, struct module_blob *blob)
{
    errval_t err;
    struct mem_region *mem_region = multiboot_find_module(bi, name);
    if (mem_region == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    // it's cached already
    if (blob->vaddr != 0) {
        assert(mem_region == blob->mem_region);
        return SYS_ERR_OK;
    }

    blob->mem_region = mem_region;
    err = spawn_map_module(mem_region,
                           &blob->size,
                           &blob->vaddr,
                           &blob->paddr);

    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_MODULE);
    } else {
        return SYS_ERR_OK;
    }
}

static errval_t
cpu_memory_prepare(size_t *size,
                   struct capref *cap_ret, void **buf_ret,
                   struct frame_identity *frameid)
{
    errval_t err;
    struct capref cap;
    void *buf;

    /* Currently, the app kernel can only be loaded in the first 2GB
       of memory. Further, it must not overlap the integer
       boundaries, i.e. 0-1, 1-2 */

    // FIXME:
    // The code below does not make sure that the kernel is loaded in the first
    // 2G, but the disabled code does not work. So using a simple frame_alloc()
    // for now.
    #if 0
    uint64_t old_minbase, old_maxlimit;
    ram_get_affinity(&old_minbase, &old_maxlimit);
    for (uint64_t minbase = 0, maxlimit = (uint64_t)1 << 30;
         minbase < (uint64_t)2 << 30;
         minbase += (uint64_t)1 << 30, maxlimit += (uint64_t)1 << 30) {

        printf("minbase=%llu maxlimit=%llu cpu_memory=%zd\n",
                minbase, maxlimit, cpu_memory);
        ram_set_affinity(minbase, maxlimit);
        err = frame_alloc(&cpu_memory_cap, cpu_memory, &cpu_memory);
        if (err_is_ok(err)) {
            goto done;
        }
    }
    USER_PANIC("No memory in the first 2GB, cannot continue booting cores");
 done:
    ram_set_affinity(old_minbase, old_maxlimit);
    #else
     err = frame_alloc(&cap, *size, size);
     if (err_is_fail(err)) {
         USER_PANIC("Failed to allocate %zd memory\n", *size);
     }
    #endif

#ifdef __gem5__
    // XXX: We map the frame for the new kernel as uncacheable. Gem5 has a
    // problem when one core has cacheing on and writes to a location where an
    // other core reads from without caches enabled. On real hardware one could
    // clean/flush the cache, but Gem5 doesn't support cache maintenance
    // operations for ARM
    err = vspace_map_one_frame_attr(&buf, *size, cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
#else
    err = vspace_map_one_frame(&buf, *size, cap, NULL, NULL);
#endif
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Mark memory as remote
    bool has_descendants;
    err = monitor_cap_remote(cap, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(cap, frameid);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    *cap_ret = cap;
    *buf_ret = buf;
    return SYS_ERR_OK;
}

static errval_t
cpu_memory_cleanup(struct capref cap, void *buf)
{
    errval_t err;

    err = vspace_unmap(buf);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace unmap CPU driver memory failed");
    }

    // XXX: Should not delete the remote cap
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    return SYS_ERR_OK;
}

static errval_t
spawn_memory_prepare(size_t size, struct capref *cap_ret,
                     struct frame_identity *frameid)
{
    errval_t err;
    struct capref cap;

    err = frame_alloc(&cap, size, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    // Mark memory as remote
    bool has_descendants;
    err = monitor_cap_remote(cap, true, &has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(cap, frameid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_identify failed");
    }

    *cap_ret = cap;
    return SYS_ERR_OK;
}

static errval_t
spawn_memory_cleanup(struct capref cap)
{

    errval_t err;
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    return SYS_ERR_OK;
}

static errval_t
elf_load_and_relocate(lvaddr_t blob_start, size_t blob_size,
                      void *to, lvaddr_t reloc_dest,
                      uintptr_t *reloc_entry)
{
    genvaddr_t entry; // entry poing of the loaded elf image
    struct Elf32_Ehdr *head = (struct Elf32_Ehdr *)blob_start;
    struct Elf32_Shdr *symhead, *rel, *symtab;
    errval_t err;

    //state.vbase = (void *)ROUND_UP(to, ARM_L1_ALIGN);
    struct monitor_allocate_state state;
    state.vbase   = to;
    state.elfbase = elf_virtual_base(blob_start);

    err = elf_load(head->e_machine,
                   monitor_elfload_allocate,
                   &state,
                   blob_start, blob_size,
                   &entry);
    if (err_is_fail(err)) {
        return err;
    }

    // Relocate to new physical base address
    symhead = (struct Elf32_Shdr *)(blob_start + (uintptr_t)head->e_shoff);
    rel = elf32_find_section_header_type(symhead, head->e_shnum, SHT_REL);
    symtab = elf32_find_section_header_type(symhead, head->e_shnum, SHT_DYNSYM);
    assert(rel != NULL && symtab != NULL);

    elf32_relocate(reloc_dest, state.elfbase,
                   (struct Elf32_Rel *)(blob_start + rel->sh_offset),
                   rel->sh_size,
                   (struct Elf32_Sym *)(blob_start + symtab->sh_offset),
                   symtab->sh_size,
                   state.elfbase, state.vbase);

    *reloc_entry = entry - state.elfbase + reloc_dest;
    return SYS_ERR_OK;
}

errval_t spawn_xcore_monitor(coreid_t coreid, int hwid, enum cpu_type cpu_type,
                             const char *cmdline,
                             struct intermon_binding **ret_binding)
{
    const char *monitorname = NULL, *cpuname = NULL;
    uint32_t arch_page_size;
    errval_t err;

    arch_page_size = BASE_PAGE_SIZE;
    monitorname = "armv7/sbin/monitor";
#if defined(__gem5__)
    cpuname = "armv7/sbin/cpu_arm_gem5";
#elif defined(__pandaboard__)
    cpuname = "armv7/sbin/cpu_omap44xx";
#else
#error "unknown armv7 architecture"
#endif

    // Get my arch ID
    uintptr_t my_arch_id = 0;
    err = invoke_monitor_get_arch_id(&my_arch_id);
    if (err_is_fail(err)) {
        return err;
    }

    // setup an intermon connection for the new core
    struct frame_identity urpc_frame_id;
    err = setup_intermon_connection(my_arch_id, hwid, ret_binding, &urpc_frame_id);
    if (err_is_fail(err)) {
        return err;
    }

    // map cpu and monitor module
    // XXX: caching these for now, until we have unmap
    static struct module_blob cpu_blob, monitor_blob;
    err = module_blob_map(cpuname, &cpu_blob);
    if (!err_is_ok(err)) {
        return err;
    }
    err = module_blob_map(monitorname, &monitor_blob);
    if (!err_is_ok(err)) {
        return err;
    }

    // allocate memory for cpu driver: we allocate a page for arm_core_data and
    // the reset for the elf image
    assert(sizeof(struct arm_core_data) <= arch_page_size);
    struct {
        size_t                size;
        struct capref         cap;
        void                  *buf;
        struct frame_identity frameid;
    } cpu_mem = {
        .size = arch_page_size + elf_virtual_size(cpu_blob.vaddr)
    };
    err = cpu_memory_prepare(&cpu_mem.size,
                             &cpu_mem.cap,
                             &cpu_mem.buf,
                             &cpu_mem.frameid);
    if (!err_is_ok(err)) {
        return err;
    }

    // Load cpu driver to the allocate space and do relocatation
    uintptr_t reloc_entry;
    err = elf_load_and_relocate(cpu_blob.vaddr,
                                cpu_blob.size,
                                cpu_mem.buf + arch_page_size,
                                cpu_mem.frameid.base + arch_page_size,
                                &reloc_entry);
    if (!err_is_ok(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    /* Chunk of memory to load monitor on the app core */
    struct capref spawn_mem_cap;
    struct frame_identity spawn_mem_frameid;
    err = spawn_memory_prepare(ARM_CORE_DATA_PAGES*arch_page_size,
                               &spawn_mem_cap,
                               &spawn_mem_frameid);
    if (!err_is_ok(err)) {
        return err;
    }

    /* Setup the core_data struct in the new kernel */
    struct arm_core_data *core_data = (struct arm_core_data *)cpu_mem.buf;

    struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_blob.vaddr;
    core_data->elf.size = sizeof(struct Elf32_Shdr);
    core_data->elf.addr = cpu_blob.paddr + (uintptr_t)head32->e_shoff;
    core_data->elf.num  = head32->e_shnum;

    core_data->module_start        = cpu_blob.paddr;
    core_data->module_end          = cpu_blob.paddr + cpu_blob.size;
    core_data->urpc_frame_base     = urpc_frame_id.base;
    core_data->urpc_frame_bits     = urpc_frame_id.bits;
    core_data->monitor_binary      = monitor_blob.paddr;
    core_data->monitor_binary_size = monitor_blob.size;
    core_data->memory_base_start   = spawn_mem_frameid.base;
    core_data->memory_bits         = spawn_mem_frameid.bits;
    core_data->src_core_id         = my_core_id;
    core_data->src_arch_id         = my_arch_id;
    core_data->dst_core_id         = coreid;
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    core_data->chan_id             = chanid;
#endif

    if (cmdline != NULL) {
        // copy as much of command line as will fit
        strncpy(core_data->kernel_cmdline, cmdline,
                sizeof(core_data->kernel_cmdline));
        // ensure termination
        core_data->kernel_cmdline[sizeof(core_data->kernel_cmdline) - 1] = '\0';
    }

    /* Invoke kernel capability to boot new core */
    // XXX: Confusion address translation about l/gen/addr
    err = invoke_monitor_spawn_core(hwid, cpu_type, (forvaddr_t)reloc_entry);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

    err = cpu_memory_cleanup(cpu_mem.cap, cpu_mem.buf);
    if (err_is_fail(err)) {
        return err;
    }

    err = spawn_memory_cleanup(spawn_mem_cap);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Initialize monitor running on app cores
 */
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding)

{
    errval_t err;

    assert(argc == 4);

    // core_id of the core that booted this core
    coreid_t core_id = strtol(argv[1], NULL, 10);
    *ret_parent_coreid = core_id;

#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    // other monitor's channel id
    assert(strncmp("chanid", argv[2], strlen("chanid")) == 0);
    int chan_id = strtol(strchr(argv[2], '=') + 1, NULL, 10);

    // arch id of the core that booted us
    assert(strncmp("archid", argv[3], strlen("archid")) == 0);
    int arch_id = strtol(strchr(argv[3], '=') + 1, NULL, 10);
#endif

    // check that the frame is big enough
    struct capref frame = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_MON_URPC,
    };
    struct frame_identity frameid;
    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_FRAME_IDENTIFY);
        return err;
    }

    size_t framesize = ((uintptr_t)1) << frameid.bits;
    if (framesize < 2 * MON_URPC_CHANNEL_LEN) {
        return LIB_ERR_UMP_FRAME_OVERFLOW;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame(&buf, framesize, frame, NULL, NULL);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        return err;
    }

#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    // Create notify cap to other monitor
    struct capref notify_cap;
    err = notification_create_cap(chan_id, arch_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    assert(chanid == 1);        // Make sure it's channel 1
    uintptr_t my_arch_id;
    err = invoke_monitor_get_arch_id(&my_arch_id);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_arch_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // setup our side of the binding
    struct intermon_ump_ipi_binding *umpb =
        malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(umpb != NULL);

    err = intermon_ump_ipi_init(umpb, get_default_waitset(),
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN,
                                buf, MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
#else
    struct intermon_ump_binding *umpb;
    umpb = malloc(sizeof(struct intermon_ump_binding));
    assert(umpb != NULL);

    err = intermon_ump_init(umpb, get_default_waitset(),
                            (char *)buf + MON_URPC_CHANNEL_LEN,
                            MON_URPC_CHANNEL_LEN,
                            buf, MON_URPC_CHANNEL_LEN);
#endif
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_UMP_CHAN_BIND);
        return err;
    }

    // Identify UMP frame for tracing
    umpb->ump_state.chan.sendid = (uintptr_t)frameid.base;
    umpb->ump_state.chan.recvid =
        (uintptr_t)(frameid.base + MON_URPC_CHANNEL_LEN);

    *ret_binding = &umpb->b;

    return SYS_ERR_OK;
}

