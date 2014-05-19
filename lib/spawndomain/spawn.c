/**
 * \file
 * \brief functionality to spawn domains
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish_kpi/domain_params.h>
#include <trace/trace.h>
#include "spawn.h"
#include "arch.h"
#include <elf/elf.h>

extern char **environ;

/**
 * \brief Setup an initial cspace
 *
 * Create an initial cspace layout
 */
static errval_t spawn_setup_cspace(struct spawninfo *si)
{
    errval_t err;
    struct capref t1;

    /* Create root CNode */
    err = cnode_create(&si->rootcn_cap, &si->rootcn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_ROOTCN);
    }

    /* Create taskcn */
    err = cnode_create(&si->taskcn_cap, &si->taskcn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_TASKCN);
    }

    // Mint into rootcn setting the guard
    t1.cnode = si->rootcn;
    t1.slot  = ROOTCN_SLOT_TASKCN;
    err = cap_mint(t1, si->taskcn_cap, 0,
                   GUARD_REMAINDER(2 * DEFAULT_CNODE_BITS));
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MINT_TASKCN);
    }

    /* Create slot_alloc_cnode */
    t1.cnode = si->rootcn;
    t1.slot  = ROOTCN_SLOT_SLOT_ALLOC0;
    err = cnode_create_raw(t1, NULL, (1<<SLOT_ALLOC_CNODE_BITS), NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_SLOTALLOC_CNODE);
    }
    t1.cnode = si->rootcn;
    t1.slot  = ROOTCN_SLOT_SLOT_ALLOC1;
    err = cnode_create_raw(t1, NULL, (1<<SLOT_ALLOC_CNODE_BITS), NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_SLOTALLOC_CNODE);
    }
    t1.cnode = si->rootcn;
    t1.slot  = ROOTCN_SLOT_SLOT_ALLOC2;
    err = cnode_create_raw(t1, NULL, (1<<SLOT_ALLOC_CNODE_BITS), NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_SLOTALLOC_CNODE);
    }

    // Create DCB
    si->dcb.cnode = si->taskcn;
    si->dcb.slot  = TASKCN_SLOT_DISPATCHER;
    err = dispatcher_create(si->dcb);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_DISPATCHER);
    }

    // Give domain endpoint to itself (in taskcn)
    struct capref selfep = {
        .cnode = si->taskcn,
        .slot = TASKCN_SLOT_SELFEP,
    };
    err = cap_retype(selfep, si->dcb, ObjType_EndPoint, 0);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_SELFEP);
    }

    // Map root CNode (in taskcn)
    t1.cnode = si->taskcn;
    t1.slot  = TASKCN_SLOT_ROOTCN;
    err = cap_mint(t1, si->rootcn_cap, 0, 0);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MINT_ROOTCN);
    }

#ifdef TRACING_EXISTS
    // Set up tracing for the child
    err = trace_setup_child(si->taskcn, si->handle);
    if (err_is_fail(err)) {
        printf("Warning: error setting up tracing for child domain\n");
        // SYS_DEBUG(err, ...);
    }
#endif

    // XXX: copy over argspg?
    memset(&si->argspg, 0, sizeof(si->argspg));

    /* Fill up basecn */
    struct capref   basecn_cap;
    struct cnoderef basecn;

    // Create basecn in rootcn
    basecn_cap.cnode = si->rootcn;
    basecn_cap.slot  = ROOTCN_SLOT_BASE_PAGE_CN;
    err = cnode_create_raw(basecn_cap, &basecn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }

    // Place the ram caps
    for (uint8_t i = 0; i < DEFAULT_CNODE_SLOTS; i++) {
        struct capref base = {
            .cnode = basecn,
            .slot  = i
        };
        struct capref ram;
        err = ram_alloc(&ram, BASE_PAGE_BITS);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_RAM_ALLOC);
        }
        err = cap_copy(base, ram);

        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CAP_COPY);
        }
        err = cap_destroy(ram);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CAP_DESTROY);
        }
    }

    return SYS_ERR_OK;
}

static errval_t spawn_setup_vspace(struct spawninfo *si)
{
    errval_t err;

    /* Create pagecn */
    si->pagecn_cap = (struct capref){.cnode = si->rootcn, .slot = ROOTCN_SLOT_PAGECN};
    err = cnode_create_raw(si->pagecn_cap, &si->pagecn, PAGE_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_PAGECN);
    }

    /* Init pagecn's slot allocator */

    // XXX: satisfy a peculiarity of the single_slot_alloc_init_raw API
    size_t bufsize = SINGLE_SLOT_ALLOC_BUFLEN(PAGE_CNODE_SLOTS);
    void *buf = malloc(bufsize);
    assert(buf != NULL);

    err = single_slot_alloc_init_raw(&si->pagecn_slot_alloc, si->pagecn_cap,
                                     si->pagecn, PAGE_CNODE_SLOTS,
                                     buf, bufsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Create root of pagetable
    err = si->pagecn_slot_alloc.a.alloc(&si->pagecn_slot_alloc.a, &si->vtree);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    // top-level table should always live in slot 0 of pagecn
    assert(si->vtree.slot == 0);

    switch(si->cpu_type) {
    case CPU_X86_64:
        err = vnode_create(si->vtree, ObjType_VNode_x86_64_pml4);
        break;

    case CPU_X86_32:
    case CPU_SCC:
#ifdef CONFIG_PAE
        err = vnode_create(si->vtree, ObjType_VNode_x86_32_pdpt);
#else
        err = vnode_create(si->vtree, ObjType_VNode_x86_32_pdir);
#endif
        break;

    case CPU_ARM5:
    case CPU_ARM7:
        err = vnode_create(si->vtree, ObjType_VNode_ARM_l1);
        break;

    default:
        assert(!"Other architecture");
        return err_push(err, SPAWN_ERR_UNKNOWN_TARGET_ARCH);
    }

    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_VNODE);
    }

    err = spawn_vspace_init(si, si->vtree, si->cpu_type);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_VSPACE_INIT);
    }

    return SYS_ERR_OK;
}

#if 0
/**
 * \brief Lookup and map an image
 */
static errval_t spawn_map(const char *name, struct bootinfo *bi,
                          lvaddr_t *binary, size_t *binary_size)
{
    errval_t err;

    /* Get the module from the multiboot */
    struct mem_region *module = multiboot_find_module(bi, name);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    /* Map the image */
    err = spawn_map_module(module, binary_size, binary, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_MODULE);
    }

    return SYS_ERR_OK;
}
#endif // 0


/**
 * \brief Determine cpu type of the image
 */
static errval_t spawn_determine_cputype(struct spawninfo *si, lvaddr_t binary)
{
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *)binary;

    switch(head->e_machine) {
    case EM_X86_64:
        si->cpu_type = CPU_X86_64;
        break;

    case EM_386:
        si->cpu_type = CPU_X86_32;
        break;

    case EM_ARM:
        si->cpu_type = CPU_ARM7;
        break;

    default:
        assert(!"Unsupported architecture type");
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Setup the dispatcher frame
 */
static errval_t spawn_setup_dispatcher(struct spawninfo *si,
                                       coreid_t core_id,
                                       const char *name,
                                       genvaddr_t entry,
                                       void* arch_info)
{
    errval_t err;

    /* Create dispatcher frame (in taskcn) */
    si->dispframe.cnode = si->taskcn;
    si->dispframe.slot  = TASKCN_SLOT_DISPFRAME;
    struct capref spawn_dispframe = {
        .cnode = si->taskcn,
        .slot  = TASKCN_SLOT_DISPFRAME2,
    };
    err = frame_create(si->dispframe, (1 << DISPATCHER_FRAME_BITS), NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_DISPATCHER_FRAME);
    }
    err = cap_copy(spawn_dispframe, si->dispframe);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_DISPATCHER_FRAME);
    }

    /* Map in dispatcher frame */
    dispatcher_handle_t handle;
    err = vspace_map_one_frame((void**)&handle, 1ul << DISPATCHER_FRAME_BITS,
                               si->dispframe, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_DISPATCHER_TO_SELF);
    }
    genvaddr_t spawn_dispatcher_base;
    err = spawn_vspace_map_one_frame(si, &spawn_dispatcher_base, spawn_dispframe,
                                     1UL << DISPATCHER_FRAME_BITS);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_DISPATCHER_TO_NEW);
    }

    /* Set initial state */
    // XXX: Confusion address translation about l/gen/addr in entry
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *disp_gen = get_dispatcher_generic(handle);
    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(handle);
    arch_registers_state_t *disabled_area =
        dispatcher_get_disabled_save_area(handle);

    /* Place core_id */
    disp_gen->core_id = core_id;

    /* Setup dispatcher and make it runnable */
    disp->udisp = spawn_dispatcher_base;
    disp->disabled = 1;
    disp->fpu_trap = 1;

    // Copy the name for debugging
    const char *copy_name = strrchr(name, '/');
    if (copy_name == NULL) {
        copy_name = name;
    } else {
        copy_name++;
    }
    strncpy(disp->name, copy_name, DISP_NAME_LEN);

    spawn_arch_set_registers(arch_info, handle, enabled_area, disabled_area);
    registers_set_entry(disabled_area, entry);

    si->handle = handle;
    return SYS_ERR_OK;
}

errval_t spawn_map_bootinfo(struct spawninfo *si, genvaddr_t *retvaddr)
{
    errval_t err;

    struct capref src = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_BOOTINFO
    };
    struct capref dest = {
        .cnode = si->taskcn,
        .slot  = TASKCN_SLOT_BOOTINFO
    };
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_COPY);
    }

    err = spawn_vspace_map_one_frame(si, retvaddr, dest, BOOTINFO_SIZE);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_BOOTINFO);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Retrive the commandline args of #name
 *
 * The arguments are malloced into a new space so need to be freed after use
 */
errval_t spawn_get_cmdline_args(struct mem_region *module,
                                char **retargs)
{
    assert(module != NULL && retargs != NULL);

    /* Get the cmdline args */
    const char *args = getopt_module(module);

    /* Allocate space */
    *retargs = malloc(sizeof(char) * strlen(args));
    if (!retargs) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /* Copy args */
    strcpy(*retargs, args);
    return SYS_ERR_OK;
}

/**
 * \brief Returns tokenized cmdline args
 *
 * \param s Argument string, which is modified in place
 * \param argv Array to be filled-in with arguments
 * \param argv_len Length of array available in argv, including terminator
 *
 * The arguments are placed in #argv, which is NULL-terminated
 *
 * \returns Number of arguments, not including terminator
 *
 * \bug Very limited handling of quoting etc.
 */
int spawn_tokenize_cmdargs(char *s, char *argv[], size_t argv_len)
{
    bool inquote = false;
    int argc = 0;
    assert(argv_len > 1);
    assert(s != NULL);

    // consume leading whitespace, and mark first argument
    while (*s == ' ' || *s == '\t') s++;
    if (*s != '\0') {
        argv[argc++] = s;
    }

    while (argc + 1 < argv_len && *s != '\0') {
        if (*s == '"') {
            inquote = !inquote;
            // consume quote mark, by moving remainder of string over it
            memmove(s, s + 1, strlen(s));
        } else if ((*s == ' ' || *s == '\t') && !inquote) { // First whitespace, arg finished
            *s++ = '\0';
            while (*s == ' ' || *s == '\t') s++; // Consume trailing whitespace
            if (*s != '\0') { // New arg started
                argv[argc++] = s;
            }
        } else {
            s++;
        }
    }

    argv[argc] = NULL;
    return argc;
}

/**
 * \brief Setup arguments and environment
 *
 * \param argv   Command-line arguments, NULL-terminated
 * \param envp   Environment, NULL-terminated
 */
static errval_t spawn_setup_env(struct spawninfo *si,
                                char *const argv[], char *const envp[])
{
    errval_t err;

    // Create frame (actually multiple pages) for arguments
    si->argspg.cnode = si->taskcn;
    si->argspg.slot  = TASKCN_SLOT_ARGSPAGE;
    struct capref spawn_argspg = {
        .cnode = si->taskcn,
        .slot  = TASKCN_SLOT_ARGSPAGE2,
    };
    err = frame_create(si->argspg, ARGS_SIZE, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_ARGSPG);
    }
    err = cap_copy(spawn_argspg, si->argspg);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_ARGSPG);
    }

    /* Map in args frame */
    genvaddr_t spawn_args_base;
    err = spawn_vspace_map_one_frame(si, &spawn_args_base, spawn_argspg, ARGS_SIZE);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_ARGSPG_TO_NEW);
    }

    void *argspg;
    err = vspace_map_one_frame(&argspg, ARGS_SIZE, si->argspg, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_ARGSPG_TO_SELF);
    }

    /* Layout of arguments page:
     *   struct spawn_domain_params; // contains pointers to other fields
     *   char buf[]; // NUL-terminated strings for arguments and environment
     *   vspace layout data follows the string data
     */
    struct spawn_domain_params *params = argspg;
    char *buf = (char *)(params + 1);
    size_t buflen = ARGS_SIZE - (buf - (char *)argspg);

    /* Copy command-line arguments */
    int i;
    size_t len;
    for (i = 0; argv[i] != NULL; i++) {
        len = strlen(argv[i]) + 1;
        if (len > buflen) {
            return SPAWN_ERR_ARGSPG_OVERFLOW;
        }
        strcpy(buf, argv[i]);
        params->argv[i] = buf - (char *)argspg + (char *)(lvaddr_t)spawn_args_base;
        buf += len;
        buflen -= len;
    }
    assert(i <= MAX_CMDLINE_ARGS);
    int argc = i;
    params->argv[i] = NULL;

    /* Copy environment strings */
    for (i = 0; envp[i] != NULL; i++) {
        len = strlen(envp[i]) + 1;
        if (len > buflen) {
            return SPAWN_ERR_ARGSPG_OVERFLOW;
        }
        strcpy(buf, envp[i]);
        params->envp[i] = buf - (char *)argspg + (char *)(lvaddr_t)spawn_args_base;
        buf += len;
        buflen -= len;
    }

    assert(i <= MAX_ENVIRON_VARS);
    params->envp[i] = NULL;

    /* Serialise vspace data */
    // XXX: align buf to next word
    char *vspace_buf = (char *)ROUND_UP((lvaddr_t)buf, sizeof(uintptr_t));
    buflen -= vspace_buf - buf;

    // FIXME: currently just the pmap is serialised
    err = si->vspace->pmap->f.serialise(si->vspace->pmap, vspace_buf, buflen);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SERIALISE_VSPACE);
    }

    /* Setup environment pointer and vspace pointer */
    params->argc = argc;
    params->vspace_buf = (char *)vspace_buf - (char *)argspg
                    + (char *)(lvaddr_t)spawn_args_base;
    params->vspace_buf_len = buflen;

    // Setup TLS data
    params->tls_init_base = (void *)vspace_genvaddr_to_lvaddr(si->tls_init_base);
    params->tls_init_len = si->tls_init_len;
    params->tls_total_len = si->tls_total_len;

    arch_registers_state_t *enabled_area =
        dispatcher_get_enabled_save_area(si->handle);
    registers_set_param(enabled_area, (uintptr_t)spawn_args_base);

    return SYS_ERR_OK;
}

static errval_t spawn_setup_fdcap(struct spawninfo *si,
                                  struct cnoderef inheritcn)
{
    errval_t err;

    struct capref src;
    src.cnode = inheritcn;
    src.slot  = INHERITCN_SLOT_FDSPAGE; 

    // Create frame (actually multiple pages) for fds
    struct capref dest;
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_FDSPAGE;

    err = cap_copy(dest, src);
    if (err_no(err) == SYS_ERR_SOURCE_CAP_LOOKUP) {
        // there was no fdcap to inherit, continue
        return SYS_ERR_OK;
    } else if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_COPY_FDCAP);
    }

    return SYS_ERR_OK;
}

static errval_t spawn_setup_sidcap(struct spawninfo *si,
                                   struct cnoderef inheritcn)
{
    errval_t err;

    struct capref src;
    src.cnode = inheritcn;
    src.slot  = INHERITCN_SLOT_SESSIONID;

    struct capref dest;
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_SESSIONID;

    err = cap_copy(dest, src);
    if (err_no(err) == SYS_ERR_SOURCE_CAP_LOOKUP) {
        // there was no sidcap to inherit, continue
        return SYS_ERR_OK;
    } else if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_COPY_SIDCAP);
    }

    return SYS_ERR_OK;
}

static errval_t spawn_setup_inherited_caps(struct spawninfo *si,
                                           struct capref inheritcn_cap)
{
    errval_t err;
    struct cnoderef inheritcn;

    if (capref_is_null(inheritcn_cap)) {
        return SYS_ERR_OK;
    }

    err = cnode_build_cnoderef(&inheritcn, inheritcn_cap);
    if (err_is_fail(err)) {
        return err;
    }

    /* Copy the file descriptor frame cap over */
    err = spawn_setup_fdcap(si, inheritcn);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_FDCAP);
    }

    /* Copy the session capability over */
    err = spawn_setup_sidcap(si, inheritcn);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_SIDCAP);
    }

    return SYS_ERR_OK;
}

static errval_t spawn_setup_argcn(struct spawninfo *si,
                                  struct capref argumentcn_cap)
{
    errval_t err;

    if (capref_is_null(argumentcn_cap)) {
        return SYS_ERR_OK;
    }

    struct capref dest = {
        .cnode = si->rootcn,
        .slot  = ROOTCN_SLOT_ARGCN
    };

    err = cap_copy(dest, argumentcn_cap);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_COPY_ARGCN);
    }

    return SYS_ERR_OK;
}


/**
 * \brief Load an image
 *
 * \param si            Struct used by the library
 * \param binary        The image to load
 * \param type          The type of arch to load for
 * \param name          Name of the image required only to place it in disp
 *                      struct
 * \param coreid        Coreid to load for, required only to place it in disp
 *                      struct
 * \param argv          Command-line arguments, NULL-terminated
 * \param envp          Environment, NULL-terminated
 * \param inheritcn_cap Cap to a CNode containing capabilities to be inherited
 * \param argcn_cap     Cap to a CNode containing capabilities passed as
 *                      arguments
 */
errval_t spawn_load_image(struct spawninfo *si, lvaddr_t binary,
                          size_t binary_size, enum cpu_type type,
                          const char *name, coreid_t coreid,
                          char *const argv[], char *const envp[],
                          struct capref inheritcn_cap, struct capref argcn_cap)
{
    errval_t err;

    si->cpu_type = type;

    /* Initialize cspace */
    err = spawn_setup_cspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_CSPACE);
    }

    /* Initialize vspace */
    err = spawn_setup_vspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_VSPACE_INIT);
    }

    genvaddr_t entry;
    void* arch_info;
    /* Load the image */
    err = spawn_arch_load(si, binary, binary_size, &entry, &arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    /* Setup dispatcher frame */
    err = spawn_setup_dispatcher(si, coreid, name, entry, arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_DISPATCHER);
    }

    /* Setup inherited caps */
    err = spawn_setup_inherited_caps(si, inheritcn_cap);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_INHERITED_CAPS);
    }

    /* Setup argument caps */
    err = spawn_setup_argcn(si, argcn_cap);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_ARGCN);
    }
 
    // Add vspace-pspace mapping to environment
    char envstr[2048];
    snprintf(envstr, 2048, "ARRAKIS_PMAP=");
    for(int i = 0; i < si->vregions; i++) {
        struct memobj_anon *m = (struct memobj_anon *)si->vregion[i]->memobj;
        assert(m->m.type == ANONYMOUS);
        for(struct memobj_frame_list *f = m->frame_list; f != NULL; f = f->next) {
            struct frame_identity id;
            err = invoke_frame_identify(f->frame, &id);
            assert(err_is_ok(err));

            char str[128];
            snprintf(str, 128, "%" PRIxGENVADDR ":%" PRIxGENPADDR ":%zx ", si->base[i] + f->offset, id.base, f->size);
            strcat(envstr, str);
        }
    }

    char **myenv = (char **)envp;
    for(int i = 0; i < MAX_ENVIRON_VARS; i++) {
        if(i + 1 == MAX_ENVIRON_VARS) {
            printf("spawnd: Couldn't set environemnt. Out of variables!\n");
            abort();
        }

        if(myenv[i] == NULL) {
            myenv[i] = envstr;
            myenv[i+1] = NULL;
            break;
        }
    }

    /* Setup cmdline args */
    err = spawn_setup_env(si, argv, envp);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_ENV);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Spawn a domain with the given args
 */
errval_t spawn_load_with_args(struct spawninfo *si, struct mem_region *module,
                              const char *name, coreid_t coreid,
                              char *const argv[], char *const envp[])
{
    errval_t err;

    /* Lookup and map the elf image */
    lvaddr_t binary;
    size_t binary_size;
    err = spawn_map_module(module, &binary_size, &binary, NULL);
    //err = spawn_map(name, bi, &binary, &binary_size);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_ELF_MAP);
    }

    /* Determine cpu type */
    err = spawn_determine_cputype(si, binary);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_DETERMINE_CPUTYPE);
    }

    /* Initialize cspace */
    err = spawn_setup_cspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_CSPACE);
    }

    /* Initialize vspace */
    err = spawn_setup_vspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_VSPACE_INIT);
    }

    /* Load the image */
    genvaddr_t entry;
    void* arch_info;

    err = spawn_arch_load(si, binary, binary_size, &entry, &arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    /* Setup dispatcher frame */
    err = spawn_setup_dispatcher(si, coreid, name, entry, arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_DISPATCHER);
    }

    /* Setup cmdline args */
    err = spawn_setup_env(si, argv, envp);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_ENV);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Spawn a domain and give it the bootinfo struct.
 * Just monitor and memserv should be spawned using this.
 */
errval_t spawn_load_with_bootinfo(struct spawninfo *si, struct bootinfo *bi,
                                  const char *name, coreid_t coreid)
{
    errval_t err;

    /* Get the module from the multiboot */
    struct mem_region *module = multiboot_find_module(bi, name);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    /* Lookup and map the elf image */
    lvaddr_t binary;
    size_t binary_size;
    err = spawn_map_module(module, &binary_size, &binary, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_ELF_MAP);
    }

    /* Determine cpu type */
    err = spawn_determine_cputype(si, binary);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_DETERMINE_CPUTYPE);
    }

    /* Initialize cspace */
    err = spawn_setup_cspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_CSPACE);
    }

    /* Initialize vspace */
    err = spawn_setup_vspace(si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_VSPACE_INIT);
    }

    /* Load the image */
    genvaddr_t entry;
    void* arch_info;
    err = spawn_arch_load(si, binary, binary_size, &entry, &arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    /* Setup dispatcher frame */
    err = spawn_setup_dispatcher(si, coreid, name, entry, arch_info);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_DISPATCHER);
    }

    /* Map bootinfo */
    // XXX: Confusion address translation about l/gen/addr in entry
    genvaddr_t vaddr;
    err = spawn_map_bootinfo(si, &vaddr);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_BOOTINFO);
    }

    /* Construct cmdline args, 0 is name, 1 is bootinfo address,
       remaining are from the multiboot */
    // Name
    char args[1024];
    strcpy(args, name);
    strcat(args, " ");

    // bootinfo addr
    char vaddr_char[32];

    // NB format here should be PRIuGENVADDR, but our ARM compiler has
    // an out-by-4 bytes issue when rendering 64-bit numbers using
    // __builtin_va_start/__builtin_va_arg.
    // [ gcc version 4.4.1 (Sourcery G++ Lite 2009q3-67) ]
    snprintf(vaddr_char, sizeof(vaddr_char), "%" PRIuPTR, (uintptr_t)vaddr);

    strcat(args, vaddr_char);
    strcat(args, " ");

#ifdef __scc__
    if(si->codeword == 0xcafebabe) {
        strcat(args, si->append_args);
        strcat(args, " ");
    }

    if(!strcmp(name, "scc/sbin/monitor")) {
        printf("starting monitor as '%s'\n", args);
    }
#endif

    // Multiboot args
    char *multiboot_args;
    err = spawn_get_cmdline_args(module, &multiboot_args);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_GET_CMDLINE_ARGS);
    }
    // Lop off the name
    char *multiboot_args_lop = strchr(multiboot_args, ' ');
    if (multiboot_args_lop) {
        multiboot_args_lop++;
        strcat(args, multiboot_args_lop);
    }

    // Tokenize
    char *argv[MAX_CMDLINE_ARGS + 1];
    spawn_tokenize_cmdargs(args, argv, ARRAY_LENGTH(argv));

    // Setup
    err = spawn_setup_env(si, argv, environ);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SETUP_ENV);
    }
    free(multiboot_args);

    // unmap bootinfo module pages
    spawn_unmap_module(binary);

    return SYS_ERR_OK;
}

errval_t spawn_run(struct spawninfo *si)
{
    return invoke_dispatcher(si->dcb, cap_dispatcher, si->rootcn_cap,
                             si->vtree, si->dispframe, true);
}

errval_t spawn_free(struct spawninfo *si)
{
    cap_destroy(si->rootcn_cap);
    cap_destroy(si->taskcn_cap);
    cap_destroy(si->pagecn_cap);
    cap_destroy(si->dispframe);
    cap_destroy(si->dcb);
    cap_destroy(si->argspg);
    cap_destroy(si->vtree);

    return SYS_ERR_OK;
}

/**
 * \brief Span a domain with the given vroot and disp_frame
 *
 * Operation similar to spawning a domain but the vroot and disp_frame
 * are already provided
 */
errval_t spawn_span_domain(struct spawninfo *si, struct capref vroot,
                           struct capref disp_frame)
{
    errval_t err;
    struct capref t1;
    struct cnoderef cnode;

    /* Spawn cspace */
    err = spawn_setup_cspace(si);
    if (err_is_fail(err)) {
        return err;
    }

    /* Create pagecn */
    t1.cnode = si->rootcn;
    t1.slot  = ROOTCN_SLOT_PAGECN;
    err = cnode_create_raw(t1, &cnode, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_PAGECN);
    }

    // Copy root of pagetable
    si->vtree.cnode = cnode;
    si->vtree.slot = 0;
    err = cap_copy(si->vtree, vroot);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_COPY_VNODE);
    }

    /* Copy dispatcher frame (in taskcn) */
    si->dispframe.cnode = si->taskcn;
    si->dispframe.slot  = TASKCN_SLOT_DISPFRAME;
    err = cap_copy(si->dispframe, disp_frame);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_COPY_VNODE);
    }

    return SYS_ERR_OK;
}
