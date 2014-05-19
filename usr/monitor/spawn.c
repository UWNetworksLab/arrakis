/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() in string.h */

#include "monitor.h"
#include <barrelfish/cpu_arch.h>

extern char **environ;

/**
 * \brief Some domains require special caps for functionality
 * and we do not have a better way to to this yet
 */
static errval_t set_special_caps(struct spawninfo *si, const char *pname)
{
    struct capref src, dest;
    errval_t err;

    // lop off leading path for name comparison
    const char *name = strrchr(pname, '/');
    if (name == NULL) {
        name = pname;
    } else {
        name++;
    }

    /* Pass IRQ cap to bfscope (XXX: kludge) */
    if (!strcmp(name, "bfscope")) {
        dest.cnode = si->taskcn;
        dest.slot  = TASKCN_SLOT_IRQ;
        src.cnode = cnode_task;
        src.slot  = TASKCN_SLOT_IRQ;
        err = cap_copy(dest, src);
        if (err_is_fail(err)) {
            return err_push(err, SPAWN_ERR_COPY_IRQ_CAP);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Spawn the domain identified by #name
 */
errval_t spawn_domain(char *name)
{
    errval_t err;

    /* Get the module from the multiboot */
    struct mem_region *module = multiboot_find_module(bi, name);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    /* Get and tokenize cmdline args */
    char *args;
    err = spawn_get_cmdline_args(module, &args);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_GET_CMDLINE_ARGS);
    }
    char *argv[MAX_CMDLINE_ARGS + 1];
    spawn_tokenize_cmdargs(args, argv, ARRAY_LENGTH(argv));

    err = spawn_module_with_args(name, module, argv, environ);
    free(args);
    return err;
}
/**
 * \brief Spawn the domain identified by #name with the provided args
 */
errval_t spawn_domain_with_args(const char *name,
                                char *const argv[], char *const envp[])
{
    struct mem_region *module = multiboot_find_module(bi, name);
    if (module == NULL) {
        return SPAWN_ERR_FIND_MODULE;
    }

    return spawn_module_with_args(name, module, argv, envp);
}

/**
 * \brief Spawn the domain identified by #module with the provided args
 */
errval_t spawn_module_with_args(const char *name, struct mem_region *module,
                                char *const argv[], char *const envp[])
{
    errval_t err;
    struct spawninfo si;

    printf("Spawning %s on core %d\n", name, my_core_id);

    err = spawn_load_with_args(&si, module, name, my_core_id, argv, envp);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    // Set special caps
    err = set_special_caps(&si, name);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SET_CAPS);
    }

    // Set connection
    err = monitor_client_setup(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    }

    // Make runnable
    err = spawn_run(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_RUN);
    }

    // Cleanup
    err = spawn_free(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_FREE);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Spawn the domain in the given image with the provided args
 */
static errval_t spawn_image_with_args(const char *name, void *image,
                                      size_t imagelen,
                                      char *const argv[], char *const envp[])
{
    errval_t err;
    struct spawninfo si;

    printf("Spawning %s on core %d\n", name, my_core_id);

    err = spawn_load_image(&si, (lvaddr_t)image, imagelen, CURRENT_CPU_TYPE,
                           name, my_core_id, argv, envp, NULL_CAP, NULL_CAP);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    // Set special caps
    err = set_special_caps(&si, name);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SET_CAPS);
    }

    // Set connection
    err = monitor_client_setup(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    }

    // Make runnable
    err = spawn_run(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_RUN);
    }

    // Cleanup
    err = spawn_free(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_FREE);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Spawn all 'boot' domains in modules (menu.lst)
 */
errval_t spawn_all_domains(void)
{
    errval_t err;

    char name[128];

    for(size_t i = 0; i < bi->regions_length; i++) {
        // Lookup modules in bootinfo
        struct mem_region *region = &bi->regions[i];
        if (region->mr_type != RegionType_Module) { /* Not of module type */
            continue;
        }

        const char *mm_name = multiboot_module_name(region);
        // Copy name because walking the multiboot changes the name field
        strncpy(name, mm_name, sizeof(name));

        // Lop off the path from the name
        const char *short_name = strrchr(name, '/');
        if (!short_name) {
            short_name = name;
        } else {
            short_name++;
        }

        /* Do not spawn special domains */
        if(!strcmp(short_name, "init") ||
           !strcmp(short_name, "skb") ||
           !strcmp(short_name, "ramfsd") ||
           !strcmp(short_name, "cpu") ||
           !strcmp(short_name, "monitor") ||
           !strcmp(short_name, "mem_serv")) {
            continue;
        }

        /* Get and tokenize cmdline args */
        char *args;
        err = spawn_get_cmdline_args(region, &args);
        if (err_is_fail(err)) {
            return err_push(err, SPAWN_ERR_GET_CMDLINE_ARGS);
        }

        // Pass the local arch-specific core ID to the PCI and spawnd domains
        if(strcmp(short_name, "pci") == 0 
           || strcmp(short_name, "spawnd") == 0
           || strcmp(short_name, "kaluga") == 0
           || strcmp(short_name, "acpi") == 0
           || strcmp(short_name, "ioapic") == 0) {
            // Get hardware core ID
            uintptr_t my_arch_id = 0;
            err = invoke_monitor_get_arch_id(&my_arch_id);
            assert(err_is_ok(err));

            char *myargs = malloc(strlen(args) + 50);
            snprintf(myargs, strlen(args) + 50, "%s apicid=%" PRIuPTR,
                     args, my_arch_id);
            free(args);
            args = myargs;
        }

        char *argv[MAX_CMDLINE_ARGS + 1];
        int argc = spawn_tokenize_cmdargs(args, argv, ARRAY_LENGTH(argv));

        /* Only spawn boot time modules */
        if (argc > 1 && strcmp(argv[1], "boot") == 0) {
            err = spawn_module_with_args(name, region, argv, environ);
            if (err_is_fail(err)) {
                return err_push(err, MON_ERR_SPAWN_DOMAIN);
            }
        }
        free(args);
    }

    return SYS_ERR_OK;
}

static struct {
    genpaddr_t base;
    size_t bytes;
} spawnd_image;

static void spawnd_image_reply_handler(struct intermon_binding *b,
                                       genpaddr_t base, uint32_t bytes)
{
    assert(spawnd_image.base == 0);
    assert(base > 0 && bytes > 0);
    spawnd_image.base = base;
    spawnd_image.bytes = bytes;
}

/**
 * \brief Span a domain to this core.
 */
errval_t spawn_spawnd(struct intermon_binding *b)
{
    assert(!bsp_monitor);

    // find out where the image is
    b->rx_vtbl.spawnd_image_reply = spawnd_image_reply_handler;
    errval_t err = b->tx_vtbl.spawnd_image_request(b, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }

    while(spawnd_image.base == 0) {
        messages_wait_and_handle_next();
    }

    // construct cap to it
    struct capability cap_raw = {
        .type = ObjType_Frame,
        .rights = CAPRIGHTS_ALLRIGHTS,
        .u.frame = {
            .base = spawnd_image.base,
            .bits = log2ceil(spawnd_image.bytes),
        }
    };
    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "allocating slot failed for spawnd image");
    }

    err = monitor_cap_create(frame, &cap_raw, my_core_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_cap_create failed");
    }

    // map the image in
    // XXX: leak memobj/region
    void *image;
    err = vspace_map_one_frame(&image, spawnd_image.bytes, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // spawn!
    char *argv[] = { "spawnd", NULL };
    return spawn_image_with_args(argv[0], image, spawnd_image.bytes, argv,
                                 environ);
}

/**
 * \brief Span a domain to this core.
 */
errval_t span_domain(struct capref vroot, struct capref dispframe)
{
    struct spawninfo si;
    errval_t err;

    printf("Spanning domain to core %d\n", my_core_id);

    // Span domain
    err = spawn_span_domain(&si, vroot, dispframe);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_SPAN);
    }

    // Set connection
    err = monitor_client_setup(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    }

    // Make runnable
    err = spawn_run(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_RUN);
    }

    // Cleanup
    err = spawn_free(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_FREE);
    }

    return SYS_ERR_OK;
}
