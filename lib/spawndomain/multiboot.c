/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include "spawn.h"

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

const char *multiboot_strings = NULL;

const char *getopt_module(struct mem_region *module)
{
    assert(module != NULL);
    assert(module->mr_type == RegionType_Module);

    const char *optstring = multiboot_strings + module->mrmod_data;

    // find the first space (or end of string if there is none)
    const char *args = strchr(optstring, ' ');
    if (args == NULL) {
        args = optstring + strlen(optstring);
    }

    // search backward for last '/' before the first ' '
    for (const char *c = args; c > optstring; c--) {
        if (*c == '/') {
            return c + 1;
        }
    }

    return optstring;
}

/// Map in the frame caps for a module into our vspace, return their location
errval_t spawn_map_module(struct mem_region *module, size_t *retsize,
                          lvaddr_t *retaddr, genpaddr_t *retpaddr)
{
    assert(module != NULL);
    assert(module->mr_type == RegionType_Module);

    errval_t err;

    size_t size = module->mrmod_size;

    void *base;
    struct memobj *memobj;
    struct vregion *vregion;
 
    err = vspace_map_anon_attr(&base, &memobj, &vregion, size, &size,
                               VREGION_FLAGS_READ);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }
    struct capref frame = {
        .cnode = cnode_module,
        .slot  = module->mrmod_slot,
    };

    if (retpaddr != NULL) {
        *retpaddr = module->mr_base;
    }

    if (retsize != NULL) {
        *retsize = size;
    }

    if (retaddr != NULL) {
        *retaddr = (lvaddr_t)base;
    }

    size_t offset = 0;
    while (size > 0) {
        assert((size & BASE_PAGE_MASK) == 0);

        struct frame_identity id;
        err = invoke_frame_identify(frame, &id);
        assert(err_is_ok(err));

        err = memobj->f.fill(memobj, offset, frame, 1UL << id.bits);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }

        err = memobj->f.pagefault(memobj, vregion, offset, 0);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }

        frame.slot ++;
        size -= (1UL << id.bits);
        offset += (1UL << id.bits);
    }

    return SYS_ERR_OK;
}

errval_t spawn_unmap_module(lvaddr_t mapped_addr)
{
    return vspace_unmap((void *)mapped_addr);
}

/// Returns a raw pointer to the modules string area string
const char *multiboot_module_rawstring(struct mem_region *region)
{
    if (multiboot_strings == NULL) {
	errval_t err;
        /* Map in multiboot module strings area */
        struct capref mmstrings_cap = {
            .cnode = cnode_module,
            .slot = 0
        };
        err = vspace_map_one_frame_attr((void**)&multiboot_strings,
                                        BASE_PAGE_SIZE, mmstrings_cap,
                                        VREGION_FLAGS_READ,
                                        NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map failed");
	    return NULL;
        }
#if 0
        printf("Mapped multiboot_strings at %p\n", multiboot_strings);
        for (int i = 0; i < 256; i++) {
            if ((i & 15) == 0) printf("%04x  ", i);
            printf ("%02x ", multiboot_strings[i]& 0xff);
            if ((i & 15) == 15) printf("\n");
        }
#endif
    }

    if (region == NULL || region->mr_type != RegionType_Module) {
        return NULL;
    }
    return multiboot_strings + region->mrmod_data;
}

errval_t multiboot_cleanup_mapping(void)
{
    errval_t err = vspace_unmap(multiboot_strings);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "multiboot_cleanup_mapping: vspace_unmap() failed\n");
        return err_push(err, LIB_ERR_VSPACE_REMOVE_REGION);
    }
    multiboot_strings = NULL;
    return SYS_ERR_OK;
}


/// returns the basename without arguments of a multiboot module
// XXX: returns pointer to static buffer. NOT THREAD SAFE
const char *multiboot_module_name(struct mem_region *region)
{
    const char *str = multiboot_module_rawstring(region);
    if (str == NULL) {
	return NULL;
    }

    // copy module data to local buffer so we can mess with it
    static char buf[128];
    strncpy(buf, str, sizeof(buf));
    buf[sizeof(buf) - 1] = '\0'; // ensure termination

    // ignore arguments for name comparison
    char *args = strchr(buf, ' ');
    if (args != NULL) {
        *args = '\0';
    }

    return buf;
}

struct mem_region *multiboot_find_module(struct bootinfo *bi, const char *name)
{
    for(size_t i = 0; i < bi->regions_length; i++) {
        struct mem_region *region = &bi->regions[i];

        const char *modname = multiboot_module_name(region);
        if (modname != NULL &&
            strncmp(modname + strlen(modname) - strlen(name),
                    name, strlen(name)) == 0) {
            return region;
        }
    }

    return NULL;
}

struct mem_region *multiboot_find_module_containing(struct bootinfo *bi,
						    const char *name,
						    const char *containing)
{
    assert(bi != NULL);
    assert(name != NULL);
    assert(containing != NULL);

    size_t namelen = strlen(name);

    for(size_t i = 0; i < bi->regions_length; i++) {
        struct mem_region *region = &bi->regions[i];
	if (region->mr_type != RegionType_Module)
	    continue;
	const char *raw = multiboot_module_rawstring(region);
	if (raw == NULL)
	    continue;

	const char *space = strchr(raw, ' ');
	if (space == NULL || (space - raw < namelen))
	    continue;

	if (strncmp(space - namelen, name, namelen))
	    continue;

	if ((strstr(raw, containing)) != NULL)
	    return region;
    }

    return NULL;
}

