/**
 * \file
 * \brief Domain spawn functionality.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWNDOMAIN_H
#define SPAWNDOMAIN_H

#include <sys/cdefs.h>

//XXX: added alignment to workaround an arm-gcc bug
//which generated (potentially) unaligned access code to those fields
/**
 * \brief Struct to refer to the various caps within a domain being spawned.
 */
struct spawninfo {
    domainid_t domain_id;
    struct cnoderef rootcn __attribute__ ((aligned(4)));
    struct cnoderef taskcn __attribute__ ((aligned(4)));
    struct cnoderef segcn  __attribute__ ((aligned(4)));
    struct cnoderef pagecn __attribute__ ((aligned(4)));
    struct capref   rootcn_cap __attribute__ ((aligned(4)));
    struct capref	taskcn_cap __attribute__ ((aligned(4)));
    struct capref	pagecn_cap __attribute__ ((aligned(4)));
    struct capref   dispframe __attribute__ ((aligned(4)));
    struct capref	dcb __attribute__ ((aligned(4)));
    struct capref	argspg __attribute__ ((aligned(4)));
    struct capref 	vtree __attribute__ ((aligned(4)));

    // Slot (in segcn) from where elfload_allocate should allocate frames from
    cslot_t elfload_slot;

    // vspace of spawned domain
    struct vspace *vspace;
    struct vregion *vregion[16];
    genvaddr_t base[16];
    unsigned int vregions;

    dispatcher_handle_t handle;
    enum cpu_type cpu_type;
    int codeword;
    char *append_args;

    // slot allocator for pagecn
    struct single_slot_allocator pagecn_slot_alloc;

    // TLS data
    genvaddr_t tls_init_base;
    size_t tls_init_len, tls_total_len;
};

__BEGIN_DECLS
errval_t spawn_get_cmdline_args(struct mem_region *module,
                                char **retargs);
int spawn_tokenize_cmdargs(char *args, char *argv[], size_t argv_len);
errval_t spawn_load_with_bootinfo(struct spawninfo *si, struct bootinfo *bi,
                                  const char *name, coreid_t coreid);
errval_t spawn_load_with_args(struct spawninfo *si, struct mem_region *module,
                              const char *name, coreid_t coreid,
                              char *const argv[], char *const envp[]);
errval_t spawn_load_image(struct spawninfo *si, lvaddr_t binary,
                          size_t binary_size, enum cpu_type type,
                          const char *name, coreid_t coreid,
                          char *const argv[], char *const envp[],
                          struct capref inheritcn_cap, struct capref argcn_cap);
errval_t spawn_run(struct spawninfo *si);
errval_t spawn_free(struct spawninfo *si);

errval_t multiboot_cleanup_mapping(void);

/* spawn_vspace.c */
errval_t spawn_vspace_init(struct spawninfo *si, struct capref vnode,
                           enum cpu_type cpu_type);
errval_t spawn_vspace_map_one_frame(struct spawninfo *si, genvaddr_t *retaddr,
                                    struct capref frame, size_t size);
errval_t spawn_vspace_map_fixed_one_frame(struct spawninfo *si, genvaddr_t addr,
                                          struct capref frame, size_t size);
errval_t spawn_vspace_map_anon_fixed_attr(struct spawninfo *si, genvaddr_t addr,
                                          size_t size, struct vregion **vregion,
                                          struct memobj **memobj,
                                          vregion_flags_t flags);

/// Returns a raw pointer to the modules string area string
const char *multiboot_module_rawstring(struct mem_region *region);
const char *multiboot_module_name(struct mem_region *region);
struct mem_region *multiboot_find_module(struct bootinfo *bi, const char *name);
struct mem_region *multiboot_find_module_containing(struct bootinfo *bi,
						    const char *name,
						    const char *containing);
errval_t spawn_map_module(struct mem_region *module, size_t *retsize,
                          lvaddr_t *retaddr, genpaddr_t *retpaddr);
errval_t spawn_unmap_module(lvaddr_t mapped_addr);
errval_t spawn_map_bootinfo(struct spawninfo *si, genvaddr_t *retvaddr);
const char *getopt_module(struct mem_region *module);

/* typedef void (*spawn_notify_func)(uint64_t domain_id); */

/* errval_t get_cmdline_args(char *name, struct bootinfo *bi, char **retargs); */
/* void detokenize_cmdargs(char *args, int argc, char *argv[]); */
/* void tokenize_cmdargs(char *args, int *argc, char *argv[]); */
errval_t spawn_span_domain(struct spawninfo *si, struct capref vroot,
                           struct capref disp_frame);
/* void spawn_notify(struct spawninfo *si, bool notify); */
/* void spawn_domain_id(struct spawninfo *si, uint64_t domain_id); */
/* errval_t spawn_make_runnable(struct spawninfo *si); */
/* errval_t spawn_make_runnable_new_domain(struct spawninfo *si, */
/*                                         uint64_t *domain_id); */
/* void spawn_register_notify(spawn_notify_func f); */

/* errval_t spawn_fill_smallcn(struct spawninfo *si, const uint8_t nos); */
/* errval_t spawn_get_cmdargs(char *name, struct bootinfo *bi, const char **cmdargs); */
/* errval_t spawn_memory(struct spawninfo *si, const char *name, uint8_t core_id, */
/*                       int argc, char *argv[], lvaddr_t binary, */
/*                       size_t binary_size); */
__END_DECLS

#endif //SPAWNDOMAIN_H
