/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_H
#define MONITOR_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <bench/bench_arch.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_defs.h>
#include <if/intermon_defs.h>
#include <monitor_invocations.h>
#include "rcap_db_common.h"
#include "queue.h"
#include "connection.h"

// Change #URPC_SIZE if changing this
#define MON_URPC_CHANNEL_LEN  (32 * UMP_MSG_BYTES)
#define MON_RAM_CHANNEL_LEN   (2  * UMP_MSG_BYTES)

// XXX: These should match the aliases in intermon.if
typedef uint64_t state_id_t;
typedef uint64_t mon_id_t;
typedef uint64_t con_id_t;
typedef uint32_t chanid_t;
typedef uint8_t  bool_t;

// XXX: from old routing library, to be removed
typedef uint32_t recordid_t;

//XXX used to wait until all monitors are up and connected. asq
extern int seen_connections;

struct intermon_state {
    struct msg_queue queue;             ///< Queue of outgoing messages
    struct intermon_binding *binding;   ///< Back-pointer to binding
    coreid_t core_id;                   ///< Core ID of monitor on other end
    rsrcid_t rsrcid;
    bool rsrcid_inflight;
    struct monitor_binding *originating_client;
};

struct monitor_state {
    struct msg_queue queue;
};

extern iref_t mem_serv_iref;
extern iref_t name_serv_iref;
extern iref_t ramfs_serv_iref;
extern iref_t monitor_rpc_iref;
extern iref_t monitor_mem_iref;
extern coreid_t my_core_id;
extern bool bsp_monitor;
extern struct capref trace_cap;
extern struct bootinfo *bi;
extern bool update_ram_alloc_binding;
extern int num_monitors;

union capability_caprep_u {
    intermon_caprep_t caprep;
    monitor_blocking_caprep_t caprepb; // XXX: identical to intermon_caprep_t
    struct capability cap;
};
STATIC_ASSERT(sizeof(union capability_caprep_u) >= sizeof(struct capability), \
                  ASSERT_CONCAT("Size mismatch:", intermon_caprep_t));

static inline void capability_to_caprep(struct capability *cap,
                                        intermon_caprep_t *caprep)
{
    union capability_caprep_u u = { .cap = *cap };
    *caprep = u.caprep;
}

static inline void caprep_to_capability(intermon_caprep_t *caprep,
                                        struct capability *cap)
{
    union capability_caprep_u u = { .caprep = *caprep };
    *cap = u.cap;
}

/* ram_alloc.c */
errval_t mon_ram_alloc_init(coreid_t core_id, struct intermon_binding *b);
errval_t mon_ram_alloc_serve(void);

/* spawn.c */
errval_t spawn_all_domains(void);
errval_t spawn_domain(char *name);
errval_t spawn_domain_with_args(const char *name, char *const argv[],
                                char *const envp[]);
errval_t spawn_module_with_args(const char *name, struct mem_region *module,
                                char *const argv[], char *const envp[]);
errval_t span_domain(struct capref vroot, struct capref dispframe);
errval_t spawn_spawnd(struct intermon_binding *b);

/* monitor_server.c */
errval_t monitor_client_setup(struct spawninfo *si);
errval_t monitor_client_setup_mem_serv(void);
errval_t monitor_client_setup_monitor(void);
errval_t monitor_server_init(struct monitor_binding *b);

/* monitor_rpc_server.c */
errval_t monitor_rpc_init(void);

/* invocations.c */
bool monitor_can_send_cap(struct capability *cap);
errval_t monitor_cap_identify(struct capref cap, struct capability *out);
errval_t monitor_domains_cap_identify(struct capref croot, capaddr_t cap,
                                      int vbits, struct capability *out);
errval_t monitor_cap_remote(struct capref cap, bool is_remote, bool * has_dep);
errval_t monitor_cap_create(struct capref dest, struct capability *cap,
                            coreid_t core_id);
errval_t monitor_identify_cnode_get_cap(struct capability *cnode_raw, 
                                        capaddr_t slot, struct capability *ret);
errval_t monitor_nullify_cap(struct capref cap);
errval_t monitor_retype_remote_cap(struct capref croot, 
                                   capaddr_t src, enum objtype newtype, 
                                   int objbits, capaddr_t to, capaddr_t slot, 
                                   int bits);
errval_t monitor_delete_remote_cap(struct capref croot, capaddr_t src, int bits);
errval_t monitor_revoke_remote_cap(struct capref croot, capaddr_t src, int bits);

/* monitor_server.c */
errval_t monitor_server_arch_init(struct monitor_binding *b);
void set_monitor_rpc_iref(iref_t iref);

/* boot.c */
void boot_core_request(struct monitor_binding *st, coreid_t id, int32_t hwid,
                       int32_t int_cpu_type, char *cmdline);
void boot_initialize_request(struct monitor_binding *st);

errval_t spawn_xcore_monitor(coreid_t id, int hwid, enum cpu_type cpu_type,
                             const char *cmdline,
                             struct intermon_binding **ret_binding);
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding);

/* main.c */
errval_t request_trace_caps(struct intermon_binding *st);
errval_t request_mem_serv_iref(struct intermon_binding *st);
errval_t request_name_serv_iref(struct intermon_binding *st);
errval_t request_ramfs_serv_iref(struct intermon_binding *st);

/* inter.c */
errval_t intermon_init(struct intermon_binding *b, coreid_t coreid);
errval_t arch_intermon_init(struct intermon_binding *b);

/* rcap_db_{null,central,twopc}.c */
errval_t rcap_db_init (void);
errval_t rcap_db_add(struct capability * cap, bool has_desc);
bool rcap_db_exists(struct capability *cap);
errval_t rcap_db_get_info(struct capability *cap, bool * has_desc, 
                          coremask_t *on_cores);
errval_t rcap_db_update_on_recv (struct capability * cap, bool has_desc,
                                 coremask_t on_cores, coreid_t from_core);
errval_t rcap_db_acquire_lock(struct capability *cap, struct rcap_st * st);
errval_t rcap_db_remote_lock_req(struct capability *cap, coreid_t from_core, 
                                 recordid_t ccast_recordid);
errval_t rcap_db_release_lock(struct capability *cap, coremask_t to_cores);
errval_t rcap_db_remote_unlock(struct capability *cap, coreid_t from_core);
errval_t rcap_db_acquire_recursive_lock(struct capability *cap,
                                        struct rcap_st * st);
errval_t rcap_db_remote_recursive_lock_req(struct capability *cap,
                                           coreid_t from_core, 
                                           recordid_t ccast_recordid);
errval_t rcap_db_release_recursive_lock(struct capability *cap, 
                                        coremask_t to_cores);
errval_t rcap_db_remote_recursive_unlock(struct capability *cap, 
                                         coreid_t from_core);
errval_t rcap_db_remote_new_core(struct capability * cap, coreid_t send_core, 
                                 coreid_t recv_core);
errval_t rcap_db_remote_details_req(struct capability * cap, 
                                    coreid_t from_core);
errval_t rcap_db_remote_recv_details(struct capability * cap, 
                                     coreid_t from_core, bool has_desc);
errval_t rcap_db_delete (struct capability * cap);
errval_t rcap_db_remote_delete (struct capability * cap, coreid_t from_core);
errval_t rcap_db_retype(struct capability * cap, bool has_descendents);
errval_t rcap_db_remote_retype (struct capability * cap, bool has_descendents,
                                coreid_t from_core);
errval_t rcap_db_revoke(struct capability * cap);
errval_t rcap_db_remote_revoke (struct capability * cap, coreid_t from_core);



/* ump_support.c */
errval_t ump_intermon_init(struct intermon_binding *ib);
errval_t ump_monitor_init(struct monitor_binding *mb);

/* multihop_support.c */
errval_t multihop_intermon_init(struct intermon_binding *ib);
errval_t multihop_monitor_init(struct monitor_binding *mb);
errval_t multihop_request_routing_table(struct intermon_binding *b);

/* trace_support.c */
errval_t trace_intermon_init(struct intermon_binding *ib);
errval_t trace_monitor_init(struct monitor_binding *mb);

/* bfscope_support.c */
errval_t bfscope_intermon_init(struct intermon_binding *ib);
errval_t bfscope_monitor_init(struct monitor_binding *mb);

/* rck_support.c */
errval_t rck_intermon_init(struct intermon_binding *ib);
errval_t rck_monitor_init(struct monitor_binding *mb);

// Resource control
errval_t rsrc_new(rsrcid_t *id);
errval_t rsrc_join_satellite(rsrcid_t id, coreid_t coreid);
errval_t rsrc_join(rsrcid_t id, struct capref dispcap,
                   struct monitor_blocking_binding *b);
errval_t rsrc_submit_manifest(rsrcid_t id, char *manifest);
errval_t rsrc_set_phase(rsrcid_t id, uintptr_t phase);
errval_t rsrc_set_phase_inter(rsrcid_t id, uintptr_t phase, uint64_t timestamp);
struct monitor_blocking_binding *rsrc_get_binding(rsrcid_t id);
errval_t rsrc_set_phase_data(rsrcid_t id, uintptr_t active, void *data,
                             size_t len);

// Time coordination
errval_t timing_sync_timer(void);
void timing_sync_timer_reply(errval_t err);
void timing_sync_bench(void);

/* domain.c */
void domain_mgmt_init(void);

/* intermon_bindings.c */
errval_t intermon_binding_set(struct intermon_state *st);
errval_t intermon_binding_get(coreid_t coreid, struct intermon_binding **ret);

/* iref.c */
errval_t iref_alloc(struct monitor_binding *binding, uintptr_t service_id,
                    iref_t *iref);
errval_t iref_get_core_id(iref_t iref, coreid_t *core_id);
errval_t iref_get_binding(iref_t iref, struct monitor_binding **binding);
errval_t iref_get_service_id(iref_t iref, uintptr_t *service_id);

#endif // MONITOR_H
