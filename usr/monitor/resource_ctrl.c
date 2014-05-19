/**
 * \file
 * \brief Resource controller
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

#define MAX_RSRC_DOMAINS        8
#define MAX_PHASES              8

struct rsrc_phase {
    bool                active;
    bool                gang_scheduling;
    enum task_type      task_type;
    unsigned long       deadline;
    unsigned long       wcet, period, release;
    unsigned short      weight;
};

struct rsrc_domain {
    bool active;
    struct monitor_blocking_binding *b;
    struct capref disp;
    struct rsrc_phase phase[MAX_PHASES];
    int active_phase;
    bool joined[MAX_CPUS];
};

static struct rsrc_domain domain[MAX_CPUS][MAX_RSRC_DOMAINS];

static inline coreid_t get_rsrc_coreid(rsrcid_t id)
{
    return id >> 16;
}

static inline struct rsrc_domain *get_rsrc_domain(rsrcid_t id)
{
    struct rsrc_domain *d = &domain[id >> 16][id & 0xffff];

    if(d->active) {
        return d;
    } else {
        return NULL;
    }
}

static inline bool coordinator(rsrcid_t id)
{
    return get_rsrc_coreid(id) == my_core_id;
}

static void rsrc_phase_data_done(void *arg)
{
    struct intermon_binding *b = arg;
    struct intermon_state *st = b->st;

    assert(st != NULL);
    assert(st->rsrcid_inflight == true);

    errval_t err = b->tx_vtbl.rsrc_join_complete(b, NOP_CONT, st->rsrcid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending rsrc_join_complete failed");
    }

    st->rsrcid_inflight = false;
}

struct send_phase_data_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_rsrc_phase_data__args args;
};

static void send_phase_data_handler(struct intermon_binding *b,
                                    struct intermon_msg_queue_elem *e);

static void send_phase_data_cont(struct intermon_binding *b, rsrcid_t id)
{
    errval_t err;
    struct rsrc_domain *d = get_rsrc_domain(id);
    assert(d != NULL);
    struct intermon_state *st = b->st;
    assert(st != NULL);

    if(st->rsrcid_inflight) {
        goto busy;
    }

    st->rsrcid = id;
    st->rsrcid_inflight = true;

    err = b->tx_vtbl.rsrc_phase_data(b, MKCONT(rsrc_phase_data_done,b), id,
                                     d->active_phase, (uint8_t *)&d->phase,
                                     sizeof(d->phase));
    if (err_is_fail(err)) {
        st->rsrcid_inflight = false;

        if(err_no(err) == FLOUNDER_ERR_TX_BUSY)
        busy:
            {
                struct send_phase_data_state *me =
                    malloc(sizeof(struct send_phase_data_state));
                assert(me != NULL);
                me->args.id = id;
                me->elem.cont = send_phase_data_handler;

                err = intermon_enqueue_send(b, &st->queue,
                                            get_default_waitset(),
                                            &me->elem.queue);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "intermon_enqueue_send failed");
                }
                return;
            }

        USER_PANIC_ERR(err, "sending phase data");
    }
}

static void send_phase_data_handler(struct intermon_binding *b,
                                    struct intermon_msg_queue_elem *e)
{
    struct send_phase_data_state *st = (struct send_phase_data_state *)e;
    send_phase_data_cont(b, st->args.id);
    free(e);
}

/**
 * \brief Send phase data to specified satellite
 */
static void send_phase_data(rsrcid_t id, coreid_t coreid)
{
    struct intermon_binding *b = NULL;
    errval_t err = intermon_binding_get(coreid, &b);
    assert(err_is_ok(err));

    send_phase_data_cont(b, id);
}

/**
 * \brief Send phase data to all satellites
 */
static void send_phase_data_all(rsrcid_t id)
{
    struct rsrc_domain *d = get_rsrc_domain(id);
    assert(d != NULL);

    for(coreid_t i = 0; i < MAX_CPUS; i++) {
        if(d->joined[i]) {
            send_phase_data(id, i);
        }
    }
}

errval_t rsrc_join_satellite(rsrcid_t id, coreid_t coreid)
{
    struct rsrc_domain *d = get_rsrc_domain(id);
    assert(coordinator(id));

    if(d == NULL) {
        return MON_ERR_RSRC_NOT_FOUND;
    }

    d->joined[coreid] = true;

    // Send it the phase data
    send_phase_data(id, coreid);

    return SYS_ERR_OK;
}

struct monitor_blocking_binding *rsrc_get_binding(rsrcid_t id)
{
    struct rsrc_domain *d = get_rsrc_domain(id);

    if(d != NULL) {
        return d->b;
    } else {
        return NULL;
    }
}

errval_t rsrc_join(rsrcid_t id, struct capref dispcap,
                   struct monitor_blocking_binding *mb)
{
    struct rsrc_domain *d = &domain[id >> 16][id & 0xffff];

    if(d->active) {
        return MON_ERR_RSRC_MEMBER_LIMIT;
    }

    d->active = true;
    d->disp = dispcap;
    d->b = mb;

    // Join as a satellite
    coreid_t coreid = get_rsrc_coreid(id);
    if(!coordinator(id)) {
        struct intermon_binding *b = NULL;
        errval_t err = intermon_binding_get(coreid, &b);
        assert(err_is_ok(err));
        err = b->tx_vtbl.rsrc_join(b, NOP_CONT, id, my_core_id);
        assert(err_is_ok(err));
    }

    return SYS_ERR_OK;
}

errval_t rsrc_new(rsrcid_t *id)
{
    static int idx = 0;

    if(idx >= MAX_RSRC_DOMAINS) {
        return MON_ERR_RSRC_ALLOC;
    }

    *id = (my_core_id << 16) | idx;
    idx++;
    return SYS_ERR_OK;
}

errval_t rsrc_submit_manifest(rsrcid_t id, char *manifest)
{
    struct rsrc_domain *d = get_rsrc_domain(id);
    int phase = 0;

    if(d == NULL) {
        return MON_ERR_RSRC_NOT_FOUND;
    }

    // Can only submit manifests on the master for now
    assert(get_rsrc_coreid(id) == my_core_id);

    // Parse manifest
    for(char *p = manifest; *p != '\0'; p = strchr(p, '\n') + 1) {
        unsigned long wcet, period, deadline, release;
        unsigned int weight;
        char type;

        struct rsrc_phase *rp = &d->phase[phase];

        rp->active = true;
        int rd = sscanf(p, "%c", &type);
        if(rd != 1) {
            return MON_ERR_RSRC_ILL_MANIFEST;
        }

        switch(type) {
        case 'G':
        case 'H':
            if((rd = sscanf(p, "%c %lu %lu %lu %lu\n", &type, &wcet, &period,
                            &deadline, &release)) < 3) {
                return MON_ERR_RSRC_ILL_MANIFEST;
            }

            printf("phase %d: %s with WCET = %lu, period = %lu, "
                   "deadline = %lu, release = %lu\n", phase,
                   type == 'G' ? "gang scheduled" : "hard real-time",
                   wcet, period, deadline, release);
            rp->gang_scheduling = (type == 'G') ? true : false;
            rp->task_type = TASK_TYPE_HARD_REALTIME;
            rp->wcet = wcet;
            rp->period = period;
            if(rd >= 4) {
                rp->deadline = deadline;
            } else {
                rp->deadline = period;
            }
            if(rd == 5) {
                rp->release = release;
            } else {
                rp->release = 0;
            }
            break;

        case 'B':
            if(sscanf(p, "B %u\n", &weight) != 1) {
                return MON_ERR_RSRC_ILL_MANIFEST;
            }
            printf("phase %d: best effort with weight %d\n", phase, weight);
            rp->task_type = TASK_TYPE_BEST_EFFORT;
            rp->weight = weight;
            break;

        default:
            // TODO: Cleanup
            return MON_ERR_RSRC_ILL_MANIFEST;
        }

        phase++;
    }

    // Activate first phase locally
    //activate_phase(id, d, active);

    // Update satellites
    send_phase_data_all(id);

    return SYS_ERR_OK;
}

/**
 * \brief Activates a resource phase locally
 */
static void activate_phase(rsrcid_t id, struct rsrc_domain *d, uintptr_t phase,
                           uint64_t timestamp)
{
    errval_t err;
    struct rsrc_phase *p = &d->phase[phase];

    d->active_phase = phase;
    assert(p->active);

    // Set phase parameters
    err = invoke_dispatcher_properties(d->disp, p->task_type, p->deadline, p->wcet,
                                       p->period, p->release + timestamp, p->weight);
    assert(err_is_ok(err));
}

struct rsrc_phase_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_rsrc_phase__args args;
};

static void rsrc_phase_handler(struct intermon_binding *b,
                               struct intermon_msg_queue_elem *e);

static void rsrc_phase_cont(struct intermon_binding *b, rsrcid_t id,
                            uintptr_t phase, uint64_t timestamp)
{
    errval_t err = b->tx_vtbl.rsrc_phase(b, NOP_CONT, id, phase, timestamp);
    if(err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct intermon_state *st = b->st;
            assert(st != NULL);
            struct rsrc_phase_state *me =
                malloc(sizeof(struct rsrc_phase_state));
            assert(me != NULL);
            me->args.id = id;
            me->args.phase = phase;
            me->args.timestamp = timestamp;
            me->elem.cont = rsrc_phase_handler;

            err = intermon_enqueue_send(b, &st->queue,
                                        get_default_waitset(),
                                        &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "intermon_enqueue_send failed");
            }
            return;
        }
        USER_PANIC_ERR(err, "sending phase change to satellites");
    }
    assert(err_is_ok(err));
}

static void rsrc_phase_handler(struct intermon_binding *b,
                               struct intermon_msg_queue_elem *e)
{
    struct rsrc_phase_state *st = (struct rsrc_phase_state *)e;
    rsrc_phase_cont(b, st->args.id, st->args.phase, st->args.timestamp);
    free(e);
}

errval_t rsrc_set_phase_inter(rsrcid_t id, uintptr_t phase, uint64_t timestamp)
{
    struct rsrc_domain *d = get_rsrc_domain(id);

    if(d == NULL) {
        return MON_ERR_RSRC_NOT_FOUND;
    }

    activate_phase(id, d, phase, timestamp);

    // Done if satellite
    if(!coordinator(id)) {
        return SYS_ERR_OK;
    }

    // Coordinator: Change the phase globally
    for(int i = 0; i <= MAX_COREID; i++) {
        if(d->joined[i]) {
            struct intermon_binding *b = NULL;
            errval_t err = intermon_binding_get(i, &b);
            assert(err_is_ok(err));
            rsrc_phase_cont(b, id, phase, timestamp);
        }
    }

    return SYS_ERR_OK;
}

errval_t rsrc_set_phase(rsrcid_t id, uintptr_t phase)
{
    // Send to other monitors to change phase globally
    if(coordinator(id)) {
        // Relative times are relative to now
        uint64_t now;
#if defined(__x86_64__) || defined(__i386__)
        errval_t err = sys_debug_timeslice_counter_read(&now);
        assert(err_is_ok(err));
#else
        now = 0;
#endif

        // Coordinator: Change the phase globally
        rsrc_set_phase_inter(id, phase, now);
    } else {
        assert(!"no");
#if 0
        // Satellite: Tell coordinator
        coreid_t coreid = get_rsrc_coreid(id);
        struct intermon_binding *b = NULL;
        errval_t err = intermon_binding_get(coreid, &b);
        assert(err_is_ok(err));
        err = b->tx_vtbl.rsrc_phase(b, NOP_CONT, id, phase);
        assert(err_is_ok(err));
#endif
    }

    return SYS_ERR_OK;
}

errval_t rsrc_set_phase_data(rsrcid_t id, uintptr_t active, void *data,
                             size_t len)
{
    struct rsrc_domain *d = get_rsrc_domain(id);

    if(d == NULL) {
        return MON_ERR_RSRC_NOT_FOUND;
    }

    // Activate current phase
    //activate_phase(id, d, active);

    // Copy phase data
    assert(len == sizeof(d->phase));
    memcpy(&d->phase, data, len);

    // Free copy
    free(data);

    return SYS_ERR_OK;
}
