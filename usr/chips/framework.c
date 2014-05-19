/**
 * \file
 * \brief Chips framework implementation
 */

/*
 * Copyright (c) 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <if/nameservice_defs.h>
#include <if/monitor_defs.h>

#include "hashtable.h"
#include "multimap.h"
#include "filter.h"
#include "framework.h"

#include "queue.h"

// enable to check for regression
// in hashtable and filter implementation
//#define CHIPS_TESTS_ENABLED
//#define CHIPS_DEBUG_REGISTRY

// the service registry
static struct multimap *registry;

// pending (blocked) lookups on the registry
struct pending_lookup {
    char *iface;
    struct nameservice_binding *b;
    struct pending_lookup *next;
};

static struct pending_lookup *pending_lookups = NULL;

static inline struct service_reference*
get_service_reference(char* key)
{
    void *val;
    registry->get_first(registry, key, &val);
    return (struct service_reference *) val;
}

static void wait_for_service_reference_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.wait_for_service_reference_response(b,
            MKCONT(free, ns), ns->ref_handle);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}


// walk queue of pending lookups, sending replies for any that now match
static void process_pending_lookups(void)
{
    struct pending_lookup *p, *prevp, *nextp;
    struct service_reference *ref;
    errval_t err;

    for (p = pending_lookups, prevp = NULL; p != NULL; p = nextp) {
        nextp = p->next;

        ref = get_service_reference(p->iface);
        if (ref != NULL) { // found entry: reply and remove from queue
            assert(p != NULL);

            struct ns_reply_state* ns = NULL;
            err = new_ns_reply(&ns, wait_for_service_reference_reply);
            assert(err_is_ok(err));
            ns->ref_handle = (nameservice_srvref_t)(uintptr_t)ref;

            printf("%"PRIuDOMAINID" chips: notifying client about %s\n", disp_get_domain_id(), p->iface);
            ns->rpc_reply(p->b, ns);

            if (p == pending_lookups) {
                assert(prevp == NULL);
                pending_lookups = nextp;
            } else {
                assert(prevp != NULL);
                prevp->next = nextp;
            }

            free(p->iface);
            free(p);
        } else {
            prevp = p;
        }
    }
}

static void register_service_handler_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.register_service_response(b, MKCONT(free, ns),
                                    ns->reg_handle);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void register_service_handler(struct nameservice_binding *b,
                                     iref_t iref, char *iface)
{
    struct dictionary *dict = NULL; // XXX: not yet supported by flounder
    struct service_registration *reg;
    struct service_reference *ref;
    errval_t err;
    int r;

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registering %s with properties\n", iface);
    print_hashtable(stderr, (struct hashtable_t *) dict);
#endif

    reg = malloc(sizeof(struct service_registration));
    assert(reg != NULL);

    ref = malloc(sizeof(struct service_reference));
    assert(ref != NULL);

    // populate service reference
    ref->iface = iface;
    ref->dict = dict;
    ref->service = iref;

    // populate service registration
    reg->ref = ref;

    r = registry->put(registry, iface, ref);
    assert(r == SYS_ERR_OK);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registry after registration:\n");
    print_hashtable(stdout, (struct hashtable_t *) registry);
#endif


    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, register_service_handler_reply);
    assert(err_is_ok(err));

    // send back the service registration handle
    // XXX: unsafe to send pointer to our local state!
    ns->reg_handle = (nameservice_reghandle_t)(uintptr_t)reg;
    ns->rpc_reply(b, ns);

    process_pending_lookups();
}

static void unregister_service_handler_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.unregister_service_response(b, MKCONT(free, ns));

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void unregister_service_handler(struct nameservice_binding *b,
                                       nameservice_reghandle_t reghandle)
{
    struct service_registration *reg = (struct service_registration *)(uintptr_t)reghandle;
    errval_t err;
    int r;

    // remove from the registry
    r = registry->remove(registry, reg->ref->iface, (void*) reg->ref);

    free(reg->ref);
    free(reg);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("registry after deregistration:\n");
    print_hashtable(stdout, (struct hashtable_t *) registry);
#endif

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, unregister_service_handler_reply);
    assert(err_is_ok(err));
    ns->rpc_reply(b, ns);
}

static void get_service_reference_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.get_service_reference_response(b, MKCONT(free, ns),
                                           ns->ref_handle);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}


static void get_service_reference_handler(
    struct nameservice_binding *b, char *iface)
{
    struct service_reference *ref;
    errval_t err;

    // get the service reference from the registry
    // can be a NULL pointer if there is no such service
    ref = get_service_reference(iface);
    free(iface);

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, get_service_reference_reply);
    assert(err_is_ok(err));
    ns->ref_handle = (nameservice_srvref_t)(uintptr_t)ref;
    ns->rpc_reply(b, ns);
}

static void wait_for_service_reference_handler(
    struct nameservice_binding *b, char *iface)
{
    struct service_reference *ref;
    errval_t err;

    // get the service reference from the registry
    // can be a NULL pointer if there is no such service
    ref = get_service_reference(iface);

    // if we didn't find anything, add it to the pending lookups queue
    if (ref == NULL) {
        printf("%"PRIuDOMAINID" chips: client waiting for %s\n", disp_get_domain_id(), iface);
        struct pending_lookup *pending = malloc(sizeof(struct pending_lookup));
        assert(pending != NULL);
        pending->iface = iface;
        assert(b != NULL);
        pending->b = b;
        pending->next = pending_lookups;
        pending_lookups = pending;
    } else {
        // reply with existing entry
        free(iface);

        struct ns_reply_state* ns = NULL;
        err = new_ns_reply(&ns, wait_for_service_reference_reply);
        assert(err_is_ok(err));
        ns->ref_handle = (nameservice_srvref_t)(uintptr_t)ref;
        ns->rpc_reply(b, ns);
    }
}

#if 0
static void getservice_references_handler(
    struct nameservice_binding *b, char *iface, char *filter_str,
    uint64_t max_count)

{
    filter_t filter = create_filter(filter_str);

#ifdef CHIPS_DEBUG_REGISTRY
    printf("looking for %ld %s services with fulfilling %s\n", max_count, iface,
           filter_str);
#endif
    free(filter_str);

    struct service_reference **refs = malloc(max_count * sizeof(struct service_reference*));
    assert(refs != NULL);

    // get the candidate list
    // TODO: this is not correct. Cut the result set and not the candidate set!
    uint64_t candidate_count = registry->get_all(registry, iface, (void**) refs, max_count);
#ifdef CHIPS_DEBUG_REGISTRY
    printf("I have %ld candidates\n", candidate_count);
#endif

    for (uint64_t i=0; i<candidate_count; i++) {
        ref = refs[i];

#ifdef CHIPS_DEBUG_REGISTRY
        printf("checking %s with properties\n", iface);
        printf("filter ");
        print_filter(stdout, filter);
        print_hashtable(stdout, (struct hashtable_t *) registry);
#endif

        assert(ref->dict != NULL);
        assert(filter != NULL);
        if (match_filter(ref->dict, filter)) {
#ifdef CHIPS_DEBUG_REGISTRY
            printf("->match\n");
#endif
            XXX: add ref to reply set
        }
    }

    free(iface);
    destroy_filter(filter);
    free(refs);

    err = b->tx_vtbl.get_services_references(st->conn, XXX);
    assert(err_is_ok(err)); // XXX
}
#endif

static void get_service_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.get_service_response(b, MKCONT(free, ns), ns->iref);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void get_service_handler(struct nameservice_binding *b,
                                nameservice_srvref_t srvref)
{
    struct service_reference *ref = (void *)(uintptr_t)srvref;
    errval_t err;

    // XXX TODO: check that the ref is still valid
    // is easier when the service ID is implemented as
    // a default attribute
    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, get_service_reply);
    assert(err_is_ok(err));
    ns->iref = ref->service;
    ns->rpc_reply(b, ns);
}

/***** Simple capability store *****/

static struct hashtable *capdb = NULL;

static void get_cap_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.get_cap_response(b, MKCONT(free, ns), ns->cap, ns->err);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void get_cap_handler(struct nameservice_binding *b, char *key)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct capref cap;

    capdb->d.get_capability(&capdb->d, key, &cap);

    if(capcmp(cap, NULL_CAP)) {
        reterr = CHIPS_ERR_UNKNOWN_NAME;
    }

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, get_cap_reply);
    assert(err_is_ok(err));
    ns->cap = cap;
    ns->err = reterr;
    ns->rpc_reply(b, ns);
}

static void put_cap_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.put_cap_response(b, MKCONT(free, ns), ns->err);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void put_cap_handler(struct nameservice_binding *b, char *key,
                            struct capref cap)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct capref dbcap;

    capdb->d.get_capability(&capdb->d, key, &dbcap);
    if(!capcmp(dbcap, NULL_CAP)) {
        reterr = CHIPS_ERR_EXISTS;
        err = cap_delete(cap);
        assert(err_is_ok(err));
    } else {
        int r = capdb->d.put_capability(&capdb->d, key, cap);
        assert(r == 0);
    }

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, put_cap_reply);
    assert(err_is_ok(err));
    ns->err = reterr;
    ns->rpc_reply(b, ns);
}

static void remove_cap_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.remove_cap_response(b, MKCONT(free, ns), ns->err);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void remove_cap_handler(struct nameservice_binding *b, char *key)
{
    errval_t err, reterr = SYS_ERR_OK;

    if(capdb->d.remove(&capdb->d, key))
      reterr = CHIPS_ERR_UNKNOWN_NAME;

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, remove_cap_reply);
    assert(err_is_ok(err));
    ns->err = reterr;
    ns->rpc_reply(b, ns);
}

/***** Semaphores  *****/

#define MAX_SEM         2048
#define MAX_QUEUE       128

struct sem {
    bool allocated;
    uint32_t value;
    struct nameservice_binding *queue[MAX_QUEUE];
//  struct nameservice_binding *holder;
};

static struct sem sems[MAX_SEM];

static void sem_new_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.sem_new_response(b, MKCONT(free, ns), ns->semval, ns->err);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void sem_new_handler(struct nameservice_binding *b, uint32_t value)
{
    errval_t err, reterr = SYS_ERR_OK;
    int i;

    for(i = 0; i < MAX_SEM; i++) {
        if(!sems[i].allocated) {
            sems[i].allocated = true;
            break;
        }
    }

    if(i != MAX_SEM) {
      printf("chips: sem_new(%d, %"PRIu32")\n", i, value);
        sems[i].value = value;
	// sems[i].holder = NULL;
    } else {
        reterr = CHIPS_ERR_OUT_OF_SEMAPHORES;
    }

    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, sem_new_reply);
    assert(err_is_ok(err));
    ns->err = reterr;
    ns->semval = i;
    ns->rpc_reply(b, ns);
}

static void sem_post_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.sem_post_response(b, MKCONT(free, ns));

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void sem_wait_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.sem_wait_response(b, MKCONT(free, ns));

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void sem_post_handler(struct nameservice_binding *b, uint32_t sem)
{
    assert(sem < MAX_SEM);
    struct sem *s = &sems[sem];
    struct ns_reply_state* ns = NULL;

    assert(s->allocated);
    errval_t err;

    //printf("%d chips: sem_post %u, %u\n", disp_get_domain_id(), sem, s->value);
    
    if(s->value == 0) {
        for(int i = 0; i < MAX_QUEUE; i++) {
            if(s->queue[i] != NULL) {
                // Wakeup one
	        //printf("%d chips: waking up one\n", disp_get_domain_id());
                struct ns_reply_state* ns1 = NULL;
                err = new_ns_reply(&ns1, sem_wait_reply);
                assert(err_is_ok(err));
                ns1->rpc_reply(s->queue[i], ns1);
                assert(err_is_ok(err));
                s->queue[i] = NULL;
                goto out;
            }
        }

	//s->holder = NULL;
    }

    // Increment
    s->value++;

out:
    err = new_ns_reply(&ns, sem_post_reply);
    assert(err_is_ok(err));
    ns->rpc_reply(b, ns);

    //printf("%d chips: sem_post done\n", disp_get_domain_id());
}

static void sem_wait_handler(struct nameservice_binding *b, uint32_t sem)
{
    assert(sem < MAX_SEM);
    struct sem *s = &sems[sem];
    assert(s->allocated);
    errval_t err;

    //printf("%d chips: sem_wait %u, %u\n", disp_get_domain_id(), sem, s->value);
    if(s->value == 0) {
      int i;

      //printf("%d chips: waiting\n", disp_get_domain_id());

/* Try to avoid the deadlock of Postgres processes entering the wait section
 * recursively.
      if(s->holder == b) {
        err = b->tx_vtbl.sem_wait_response(b, NOP_CONT);
        assert(err_is_ok(err));
	return;
      }
*/
        // Wait
        for(i = 0; i < MAX_QUEUE; i++) {
            if(s->queue[i] == NULL) {
                s->queue[i] = b;
                break;
            }
        }

	//s->holder = b;
	assert(i < MAX_QUEUE);
    } else {
        // Decrement and continue
      //printf("%d chips: continuing\n", disp_get_domain_id());
        s->value--;
        struct ns_reply_state* ns = NULL;
        err = new_ns_reply(&ns, sem_wait_reply);
        assert(err_is_ok(err));
        ns->rpc_reply(b, ns);
    }
}

static void sem_trywait_reply(struct nameservice_binding *b,
        struct ns_reply_state* ns)
{
    errval_t err;
    err = b->tx_vtbl.sem_trywait_response(b, MKCONT(free, ns), (bool)ns->semval);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, ns);
            return;
        }
        USER_PANIC_ERR(err, "chips: sending %s failed!", __FUNCTION__);
    }
}

static void sem_trywait_handler(struct nameservice_binding *b, uint32_t sem)
{
    assert(sem < MAX_SEM);
    struct sem *s = &sems[sem];
    assert(s->allocated);
    errval_t err;
    bool success;

    if(s->value == 0) {
        success = false;
    } else {
        s->value--;
        success = true;
    }

    //printf("%d chips: trywait %u, %s\n", disp_get_domain_id(), sem, success ? "yes" : "no");
    struct ns_reply_state* ns = NULL;
    err = new_ns_reply(&ns, sem_trywait_reply);
    assert(err_is_ok(err));
    ns->semval = success;
    ns->rpc_reply(b, ns);
}

/**********/

static struct nameservice_rx_vtbl nameservice_rx_vtbl = {
    .register_service_call = register_service_handler,
    .unregister_service_call = unregister_service_handler,
    .get_service_reference_call = get_service_reference_handler,
    .wait_for_service_reference_call = wait_for_service_reference_handler,
    .get_service_call = get_service_handler,

    .get_cap_call = get_cap_handler,
    .put_cap_call = put_cap_handler,
    .remove_cap_call = remove_cap_handler,

    .sem_new_call = sem_new_handler,
    .sem_post_call = sem_post_handler,
    .sem_wait_call = sem_wait_handler,
    .sem_trywait_call = sem_trywait_handler,
};

static void export_handler(void *st, errval_t err, iref_t iref)
{
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice export failed!");
        abort();
    }

    trace_event(TRACE_SUBSYS_CHIPS, TRACE_EVENT_CHIPS_LISTENCB, 0);

    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_name_iref_request(mb, NOP_CONT, iref);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send set_name_iref_request to monitor");
        // XXX: cleanup
    }
}

static errval_t connect_handler(void *st, struct nameservice_binding *b)
{
	b->st = NULL;
    b->rx_vtbl = nameservice_rx_vtbl;
    return SYS_ERR_OK;
}

#ifdef CHIPS_TESTS_ENABLED
static void registry_tests(void)
{
    // put into the ht
    int r = registry->h.d.put_string((struct dictionary *) registry, "test", "value");
    assert(r == 0);
    assert(registry->h.entry_count == 1);

    // get from the ht
    char* val;
    uint8_t type = registry->h.d.get((struct dictionary *) registry, "test", (void**) &val);
    assert(val != NULL);
    assert(type == TYPE_STRING);
    assert(strcmp(val, "value") == 0);

    filter_t filter1 = create_filter("(|(a>=10)(test=value))");
    filter_t filter2 = create_filter("(test=foo)");


    print_filter(stderr, filter1);
    print_filter(stderr, filter2);
    assert(match_filter((struct dictionary *) registry, filter1) == 1);
    assert(match_filter((struct dictionary *) registry, filter2) == 0);

    destroy_filter(filter1);
    destroy_filter(filter2);

    r = registry->h.d.put_word((struct dictionary *) registry, "test2", 4711);
    assert(r == 0);
    assert(registry->h.entry_count == 2);

    uintptr_t val2;
    type = registry->h.d.get((struct dictionary *) registry, "test2", (void**) &val2);
    assert(type == TYPE_WORD);
    assert(val2 == 4711);

    r = registry->h.d.remove((struct dictionary *) registry, "test");
    assert(r == 0);

    type = registry->h.d.get((struct dictionary *) registry, "test", (void**) &val);
    assert(val == NULL);


    filter_t filter3 = create_filter("(|(a>=10)(test2<=5))");
    filter_t filter4 = create_filter("(&(test2=*)(test2>=5))");

    assert(match_filter((struct dictionary *) registry, filter3) == 0);
    assert(match_filter((struct dictionary *) registry, filter4) == 1);

    destroy_filter(filter3);
    destroy_filter(filter4);

    r = registry->h.d.remove((struct dictionary *) registry, "test2");
    assert(r == 0);

    type = registry->h.d.get((struct dictionary *) registry, "test2", (void**) &val);
    assert(val == NULL);
}
#endif

int main(int argc, char **argv)
{
    errval_t err;

    // create the service registry
    registry = create_multimap();
    assert(registry != NULL);

    // create the capability database
    capdb = create_hashtable();
    assert(capdb != NULL);

#ifdef CHIPS_TESTS_ENABLED
    registry_tests();
#endif

    err = nameservice_export(NULL, export_handler, connect_handler,
                             get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    messages_handler_loop();
    return EXIT_FAILURE;
}
