/**
 * \file
 * \brief Contains handler functions for server-side octopus interface RPC call.
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <if/octopus_defs.h>

#include <octopus_server/service.h>
#include <octopus_server/query.h>
#include <octopus_server/debug.h>

#include <octopus/parser/ast.h>
#include <octopus/definitions.h>

#include <bench/bench.h>

#include "queue.h"

/**
 * Name prefix used to by the functions set_with_idcap_handler() and
 * get_with_idcap_handler() to store and retrieve records by idcap.
 *
 * This essentially emulates a dedicated namespace for records stored with an
 * id cap. Octopus and the SKB do not support dedicated namespaces atm.
 * FIXME: store records set with the function 'set_with_idcap' in a dedicated
 *        namespace.
 */
#define IDCAPID_NAME_PREFIX "idcapid."

static uint64_t current_id = 1;

static inline errval_t check_query_length(char* query) {
    if (strlen(query) >= MAX_QUERY_LENGTH) {
        return OCT_ERR_QUERY_SIZE;
    }

    return SYS_ERR_OK;
}

errval_t new_oct_reply_state(struct oct_reply_state** drt,
        oct_reply_handler_fn reply_handler)
{
    assert(*drt == NULL);
    *drt = malloc(sizeof(struct oct_reply_state));
    if (*drt == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    //memset(*drt, 0, sizeof(struct oct_reply_state));
    (*drt)->query_state.std_out.buffer[0] = '\0';
    (*drt)->query_state.std_out.length = 0;
    (*drt)->query_state.std_err.buffer[0] = '\0';
    (*drt)->query_state.std_err.length = 0;

    (*drt)->binding = 0;
    (*drt)->return_record = false;
    (*drt)->error = 0;

    // For set_watch()
    (*drt)->mode = 0;
    (*drt)->client_state = 0;
    (*drt)->client_handler = 0;
    (*drt)->server_id = 0;

    (*drt)->reply = reply_handler;
    (*drt)->next = NULL;

    return SYS_ERR_OK;
}

static void free_oct_reply_state(void* arg)
{
    if (arg != NULL) {
        struct oct_reply_state* drt = (struct oct_reply_state*) arg;
        // In case we have to free things in oct_reply_state, free here...

        free(drt);
    } else {
        assert(!"free_reply_state with NULL argument?");
    }
}

static void trigger_send_handler(struct octopus_binding* b,
        struct oct_reply_state* drs)
{
    char* record = drs->query_state.std_out.buffer[0] != '\0' ?
            drs->query_state.std_out.buffer : NULL;

    errval_t err;
    err = b->tx_vtbl.trigger(b, MKCONT(free_oct_reply_state, drs),
            drs->server_id,
            drs->client_handler,
            drs->mode,
            record,
            drs->client_state);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

static inline bool can_install_trigger(octopus_trigger_t trigger, errval_t error)
{
    return trigger.m > 0 &&
           (trigger.in_case == err_no(error) ||
           (trigger.m & OCT_ALWAYS_SET) != 0 );
}

static inline uint64_t install_trigger(struct octopus_binding* binding,
        struct ast_object* ast, octopus_trigger_t trigger, errval_t error)
{
    errval_t err;
    uint64_t watch_id = 0;

    if (can_install_trigger(trigger, error)) {
        struct oct_reply_state* trigger_reply = NULL;
        err = new_oct_reply_state(&trigger_reply, trigger_send_handler);
        assert(err_is_ok(err));

        trigger_reply->client_handler = trigger.trigger;
        trigger_reply->client_state = trigger.st;

        trigger_reply->binding = (trigger.send_to == octopus_BINDING_RPC) ?
                binding : get_event_binding(binding);
        if (trigger_reply->binding == NULL) {
            fprintf(stderr, "No event binding for trigger, send events "
                            "over regular binding.");
            trigger_reply->binding = binding;
        }

        err = set_watch(binding, ast, trigger.m, trigger_reply, &watch_id);
        assert(err_is_ok(err));
    }

    return watch_id;
}

static void remove_trigger_reply(struct octopus_binding* b,
        struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.remove_trigger_response(b,
            MKCONT(free_oct_reply_state, drs),
            drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void remove_trigger_handler(struct octopus_binding *b, octopus_trigger_id_t tid)
{
    struct oct_reply_state* drs = NULL;
    errval_t err = new_oct_reply_state(&drs, remove_trigger_reply);
    assert(err_is_ok(err));

    drs->error = del_watch(b, tid, &drs->query_state);
    drs->reply(b, drs);
}

/*static inline void arrival_rate(void)
{
    static cycles_t measure_time = 10000;
    static uint64_t arrivals = 0;
    static cycles_t start = 0;
    arrivals++;
    if ( (arrivals % 100) == 0 && bench_tsc_to_ms(bench_tsc() - start) > measure_time) {
        printf("Get Rate per sec: %lu\n", arrivals / (measure_time / 1000));
        start = bench_tsc();
        arrivals = 0;
    }
}*/

static void get_reply(struct octopus_binding* b, struct oct_reply_state* drt)
{
    errval_t err;
    char* reply = err_is_ok(drt->error) ?
            drt->query_state.std_out.buffer : NULL;
    err = b->tx_vtbl.get_response(b, MKCONT(free_oct_reply_state, drt),
            reply, drt->server_id, drt->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drt);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void get_handler(struct octopus_binding *b, char *query, octopus_trigger_t trigger)
{
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;
    err = new_oct_reply_state(&drs, get_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        drs->server_id = install_trigger(b, ast, trigger, err);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void get_names_reply(struct octopus_binding* b,
        struct oct_reply_state* drt)
{
    errval_t err;
    char* reply = err_is_ok(drt->error) ?
            drt->query_state.std_out.buffer : NULL;
    err = b->tx_vtbl.get_names_response(b, MKCONT(free_oct_reply_state, drt),
            reply, drt->server_id, drt->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drt);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void get_names_handler(struct octopus_binding *b, char *query, octopus_trigger_t t)
{
    OCT_DEBUG(" get_names_handler: %s\n", query);

    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, get_names_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record_names(ast, &drs->query_state);
        drs->server_id = install_trigger(b, ast, t, err);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void set_reply(struct octopus_binding* b, struct oct_reply_state* drs)
{
    char* record = err_is_ok(drs->error) && drs->return_record ?
            drs->query_state.std_out.buffer : NULL;

    errval_t err;
    err = b->tx_vtbl.set_response(b, MKCONT(free_oct_reply_state, drs), record,
            drs->server_id, drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void set_handler(struct octopus_binding *b, char *query, uint64_t mode,
        octopus_trigger_t trigger, bool get)
{
    OCT_DEBUG(" set_handler: %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, set_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        if (ast->u.on.name->type == nodeType_Ident) {
            err = set_record(ast, mode, &drs->query_state);
            drs->server_id = install_trigger(b, ast, trigger, err);
        }
        else {
            // Since we don't have any ACLs atm. we do not
            // allow name to be a regex/variable, because
            // we it's not guaranteed which records get
            // modified in this case.
            err = OCT_ERR_NO_RECORD_NAME;
        }
    }

out:
    drs->error = err;
    drs->return_record = get;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static errval_t build_query_with_idcap(char **query_p, struct capref idcap,
                                       char *attributes)
{
    errval_t err;
    idcap_id_t id = 0;
    size_t query_size, bytes_written;

    // retrieve id from idcap
    err = invoke_idcap_identify(idcap, &id);
    if (err_is_fail(err)) {
        return err_push(err, OCT_ERR_IDCAP_INVOKE);
    }
    cap_delete(idcap);

    if (attributes == NULL) {
        attributes = "";
    }

    // build query using the idcapid and the attributes
    query_size = snprintf(NULL, 0, IDCAPID_NAME_PREFIX "%" PRIxIDCAPID "%s", id,
                          attributes);
    *query_p = (char *) malloc(query_size + 1); // include \0
    if (*query_p == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bytes_written = snprintf(*query_p, query_size + 1, IDCAPID_NAME_PREFIX
                             "%" PRIxIDCAPID "%s", id, attributes);

    return SYS_ERR_OK;
}

static void get_with_idcap_reply(struct octopus_binding *b,
                                 struct oct_reply_state *drt)
{
    errval_t err;
    char *reply = err_is_ok(drt->error) ?
                  drt->query_state.std_out.buffer : NULL;
    err = b->tx_vtbl.get_with_idcap_response(b,
                                             MKCONT(free_oct_reply_state, drt),
                                             reply, drt->server_id, drt->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drt);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void get_with_idcap_handler(struct octopus_binding *b, struct capref idcap,
                            octopus_trigger_t trigger)
{
    errval_t err;
    char *query = NULL;
    struct oct_reply_state *drs = NULL;
    struct ast_object *ast = NULL;

    err = build_query_with_idcap(&query, idcap, "");
    if (err_is_fail(err)) {
        goto out;
    }

    OCT_DEBUG("get_with_idcap_handler: %s\n", query);

    err = new_oct_reply_state(&drs, get_with_idcap_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        drs->server_id = install_trigger(b, ast, trigger, err);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    if (query != NULL) {
        free(query);
    }
}

static void set_with_idcap_reply(struct octopus_binding *b,
                                 struct oct_reply_state *drs)
{
    char *record = err_is_ok(drs->error) && drs->return_record ?
            drs->query_state.std_out.buffer : NULL;

    errval_t err;
    err = b->tx_vtbl.set_with_idcap_response(b,
                                             MKCONT(free_oct_reply_state, drs),
                                             record, drs->server_id,
                                             drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void set_with_idcap_handler(struct octopus_binding *b, struct capref idcap,
                            char *attributes, uint64_t mode,
                            octopus_trigger_t trigger, bool get)
{
    errval_t err;
    char *query = NULL;
    struct oct_reply_state *drs = NULL;
    struct ast_object *ast = NULL;

    err = new_oct_reply_state(&drs, set_with_idcap_reply);
    assert(err_is_ok(err));

    err = build_query_with_idcap(&query, idcap, attributes);
    if (err_is_fail(err)) {
        goto out;
    }
    OCT_DEBUG(" set_with_idcap_handler: %s\n", query);

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        if (ast->u.on.name->type == nodeType_Ident) {
            err = set_record(ast, mode, &drs->query_state);
            drs->server_id = install_trigger(b, ast, trigger, err);
        } else {
            err = OCT_ERR_NO_RECORD_NAME;
        }
    }

out:
    drs->error = err;
    drs->return_record = get;
    drs->reply(b, drs);

    free_ast(ast);
    free(attributes);
    if (query != NULL) {
        free(query);
    }

}

static void del_reply(struct octopus_binding* b, struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.del_response(b, MKCONT(free_oct_reply_state, drs),
            drs->server_id, drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void del_handler(struct octopus_binding* b, char* query, octopus_trigger_t trigger)
{
    OCT_DEBUG(" del_handler: %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, del_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        if (ast->u.on.name->type == nodeType_Ident) {
            err = del_record(ast, &drs->query_state);
            drs->server_id = install_trigger(b, ast, trigger, err);
        }
        else {
            // Since we don't have any ACLs atm. we do not
            // allow name to be a regex/variable
            // (see set_handler).
            err = OCT_ERR_NO_RECORD_NAME;
        }
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void exists_reply(struct octopus_binding* b, struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.exists_response(b, MKCONT(free_oct_reply_state, drs),
            drs->server_id, drs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void exists_handler(struct octopus_binding* b, char* query,
        octopus_trigger_t trigger)
{
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, exists_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        drs->server_id = install_trigger(b, ast, trigger, err);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void wait_for_reply(struct octopus_binding* b, struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.wait_for_response(b, MKCONT(free_oct_reply_state, drs),
            drs->query_state.std_out.buffer, drs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

// XXX: For compatibility reasons with nameserver API
void wait_for_handler(struct octopus_binding* b, char* query) {
    errval_t err = SYS_ERR_OK;
    errval_t set_watch_err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, wait_for_reply);
    drs->binding = b;
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        if (err_no(err) == OCT_ERR_NO_RECORD) {
            debug_printf("waiting for: %s\n", query);
            uint64_t wid;
            set_watch_err = set_watch(b, ast, OCT_ON_SET, drs, &wid);
        }
    }

out:
    if (err_no(err) != OCT_ERR_NO_RECORD || err_is_fail(set_watch_err)) {
        drs->error = err;
        if (err_is_fail(set_watch_err)) {
            // implies err = OCT_ERR_NO_RECORD
            drs->error = set_watch_err;
        }
        drs->reply(b, drs);
    }

    free_ast(ast);
    free(query);
}

static void subscribe_reply(struct octopus_binding* b,
        struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.subscribe_response(b, MKCONT(free_oct_reply_state, drs),
            drs->server_id, drs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void subscribe_handler(struct octopus_binding *b, char* query,
        uint64_t trigger_fn, uint64_t state)
{
    OCT_DEBUG("subscribe: query = %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_oct_reply_state(&drs, subscribe_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = add_subscription(b, ast, trigger_fn, state, drs);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void unsubscribe_reply(struct octopus_binding* b,
        struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.unsubscribe_response(b, MKCONT(free_oct_reply_state, drs),
            drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

static void send_subscribed_message(struct octopus_binding* b, struct oct_reply_state* drs)
{
    errval_t err = SYS_ERR_OK;
    char* record = drs->query_state.std_out.buffer[0] != '\0' ?
            drs->query_state.std_out.buffer : NULL;

    err = b->tx_vtbl.subscription(b, MKCONT(free_oct_reply_state, drs),
            drs->server_id, drs->client_handler,
            drs->mode, record, drs->client_state);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }

}

void unsubscribe_handler(struct octopus_binding *b, uint64_t id)
{
    errval_t err = SYS_ERR_OK;

    OCT_DEBUG("unsubscribe: id = %"PRIu64"\n", id);

    struct oct_reply_state* srs = NULL;
    err = new_oct_reply_state(&srs, unsubscribe_reply);
    assert(err_is_ok(err));

    err = del_subscription(b, id, &srs->query_state);
    if (err_is_ok(err)) {
        uint64_t binding;
        uint64_t client_handler;
        uint64_t client_state;
        uint64_t server_id;

        skb_read_output_at(srs->query_state.std_out.buffer,
                "subscriber(%"SCNu64", %"SCNu64", %"SCNu64", %"SCNu64")",
                &binding, &client_handler, &client_state, &server_id);

        struct oct_reply_state* subscriber = NULL;
        err = new_oct_reply_state(&subscriber,
                send_subscribed_message);
        assert(err_is_ok(err));

#if defined(__i386__) || defined(__arm__)
        subscriber->binding = (struct octopus_binding*)(uint32_t)binding;
#else
        subscriber->binding = (struct octopus_binding*)binding;
#endif
        subscriber->client_handler = client_handler;
        subscriber->client_state = client_state;
        subscriber->server_id = server_id;
        subscriber->mode = OCT_REMOVED;

        OCT_DEBUG("publish msg to: recipient:%"PRIu64" id:%"PRIu64"\n", binding, server_id);
        subscriber->reply(subscriber->binding, subscriber);
    }

    srs->error = err;
    srs->reply(b, srs);
}

static void publish_reply(struct octopus_binding* b, struct oct_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.publish_response(b, MKCONT(free_oct_reply_state, drs),
            drs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void publish_handler(struct octopus_binding *b, char* record)
{
    OCT_DEBUG("publish_handler query: %s\n", record);
    errval_t err = SYS_ERR_OK;

    struct oct_reply_state* drs = NULL;
    err = new_oct_reply_state(&drs, publish_reply);
    assert(err_is_ok(err));

    err = check_query_length(record);
    if (err_is_fail(err)) {
        drs->error = err;
        drs->reply(b, drs);
        goto out1;
    }

    struct ast_object* ast = NULL;
    err = generate_ast(record, &ast);
    if (err_is_fail(err)) {
        drs->error = err;
        drs->reply(b, drs);
        goto out2;
    }


    if (err_is_ok(err)) {
        err = find_subscribers(ast, &drs->query_state);
        if (err_is_ok(err)) {
            // Reply to publisher
            drs->error = err;
            drs->reply(b, drs);


            struct list_parser_status status;
            skb_read_list_init_offset(&status, drs->query_state.std_out.buffer, 0);

            // TODO remove skb list parser dependency
            // Send to all subscribers
            uint64_t binding;
            uint64_t client_handler;
            uint64_t client_state;
            uint64_t server_id;

            while (skb_read_list(&status, "subscriber(%"SCNu64", %"SCNu64", %"SCNu64", %"SCNu64")",
                    &binding, &client_handler, &client_state, &server_id)) {

                struct oct_reply_state* subscriber = NULL;
                err = new_oct_reply_state(&subscriber,
                        send_subscribed_message);
                assert(err_is_ok(err));
#if defined(__i386__) || defined(__arm__)
                subscriber->binding = (struct octopus_binding*)(uint32_t)binding;
#else
                subscriber->binding = (struct octopus_binding*)binding;
#endif
                subscriber->client_handler = client_handler;
                strcpy(subscriber->query_state.std_out.buffer, record);
                subscriber->client_state = client_state;
                subscriber->server_id = server_id;
                subscriber->mode = OCT_ON_PUBLISH;

                OCT_DEBUG("publish msg to: recipient:%"PRIu64" id:%"PRIu64"\n", binding, server_id);
                subscriber->reply(subscriber->binding, subscriber);
            }
        }
    }

out2:
    free_ast(ast);
out1:
    free(record);
}

void get_identifier(struct octopus_binding* b)
{
    errval_t err = b->tx_vtbl.get_identifier_response(b, NOP_CONT,
            current_id++);
    assert(err_is_ok(err));
}

static void identify_binding_reply(struct octopus_binding* b,
        struct oct_reply_state* drs)
{
    errval_t err;
    // TODO send drs->error back to client!
    err = b->tx_vtbl.identify_response(b, MKCONT(free_oct_reply_state, drs));
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            oct_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }

}

void identify_binding(struct octopus_binding* b, uint64_t id,
        octopus_binding_type_t type)
{
    assert(id <= current_id);

    struct oct_reply_state* drs = NULL;
    errval_t err = new_oct_reply_state(&drs, identify_binding_reply);
    assert(err_is_ok(err));

    OCT_DEBUG("set binding: id=%"PRIu64" type=%d\n", id, type);
    drs->error = set_binding(type, id, b);
    drs->reply(b, drs);
}

