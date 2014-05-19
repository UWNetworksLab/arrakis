/**
 * \file
 * \brief octopus Query Interface Header file
 *
 * The server must implement this interface in order for dist2
 * to work accordingly.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_QUERY_H_
#define OCTOPUS_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>

#include <octopus_server/service.h>
#include <octopus/parser/ast.h>

/**
 * \brief Stores a binding for the given id.
 *
 * Used to find the event binding for a given RPC connection
 * since in a regular flounder interface RPC calls are on on a
 * different waitset we currently need two binding.
 *
 * \param type Binding type (RPC or Event)
 * \param id Identifier unique per client
 * \param binding Pointer value of binding
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_UNSUPPORTED_BINDING
 * \retval OCT_ERR_ENGINE_FAIL
 * \retval LIB_ERR_MALLOC_FAIL
 */
errval_t set_binding(octopus_binding_type_t type, uint64_t id, void* binding);

/**
 * Given a query returns a number of record names matching
 * the query. The record names are stored as a comma separated string
 * in oct_query_state.
 *
 * \param ast Abstract Syntax Tree of query.
 * \param dqs Contains the result of the query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t get_record_names(struct ast_object* ast, struct oct_query_state* dqs);

/**
 * \brief Returns a record matching the given query.
 *
 * \param ast Query supplied by client converted to AST.
 * \param dqs Contains the result of the query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t get_record(struct ast_object* ast, struct oct_query_state* dqs);

/**
 * \brief Stores a record in the database.
 *
 * \param ast Record to set.
 * \param mode A combination of modes as defined in getset.h.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t set_record(struct ast_object* ast, uint64_t mode,
        struct oct_query_state* dqs);

/**
 * \brief Deletes a record in the database.
 *
 * \param ast Record to delete.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t del_record(struct ast_object*, struct oct_query_state*);

/**
 * Sets a watch for a record(s) matching the given query. The Query Engine
 * is supposed to use the drs struct to reply to the client once the watch is
 * triggered.
 *
 * \param b RPC Binding
 * \param ast AST to watch for
 * \param mode When to trigger the watch (del or set).
 * \param drs Reply state used to reply to the client.
 * \param wid ID of the installed watch (used for remove).
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t set_watch(struct octopus_binding* b, struct ast_object* ast,
        uint64_t mode, struct oct_reply_state* drs, uint64_t* wid);

/**
 * \brief Removes a watch
 *
 * \param b Binding of caller
 * \param id Trigger Id supplied by caller
 * \param dqs Query state
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_INVALID_ID
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t del_watch(struct octopus_binding* b, octopus_trigger_id_t id,
        struct oct_query_state* dqs);

/**
 * \brief Adds a subscription.
 *
 * \param b RPC binding of subscriber.
 * \param ast Subscription template (to match with published records).
 * \param trigger_fn Client handler function.
 * \param state Additional state argument supplied by client.
 * \param drs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_MAX_SUBSCRIPTIONS
 * \retval OCT_ERR_ENGINE_FAIL
 * \retval LIB_ERR_MALLOC_FAIL
 */
errval_t add_subscription(struct octopus_binding* b, struct ast_object* ast,
        uint64_t trigger_fn, uint64_t state, struct oct_reply_state* drs);

/**
 * \brief Deletes a subscription for a given (Binding, Id) pair.
 *
 * \param b RPC binding of subscriber.
 * \param id ID of the subscription.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_SUBSCRIPTION
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t del_subscription(struct octopus_binding* b, uint64_t id,
        struct oct_query_state* dqs);

/**
 * Find all subscribers with a matching subscription for the given
 * AST.
 *
 * \param ast Record to match with stored subscription.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_SUBSCRIBERS
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t find_subscribers(struct ast_object* ast, struct oct_query_state* dqs);

/**
 * \brief Find the event binding of the client based on his RPC binding.
 *
 * \param binding RPC binding
 * \return Pointer of event binding or NULL on error.
 */
struct octopus_binding* get_event_binding(struct octopus_binding* binding);

#endif /* OCTOPUS_QUERY_H_ */
