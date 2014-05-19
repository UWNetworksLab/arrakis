/**
 * \file
 * \brief Publish/Subscribe client API implementation
 *
 * The individual handler functions are stored in a function table on the
 * client side. The API provides convenience functions for subscribe/
 * unsubscribe and publish.
 *
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>

#include <octopus/init.h>
#include <octopus/pubsub.h>

#include "common.h"
#include "handler.h"

void subscription_handler(struct octopus_binding *b, subscription_t id,
        uint64_t fn, octopus_mode_t mode, char *record,
        uint64_t st)
{

    // XXX: Probably send some offset around and use 32bit in flounder?
    // XXX: The casting to uintptr_t is for 32-bit archs
    subscription_handler_fn handler_fn = (subscription_handler_fn)(uintptr_t)fn;
    void* state = (void*)(uintptr_t)st;

    if (handler_fn != NULL) {
        handler_fn(mode, record, state);
    }
    else {
        fprintf(stderr, "Incoming subscription(%"PRIu64") for %s with unset handler function.",
                id, record);
        free(record);
    }
}

/**
 * \brief Subscribe for a given type of message.
 *
 * \param[in] function Handler function in case a matching record is
 * published.
 * \param[in] state State passed on to handler function.
 * \param[out] id Id of the subscription. In case of the value is undefined.
 * \param query What type of records you want to subscribe.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_MAX_SUBSCRIPTIONS
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_subscribe(subscription_handler_fn function, const void *state,
        subscription_t *id, const char *query, ...)
{
    assert(function != NULL);
    assert(query != NULL);
    assert(id != NULL);

    va_list args;
    errval_t err = SYS_ERR_OK;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // send to skb
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;

    uint64_t fl_function = 0;
    uint64_t fl_state = 0;

    fl_function = (uint64_t)(uintptr_t)function;
    fl_state = (uint64_t)(uintptr_t)state;

    err = cl->call_seq.subscribe(cl, buf, fl_function,
            fl_state, id, &error_code); // XXX: Sending Pointer as uint64
    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Unsubscribes a subscription.
 *
 * \param id Id of the subscription (as provided by oct_subscribe).
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_unsubscribe(subscription_t id)
{
    // send to skb
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    errval_t err = cl->call_seq.unsubscribe(cl, id, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    return err;
}

/**
 * \brief Publishes a record.
 *
 * \param record The record to publish.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_publish(const char *record, ...)
{
    assert(record != NULL);

    va_list args;
    errval_t err = SYS_ERR_OK;

    char *buf = NULL;
    FORMAT_QUERY(record, args, buf);

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    err = cl->call_seq.publish(cl, buf, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}
