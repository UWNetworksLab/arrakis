/**
 * \file
 * \brief Test RPC calls with triggers in octopus
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <skb/skb.h>
#include <octopus/octopus.h>

#include "common.h"

static void trigger_handler(octopus_mode_t m, char* record, void* state)
{
    size_t* received = (size_t*) state;
    *received = *received + 1;
    debug_printf("trigger_handler got: %s\n", record);

    assert(m & OCT_ON_DEL);
    assert(m & OCT_REMOVED);
    free(record);
}

static void persistent_trigger(octopus_mode_t m, char* record, void* state) {
    size_t* received = (size_t*) state;
    *received = *received + 1;

    if (m & OCT_ON_SET) {
        debug_printf("persistent_trigger ON SET: %s\n", record);
    }
    if (m & OCT_ON_DEL) {
        debug_printf("persistent_trigger ON DEL: %s\n", record);
    }
    if (m & OCT_REMOVED) {
        debug_printf("persistent trigger CLEANUP: %s\n", record);
        assert(record == NULL);
    }

    free(record);
}

int main(int argc, char *argv[])
{
    oct_init();
    errval_t err = SYS_ERR_OK;
    octopus_trigger_id_t tid;
    size_t received = 0;

    err = oct_set("obj1 { attr: 1 }");
    ASSERT_ERR_OK(err);
    err = oct_set("obj2 { attr: 2 }");
    ASSERT_ERR_OK(err);
    err = oct_set("obj3 { attr: 3 }");
    ASSERT_ERR_OK(err);

    struct octopus_thc_client_binding_t* c = oct_get_thc_client();

    octopus_trigger_t record_deleted = oct_mktrigger(SYS_ERR_OK,
            octopus_BINDING_EVENT, OCT_ON_DEL, trigger_handler, &received);

    errval_t error_code = SYS_ERR_OK;
    char* output = NULL;
    err = c->call_seq.get(c, "r'^obj.$' { attr: 3 } ", record_deleted, &output,
            &tid, &error_code);
    ASSERT_ERR_OK(err);
    ASSERT_ERR_OK(error_code);
    ASSERT_STRING(output, "obj3 { attr: 3 }");
    debug_printf("tid is: %lu\n", tid);
    free(output);

    oct_del("obj3");
    while (received != 1) {
        messages_wait_and_handle_next();
    }

    received = 0;
    tid = 0;
    octopus_mode_t m = OCT_ON_SET | OCT_ON_DEL | OCT_PERSIST;
    octopus_trigger_t ptrigger = oct_mktrigger(SYS_ERR_OK,
            octopus_BINDING_EVENT, m, persistent_trigger, &received);
    output = NULL;
    err = c->call_seq.get(c, "obj2", ptrigger, &output,
            &tid, &error_code);
    ASSERT_ERR_OK(err);
    ASSERT_ERR_OK(error_code);
    debug_printf("tid is: %lu\n", tid);
    ASSERT_STRING(output, "obj2 { attr: 2 }");

    oct_del("obj2");
    while (received != 1) {
        messages_wait_and_handle_next();
    }

    received = 0;
    oct_set("obj2 { attr: 'asdf' }");
    while (received != 1) {
        messages_wait_and_handle_next();
    }

    received = 0;
    err = oct_remove_trigger(tid);
    DEBUG_ERR(err, "remove trigger");
    ASSERT_ERR_OK(err);
    while (received != 1) {
        messages_wait_and_handle_next();
    }

    printf("d2trigger SUCCESS!\n");
    return EXIT_SUCCESS;
}
