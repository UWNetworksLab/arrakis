/**
 * \file
 * \brief Semaphore API Implementation
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <octopus/init.h>
#include <octopus/getset.h>
#include <octopus/trigger.h>
#include <octopus/lock.h>
#include <octopus/semaphores.h>

#include "common.h"

static uint32_t get_next_id(void)
{
    uint64_t id = 0;
    char* lock_record = NULL;
    char* record = NULL;

    // Find a valid ID for our next semaphore

    // This lock makes sure that we don't
    // have concurrent access to sem.ids
    errval_t err = oct_lock("sem.lock", &lock_record);
    assert(err_is_ok(err));

    err = oct_get(&record, "sem.ids { current_id: _ }");
    if (err_is_ok(err)) {
        err = oct_read(record, "_ { current_id: %d }", &id);
        assert(err_is_ok(err));
    }
    else if (err_no(err) == OCT_ERR_NO_RECORD) {
        err = oct_set("sem.ids { current_id: 0 }");
        assert(err_is_ok(err));
    }
    else {
        assert(!"Should not happen.");
    }

    id += 1;

    err = oct_set("sem.ids { current_id: %lu }", id);
    assert(err_is_ok(err));

    err = oct_unlock(lock_record);
    free(lock_record);
    free(record);
    assert(err_is_ok(err));

    return id;
}

errval_t oct_sem_new(uint32_t* id, size_t value)
{
    // Find a valid ID for our next semaphore
    *id = get_next_id();
    //debug_printf("oct_sem_new id is: %d\n", *id);

    errval_t err = SYS_ERR_OK;
    for (size_t i=0; i < value; i++) {
        err = oct_sem_post(*id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return err;
}

errval_t oct_sem_post(uint32_t id)
{
    return oct_mset(SET_SEQUENTIAL, "sem.%"PRIu32". { sem: %"PRIu32" }", id, id);
}

errval_t oct_sem_wait(uint32_t id)
{
    errval_t err = SYS_ERR_OK;
    char* result = NULL;
    octopus_trigger_id_t tid;
    octopus_trigger_t t = oct_mktrigger(OCT_ERR_NO_RECORD,
            octopus_BINDING_RPC, OCT_ON_SET, NULL, NULL);
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();

    char query[100];
    snprintf(query, 99, "r'sem\\.%"PRIu32"\\.[0-9]+' { sem: %"PRIu32" }", id, id);

    char lock_name[100];
    snprintf(lock_name, 99, "sem.%"PRIu32"", id);

    // XXX: The current implementation suffers from a herd effect,
    // may be worth it to use locks for this critical section
    while (1) {
        cl->call_seq.get(cl, query, t, &result, &tid, &err);

        if (err_is_ok(err)) {
            errval_t del_err = oct_del(result);
            free(result);
            result = NULL;

            if (err_is_ok(del_err)) {
                break; // Decreased successfully
            }
            else if (err_no(del_err) == OCT_ERR_NO_RECORD) {
                continue; // Need to start over
            }
            else {
                err = del_err;
                break; // Unexpected error
            }
        }
        else if (err_no(err) == OCT_ERR_NO_RECORD) {
            // No record found, wait until one is posted
            char* trigger_result = NULL;
            uint64_t fn, mode, state;
            cl->recv.trigger(cl, &tid, &fn, &mode, &trigger_result, &state);
            free(trigger_result);
        }
        else {
            break; // Unexpected error
        }
    }

    free(result);
    return err;
}

errval_t oct_sem_trywait(uint32_t id)
{
    errval_t err = SYS_ERR_OK;

    char* result = NULL;

    err = oct_get(&result, "r'sem\\.%d\\.[0-9]+' { sem: %d }", id, id);
    if (err_is_ok(err)) {
        err = oct_del(result);
    }
    else if (err_no(err) == OCT_ERR_NO_RECORD) {
        // Return with no record error to caller
    }

    free(result);
    return err;

}
