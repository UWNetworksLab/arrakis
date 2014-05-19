#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>

#include "kaluga.h"

errval_t trigger_existing_and_watch(const char* query,
        trigger_handler_fn event_handler, void* state,
        octopus_trigger_id_t* tid)
{
    errval_t error_code;
    char** names = NULL;
    char* output = NULL;
    char* record = NULL; // freed by cpu_change_event
    size_t len = 0;
    octopus_trigger_t t = oct_mktrigger(0, octopus_BINDING_EVENT,
            TRIGGER_ALWAYS, event_handler, state);

    // Get current cores registered in system
    struct octopus_thc_client_binding_t* rpc = oct_get_thc_client();
    errval_t err = rpc->call_seq.get_names(rpc, query,
            t, &output, tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

    switch(err_no(err)) {
    case SYS_ERR_OK:
        err = oct_parse_names(output, &names, &len);
        if (err_is_fail(err)) {
            goto out;
        }

        for (size_t i=0; i < len; i++) {
            KALUGA_DEBUG("get record for name:%s\n", names[i]);
            err = oct_get(&record, names[i]);

            switch (err_no(err)) {
            case SYS_ERR_OK:
                event_handler(OCT_ON_SET, record, state);
                break;

            case OCT_ERR_NO_RECORD:
                assert(record == NULL);
                break;

            default:
                DEBUG_ERR(err, "Unable to retrieve core record for %s", names[i]);
                assert(record == NULL);
                break;
            }
        }
        break;
    case OCT_ERR_NO_RECORD:
        err = SYS_ERR_OK; // Overwrite (trigger is set)
        break;

    default:
        // Do nothing (wait for trigger)
        break;
    }

out:
    oct_free_names(names, len);
    free(output);

    return err;
}
