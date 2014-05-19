/**
 * \file
 * \brief Benchmark get/set throughput.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <octopus/octopus.h>
#include <skb/skb.h>

/**
 * Usage: d2bench1 <#clients> <get|set>
 */
int main(int argc, char** argv)
{
	int clients = atoi(argv[1]);
	assert(clients > 0);

    oct_init();

    char payload[256] = { [0 ... 254] = 'a', [255] = '\0' };

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    octopus_trigger_id_t tid;
    assert(cl != NULL);

    char record[300];
    sprintf(record, "rec { attr: '%s' }", payload);
    printf("record is: %s\n", record);

    errval_t err = oct_set(record);
    assert(err_is_ok(err));

    char* barrier = NULL;
    err = oct_barrier_enter("d2bench1", &barrier, clients);
    assert(err_is_ok(err));

    char* reply;
    errval_t error_code;

    bool stopped = false;
    if (strcmp(argv[2], "get") == 0) {
		while ( !stopped ) {
			cl->call_seq.get(cl, "rec", NOP_TRIGGER, &reply, &tid, &error_code);
			free(reply);
			//DEBUG_ERR(error_code, "got record");
		}
    }
    else if (strcmp(argv[2], "set") == 0) {
		while ( !stopped ) {
			cl->call_seq.set(cl, record, SET_DEFAULT, NOP_TRIGGER, false, &reply, &tid, &error_code);
			//assert(reply == NULL);
			//DEBUG_ERR(error_code, "set record");
		}
    }
    else {
    	assert(!"Invalid argv[2]");
    }

    return EXIT_SUCCESS;
}
