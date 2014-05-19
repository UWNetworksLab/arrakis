/**
 * \file
 * \brief Server part of the SKB
 *
 * This file exports the SKB functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

//turn on if you want to see everything which is executed on the SKB.
//this helps to create a (data) file which can be used to play on linux
//a sed script can filter these lines by checking the prefix

//#define PRINT_SKB_FILE

#define SKB_FILE_OUTPUT_PREFIX "SKB_FILE:"


#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <eclipse.h>
#include <include/skb_server.h>
#include <include/skb_debug.h>
#include <include/queue.h>

#include <skb/skb.h>


errval_t new_reply_state(struct skb_reply_state** srs, rpc_reply_handler_fn reply_handler)
{
	assert(*srs == NULL);
	*srs = malloc(sizeof(struct skb_reply_state));
	if(*srs == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}
	memset(*srs, 0, sizeof(struct skb_reply_state));

	(*srs)->rpc_reply = reply_handler;
	(*srs)->next = NULL;

	return SYS_ERR_OK;
}


void free_reply_state(void* arg) {
	if(arg != NULL) {
		struct skb_reply_state* srt = (struct skb_reply_state*) arg;
		free(srt);
	}
	else {
		assert(!"free_reply_state with NULL argument?");
	}
}


errval_t execute_query(char* query, struct skb_query_state* st)
{
	assert(query != NULL);
    assert(st != NULL);
	int res;

    ec_ref Start = ec_ref_create_newvar();

	st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;

    /* Processing */
    ec_post_string(query);
    // Flush manually
    ec_post_goal(ec_term(ec_did("flush",1), ec_long(1)));
    ec_post_goal(ec_term(ec_did("flush",1), ec_long(2)));

    while(st->exec_res == PFLUSHIO) {
        st->exec_res = ec_resume1(Start);

        res = 0;
        do {
            res = ec_queue_read(1, st->output_buffer + st->output_length,
                                BUFFER_SIZE - res);
            st->output_length += res;
        } while ((res != 0) && (st->output_length < BUFFER_SIZE));
        st->output_buffer[st->output_length] = 0;

        res = 0;
        do {
            res = ec_queue_read(2, st->error_buffer + st->error_output_length,
                                BUFFER_SIZE - res);
            st->error_output_length += res;
        } while ((res != 0) &&
                    (st->error_output_length < BUFFER_SIZE));

        st->error_buffer[st->error_output_length] = 0;
    }

    if(st->exec_res == PSUCCEED) {
        ec_cut_to_chp(Start);
        ec_resume();
    }

    ec_ref_destroy(Start);

    return SYS_ERR_OK;
}


static void run_reply(struct skb_binding* b, struct skb_reply_state* srt) {
    errval_t err;
    err = b->tx_vtbl.run_response(b, MKCONT(free_reply_state, srt),
			                      srt->skb.output_buffer,
			                      srt->skb.error_buffer,
			                      srt->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srt);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


static void run(struct skb_binding *b, char *query)
{
    //debug_printf("skb run: query = %s\n", query);
	struct skb_reply_state* srt = NULL;
	errval_t err = new_reply_state(&srt, run_reply);
	assert(err_is_ok(err)); // TODO

	err = execute_query(query, &srt->skb);
	assert(err_is_ok(err));

	/*
	debug_printf("skb output was: %s\n", srt->skb.output_buffer);
	debug_printf("skb error  was: %s\n", srt->skb.error_buffer);
	debug_printf("skb exec res: %d\n", srt->skb.exec_res);
	*/

    run_reply(b, srt);
	free(query);
}


static struct skb_rx_vtbl rx_vtbl = {
    .run_call = run,
};


static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    char buf[100];
    sprintf(buf, "add_object(skb, [val(iref, %"PRIu32")], []).", iref);

    struct skb_query_state* sqs = malloc(sizeof(struct skb_query_state));
    err = execute_query(buf, sqs);
    //debug_printf("sqs->res: %d sqs->error: %s\n", sqs->exec_res, sqs->error_buffer);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice register failed");
        abort();
    }

    free(sqs);
}


static errval_t connect_cb(void *st, struct skb_binding *b)
{
	// Set up continuation queue
    b->st = NULL;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}


void skb_server_init(void)
{
    errval_t err;
    err = skb_export(NULL, export_cb, connect_cb, get_default_waitset(),
                     IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}
