/** \file
 * \brief SKB server prototypes
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef SKB_SERVER_H_
#define SKB_SERVER_H_

#include <barrelfish/barrelfish.h>

#include <if/skb_defs.h>

#define POST_EXECUTE 1

void skb_server_init(void);
void post_and_execute_string(void);


#define BUFFER_SIZE (32 * 1024)


struct skb_query_state {
    char output_buffer[BUFFER_SIZE];
    char error_buffer[BUFFER_SIZE];
    int output_length;
    int error_output_length;
    int exec_res;
};

struct skb_reply_state;

typedef void(*rpc_reply_handler_fn)(struct skb_binding*, struct skb_reply_state*);
//typedef void(*event_send_handler_fn)(struct skb_events_binding*, struct skb_reply_state*);

struct skb_reply_state {
	struct skb_query_state skb;
	rpc_reply_handler_fn rpc_reply;
	errval_t error;

	struct skb_reply_state *next;
};

errval_t new_reply_state(struct skb_reply_state**, rpc_reply_handler_fn);
void free_reply_state(void*);
errval_t execute_query(char*, struct skb_query_state*);

#endif
