/**
 * \file
 * \brief octopus service handler header file.
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef OCTOPUS_SERVICE_H_
#define OCTOPUS_SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>
#include <octopus/definitions.h>

struct oct_reply_state;

struct skb_writer {
    char buffer[MAX_QUERY_LENGTH]; // TODO can be bigger than max query length...
    size_t length;
};

struct oct_query_state {
    struct skb_writer std_out;
    struct skb_writer std_err;
    int exec_res;
};

typedef void(*oct_reply_handler_fn)(struct octopus_binding*, struct oct_reply_state*);

struct oct_reply_state {
    struct octopus_binding* binding;
    oct_reply_handler_fn reply;

    struct oct_query_state query_state;
    bool return_record;
    errval_t error;

    // Pubsub / Trigger state
    uint64_t client_handler;
    uint64_t client_state;
    octopus_mode_t mode;
    octopus_trigger_id_t server_id;

    // For capability storage
    struct capref cap;

    struct oct_reply_state *next;
};

errval_t new_oct_reply_state(struct oct_reply_state**, oct_reply_handler_fn);

void get_names_handler(struct octopus_binding*, char*, octopus_trigger_t);
void get_handler(struct octopus_binding*, char*, octopus_trigger_t);
void set_handler(struct octopus_binding*, char*, uint64_t, octopus_trigger_t, bool);
void get_with_idcap_handler(struct octopus_binding*, struct capref,
                            octopus_trigger_t);
void set_with_idcap_handler(struct octopus_binding*, struct capref, char*,
                            uint64_t, octopus_trigger_t, bool);
void del_handler(struct octopus_binding*, char*, octopus_trigger_t);
void exists_handler(struct octopus_binding*, char*, octopus_trigger_t);
void wait_for_handler(struct octopus_binding*, char*);
void remove_trigger_handler(struct octopus_binding*, octopus_trigger_id_t);

void subscribe_handler(struct octopus_binding*, char*, uint64_t, uint64_t);
void publish_handler(struct octopus_binding*, char*);
void unsubscribe_handler(struct octopus_binding*, uint64_t);

void get_identifier(struct octopus_binding*);
void identify_binding(struct octopus_binding*, uint64_t, octopus_binding_type_t);

// Capability Storage
void get_cap_handler(struct octopus_binding*, char*);
void put_cap_handler(struct octopus_binding*, char*, struct capref);
void remove_cap_handler(struct octopus_binding*, char*);

#endif /* OCTOPUS_SERVICE_H_ */
