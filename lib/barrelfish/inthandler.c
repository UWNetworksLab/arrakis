/**
 * \file User-level interrupt handler support
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>
#include <if/monitor_blocking_rpcclient_defs.h>

struct waitset *barrelfish_interrupt_waitset = NULL;

/* allocate inrq */
static errval_t arm_allocirq(struct capref ep, uint32_t irq)
{
    errval_t err, msgerr;

    struct monitor_blocking_rpc_client *r = get_monitor_blocking_rpc_client();
    err = r->vtbl.arm_irq_handle(r, ep, irq, &msgerr);
    if (err_is_fail(err)){
        return err;
    } else {
        return msgerr;
    }
}

/* Allocate vector from local monitor */
static errval_t allocirq(struct capref ep, uint32_t *retvector)
{
    errval_t err, msgerr;
    uint32_t vector;

    struct monitor_blocking_rpc_client *r = get_monitor_blocking_rpc_client();
    err = r->vtbl.irq_handle(r, ep, &msgerr, &vector);
    if (err_is_fail(err)){
        return err;
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    } else {
        *retvector = vector;
        return msgerr;
    }
}



struct interrupt_handler_state {
    struct lmp_endpoint *idcep;
    interrupt_handler_fn handler;
    void *handler_arg;
};

static void generic_interrupt_handler(void *arg)
{
    struct interrupt_handler_state *state = arg;
    errval_t err;

    // consume message
    struct lmp_recv_buf buf = { .buflen = 0 };
    err = lmp_endpoint_recv(state->idcep, &buf, NULL);
    assert(err_is_ok(err));

    // run real handler
    //if (init_complete) {
    state->handler(state->handler_arg);

    // re-register
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = arg,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    assert(err_is_ok(err));
}


/**
 * \brief Setup an interrupt handler function to receive device interrupts
 *        on the ARM platform
 *
 * \param handler Handler function
 * \param handler_arg Argument passed to #handler
 * \param irq the IRQ number to activate
 */
errval_t inthandler_setup_arm(interrupt_handler_fn handler, void *handler_arg,
        uint32_t irq)
{
    errval_t err;

    /* alloc state */
    struct interrupt_handler_state *state;
    state = malloc(sizeof(struct interrupt_handler_state));
    assert(state != NULL);

    state->handler = handler;
    state->handler_arg = handler_arg;

    /* create endpoint to handle interrupts */
    struct capref epcap;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &state->idcep);
    if (err_is_fail(err)) {
        free(state);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // allocate a local interrupt vector for this endpoint
    err = arm_allocirq(epcap, irq);
    if (err_is_fail(err)) {
        return err;
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = state,
    };
    err = lmp_endpoint_register(state->idcep, get_default_waitset(), cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(state->idcep);
        // TODO: release vector
        free(state);
        return err_push(err, LIB_ERR_LMP_ENDPOINT_REGISTER);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Setup an interrupt handler function to receive device interrupts
 *
 * \param handler Handler function
 * \param handler_arg Argument passed to #handler
 * \param ret_vector On success, returns interrupt vector with which
 *                   handler is associated
 */
errval_t inthandler_setup(interrupt_handler_fn handler, void *handler_arg,
                          uint32_t *ret_vector)
{
    errval_t err;

    if(barrelfish_interrupt_waitset == NULL) {
        barrelfish_interrupt_waitset = get_default_waitset();
    }

    /* alloc state */
    struct interrupt_handler_state *state;
    state = malloc(sizeof(struct interrupt_handler_state));
    assert(state != NULL);

    state->handler = handler;
    state->handler_arg = handler_arg;

    /* create endpoint to handle interrupts */
    struct capref epcap;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &state->idcep);
    if (err_is_fail(err)) {
        free(state);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // allocate a local interrupt vector for this endpoint
    err = allocirq(epcap, ret_vector);
    if (err_is_fail(err)) {
        return err;
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = state,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(state->idcep);
        // TODO: release vector
        free(state);
        return err_push(err, LIB_ERR_LMP_ENDPOINT_REGISTER);
    }

    return SYS_ERR_OK;
}
