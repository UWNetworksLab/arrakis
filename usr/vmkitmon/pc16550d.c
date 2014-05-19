/**
 * \file
 * \brief Virtual 16550 UART controller.
 *
 * This represents a simple implementation of a 16550 uart controller. It
 * neglects all timing issues, tranfer rate and error conditions. It declines to
 * know about FIFOs (which make it to a 16450 controller in fact). In addition
 * we are almost agnostic to the MCR and MSR register.
 *
 * The virtual UART controller can be attached to a host UART driver.
 */

/*
 * Copyright (c) 2009, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/monitor_defs.h>
#include <if/serial_defs.h>

#include <stdlib.h>

#include "vmkitmon.h"
#include "pc16550d.h"

#define FIFO_POS(x)     ((x) & PC16550D_FIFO_MASK)

struct pc16550d *
pc16550d_new (uint16_t base_port, uint8_t irq, struct lpc *lpc)
{
    assert(lpc != NULL);

    struct pc16550d *u = calloc(1, sizeof(struct pc16550d));

    pc16550d_mem_initialize(&u->dev, (mackerel_addr_t)u->regs);

    u->base_port = base_port;
    u->irq = irq;
    u->lpc = lpc;
    u->forward_state = PC16550d_FORWARD_NONE;

    // initialize the LSR register to have an empty transmit buffer
    pc16550d_mem_lsr_thre_wrf(&u->dev, 1);
    pc16550d_mem_lsr_temt_wrf(&u->dev, 1);

    return u;
}

/**
 * \brief Check all necessary conditions to raise a certain irq and does so.
 *
 * This method must be called at the end of every function returning back to
 * the guest otherwise some interrupts may get lost.
 */
static inline void
process_interrupt_conditions (struct pc16550d *u)
{
    // cycle through the sources for interrupts in the order of their priority
    // check whethter they are enabled and pending and raise them accordingly

    // data overrun
    if (pc16550d_mem_ier_rd(&u->dev).elsi &&
        pc16550d_mem_lsr_rd(&u->dev).oe) {

        pc16550d_mem_iir_iid_wrf(&u->dev, pc16550d_mem_irq_rls);
    }
    // receiver data available
    else if (pc16550d_mem_ier_rd(&u->dev).erbfi &&
             pc16550d_mem_lsr_rd(&u->dev).dr) {
        pc16550d_mem_iir_iid_wrf(&u->dev, pc16550d_mem_irq_rda);
    }
    // TODO: Here we need the timeout interrupt
    // transmitter holding register emtpy
    else if (pc16550d_mem_ier_rd(&u->dev).etbei &&
             pc16550d_mem_lsr_rd(&u->dev).thre) {
        pc16550d_mem_iir_iid_wrf(&u->dev, pc16550d_mem_irq_thre);
    }
    // no interrupt condition available
    else {
        pc16550d_mem_iir_iid_wrf(&u->dev, pc16550d_mem_irq_none);
    }

    // if there is an intr pending then inform the PIC accordingly
    if (pc16550d_mem_iir_rd(&u->dev).iid != pc16550d_mem_irq_none) {
        lpc_pic_assert_irq(u->lpc, u->irq);
    }
}

/* this method clears the current pending interrupt
 * to eventually raise the next interrupt */
static inline void
clear_interrupt (struct pc16550d *u)
{
    pc16550d_mem_iir_iid_wrf(&u->dev, pc16550d_mem_irq_none);
}

static inline void
process_lsr_change (struct pc16550d *u)
{
    // for now we only process the interrupts
}

static inline void
output_handler (struct pc16550d *u, char c)
{
    switch (u->forward_state) {
        case PC16550d_FORWARD_NONE:
            break;

        case PC16550d_FORWARD_UART:
            {
                errval_t err;
                struct pc16550d_forward_uart *state = u->forward_uart_state;

                err = state->binding->tx_vtbl.output(state->binding, NOP_CONT,
                                                     &c, 1);
                assert(err_is_ok(err));

                // Wait until character is sent.
                while (!state->binding->can_send(state->binding)) {
                    err = event_dispatch(state->ws);
                    assert(err_is_ok(err));
                }
            }
            break;

        default:
            assert(!"NYI");
    }
}

static inline void
process_thr_change (struct pc16550d *u)
{
    // put out the character
    char chr = pc16550d_mem_thr_rd_raw(&u->dev);
    output_handler(u, chr);

    // writing the THR reg resets the THRE interrupt pending state
    if (pc16550d_mem_iir_rd(&u->dev).iid == pc16550d_mem_irq_thre) {
        clear_interrupt(u);
    }

    // we always simulate the transmitter register to be empty (an infinitly
    // fast serial line)
    pc16550d_mem_lsr_thre_wrf(&u->dev, 1);
    pc16550d_mem_lsr_temt_wrf(&u->dev, 1);

    process_lsr_change(u);
}

static inline void
process_fcr_change (struct pc16550d *u)
{
    if (pc16550d_mem_fcr_rd(&u->dev).rfifo_reset) {
        u->fifo_in_produced = u->fifo_in_consumed = 0;
        pc16550d_mem_lsr_dr_wrf(&u->dev, 0);
        pc16550d_mem_lsr_oe_wrf(&u->dev, 0);
        process_lsr_change(u);
    }
}

int
pc16550d_handle_pio_read (struct pc16550d *u, uint16_t port,
                          enum opsize size, uint32_t *val)
{
    assert(u != NULL);
    assert(port >= u->base_port);

    port -= u->base_port;

    switch (port) {
    case 0:
        if (pc16550d_mem_lcr_rd(&u->dev).dlab) {
            // DL(L) read
            switch (size) {
            case OPSIZE_8:
                *val = pc16550d_mem_dll_rd_raw(&u->dev);
                break;
            default:
                *val = pc16550d_mem_dl_rd_raw(&u->dev);
                break;
            }
        } else {
            // RBR read
            if (FIFO_POS(u->fifo_in_produced) ==
                FIFO_POS(u->fifo_in_consumed)) {
                *val = 0;
            } else {
                *val = u->fifo_in[FIFO_POS(u->fifo_in_consumed)];
                u->fifo_in_consumed++;
            }
            // reset the data ready bit
            if (FIFO_POS(u->fifo_in_produced) ==
                FIFO_POS(u->fifo_in_consumed)) {
                pc16550d_mem_lsr_dr_wrf(&u->dev, 0);
                process_lsr_change(u);
            }
        }
        break;
    case 1:
        if (pc16550d_mem_lcr_rd(&u->dev).dlab) {
            // DLM read
            *val = pc16550d_mem_dlm_rd_raw(&u->dev);
        } else {
            // IER READ
            *val = pc16550d_mem_ier_rd_raw(&u->dev);
        }
        break;
    case 2:
        // IIR read
        *val = pc16550d_mem_iir_rd_raw(&u->dev);
        // reading the IIR reg resets the THRE interrupt pending state
        // (after the read)
        if (pc16550d_mem_iir_rd(&u->dev).iid == pc16550d_mem_irq_thre) {
            clear_interrupt(u);
        }
        break;
    case 3:
        // LCR read
        *val = pc16550d_mem_lcr_rd_raw(&u->dev);
        break;
    case 4:
        // MCR read
        *val = pc16550d_mem_mcr_rd_raw(&u->dev);
        break;
    case 5:
        // LSR read
        *val = pc16550d_mem_lsr_rd_raw(&u->dev);
        // reset possible error conditions
        pc16550d_mem_lsr_oe_wrf(&u->dev, 0);
        process_lsr_change(u);
        break;
    case 6:
        // MSR read
        *val = pc16550d_mem_msr_rd_raw(&u->dev);
        break;
    case 7:
        // SCR read
        *val = pc16550d_mem_scr_rd_raw(&u->dev);
        break;
    default:
        assert(!"pc16550d: read access to unknown port");
        break;
    }

    // check whether interrupts shall be raised
    process_interrupt_conditions(u);

    return HANDLER_ERR_OK;
}

int
pc16550d_handle_pio_write (struct pc16550d *u, uint16_t port,
                           enum opsize size, uint32_t val)
{
    assert(u != NULL);
    assert(port >= u->base_port);

    port -= u->base_port;

    /* all registers which do no processing after they are written and do not
     * abort the application just ignore their content and store it in case it
     * is read by the user */

    switch (port) {
    case 0:
        if (pc16550d_mem_lcr_rd(&u->dev).dlab) {
            // DL(L) write
            switch (size) {
            case OPSIZE_8:
                pc16550d_mem_dll_wr_raw(&u->dev, val);
                break;
            default:
                pc16550d_mem_dl_wr_raw(&u->dev, val);
            }
            pc16550d_mem_thr_wr_raw(&u->dev, val);
        } else {
            // THR write
            pc16550d_mem_thr_wr_raw(&u->dev, val);
            process_thr_change(u);
        }
        break;
    case 1:
        if (pc16550d_mem_lcr_rd(&u->dev).dlab) {
            // DLM write
            pc16550d_mem_dlm_wr_raw(&u->dev, val);
        } else {
            // IER write
            pc16550d_mem_ier_wr_raw(&u->dev, val);
            /* this register only holds info necessary for other operations
             * therefore we do not need to do any processing */
        }
        break;
    case 2:
        // FCR write
        pc16550d_mem_fcr_wr_raw(&u->dev, val);
        process_fcr_change(u);
        break;
    case 3:
        // LCR write
        pc16550d_mem_lcr_wr_raw(&u->dev, val);
        break;
    case 4:
        // MCR write
        pc16550d_mem_mcr_wr_raw(&u->dev, val);
        break;
    case 5:
        // LSR write
        assert(!"LSR should not be written to");
        pc16550d_mem_lsr_wr_raw(&u->dev, val);
        process_lsr_change(u);
        break;
    case 6:
        assert(!"MSR should not be written to");
        // MSR write
        pc16550d_mem_msr_wr_raw(&u->dev, val);
        break;
    case 7:
        // SCR write
        pc16550d_mem_scr_wr_raw(&u->dev, val);
        break;
    default:
        assert(!"pc16550d: write access to unknown port");
        break;
    }

    // check whether interrupts shall be raised
    process_interrupt_conditions(u);

    return HANDLER_ERR_OK;
}

static void
input_handler (void *user_data, const char *str, size_t size)
{
    assert(user_data != NULL);

    struct pc16550d *u = user_data;

    if (size == 0) {
        return;
    }

    // copy the string into our fifo
    for (int i = 0; i < size; i++) {
        u->fifo_in[FIFO_POS(u->fifo_in_produced)] = str[i];
        u->fifo_in_produced++;

        // check for overrun
        if (FIFO_POS(u->fifo_in_produced) ==
            FIFO_POS(u->fifo_in_consumed)) {
            u->fifo_in_produced = u->fifo_in_consumed = 0;
            pc16550d_mem_lsr_oe_wrf(&u->dev, 1);
        }
    }

    // tell the user that data is available
    pc16550d_mem_lsr_dr_wrf(&u->dev, 1);

    // raise interrupts when necessary
    process_interrupt_conditions(u);
}

static void serial_input_handler(struct serial_binding *b, char *data,
                                 size_t size)
{
    struct pc16550d *state = b->st;

    input_handler(state, data, size);
}

static void serial_bind_cb(void *st, errval_t err, struct serial_binding *b)
{
    assert(err_is_ok(err));
    struct pc16550d *state = st;

    state->forward_uart_state->binding = b;
    state->forward_uart_state->connected = true;
    b->st = state;
    b->rx_vtbl.input = serial_input_handler;
}

/**
 * \brief Connect the virtual serial port (UART) to a physical serial port
 *        on the host.
 *
 * \param user_data PC16550d driver struct.
 * \param host_uart Name of the host uart driver.
 */
void
pc16550d_attach_to_host_uart (struct pc16550d *user_data, const char *host_uart)
{
    assert(user_data != NULL);
    assert(host_uart != NULL);

    errval_t err;
    iref_t iref;

    // Initialization
    struct pc16550d_forward_uart *state =
        malloc(sizeof(struct pc16550d_forward_uart));
    assert(state != NULL);
    state->connected = false;
    state->ws = get_default_waitset();

    // Adjust PC16550d state
    user_data->forward_state = PC16550d_FORWARD_UART;
    user_data->forward_uart_state = state;

    // Bind to uart driver
    err = nameservice_lookup(host_uart, &iref);
    assert(err_is_ok(err));
    err = serial_bind(iref, serial_bind_cb, user_data, state->ws,
                      IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    // Dispatch the monitor binding until the bind completes.
    struct monitor_binding *monitor_b = get_monitor_binding();
    struct waitset *monitor_ws = monitor_b->waitset;
    while (!state->connected) {
        err = event_dispatch(monitor_ws);
    }
}
