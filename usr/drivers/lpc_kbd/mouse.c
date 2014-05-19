/**
 * \file
 * \brief Basic PS/2 mouse protocol driver
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
#include <barrelfish/barrelfish.h>

#include "lpc_kbd.h"

#if 0
#define DEBUG(x...) debug_printf(x)
#else
#define DEBUG(x...) (void)0
#endif

// subset of commands
#define CMD_ACK         0xfa ///< ACK *from the mouse* in response to a command
#define CMD_RESET       0xff ///< Reset the mouse
#define CMD_SETDEFAULTS 0xf6 ///< Apply default values
#define CMD_DISABLE     0xf5 ///< Disable data reporting
#define CMD_ENABLE      0xf4 ///< Enable data reporting
#define CMD_GETDEVID    0xf2 ///< Get device ID

// when the mouse is plugged in or resets itself, it sends these in sequence
#define RESET0          0xaa
#define RESET1          0x00

// state of data reception path
static enum {
    STATE_IDLE,         ///< Nothing doing
    STATE_PKT1,         ///< We saw the first byte of a 3- or 4-byte packet
    STATE_PKT2,         ///< We saw the first two bytes of a 3- or 4-byte packet
    STATE_PKT3,         ///< We saw the first three bytes of a 4-byte packet
    STATE_SEND_CMD,     ///< We sent a command, and we're waiting for the ack
} data_state;

// state of initialisation path (next level up)
static enum {
    INIT_STATE_STARTUP,         ///< Just started...
    INIT_STATE_SENT_RESET,      ///< Sent reset command
    INIT_STATE_SENT_DEFAULTS,   ///< Sent set defaults command
    INIT_STATE_SENT_ENABLE,     ///< Sent enable commant
    INIT_STATE_COMPLETE,        ///< Up and running
    INIT_STATE_SAW_RESET,       ///< Saw a reset packet
} init_state;

static bool sending_command;

static void init_data(uint8_t val);

static void handle_packet(uint8_t *buf, size_t len)
{
    assert(len == 3);

    // unpack packet (TODO: use Mackerel?)
    bool yoverflow = (buf[0] & 0x80) != 0;
    bool xoverflow = (buf[0] & 0x40) != 0;
    bool ysign     = (buf[0] & 0x20) != 0;
    bool xsign     = (buf[0] & 0x10) != 0;
    bool always1   = (buf[0] & 0x08) != 0;
    bool buttonm   = (buf[0] & 0x04) != 0;
    bool buttonr   = (buf[0] & 0x02) != 0;
    bool buttonl   = (buf[0] & 0x01) != 0;
    uint8_t uxdelta = buf[1];
    uint8_t uydelta = buf[2];

    if (!always1) {
        DEBUG("invalid packet, discarded\n");
        return;
    }

    if (yoverflow || xoverflow) {
        DEBUG("X or Y overflow set: probably bogus?\n");
        return;
    }

    int xdelta, ydelta;

    if (ysign) {
        ydelta = uydelta | (-1 & ~0xff);
    } else {
        ydelta = uydelta;
    }

    if (xsign) {
        xdelta = uxdelta | (-1 & ~0xff);
    } else {
        xdelta = uxdelta;
    }

    DEBUG("%dx%d L%u M%u R%u\n", xdelta, ydelta, buttonl, buttonm, buttonr);
    mouse_event(xdelta, ydelta, buttonl, buttonm, buttonr);
}

// this is called when we receive a byte from the mouse
void mouse_data(uint8_t val)
{
    static uint8_t buf[3];

    if (sending_command) {
        DEBUG("ignored 0x%x while sending command\n", val);
        return;
    }

    DEBUG("got 0x%x in state %d:%d\n", val, data_state, init_state);

    switch (data_state) {
    case STATE_IDLE:
        if (init_state == INIT_STATE_COMPLETE) {
            buf[0] = val;
            data_state++;
        }
        break;

    case STATE_PKT1:
        buf[1] = val;
        if (buf[0] == RESET0 && buf[1] == RESET1) {
            init_state = INIT_STATE_SAW_RESET;
            data_state = STATE_IDLE;
            init_data(0);
        } else {
            data_state++;
        }
        break;

    case STATE_PKT2:
        buf[2] = val;
        handle_packet(buf, sizeof(buf));
        data_state = STATE_IDLE;
        break;

    case STATE_SEND_CMD:
        if (val == CMD_ACK) {
            data_state = STATE_IDLE;
            init_data(val);
        } else {
            DEBUG("unexpected data %x while waiting for command ACK\n", val);
        }
        break;

    default:
        USER_PANIC("unhandled state %d\n", data_state);
    }
}

static void send_cmd(uint8_t cmd)
{
    DEBUG("sending 0x%x in state %d:%d\n", cmd, data_state, init_state);

    assert(data_state == STATE_IDLE);
    data_state = STATE_SEND_CMD;
    sending_command = true;
    send_mouse_cmd(cmd);
    sending_command = false;
}

static void init_data(uint8_t val)
{
    switch (init_state) {
    case INIT_STATE_STARTUP:
        send_cmd(CMD_RESET);
        init_state = INIT_STATE_SENT_RESET;
        break;

    case INIT_STATE_SENT_RESET:
        assert(val == CMD_ACK);
    case INIT_STATE_SAW_RESET:
        send_cmd(CMD_SETDEFAULTS);
        init_state = INIT_STATE_SENT_DEFAULTS;
        break;

    case INIT_STATE_SENT_DEFAULTS:
        assert(val == CMD_ACK);
        send_cmd(CMD_ENABLE);
        init_state = INIT_STATE_SENT_ENABLE;
        break;

    case INIT_STATE_SENT_ENABLE:
        assert(val == CMD_ACK);
        init_state = INIT_STATE_COMPLETE;
        break;

    case INIT_STATE_COMPLETE:
        USER_PANIC("unexpected data 0x%x in complete init state\n", val);
        break;
    }
}

// this is called when we initialise
void mouse_init(void)
{
    init_state = INIT_STATE_STARTUP;
    init_data(0);
}
