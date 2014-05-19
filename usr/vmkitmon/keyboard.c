/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "vmkitmon.h"
#include "keyboard.h"
#include <stdlib.h>
#include <barrelfish/terminal.h>

struct key_map_entry {
    uint8_t ascii, scan_code;
};

// this table maps from input through the BF kernel to a key in the guest
// scan codes taken from http://heim.ifi.uio.no/~stanisls/helppc/scan_codes.html
static struct key_map_entry key_map[128] = {
    // capital letters
    ['A'] = { 0x41, 0x1E },
    ['B'] = { 0x42, 0x30 },
    ['C'] = { 0x43, 0x2E },
    ['D'] = { 0x44, 0x20 },
    ['E'] = { 0x45, 0x12 },
    ['F'] = { 0x46, 0x21 },
    ['G'] = { 0x47, 0x22 },
    ['H'] = { 0x48, 0x23 },
    ['I'] = { 0x49, 0x17 },
    ['J'] = { 0x4A, 0x24 },
    ['K'] = { 0x4B, 0x25 },
    ['L'] = { 0x4C, 0x26 },
    ['M'] = { 0x4D, 0x32 },
    ['N'] = { 0x4E, 0x31 },
    ['O'] = { 0x4F, 0x18 },
    ['P'] = { 0x50, 0x19 },
    ['Q'] = { 0x51, 0x10 },
    ['R'] = { 0x52, 0x13 },
    ['S'] = { 0x53, 0x1F },
    ['T'] = { 0x54, 0x14 },
    ['U'] = { 0x55, 0x16 },
    ['V'] = { 0x56, 0x2F },
    ['W'] = { 0x57, 0x11 },
    ['X'] = { 0x58, 0x2D },
    ['Y'] = { 0x59, 0x15 },
    ['Z'] = { 0x5A, 0x2C },
    // small letters
    ['a'] = { 0x61, 0x1E },
    ['b'] = { 0x62, 0x30 },
    ['c'] = { 0x63, 0x2E },
    ['d'] = { 0x64, 0x20 },
    ['e'] = { 0x65, 0x12 },
    ['f'] = { 0x66, 0x21 },
    ['g'] = { 0x67, 0x22 },
    ['h'] = { 0x68, 0x23 },
    ['i'] = { 0x69, 0x17 },
    ['j'] = { 0x6A, 0x24 },
    ['k'] = { 0x6B, 0x25 },
    ['l'] = { 0x6C, 0x26 },
    ['m'] = { 0x6D, 0x32 },
    ['n'] = { 0x6E, 0x31 },
    ['o'] = { 0x6F, 0x18 },
    ['p'] = { 0x70, 0x19 },
    ['q'] = { 0x71, 0x10 },
    ['r'] = { 0x72, 0x13 },
    ['s'] = { 0x73, 0x1F },
    ['t'] = { 0x74, 0x14 },
    ['u'] = { 0x75, 0x16 },
    ['v'] = { 0x76, 0x2F },
    ['w'] = { 0x77, 0x11 },
    ['x'] = { 0x78, 0x2D },
    ['y'] = { 0x79, 0x15 },
    ['z'] = { 0x7A, 0x2C },
    // control characters
    [0x08] = { 0x08, 0x0e },    // back-space
    [0x7f] = { 0x7f, 0x0e },    // delete
    [0x0d] = { 0x0d, 0x1c },    // enter
    [0x1b] = { 0x1b, 0x01 },    // escape
    [0x20] = { 0x20, 0x39 },    // space
    [0x09] = { 0x09, 0x0f }     // tab1
};

struct keyboard *
keyboard_new (struct guest *g)
{
    struct keyboard *ret = calloc(1, sizeof(struct keyboard));

    ret->guest = g;
    ret->status = KEYBOARD_STATUS_ENABLED;

    return ret;
}

static inline int
handle_60h_out (struct keyboard *k, uint32_t val)
{
    if (!(k->status & KEYBOARD_STATUS_IN_CMD)) {
        printf("keyboard: non command input\n");
        return -1;
    }

    switch (k->write_cmd) {
        // Write Output Port
        case 0xd1:
            /* ------------------------------------------------------------------
             * |KBD data|KBD clk|in buf empty|out buf empty|ud|ud|gate a20|reset|
             * ------------------------------------------------------------------
             */
            // check for gate a20
            if (val & 0x2) {
                k->guest->a20_gate_enabled = true;
            } else {
                k->guest->a20_gate_enabled = false;
            }
            // check for reset
            if (!(val & 0x1)) {
                assert(!"System reset requested");
            }
            break;
        default:
            printf("keyboard: Unhandeled keyboard command");
            return 1;
    }

    k->write_cmd = 0;
    k->status &= ~KEYBOARD_STATUS_IN_CMD;

    return 0;
}

int
keyboard_handle_pio_read (struct keyboard *k, uint16_t port,
                         enum opsize size, uint32_t *val)
{
    switch (port) {
    case 0x64:
        assert(size & OPSIZE_8);
        *val = k->status;
        return HANDLER_ERR_OK;
    }

    return -1;
}

int
keyboard_handle_pio_write (struct keyboard *k, uint16_t port,
                           enum opsize size, uint32_t val)
{
    switch (port) {
    case 0x60:
        assert(size & OPSIZE_8);
        return handle_60h_out(k, val);
    case 0x64:
        assert(size & OPSIZE_8);
        k->write_cmd = val;
        k->status |= KEYBOARD_STATUS_IN_CMD;
        return HANDLER_ERR_OK;
    }

    return -1;
}

/* waits until a character arrives in the monitor */
/* FIXME: perhaps it is not such a good idea to stall the whole monitor for
          this */
static int
handle_get_keystroke (struct keyboard *k, struct guest *g)
{
    int r;
    uint8_t chr;

    // loop for now here since we have too many unhandeled keys atm
    // FIXME: use a complete keymap to avoid this loop
    while (1) {
        // wait for input
        r = terminal_read((char*)&chr, 1);
        assert(r == 1);

        // check whether our keymap contains the entered key
        if (key_map[chr].scan_code != 0) {
            guest_set_al(g, key_map[chr].ascii);
            guest_set_ah(g, key_map[chr].scan_code);
            break;
        } else {
            printf("keyboard: Unhandeled input char with code %x\n", chr);
        }
    }

    return HANDLER_ERR_OK;
}

int
keyboard_handle_int16 (struct keyboard *k, struct guest *g) {
    switch (guest_get_ah(g)) {
        // KEYBOARD - GET KEYSTROKE
        case 0:
            return handle_get_keystroke(k, g);
        default:
            break;
    }

    return HANDLER_ERR_UNHANDELED;
}
