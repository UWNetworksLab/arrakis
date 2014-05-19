/**
 * \file
 * \brief Legacy keyboard and mouse (i8042) driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

#include <dev/lpc_kbd_dev.h>
#include "lpc_kbd.h"

#define KEYBOARD_IRQ    1
#define MOUSE_IRQ       12
#define IOPORT_BASE     0x60
#define IOPORT_MAX      0x64

/// The keyboard
static struct lpc_kbd_t kbd;
static bool init_complete;

static void handle_input(lpc_kbd_status_t st)
{
    assert(lpc_kbd_status_obf_extract(st));

    uint8_t val = lpc_kbd_input_rd(&kbd);

    if (lpc_kbd_status_aobf_extract(st)) {
        // we have mouse data
        mouse_data(val);
    } else {
        // we have keyboard data
        static bool extended;
        if (val == 0xe0) {
            assert(!extended);
            extended = true;
        } else {
            key_event(val, extended);
            extended = false;
        }
    }
}

static void interrupt_handler(void *arg)
{
    // ignore interrupts while in initialisation
    if (!init_complete) {
        return;
    }

    // read status register
    lpc_kbd_status_t st = lpc_kbd_status_rd(&kbd);
    if (lpc_kbd_status_obf_extract(st)) {
        handle_input(st);
    } else {
        // debug_printf("we took an interrupt, but there's nothing to read?\n");
    }
}

static void send_command(lpc_kbd_cmd_t cmd)
{
    // ensure input buffer and output buffer are empty
    lpc_kbd_status_t st;
    int nloop = 0;

    do {
        st = lpc_kbd_status_rd(&kbd);
        if (lpc_kbd_status_obf_extract(st)) {
            handle_input(st);
        }
        if (++nloop == 1000) {
            debug_printf("stuck in send_command: obf=%u aobf=%u ibf=%u\n",
                         lpc_kbd_status_obf_extract(st),
                         lpc_kbd_status_aobf_extract(st),
                         lpc_kbd_status_ibf_extract(st));
        }
    } while (lpc_kbd_status_obf_extract(st) || lpc_kbd_status_ibf_extract(st));

    // send the command
    lpc_kbd_command_wr(&kbd, cmd);
}

static void send_data(uint8_t val)
{
    // ensure input buffer and output buffer are empty
    lpc_kbd_status_t st;
    do {
        st = lpc_kbd_status_rd(&kbd);
    } while (lpc_kbd_status_obf_extract(st) || lpc_kbd_status_ibf_extract(st));

    lpc_kbd_output_wr(&kbd, val);
}

static void init(void)
{
    lpc_kbd_status_t st;

    if (init_complete) {
        return;
    }

    // init mackerel state
    lpc_kbd_initialize(&kbd, IOPORT_BASE);

    // our ultimate goal here is merely to enable mouse and keyboard interrupts
    // to achieve this, however, we do a little dance with the 8042 controller

    // disable keyboard and mouse, since we share the channel, and don't
    // want device data misinterpreted as the command byte
    send_command(lpc_kbd_kbd_disable);
    send_command(lpc_kbd_aux_disable);

    // issue command to read command byte
    send_command(lpc_kbd_rd_ccmd);

    // wait for the buffer to fill
    do {
        st = lpc_kbd_status_rd(&kbd);
    } while(!lpc_kbd_status_obf_extract(st));
    // XXX: we are supposed to wait for 7us before reading
    lpc_kbd_ccmd_t ccmd = lpc_kbd_input_rd(&kbd);

    // check that we really succeeded in disabling things beforehand
    assert(lpc_kbd_ccmd_kbd_dis_extract(ccmd));
    assert(lpc_kbd_ccmd_aux_dis_extract(ccmd));

    // enable both devices and both interrupts, and enable translation
    ccmd = lpc_kbd_ccmd_kbd_dis_insert(ccmd, 0);
    ccmd = lpc_kbd_ccmd_kbd_int_insert(ccmd, 1);
    ccmd = lpc_kbd_ccmd_aux_dis_insert(ccmd, 0);
    ccmd = lpc_kbd_ccmd_aux_int_insert(ccmd, 1);
    ccmd = lpc_kbd_ccmd_kbd_xl_insert(ccmd, 1);

    // write back the new command byte
    send_command(lpc_kbd_wr_ccmd);
    send_data(ccmd);

    init_complete = true;

    mouse_init();
}

void send_mouse_cmd(uint8_t cmd)
{
    assert(init_complete);

    // issue command to write to aux port
    send_command(lpc_kbd_write_aux);

    // send the actual command
    send_data(cmd);
}

int drv_init(void)
{
    int r = pci_client_connect();
    assert(r == 0); // XXX

    r = pci_register_legacy_driver_irq(init, IOPORT_BASE, IOPORT_MAX,
                                       KEYBOARD_IRQ, interrupt_handler, NULL);
    if (r != 0) {
        return r;
    }

    r = pci_register_legacy_driver_irq(init, IOPORT_BASE, IOPORT_MAX,
                                       MOUSE_IRQ, interrupt_handler, NULL);
    if (r != 0) {
        return r;
    }

    return 0;
}
