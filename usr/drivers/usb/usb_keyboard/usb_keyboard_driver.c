/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>
#include <usb/usb_device.h>
#include <usb/usb_request.h>
#include <usb/usb_transfer.h>
#include <usb/class/usb_hid.h>

#include "usb_keyboard_driver.h"
#include "usb_keyboard_service.h"

#include "usb_keyboard_keymap.h"

static struct usb_keyboard keyboard;

/*
 * setup information for the two transfer types needed
 */

static usb_transfer_setup_t keyboard_tconf[USB_KEYBOARD_NUM_TRANSFERS] = {
    [USB_KEYBOARD_DATA] = {
        .type = USB_TYPE_INTR,
        .interface = 0,
        .endpoint = USB_ENDPOINT_ADDRESS_ANY, /* any address */
        .direction = USB_ENDPOINT_DIRECTION_IN,
        .max_bytes = 0, /* use wMaxPacketSize */
        .flags = {
            .short_xfer_ok = 1,
            .pipe_on_falure = 1,
            .auto_restart = 1,
        },
    },
};

/**
 *
 */
static void usb_keyboard_put_key(uint32_t key)
{
    if (keyboard.input_size < USB_KEYBOARD_IN_BUFSIZE) {
        keyboard.input[keyboard.input_tail] = key;
        ++(keyboard.input_size);
        ++(keyboard.input_tail);
        if (keyboard.input_tail >= USB_KEYBOARD_IN_BUFSIZE) {
            keyboard.input_tail = 0;
        } USB_DEBUG_KBD("buffer: but key %x\n", key);
    } else {
        debug_printf("WARNING: input buffer is full\n");
    }
}

/**
 *
 */
static int32_t usb_keyboard_get_key(void)
{
    int32_t c;

    if (keyboard.input_size == 0) {
        /* start transfer, if not already started */
        //usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);
    }

    if (keyboard.input_size == 0) {
        c = -1;
        USB_DEBUG_KBD("buffer: return no key \n");
    } else {
        c = keyboard.input[keyboard.input_head];
        --(keyboard.input_size);
        ++(keyboard.input_head);
        if (keyboard.input_head >= USB_KEYBOARD_IN_BUFSIZE) {
            keyboard.input_head = 0;
        } USB_DEBUG_KBD("buffer: return key %x\n", c);
    }
    return (c);
}

static uint32_t usb_keyboard_keyaction(uint32_t keycode, uint32_t up)
{
    union usb_keyboard_modifiers *mods = &keyboard.state;

    uint32_t action;
    struct usb_key_map *key = &usb_keyboard_key_map[keycode];

    uint32_t state = ((mods->shift_l | mods->shift_r) ? 1 : 0)
            | ((mods->ctrl_l | mods->ctrl_r) ? 2 : 0)
            | ((mods->alt_l | mods->alt_r) ? 4 : 0);

    if (((key->flgs & 0x1) && mods->clock)
            || ((key->flgs & 0x02) && mods->nlock)) {
        state ^= 1;
    }

    if (up) {
        USB_DEBUG_KBD(
                "keyaction release: key = %x, %c\n", key->map[0], key->map[0]);
        action = keyboard.last_active[keycode];
        keyboard.last_active[keycode] = NOP;

        switch (action) {
            case LSH:
                mods->shift_l = 0;
                break;
            case RSH:
                mods->shift_r = 0;
                break;
            case RCTR:
                mods->ctrl_r = 0;
                break;
            case LCTR:
                mods->ctrl_l = 0;
                break;
            case LALT:
                mods->alt_l = 0;
                break;
            case RALT:
                mods->alt_r = 0;
                break;
            case NLK:
                //mods->nlock ^= 1;
                break;
            case SLK:
                //mods->slock ^= 1;
                break;
            case CLK:
                //mods->clock ^= 1;
                break;
            case NOP:
                /* release of normal key */
                return (USB_KEYBOARD_KEY_NOKEY);
        }
        return (SPCLKEY | RELKEY | action);
    } else {
        /* key press */
        action = key->map[state];
        USB_DEBUG_KBD(
                "keyaction release: key = [%x, %x], [%c, %c]\n", key->map[0], key->map[state], key->map[0], key->map[state]);

        if (key->spcl & (0x80 >> state)) {
            if (keyboard.last_active[keycode] == NOP) {
                keyboard.last_active[keycode] = action;
            }
            if (keyboard.last_active[keycode] != action) {
                action = NOP;
            }
            switch (action) {
                case NLK:
                    mods->nlock ^= 1;
                    break;
                case SLK:
                    mods->slock ^= 1;
                    break;
                case CLK:
                    mods->clock ^= 1;
                    break;
                case BTAB:
                    action |= 0x08000000; /* back tab */
                    break;
                case LSH:
                    mods->shift_l = 1;
                    break;
                case RSH:
                    mods->shift_r = 1;
                    break;
                case LCTR:
                    mods->ctrl_l = 1;
                    break;
                case RCTR:
                    mods->ctrl_r = 1;
                    break;
                case LALT:
                    mods->alt_l = 1;
                    break;
                case RALT:
                    mods->alt_r = 1;
                    break;
                case NOP:
                    return (USB_KEYBOARD_KEY_NOKEY);
                    break;
                default:
                    USB_DEBUG_KBD("accents...\n");
                    break;

            }
            return (0x80000000 | action);
        } else {
            keyboard.last_active[keycode] = NOP;
            return (action);
        }

    }

}

/**
 * \brief
 */
static void usb_keyboard_set_leds(void)
{

    struct usb_device_request req;

    req.bRequest = USB_HID_REQUEST_SET_REPORT;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.wValue = USB_HID_REPORT_OUTPUT << 8;

    req.wIndex = keyboard.usb_iface_number;

    memset(keyboard.buffer, 0, USB_KEYBOARD_BUFSIZE);

    uint8_t report_id = 0;

    if (keyboard.numlock.valid) {
        if (keyboard.state.nlock) {
            keyboard.keyboard_led_state.numlock = 1;
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.numlock.loc, 1);
        } else {
            keyboard.keyboard_led_state.numlock = 1;
        }
        report_id = keyboard.numlock.report_id;
    }

    if (keyboard.scrolllock.valid) {
        if (keyboard.state.slock) {
            keyboard.keyboard_led_state.scrolllock = 1;
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.scrolllock.loc, 1);
        } else {
            keyboard.keyboard_led_state.scrolllock = 1;
        }
        report_id = keyboard.scrolllock.report_id;
    }

    if (keyboard.capslock.valid) {
        if (keyboard.state.clock) {
            keyboard.keyboard_led_state.capslock = 1;
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.capslock.loc, 1);
        } else {
            keyboard.keyboard_led_state.capslock = 0;
        }
        report_id = keyboard.capslock.report_id;
    }

    uint16_t len = keyboard.keyboard_led_size;
    /* check if we don't have too much LEDs set on */
    if (len > (USB_KEYBOARD_BUFSIZE - 1)) {
        len = (USB_KEYBOARD_BUFSIZE - 1);
    }

    keyboard.buffer[0] = report_id;
    void *data;
    if (report_id != 0) {
        len++;
        data = (void *) (keyboard.buffer);
    } else {
        data = (void *) (&keyboard.buffer[1]);
    }

    req.wLength = len;

    usb_error_t err = usb_do_request_write(&req, len, data);

    if (err != USB_ERR_OK) {
        debug_printf("WARNING: set LED request not executed propperly: %s\n",
                usb_get_error_string(err));
    }

}

#if USB_KEYBOARD_MODE_ATCODE
static int32_t usb_keyboard_atcode(uint32_t keycode, int32_t up)
{
    static const int scan[] = {
        /* 89 */
        0x11c, /* Enter */
        /* 90-99 */
        0x11d, /* Ctrl-R */
        0x135, /* Divide */
        0x137 | 0x800, /* PrintScreen */
        0x138, /* Alt-R */
        0x147, /* Home */
        0x148, /* Up */
        0x149, /* PageUp */
        0x14b, /* Left */
        0x14d, /* Right */
        0x14f, /* End */
        /* 100-109 */
        0x150, /* Down */
        0x151, /* PageDown */
        0x152, /* Insert */
        0x153, /* Delete */
        0x146, /* XXX Pause/Break */
        0x15b, /* Win_L(Super_L) */
        0x15c, /* Win_R(Super_R) */
        0x15d, /* Application(Menu) */

        /* SUN TYPE 6 USB KEYBOARD */
        0x168, /* Sun Type 6 Help */
        0x15e, /* Sun Type 6 Stop */
        /* 110 - 119 */
        0x15f, /* Sun Type 6 Again */
        0x160, /* Sun Type 6 Props */
        0x161, /* Sun Type 6 Undo */
        0x162, /* Sun Type 6 Front */
        0x163, /* Sun Type 6 Copy */
        0x164, /* Sun Type 6 Open */
        0x165, /* Sun Type 6 Paste */
        0x166, /* Sun Type 6 Find */
        0x167, /* Sun Type 6 Cut */
        0x125, /* Sun Type 6 Mute */
        /* 120 - 128 */
        0x11f, /* Sun Type 6 VolumeDown */
        0x11e, /* Sun Type 6 VolumeUp */
        0x120, /* Sun Type 6 PowerDown */

        /* Japanese 106/109 keyboard */
        0x73, /* Keyboard Intl' 1 (backslash / underscore) */
        0x70, /* Keyboard Intl' 2 (Katakana / Hiragana) */
        0x7d, /* Keyboard Intl' 3 (Yen sign) (Not using in jp106/109) */
        0x79, /* Keyboard Intl' 4 (Henkan) */
        0x7b, /* Keyboard Intl' 5 (Muhenkan) */
        0x5c,
    /* Keyboard Intl' 6 (Keypad ,) (For PC-9821 layout) */
    };

    if ((keycode >= 89)
            && (keycode < (89 + (sizeof(scan) / sizeof(scan[0]))))) {
        keycode = scan[keycode - 89];
    }
    /* Pause/Break */
    if ((keycode == 104)
            && (!(keyboard.new_data.modifiers.ctrl_l
                    | keyboard.new_data.modifiers.ctrl_r))) {
        keycode = (0x45 | 0x200 | 0x400);
    }
    if ((keyboard.new_data.modifiers.shift_l
            | keyboard.new_data.modifiers.shift_r)) {
        keycode &= ~0x800;
    }
    keycode |= (up ? 0x80 : 0x00);

    if (keycode & 0xF00) {
        if (keycode & 0x400) {
            /* Ctrl */
            keyboard.at_buffered_char[0] = (0x1d | (keycode & 0x80));
            keyboard.at_buffered_char[1] = (keycode & ~0xF00);
        } else if (keycode & 0x800) {
            /* Shift */
            keyboard.at_buffered_char[0] = (0x2a | (keycode & 0x80));
            keyboard.at_buffered_char[1] = (keycode & ~0x800);
        } else {
            keyboard.at_buffered_char[0] = (keycode & ~0xF00);
            keyboard.at_buffered_char[1] = 0;
        }
        return ((keycode & 0x100) ? 0xe0 : 0xe1);
    }
    return (keycode);
}

static int32_t usb_keyboard_read(void)
{
    uint32_t keycode;
    uint32_t scancode;

    if (keyboard.at_buffered_char[0]) {
        scancode = keyboard.at_buffered_char[0];
        if (scancode & 0xF00) {
            keyboard.at_buffered_char[0] &= ~0xF00;
            return ((scancode & 0x100) ? 0xE0 : 0xE1);
        }
        keyboard.at_buffered_char[0] = keyboard.at_buffered_char[1];
        keyboard.at_buffered_char[1] = 0;
        return (scancode);
    }

    int32_t usbcode = usb_keyboard_get_key();

    if (usbcode == -1) {
        return (-1);
    }

    keycode = usb_keyboard_keycodes[usbcode & 0xFF];
    if (keycode == NN) {
        return (-1);
    }
    return (usb_keyboard_atcode(keycode, usbcode & USB_KEYBOARD_KEY_RELEASE));
}
#else
static int32_t usb_keyboard_read(void)
{
    return (-1);
}
#endif

static uint32_t usb_keyboard_read_char(void)
{
    uint32_t keycode;
    uint32_t action = USB_KEYBOARD_KEY_NOKEY;
    int32_t usbcode;
#if USB_KEYBOARD_MODE_ATCODE
    uint32_t scancode;
#endif

    uint8_t error = 0;

    while (1) {
        /* return composed char */
        if (keyboard.composed_char > 0 && keyboard.composed_done) {
            action = keyboard.composed_char;
            keyboard.composed_char = 0;
            if (action > 0xFF) {
                /* invalid char */
                return (USB_KEYBOARD_KEY_ERROR);
            }
            return (action);
        }

#if USB_KEYBOARD_MODE_ATCODE
        scancode = keyboard.at_buffered_char[0];
        if (scancode) {
            if (scancode & 0xF00) {
                keyboard.at_buffered_char[0] = (scancode & ~0xF00);
                return ((scancode & 0x100) ? 0xE0 : 0xE1);
            }
            keyboard.at_buffered_char[0] = keyboard.at_buffered_char[1];
            keyboard.at_buffered_char[1] = 0;
            return (scancode);
        }
#endif
        usbcode = usb_keyboard_get_key();
        if (usbcode == -1) {
            return (USB_KEYBOARD_KEY_NOKEY);
        }

#if USB_KEYBOARD_MODE_ATCODE
        keycode = usb_keyboard_keycodes[USB_KEYBOARD_KEY_INDEX(usbcode)];
        if (keycode == NN) {
            return (USB_KEYBOARD_KEY_NOKEY);
        }
        return (usb_keyboard_atcode(keycode,
                (usbcode & USB_KEYBOARD_KEY_RELEASE)));
#else
        keycode = usb_keyboard_keycodes[USB_KEYBOARD_KEY_INDEX(usbcode)];

        if (keycode == NN) {
            USB_DEBUG_KBD("read char: no key (NN)\n");
            return (USB_KEYBOARD_KEY_NOKEY);
        }
#endif

        switch (keycode) {
            case 0x38:
                /* alt_l */
                if (usbcode & USB_KEYBOARD_KEY_RELEASE) {
                    if (!keyboard.composed_done) {
                        keyboard.composed_done = 1;
                    }
                    if (keyboard.composed_char > 0xFF) {
                        keyboard.composed_char = 0;
                    }
                } else {
                    if (keyboard.composed_done) {
                        keyboard.composed_done = 0;
                        keyboard.composed_char = 0;
                    }
                }
                break;

            case 0x5C:
                /* print screen */
                keycode = 0x54;
                break;

            case 0x68:
                /* pause / break */
                keycode = 0x6c;
                break;
        }

        USB_DEBUG_KBD(
                "read char: ucode = %x, keycode = %x, released = %x\n", usbcode, keycode, usbcode & USB_KEYBOARD_KEY_RELEASE);

        if (usbcode & USB_KEYBOARD_KEY_RELEASE) {
            keycode |= USB_KEYBOARD_SCAN_RELEASE;
        }

        if (!keyboard.composed_done) {
            switch (keycode) {
                case 0x47:
                case 0x48:
                case 0x49:
                    /* keypad 7,8,9 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x40;
                    if (keyboard.composed_char > 0xFF) {
                        error = 1;
                    }
                    break;
                case 0x4B:
                case 0x4C:
                case 0x4D:
                    /* keypad 4,5,6 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x47;
                    if (keyboard.composed_char > 0xFF) {
                        return (USB_KEYBOARD_KEY_ERROR);
                    }
                    break;
                case 0x4F:
                case 0x50:
                case 0x51:
                    /* keypad 1,2,3 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x4E;
                    if (keyboard.composed_char > 0xFF) {
                        return (USB_KEYBOARD_KEY_ERROR);
                    }
                    break;
                case 0x52:
                    keyboard.composed_char *= 10;
                    if (keyboard.composed_char > 0xFF) {
                        return (USB_KEYBOARD_KEY_ERROR);
                    }
                    break;
                    /* key released, no interest here */
                case USB_KEYBOARD_SCAN_RELEASE | 0x47:
                case USB_KEYBOARD_SCAN_RELEASE | 0x48:
                case USB_KEYBOARD_SCAN_RELEASE | 0x49: /* keypad 7,8,9 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x4B:
                case USB_KEYBOARD_SCAN_RELEASE | 0x4C:
                case USB_KEYBOARD_SCAN_RELEASE | 0x4D: /* keypad 4,5,6 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x4F:
                case USB_KEYBOARD_SCAN_RELEASE | 0x50:
                case USB_KEYBOARD_SCAN_RELEASE | 0x51: /* keypad 1,2,3 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x52: /* keypad 0 */
                    continue;
                    break;
                case 0x38:
                    /* alt_l */
                    break;
                default:
                    if (keyboard.composed_char > 0) {
                        keyboard.composed_done = 1;
                        keyboard.composed_char = 0;
                        return (USB_KEYBOARD_KEY_ERROR);
                    }
                    break;
            }
        }

        action = usb_keyboard_keyaction(keycode & 0x7F, keycode & 0x80);
        if (action != USB_KEYBOARD_KEY_NOKEY) {
            USB_DEBUG_KBD("read char: return code %x\n", action);
            return (action);
        }
    }

    return (USB_KEYBOARD_KEY_NOKEY);
}

static void usb_keyboard_process_data(void)
{
    if (USB_KEYBOARD_MODE_ATCODE) {
        int32_t sc;
        while ((sc = usb_keyboard_read()) != -1) {
            if (sc & 0xF00) {
                key_event((uint8_t) (0xFF & sc), 1);
            } else {
                key_event((uint8_t) (0xFF & sc), 0);
            }
        }
    } else {
        uint32_t c;
        char cp[2];
        while ((c = usb_keyboard_read_char()) != USB_KEYBOARD_KEY_NOKEY) {
            if (c < 0xFF) {
                cp[0] = (char) (0xFF & c);
                if (cp[0] == '\n') {
                    cp[1] = cp[0];
                    cp[0] = '\r';
                    sys_print(cp, 2);
                } else {
                    sys_print(cp, 1);
                }
                //debug_printf("%c\n", 0xFF & c);
                printf("f");
            } else {
                if ((c & SPCLKEY) && !(c & RELKEY)) {
                    // debug_printf("special: %x\n", c);
                    usb_keyboard_set_leds();
                }
            }
        }
    }
}

/**
 * \brief   this function gets called if the data transfer is completed
 *
 * \param   err     outcome of the transfer
 * \param   data    raw data buffer
 * \param   length  number of bytes in the data buffer
 *
 */
static void usb_keyboard_transfer_cb(usb_error_t err, void *data_in,
        uint32_t length)
{
    //USB_DEBUG("usb_keyboard_transfer_cb() %u\n", length);

    uint8_t *data = (uint8_t*) data_in;

    //debug_printf("[%x][%x][%x][%x][%x][%x][%x][%x]\n", data[0], data[1], data[2],
    //        data[3],data[4], data[5], data[6], data[7]);

    if (err != USB_ERR_OK) {
        debug_printf("WARNING: transfer not completed propperly.\n");
        return;
    }

    if (length == 0) {
        /* no data, do nothing. the transfer is set to auto start... */
        return;
    }

    uint8_t rid = 0;

    /*
     * if a keyboard ID is set, then the data contains a HID ID byte which we
     * have to remove first
     */
    if (keyboard.keyboard_id) {
        debug_printf("copy out id byte..\n");
        rid = *((uint8_t *) data);
        data++;
        length--;
        if (length == 0) {
            /* just the HID ID byte.. no data, */
            return;
        }
    }

    memset(&keyboard.new_data, 0, sizeof(keyboard.old_data));
    keyboard.modifiers.generic = 0x0000;

    USB_KEYBOARD_MODIFIER_CHECK(ctrl_l);
    USB_KEYBOARD_MODIFIER_CHECK(ctrl_r);
    USB_KEYBOARD_MODIFIER_CHECK(shift_l);
    USB_KEYBOARD_MODIFIER_CHECK(shift_r);
    USB_KEYBOARD_MODIFIER_CHECK(alt_l);
    USB_KEYBOARD_MODIFIER_CHECK(alt_r);
    USB_KEYBOARD_MODIFIER_CHECK(win_r);
    USB_KEYBOARD_MODIFIER_CHECK(win_l);

    keyboard.new_data.modifiers = keyboard.modifiers;

    if (keyboard.events.valid && (rid == keyboard.events.report_id)) {
        uint32_t i = keyboard.events.loc.count;
        if (i > USB_KEYBOARD_KEYCODES) {
            i = USB_KEYBOARD_KEYCODES;
        }
        if (i > length) {
            i = length;
        }

        while (i--) {
            keyboard.new_data.keycode[i] = usb_hid_get_data(&data[i],
                    length - i, &(keyboard.events.loc));
        }
    }

    if (keyboard.new_data.keycode[0] == USB_KEYBOARD_KEY_ERROR) {
        USB_DEBUG("keyboard key error...");
        return;
    }

    union usb_keyboard_modifiers *old_mod = &keyboard.old_data.modifiers;
    union usb_keyboard_modifiers *new_mod = &keyboard.new_data.modifiers;

    /* check for released modifiers keys */
    if (old_mod->generic != new_mod->generic) {
        USB_KEYBOARD_KEY_RELEASE_CHECK(ctrl_l, 0xe0);
        USB_KEYBOARD_KEY_RELEASE_CHECK(ctrl_r, 0xe4);
        USB_KEYBOARD_KEY_RELEASE_CHECK(shift_r, 0xe5);
        USB_KEYBOARD_KEY_RELEASE_CHECK(shift_l, 0xe1);
        USB_KEYBOARD_KEY_RELEASE_CHECK(alt_l, 0xe2);
        USB_KEYBOARD_KEY_RELEASE_CHECK(alt_r, 0xe6);
        USB_KEYBOARD_KEY_RELEASE_CHECK(win_r, 0xe7);
        USB_KEYBOARD_KEY_RELEASE_CHECK(win_l, 0xe3);
    }

    uint8_t key;
    uint8_t found = 0;

    /* check for released keys */
    for (uint32_t i = 0; i < USB_KEYBOARD_KEYCODES; i++) {
        key = keyboard.old_data.keycode[i];
        found = 0;
        if (key == 0) {
            continue;
        }
        for (uint32_t j = 0; j < USB_KEYBOARD_KEYCODES; j++) {
            if (keyboard.new_data.keycode[j] == 0) {
                continue;
            }
            if (key == keyboard.new_data.keycode[j]) {
                /* found the key again, so its not released...*/
                found = 1;
                break;
            }
        }
        if (!found) {
            /* key is released. put key release into buffer */
            usb_keyboard_put_key(key | USB_KEYBOARD_KEY_RELEASE);
        }
    }

    /* check for pressed keys */
    for (uint32_t i = 0; i < USB_KEYBOARD_KEYCODES; i++) {
        key = keyboard.new_data.keycode[i];
        if (key == 0) {
            continue;
        }
        found = 0;
        for (uint32_t j = 0; j < USB_KEYBOARD_KEYCODES; j++) {
            if (keyboard.old_data.keycode[j] == 0) {
                continue;
            }
            if (key == keyboard.old_data.keycode[j]) {
                /* key is still pressed... */
                found = 1;
                break;
            }
        }
        if (!found) {
            usb_keyboard_put_key(key | USB_KEYBOARD_KEY_PRESS);
        }
    }

    memcpy(&keyboard.old_data, &keyboard.new_data, sizeof(keyboard.old_data));

    usb_keyboard_process_data();
}

/*
 * --------------------------------------------------------------------------
 * USB Keyboard initialization and de-initialization functions
 * --------------------------------------------------------------------------
 */

#define USB_KEYBOARD_PARSE_DEBUG(_str, _key) \
    USB_DEBUG_HID("%s: pos = %u, count = %u, size = %u, report_id = %u\n",\
            _str, keyboard._key.loc.position, keyboard._key.loc.count,\
        keyboard._key.loc.size, keyboard._key.report_id );

/**
 * \brief   this function parses the HID descriptor and sets some special
 *          key location values in the keyboard struct
 *
 * \param ptr pointer to the hid descriptor
 * \param len the length of the HID descriptor
 */
static void usb_keyboard_parse_hid(const uint8_t *ptr, uint32_t len)
{
    uint32_t flags;

    /* check if there is an ID byte */
    keyboard.keyboard_size = usb_hid_report_size(ptr, len, USB_HID_KIND_INPUT,
            &keyboard.keyboard_id);

    /* figure out some keys */

    /* key CTRL left */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE0),
            USB_HID_KIND_INPUT, 0, &keyboard.ctrl_l.loc, &flags,
            &keyboard.ctrl_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_l.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("ctrl_l ", ctrl_l);
    }

    /* key CTRL right */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE4),
            USB_HID_KIND_INPUT, 0, &keyboard.ctrl_r.loc, &flags,
            &keyboard.ctrl_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_r.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("ctrl_r ", ctrl_r);
    }

    /* key SHIFT left */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE1),
            USB_HID_KIND_INPUT, 0, &keyboard.shift_l.loc, &flags,
            &keyboard.shift_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.shift_l.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("shift_l", shift_l);
    }

    /* key SHIFT right */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE5),
            USB_HID_KIND_INPUT, 0, &keyboard.shift_r.loc, &flags,
            &keyboard.shift_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.shift_r.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("shift_r", shift_r);
    }

    /* key ALT left */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE2),
            USB_HID_KIND_INPUT, 0, &keyboard.alt_l.loc, &flags,
            &keyboard.alt_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.alt_l.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("alt_l  ", alt_l);
    }

    /* key ALT right */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE6),
            USB_HID_KIND_INPUT, 0, &keyboard.alt_r.loc, &flags,
            &keyboard.alt_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.alt_r.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("alt_r  ", alt_r);
    }

    /* key WIN left */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE3),
            USB_HID_KIND_INPUT, 0, &keyboard.win_l.loc, &flags,
            &keyboard.win_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_l.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("win_l  ", win_l);
    }

    /* key WIN right */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE7),
            USB_HID_KIND_INPUT, 0, &keyboard.win_r.loc, &flags,
            &keyboard.win_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_r.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("win_r  ", win_r);
    }
    /* figure out event buffer */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0x00),
            USB_HID_KIND_INPUT, 0, &keyboard.events.loc, &flags,
            &keyboard.events.report_id)) {

        keyboard.events.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("events", events);

    }

    /* figure out leds on keyboard */
    keyboard.keyboard_led_size = usb_hid_report_size(ptr, len,
            USB_HID_KIND_OUTPUT, NULL);

    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x01),
            USB_HID_KIND_OUTPUT, 0, &keyboard.numlock.loc, &flags,
            &keyboard.numlock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.numlock.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("lednl ", numlock);
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x02),
            USB_HID_KIND_OUTPUT, 0, &keyboard.capslock.loc, &flags,
            &keyboard.capslock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.capslock.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("ledcs ", capslock);
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x03),
            USB_HID_KIND_OUTPUT, 0, &keyboard.scrolllock.loc, &flags,
            &keyboard.scrolllock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.scrolllock.valid = 1;

        USB_KEYBOARD_PARSE_DEBUG("ledsl", scrolllock);
    }
}

/**
 * \brief   initializes the USB keyboard
 */
usb_error_t usb_keyboard_init(void)
{
    USB_DEBUG("usb_keyboard_init()...\n");

    memset(&keyboard, 0, sizeof(struct usb_keyboard));

    /*
     * The HID class uses the standard request Get_Descriptor as described in
     * the USB Specification. When a Get_Descriptor(Configuration) request is
     * issued, it returns the Configuration descriptor, all Interface
     * descriptors, all Endpoint descriptors, and the HID descriptor for each
     * interface.
     */

    keyboard.num_config = usb_device_get_num_config();

    struct usb_interface *iface = usb_device_get_iface(0);

    if (iface != NULL && iface->iface_class != USB_HID_CLASS_CODE) {
        debug_printf("ERROR: device is not HID class..\n");
        return (USB_ERR_INVAL);
    }

    if (iface->iface_protocol != USB_HID_PROTOCOL_KEYBOARD) {
        debug_printf("ERROR: device is not a keyboard");
        return (USB_ERR_INVAL);
    }

    /*
     * setting up the USB transfers
     */
    usb_error_t err = usb_transfer_setup_intr(
            &keyboard_tconf[USB_KEYBOARD_DATA], usb_keyboard_transfer_cb,
            &keyboard.xferids[USB_KEYBOARD_DATA]);

    err = usb_transfer_setup_intr(&keyboard_tconf[USB_KEYBOARD_DATA],
            usb_keyboard_transfer_cb, &keyboard.xferids[USB_KEYBOARD_DATA_2]);

    if (err != USB_ERR_OK) {
        debug_printf("Failed to setup USB transfer: %s\n",
                usb_get_error_string(err));
        return (err);
    }

    if (err != USB_ERR_OK) {
        debug_printf("Failed to setup USB transfer: %s\n",
                usb_get_error_string(err));
        return (err);
    }

    struct usb_hid_descriptor *hid_ptr;
    uint16_t hid_length;
    err = usb_hid_get_hid_descriptor(&hid_ptr, &hid_length, 0);
    if (err != USB_ERR_OK) {
        debug_printf("could not get the HID descriptor: %s\n",
                usb_get_error_string(err));
    }

    if (err == USB_ERR_OK) {
        USB_DEBUG_KBD("Parsing HID descriptor of %d bytes\n", (int16_t)hid_length);
        usb_keyboard_parse_hid((void *) hid_ptr, hid_length);
        free(hid_ptr);
    }

    /* TODO: figure out why it takes so long with interrupts... */
    err = usb_hid_set_idle(0, 16, 0);
    if (err != USB_ERR_OK) {
        USB_DEBUG("NOTICE: setting idle rate failed. (ignored)\n");
    }

    return (USB_ERR_OK);
}

usb_error_t usb_keyboard_start_transfers(void)
{
    usb_error_t err;
    /* start the interrupt transfer */
    err = usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);
    if (err != USB_ERR_OK) {
        return (err);
    }
    err = usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA_2]);

    return (err);
}

/**
 * \brief deinitializes the transfers upon shutting down the keyboard
 *        driver
 */
void usb_keyboard_deinit(void)
{
    for (uint32_t i = 0; i < USB_KEYBOARD_NUM_TRANSFERS; i++) {
        usb_transfer_unsetup(keyboard.xferids[i]);
    }

}
