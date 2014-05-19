/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_KEYBOARD_DRIVER_H_
#define USB_KEYBOARD_DRIVER_H_

#include <usb/usb_transfer.h>
#include <usb/class/usb_hid.h>

#define USB_KEYBOARD_MODE_ATCODE 1

#define USB_KEYBOARD_BUFSIZE 64
#define USB_KEYBOARD_MODIFIIERS 8
#define USB_KEYBOARD_KEYCODES 6
#define USB_KEYBOARD_IN_BUFSIZE \
    (2*(USB_KEYBOARD_MODIFIIERS + (2*USB_KEYBOARD_KEYCODES)))

/// enumeration of USB keyboard transfers
enum {
    USB_KEYBOARD_DATA,          ///< used for data transfers to get key events
    USB_KEYBOARD_DATA_2,
    USB_KEYBOARD_NUM_TRANSFERS,  ///< number of transfers
};

/**
 * this union represents the possible modifiers for the pressed key code
 */
union usb_keyboard_modifiers {
    struct {
        uint8_t ctrl_l :1;
        uint8_t shift_l :1;
        uint8_t alt_l :1;
        uint8_t win_l :1;
        uint8_t ctrl_r :1;
        uint8_t shift_r :1;
        uint8_t alt_r :1;
        uint8_t win_r :1;
        uint8_t eject :1;
        uint8_t fn :1;
        uint8_t slock : 1;
        uint8_t nlock : 1;
        uint8_t clock : 1;
        uint8_t _unused :3;
    };
    uint16_t generic;
};

#define USB_KEYBOARD_MODIFIER_CHECK(_mod) \
    if (keyboard._mod.valid && (rid == keyboard._mod.report_id)) { \
        if (usb_hid_get_data(data, length, &(keyboard._mod.loc))) {\
            keyboard.modifiers._mod = 1;\
        }\
    }\

#define USB_KEYBOARD_KEY_RELEASE_CHECK(_key, _value) \
    if (old_mod->_key != new_mod->_key) { \
        usb_keyboard_put_key(_value | (new_mod->_key ? \
                USB_KEYBOARD_KEY_PRESS : USB_KEYBOARD_KEY_RELEASE)); \
    } \

/**
 *
 */
struct usb_keyboard_led {
    uint8_t scrolllock :1;
    uint8_t numlock :1;
    uint8_t capslock :1;
    uint8_t reserved :5;
};

/**
 * this structure represents a pressed key event with the corresponding
 * modifiers such as alt/shift/ctrl...
 */
struct usb_keyboard_data {
    union usb_keyboard_modifiers modifiers;  ///< the activated modifiers
    uint8_t keycode[USB_KEYBOARD_KEYCODES];  ///> the extracted keycode
};


#define USB_KEYBOARD_KEY_ERROR 0x20000000 //0x01
#define USB_KEYBOARD_KEY_NOKEY 0x01000000 //0x0
#define USB_KEYBOARD_KEY_PRESS 0x00
#define USB_KEYBOARD_KEY_RELEASE 0x400
#define USB_KEYBOARD_SCAN_RELEASE 0x80

struct usb_keyboard_key {
    struct usb_hid_location loc;
    uint8_t report_id;
    uint8_t valid;
};

struct usb_keyboard {
    /* location of special keys */
    struct usb_keyboard_key ctrl_l;
    struct usb_keyboard_key ctrl_r;
    struct usb_keyboard_key shift_l;
    struct usb_keyboard_key shift_r;
    struct usb_keyboard_key alt_l;
    struct usb_keyboard_key alt_r;
    struct usb_keyboard_key win_l;
    struct usb_keyboard_key win_r;
    struct usb_keyboard_key events;
    struct usb_keyboard_key numlock;
    struct usb_keyboard_key capslock;
    struct usb_keyboard_key scrolllock;

    union usb_keyboard_modifiers state;
    union usb_keyboard_modifiers modifiers;
    struct usb_keyboard_data new_data;
    struct usb_keyboard_data old_data;

    uint8_t num_config;  ///< the number of configurations
    usb_xfer_id_t xferids[USB_KEYBOARD_NUM_TRANSFERS];
    uint8_t buffer[USB_KEYBOARD_BUFSIZE];

    uint32_t last_active[128];
    uint32_t composed_char;
    uint8_t composed_done;

#if USB_KEYBOARD_MODE_ATCODE
    uint32_t at_buffered_char[2];
#endif

    uint8_t usb_iface_number;

    uint8_t keyboard_id;
    int32_t keyboard_size;
    uint16_t keyboard_led_size;
    struct usb_keyboard_led keyboard_led_state;

    /* input buffers */
    uint32_t input[USB_KEYBOARD_IN_BUFSIZE];
    uint16_t input_head;
    uint16_t input_tail;
    uint16_t input_size;
};

typedef struct usb_keyboard usb_keyboard_t;

void usb_keyboard_deinit(void);
usb_error_t usb_keyboard_init(void);
usb_error_t usb_keyboard_start_transfers(void);

//#define USB_DEBUG_KBD(x...) debug_printf(x)
#define USB_DEBUG_KBD(x...)


#endif /* USB_KEYBOARD_DRIVER_H_ */
