/**
 * \brief this file contains definitions for the the USB HID class
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_CLASS_HID_H_
#define LIBUSB_CLASS_HID_H_

#include <usb/usb_descriptor.h>

#define USB_HID_CLASS_CODE 0x3
#define USB_HID_SUBCLASS_CODE_BOOT 0x1
#define USB_HID_PROTOCOL_NONE 0x0
#define USB_HID_PROTOCOL_KEYBOARD 0x1
#define USB_HID_PROTOCOL_MOUSE 0x2

/*
 * USB HID class descriptor types
 */

/// Descriptor type for the HID
#define USB_DESCRIPTOR_TYPE_HID      0x21

/// Descriptor type for HID report
#define USB_DESCRIPTOR_TYPE_REPORT   0x22

/// descriptor type for HID physical
#define USB_DESCRIPTOR_TYPE_PHYSICAL 0x23

/*
 * HID class specific request types
 */

#define USB_HID_REQUEST_GET_REPORT   0x01
#define USB_HID_REQUEST_SET_REPORT   0x09

#define USB_HID_REQUEST_GET_IDLE     0x02
#define USB_HID_REQUEST_SET_IDLE     0x0A

#define USB_HID_REQUEST_GET_PROTOCOL 0x03
#define USB_HID_REQUEST_SET_PROTOCOL 0x0B

#define USB_HID_REQUEST_GET_DESCRIPTOR 0x06
#define USB_HID_REQUEST_SET_DESCRIPTOR 0x07

/**
 * struct definition for a HID descriptor type
 * XXX: the length cannot be an uint16, because of alignment issues
 */
struct usb_hid_dtype {
    uint8_t bDescriptorType;        ///< the type of the optional descriptor
    uint8_t wDescriptorLength[2];   ///< the size of the optional descriptor
}__attribute__((packed));

/// type definition of the HID descriptor type
typedef struct usb_hid_dtype usb_hid_dtype_t;

/// makro definition for getting the length of a descriptor type
#define USB_HID_DTYPE_GET_LEN(_d) \
    ((_d)->wDescriptorLength[0] | ((uint16_t)((_d)->wDescriptorLength[1] << 8)))

/**
 * USB HID Descriptor Type
 * Device Class Definition for HID Devices, Section 6.2.1
 *
 * Note: This descriptor is of variable size, the last field descriptors
 *       contains the information of at least one descriptor (i.e. the report
 *       descriptor)
 */
struct usb_hid_descriptor {
    uint8_t bLength;                ///< the total size in bytes of this descr
    uint8_t bDescriptorType;        ///< constant, specifying the HID descr type
    uint16_t bcdHID;                ///< HID class specific release version
    uint8_t bCountryCode;           ///< country code for localized hardware
    uint8_t bNumDescriptors;        ///< the number of class descriptors > 1
    usb_hid_dtype_t descriptors[1];  ///< subordinate descriptors
}__attribute__((packed));

/// USB HID descriptor type
typedef struct usb_hid_descriptor usb_hid_descriptor_t;

/// calcualtes the size of the HID descriptor
#define USB_HID_DESCRIPTOR_SIZE(n) (9+((n)*3))

/*
 * -------------------------------------------------------------------------
 * USB HID class report items
 * -------------------------------------------------------------------------
 */

#define USB_HID_REPORT_INPUT 0x01
#define USB_HID_REPORT_OUTPUT 0x02
#define USB_HID_REPORT_FEATURE 0x03

/*
 * some flags
 */
#define USB_HID_IO_CONST   0x001
#define USB_HID_IO_VARIABLE    0x002
#define USB_HID_IO_RELATIVE    0x004
#define USB_HID_IO_WRAP    0x008
#define USB_HID_IO_NONLINEAR   0x010
#define USB_HID_IO_NOPREF  0x020
#define USB_HID_IO_NULLSTATE   0x040
#define USB_HID_IO_VOLATILE    0x080
#define USB_HID_IO_BUFBYTES    0x100

/// enumeration representing the type of the report item
enum usb_hid_kind {
    USB_HID_KIND_INPUT = 0,         ///< input report
    USB_HID_KIND_OUTPUT = 1,        ///< output report
    USB_HID_KIND_FEATURE = 2,       ///< feature report
    USB_HID_KIND_COLLECTION = 3,    ///< collection start
    USB_HID_KIND_ENDCOLLECTION = 4,  ///< collection end
};

/// struct definition for report id - position association
struct usb_hid_pos_data {
    int32_t rid;        ///< report id
    uint32_t position;  ///< position
};

/// struct definition for storing information about the HID item in an report
struct usb_hid_location {
    uint32_t size;      ///< the size of one item element
    uint32_t count;     ///< the number of elements in this location
    uint32_t position;  ///< the position within the report
};

/// This struct represents an item in a USB report
struct usb_hid_item {
    /* Global */
    int32_t _usage_page;         ///<
    int32_t logical_minimum;     ///<
    int32_t logical_maximum;     ///<
    int32_t physical_minimum;    ///<
    int32_t physical_maximum;    ///<
    int32_t unit_exponent;       ///<
    int32_t unit;                ///<
    int32_t report_ID;           ///<
    /* Local */
    int32_t usage;               ///<
    int32_t usage_minimum;       ///<
    int32_t usage_maximum;       ///<
    int32_t designator_index;    ///<
    int32_t designator_minimum;  ///<
    int32_t designator_maximum;  ///<
    int32_t string_index;        ///<
    int32_t string_minimum;      ///<
    int32_t string_maximum;      ///<
    int32_t set_delimiter;       ///<
    /* Misc */
    int32_t collection;          ///<
    int32_t collevel;            ///<
    enum usb_hid_kind kind;      ///<
    uint32_t flags;              ///<
    /* Location */
    struct usb_hid_location loc;  ///<
};

/*
 * definitions for parsing the report descriptors
 */

#define USB_HID_MAXUSAGE 64
#define USB_HID_MAXPUSH 4
#define USB_HID_MAXID 16

/**
 * this structure is used for storing the necessary data when parsing
 * USB HID reports
 */
struct usb_hid_data {
    const uint8_t *start;
    const uint8_t *end;
    const uint8_t *p;
    struct usb_hid_item cur[USB_HID_MAXPUSH];
    struct usb_hid_pos_data last_pos[USB_HID_MAXID];
    int32_t usages_min[USB_HID_MAXUSAGE];
    int32_t usages_max[USB_HID_MAXUSAGE];
    int32_t usage_last; /* last seen usage */
    uint32_t loc_size; /* last seen size */
    uint32_t loc_count; /* last seen count */
    uint8_t kindset; /* we have 5 kinds so 8 bits are enough */
    uint8_t pushlevel; /* current pushlevel */
    uint8_t ncount; /* end usage item count */
    uint8_t icount; /* current usage item count */
    uint8_t nusage; /* end "usages_min/max" index */
    uint8_t iusage; /* current "usages_min/max" index */
    uint8_t ousage; /* current "usages_min/max" offset */
    uint8_t susage; /* usage set flags */
};

#define USB_HID_REQUEST_TYPE_WRITE 0xA1
#define USB_HID_REQUEST_TYPE_READ 0x21

#define USB_HID_REQUEST_VALUE_IN      0x01
#define USB_HID_REQUEST_VALUE_OUT     0x02
#define USB_HID_REQUEST_VALUE_FEATURE 0x03

/*
 * -------------------------------------------------------------------------
 *  Country Codes
 * -------------------------------------------------------------------------
 */

#define USB_HID_COUNTRY_DEFAULT         0
#define USB_HID_COUNTRY_ARABIC          1
#define USB_HID_COUNTRY_BELGIAN         2
#define USB_HID_COUNTRY_CANADIAN        3
#define USB_HID_COUNTRY_CANADIAN_FRENCH 4
#define USB_HID_COUNTRY_CZECH           5
#define USB_HID_COUNTRY_DANISH          6
#define USB_HID_COUNTRY_FINNISH         7
#define USB_HID_COUNTRY_FRENCH          8
#define USB_HID_COUNTRY_GERMAN          9
#define USB_HID_COUNTRY_GREEK          10
#define USB_HID_COUNTRY_HEBREW         11
#define USB_HID_COUNTRY_HUNGARY        12
#define USB_HID_COUNTRY_INTERNATIONAL  13
#define USB_HID_COUNTRY_ITALIAN        14
#define USB_HID_COUNTRY_JAPAN          15
#define USB_HID_COUNTRY_KOREAN         16
#define USB_HID_COUNTRY_LATIN          17
#define USB_HID_COUNTRY_DUTCH          18
#define USB_HID_COUNTRY_NORWEGIAN      19
#define USB_HID_COUNTRY_PERSIAN        20
#define USB_HID_COUNTRY_POLAND         21
#define USB_HID_COUNTRY_PORTUGUESE     22
#define USB_HID_COUNTRY_RUSSIA         23
#define USB_HID_COUNTRY_SLOVAKIA       24
#define USB_HID_COUNTRY_SPANISH        25
#define USB_HID_COUNTRY_SWEDISH        26
#define USB_HID_COUNTRY_SWISS_FRENCH   27
#define USB_HID_COUNTRY_SWISS_GERMAN   28
#define USB_HID_COUNTRY_SWITZERLAND    29
#define USB_HID_COUNTRY_TAIWAN         30
#define USB_HID_COUNTRY_TURKISH_Q      31
#define USB_HID_COUNTRY_UK             32
#define USB_HID_COUNTRY_US             33
#define USB_HID_COUNTRY_YUGOSLAVIA     34
#define USB_HID_COUNTRY_TURKISH_F      35
// other 36-255 reserved

/*
 * -------------------------------------------------------------------------
 * USB HID Physical Designator Values
 *
 * This defines specify which part of the human body
 * triggered the input.
 *
 * e.g. the hand or finger
 * -------------------------------------------------------------------------
 */
#define USB_HID_DESIGNATOR_NONE      0x00
#define USB_HID_DESIGNATOR_HAND      0x01
#define USB_HID_DESIGNATOR_EYEBALL   0x02
#define USB_HID_DESIGNATOR_EYEBROW   0x03
#define USB_HID_DESIGNATOR_EYELID    0x04
#define USB_HID_DESIGNATOR_EAR       0x05
#define USB_HID_DESIGNATOR_NOSE      0x06
#define USB_HID_DESIGNATOR_MOUTH     0x07
#define USB_HID_DESIGNATOR_UPPER_LIP 0x08
#define USB_HID_DESIGNATOR_LOWER_LIP 0x09
#define USB_HID_DESIGNATOR_JAW       0x0A
#define USB_HID_DESIGNATOR_NECK      0x0B
#define USB_HID_DESIGNATOR_UPPER_ARM 0x0C
#define USB_HID_DESIGNATOR_ELBOW     0x0D
#define USB_HID_DESIGNATOR_FOREARM   0x0E
#define USB_HID_DESIGNATOR_WRIST     0x0F
#define USB_HID_DESIGNATOR_PALM      0x10
#define USB_HID_DESIGNATOR_THUMB     0x11
#define USB_HID_DESIGNATOR_I_FINGER  0x12
#define USB_HID_DESIGNATOR_M_FINGER  0x13
#define USB_HID_DESIGNATOR_R_FINGER  0x14
#define USB_HID_DESIGNATOR_L_FINGER  0x15
#define USB_HID_DESIGNATOR_HEAD      0x16
#define USB_HID_DESIGNATOR_SHOULDER  0x17
#define USB_HID_DESIGNATOR_HIP       0x18
#define USB_HID_DESIGNATOR_WAIST     0x19
#define USB_HID_DESIGNATOR_THIGH     0x1A
#define USB_HID_DESIGNATOR_KNEE      0x1B
#define USB_HID_DESIGNATOR_CALF      0x1C
#define USB_HID_DESIGNATOR_ANKLE     0x1D
#define USB_HID_DESIGNATOR_FOOT      0x1E
#define USB_HID_DESIGNATOR_HEEL      0x1F
#define USB_HID_DESIGNATOR_BALL_FOOT 0x20
#define USB_HID_DESIGNATOR_BIG_TOE   0x21
#define USB_HID_DESIGNATOR_TOE_2     0x22
#define USB_HID_DESIGNATOR_TOE_3     0x23
#define USB_HID_DESIGNATOR_TOE_4     0x24
#define USB_HID_DESIGNATOR_TOE_5     0x25
#define USB_HID_DESIGNATOR_BROW      0x26
#define USB_HID_DESIGNATOR_CHEEK     0x27
/* 28-FF reserved */

/*
 * -------------------------------------------------------------------------
 * USB HID Physical Qualifier Fields
 *
 * This values represent which of the possible multiple input
 * designators triggered the input.
 *
 * e.g. right or left hand
 * -------------------------------------------------------------------------
 */
#define USB_HID_PHYSICAL_QUALIFIER_NONE       0x0
#define USB_HID_PHYSICAL_QUALIFIER_RIGHT      0x1
#define USB_HID_PHYSICAL_QUALIFIER_LEFT       0x2
#define USB_HID_PHYSICAL_QUALIFIER_BOTH       0x3
#define USB_HID_PHYSICAL_QUALIFIER_EITHER     0x4
#define USB_HID_PHYSICAL_QUALIFIER_CENTER     0x5
#define USB_HID_PHYSICAL_QUALIFIER_RESERVED   0x6
#define USB_HID_PHYSICAL_QUALIFIER_RESERVED_  0x7

/*
 * -------------------------------------------------------------------------
 * USB HID Usages
 * -------------------------------------------------------------------------
 */
#define USB_HID_USAGE_UNDEFINED         0x0000
#define USB_HID_USAGE_GENERIC_DESKTOP   0x0001
#define USB_HID_USAGE_SIMULATION        0x0002
#define USB_HID_USAGE_VR_CONTROLS       0x0003
#define USB_HID_USAGE_SPORTS_CONTROLS   0x0004
#define USB_HID_USAGE_GAMING_CONTROLS   0x0005
#define USB_HID_USAGE_KEYBOARD          0x0007
#define USB_HID_USAGE_LEDS              0x0008
#define USB_HID_USAGE_BUTTON            0x0009
#define USB_HID_USAGE_ORDINALS          0x000a
#define USB_HID_USAGE_TELEPHONY         0x000b
#define USB_HID_USAGE_CONSUMER          0x000c
#define USB_HID_USAGE_DIGITIZERS        0x000d
#define USB_HID_USAGE_PHYSICAL_IFACE    0x000e
#define USB_HID_USAGE_UNICODE           0x0010
#define USB_HID_USAGE_ALPHANUM_DISPLAY  0x0014
#define USB_HID_USAGE_MONITOR           0x0080
#define USB_HID_USAGE_MONITOR_ENUM_VAL  0x0081
#define USB_HID_USAGE_VESA_VC           0x0082
#define USB_HID_USAGE_VESA_CMD          0x0083
#define USB_HID_USAGE_POWER             0x0084
#define USB_HID_USAGE_BATTERY_SYSTEM    0x0085
#define USB_HID_USAGE_BARCODE_SCANNER   0x008b
#define USB_HID_USAGE_SCALE             0x008c
#define USB_HID_USAGE_CAMERA_CONTROL    0x0090
#define USB_HID_USAGE_ARCADE            0x0091
#define USB_HID_USAGE_MICROSOFT         0xff00

/* Usages, generic desktop */
#define USB_HID_USAGE_POINTER           0x0001
#define USB_HID_USAGE_MOUSE             0x0002
#define USB_HID_USAGE_JOYSTICK          0x0004
#define USB_HID_USAGE_GAME_PAD          0x0005
#define USB_HID_USAGE_DKEYBOARD         0x0006
#define USB_HID_USAGE_KEYPAD            0x0007
#define USB_HID_USAGE_X                 0x0030
#define USB_HID_USAGE_Y                 0x0031
#define USB_HID_USAGE_Z                 0x0032
#define USB_HID_USAGE_RX                0x0033
#define USB_HID_USAGE_RY                0x0034
#define USB_HID_USAGE_RZ                0x0035
#define USB_HID_USAGE_SLIDER            0x0036
#define USB_HID_USAGE_DIAL              0x0037
#define USB_HID_USAGE_WHEEL             0x0038
#define USB_HID_USAGE_HAT_SWITCH        0x0039
#define USB_HID_USAGE_COUNTED_BUFFER    0x003a
#define USB_HID_USAGE_BYTE_COUNT        0x003b
#define USB_HID_USAGE_MOTION_WAKEUP     0x003c
#define USB_HID_USAGE_VX                0x0040
#define USB_HID_USAGE_VY                0x0041
#define USB_HID_USAGE_VZ                0x0042
#define USB_HID_USAGE_VBRX              0x0043
#define USB_HID_USAGE_VBRY              0x0044
#define USB_HID_USAGE_VBRZ              0x0045
#define USB_HID_USAGE_VNO               0x0046
#define USB_HID_USAGE_TWHEEL            0x0048
#define USB_HID_USAGE_SYSTEM_CONTROL    0x0080
#define USB_HID_USAGE_SYSTEM_POWER_DOWN 0x0081
#define USB_HID_USAGE_SYSTEM_SLEEP      0x0082
#define USB_HID_USAGE_SYSTEM_WAKEUP     0x0083
#define USB_HID_USAGE_SYSTEM_CONTEXT_MENU 0x0084
#define USB_HID_USAGE_SYSTEM_MAIN_MENU  0x0085
#define USB_HID_USAGE_SYSTEM_APP_MENU   0x0086
#define USB_HID_USAGE_SYSTEM_MENU_HELP  0x0087
#define USB_HID_USAGE_SYSTEM_MENU_EXIT  0x0088
#define USB_HID_USAGE_SYSTEM_MENU_SELECT 0x0089
#define USB_HID_USAGE_SYSTEM_MENU_RIGHT 0x008a
#define USB_HID_USAGE_SYSTEM_MENU_LEFT  0x008b
#define USB_HID_USAGE_SYSTEM_MENU_UP    0x008c
#define USB_HID_USAGE_SYSTEM_MENU_DOWN  0x008d
#define USB_HID_USAGE_APPLE_EJECT       0x00b8

/* Usages Digitizers */
#define USB_HID_USAGE_UNDEFINED         0x0000
#define USB_HID_USAGE_TIP_PRESSURE      0x0030
#define USB_HID_USAGE_BARREL_PRESSURE   0x0031
#define USB_HID_USAGE_IN_RANGE          0x0032
#define USB_HID_USAGE_TOUCH             0x0033
#define USB_HID_USAGE_UNTOUCH           0x0034
#define USB_HID_USAGE_TAP               0x0035
#define USB_HID_USAGE_QUALITY           0x0036
#define USB_HID_USAGE_DATA_VALID        0x0037
#define USB_HID_USAGE_TRANSDUCER_INDEX  0x0038
#define USB_HID_USAGE_TABLET_FKEYS      0x0039
#define USB_HID_USAGE_PROGRAM_CHANGE_KEYS 0x003a
#define USB_HID_USAGE_BATTERY_STRENGTH  0x003b
#define USB_HID_USAGE_INVERT            0x003c
#define USB_HID_USAGE_X_TILT            0x003d
#define USB_HID_USAGE_Y_TILT            0x003e
#define USB_HID_USAGE_AZIMUTH           0x003f
#define USB_HID_USAGE_ALTITUDE          0x0040
#define USB_HID_USAGE_TWIST             0x0041
#define USB_HID_USAGE_TIP_SWITCH        0x0042
#define USB_HID_USAGE_SEC_TIP_SWITCH    0x0043
#define USB_HID_USAGE_BARREL_SWITCH     0x0044
#define USB_HID_USAGE_ERASER            0x0045
#define USB_HID_USAGE_TABLET_PICK       0x0046

/* Usages, Consumer */
#define USB_HID_USAGE_AC_PAN      0x0238

/// Macro for combining two usages
#define USB_HID_USAGE_COMBINE(x,y) (((x) << 16) | (y))

struct usb_hid_data *usb_hid_start_parse(const void *d, uint32_t len,
        int32_t kindset);

void usb_hid_end_parse(struct usb_hid_data *s);

int32_t usb_hid_get_item(struct usb_hid_data *s, struct usb_hid_item *h);

int32_t usb_hid_report_size(const void *buf, uint32_t len, enum usb_hid_kind k,
        uint8_t *id);

int32_t usb_hid_locate(const void *desc, uint32_t size, uint32_t usage,
        enum usb_hid_kind kind, uint8_t index, struct usb_hid_location *loc,
        uint32_t *flags, uint8_t *id);

int32_t usb_hid_get_data(const uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc);

uint32_t usb_hid_get_data_unsigned(const uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc);

void usb_hid_put_data_unsigned(uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc, uint32_t value);

int32_t usb_hid_is_collection(const void *desc, uint32_t size, uint32_t usage);

struct usb_hid_descriptor *usb_hid_get_descriptor_from_usb(
        struct usb_config_descriptor *cd, struct usb_interface_descriptor *id);

usb_error_t usb_hid_get_hid_descriptor(struct usb_hid_descriptor **ret_desc,
        uint16_t *ret_size, uint8_t iface_index);

usb_error_t usb_hid_get_report_descriptor(struct usb_hid_descriptor **d,
        uint16_t size, uint8_t iface);

usb_error_t usb_hid_set_idle(uint8_t iface, uint8_t duration, uint8_t id);

#endif /* LIBUSB_CLASS_HID_H_ */

