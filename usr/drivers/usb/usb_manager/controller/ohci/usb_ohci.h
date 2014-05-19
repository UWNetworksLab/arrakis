/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * =======================================================================
 * This file contains the definitions of the data structures used by the
 * OHCI Host Controller. This data structures are hardware defined and
 * have to be outlined in this way. Additional fields can be added at the
 * end of the data structures
 * =======================================================================
 */

#ifndef USB_OHCI_H_
#define USB_OHCI_H_


#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_descriptor.h>
#include <usb_hub.h>

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Communication Area
 * ------------------------------------------------------------------------
 * A structure in shared main memory established by the Host
 * Controller Driver (HCD). This structure is used for
 * communication between the HCD and the Host Controller.
 * The HCD maintains a pointer to this structure in the Host
 * Controller.
 *
 * Alignment Constraints: must be 256 byte aligned
 *
 * Fields:
 *  - interrupt_table:  pointers to the interrupt ed lists there are 32 of
 *                      them at a granularity of 1ms each.
 *  - frame_number:     the current USB frame number
 *  - pad_1:            is set to 0 when the HC updates the frame number
 *  - done_head:        pointer to the completed transfer descriptors
 *  - reserved:         reserved for use by host controler
 */
#define USB_OHCI_NO_IRQS         32
#define USB_OHCI_HCCA_RESERVED  116
#define USB_OHCI_HCCA_ALIGN     256
#define USB_OHCI_HCCA_SIZE      256
struct usb_ohci_hcca {
    usb_paddr_t         hcca_interrupt_table[USB_OHCI_NO_IRQS];
    volatile uint16_t   hcca_frame_number;
    volatile uint16_t   hcca_pad_1;
    usb_paddr_t         hcca_done_head;
    volatile uint8_t    _reserved[USB_OHCI_HCCA_RESERVED];
} __aligned(USB_OHCI_HCCA_ALIGN);

// typedef for the HCCA struct
typedef struct usb_ohci_hcca usb_ohci_hcca_t;

/*
 * The LSb of hcca_done_head is set to 1 when there was an unmasked
 * HcInterruptStatus set at the time when hcca_done_head was written
 */
#define USB_OHCI_HCCA_UNMASKED_IRQ(hcca) (hcca->hcca_done_head & 0x1)



/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Endpoint Descriptor (Section 4.2)
 * ------------------------------------------------------------------------
 * A memory structure which describes information necessary for
 * the Host Controller to communicate (via Transfer
 * Descriptors) with a device Endpoint. An Endpoint Descriptor
 * includes a Transfer Descriptor pointer.
 *
 * Alignment Constraints: 16 byte aligned
 *
 * Fields:
 *  - ed_control:           control flags of the endpoint
 *      - max_packet_size:  maximum number of bytes supported (sent or receive)
 *      - td_format:        format of the transfer descriptors of this EP
 *      - skip:             flag indicating if the HC skips the EP
 *      - direction:        data flow direction: IN | OUT | TD-dependent
 *      - endpoint_number:  address of this EP within the USB device (function)
 *      - function_address: address of this USB device (function) on the USB
 *  - ed_tailP:             phys pointer to a transfer descriptor of this EP
 *  - ed_headP:             phys pointer to a transfer descriptor of this EP
 *                          + flags: HALTED | CARRY
 *  - ed_nextED:            physical pointer to the next EP descriptor
 *
 * Extra Fields:
 *  - next:             virtual pointer to the next ED of the list
 *  - prev:             virtual pointer to the previous ED of the list
 *  - obj_next:         virtual pointer to the next ED
 *  - ed_self:          physical address of this endpoint descriptor
 */
#define USB_OHCI_ED_ALIGN 16
#define USB_OHCI_ED_HW_SIZE 16

struct usb_ohci_ed_ctrl {
    volatile uint8_t  _unused : 5;
    volatile uint16_t max_packet_size : 11;
    volatile uint8_t  td_format  : 1;
    volatile uint8_t  skip  : 1;
    volatile uint8_t  speed  : 1;
    volatile uint8_t  direction  : 2;
    volatile uint8_t  endpoint_number : 4;
    volatile uint8_t  function_address : 7;
 };

struct usb_ohci_ed {
    struct usb_ohci_ed_ctrl  ed_control;
    usb_paddr_t              ed_tailP;
    usb_paddr_t              ed_headP;
    usb_paddr_t              ed_nextED;

    /* extra fields */
    struct usb_ohci_ed      *next;
    struct usb_ohci_ed      *prev;
    struct usb_ohci_ed      *obj_next;
    usb_paddr_t             ed_self;
}__aligned(USB_OHCI_ED_ALIGN);

typedef struct usb_ohci_ed       usb_ohci_ed_t;
typedef struct usb_ohci_ed_ctrl  usb_ohci_ed_ctrl_t;

// Dataflow directions of the endpoint
#define USB_OHCI_ED_DIRECTION_OUT      1
#define USB_OHCI_ED_DIRECTION_IN       2
#define USB_OHCI_ED_DIRECTION_FROM_TD  0
#define USB_OHCI_ED_DIRECTION_FROM_TD_ 3

// Possible formats of the transfer descriptors
#define USB_OHCI_ED_FORMAT_GENERAL 0
#define USB_OHCI_ED_FORMAT_ISOCHR  1

// Endpoint Speeds
#define USB_OHCI_ED_FULLSPEED 0
#define USB_OHCI_ED_LOWSPEED  1

// Endpoint halted or carry bits
#define USB_OHCI_EP_HALTED(_ep) (((_ep)->ed_headP) & 0x01)
#define USB_OHCI_EP_CARRY(_ep)  (((_ep)->ed_headP) & 0x02)

/*
 * check if the endpoint has transfer descriptors.
 * the TDs are 16 byte aligned, thus the last 4 bits are always zero,
 * we have to mask here, because the headP contains two flags.
 * if headP and tailP have the same value, then there is no TD
 */
#define USB_OHCI_EP_HAS_TD(headP, tailP) (((headP ^ tailP) & (~0xF)) != 0)


/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Transfer Descriptor (Section 4.3)
 * ------------------------------------------------------------------------
 * A memory structure which describes information necessary for
 * the Host Controller to transfer a block of data to or from a
 * device endpoint.
 *
 * Alignment Constraints: 16 byte aligned
 *
 * Fields:
 *  - td_control
 *      - condition_code:   status of the TD's last atempted transaction
 *      - error_count:      number of errors happened for this transaction
 *      - data_toggle:      updated after each successfull transmission
 *      - delay_interrupt:  delay / disable the completition interrupt
 *      - direction_pid:    direction of dataflow for this transfer
 *      - rounding:         flag indicating if the last packet must fit exactly
 *  - td_current_buffer:   physical pointer to the buffer
 *  - td_nextTD:           physical pointer to the next transfer descriptor
 *  - td_buffer_end:       physical pointer to the last byte in the buffer
 *
 * Extra Fields:
 *  - buf:              buffer space for small transfer (max. 64 bytes)
 *  - obj_next:         virtual pointer to the next td
 *  - alt_next:         alternative virtual  next pointer
 *  - td_self:          physical address of this endpoint
 *  - len:              length of the data block of this transfer
 */
#define USB_OHCI_TD_ALIGN 16

struct usb_ohci_td_ctrl {
    volatile uint8_t  condition_code : 4;
    volatile uint8_t  error_count : 2;
    volatile uint8_t  data_toggle : 2;
    volatile uint8_t  delay_interrupt : 3;
    volatile uint8_t  direction_pid : 2;
    volatile uint8_t  rounding : 1;
    volatile uint32_t _unused : 18;
};

struct usb_ohci_td {
    struct usb_ohci_td_ctrl td_control;
    usb_paddr_t             td_current_buffer;
    usb_paddr_t             td_nextTD;
    usb_paddr_t             td_buffer_end;

    /* extra fields */
    uint8_t                 buf[64];
    struct usb_ohci_td      *obj_next;
    struct usb_ohci_td      *alt_next;
    usb_paddr_t             td_self;
    uint32_t                len;
} __aligned(USB_OHCI_TD_ALIGN);

typedef struct usb_ohci_td_ctrl usb_ohci_td_ctrl_t;
typedef struct usb_ohci_td      usb_ohci_td_t;

// Transfer data flow direction
#define USB_OHCI_PID_SETUP      0
#define USB_OHCI_PID_IN         1
#define USB_OHCI_PID_OUT        2
#define USB_OHCI_PID_RESERVERD  3

// disable the interrupts for this transfer
#define USB_OHCI_TD_DISABLE_IRQ 7

// TD integrated buffer offset
#define USB_OHCI_TD_BUFFER_OFFSET 16
#define USB_OHCI_TD_BUFFER_SIZE 64

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Transfer Descriptor for ISOCHRONOUS transfers
 * ------------------------------------------------------------------------
 *
 * Alignment Constraints: must be 32 byte aligned
 *
 * Fields:
 *  - condition_code:   status of the TD's last atempted transaction
 *  - frame_count:      number of data packets in this TD (0=1, 7=8)
 *  - delay_interrupt:  delay / disable the completition interrupt
 *  - starting_frame:   frame number of the first data packet to be sent
 *  - buf_page_0:       physical page number of the first byte of the data buffer
 *  - nextTD:           pointer to the next isochr. transfer descriptor
 *  - buffer_end:       physical address of the last byte in the buffer
 *  - offset_psw:       offset or completition code of an isochr. data packet
 *      - condition_code:   the condition code of the isochronus packet
 *      - size:             length of the isochronus packet
 *
 * Extra Fields:
 *  - obj_next:         virtual pointer to the next itd
 *  - itd_self:         physical addres of this transfer descriptor
 *  - frames:           number of frames
 */
#define USB_OHCI_ISOCHRONUS_TD_OFFSETS   8
#define USB_OHCI_ISOCHRONUS_TD_ALIGN    32

union usb_ohci_td_offset {
    volatile uint16_t        offset;

    struct {
        volatile uint8_t  condition_code : 4;
        volatile uint8_t  zero : 1;
        volatile uint16_t size : 11;
    } psw;
};

struct usb_ohci_itd {
    volatile uint8_t          condiction_code : 4;
    volatile uint8_t          frame_count : 4;
    volatile uint8_t          delay_interrupt : 3;
    volatile uint8_t          unused : 5;
    volatile uint16_t         starting_frame;
    usb_paddr_t               buf_page_0;
    usb_paddr_t               nextTD;
    usb_paddr_t               buffer_end;
    union usb_ohci_td_offset  offset_psw[USB_OHCI_ISOCHRONUS_TD_OFFSETS];

    /* extra fields */
    struct ohci_itd           *obj_next;
    usb_paddr_t               itd_self;
    uint8_t                   frames;
}__aligned(USB_OHCI_ISOCHRONUS_TD_ALIGN);

typedef struct usb_ohci_itd     usb_ohci_itd_t;

// size and alignment constraints
#define USB_OHCI_OFFSET(id) (id + (1*(id % 2)) -(1*!(id % 2)))

// ohci status codes
#define USB_OHCI_STATUS_OK                    0x00
#define USB_OHCI_STATUS_CRC_ERROR             0x01
#define USB_OHCI_STATUS_BIT_STUFFING          0x02
#define USB_OHCI_STATUS_DATA_TOGGLE_MISMATCH  0x03
#define USB_OHCI_STATUS_STALL                 0x04
#define USB_OHCI_STATUS_DEVICE_NOT_RESPONDING 0x05
#define USB_OHCI_STATUS_PID_CHECK_FAILURE     0x06
#define USB_OHCI_STATUS_UNEXPECTED_PID        0x07
#define USB_OHCI_STATUS_DATA_OVERRUN          0x08
#define USB_OHCI_STATUS_DATA_UNDERRUN         0x09
#define USB_OHCI_STATUS_BUFFER_OVERRUN        0x0C
#define USB_OHCI_STATUS_BUFFER_UNDERRUN       0x0D

// Special delays we have to wait when changing certain registers
#define USB_OHCI_DELAY_POWER_ENABLE 5
#define USB_OHCI_DELAY_READ_DESCR   5

/*
 * there are 32 possible interrupt list. each of which consits of
 * interrupt EDs and an ISOCHR at the end.
 */
#define USB_OHCI_NO_EP_DESCRIPTORS (2*USB_OHCI_NO_IRQS)

#define USB_OHCI_PAGE_SIZE      0x1000
#define USB_OHCI_PAGE(x)        ((x) &~ 0xfff)
#define USB_OHCI_PAGE_OFFSET(x) ((x) & 0xfff)
#define USB_OHCI_PAGE_MASK(x)   ((x) & 0xfff)

// the OHCI controller supports maximum 127 devices
#define USB_OHCI_MAX_DEVICES 127

/* ENDPOINT TYPE CODES */
#define USB_OHCI_EP_TYPE_ISOC 0
#define USB_OHCI_EP_TYPE_INTR 1
#define USB_OHCI_EP_TYPE_CTRL 2
#define USB_OHCI_EP_TYPE_BULK 3


#define USB_OHCI_INTERRUPT_ENDPOINT 1

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * Fields:
 *  - status:  configuration descriptor
 *  - port_status:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
 union usb_ohci_hub_desc {
    struct usb_status status;
    struct usb_hub_port_status port_status;
    struct usb_hub_descriptor hub_descriptor;
    uint8_t temp[128];
};


/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * Fields:
 *  - confg_desc:  configuration descriptor
 *  - iface_desc:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
struct usb_ohci_config_desc {
    struct usb_config_descriptor config_desc;
    struct usb_interface_descriptor iface_desc;
    struct usb_endpoint_descriptor ep_desc;
} __packed;

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * TODO: REVISE
 *
 * Fields:
 *  - confg_desc:  configuration descriptor
 *  - iface_desc:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
typedef struct usb_ohci_hc {
    struct ohci_t *ohci_base;
    union usb_ohci_hub_desc root_hub_desc;
    uint8_t root_hub_num_ports;
    uint8_t root_hub_intr_data[32];
    uint8_t root_hub_address;
    uint8_t root_hub_config;
    uint32_t enabled_intrs; /* enabled interrupts */

    struct usb_device *devices[USB_OHCI_MAX_DEVICES];

    struct usb_ohci_hcca *hcca;
    struct usb_ohci_ed *qh_ctrl_last;
    struct usb_ohci_ed *qh_ctrl_first;
    struct usb_ohci_ed *qh_bulk_last;
    struct usb_ohci_ed *qh_bulk_first;
    struct usb_ohci_ed *qh_isoc_last;
    struct usb_ohci_ed *qh_intr_last[USB_OHCI_NO_EP_DESCRIPTORS];

    struct usb_ohci_ed *qh_ed_free;
    struct usb_ohci_td *qh_td_free;
    struct usb_ohci_itd *qh_itd_free;

    uint16_t intr_stats[USB_OHCI_NO_EP_DESCRIPTORS];  //keeps track of the interrupt transfes

    struct usb_host_controller *controller;

    uint16_t id_vendor;

    char vendor[16];

} usb_ohci_hc_t;

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Function Prototypes
 * ------------------------------------------------------------------------
 */
usb_error_t usb_ohci_init(usb_ohci_hc_t *sc, uintptr_t base);
void usb_ohci_detach(usb_ohci_hc_t *sc);
void usb_ohci_interrupt(usb_ohci_hc_t *sc);

#endif /* _USB_OHCI_H_ */
