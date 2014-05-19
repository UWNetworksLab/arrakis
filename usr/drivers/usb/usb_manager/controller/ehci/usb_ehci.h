/**
 * \brief this file contains general, type and function definitions for the
 *        EHCI controller
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_EHCI_H_
#define USB_EHCI_H_

// general include for types
#include <usb/usb.h>

// include for the ehci_t mackerel type
#include "ehci_device.h"

// include for the root hub
#include <usb_hub.h>

/// maximum devices supported by the host controller
#define USB_EHCI_MAX_DEVICES 128

/// the special interrupt endpoint
#define USB_EHCI_INTR_EP 1

/// the alignment of a frame used in the frame list
#define USB_EHCI_FRAMELIST_ALIGN   0x1000

/// the number of USB frames
#define USB_EHCI_FRAMELIST_COUNT     1024

/// the number of virtual USB frames
#define USB_EHCI_VFRAMELIST_COUNT     128

/*
 * Transfer Directions
 */

/// Transfer direction is output
#define USB_EHCI_DIRECTION_OUT 0

/// Transfer direction is input
#define USB_EHCI_DIRECTION_IN 1

/*
 * Structure Alignment Constraints for EHCI host controller, the EHCI specs
 * define certain alignment constraints that the hardware accessible data
 * structures must have
 */

/// EHCI Isochronus HS Transfer Descriptor (iTD) alignment
#define USB_EHCI_ITD_ALIGN 64

/// EHCI Split Isochronus Transfer Descriptor (siTD) alignment
#define USB_EHCI_SITD_ALIGN 64

/// EHCI Queue Element Transfer Descriptor (qTD) alignemnt
#define USB_EHCI_QTD_ALIGN 64

/// EHCI Queue Head Descriptor (QH) alignment
#define USB_EHCI_QH_ALIGN 128

/// Periodic Frame Span Traversal Node (FSTN) alignment
#define USB_EHCI_FSTN_ALIGN 32

/// Alignment of a USB EHCI buffer page
#define USB_EHCI_PAGE_ALIGN 4096

/// Size of a EHCI buffer page (4kb)
#define USB_EHCI_BUFFER_SIZE 0x1000

/*
 * --------------------------------------------------------------------------
 * Next Schedule Element Pointer Types
 * EHCI Specification, Section 3.3.1, Table 3-2
 * --------------------------------------------------------------------------
 *
 * This values indicate the type of the element referenced by the next link
 * pointer in the respective transfer chains.
 */

/// the number of bits the link type value is shifted
#define USB_EHCI_LINKTYPE_SHIFT 1

/// Type Isochronus Transfer Descriptor (iTD)
#define USB_EHCI_LINKTYPE_ITD   (0 << USB_EHCI_LINKTYPE_SHIFT)

/// Type Queue Head (QH)
#define USB_EHCI_LINKTYPE_QH    (1  << USB_EHCI_LINKTYPE_SHIFT)

/// Type Split Transaction Isochronus Transfer Descriptor (siTD)
#define USB_EHCI_LINKTYPE_SITD  (2  << USB_EHCI_LINKTYPE_SHIFT)

/// Frame Span Traversal Node
#define USB_EHCI_LINKTYPE_FSTN  (3  << USB_EHCI_LINKTYPE_SHIFT)

/// The link pointer field is valid
#define USB_EHCI_LINK_VALID     0

/// the link pointer field is terminate
#define USB_EHCI_LINK_TERMINATE 1

/*
 * ==========================================================================
 * ISOCHRONUS TRANSFER DESCRIPTOR
 * EHCI Specification, Section 3.3
 * ==========================================================================
 */

/*
 * iTD Transaction Status and Control List
 * EHCI Specification, Section 3.3.2, Table 3-3
 *
 * The host controller uses the information in each transaction description
 * plus the endpoint information contained in the first three dwords of the
 * Buffer Page Pointer list, to execute a transaction on the USB.
 */

/* iTD status field values */

/// Set by software to enable the execution of the transaction
#define USB_EHCI_ITD_STATUS_ACTIVE      0x8

/// Set by HC. Indicates a buffer overrun or under run of this transaction
#define USB_EHCI_ITD_STATUS_DATA_ERR    0x4

/// Set by HC The transaction generated a babble
#define USB_EHCI_ITD_STATUS_BABBLE      0x2

/// Set by HC. Transaction Error occurred because of bad response
#define USB_EHCI_ITD_STATUS_TRANS_ERR   0x1

/**
 * iTD status field bit representation.
 */
struct usb_ehci_itd_status {
    uint32_t offset :12;      ///< offset in the buffer in bytes
    uint32_t page_select :3;  ///< page buffer to use (0..6)
    uint32_t ioc :1;          ///< issue an interrupt upon completion
    uint32_t length :12;      ///< length of the data in bytes
    uint32_t status :4;       ///< transaction status record
};

/// iTD status field type
typedef struct usb_ehci_itd_status usb_ehci_itd_status_t;

/**
 * Representation of the buffer pointer field for the isochronus transfer
 * descriptor. The first 3 buffer points have a additional information stored
 * in the unused bits for the address.
 */
union usb_ehci_itd_bp {
    struct bp_0 {
        uint32_t device_addres :7;      ///< the device of for this TD
        uint32_t endpoint :4;           ///< then endpoint number for this TD
        uint32_t _reserved :1;          ///< reserved, has to be zero
        usb_paddr_t bp :20;             ///< physaddr of the buffer (4k aligned)
    } bp_0;                             ///< Representation for BP 0
    struct bp_1 {
        uint32_t max_packet_size :11;   ///< maximum packet size for this EP
        uint32_t direction :1;          ///< Direction of the transfer (1=IN)
        usb_paddr_t bp :20;             ///< physaddr of the buffer (4k aligned)
    } bp_1;                             ///< Representation for BP 1
    struct bp_2 {
        uint32_t multi :2;              ///< num of transactions per uFrame 0..3
        uint32_t _reserved :10;         ///< reserved, has to be zero
        usb_paddr_t bp :20;             ///< physaddr of the buffer (4k aligned)
    } bp_2;                             ///< Representation for BP 2
    usb_paddr_t address;                ///< Representation for BP 3..6
};

/// iTD buffer pointer type
typedef union usb_ehci_itd_bp usb_ehci_itd_bp_t;

/**
 * Isochronus (High-Speed) Transfer Descriptor (iTD)
 * EHCI Specification Section 3.3
 *
 * This structure is used only for high-speed isochronous endpoints. All other
 * transfer types should use queue structures. Isochronous TDs must be aligned
 * on a 32-byte boundary.
 */
struct usb_ehci_itd {
    /* hardware required fields */
    usb_paddr_t itd_next;                 ///< next link pointer
    usb_ehci_itd_status_t itd_status[8];  ///< status fields of the transfer
    usb_ehci_itd_bp_t itd_bp[7];          ///< buffer pointer list
    /* extra fields for sofware management */
    usb_paddr_t itd_self;                ///< physical address of this iTD
    struct usb_ehci_itd *next;           ///< virtual pointer of iTDs
    struct usb_ehci_itd *prev;           ///< virtual pointer of iTDs
    struct usb_ehci_itd *obj_next;       ///< virtual pointer to the next object
}__aligned(USB_EHCI_ITD_ALIGN);

/// EHCI Isochronus Transfer Descriptor Type
typedef struct usb_ehci_itd usb_ehci_itd_t;

/*
 * ==========================================================================
 * SPLIT ISOCHRONUS TRANSFER DESCRIPTOR
 * EHCI Specification, Section 3.4
 * ==========================================================================
 */

/**
 * siTD Endpoint Capabilities/Characteristics
 * EHCI Specification Section 3.4.2
 *
 * Dwords 1 and 2 specify static information about the full-speed endpoint,
 * the addressing of the parent transaction translator and micro-frame
 * scheduling control.
 */
struct usb_ehci_sitd_ep {
    uint32_t device_address :7; ///< the device address of this transfer
    uint32_t _reserved_3 :1;    ///< reserved, should be zero
    uint32_t endpoint :4;       ///< endpoint number of this transfer
    uint32_t _reserved_2 :4;    ///< reserved, should be zero
    uint32_t hub_address :7;    ///< device address of the high speed hub
    uint32_t _reserved_1 :1;    ///< reserved, should be zero
    uint32_t port_number :7;    ///< port number at the high speed hub
    uint32_t direction :1;      ///< direction of the transfer
    uint32_t s_mask :8;         ///< split start mask
    uint32_t c_mask :8;         ///< split competition mask
    uint32_t _reserved_4 :16;   ///< reserved, should be zero
};

/// siTD Endoint Capabilities/Characteristics state
typedef struct usb_ehci_sitd_ep usb_ehci_sitd_ep_t;

/**
 * siTD Transfer State
 * EHCI Specification Section 3.4.3
 *
 * Dwords 3-6 are used to manage the state of the transfer.
 */
struct usb_ehci_sitd_state {
    uint32_t status :8;      ///< status of the transaction executed
    uint32_t c_mask :8;      ///< frame complete-split progress mask
    uint32_t length :10;     ///< total bytes to transfer
    uint32_t _reserved :4;   ///< reserved, should be zero
    uint32_t page_select :1; ///< which data page pointer to use
    uint32_t ioc :1;         ///< interrupt on complete
};

/// siTD transaction state type
typedef struct usb_ehci_sitd_state usb_ehci_sitd_state_t;

/* siTD status field values */

/// siTD status active, set by software to start the transaction
#define USB_EHCI_SITD_STATUS_ACTIVE         0x80

/// siTD status error, set by HC if there was an error
#define USB_EHCI_SITD_STATUS_ERROR          0x40

/// siTD status data buffer error upon buffer overrun or underrun
#define USB_EHCI_SITD_STATUS_DATA_ERR       0x20

/// siTD status babble detected
#define USB_EHCI_SITD_STATUS_BABBLE         0x10

/// siTD status transaction error (e.g. timeout, crc, ..)
#define USB_EHCI_SITD_STATUS_TRANS_ERR      0x08

/// siTD status missed micro frame, HC missed a requered complete-split
#define USB_EHCI_SITD_STATUS_MISSED_FRAME   0x04

/// siTD status split transaction state 0=start splitt, 1=compelte split
#define USB_EHCI_SITD_STATUS_SPLIT          0x02

/// siTD status reserved bit 0
#define USB_EHCI_SITD_STATUS_RESERVED       0x01

/* Transaction position (TP) values */

/// Transaction Position: ALL, the entire FS payload is in this transaction
#define USB_EHCI_SITD_TP_ALL    0x0

/// Transaction Position BEGIN, first data payload of a FS transaction
#define USB_EHCI_SITD_TP_BEGIN  0x1

/// Transaction Position MID, the middle payload for a FS OUT transaction
#define USB_EHCI_SITD_TP_MID    0x2

/// Transaction Position END, the last payload for a FS OUT transaction
#define USB_EHCI_SITD_TP_END    0x3

/**
 * siTD Buffer Pointer List
 * EHCI Specification, Section 3.4.4
 * Dwords 4 and 5 are the data buffer page pointers for the transfer. This
 * structure supports one physical page cross. The most significant 20 bits
 * of each Dword in this section are the 4K (page) aligned buffer pointers.
 * The least significant 12 bits of each Dword are used as additional
 * transfer state.
 */
union usb_ehci_sitd_bp {
    struct usb_ehci_sitd_bp_0 {
        usb_paddr_t offset :12;     ///< offset into the page buffer
        usb_paddr_t bp :20;         ///< physical address of the buffer
    } bp_0;                         ///< representation for BP 0
    struct usb_ehci_sitd_bp_1 {
        uint32_t count :3;          ///< number of OUT start-split requests
        uint32_t tp :2;             ///< transaction position
        uint32_t _reserved :7;      ///< reserved, should be zero
        usb_paddr_t bp :20;         ///< physical address of the buffer
    } bp_1;                         ///< representation for BP 1
    usb_paddr_t address;
};

/// siTD buffer pointer list type
typedef union usb_ehci_sitd_bp usb_ehci_sitd_bp_t;

/// siTD back link validity check
#define USB_EHCI_SITD_BACK_LINK_VALID(sitd) \
    (!((sitd)->sitd_back_link) & 0x1))

/**
 * Split Transaction Isochronous Transfer Descriptor (siTD)
 * EHCI Specifiction Section 3.4
 *
 * All Full-speed isochronous transfers through Transaction Translators are
 * managed using the siTD data structure. This data structure satisfies the
 * operational requirements for managing the split transaction protocol.
 */
struct usb_ehci_sitd {
    /* hardware required fields */
    usb_paddr_t sitd_next;            ///< next link pointer
    usb_ehci_sitd_ep_t sitd_ep_char;  ///< endpoint characteristics
    usb_ehci_sitd_state_t sitd_state; ///< transaction state
    usb_ehci_sitd_bp_t sitd_bp[2];    ///< buffer pointer list
    usb_paddr_t sitd_back_link;       ///< back link
    /* extra fields for software managment */
    usb_paddr_t sitd_self;            ///< the physical address of this siTD
    struct usb_ehci_sitd *next;       ///< virtual pointer to next sitD
    struct usb_ehci_sitd *prev;       ///< virtual pointer to previous siTD
    struct usb_ehci_sitd *obj_next;   ///< virtual pointer to next obj
}__aligned(USB_EHCI_SITD_ALIGN);

/// EHCI siTD type
typedef struct usb_ehci_sitd usb_ehci_sitd_t;

/*
 * ==========================================================================
 * QUEUE ELEMENT TRANSFER DESCRIPTOR
 * EHCI Specification, Section 3.5
 * ==========================================================================
 */

/**
 * Queue Element Transfer Descriptor Token
 * EHCI Specification, Section 3.5.3
 *
 * The third DWORD of a queue element transfer descriptor contains most of the
 * information the host controller requires to execute a USB transaction (the
 * remaining endpoint-addressing information is specified in the queue head).
 */
struct usb_ehci_qtd_token {
    uint32_t status :8;        ///< the status of this qTD element
    uint32_t pid :2;           ///< pid code [OUT|IN|SETUP]
    uint32_t err_count :2;     ///< number of consecutive errors
    uint32_t current_page :3;  ///< index into the buffer pointer list
    uint32_t ioc :1;           ///< interrupt on complete
    uint32_t bytes :15;        ///< total bytes to transfer,
    uint32_t data_toggle :1;   ///< data toggle bit
};

typedef struct usb_ehci_qtd_token usb_ehci_qtd_token_t;

/// the maximum number of bytes to transfer with one qtd (16k)
#define USB_EHCI_QTD_MAX_BYTES 0x4000;

/* qTD PID Codes */

/// qTD PID for OUT token
#define USB_EHCI_QTD_PID_OUT    0x0

/// qTD PID for IN token
#define USB_EHCI_QTD_PID_IN     0x1

/// qTD PID for SETUP token
#define USB_EHCI_QTD_PID_SETUP  0x2

/* qTD Status Codes */

/// qTD Status active, set to one to start the transaction
#define USB_EHCI_QTD_STATUS_ACTIVE      0x80

/// qTD status halted, indicates a serious error
#define USB_EHCI_QTD_STATUS_HALTED      0x40

/// qTD status data buffer error, data over/under run
#define USB_EHCI_QTD_STATUS_DATA_ERR    0x20

/// qTD status babble, a babble was detected
#define USB_EHCI_QTD_STATUS_BABBLE      0x10

/// qTD status transaction error
#define USB_EHCI_QTD_STATUS_TRANS_ERR   0x08

/// qTD status missed micro frame
#define USB_EHCI_QTD_STATUS_MISS        0x04

/// qTD status split transaction state, 0=do start split, 1=do complete split
#define USB_EHCI_QTD_STATUS_SPLIT_STATE 0x02

/// qTD status ping, 1=do ping, 0=do OUT
#define USB_EHCI_QTD_STATUS_PING        0x01

/// checks the next field for validity of a qTD
#define USB_EHCI_QTD_VALID_NEXT(qtd) \
    (!(((next)->qtd_next) & 0x1))

/// checks the alternative next field of a qTD for validity
#define USB_EHCI_QTD_VALID_ALT(qtd) \
    (!(((next)->qtd_alt_next) & 0x1))

/**
 * defines the structure of the elements in the buffer pointer list inside
 * a qTD
 */
union usb_ehci_qtd_bp {
    struct usb_ehci_qtd_bp0 {
        uint16_t offset :12;      ///< offset into the memory region
        usb_paddr_t bp_list :20;  ///< physaddr of a 4k aligned memory region
    } bp_0;                       ///< representation for the first buffer page
    usb_paddr_t address;          ///< physaddr of a 5k aligned memory region
};

/// qTD buffer page type
typedef union usb_ehci_qtd_bp usb_ehci_qtd_bp_t;

/// the number of buffers in a qtd
#define USB_EHCI_QTD_NUM_BUFFERS 5

/**
 * Queue Element Transfer Descriptor (qTD)
 * EHCI Specification, Section 3.5
 *
 * This data structure is only used with a queue head (see Section 3.6). This
 * data structure is used for one or more USB transactions and can transfer up
 * to 20480 (5*4096) bytes.
 *
 * The buffer associated with this transfer must be virtually contiguous. The
 * buffer may start on any byte boundary. A separate buffer pointer list element
 * must be used for each physical page in the buffer, regardless of whether the
 * buffer is physically contiguous.
 */
struct usb_ehci_qtd {
    /* hardware required fields */
    usb_paddr_t qtd_next;           ///< phyaddr of the next qTD to process
    usb_paddr_t qtd_alt_next;       ///< phyaddr to the next data stream
    usb_ehci_qtd_token_t qtd_token; ///< various status fiels
    usb_ehci_qtd_bp_t qtd_bp[5];    ///< buffer pointer lists
    /* extra fields for software management */
    struct usb_ehci_qtd *alt_next;  ///< virtual pointer to the alt_next
    struct usb_ehci_qtd *obj_next;  ///< virtual pointer to the next object
    uint32_t qtd_self;              ///< physical address of this qTD
    uint32_t len;                   ///< length of data processed
}__aligned(USB_EHCI_QTD_ALIGN);

/// ehci queue element descriptor type
typedef struct usb_ehci_qtd usb_ehci_qtd_t;

/*
 * ==========================================================================
 * QUEUE HEAD TRANSFER DESCRIPTOR
 * EHCI Specification, Section 3.6
 * ==========================================================================
 */

/**
 * Queue Head Endpoint Characteristics / Capabilities
 * EHCI Specification, Section 3.6.2
 *
 * The second and third Dwords of a Queue Head specifies static information
 * about the endpoint. This information does not change over the lifetime of
 * the endpoint. There are three types of information in this region:
 *  - Endpoint Characteristics
 *  - Endpoint Capabilities
 *  - Split Transaction Characteristics
 */
struct usb_ehci_qh_ep {
    /* endpoint characteristics */
    uint32_t device_address :7;     ///< the device for this request
    uint32_t inactive :1;           ///< request active bit set to zero
    uint32_t ep_number :4;          ///< endpoint number for this request
    uint32_t ep_speed :2;           ///< endpoint speed
    uint32_t data_toggle_ctrl :1;   ///< initial data toggle from qTD
    uint32_t head_reclamation :1;   ///< this QH is the head of the reclamation
    uint32_t max_packet_size :11;   ///< the maximum packet size (max 1024)
    uint32_t is_control_ep :1;      ///< EP is control (for non HS EPs only)
    uint32_t nak_count_reload :4;   ///< value to re load the NAK fields
    /* endpoint capabilities */
    uint32_t irq_mask :8;           ///< interrupt schedule mask
    uint32_t complete_mask :8;      ///< split completition mask
    uint32_t hub_addr :7;           ///< address of the hub doint FS/LS trans
    uint32_t port_number :7;        ///< port number identifier of USB2.0 hub
    uint32_t mult :2;               ///< number of transactions issued
};

/// queue head endpoint characteristics type
typedef struct usb_ehci_qh_ep usb_ehci_qh_ep_t;

/* endpoint field speed values */

/// QH endpoint is full speed
#define USB_EHCI_QH_SPEED_FULL 0

/// QH endpoint is low speed
#define USB_EHCI_QH_SPEED_LOW  1

/// QH endpoint is high speed
#define USB_EHCI_QH_SPEED_HIGH 2

/// QH endpoint speed reserved value
#define USB_EHCI_QH_SPEED_RES  3

/// Extract the type information of the QHLP fields
#define USB_EHCI_QH_LINK_TYPE(qh) \
    ((((qh)->qh_link) >> 1) & 0x3)

/// Extract the NAK count of a queue head
#define USB_EHCI_QH_NAK_COUNT(qh) \
    ((((qh)->qh_alt_next_qtd) >> 1) & 0xF)

/**
 * Queue head status fields
 */
struct usb_ehci_qh_status {
    uint32_t status :8;          ///< status of the transfer
    uint32_t pid :2;             ///< PID of the transfer
    uint32_t err_count :2;       ///< error counter
    uint32_t current_page :3;    ///< current buffer page
    uint32_t ioc :1;             ///< interrupt on competition flag
    uint32_t bytes :15;          ///< length of the transfer in bytes
    uint32_t data_togglet :1;    ///< data toggle control
};

/// usb_ehci_qh_status_t type definition
typedef struct usb_ehci_qh_status usb_ehci_qh_status_t;

/**
 * EHCI Queue Head Buffer Page bit representations
 */
union usb_ehci_qh_bp {
    struct usb_ehci_qh_bp_0 {
        uint32_t offset :12;        ///< offset into the buffer
        usb_paddr_t address :20;    ///< address of the buffer page
    } bp_0;                         ///< format of buffer page 0
    struct usb_ehci_qh_bp_1 {
        uint32_t c_prog_mask :8;    ///< split transaction progress
        uint32_t _reserved :4;       ///< reserved, should be zero
        usb_paddr_t address :20;    ///< address of the buffer page
    } bp_1;                         ///< format of buffer page 1
    struct usb_ehci_qh_bp_2 {
        uint32_t tag :5;            ///< frame tag
        uint32_t bytes :7;          ///< keeps track of bytes sent / received
        usb_paddr_t address :20;    ///< address of the buffer page
    } bp_2;                         ///< format of buffer page 2
    usb_paddr_t bp;                 ///< address of the buffer page (4k align)
};

/// EHCI QH buffer pointer page type
typedef union usb_ehci_qh_bp usb_ehci_qh_bp_t;

/**
 * Queue Head Descriptor
 * EHCI Specification, Section 3.6
 *
 * Each transfer contains one queue head that has a list of qTDs linked to
 */
struct usb_ehci_qh {
    /* hardware required fields */
    usb_paddr_t qh_link;            ///< physaddr to the next data object (QHLP)
    usb_ehci_qh_ep_t qh_ep;         ///< EP characteristics for this transfer
    usb_paddr_t qh_curr_qtd;        ///< current queue element processed
    usb_paddr_t qh_next_qtd;        ///< the next queue element
    usb_paddr_t qh_alt_next_qtd;    ///< the next alternative queue element
    usb_ehci_qh_status_t qh_status; ///< status of the queue transfer
    usb_ehci_qh_bp_t bp_list[5];    ///< buffer pointer list
    /* extra fields for software management */
    struct usb_ehci_qh *next;       ///< virtual pointer to next element
    struct usb_ehci_qh *prev;       ///< virtual pointer to previous element
    struct usb_ehci_qh *obj_next;   ///< virtual pointer to next object
    usb_paddr_t qh_self;            ///< physical address of this QH
}__aligned(USB_EHCI_QH_ALIGN);

/// USB EHCI queue head type
typedef struct usb_ehci_qh usb_ehci_qh_t;

/*
 * ==========================================================================
 * PERIODIC FRAME SPAN TRAVERSAL NODE
 * EHCI Specification, Section 3.7
 * ==========================================================================
 */

/**
 * Periodic Frame Span Traversal Node (FSTN)
 * EHCI Specification, Section 3.7
 *
 * This data structure is to be used only for managing Full- and Low-speed
 * transactions that span a Host-frame boundary. Software must not use an FSTN
 * in the Asynchronous Schedule
 */
struct usb_ehci_fstn {
    usb_paddr_t fstn_link;    ///< physaddr of the next data object
    usb_paddr_t fstn_back;    ///< linkpointer to a queue head
}__aligned(USB_EHCI_FSTN_ALIGN);

/// EHCI Periodic Frame Span Traversal Node type
typedef struct usb_ehci_fstn usb_ehci_fstn_t;

/// extract the type of the normal path pointer field
#define USB_EHCI_FSTN_LINK_TYPE(fstn) \
    ((((fstn)->fstn_link) >> 1) & 0x3)
/// extract the type of back path pointer field
#define USB_EHCI_FSTN_BACK_TYPE(fstn) \
    ((((fstn)->fstn_back) >> 1) & 0x3)

/// validity check for the normal path pointer
#define USB_EHCI_FSTN_LINK_VALID(fstn) \
    (!(((fstn)->fstn_back) & 0x1))

/// validity check for the back path pointer
#define USB_EHCI_FSTN_BACK_VALID(fstn) \
    (!(((fstn)->fstn_back) & 0x1))

/*
 * ==========================================================================
 * USB EHCI HOST CONTROLLER SOFTWARE CONTROL
 * ==========================================================================
 */

/**
 * this data structure defines the structure of the EHCI root hub scratch buffer
 * used for serving root hub requests
 */
union usb_ehci_hub_descriptor {
    struct usb_status status;
    struct usb_hub_port_status port_status;
    struct usb_hub_descriptor hub_desc;
    uint8_t temp[128];
};

/**
 * EHCI Controller Software Control
 *
 * This data structure contains data for management of the EHCI controller
 */
struct usb_ehci_hc {
    struct ehci_t ehci_base;              ///< the mackerel device base
    usb_host_controller_t *controller;     ///< pointer to the generic HC
    uint32_t enabled_interrupts;           ///< the currently enabled interrupts
    uint16_t ehci_revision;                ///< host controller revision

    /* root hub */
    union usb_ehci_hub_descriptor rh_desc; ///< root hub descriptor scratch buf
    uint8_t rh_num_ports;                  ///< the number of root hub ports
    uint8_t rh_device_address;             ///< the root hub device address
    uint8_t rh_device_config;              ///< current root hub configuration
    char rh_vendor[16];                    ///< root hub vendor string
    uint8_t rh_reset;                      ///< root hub reset flag
    struct usb_device *rh_device;          ///< pointer to the root hub device

    /* devices */
    struct usb_device *devices[USB_EHCI_MAX_DEVICES];  ///< connected devices

    /* free lists of allocated structures */
    struct usb_ehci_qh *qh_free;           ///< pointer to free queue heads
    struct usb_ehci_sitd *sidt_free;       ///< pointer to free siTD
    struct usb_ehci_itd *itd_free;         ///< pointer to free iTD

    /* transfers */
    struct usb_ehci_qh *qh_async_last;      ///< pointer to the last async qh
    struct usb_ehci_qh *qh_async_first;     ///< pointer to the first async qh
    struct usb_ehci_qh *qh_terminate;       ///< pointer to terminator qh

    /* interrupt transfers */
    struct usb_ehci_qh *qh_intr_last[USB_EHCI_VFRAMELIST_COUNT];  ///< intr qh
    uint16_t qh_intr_stat[USB_EHCI_VFRAMELIST_COUNT];  ///< interrupt statistics

    /* isochronus transfer lists */
    struct usb_ehci_sitd *qh_sitd_fs_last[USB_EHCI_VFRAMELIST_COUNT];
    struct usb_ehci_itd *qh_itd_hs_last[USB_EHCI_VFRAMELIST_COUNT];
    usb_paddr_t pframes_phys;               ///< physical frames
    usb_paddr_t *pframes;                   ///< physical frames
};

/// USB EHCI host controller type
typedef struct usb_ehci_hc usb_ehci_hc_t;

/* Function Prototypes */

void usb_ehci_interrupt(usb_host_controller_t *host);
usb_error_t usb_ehci_init(usb_ehci_hc_t *hc, uintptr_t controller_base);
usb_error_t usb_ehci_hc_reset(usb_ehci_hc_t *hc);
usb_error_t usb_ehci_initialize_controller(usb_ehci_hc_t *hc);

#endif /* USB_EHCI_H_ */
