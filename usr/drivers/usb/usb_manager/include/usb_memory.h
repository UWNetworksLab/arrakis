/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_MEMORY_H_
#define USB_MEMORY_H_

#include <barrelfish/barrelfish.h>
#include <usb/usb.h>

/* prototypes */
struct usb_xfer;

#define USB_PAGE_SIZE 0x1000 // 4k


/*
 * ------------------------------------------------------------------------
 * USB Memory Block
 * ------------------------------------------------------------------------
 * This data structures is used to associate a virtual address with the
 * respecting physical address
 *
 * Fields:
 *  - phys_addr:    physical address of the memory region
 *  - size:         size of the memory region
 *  - buffer:       virtual address of the memory region
 */
struct usb_memory_block {
    usb_paddr_t phys_addr;
    uint32_t size;
    void *buffer;
};


/*
 * ------------------------------------------------------------------------
 * USB memory page
 * ------------------------------------------------------------------------
 * This data structure is used for allocating the hardware descriptors
 * such as transfer descriptors and endpoint descriptors.
 *
 * Fields:
 *  - cap:          barrelfish frame capability
 *  - frame_id:     barrelfish frame id
 *  - phys_addr:    physial base address of the usb page
 *  - size:         size of the usb page
 *  - buffer:       virtual address of the buffer
 */
struct usb_page {
  struct capref cap;
  struct frame_identity frame_id;
  struct usb_memory_block page;
  struct usb_memory_block free;
  struct usb_page *next;
};


/*
 * ------------------------------------------------------------------------
 * USB memory page
 * ------------------------------------------------------------------------
 * This data structure is used for allocating buffers for device DMA transfers-
 *
 * Fields:
 *  - cap:          barrelfish frame capability
 *  - frame_id:     barrelfish frame id
 *  - phys_addr:    physial base address of the usb page
 *  - size:         size of the usb page
 *  - buffer:       virtual address of the buffer
 */
struct usb_dma_page {
    struct capref cap;
    struct frame_identity frame_id;
    usb_paddr_t phys_addr;
    uint32_t  size;
    void *buffer;
    struct usb_xfer *xfer;
    struct usb_dma_page *next;
};


/*
 * ------------------------------------------------------------------------
 * USB Memory Page List
 * ------------------------------------------------------------------------
 * This data structure is used for allocating buffers for device DMA transfers-
 *
 * Fields:
 *  - cap:          barrelfish frame capability
 *  - frame_id:     barrelfish frame id

 */
struct usb_page_list_entry {
    struct usb_page_list_entry *next;
    struct usb_page *page;
};





struct usb_page *usb_mem_page_alloc(void);
void usb_mem_page_free(struct usb_page *ret_page);

uint32_t usb_mem_next_block(uint32_t size, uint32_t align, struct usb_page *page,
                        struct usb_memory_block *ret_mem);

struct usb_dma_page *usb_mem_dma_alloc(uint32_t size, uint32_t align);
void usb_mem_dma_free(struct usb_dma_page *page);

void usb_mem_copy_in(struct usb_dma_page *pg, uint32_t offset, const void *data, uint32_t length);
void usb_mem_copy_out(struct usb_dma_page *pg, uint32_t offset, void *data, uint32_t length);
#endif /* USB_MEMORY_H_ */
