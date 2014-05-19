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

#include <usb/usb.h>

#include <usb_memory.h>

#include "usb_ohci.h"
#include "usb_ohci_memory.h"

static struct usb_ohci_td *free_tds = NULL;
static struct usb_ohci_ed *free_eds = NULL;

static struct usb_page *free_pages = NULL;

static struct usb_ohci_hcca *hcca = NULL;
static usb_paddr_t hcca_phys = 0;

struct usb_ohci_hcca *usb_ohci_hcca_alloc(void)
{
    if (hcca != NULL) {
        return hcca;
    }

    // we do not have any free page anymore
    if (free_pages == NULL) {
        free_pages = usb_mem_page_alloc();
    }

    uint32_t ret_size;
    struct usb_memory_block mem;

    // allocate a new memory block in the usb page
    ret_size = usb_mem_next_block(USB_OHCI_HCCA_SIZE, USB_OHCI_HCCA_ALIGN,
            free_pages, &mem);

    /*
     * check if we have enough space, this may occur when we have just a few
     * bytes left in the page, allocate new page and try again
     */
    if (ret_size < USB_OHCI_HCCA_SIZE) {
        if (free_pages->free.size < sizeof(struct usb_ohci_ed)) {
            /*
             * TODO: memory waste
             */
        } else if (free_pages->free.size < 64) {
            /*
             * we can allocate a endpoint descriptor instead and store it
             */
            struct usb_ohci_ed *ed = usb_ohci_ed_alloc();
            ed->obj_next = free_eds;
            free_eds = ed;
        }
        // get a new page;
        free_pages = usb_mem_page_alloc();
    }

    /*
     * retry allocating a new hcca in the given block
     */
    ret_size = usb_mem_next_block(USB_OHCI_HCCA_SIZE, USB_OHCI_HCCA_ALIGN,
            free_pages, &mem);

    assert(ret_size >= USB_OHCI_HCCA_SIZE);

    hcca = (struct usb_ohci_hcca *) mem.buffer;
    hcca_phys = mem.phys_addr;

    return hcca;
}

usb_paddr_t usb_ohci_hcca_physaddr(void)
{
    if (hcca_phys != 0) {
        return hcca_phys;
    }
    usb_ohci_hcca_alloc();
    return hcca_phys;
}

/**
 * \brief   this function allocates a td descriptor used for the usb transfers
 *
 * \return  pointer to a usb_ohci_td struct
 */
struct usb_ohci_td *usb_ohci_td_alloc(void)
{
    struct usb_ohci_td * ret = NULL;

    /*
     * check if we have a free td left
     */
    if (free_tds != NULL) {
        ret = free_tds;
        free_tds = free_tds->obj_next;
        ret->obj_next = NULL;
        return ret;
    }

    uint32_t min_size = sizeof(struct usb_ohci_td);

    // we do not have any free page anymore
    if (free_pages == NULL) {
        free_pages = usb_mem_page_alloc();
    }

    uint32_t ret_size;
    struct usb_memory_block mem;

    // allocate a new memory block in the usb page
    ret_size = usb_mem_next_block(sizeof(struct usb_ohci_td), USB_OHCI_TD_ALIGN,
            free_pages, &mem);

    /*
     * check if we have enough space, this may occur when we have just a few
     * bytes left in the page, allocate new page and try again
     */
    if (ret_size < min_size) {
        if (free_pages->free.size < 32) {
            /*
             * TODO: memory waste
             */
        } else if (free_pages->free.size < 64) {
            /*
             * we can allocate a endpoint descriptor instead and store it
             */
            struct usb_ohci_ed *ed = usb_ohci_ed_alloc();
            ed->obj_next = free_eds;
            free_eds = ed;
        }
        // get a new page;
        free_pages = usb_mem_page_alloc();
    }

    /*
     * retry allocating a new td descriptor in the given block
     */
    ret_size = usb_mem_next_block(sizeof(struct usb_ohci_td), USB_OHCI_TD_ALIGN,
            free_pages, &mem);

    assert(ret_size >= min_size);

    // now we have memory for the td descriptor and we can set it up

    ret = (struct usb_ohci_td *) mem.buffer;
    ret->td_self = mem.phys_addr;
    ret->td_current_buffer = mem.phys_addr + USB_OHCI_TD_BUFFER_OFFSET;
    ret->td_buffer_end = ret->td_current_buffer + USB_OHCI_TD_BUFFER_SIZE;
    ret->obj_next = NULL;

    return ret;
}

void usb_ohci_td_free(struct usb_ohci_td *td)
{
    td->td_current_buffer = td->td_self + USB_OHCI_TD_BUFFER_OFFSET;
    td->td_buffer_end = td->td_current_buffer + USB_OHCI_TD_BUFFER_SIZE;
    td->td_nextTD = 0;
    td->alt_next = NULL;
    td->len = 0;
    td->obj_next = free_tds;
    free_tds = td;
}

struct usb_ohci_ed *usb_ohci_ed_alloc(void)
{
    struct usb_ohci_ed * ret = NULL;

    /*
     * check if we have a free td left
     */
    if (free_tds != NULL) {
        ret = free_eds;
        free_eds = free_eds->obj_next;
        ret->obj_next = NULL;
        return ret;
    }

    uint32_t min_size = sizeof(struct usb_ohci_ed);

    // we do not have any free page anymore
    if (free_pages == NULL) {
        free_pages = usb_mem_page_alloc();
    }

    uint32_t ret_size;
    struct usb_memory_block mem;

    // allocate a new memory block in the usb page
    ret_size = usb_mem_next_block(sizeof(struct usb_ohci_ed), USB_OHCI_ED_ALIGN,
            free_pages, &mem);

    /*
     * check if we have enough space, this may occur when we have just a few
     * bytes left in the page, allocate new page and try again
     */
    if (ret_size < min_size) {
        if (free_pages->free.size < 32) {
            /*
             * TODO: memory waste
             */
        }
        // get a new page;
        free_pages = usb_mem_page_alloc();
        /*
         * retry allocating a new td descriptor in the given block
         */
        ret_size = usb_mem_next_block(sizeof(struct usb_ohci_ed),
                USB_OHCI_ED_ALIGN, free_pages, &mem);
    }

    assert(ret_size >= min_size);

    // now we have memory for the td descriptor and we can set it up

    ret = (struct usb_ohci_ed *) mem.buffer;
    memset(ret, 0, sizeof(struct usb_ohci_ed));

    ret->ed_self = mem.phys_addr;

    return ret;

}

void usb_ohci_ed_free(struct usb_ohci_ed *ed)
{
    /*
     * delete the hardware part of the descriptor
     */
    memset(ed, 0, USB_OHCI_ED_HW_SIZE);
    ed->obj_next = free_eds;
    ed->prev = NULL;
    ed->next = NULL;
    free_eds = ed;
}

struct usb_ohci_itd *usb_ohci_itd_alloc(void)
{
    assert(!"NYI: allocating itd is not implemented. ");
    return NULL;
}

void usb_ohci_itd_free(struct usb_ohci_itd *td)
{
    assert(!"NYI: freeing itd is not implemented. ");
}

usb_paddr_t usb_ohci_buffer_alloc(uint32_t size, uint32_t align)
{
    return 0;
}

void usb_ohci_buffer_free(usb_paddr_t buf)
{

}
