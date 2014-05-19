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

#include <usb_controller.h>
#include "usb_ehci.h"
#include "usb_ehci_memory.h"

/// which version of data structures we have to use
static usb_ds_size_t ds_size = USB_EHCI_DS_32BIT;

/*
 * the free lists
 */
static struct usb_ehci_itd *free_itd = NULL;
static struct usb_ehci_sitd *free_sitd = NULL;
static struct usb_ehci_qtd *free_qtd = NULL;
static struct usb_ehci_qh *free_qh = NULL;

static struct usb_page *used_pages = NULL;
static struct usb_page *free_pages = NULL;

/**
 * \brief this function sets the used ehci data structure size
 *        to 32 bit or 64 bit
 */
void usb_ehci_set_datastruct_size(usb_ds_size_t size)
{
    switch (size) {
        case USB_EHCI_DS_32BIT:
            ds_size = size;
            break;
        case USB_EHCI_DS_64BIT:
            //ds_size = size;
            assert( !"NYI: Support of 64 bit data structures "
            "not yet supported\n");
            break;
        default:
            debug_printf("ERROR: Unknown data structure size\n");
            break;
    }
}

/*
 * \brief   prints out the sizes of the used data structures
 */
void usb_ehci_print_datastruct_sizes(void)
{
    printf("sizeof(struct usb_ehci_itd) = %u\n", sizeof(struct usb_ehci_itd));
    printf("sizeof(struct usb_ehci_sitd) = %u\n", sizeof(struct usb_ehci_sitd));
    printf("sizeof(struct usb_ehci_qh) = %u\n", sizeof(struct usb_ehci_qh));
    printf("sizeof(struct usb_ehci_qtd) = %u\n", sizeof(struct usb_ehci_qtd));
    printf("sizeof(struct usb_ehci_fstn) = %u\n", sizeof(struct usb_ehci_fstn));
}

/*
 * \brief allocates an empty queue head descriptors without buffer pages
 *
 * \return pointer to a correctly aligned queue head pointer
 */
struct usb_ehci_qh *usb_ehci_qh_alloc(void)
{
    usb_ehci_qh_t *qh;

    /*
     * we have a free queue head left, so we can use it
     */
    if (free_qh != NULL) {
        qh = free_qh;
        free_qh = qh->obj_next;
        return (qh);
    }

    struct usb_page *page;

    /* get a free page or allocate a new one*/
    if (free_pages == NULL) {
        page = usb_mem_page_alloc();
    } else {
        page = free_pages;
        free_pages = page->next;
    }

    uint32_t size_qh = sizeof(struct usb_ehci_qh);
    uint32_t num_qh = page->free.size / size_qh;

    /* initiate the variables with the start addresses */
    qh = page->free.buffer;
    usb_paddr_t qh_self = page->free.phys_addr;

    for (uint32_t i = 0; i < num_qh; i++) {
        assert(!(qh_self % USB_EHCI_QH_ALIGN));
        USB_DEBUG_MEM(" Allocating QH: vaddr=%p, phys=%p (%x) [%x] (%u of %u)\n", qh,
                qh_self, qh_self % USB_EHCI_QH_ALIGN, size_qh, i, num_qh);
        /* reset memory */
        memset(qh, 0, size_qh);

        /* set the physical address */
        qh->qh_self = qh_self;

        /* put it into the free list */
        qh->obj_next = free_qh;
        free_qh = qh;

        qh++;
        qh_self += size_qh;
    }

    /* update free information */
    page->free.size -= (num_qh * size_qh);
    page->free.buffer += (num_qh * size_qh);
    page->free.phys_addr += (num_qh * size_qh);

    /* put page into free list */
    page->next = used_pages;
    used_pages = page;

    /* return a fresh qh */
    qh = free_qh;
    free_qh = qh->obj_next;

    return (qh);
}

/**
 * \brief   frees up a queue head descriptor but keeps the associated
 *          buffer pages
 *
 * \param   qh  the queue head to be freed
 */
void usb_ehci_qh_free(struct usb_ehci_qh *qh)
{
    /*
     * clear out all physical fields of this queue head
     * and set the link address to terminated
     */
    memset(qh, 0, 0x1B);
    qh->qh_link |= USB_EHCI_LINK_TERMINATE;
    qh->qh_alt_next_qtd |= USB_EHCI_LINK_TERMINATE;
    qh->qh_next_qtd |= USB_EHCI_LINK_TERMINATE;

    qh->bp_list[0].bp &= 0xFFFFF000;
    qh->bp_list[1].bp &= 0xFFFFF000;
    qh->bp_list[2].bp &= 0xFFFFF000;
    qh->bp_list[3].bp &= 0xFFFFF000;
    qh->bp_list[4].bp &= 0xFFFFF000;

    /*
     * clear out virtual pointers and add it to the free list
     */
    qh->next = NULL;
    qh->prev = NULL;
    qh->obj_next = free_qh;
    free_qh = qh;
}

struct usb_ehci_qtd *usb_ehci_qtd_alloc(void)
{
    usb_ehci_qtd_t *qtd;

    /*
     * we have a free queue transfer descriptor left, so we can use it
     */
    if (free_qtd != NULL) {
        qtd = free_qtd;
        free_qtd = qtd->obj_next;
        return (qtd);
    }

    /*
     * we have to populate our free queue element transfer descriptor
     */
    struct usb_page *page;

    if (free_pages == NULL) {
        page = usb_mem_page_alloc();
    } else {
        page = free_pages;
        free_pages = page->next;
    }

    uint32_t size_qtd = sizeof(struct usb_ehci_qtd);
    uint32_t num_qtd = page->free.size / size_qtd;

    qtd = page->free.buffer;
    usb_paddr_t qtd_self = page->free.phys_addr;

    for (uint32_t i = 0; i < num_qtd; i++) {
        assert(!(qtd_self % USB_EHCI_QTD_ALIGN));
        USB_DEBUG_MEM(" Allocating QTD: vaddr=%p, phys=%p (%x) [%x]\n", qtd,
                qtd_self, qtd_self % USB_EHCI_QTD_ALIGN, size_qtd);
        memset(qtd, 0, size_qtd);
        qtd->qtd_self = qtd_self;
        qtd->obj_next = free_qtd;
        free_qtd = qtd;

        qtd++;
        qtd_self += size_qtd;
    }

    page->free.size -= (num_qtd * size_qtd);
    page->free.buffer += (num_qtd * size_qtd);
    page->free.phys_addr += (num_qtd * size_qtd);

    page->next = used_pages;
    used_pages = page;

    qtd = free_qtd;
    free_qtd = qtd->obj_next;

    return (qtd);

}

void usb_ehci_qtd_free(struct usb_ehci_qtd *qtd)
{
    /*
     * clear out all physical fields except the buffer pointers
     */
    memset(qtd, 0, 0xB);
    qtd->qtd_alt_next |= USB_EHCI_LINK_TERMINATE;
    qtd->qtd_next |= USB_EHCI_LINK_TERMINATE;
    qtd->qtd_bp[0].address &= 0xFFFFF000;
    qtd->qtd_bp[1].address &= 0xFFFFF000;
    qtd->qtd_bp[2].address &= 0xFFFFF000;
    qtd->qtd_bp[3].address &= 0xFFFFF000;
    qtd->qtd_bp[4].address &= 0xFFFFF000;

    /*
     * clear out all virtual information
     */
    qtd->len = 0;
    qtd->alt_next = NULL;

    qtd->obj_next = free_qtd;
    free_qtd = qtd;
}

struct usb_ehci_sitd *usb_ehci_sitd_alloc(void)
{
    usb_ehci_sitd_t *sitd;

    /*
     * we have a free queue transfer descriptor left, so we can use it
     */
    if (free_sitd != NULL) {
        sitd = free_sitd;
        free_sitd = sitd->obj_next;
        return (sitd);
    }

    /*
     * we have to populate our free queue element transfer descriptor
     */
    struct usb_page *page;

    if (free_pages == NULL) {
        page = usb_mem_page_alloc();
    } else {
        page = free_pages;
        free_pages = page->next;
    }

    uint32_t size_sitd = sizeof(struct usb_ehci_sitd);
    uint32_t num_sitd = page->free.size / size_sitd;

    sitd = page->free.buffer;
    usb_paddr_t sitd_self = page->free.phys_addr;

    for (uint32_t i = 0; i < num_sitd; i++) {
        assert(!(sitd_self % USB_EHCI_SITD_ALIGN));
        USB_DEBUG_MEM(" Allocating SITD: vaddr=%p, phys=%p (%x) [%x]\n", sitd,
                sitd_self, sitd_self % USB_EHCI_SITD_ALIGN, size_sitd);
        memset(sitd, 0, size_sitd);
        sitd->sitd_self = sitd_self;
        sitd->obj_next = free_sitd;
        free_sitd = sitd;

        sitd++;
        sitd_self += size_sitd;
    }

    page->free.size -= (num_sitd * size_sitd);
    page->free.buffer += (num_sitd * size_sitd);
    page->free.phys_addr += (num_sitd * size_sitd);

    page->next = used_pages;
    used_pages = page;

    sitd = free_sitd;
    free_sitd = sitd->obj_next;

    return (sitd);
}
void usb_ehci_sitd_free(struct usb_ehci_sitd *sitd)
{
    memset(sitd, 0, 0xF);
    sitd->sitd_next |= USB_EHCI_LINK_TERMINATE;
    sitd->sitd_bp[0].address &= 0xFFFFF000;
    sitd->sitd_bp[0].address &= 0xFFFFF000;
    sitd->sitd_back_link |= USB_EHCI_LINK_TERMINATE;

    sitd->prev = NULL;
    sitd->next = NULL;

    sitd->obj_next = free_sitd;
    free_sitd = sitd;
}

struct usb_ehci_itd *usb_ehci_itd_alloc(void)
{
    usb_ehci_itd_t *itd;

    /*
     * we have a free queue transfer descriptor left, so we can use it
     */
    if (free_itd != NULL) {
        itd = free_itd;
        free_itd = itd->obj_next;
        return (itd);
    }

    /*
     * we have to populate our free queue element transfer descriptor
     */
    struct usb_page *page;

    if (free_pages == NULL) {
        page = usb_mem_page_alloc();
    } else {
        page = free_pages;
        free_pages = page->next;
    }

    uint32_t size_itd = sizeof(struct usb_ehci_itd);
    uint32_t num_itd = page->free.size / size_itd;

    itd = page->free.buffer;
    usb_paddr_t itd_self = page->free.phys_addr;

    for (uint32_t i = 0; i < num_itd; i++) {
        USB_DEBUG_MEM(" Allocating ITD: vaddr=%p, phys=%p (%x) [%x]\n", itd,
                itd_self, itd_self % USB_EHCI_ITD_ALIGN, size_itd);
        assert(!(itd_self % USB_EHCI_ITD_ALIGN));
        memset(itd, 0, size_itd);
        itd->itd_self = itd_self;
        itd->obj_next = free_itd;
        free_itd = itd;

        itd++;
        itd_self += size_itd;
    }

    page->free.size -= (num_itd * size_itd);
    page->free.buffer += (num_itd * size_itd);
    page->free.phys_addr += (num_itd * size_itd);

    page->next = used_pages;
    used_pages = page;

    itd = free_itd;
    free_itd = itd->obj_next;

    return (itd);
}

void usb_ehci_itd_free(struct usb_ehci_itd *itd)
{
    memset(itd, 0, 0x23);
    itd->itd_next |= USB_EHCI_LINK_TERMINATE;
    itd->itd_bp[0].address &= 0xFFFFF000;
    itd->itd_bp[1].address &= 0xFFFFF000;
    itd->itd_bp[2].address &= 0xFFFFF000;
    itd->itd_bp[3].address &= 0xFFFFF000;
    itd->itd_bp[4].address &= 0xFFFFF000;
    itd->itd_bp[5].address &= 0xFFFFF000;
    itd->itd_bp[6].address &= 0xFFFFF000;

    itd->next = NULL;
    itd->prev = NULL;

    itd->obj_next = free_itd;
    free_itd = itd;
}

usb_paddr_t usb_ehci_buffer_page_alloc(void)
{
    struct usb_page *page;
    if (free_pages != NULL) {
        /* get the free apge */
        page = free_pages;
        free_pages = page->next;

        /* put it into the used page */
        page->next = used_pages;
        used_pages = page;

        /* return address */
        return (page->page.phys_addr);
    }

    page = usb_mem_page_alloc();

    // page has to be 4k aligned
    assert(!(page->page.phys_addr & 0xFFF));

    page->next = used_pages;
    used_pages = page;

    return (page->page.phys_addr);
}

void usb_ehci_buffer_page_free(usb_paddr_t buf)
{
    struct usb_page *page = used_pages;
    struct usb_page *prev = page;
    while (page->next != NULL) {
        if (page->page.phys_addr == buf) {
            break;
        }
        prev = page;
        page = page->next;
    }

    if (page->page.phys_addr != buf) {
        /* page not found */
        debug_printf("WARNING: freeing up a page that was not allocated");
        return;
    }

    if (prev == page) {
        used_pages = page->next;
    } else {
        prev->next = page->next;
    }

    page->next = free_pages;
    free_pages = page;
}

void usb_ehci_pframes_alloc(usb_ehci_hc_t *hc)
{
    struct usb_page *page;

    page = usb_mem_page_alloc();

    assert(!(page->page.phys_addr % USB_EHCI_FRAMELIST_ALIGN));

    hc->pframes = page->page.buffer;
    hc->pframes_phys = page->page.phys_addr;

    page->next = used_pages;
    used_pages = page;
}

