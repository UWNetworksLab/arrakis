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
#include <barrelfish/barrelfish.h>

#include "usb_ohci.h"
#include "usb_ohci_queue.h"






usb_ohci_ed_t *usb_ohci_remove_qh(usb_ohci_ed_t *ed, usb_ohci_ed_t *last)
{
    if (ed->prev) {
        ed->prev->next = ed->next;
        ed->prev->ed_nextED = ed->ed_nextED;

        // TODO: needs page cache flush ed->prev->page_cache
        if (ed->next) {
            ed->next->prev = ed->prev;
        }

        if (last == ed) {
            last = ed->prev;
            // TODO: needs page cache flush ed->next->page_cache
        }

        ed->prev = 0;

        // TODO: needs page cache flush ed->page_cache
    }

    return last;
}

usb_ohci_ed_t *usb_ohci_append_qh(usb_ohci_ed_t *ed, usb_ohci_ed_t *last)
{
    // check if the ED is not already linked
    if (ed->prev != NULL) {
        return last;
    }

    ed->next = last->next;
    ed->ed_nextED = last->ed_nextED;
    ed->ed_tailP = 0;

    ed->prev = last;

    // todo needs flush: ed->page_cache

    last->next = ed;
    last->ed_nextED = ed->ed_self;

    // todo needs flush: last->page_cache

    return ed;
}
