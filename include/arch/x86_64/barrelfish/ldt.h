/**
 * \file
 * \brief "Public" interface to Barrelfish userland LDT
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _BARRELFISH_X86_64_LDT_H
#define _BARRELFISH_X86_64_LDT_H

errval_t ldt_alloc_segment(void *segbase, uint16_t *ret_selector);
errval_t ldt_update_segment(uint16_t selector, void *segbase);
errval_t ldt_free_segment(uint16_t selector);

#endif
