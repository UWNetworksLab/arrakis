/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_WAKEUP_H
#define KERNEL_WAKEUP_H

void wakeup_remove(struct dcb *dcb);
void wakeup_set(struct dcb *dcb, systime_t waketime);
void wakeup_check(systime_t now);
bool wakeup_is_pending(void);

#endif
