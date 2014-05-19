/**
 * \file
 * \brief Kernel scheduling API
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_SCHEDULE_H
#define KERNEL_SCHEDULE_H

struct dcb *schedule(void);
void make_runnable(struct dcb *dcb);
void scheduler_remove(struct dcb *dcb);
void scheduler_yield(struct dcb *dcb);
void scheduler_reset_time(void);

#endif
