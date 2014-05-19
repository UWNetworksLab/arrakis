/**
 * \file
 * \brief Header file for classic 8259A PIC
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PIC_H
#define PIC_H

void pic_init(void);
bool pic_have_interrupt(int irq);
int pic_pending_interrupt(void);
void pic_eoi(int irq);
void pic_toggle_irq(int irq, bool enable);

#endif
