/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IPI_NOTIFY_H
#define IPI_NOTIFY_H

errval_t ipi_register_notification(capaddr_t ep, int chanid);
void ipi_handle_notify(void);
struct sysret ipi_raise_notify(coreid_t coreid, uintptr_t chanid);
void ipi_notify_init(void);

#endif
