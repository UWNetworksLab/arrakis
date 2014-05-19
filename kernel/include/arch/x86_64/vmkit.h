/**
 * \file
 * \brief VMKit Kernel interface.
 */

/*
 * Copyright (c) 2009, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VMKIT_H
#define VMKIT_H

#define VMKIT_ERR_OK        0
#define VMKIT_ERR_UNAVAIL   (-1)

errval_t vmkit_enable_virtualization (void);
int vmkit_disable_virtualization (void);
void __attribute__ ((noreturn)) vmkit_vmenter (struct dcb *dcb);
void __attribute__ ((noreturn)) vmkit_vmexec (struct dcb *dcb, lvaddr_t entry);

#endif // _VMKIT_H
