/**
 * \file
 * \brief ACPI daemon Flounder handler functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ACPI_SHARED_H_
#define ACPI_SHARED_H_

#include <acpi.h>
#include <if/acpi_defs.h>
#include <errors/errno.h>
#include <pci/confspace/pci_confspace.h>

#define BIOS_BITS       20
extern struct capref my_devframes_cnode;
extern struct mm pci_mm_physaddr;

errval_t find_all_apics(void);

int init_acpi(void);
ACPI_STATUS acpi_eval_integer(ACPI_HANDLE handle, char *name, ACPI_INTEGER *ret);
void acpi_get_irqtable_device(ACPI_HANDLE parent, acpi_pci_address_t device,
        ACPI_HANDLE *child, uint8_t bus);
void video_init(void);
void buttons_init(void);
void ec_probe_ecdt(void);
void ec_init(void);

void start_service(void);

extern bool vtd_force_off;

#endif /* ACPI_SHARED_H_ */
