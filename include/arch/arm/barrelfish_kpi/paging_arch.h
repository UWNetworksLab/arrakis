/**
 * \file
 * \brief Arch specific paging definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_PAGING_H
#define ARCH_ARM_BARRELFISH_KPI_PAGING_H

#if defined(__ARM_ARCH_5TEJ__)
#include <target/arm/barrelfish_kpi/paging_arm_v5.h>
#elif defined(__ARM_ARCH_5TE__)
#include <target/arm/barrelfish_kpi/paging_arm_v5.h>
#elif defined(__ARM_ARCH_7A__)
#include <target/arm/barrelfish_kpi/paging_arm_v7.h>
#elif defined(__ARM_ARCH_7M__)
#include <target/arm/barrelfish_kpi/paging_arm_v7m.h>
#else
#error "Missing ARM Paging header file"
#endif

#endif // ARCH_ARM_BARRELFISH_KPI_PAGING_H
