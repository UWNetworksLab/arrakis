/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define ACPI_USE_STANDARD_HEADERS

#ifdef __x86_64__
#       define COMPILER_DEPENDENT_INT64   long
#       define COMPILER_DEPENDENT_UINT64  unsigned long
#       define ACPI_MACHINE_WIDTH 64
#elif defined(__i386__)
#       define COMPILER_DEPENDENT_INT64   long long
#       define COMPILER_DEPENDENT_UINT64  unsigned long long
#       define ACPI_MACHINE_WIDTH 32
#       define ACPI_USE_NATIVE_DIVIDE   // GCC on i386 has native 64-bit integers
#else
#       error Unknown architecture
#endif

#define ACPI_USE_SYSTEM_CLIBRARY
#define ACPI_FLUSH_CPU_CACHE()
#define ACPI_USE_LOCAL_CACHE

#ifndef ACPI_GET_FUNCTION_NAME
#define ACPI_GET_FUNCTION_NAME __FUNCTION__
#endif

#include <inttypes.h>
extern int acpi_acquire_global_lock(uint32_t *lock);
extern int acpi_release_global_lock(uint32_t *lock);

#define ACPI_SYSTEM_XFACE
#define ACPI_EXTERNAL_XFACE
#define ACPI_INTERNAL_XFACE
#define ACPI_INTERNAL_VAR_XFACE

#define ACPI_ASM_MACROS
#define BREAKPOINT3
#define ACPI_DISABLE_IRQS() assert(!"NYI: ACPI_DISABLE_IRQS");
#define ACPI_ENABLE_IRQS()  assert(!"NYI: ACPI_ENABLE_IRQS");
#define ACPI_ACQUIRE_GLOBAL_LOCK(GLptr, Acq) \
        do {(Acq) = acpi_acquire_global_lock(&((GLptr)->GlobalLock));} while(0)
#define ACPI_RELEASE_GLOBAL_LOCK(GLptr, Acq) \
        do {(Acq) = acpi_release_global_lock(&((GLptr)->GlobalLock));} while(0)
