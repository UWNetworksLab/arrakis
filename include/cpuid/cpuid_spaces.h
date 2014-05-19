/**
 * \file
 * \brief Address-space support for Mackerel CPUID device definitions
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CPUID_SPACES_H
#define CPUID_SPACES_H

static inline uint32_t cpuid_eax_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_eax_write_32(cpuid_t *dev, size_t offset,
//                                      uint32_t value);

static inline uint32_t cpuid_ebx_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_ebx_write_32(cpuid_t *dev, size_t offset,
//                                      uint32_t value);

static inline uint32_t cpuid_ecx_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_ecx_write_32(cpuid_t *dev, size_t offset,
//                                      uint32_t value);

static inline uint32_t cpuid_edx_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_edx_write_32(cpuid_t *dev, size_t offset,
//                                      uint32_t value);

static inline uint32_t cpuid_dcpa_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_dcpa_write_32(cpuid_t *dev, size_t offset,
//                                       uint32_t value);

static inline uint32_t cpuid_dcpb_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_dcpb_write_32(cpuid_t *dev, size_t offset,
//                                       uint32_t value);

static inline uint32_t cpuid_dcpc_read_32(cpuid_t *dev, size_t offset);
//static inline void cpuid_dcpc_write_32(cpuid_t *dev, size_t offset,
//                                       uint32_t value);

#endif // CPUID_SPACES_H

