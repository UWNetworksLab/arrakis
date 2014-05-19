/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AMD_SVM_H
#define AMD_SVM_H

#include <stdint.h>
#include "amd_vmcb_dev.h"

/* EXITCODE */

#define SVM_VMEXIT_CR0_READ         0x0
#define SVM_VMEXIT_CR0_WRITE        0x10
#define SVM_VMEXIT_CR0_SEL_WRITE    0x65
#define SVM_VMEXIT_IDTR_WRITE       0x6a
#define SVM_VMEXIT_GDTR_WRITE       0x6b
#define SVM_VMEXIT_CPUID            0x72
#define SVM_VMEXIT_SWINT            0x75
#define SVM_VMEXIT_HLT              0x78
#define SVM_VMEXIT_IOIO             0x7b
#define SVM_VMEXIT_MSR              0x7c
#define SVM_VMEXIT_VMMCALL          0x81
#define SVM_VMEXIT_NPF              0x400

/* IO ACCESS FLAGS (EXITINFO1) */
#define SVM_IOIO_TYPE_MASK          (1 << 0)
#define SVM_IOIO_STR_MASK           (1 << 2)
#define SVM_IOIO_REP_MASK           (1 << 3)
#define SVM_IOIO_SZ8_MASK           (1 << 4)
#define SVM_IOIO_SZ16_MASK          (1 << 5)
#define SVM_IOIO_SZ32_MASK          (1 << 6)
#define SVM_IOIO_A16_MASK           (1 << 7)
#define SVM_IOIO_A32_MASK           (1 << 8)
#define SVM_IOIO_A64_MASK           (1 << 9)


/**
 * \brief Convenience macro to write real-mode segmentation registers
 *
 * Write the selector, base and limit to a selector reg according to real-mode
 * segmentation rules to the VMCB.
 */
#define VMCB_WRITE_SEGREG_REALMODE(vmcb,reg,selector)                   \
do {                                                                    \
    amd_vmcb_ ##reg## _selector_wr((vmcb), (selector));                 \
    amd_vmcb_ ##reg## _base_wr((vmcb), (selector) << 4);                \
    amd_vmcb_ ##reg## _limit_wr((vmcb), ((selector) << 4) + 0xffff);    \
} while (0)

#endif // AMD_SVM_H
