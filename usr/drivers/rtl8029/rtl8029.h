/**
 * \file
 * \brief Realtek RTL8029(AS) driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RTL8029_H
#define RTL8029_H

#include <barrelfish/barrelfish.h>
#include <if/net_queue_manager_defs.h>
#include <pci/pci.h>
#include "rtl8029_debug.h"


#define RTL8029_IOBASE	0xC100
#define RTL8029_IOEND	0xC11f
#define RTL8029_IRQ		11

/// Use start of ASIC memory as transmit buffer
#define WRITE_PAGE      0x40

/// MTU is 1500 bytes, plus Ethernet header and CRC is max packet size
#define PACKET_SIZE     (1500 + 14 + 4)

/// Maximum packet size is write buffer size
#define WRITE_BUF_SIZE  PACKET_SIZE

/// Read buffer starts at page boundary after write buffer (0x4000 +
/// 1500)
#define READ_START_PAGE 0x46

/// Read buffer stops at end of ASIC memory
#define READ_STOP_PAGE  0xc0

/// Read buffer size in bytes (31232 bytes)
#define READ_BUF_SIZE   ((READ_STOP_PAGE - READ_START_PAGE) << 8)


#endif // RTL8029_H
