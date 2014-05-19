/**
 * \brief Contains the physical memory locations of device registers in
 * the OMAP4 platform.
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVICE_REGISTERS_H_
#define DEVICE_REGISTERS_H_

#define OMAP44XX_CAM_CM2    0x4A009000
#define OMAP44XX_CAM_PRM    0x4A307000
#define OMAP44XX_FDIF       0x4A10A000

#define OMAP44XX_CM2        0x4A009300
#define OMAP44XX_CLKGEN_CM2 0x4A008100
#define OMAP44XX_L4PER_CM2  0x4A009400

#define OMAP44XX_DEVICE_PRM 0x4A307B00
#define OMAP44XX_INTRCONN_SOCKET_PRM 0x4A306000

// 256 BYTES
#define OMAP44XX_I2C3 0x48060000
#define OMAP44XX_I2C1 0x48070000
#define OMAP44XX_I2C2 0x48072000
#define OMAP44XX_I2C4 0x48350000

// 4K BYTES
#define OMAP44XX_SYSCTRL_GENERAL_CORE 0x4A002000
#define OMAP44XX_SYSCTRL_GENERAL_WAKEUP 0x4A30C000
#define OMAP44XX_SYSCTRL_PADCONF_CORE 0x4A100000
#define OMAP44XX_SYSCTRL_PADCONF_WAKEUP 0x4A31E000

#define OMAP44XX_HSUSBHOST  0x4A064000

// 4KB BYTES
#define OMAP44XX_MMCHS1 0x4809C000
#define OMAP44XX_MMCHS2 0x480B4000
#define OMAP44XX_MMCHS3 0x480AD000
#define OMAP44XX_MMCHS4 0x480D1000
#define OMAP44XX_MMCHS5 0x480D5000

// UARTs
#define OMAP44XX_MAP_L4_PER_UART1                       0x4806A000
#define OMAP44XX_MAP_L4_PER_UART1_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_UART2                       0x4806C000
#define OMAP44XX_MAP_L4_PER_UART2_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_UART3                       0x48020000
#define OMAP44XX_MAP_L4_PER_UART3_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_UART4                       0x4806E000
#define OMAP44XX_MAP_L4_PER_UART4_SIZE                  0x1000

#endif // DEVICE_REGISTERS_H_
