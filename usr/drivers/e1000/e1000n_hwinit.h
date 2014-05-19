/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000_hw.h
 *
 *  Created on: Feb 13, 2013
 *      Author: mao
 */

#ifndef E1000_HWINIT_H_
#define E1000_HWINIT_H_


/* Default values for the transmit IPG register */
#define DEFAULT_825XX_TIPG_IPGT        10
#define DEFAULT_825XX_TIPG_IPGT_FIBER  9
#define DEFAULT_825XX_TIPG_IPGT_COPPER 8
#define DEFAULT_82544_TIPG_IPGT_FIBER  6
#define DEFAULT_82544_TIPG_IPGT_COPPER 8  

#define DEFAULT_82575_TIPG_IPGR1 8
#define DEFAULT_82542_TIPG_IPGR1 2
#define DEFAULT_82543_TIPG_IPGR1 10

#define DEFAULT_82575_TIPG_IPGR2 7
#define DEFAULT_82542_TIPG_IPGR2 10
#define DEFAULT_82543_TIPG_IPGR2 10

#define IGP_ACTIVITY_LED_MASK   0xFFFFF0FF

#endif /* E1000_HW_H_ */
