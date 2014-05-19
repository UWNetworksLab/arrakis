/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000_helpers.c
 *
 *  Created on: Feb 14, 2013
 *      Author: mao
 */
#include "e1000n.h"

/*****************************************************************
 *
 *
 ****************************************************************/
e1000_mac_type_t e1000_get_mac_type(uint32_t vendor, uint32_t device_id)
{
    if (vendor == PCI_VENDOR_INTEL) {

        switch (device_id) {
        case E1000_DEVICE_82542:
            return e1000_82542;
        case E1000_DEVICE_82543GC_FIBER:
        case E1000_DEVICE_82543GC_COPPER:
            return e1000_82543;
        case E1000_DEVICE_82544EI_COPPER:
        case E1000_DEVICE_82544EI_FIBER:
        case E1000_DEVICE_82544GC_COPPER:
        case E1000_DEVICE_82544GC_LOM:
            return e1000_82544;
        case E1000_DEVICE_82540EM:
        case E1000_DEVICE_82540EM_LOM:
        case E1000_DEVICE_82540EP:
        case E1000_DEVICE_82540EP_LOM:
        case E1000_DEVICE_82540EP_LP:
            return e1000_82540;
        case E1000_DEVICE_82545EM_COPPER:
        case E1000_DEVICE_82545EM_FIBER:
            return e1000_82545;
        case E1000_DEVICE_82545GM_COPPER:
        case E1000_DEVICE_82545GM_FIBER:
        case E1000_DEVICE_82545GM_SERDES:
            return e1000_82545_rev_3;
        case E1000_DEVICE_82546EB_COPPER:
        case E1000_DEVICE_82546EB_FIBER:
        case E1000_DEVICE_82546EB_QUAD_COPPER:
            return e1000_82546;
        case E1000_DEVICE_82546GB_COPPER:
        case E1000_DEVICE_82546GB_FIBER:
        case E1000_DEVICE_82546GB_SERDES:
        case E1000_DEVICE_82546GB_PCIE:
        case E1000_DEVICE_82546GB_QUAD_COPPER:
        case E1000_DEVICE_82546GB_QUAD_COPPER_KSP3:
            return e1000_82546_rev_3;
        case E1000_DEVICE_82541EI:
        case E1000_DEVICE_82541EI_MOBILE:
        case E1000_DEVICE_82541ER_LOM:
            return e1000_82541;
        case E1000_DEVICE_82541ER:
        case E1000_DEVICE_82541GI:
        case E1000_DEVICE_82541GI_LF:
        case E1000_DEVICE_82541GI_MOBILE:
            return e1000_82541_rev_2;
        case E1000_DEVICE_82547EI:
        case E1000_DEVICE_82547EI_MOBILE:
            return e1000_82547;
        case E1000_DEVICE_82547GI:
            return e1000_82547_rev_2;
        case E1000_DEVICE_82563EB:
            return e1000_82563;
        case E1000_DEVICE_82571EB_COPPER:
        case E1000_DEVICE_82571EB_FIBER:
        case E1000_DEVICE_82571EB_SERDES:
        case E1000_DEVICE_82571EB_SERDES_DUAL:
        case E1000_DEVICE_82571EB_SERDES_QUAD:
        case E1000_DEVICE_82571EB_QUAD_COPPER:
        case E1000_DEVICE_82571EB_QUAD_FIBER:
        case E1000_DEVICE_82571EB_QUAD_COPPER_LOWPROFILE:
            return e1000_82571;
        case E1000_DEVICE_82572EI_COPPER:
        case E1000_DEVICE_82572EI_FIBER:
        case E1000_DEVICE_82572EI_SERDES:
        case E1000_DEVICE_82572EI:
            return e1000_82572;
        case E1000_DEVICE_82573E:
        case E1000_DEVICE_82573E_IAMT:
        case E1000_DEVICE_82573L:
            return e1000_82573;
        case E1000_DEVICE_82574L:
            return e1000_82574;
        case E1000_DEVICE_82575EB:
            return e1000_82575;
        case E1000_DEVICE_82576EG:
            return e1000_82576;
        default:
            E1000_DEBUG("Unsupported device: vendor: 0x%x,  device id: 0x%x\n", PCI_VENDOR_INTEL, device_id);
            return e1000_undefined;
        }
    }

    return e1000_undefined;
}


/*****************************************************************
 * allocate a single frame, mapping it into our vspace with given
 * attributes
 ****************************************************************/
void *alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap)
{
    struct capref frame;
    errval_t err;
    void *va;

    err = frame_alloc(&frame, size, NULL);
    if (err_is_fail(err)) {
        E1000_DEBUG("ERROR: frame_alloc failed.\n");
        return NULL;
    }

    err = vspace_map_one_frame_attr(&va, size, frame, attr, NULL, NULL);

    if (err_is_fail(err)) {
        E1000_DEBUG("Error: vspace_map_one_frame failed\n");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}
