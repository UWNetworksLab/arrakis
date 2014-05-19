/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
//#include <barrelfish/nameservice_client.h>
//#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <net_device_manager/net_ports_service.h>
#include <net_device_manager/net_device_manager.h>

#include "port_management_support.h"
#include "device_manager_debug.h"


/****************************************************************
* Global datastructure
*****************************************************************/

/*****************************************************************
* Prototypes
*****************************************************************/

// find out the proper filter manager based on requested type
static struct filters_tx_vtbl *lookup_filt_mng(uint8_t filt_mng_type)
{
    switch(filt_mng_type) {
        case 0: // software filter manager
                return get_soft_filt_mng_sign();
                break;

        case 1: // e10K hardware filter manager
                return get_e10k_filt_mng_sign();
                break;
/*
        case 2: // Solarflare filter manager
                return NULL;
                break;
*/

        default: // Unknown filter manager
                USER_PANIC("Filter Manager type %"PRIu8" not supported\n",
                            filt_mng_type);
                abort();
                return NULL;
                break;

    } // end switch : for filter type
    return NULL;
} // end function: lookup_filt_mng


// initializes the hardware independent part of device manager
errval_t init_device_manager(char *dev_name, uint64_t valid_queues,
        uint8_t filt_mng_type)
{
    // making sure that parameters passed are sensible
    assert(dev_name != NULL);
    assert(valid_queues > 0);
    NDM_DEBUG("init_device_manager: called for dev[%s] with %"PRIu64" queues\n",
            dev_name, valid_queues);

    // making sure that this is the first call to this function
    assert(qlist == NULL);
    assert(total_queues == 0);

    // set the total queues
    total_queues = valid_queues;

    // TODO: memory from local NUMA domain
    qlist = (struct NIC_q_closure *)malloc (sizeof(struct NIC_q_closure) *
                total_queues );
    if (qlist == NULL) {
        USER_PANIC("init_dev_mng: Not enough memory (malloc failed)\n");
        return PORT_ERR_NOT_ENOUGH_MEMORY;
    }


    // Based on what device it is, choose proper filter_manager
    struct filters_tx_vtbl *filt_mng_ptr = lookup_filt_mng(filt_mng_type);


    // initialize closures for all queues
    memset(qlist, 0, (sizeof(struct NIC_q_closure) * total_queues));
    for (qid_t i = 0; i < total_queues; ++i) {
        qlist[i].qid = i;
        qlist[i].filt_mng = filt_mng_ptr;
    } // for each queue

    // Also, for shared queue (qid = 0), use soft_filt_mng
    qlist[0].filt_mng = lookup_filt_mng(0);

    return init_ports_service(dev_name);
//    return SYS_ERR_OK;
} // end function: init_device_manager

