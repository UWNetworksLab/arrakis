/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**      System: PDS (Parallel Distributed System)
**              MPS (Message Passing System)
**        File: upcalls.c
**      Author: Kees Schuerman
** Description: Upcall routines common to the PDS and MPS
***********************************************************************/

#include "config.h"
#include <pds.h>	/* PDS Library Interface		      */

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>


/**********************************************************************
** AMSG Primitives
***********************************************************************/

/*ARGSUSED*/
void
amsg_warn(msg_warn,culprit)
    amsg_warn_t msg_warn;
    aport_id_t culprit;
{
#ifdef DEBUG_MPS
    (void) fprintf(stderr,"%d: amsg_warn: %d - culprit: %d ...\n", 
	    	   bport_self(),msg_warn,culprit);
#endif
}


void
amsg_error(msg_error,culprit)
    amsg_error_t msg_error;
    aport_id_t culprit;
{
    (void) fprintf(stderr,"%d: amsg_error: %d - culprit: %d ...\n", 
	           bport_self(),msg_error,culprit);
}


void
amsg_panic(msg_panic,culprit)
    amsg_panic_t msg_panic;
    aport_id_t culprit;
{
    (void) fprintf(stderr,"%d: amsg_panic: %d - culprit: %d ...\n", 
	           bport_self(),msg_panic,culprit);
}



/**********************************************************************
** BMSG Primitives
***********************************************************************/

#define BMSG_RETRIES_MAX         10000

#if defined(__STDC__)
extern void worker_bport_ack(bport_id_t port_id, 
                             bport_primitive_t port_primitive,
                             bmsg_ret_t ret);
extern void worker_bport_notify(bport_id_t port_id,
                                bport_primitive_t port_primitive);
#else
extern void worker_bport_ack();
extern void worker_bport_notify();
#endif


void
bport_ack(port_id,port_primitive,ret)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
    bmsg_ret_t ret;
{
    if (par_present())
	worker_bport_ack(port_id,port_primitive,ret);
}


void
bport_notify(port_id,port_primitive)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
{
    if (par_present())
	worker_bport_notify(port_id,port_primitive);
}


/*ARGSUSED*/
void
bmem_ack(mem_id,mem_primitive,ret)
    bmem_id_t mem_id;
    bmem_primitive_t mem_primitive;
    bmsg_ret_t ret;
{
#ifdef DEBUG_MPS
    (void) fprintf(stderr,"%d: bmem_ack: mem_id: %d mem_primitive:%d ret: %d\n",
                   bport_self(),mem_id,mem_primitive,ret);
#endif
}


/*ARGSUSED*/
void
bmem_notify(port_id,mem_primitive,mem_address,mem_data_size)
    bport_id_t port_id;
    bmem_primitive_t mem_primitive;
    bmem_address_t mem_address;
    bmem_size_t mem_data_size;
{
#ifdef DEBUG_MPS
    (void) fprintf(stderr,"%d: bmem_notify: port_id: %d %d %d %d\n",
                   bport_self(),port_id,mem_primitive,mem_address,mem_data_size);
#endif
}


void
bmsg_warn(msg_warn,culprit)
    bmsg_warn_t msg_warn;
    bport_id_t culprit;
{
    int retries;
    bport_t dummy;

    switch (msg_warn) {
	case BMSG_WEP_PORT :
	    retries = 0;
	    if (bport_port(culprit, &dummy) == BMSG_OK)
	    {
		while ((bport_close(culprit) != BMSG_NOPORT) && 
                   (retries++ < BMSG_RETRIES_MAX))
		    ;
	    }
	    break;
	default :
#ifdef DEBUG_MPS
    	    (void) fprintf(stderr,"%d: bmsg_warn: %d - culprit: %d ...\n", 
		           bport_self(),msg_warn,culprit);
#endif
	    break;
    }
}


void
bmsg_error(msg_error,culprit)
    bmsg_error_t msg_error;
    bport_id_t culprit;
{
    switch (msg_error) {
        case BMSG_WEP_PDIED :
            if (culprit == NSRV_BPORT_ID)
                (void) fprintf(stderr,"%d: bmsg_error: name server died !\n",
		               bport_self());
            else
                (void) fprintf(stderr,"%d: bmsg_error: bport %d died !\n",
                               bport_self(),culprit);
            return;
        default :
#ifdef DEBUG_MPS
    	    (void) fprintf(stderr,"%d: bmsg_error: %d - culprit: %d ...\n", 
		           bport_self(),msg_error,culprit);
#endif
            break;
    }
}


void
bmsg_panic(msg_panic,culprit)
    bmsg_panic_t msg_panic;
    bport_id_t culprit;
{
    (void) fprintf(stderr,"%d: bmsg_panic: %d - culprit: %d ...\n", 
	           bport_self(),msg_panic,culprit);
}


void
bproc_trigger(port)
    bport_t * port;
{
    if (port->bport_id == bport_self())
    {
        (void) bmsg_trigger(BMSG_INTRA_DOMAIN);
    }
    else if (kill((int) port->bpid, SIGIO) != 0)
    {
        (void) fprintf(stderr, "bport %d died\n", (int) port->bport_id);
    }
}


/**********************************************************************
** Miscellaneous
***********************************************************************/

void
msg_trigger()
{
    if (bmsg_ready())
        (void) bmsg_trigger((BMSG_INTER_DOMAIN | BMSG_INTRA_DOMAIN));
}


