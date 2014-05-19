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
**      System: Parallel Distributed System
**        File: lbmsg.c
**      Author: Kees Schuerman
**      SccsId: "%W% %G%"
** Description: Message Passing System: Base Layer Interface
***********************************************************************/

/* LINTLIBRARY */

#include <pds.h>


/* 
** Port Primitives
*/

bmsg_ret_t
bport_familiar(port_id, familiar)
    bport_id_t port_id;
    bmsg_bool_t * familiar;
{
    return(BMSG_OK);
}

bmsg_ret_t
bport_port(port_id, bport)
    bport_id_t port_id;
    bport_t * bport;
{
    return(BMSG_OK);
}

bmsg_ret_t
bport_open(bport)
    bport_t * bport;
{    
    return(BMSG_OK);
}

bmsg_ret_t
bport_close(port_id)
    bport_id_t port_id;
{
    return(BMSG_OK);
}

bmsg_ret_t
bport_flush(port_id)
    bport_id_t port_id;
{
    return(BMSG_OK);
}

bmsg_ret_t
bport_block(port_id)
    bport_id_t port_id;
{
    return(BMSG_OK);
}

bmsg_ret_t
bport_unblock(port_id)
    bport_id_t port_id;
{
    return(BMSG_OK);
}

void 
bport_ack(port_id,port_primitive,ret)  
    bport_id_t port_id;
    bport_primitive_t port_primitive;
    bmsg_ret_t ret;
{
}

void 
bport_notify(port_id,port_primitive)  
    bport_id_t port_id;
    bport_primitive_t port_primitive;
{
}


/* 
** Message Primitives
*/

bmsg_ret_t
bmsg_alloc(size,data,msg)
    bmsg_size_t size;
    bmsg_data_t * * data;
    bmsg_t * msg;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmsg_free(msg)
    bmsg_t msg;
{
    return(BMSG_OK);
}

bmsg_size_t 
bmsg_size(msg)
    bmsg_t msg;
{
    return((bmsg_size_t) 0);
}

bmsg_data_t * 
bmsg_data(msg)
    bmsg_t msg;
{
    return((bmsg_data_t *) 0);
}

bmsg_ret_t
bmsg_send(port_id,msg,size)
    bport_id_t port_id;
    bmsg_t msg;
    bmsg_size_t size;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmsg_receive(msg,data,size,port_id,familiar)
    bmsg_t * msg;
    bmsg_data_t * * data;
    bmsg_size_t * size;
    bport_id_t * port_id;
    bmsg_bool_t * familiar;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmsg_peek(msg,data,size,port_id,familiar)
    bmsg_t * msg;
    bmsg_data_t * * data;
    bmsg_size_t * size;
    bport_id_t * port_id;
    bmsg_bool_t * familiar;
{
    return(BMSG_OK);
}

void 
bmsg_notify()  
{
}


/* 
** Memory Primitives
*/

void bmem_cpy(mem_dst_address,mem_src_address,mem_data_size)
    bmem_address_t mem_dst_address;
    bmem_address_t mem_src_address;
    bmem_size_t mem_data_size;
{
    return;
}

bmsg_ret_t
bmem_put(port_id,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    bport_id_t port_id;
    bmem_id_t * mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmem_get(port_id,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    bport_id_t port_id;
    bmem_id_t * mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    return(BMSG_OK);
}

void 
bmem_ack(mem_id,mem_primitive,ret)  
    bmem_id_t mem_id;
    bmem_primitive_t mem_primitive;
    bmsg_ret_t ret;
{
}

void 
bmem_notify(port_id,mem_primitive,mem_address,mem_data_size)  
    bport_id_t port_id;
    bmem_primitive_t mem_primitive;
    bmem_address_t mem_address;
    bmem_size_t mem_data_size;
{
}


/* 
** Miscellaneous Primitives
*/

bmsg_ret_t
bmsg_init(port_id,domain,option)
    bport_id_t port_id;
    bdomain_t * domain;
    bmsg_option_t option;
{
    return(BMSG_OK);
}

void
bmsg_exit()
{
}

bmsg_ret_t
bmsg_set_option(optname,optval)
    bmsg_optname_t optname;
    bmsg_optval_t optval;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmsg_get_option(optname,optval)
    bmsg_optname_t optname;
    bmsg_optval_t * optval;
{
    return(BMSG_OK);
}

bmsg_ret_t
bmsg_trigger(option)
    bmsg_option_t option;
{
    return(BMSG_OK);
}

void 
bproc_trigger(port) 
    bport_t * port;
{
}

void 
bmsg_warn(msg_warn,culprit)
    bmsg_warn_t msg_warn;
    bport_id_t culprit;
{
}

void 
bmsg_error(msg_error,culprit)	
    bmsg_error_t msg_error;
    bport_id_t culprit;
{
}

void 
bmsg_panic(msg_panic,culprit)  
    bmsg_panic_t msg_panic;
    bport_id_t culprit;
{
}

bmsg_ret_t 
bmsg_info(msg_info)  
    bmsg_info_t * msg_info;
{ 
    return(BMSG_OK); 
}

bmsg_ret_t
bmem_info(mem_info)
    bmem_info_t * mem_info;
{
    return(BMSG_OK); 
}

bmsg_ret_t
bport_info(port_id,port_info)
    bport_id_t port_id;
    bport_info_t * port_info;
{
    return(BMSG_OK); 
}


bmsg_ret_t 
bmem_address(msg_address,mem_address)
    bmsg_address_t msg_address;
    bmem_address_t * mem_address;
{
    return(BMSG_OK);
}

void 
bmsg_address(mem_address,msg_address)
    bmem_address_t mem_address;
    bmsg_address_t * msg_address;
{
}


void 
bmsg_perror(bret,s)
    bmsg_ret_t bret;
    char * s;
{
}


bmsg_ret_t 
bmsg_version(version)
    bmsg_version_t * version;
{
    return(BMSG_OK);
}

