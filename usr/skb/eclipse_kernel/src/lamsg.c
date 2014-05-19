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
**        File: lamsg.c
**      Author: Kees Schuerman
**      SccsId: "%W% %G%"
** Description: Message Passing System: Application Layer Interface
***********************************************************************/

/* LINTLIBRARY */

#include <pds.h>


/* 
** Port Primitives
*/

amsg_ret_t 
aport_allocate(port_id,notify_procedure)
    aport_id_t * port_id;
    void (* notify_procedure)();
{ 
    return(AMSG_OK); 
}

amsg_ret_t
aport_deallocate(port_id)
    aport_id_t port_id;
{ 
    return((aport_id_t) 0); 
}

amsg_ret_t
aport_port(port_id,port)
    aport_id_t port_id;
    aport_t * port;
{ 
    return(AMSG_OK); 
}

aport_id_t
aport_id(bport_id,index)
    bport_id_t bport_id;
    unsigned index;
{
    return((aport_id_t) 0); 
}

bport_id_t
aport_bport_id(port_id)
    aport_id_t port_id;
{
    return((bport_id_t) 0);
}

amsg_ret_t
aport_flush(port_id)
    aport_id_t port_id;
{
    return(AMSG_OK);
}

amsg_ret_t
aport_set_option(port_id,optname,optval)
    aport_id_t port_id;
    aport_optname_t optname;
    aport_optval_t optval;
{
    return(AMSG_OK);
}

amsg_ret_t
aport_get_option(port_id,optname,optval)
    aport_id_t port_id;
    aport_optname_t optname;
    aport_optval_t * optval;
{
    return(AMSG_OK);
}


/* 
** Message Primitives
*/

amsg_ret_t
amsg_alloc(size,data,msg)
    amsg_size_t size;
    amsg_data_t * * data;
    amsg_t * msg;
{
    return(AMSG_OK);
}

amsg_ret_t
amsg_free(msg)
    amsg_t msg;
{
    return(AMSG_OK);
}

amsg_size_t
amsg_size(msg)
    amsg_t msg;
{
    return((amsg_size_t) 0);
}

amsg_data_t *
amsg_data(msg)
    amsg_t msg;
{
    return((amsg_data_t *) 0);
}

amsg_ret_t
amsg_send(port_id,msg,msg_type,msg_count,option)
    aport_id_t port_id;
    amsg_t msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_option_t option;
{
    return(AMSG_OK);
}

amsg_ret_t
amsg_receive(port_id,msg,msg_data,msg_type,msg_count,option)
    aport_id_t port_id;
    amsg_t * msg;
    amsg_data_t * * msg_data;
    amsg_type_t * msg_type;
    amsg_count_t * msg_count;
    amsg_option_t option;
{
    return(AMSG_OK);
}

amsg_ret_t
amsg_peek(port_id,msg,msg_data,msg_type,msg_count)
    aport_id_t port_id;
    amsg_t * msg;
    amsg_data_t * * msg_data;
    amsg_type_t * msg_type;
    amsg_count_t * msg_count;
{
    return(AMSG_OK);
}


/* 
** Miscellaneous Primitives
*/

amsg_ret_t
amsg_init(size,notify_procedure,port_id,option)
    unsigned size;
    void (* notify_procedure []) ();
    aport_id_t port_id [];
    amsg_option_t option;
{
    return(AMSG_OK);
}

void
amsg_exit()
{
}

void 
amsg_warn(msg_warn,culprit)
    amsg_warn_t msg_warn;
    aport_id_t culprit;
{
}

void 
amsg_error(msg_error,culprit)  
    amsg_error_t msg_error;
    aport_id_t culprit;
{
}

void 
amsg_panic(msg_panic,culprit)
    amsg_panic_t msg_panic;
    aport_id_t culprit;
{
}

amsg_ret_t
amsg_info(info)
    amsg_info_t * info;
{
    return(AMSG_OK);
}

amsg_ret_t
aport_info(port_id,info)
    aport_id_t port_id;
    aport_info_t * info;
{
    return(AMSG_OK);
}

void
amsg_perror(aret,s)
    amsg_ret_t aret;
    char * s;
{
}


amsg_ret_t
amsg_version(version)
    amsg_version_t * version;
{
    return(AMSG_OK);
}

