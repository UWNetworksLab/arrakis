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
**        File: lnsrv.c
**      Author: Kees Schuerman
**      SccsId: "%W% %G%"
** Description: Name Server Interface
***********************************************************************/

/* LINTLIBRARY */

#include <pds.h>
#include <nsrv.h>


/* 
** Name Server Primitives
*/

nsrv_ret_t
nsrv_init(hostname,port_number)
    char * hostname;
    unsigned * port_number;
{
    return(NSRV_OK);
}

void
nsrv_exit()
{
}

char * 
nsrv_data_base()
{
    return((char *) 0);
}

char * 
nsrv_msg_base()
{
    return((char *) 0);
}

nsrv_ret_t
nsrv_aport_register(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    aport_t * port;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_aport_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_aport_look_up(key,name,port)
    nsrv_name_t key;
    nsrv_name_t name;
    aport_t * port;
{    
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bport_register(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bport_t * port;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bport_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bport_look_up(key,name,port)
    nsrv_name_t key;
    nsrv_name_t name;
    bport_t * port;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bdomain_register(key,name,signature,domain)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bdomain_t * domain;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bdomain_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_bdomain_look_up(key,name,domain)
    nsrv_name_t key;
    nsrv_name_t name;
    bdomain_t * domain;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_new_bport_id(signature,port_id)
    nsrv_name_t signature;
    bport_id_t * port_id;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_free_bport_id(signature,port_id)
    nsrv_name_t signature;
    bport_id_t port_id;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_new_bdomain_id(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t * domain_id;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_free_bdomain_id(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t domain_id;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_ping(hostname,port_number)
    char * hostname;
    unsigned * port_number;
{
    return(NSRV_OK);
}

nsrv_ret_t
nsrv_version(version)
    nsrv_version_t * version;
{
    return(NSRV_OK);
}

void
nsrv_perror(nret,s)
    nsrv_ret_t nret;
    char * s;
{
}

