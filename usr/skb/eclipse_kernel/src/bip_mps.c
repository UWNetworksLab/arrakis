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
 * Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**      System: MPS (Message Passing System)
**        File: mps.c
**      Author: Kees Schuerman
***********************************************************************/

#include "config.h"
#include "sepia.h"
#include <pds.h>	/* PDS Library Interface		      */
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"

#include <unistd.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif


#define BmsgReturn(bret) {              \
    switch (bret) {                     \
        case BMSG_OK :                  \
        case BMSG_POPENED :             \
        case BMSG_PUNBLOCKED :          \
        case BMSG_POPENING :            \
        case BMSG_PCLOSING :            \
        case BMSG_PBLOCKING :           \
        case BMSG_PUNBLOCKING :         \
            Succeed_;                   \
        default :                       \
	    pds_error_string = bmsg_error_string(bret); \
            Bip_Error(MPS_ERROR);          \
    }                                   \
}


#define AmsgReturn(aret) {              \
    switch (aret) {                     \
        case AMSG_OK :                  \
            Succeed_;                   \
        default :                       \
	    pds_error_string = amsg_error_string(aret); \
            Bip_Error(MPS_ERROR);          \
    }                                   \
}


#define NsrvReturn(nret) {              \
    switch (nret) {                     \
        case NSRV_OK :                  \
            Succeed_;                   \
        default :                       \
	    pds_error_string = nsrv_error_string(nret); \
            Bip_Error(MPS_ERROR);          \
    }                                   \
}


/**********************************************************************
** MPS Process Connection Establishment
***********************************************************************/

#define MPS_RETRIES_MAX		10000



/**********************************************************************
** MPS Domains
***********************************************************************/

#define	DOMAIN_SIZE	0x800000	/* 8 MByte */



/**********************************************************************
** MPS Process Registration
***********************************************************************
** The primitive pds_init() registers the invoking process' bport in
** the name server. This is done under a name which is the string 
** representation of the process' bport identifier. Since bport 
** identifiers are unique, the name under which it is registered is 
** unique also. A session key is therefore not really necessary. The
** uniqueness of the signature is ensured by taking the combination
** of hostname and process identifier.
**
** signature: <hostname>.<pid>
**      name: <bport_self>
**       key: <DummyKey>		
***********************************************************************/

#define PID_MAX		999999
#define PID_LEN		6	
#define HOST_NAMELEN	(NSRV_NAMELEN - 1 - PID_LEN)

#define	DummyKey	""



/**********************************************************************
** Some Global Variables
***********************************************************************/

static nsrv_name_t mps_signature;
static nsrv_name_t mps_procname;
static int mps_initialised=0;
static int mps_nsrv_initialised=0;
static int mps_amsg_initialised=0;
static int mps_bmsg_initialised=0;

static char *pds_error_string = (char *) 0;

/**********************************************************************
** Error primitives
***********************************************************************/

int
p_mps_error(value v, type t)
{
    value vstr;
    if (!pds_error_string)
	Fail_;
    Cstring_To_Prolog(pds_error_string, vstr);
    pds_error_string = (char *) 0;
    Return_Unify_String(v, t, vstr.ptr);
}

/**********************************************************************
** Name Server Primitives
***********************************************************************/

int
p_mps_ping_1(value v_hostname, type t_hostname)
{
    unsigned portnumber;
    char * hostname;
    nsrv_ret_t nret;

    Get_Name(v_hostname, t_hostname, hostname);

    if (strlen(hostname) == 0) {
	Bip_Error(RANGE_ERROR);
    }

    portnumber = 0;

    nret = nsrv_ping(hostname,&portnumber);
    switch(nret)
    {
    case NSRV_OK :
        Succeed_;
    default:
    	Fail_;
    }
}



int
p_mps_ping_2(value v_hostname, type t_hostname, value v_portnumber, type t_portnumber)
{
    unsigned portnumber;
    char * hostname;
    nsrv_ret_t nret;

    Get_Name(v_hostname, t_hostname, hostname);
    Check_Integer(t_portnumber);

    if (v_portnumber.nint <= 0) {
        Bip_Error(RANGE_ERROR);
    }
    else
	portnumber = v_portnumber.nint;

    if (strlen(hostname) == 0) {
	Bip_Error(RANGE_ERROR);
    }

    nret = nsrv_ping(hostname,&portnumber);
    switch(nret)
    {
    case NSRV_OK :
        Succeed_;
    default:
    	Fail_;
    }
}


int
p_mps_port_register_4(value v_key, type t_key, value v_name, type t_name, value v_signature, type t_signature, value v_port, type t_port)
{
    char * key;
    char * name;
    char * signature;
    aport_t port;
    nsrv_ret_t nret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Get_Name(v_key, t_key, key);
    Get_Name(v_name, t_name, name);
    Get_Name(v_signature, t_signature, signature);
    Check_Integer(t_port);

    port.aport_id = v_port.nint;
    port.bport_id = aport_bport_id((aport_id_t) (v_port.nint));
    port.bdomain_id = bdomain_self();

    if (port.bport_id != bport_self())
	Bip_Error(MPS_ERROR);

    nret = nsrv_aport_register(key,name,signature,&port);
    NsrvReturn(nret);
}


int
p_mps_port_lookup_3(value v_key, type t_key, value v_name, type t_name, value v_port, type t_port)
{
    char * key;
    char * name;
    aport_t port;
    nsrv_ret_t nret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Get_Name(v_key, t_key, key);
    Get_Name(v_name, t_name, name);
    Check_Output_Integer(t_port);

    nret = nsrv_aport_look_up(key,name,&port);
    switch(nret)
    {
    case NSRV_OK :
        Return_Unify_Integer(v_port,t_port,port.aport_id);
    case NSRV_NOT_REGISTERED :
    	Fail_;
    default:
	pds_error_string = nsrv_error_string(nret);
        Bip_Error(MPS_ERROR);
    }
}


int
p_mps_port_deregister_3(value v_key, type t_key, value v_name, type t_name, value v_signature, type t_signature)
{
    char * key;
    char * name;
    char * signature;
    aport_t port;
    nsrv_ret_t nret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Get_Name(v_key, t_key, key);
    Get_Name(v_name, t_name, name);
    Get_Name(v_signature, t_signature, signature);

    nret = nsrv_aport_look_up(key,name,&port);
    if (nret == NSRV_OK) {
	if (port.bport_id != bport_self()) {
	    Bip_Error(MPS_ERROR);
	}
        nret = nsrv_aport_deregister(key,name,signature);
    }
    NsrvReturn(nret);
}



/**********************************************************************
** MPS Control Primitives
***********************************************************************/

static void
exit_mps(void)
{
    if (!mps_initialised)
    	return;

    (void) nsrv_bport_deregister(DummyKey,mps_procname,mps_signature);    
    (void) nsrv_free_bdomain_id(mps_signature,bdomain_self());
    (void) nsrv_free_bport_id(mps_signature,bport_self());
    if (mps_nsrv_initialised) {
	nsrv_exit();
	mps_nsrv_initialised = 0;
    }
    if (mps_amsg_initialised) {
	amsg_exit();
	mps_amsg_initialised = 0;
    }
    if (mps_bmsg_initialised) {
	bmsg_exit();
	mps_bmsg_initialised = 0;
    }
    mps_initialised = 0;
}


int
p_mps_exit_0(void)
{
    exit_mps();

    Succeed_;
}


int
p_mps_init_2(value v_hostname, type t_hostname, value v_portnumber, type t_portnumber)
{
    char localhostname[HOST_NAMELEN+1];
    char * hostname;
    unsigned portnumber;
    bdomain_id_t domain_id = 0;
    bport_id_t port_id;
    bdomain_t domain;
    bport_t port;
    nsrv_ret_t nret;
    bmsg_ret_t bret;
    amsg_ret_t aret;
 
    if (ec_options.parallel_worker)
	Bip_Error(NOT_IN_PARALLEL);

    if (mps_initialised)
	Bip_Error(MPS_ERROR);

    Get_Name(v_hostname, t_hostname, hostname);
    Check_Output_Integer(t_portnumber);

    if (IsInteger(t_portnumber)) {
	if (v_portnumber.nint <= 0) {
	    Bip_Error(RANGE_ERROR);
        }
	else
            portnumber = v_portnumber.nint;
    }
    else
        portnumber = 0;
    if (strlen(hostname) == 0) {
	Bip_Error(RANGE_ERROR);
    }

    if (gethostname(localhostname,HOST_NAMELEN+1) != 0) {
	Bip_Error(SYS_ERROR);
    }
    localhostname[HOST_NAMELEN] = '\0';

    (void) sprintf(mps_signature,
	    "%s.%6d",localhostname,getpid() % (PID_MAX+1));

    /* 
    ** Initialise NSRV 
    */

    if (!nsrv_ready()) {
        nret = nsrv_init(hostname,&portnumber);
        if (nret != NSRV_OK)
	    NsrvReturn(nret);
	mps_nsrv_initialised = 1;
    }
    else 
	    Bip_Error(MPS_ERROR);

    /* 
    ** Initialise BMSG 
    */

    if (!bmsg_ready()) {
	nret = nsrv_new_bdomain_id(mps_signature,&domain_id);
	if (nret != NSRV_OK) 
	    NsrvReturn(nret);
	nret = nsrv_new_bport_id(mps_signature,&port_id);
	if (nret != NSRV_OK) {
	    (void) nsrv_free_bdomain_id(mps_signature,domain_id);
	    NsrvReturn(nret);
	}
	domain.bdomain_id = domain_id;
	domain.bdomain_size = DOMAIN_SIZE;	
	if (!shared_mem_base())
	    domain.bdomain_start = (bmem_address_t) (shared_mem_base());
	else
	    domain.bdomain_start = (bmem_address_t)
				   (shared_mem_base() + DOMAIN_SIZE);
	domain.bdomain_start = 0;
	(void) sprintf(domain.bdomain_file,
	        "/tmp/mps.%d.map",domain_id);
	bret = bmsg_init(port_id,&domain,BDOMAIN_CREATE);
	if (bret != BMSG_OK) {
	    (void) nsrv_free_bdomain_id(mps_signature,domain_id);
	    (void) nsrv_free_bport_id(mps_signature,port_id);
	    BmsgReturn(bret);
	}
	mps_bmsg_initialised = 1;
    }

    /* 
    ** Initialise AMSG 
    */

    if (!amsg_ready()) {
	aret = amsg_init((unsigned) 0, 0, 0,0);
	if (aret != AMSG_OK) {
	    exit_mps();
	    AmsgReturn(aret);
	}
	mps_amsg_initialised = 1;
    }

    /* 
    ** Initialise NSRV Type System
    */

    nret = nsrv_types_init();
    if (nret != NSRV_OK) {
	exit_mps();
	NsrvReturn(nret);
    }

    bret = bport_port(bport_self(),&port);
    if (bret != BMSG_OK) {
	exit_mps();
	BmsgReturn(bret);
    }
    (void) sprintf(mps_procname,"%d",bport_self());

    nret = nsrv_bport_register(DummyKey,mps_procname,mps_signature,&port);    
    if (nret != NSRV_OK) {
	exit_mps();
	NsrvReturn(nret);
    }

    mps_initialised = 1;

    Return_Unify_Integer(v_portnumber,t_portnumber,portnumber);
}



/**********************************************************************
** MPS Port Primitives
***********************************************************************/

static void
port_notifier(aport_id_t port_id)
{
    pri		*proc;
    pword	*p = TG;
    value	mod;

    /*
    ** Get port's data pointer which points to client's
    ** port handler predicate PID
    */
    (void) aport_get_option(port_id,
                                APORT_DATA_PTR,
                                (aport_optval_t *) &proc);

    TG += 3;
    p[0].tag.kernel = TCOMP;
    p[0].val.ptr = p + 1;
    p[1].tag.kernel = TDICT;
    p[1].val.did = proc->did;
    p[2].tag.kernel = TINT;
    p[2].val.nint = (long) port_id;
    mod.did = proc->module_def;	/* call from the lookup module */

    (void) query_emulc(p->val, p->tag, mod, tdict);
}

int
p_mps_port_allocate_3(value v_notifier, type t_notifier, value v_portid, type t_portid, value vmod, type tmod)
{
    aport_id_t portid;
    dident	functor;
    pri		*proc;
    void 	(*notifier)();
    amsg_ret_t aret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Check_Output_Integer(t_portid);
    Get_Proc_Did(v_notifier, t_notifier, functor);

    proc = visible_procedure(functor, vmod.did, tmod, PRI_CREATE|PRI_REFER);
    if (proc == 0) {
	Bip_Error(NOENTRY)
    }
    if (functor == d_.true0 && proc->module_ref == d_.kernel_sepia)
	notifier = (void (*)()) 0;
    else
	notifier = port_notifier;

    aret = aport_allocate(&portid, notifier);
    if (aret != AMSG_OK) {
        AmsgReturn(aret);
    }
    if (notifier != (void (*)()) 0) {
	(void) aport_set_option(portid,
				    APORT_DATA_PTR,
				    (aport_optval_t) proc);
    }
    Return_Unify_Integer(v_portid,t_portid,portid);
}


int
p_mps_port_deallocate_1(value v_portid, type t_portid)
{
    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Check_Integer(t_portid);

    AmsgReturn(aport_deallocate((aport_id_t) (v_portid.nint)));
}



/**********************************************************************
** MPS Message Primitives
***********************************************************************/

int
p_mps_str_send_2(value v_portid, type t_portid, value v_str, type t_str)
{
    bport_t peer;
    aport_id_t portid;
    amsg_t msg;
    amsg_data_t * msg_data;
    amsg_count_t msg_count;
    static nsrv_name_t peername;
    int retries;
    nsrv_ret_t nret;
    amsg_ret_t aret;
    amsg_ret_t bret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Check_Integer(t_portid);
    Check_String(t_str);

    msg_count = StringLength(v_str) + 1;
    aret = amsg_alloc(msg_count,&msg_data,&msg);
    if (aret != AMSG_OK)
        AmsgReturn(aret);
    bmem_cpy((bmem_address_t) msg_data, (bmem_address_t) StringStart(v_str),
	msg_count);
    portid = v_portid.nint;
    aret = amsg_send(portid,msg,MDT_BYTE,msg_count,0);
    if (aret == AMSG_NOPORT) {
	(void) sprintf(peername,"%d",aport_bport_id(portid));
        nret = nsrv_bport_look_up(DummyKey,peername,&peer);    
        if (nret == NSRV_OK) {
	    retries = 0;
	    do 
		bret = bport_open(&peer);
	    while (((bret == BMSG_POPENING) || (bret == BMSG_PNOTAVAILABLE))
		   && (retries++ < MPS_RETRIES_MAX));
	    if (bret == BMSG_POPENED)
    		aret = amsg_send(portid,msg,MDT_BYTE,msg_count,0);
	    else {
        	(void) amsg_free(msg);
    		BmsgReturn(bret);
	    }
        }
    }
    if (aret != AMSG_OK)
        (void) amsg_free(msg);
    AmsgReturn(aret);
}


int
p_mps_str_receive_2(value v_portid, type t_portid, value v_str, type t_str)
{
    amsg_t msg;
    amsg_data_t * msg_data;
    amsg_type_t msg_type;
    amsg_count_t	msg_count;
    char	*buf;
    value v;
    amsg_ret_t aret;

    if (!mps_initialised)
	Bip_Error(MPS_ERROR);

    Check_Integer(t_portid);
    Check_Output_String(t_str);

    aret = amsg_receive((aport_id_t) (v_portid.nint),&msg,&msg_data,&msg_type,&msg_count,0);
    switch(aret)
    {
    case AMSG_OK:
    	break;
    case AMSG_NOMESSAGE:
    	Fail_;
    default:
	pds_error_string = amsg_error_string(aret);
    	Bip_Error(MPS_ERROR);
    }
    if (msg_type != MDT_BYTE) 
	Bip_Error(MPS_ERROR);
    Make_Stack_String(msg_count, v, buf);
    Copy_Bytes(buf, (char *) msg_data, msg_count);
    (void) amsg_free(msg);
    Return_Unify_String(v_str,t_str,v.ptr);
}

mps_present(void)
{
    return 1;
}

void
msg_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("mps_error", 1),
				p_mps_error, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("mps_ping", 1),
				p_mps_ping_1, B_SAFE|U_NONE);
	(void) built_in(in_dict("mps_ping", 2),
				p_mps_ping_2, B_SAFE|U_NONE);
	(void) built_in(in_dict("mps_port_register", 4),
				p_mps_port_register_4, B_SAFE|U_NONE);
	(void) built_in(in_dict("mps_port_deregister", 3),
				p_mps_port_deregister_3, B_SAFE|U_NONE);
	built_in(in_dict("mps_port_lookup", 3),
				p_mps_port_lookup_3, B_SAFE|U_SIMPLE)
		-> mode = BoundArg(3, CONSTANT);
	built_in(in_dict("mps_init", 2),
				p_mps_init_2, B_SAFE|U_SIMPLE)
		-> mode = BoundArg(2, CONSTANT);
	(void) built_in(in_dict("mps_exit", 0),
				p_mps_exit_0, B_SAFE|U_NONE);
	built_in(in_dict("mps_port_allocate", 3),
				p_mps_port_allocate_3, B_SAFE|U_SIMPLE)
		-> mode = BoundArg(1, CONSTANT);
	(void) built_in(in_dict("mps_port_deallocate", 1),
				p_mps_port_deallocate_1, B_SAFE|U_NONE);
	(void) exported_built_in(in_dict("mps_str_send", 2),
				p_mps_str_send_2, B_SAFE|U_NONE);
	exported_built_in(in_dict("mps_str_receive", 2),
				p_mps_str_receive_2, B_SAFE|U_SIMPLE)
		-> mode = BoundArg(2, CONSTANT);
    }
}


