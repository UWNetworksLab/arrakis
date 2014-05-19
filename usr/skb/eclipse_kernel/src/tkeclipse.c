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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 *      System: Eclipse
 *
 *	$Id: tkeclipse.c,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 *
 *	Code for embedding eclipse into a tcl program
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <string.h>

#include <tcl.h>
#include "eclipse.h"

#include "tkcommon.h"

#ifdef __STDC__
int EcInit(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcCleanup(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcSetOption(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcPostString(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcPostGoal(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcPostEvent(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcResume(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcRunning(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcResumeStatus(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcHandleEvents(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcQueueWrite(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcQueueRead(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcQueueOpen(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcStreamNr(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
#endif


/*---------------------------------------------------------------------------
 * ec_init
 * ec_cleanup
 *---------------------------------------------------------------------------*/

int
EcInit(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    if (objc != 1)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "");
	return TCL_ERROR;
    }
    if (ec_init() != PSUCCEED)
    {
	Tcl_SetResult(interp, "couldn't initialize ECLiPSe", TCL_STATIC);
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
EcCleanup(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    if (objc != 1)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "");
	return TCL_ERROR;
    }
    ec_cleanup();
    return TCL_OK;
}

/*---------------------------------------------------------------------------
 * ec_set_option option_name option_val
 *---------------------------------------------------------------------------*/

int
EcSetOption(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    long option_id, option_val;
    int err;

    if (objc != 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "option_name option_value");
	return TCL_ERROR;
    }
    err = Tcl_GetLongFromObj(interp, objv[1], &option_id);
    if (err != TCL_OK)
    {
	char *option_name = Tcl_GetStringFromObj(objv[1], NULL);
	Tcl_ResetResult(interp);
	if (!strcmp(option_name, "argc")) option_id = EC_OPTION_ARGC;
	else if (!strcmp(option_name, "argv")) option_id = EC_OPTION_ARGV;
	else if (!strcmp(option_name, "localsize")) option_id = EC_OPTION_LOCALSIZE;
	else if (!strcmp(option_name, "globalsize")) option_id = EC_OPTION_GLOBALSIZE;
	else if (!strcmp(option_name, "privatesize")) option_id = EC_OPTION_PRIVATESIZE;
	else if (!strcmp(option_name, "sharedsize")) option_id = EC_OPTION_SHAREDSIZE;
	else if (!strcmp(option_name, "default_module")) option_id = EC_OPTION_DEFAULT_MODULE;
	else if (!strcmp(option_name, "eclipsedir")) option_id = EC_OPTION_ECLIPSEDIR;
	else if (!strcmp(option_name, "io")) option_id = EC_OPTION_IO;
	else {
	    Tcl_SetResult(interp, "integer expected", TCL_STATIC);
	    return TCL_ERROR;
	}
    }
    err = ec_set_option_ptr(option_id, NULL);
    if (err == PSUCCEED)	/* it's a valid string option */
    {
	char *s = Tcl_GetStringFromObj(objv[2], NULL);
	(void) ec_set_option_ptr(option_id, strcpy(Tcl_Alloc(strlen(s)+1), s));
    }
    else			/* it must be an integer option */
    {
	err = Tcl_GetLongFromObj(interp, objv[2], &option_val);
	if (err != TCL_OK)
	{
	    Tcl_SetResult(interp, "integer expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	err = ec_set_option_long(option_id, option_val);
	if (err != PSUCCEED)
	{
	    Tcl_SetResult(interp, "invalid option number", TCL_STATIC);
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

/*---------------------------------------------------------------------------
 * ec_post_event event_string
 *---------------------------------------------------------------------------*/

int
EcPostEvent(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    if (objc != 2)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "event_string");
	return TCL_ERROR;
    }
    if (ec_post_event_string(Tcl_GetStringFromObj(objv[1], NULL)) != PSUCCEED)
    {
	Tcl_SetResult(interp, "could not post event to ECLiPSe", TCL_STATIC);
	return TCL_ERROR;
    }
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * ec_resume
 *---------------------------------------------------------------------------*/

int
EcResume(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    int async, res;
    long arg = 0;
    Tcl_Obj *obj;

    if (objc == 1)
    {
	async = 0;
    }
    else if (objc == 2)
    {
	int res = Tcl_GetBooleanFromObj(interp, objv[1], &async);
	if (res != TCL_OK) {
	    Tcl_SetResult(interp, "ec_resume_: boolean expected", TCL_STATIC);
	    return TCL_ERROR;
	}
    }
    else
    {
	Tcl_WrongNumArgs(interp, 1, objv, "?async?");
	return TCL_ERROR;
    }
    /* ec_resume_async() can only return PSUCCEED, PRUNNING or SYS_ERROR */
    res =  async ? ec_resume_async() : ec_resume_long(&arg);
    switch (res)
    {
    case PSUCCEED:
	Tcl_SetResult(interp, "success", TCL_STATIC);
	return TCL_OK;
    case PFAIL:
	Tcl_SetResult(interp, "fail", TCL_STATIC);
	return TCL_OK;
    case PTHROW:
	Tcl_SetResult(interp, "throw", TCL_STATIC);
	return TCL_OK;
    case PRUNNING:
	Tcl_SetResult(interp, "running", TCL_STATIC);
	return TCL_OK;
    case PYIELD:
	obj = Tcl_NewStringObj("yield", -1);
	break;
    case PWAITIO:
	obj = Tcl_NewStringObj("waitio", -1);
	break;
    case PFLUSHIO:
	obj = Tcl_NewStringObj("flushio", -1);
	break;
    default:
	Tcl_SetResult(interp, async
		? "could not start ECLiPSe thread in ec_resume_async()"
		: "unrecognized return code from ec_resume()", TCL_STATIC);
	return TCL_ERROR;
    }
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewLongObj(arg));
    Tcl_SetObjResult(interp, obj);
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * ec_handle_events
 * Ignore posted goals, don't continue -> only handle events
 * Event handlers must not fail, throw or yield.
 * Allowed is only success, waitio, flushio.
 * In case of previous ec_resume_async we may also still be running.
 *---------------------------------------------------------------------------*/

int
EcHandleEvents(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    long arg;
    Tcl_Obj *obj;
    int res;

    if (objc != 1)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "");
	return TCL_ERROR;
    }

    res = ec_handle_events(&arg);
    switch (res)
    {
    case PRUNNING:
	Tcl_SetResult(interp, "running", TCL_STATIC);
	return TCL_OK;
    case PSUCCEED:
	Tcl_SetResult(interp, "success", TCL_STATIC);
	return TCL_OK;
    case PWAITIO:
	obj = Tcl_NewStringObj("waitio", -1);
	break;
    case PFLUSHIO:
	obj = Tcl_NewStringObj("flushio", -1);
	break;

    case PFAIL:
	Tcl_SetResult(interp, "fail", TCL_STATIC);
	return TCL_ERROR;
    case PTHROW:
	Tcl_SetResult(interp, "throw", TCL_STATIC);
	return TCL_ERROR;
    case PYIELD:
	Tcl_SetResult(interp, "yield", TCL_STATIC);
	return TCL_ERROR;
    default:
	Tcl_SetResult(interp, "unrecognized return code from ec_handle_events()", TCL_STATIC);
	return TCL_ERROR;
    }
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewLongObj(arg));
    Tcl_SetObjResult(interp, obj);
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * ec_running
 * returns a boolean indicating whether an eclipse thread is running.
 *---------------------------------------------------------------------------*/

int
EcRunning(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    if (objc != 1)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "");
	return TCL_ERROR;
    }
    Tcl_SetObjResult(interp, Tcl_NewBooleanObj(ec_running()));
    return TCL_OK;
}

/*---------------------------------------------------------------------------
 * ec_resume_status ?timeout?
 * returns (again) the status of the last ec_resume or ec_handle_events
 *---------------------------------------------------------------------------*/

int
EcResumeStatus(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    long arg;
    Tcl_Obj *obj;
    int timeout;
    int err;

    if (objc == 1)
    {
	timeout = 0;
    }
    else if (objc == 2)
    {
	err = Tcl_GetIntFromObj(interp, objv[1], &timeout);
	if (err != TCL_OK)
	{
	    Tcl_SetResult(interp, "ec_resume_status: integer expected", TCL_STATIC);
	    return TCL_ERROR;
	}
    }
    else
    {
	Tcl_WrongNumArgs(interp, 1, objv, "?timeout?");
	return TCL_ERROR;
    }
    switch (ec_wait_resume_status_long(&arg, timeout))
    {
    case PSUCCEED:
	Tcl_SetResult(interp, "success", TCL_STATIC);
	return TCL_OK;
    case PFAIL:
	Tcl_SetResult(interp, "fail", TCL_STATIC);
	return TCL_OK;
    case PTHROW:
	Tcl_SetResult(interp, "throw", TCL_STATIC);
	return TCL_OK;
    case PRUNNING:
	Tcl_SetResult(interp, "running", TCL_STATIC);
	return TCL_OK;
    case PYIELD:
	obj = Tcl_NewStringObj("yield", -1);
	break;
    case PWAITIO:
	obj = Tcl_NewStringObj("waitio", -1);
	break;
    case PFLUSHIO:
	obj = Tcl_NewStringObj("flushio", -1);
	break;
    default:
	Tcl_SetResult(interp, "unrecognized return code from ec_resume_status()", TCL_STATIC);
	return TCL_ERROR;
    }
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewLongObj(arg));
    Tcl_SetObjResult(interp, obj);
    return TCL_OK;
}

/*---------------------------------------------------------------------------
 * read/write directly from/to ECLiPSe queue
 *---------------------------------------------------------------------------*/

int
EcQueueWrite(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    int stream_nr, len;
    char *s;
    int err;

    if (objc != 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "eclipse_name data");
	return TCL_ERROR;
    }

    /* get the eclipse stream number, objv[1] is number or string */
    err = Tcl_GetIntFromObj(interp, objv[1], &stream_nr);
    if (err != TCL_OK)
    {
	stream_nr = ec_stream_nr(Tcl_GetStringFromObj(objv[1], NULL));
	if (stream_nr < 0)
	{
	    Tcl_SetResult(interp, "ec_queue_write: no such ECLiPSe stream", TCL_STATIC);
	    return TCL_ERROR;
	}
	Tcl_ResetResult(interp);
    }
    s = Tcl_GetByteArrayFromObj(objv[2], &len);
    if (ec_queue_write(stream_nr, s, len) < 0)
    {
	Tcl_SetResult(interp, "ec_queue_write: cannot write ECLiPSe stream", TCL_STATIC);
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
EcQueueRead(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    Tcl_Obj *result;
    int stream_nr, len;
    int err;

    if (objc != 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "eclipse_name size");
	return TCL_ERROR;
    }

    /* get the eclipse stream number, objv[1] is number or string */
    err = Tcl_GetIntFromObj(interp, objv[1], &stream_nr);
    if (err != TCL_OK)
    {
	stream_nr = ec_stream_nr(Tcl_GetStringFromObj(objv[1], NULL));
	if (stream_nr < 0)
	{
	    Tcl_SetResult(interp, "ec_queue_read: no such ECLiPSe stream", TCL_STATIC);
	    return TCL_ERROR;
	}
	Tcl_ResetResult(interp);
    }
    err = Tcl_GetIntFromObj(interp, objv[2], &len);
    if (err != TCL_OK)
    {
	Tcl_SetResult(interp, "ec_queue_read: integer expected", TCL_STATIC);
	return TCL_ERROR;
    }
    result = Tcl_NewObj();
    len = ec_queue_read(stream_nr, Tcl_SetByteArrayLength(result,len), len);
    if (len < 0)
    {
	interp->result = "ec_queue_read: cannot read from ECLiPSe stream";
	return TCL_ERROR;
    }
    Tcl_SetByteArrayLength(result, len);
    Tcl_SetObjResult(interp, result);
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * Channel driver: mapping ECLiPSe streams to TCL channels
 *---------------------------------------------------------------------------*/

static int EcStreamClose(ClientData, Tcl_Interp *);
static int EcStreamInput(ClientData, char *, int, int *);
static int EcStreamOutput(ClientData, const char *, int, int *);
static void EcStreamWatch(ClientData, int);
static int EcStreamGetHandle(ClientData, int, ClientData *);

/*ARGSUSED*/
static int
EcStreamClose(ClientData nst, Tcl_Interp *interp)
{
    return 0;
}

static int
EcStreamInput(ClientData stream_nr, char *buf, int size, int *err)
{
    int nread;
    nread = ec_queue_read((int)(word)stream_nr, buf, size);
    if (nread < 0)
    {
	*err = EIO;
    	return -1;
    }
    return nread;
}

static int
EcStreamOutput(ClientData stream_nr, const char *buf, int size, int *err)
{
    int nread;
    nread = ec_queue_write((int)(word)stream_nr, (char *) buf, size);
    if (nread < 0)
    {
	*err = EIO;
    	return -1;
    }
    return nread;
}

/*ARGSUSED*/
static void
EcStreamWatch(ClientData stream_nr, int mask)
{
}

/*ARGSUSED*/
static int
EcStreamGetHandle(ClientData stream_nr, int direction, ClientData *handlePtr)
{
    return TCL_ERROR;
}


Tcl_ChannelType ec_stream_channel = {
    	"eclipse_stream",
	NULL,
	EcStreamClose,
	EcStreamInput,
	EcStreamOutput,
	NULL,
	NULL,
	NULL,
	EcStreamWatch,
	EcStreamGetHandle
    };


/*---------------------------------------------------------------------------
 * ec_queue_open eclipse_name access
 *---------------------------------------------------------------------------*/

int
EcQueueOpen(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    int stream_nr, mask;
    char *accessvar;
    char mode;
    Tcl_Channel channel;
    char channelName[16];
    int err;

    if (objc != 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "eclipse_name access_mode");
	return TCL_ERROR;
    }

    /* get the eclipse stream number, objv[1] is number or string */
    err = Tcl_GetIntFromObj(interp, objv[1], &stream_nr);
    if (err != TCL_OK)
    {
	stream_nr = ec_stream_nr(Tcl_GetStringFromObj(objv[1], NULL));
	if (stream_nr < 0)
	{
	    Tcl_SetResult(interp, "no such ECLiPSe stream", TCL_STATIC);
	    return TCL_ERROR;
	}
    }

    accessvar = Tcl_GetStringFromObj(objv[2], NULL);
    /* convert fromec to r, toec to w */
    if (strcmp(accessvar, "fromec\0") == 0 || strcmp(accessvar, "r\0") == 0) 
    {
       mode = 'r';
    } else if (strcmp(accessvar, "toec\0") == 0 || strcmp(accessvar, "w\0") == 0)
    {
       mode = 'w';
    } else {
	Tcl_SetResult(interp, "arg 2: fromec (r) or toec (w) expected", TCL_STATIC);
	return TCL_ERROR;
    }
    mask = (mode == 'r') ? TCL_READABLE : TCL_WRITABLE;

    sprintf(channelName, "ec_queue%d", stream_nr);
    if (Tcl_GetChannel(interp, channelName, NULL) != NULL)
    {
	Tcl_SetResult(interp, "channel name exists already", TCL_STATIC);
	return TCL_ERROR;
    }
    channel = Tcl_CreateChannel(&ec_stream_channel, channelName,
    			(ClientData)(word)stream_nr, mask);
    if (!channel)
    {
	Tcl_SetResult(interp, "couldn't create channel", TCL_STATIC);
	return TCL_ERROR;
    }
    (void) Tcl_SetChannelOption(NULL, (Tcl_Channel) channel,
    	"-translation", "binary");
    (void) Tcl_SetChannelOption(NULL, (Tcl_Channel) channel,
    	"-buffering", "none");
    Tcl_RegisterChannel(interp, channel);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(channelName, -1));
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * ec_stream_nr stream_name
 *---------------------------------------------------------------------------*/

int
EcStreamNr(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    int err, stream_nr;

    if (objc != 2)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "stream_name");
	return TCL_ERROR;
    }
    err = Tcl_GetIntFromObj(interp, objv[1], &stream_nr);
    if (err != TCL_OK)
    {
	stream_nr = ec_stream_nr(Tcl_GetStringFromObj(objv[1], NULL));
    }
    if (stream_nr < 0)
    {
	Tcl_SetResult(interp, "no such ECLiPSe stream", TCL_STATIC);
	return TCL_ERROR;
    }
    Tcl_SetObjResult(interp, Tcl_NewIntObj(stream_nr));
    return TCL_OK;

}


/*---------------------------------------------------------------------------
 * ec_post_goal goal ?format?
 *---------------------------------------------------------------------------*/

int
EcPostGoal(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    char *exdr_string;
    int len;
    int res = EcTcl2Exdr(clientdata, interp, objc, objv);
    if (res != TCL_OK)
    	return res;
    exdr_string = Tcl_GetByteArrayFromObj(Tcl_GetObjResult(interp), &len);
    ec_post_exdr(len, exdr_string);
    Tcl_ResetResult(interp);
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * Create the Tcl commands
 *---------------------------------------------------------------------------*/

int
Tkeclipse_Init(Tcl_Interp *interp)
{
    Tcl_CreateObjCommand(interp, "ec_init_", EcInit,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_cleanup", EcCleanup,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_set_option", EcSetOption,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_post_goal", EcPostGoal,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_post_event", EcPostEvent,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_resume_", EcResume,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_running", EcRunning,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_resume_status", EcResumeStatus,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_handle_events_", EcHandleEvents,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_queue_write", EcQueueWrite,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_queue_read", EcQueueRead,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_stream_nr", EcStreamNr,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateObjCommand(interp, "ec_queue_open_", EcQueueOpen,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}

