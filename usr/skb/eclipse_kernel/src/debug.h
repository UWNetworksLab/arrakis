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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: debug.h,v 1.1 2008/06/30 17:43:52 jschimpf Exp $
 *
 */

/*
 * IDENTIFICATION		debug.h
 *
 * DESCRIPTION			Contains the definitions necessary for the	
 *				debugger.
 *
 *
 * CONTENTS:
 *
 *
 */

/*
 * The ports numbers. Those which are displayed in the trace must
 * be at the beginning.
 */

#define PORT_MASK		0x003f /* must match port_mask/1 in asm.pl */
#define NO_ARGS			0x0100 /* arguments unavailable */
#define INLINE_PORT		0x0200
#define LAST_CALL		0x0400
#define FIRST_CALL		0x0800
#define BREAKPOINT		0x1000 /* bit-flag in Port to indicate a breakpoint   */

/*
 * CAUTION: This port numbering is replicated in tracer.pl
 * There is also a name table in printam.c
 */

#define NO_PORT			0

#define CALL_PORT		1
#define DEXIT_PORT		2
#define EXIT_PORT		3
#define PREDO_PORT		4
#define FAIL_PORT		5
#define WAKE_PORT		6
#define LEAVE_PORT		7
#define DELAY_PORT		8
#define NEXT_PORT		9
#define UNIFY_PORT		10
#define SPYTERM			11
#define MODIFY_PORT		12
#define ELSE_PORT		13
/* #define xxx_PORT		14 */
/* #define xxx_PORT		15 */
#define OTHER_PORT		16


/*
 * These masks specify which ports can be created for each
 * of the notifications. Used for prefiltering on ports.
 */
#define ANY_NOTIFIES	0xffff

#define EXIT_NOTIFIES	(1<<(DEXIT_PORT-1)|1<<(EXIT_PORT-1))
#define CALL_NOTIFIES	(1<<(CALL_PORT-1)|EXIT_PORT_BITS)
#define WAKE_NOTIFIES	(1<<(WAKE_PORT-1)|EXIT_PORT_BITS)
#define OTHER_NOTIFIES	(1<<(OTHER_PORT-1))


#define PortFilterBit(port)	(1<<((port)-1))
#define PortWanted(port)	(PortFilterBit(port) & PORTFILTER)
#define ExitPortWanted		(EXIT_NOTIFIES & PORTFILTER)
#define AnyPortWanted		PORTFILTER


#define UNTRACEABLE_INVOC        (-1)
#define ValidInvoc(i)		((i) > 0 || (i) == UNTRACEABLE_INVOC)

