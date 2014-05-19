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
 * VERSION	$Id: procedure.c,v 1.2 2008/08/03 22:13:51 jschimpf Exp $
 *
 * IDENTIFICATION		procedure.c
 *
 * DESCRIPTION	
 *
 *	Used to contain the ECLiPSe compiler, now only .eco boot loader.
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	1.0		created the file
 * Micha Meier	2.2	20.7.89	rewritten for the new compiler
 */

 /*
  * INCLUDES:
  */
#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include 	"error.h"
#include	"mem.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"io.h"
#include	"read.h"
#include	"module.h"


/*
 * DEFINES:
 */
#define Query(did)		((did == d_.rulech1 || did == d_.goalch))


 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */

static dident
		d_module2,
		d_module3,
		d_module_interface,
		d_begin_module,
		d_create_module3_,
		d_erase_module_,
		d_eclipse_language_;

dident
		d_call_susp_;

pword		woken_susp_;


/*
 * Check whether the next len characters in stream nst match header[].
 * If yes, skip them, otherwise don't advance the stream pointer.
 */

static int
_skip_header_if_present(stream_id nst, char *header, int len)
{
    int i, res;

    if (IsTty(nst))
    {
	/* Don't expect headers on a tty. This fixes bug 473 (having
	 * to type CTRL-D 3 times to get out of the [user] prompt)  */
    	return PFAIL;
    }
    for (i=0; i<len; ++i)
    {
	res = ec_getch(nst);
	if (res < 0  ||  (char) res != header[i])
	{
	    /* header doesn't match: unget everything */
	    while(i-- >= 0)
	    	ec_ungetch(nst);
	    return PFAIL;
	}
    }
    return PSUCCEED;
}


/*
 * Source files may start with a UTF-8 Byte-Order-Mark, which we want to skip
 */

#define UTF8_BOM_LENGTH	3
static char utf8_bom[UTF8_BOM_LENGTH] = {'\357','\273','\277'};


/*
 * Current eco file version. This must correspond to
 * the number in dump_header/1 in the file io.pl.
 */
#define ECO_CURRENT_VERSION	0x17

#define MAGIC_LEN 3
static char eco_magic[MAGIC_LEN] = {'\354','\034','\051'};

static int
_read_eco_header(stream_id nst)
{
    int i, res;
    /*
     * temporarily limit buffering to the header size because
     * we may have to switch to SSCRAMBLE mode for the rest!
     */
    int bufsize = StreamSize(nst);
    StreamSize(nst) = MAGIC_LEN+1;

    /* check for eco header and skip if present */
    res = _skip_header_if_present(nst, eco_magic, MAGIC_LEN);
    StreamSize(nst) = bufsize;
    if (res != PSUCCEED) 
    	return PFAIL;

    /* next byte indicates the eco version */
    res = ec_getch(nst);
    if (res < 0)
    	return res;
    if (res != ECO_CURRENT_VERSION)
	return BAD_DUMP_VERSION;

    StreamMode(nst) |= SSCRAMBLE;
    StreamRand(nst) = 73540 ^ 0x9bc33c86;

    /* read the rest of the header */
    for(i=0; i<8; ++i)
    	res = ec_getch(nst);
    return res < 0 ? res : PSUCCEED;
}


void
compiler_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	CompileId = 0;
    }

    d_call_susp_ = in_dict("call_suspension", 1);
    d_module2 = in_dict("module", 2);
    d_module3 = in_dict("module", 3);
    d_module_interface = in_dict("module_interface", 1);
    d_begin_module = in_dict("begin_module", 1);
    d_erase_module_ = in_dict("erase_module", 1);
    d_create_module3_ = in_dict("create_module", 3);
    d_eclipse_language_ = in_dict("eclipse_language", 0);


    /*
     * A suspension which is marked as dead. Any suspension that occurs 
     * in a compiled clause is compiled into a TSUSP pointer to this one.
     */
    Init_Susp_Dead(&woken_susp_);
}



#ifdef PRINTAM
void
print_procedure(dident wdid, vmcode *code)
{
	extern int	als(long int addr);

	p_fprintf(current_output_, "\n%s/", DidName(wdid));
	p_fprintf(current_output_, "%d:\n", DidArity(wdid));

	(void) als((long) code);
	ec_flush(current_output_);
}
#endif




/***********************************************************************
 * Load an .eco file
 *
 * An .eco file contains only directives
 * Only module directives are treated specially here
 * Pragmas are ignored for backward compatibility (they should not occur)
 * The calling module is passed in *module, and the current module
 * at the end of the eco file is returned in *module.
 ***********************************************************************/

int
ec_load_eco_from_stream(stream_id nst, int options, pword *module)
{
    int res;
    pword *clause, *query, *pw;
    pword query_pw, kernel_pw;
    pword top_module = *module;
    int encoded = 0;

    /* we are expecting an eco-encoded file, but we allow text as well */
    res = _read_eco_header(nst);
    encoded = (res == PSUCCEED);
    StreamMode(nst) |= SNOMACROEXP; /* to avoid problems in text-eco files */
    kernel_pw.val.did = d_.kernel_sepia;
    kernel_pw.tag.kernel = ModuleTag(d_.kernel_sepia);

    for(;;)
    {
	int recreate_module = 0;
	pword exports_pw, language_pw;
	pword *new_module = 0;

#ifdef EXTENSIVE_DEBUGGING
//asq: added the following line as suggested by Joachim Schimpf

    printf("%s:%d\n", DidName(StreamName(nst)), StreamLine(nst));
#endif

	if (encoded)			/* encoded dbformat */
	{
	    int n;
	    long nread;

	    char *s = ec_getstring(nst, 4L, &nread);
	    if (!(s))
		return nread;	/* error code */
	    if (nread < 4L)
		return (nread == 0L) ? PSUCCEED : UNEXPECTED_EOF;

	    n = (unsigned char) *s++ << 24;
	    n |= (unsigned char) *s++ << 16;
	    n |= (unsigned char) *s++ << 8;
	    n |= (unsigned char) *s;
	    s = ec_getstring(nst, n, &nread);
	    if (!(s))
		return nread;	/* error code */
	    if (nread < n)
		return UNEXPECTED_EOF;

	    clause = dbformat_to_term(s, module->val.did, module->tag);
	    if (!clause)
		return NOT_DUMP_FILE;
	}
	else				/* text format, call the parser */
	{
	    res = ec_read_term(nst,
    		(GlobalFlags & VARIABLE_NAMES ? VARNAMES_PLEASE : 0),
		&query_pw, 0, 0, module->val, module->tag);
	    if (res != PSUCCEED)
	    	return (res == PEOF) ? PSUCCEED : NOT_DUMP_FILE;

	    clause = &query_pw;
	}

	Dereference_(clause);
	if (!IsStructure(clause->tag) || !Query(clause->val.ptr->val.did))
	    return NOT_DUMP_FILE;

	pw = query = clause->val.ptr + 1;
	Dereference_(pw);
	if (IsStructure(pw->tag))	/* look for special directives */
	{
	    if (pw->val.ptr->val.did == d_.module1)
	    {
		recreate_module = 1;
		new_module = &pw->val.ptr[1];
		Make_Nil(&exports_pw);
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    if (pw->val.ptr->val.did == d_module_interface)
	    {
		recreate_module = 1;
		new_module = &pw->val.ptr[1];
		Make_Nil(&exports_pw);
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    else if (pw->val.ptr->val.did == d_module2)
	    {
		recreate_module = 1;
		new_module = &pw->val.ptr[1];
		exports_pw = pw->val.ptr[2];
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    else if (pw->val.ptr->val.did == d_module3)
	    {
		recreate_module = 1;
		new_module = &pw->val.ptr[1];
		exports_pw = pw->val.ptr[2];
		language_pw = pw->val.ptr[3];
	    }
	    else if (pw->val.ptr->val.did == d_begin_module)
	    {
		new_module = &pw->val.ptr[1];
		query = &query_pw;	/* don't execute anything */
		Make_Atom(query, d_.true0);
	    }
	    else if (pw->val.ptr->val.did == d_.pragma)
	    {
		query = &query_pw;	/* ignore pragmas, replace with true */
		Make_Atom(query, d_.true0);
	    }
	}
	else if (pw->val.did == d_.system || pw->val.did == d_.system_debug)
	{
	    query = &query_pw;	/* ignore pragmas, replace with true */
	    Make_Atom(query, d_.true0);
	}

	if (recreate_module)		/* build translated module query */
	{
	    pword *pgoal, *pcont;
	    query = &query_pw;
	    Make_Struct(query, TG);
	    /* If module changes, raise CODE_UNIT_LOADED event first */
	    if (module->val.did != top_module.val.did)
	    {
		pcont = TG;
		Push_Struct_Frame(d_.comma);
		Make_Struct(&pcont[1], TG);
		pgoal = TG;
		Push_Struct_Frame(d_.syserror);
		Make_Integer(&pgoal[1], CODE_UNIT_LOADED);
		Make_Atom(&pgoal[2], d_.eof);
		pgoal[3] = *module;
		pgoal[4] = *module;
		Make_Struct(&pcont[2], TG);
	    }
	    pcont = TG;
	    Push_Struct_Frame(d_.comma);
	    Make_Struct(&pcont[1], TG);
	    pgoal = TG;
	    Push_Struct_Frame(d_erase_module_);
	    pgoal[1] = *new_module;
	    Make_Struct(&pcont[2], TG);
	    pgoal = TG;
	    Push_Struct_Frame(d_create_module3_);
	    pgoal[1] = *new_module;
	    pgoal[2] = exports_pw;
	    pgoal[3] = language_pw;

	    res = query_emulc(query->val, query->tag, kernel_pw.val, kernel_pw.tag);
	}
	else
	{
	    /* execute the query/directive */
	    res = query_emulc(query->val, query->tag, module->val, module->tag);
	}

	if (res != PSUCCEED)
	{
	    pw = TG;
	    Push_Struct_Frame(d_.syserror);
	    Make_Integer(&pw[1], QUERY_FAILED);
	    pw[2] = *query;
	    pw[3] = *module;
	    pw[4] = *module;
	    query = &query_pw;
	    Make_Struct(query, pw);
	    (void) query_emulc(query->val, query->tag, kernel_pw.val, kernel_pw.tag);
	}

	if (new_module)			/* change to new context module */
	{
	    Dereference_(new_module);
	    *module = *new_module;
	}
    }
    return PSUCCEED;
}

