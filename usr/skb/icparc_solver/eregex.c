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
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): J. Chamois
 * 
 * END LICENSE BLOCK */
/*----------------------------------------------------------------------
 * System:	ECLiPSe Constraint Logic Programming System
 * Author:	J Chamois
 * Licence:	This code is in the public domain
 * Version:	$Id: eregex.c,v 1.1.1.1 2006/09/23 01:53:30 snovello Exp $
 *----------------------------------------------------------------------*/

#include <sys/types.h>
#include <stdlib.h>	/* for malloc() */
#include <string.h>
#include <pcreposix.h>
#include "eclipse.h"

#include <stdio.h>


Extern stream_id Winapi	ec_stream_id(int);
Extern int ec_outf(stream_id, const char*, int);
Extern int ec_newline(stream_id);

#define MAXMSGSIZE 512
#define EC_EXTERNAL_ERROR -213


static void
_regfree(t_ext_ptr preg)
{
    regfree((regex_t*) preg);
    free(preg);
}


static t_ext_type ec_xt_regex = {_regfree,0,0,0,0,0,0,0,0};


static int
_reg_error(int err, regex_t *preg)
{
    char buf[MAXMSGSIZE];
    (void) regerror(err, preg, buf, MAXMSGSIZE);
    (void) ec_outf(ec_stream_id(2), buf, strlen(buf));
    (void) ec_newline(ec_stream_id(2));
    return EC_EXTERNAL_ERROR;
}


static int
_get_flags(pword list, int *pcflags, int *peflags)
{
    int err;
    pword car, cdr;
    *pcflags = REG_EXTENDED;
    *peflags = 0;
    for ( ; (err = ec_get_list(list,&car,&cdr)) == PSUCCEED; list = cdr)
    {
	char *string;
        err = ec_get_string(car, &string);
        if (err != PSUCCEED) return err;
        if (!strcmp(string, "extended"))	*pcflags |= REG_EXTENDED;
        else if (!strcmp(string, "basic"))	*pcflags &= ~REG_EXTENDED;
        else if (!strcmp(string, "icase"))	*pcflags |= REG_ICASE;
        else if (!strcmp(string, "newline"))	*pcflags |= REG_NEWLINE;
        else if (!strcmp(string, "nosub"))	*pcflags |= REG_NOSUB;
        else if (!strcmp(string, "notbol"))	*peflags |= REG_NOTBOL;
        else if (!strcmp(string, "noteol"))	*peflags |= REG_NOTEOL;
	else return RANGE_ERROR;
    }
    return err == PFAIL ? PSUCCEED : err;
}


static int
_get_compiled_pattern(pword arg, int cflags, regex_t *pcompiled_reg, regex_t **ppreg)
{
    int err = ec_get_handle(arg, &ec_xt_regex, (t_ext_ptr*) ppreg);
    if (err != PSUCCEED)
    {
	char *pattern;
	err = ec_get_string(arg, &pattern);
	if (err != PSUCCEED) return err;

	err = regcomp(pcompiled_reg, pattern, cflags);
	if (err) return _reg_error(err, pcompiled_reg);
	*ppreg = pcompiled_reg;
    }
    return PSUCCEED;
}


int
ec_regcomp()		/* (+Pattern,+Flags,-CompiledPattern) */
{
    int err, cflags, eflags;
    char *pattern;
    regex_t *preg;

    err = ec_get_string(ec_arg(1), &pattern);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(2), &cflags, &eflags);
    if (err != PSUCCEED) return err;

    preg = (regex_t *) malloc(sizeof(regex_t));
    err = regcomp(preg, pattern, cflags);
    if (err) return _reg_error(err, preg);

    return ec_unify(ec_arg(3), ec_handle(&ec_xt_regex, preg));
}


int
ec_regmatch()		/* (+Pattern,+String,+Flags) */
{
    int err, cflags, eflags;
    regex_t compiled_reg, *preg;
    char *string;

    err = ec_get_string(ec_arg(2), &string);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(3), &cflags, &eflags);
    if (err != PSUCCEED) return err;
    err = _get_compiled_pattern(ec_arg(1), cflags|REG_NOSUB, &compiled_reg, &preg);
    if (err != PSUCCEED) return err;

    err = regexec(preg, string, 0, 0, eflags);
    if (preg == &compiled_reg)
	regfree(preg);

    return err == 0 ? PSUCCEED
    	: err == REG_NOMATCH ? PFAIL
	: _reg_error(err, preg);
}


int
ec_regmatch4()		/* (+Pattern,+String,+Flags,-Match) */
{
    int err, cflags, eflags;
    regex_t compiled_reg, *preg;
    regmatch_t match;
    char *string;

    err = ec_get_string(ec_arg(2), &string);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(3), &cflags, &eflags);
    if (err != PSUCCEED) return err;
    if (cflags & REG_NOSUB) return RANGE_ERROR;
    err = _get_compiled_pattern(ec_arg(1), cflags, &compiled_reg, &preg);
    if (err != PSUCCEED) return err;

    err = regexec(preg, string, 1, &match, eflags);
    if (preg == &compiled_reg)
	regfree(preg);
    if (err)
	return err == REG_NOMATCH ? PFAIL : _reg_error(err, preg);
    return ec_unify(ec_arg(4),
	ec_length_string(match.rm_eo - match.rm_so, string + match.rm_so));
}


int
ec_regmatchsub()	/* (+Pattern,+String,+Flags,-ListOfSubMatches) */
{
    int err, cflags, eflags;
    regex_t compiled_reg, *preg;
    size_t nmatch;
    char *string;
    pword list;

    err = ec_get_string(ec_arg(2), &string);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(3), &cflags, &eflags);
    if (err != PSUCCEED) return err;
    if (cflags & REG_NOSUB) return RANGE_ERROR;
    err = _get_compiled_pattern(ec_arg(1), cflags, &compiled_reg, &preg);
    if (err != PSUCCEED) return err;

    nmatch = preg->re_nsub + 1;
    {
	int res;
#ifdef __GNUC__
	regmatch_t pmatch[nmatch];	/* not standard C! */
#else
	regmatch_t *pmatch = (regmatch_t *) malloc(nmatch*sizeof(regmatch_t));
#endif

	err = regexec(preg, string, nmatch, pmatch, eflags);
	if (preg == &compiled_reg)
	    regfree(preg);
	if (err)
	{
	    res = (err == REG_NOMATCH ? PFAIL : _reg_error(err, preg));
	}
	else
	{
	    list = ec_nil();    /* build the list backwards */
	    while(--nmatch)
	    {
		list = ec_list(
		    ec_length_string(pmatch[nmatch].rm_eo - pmatch[nmatch].rm_so,
			string + pmatch[nmatch].rm_so),
		    list);
	    }
	    res = ec_unify(ec_arg(4), list);
	}
#ifndef __GNUC__
	free(pmatch);
#endif
	return res;
    }
}


int
ec_regmatchall()	/* (+Pattern,+String,+Flags,-ListOfFullMatches) */
{
    int err, cflags, eflags;
    regex_t compiled_reg, *preg;
    regmatch_t match;
    char *string;
    pword list, tail, newtail;
    long lstring;

    err = ec_get_string_length(ec_arg(2), &string, &lstring);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(3), &cflags, &eflags);
    if (err != PSUCCEED) return err;
    if (cflags & REG_NOSUB) return RANGE_ERROR;
    err = _get_compiled_pattern(ec_arg(1), cflags, &compiled_reg, &preg);
    if (err != PSUCCEED) return err;

    list = tail = ec_newvar();	/* build list forward */
    for (;;)
    {
	err = regexec(preg, string, 1, &match, eflags);
	if (err == REG_NOMATCH)
	    break;
	if (err)
	{
	    if (preg == &compiled_reg)
		regfree(preg);
	    return _reg_error(err, preg);
	}
	if (match.rm_eo == match.rm_so)
	{
	    char msg[] = "infinitely many empty strings match";
	    (void) ec_outf(ec_stream_id(2), msg, strlen(msg));
	    (void) ec_newline(ec_stream_id(2));
	    return EC_EXTERNAL_ERROR;
	}

	newtail = ec_newvar();	/* append list element */
	(void) ec_unify(tail, ec_list(
		ec_length_string(match.rm_eo - match.rm_so,
		    string + match.rm_so),
		newtail));
	tail = newtail;

	if (match.rm_eo > lstring)
	    break;
	lstring -= match.rm_eo;
	string += match.rm_eo;
    }
    (void) ec_unify(tail, ec_nil());
    if (preg == &compiled_reg)
	regfree(preg);
    return ec_unify(ec_arg(4), list);
}


int
ec_regsplit()	/* (+Pattern,+String,+Flags,-SplitString) */
{
    int err, cflags, eflags;
    regex_t compiled_reg, *preg;
    regmatch_t match;
    char *string;
    pword list, tail, newtail;
    long lstring;

    err = ec_get_string_length(ec_arg(2), &string, &lstring);
    if (err != PSUCCEED) return err;
    err = _get_flags(ec_arg(3), &cflags, &eflags);
    if (err != PSUCCEED) return err;
    if (cflags & REG_NOSUB) return RANGE_ERROR;
    err = _get_compiled_pattern(ec_arg(1), cflags, &compiled_reg, &preg);
    if (err != PSUCCEED) return err;

    list = tail = ec_newvar();	/* build list forward */
    for (;;)
    {
	err = regexec(preg, string, 1, &match, eflags);
	if (err == REG_NOMATCH)
	    break;
	if (err)
	{
	    if (preg == &compiled_reg)
		regfree(preg);
	    return _reg_error(err, preg);
	}
	if (match.rm_eo == match.rm_so)
	{
	    char msg[] = "infinitely many empty strings match";
	    (void) ec_outf(ec_stream_id(2), msg, strlen(msg));
	    (void) ec_newline(ec_stream_id(2));
	    return EC_EXTERNAL_ERROR;
	}

	newtail = ec_newvar();	/* append list element */
	(void) ec_unify(tail, ec_list(
		ec_length_string(match.rm_so,
		    string), ec_list(
		ec_length_string(match.rm_eo - match.rm_so,
		    string + match.rm_so),
		newtail)));
	tail = newtail;

	if (match.rm_eo > lstring)
	    break;
	lstring -= match.rm_eo;
	string += match.rm_eo;
    }
    (void) ec_unify(tail, ec_list(
	    ec_length_string(lstring, string), ec_nil()));
    if (preg == &compiled_reg)
	regfree(preg);
    return ec_unify(ec_arg(4), list);
}

