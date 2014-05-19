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
 *	$Id: tkexdr.c,v 1.1 2008/06/30 17:43:58 jschimpf Exp $
 *
 *	Code for exdr communications with ECLiPSe in a tcl program
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <string.h>

#include <tcl.h>

#include "config.h"
#include "tkcommon.h"

/* define a pointer-sized integer type */
#if (SIZEOF_CHAR_P == SIZEOF_INT)
typedef unsigned int	uword;
#else
#if (SIZEOF_CHAR_P == SIZEOF_LONG)
typedef unsigned long	uword;
#endif
#endif

#ifdef __STDC__
int EcReadExdr(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcTcl2Exdr(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
int EcExdr2Tcl(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST []);
#endif



/*---------------------------------------------------------------------------
 * Serialisation of ground terms for communication with other languages
 *
 * EXDR Version 2
 * 
 * ExdrTerm      ::=   'V' Version CompactFlag? Term
 * CompactFlag   ::=   'C'
 * Term          ::=   (Integer|Double|String|List|Nil|Struct|Variable)
 * Integer       ::=   ('B' <byte> | 'I' XDR_int | 'J' XDR_long)
 * Double        ::=   'D' XDR_double
 * String        ::=   ('S' Length <byte>* | 'R' Index)
 * List          ::=   '[' Term (List|Nil)
 * Nil           ::=   ']'
 * Struct        ::=   'F' Arity String Term*
 * Variable      ::=   '_'
 * Length        ::=   XDR_nat
 * Index         ::=   XDR_nat
 * Arity         ::=   XDR_nat
 * Version       ::=   <byte>
 * XDR_int       ::=   <4 bytes, msb first>
 * XDR_long      ::=   <8 bytes, msb first>
 * XDR_double    ::=   <8 bytes, ieee double, exponent first>
 * XDR_nat       ::=   <8 bits: 1 + seven bits unsigned value>
 *                   | XDR_int                     // >= 0
 *---------------------------------------------------------------------------*/

#define EXDR_VERSION			2
#define EXDR_HEADER_LEN			2
#define EXDR_COMPRESSED_HEADER_LEN	3

static char exdr_header[EXDR_COMPRESSED_HEADER_LEN] = {'V',EXDR_VERSION,'C'};

/* read n bytes from 'channel' to 'bp' */
#define Tcl_Read_Check(n) \
 	{ if (Tcl_Read(channel, bp, n) < n) goto _error_; }

#define Load_Byte(n)    (n) = *bp++;
#define Load_Word(n) {                        \
        (n) = *bp++;                         \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
}
#define Load_DWord(n) {                        \
        (n) = *bp++;                         \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
        (n) = (n) << 8 | (*bp++) & 0xff;     \
}
#define Load_Nat(GET,n) {			\
	GET(1);					\
        (n) = *bp++;				\
	if ((n) & 0x80) {			\
	    (n) &= 0x7f;			\
	} else {				\
	    GET(3);				\
	    (n) = (n) << 8 | (*bp++) & 0xff;	\
	    (n) = (n) << 8 | (*bp++) & 0xff;	\
	    (n) = (n) << 8 | (*bp++) & 0xff;	\
	}					\
}

#define Store_Byte(byte) *dest++ = (byte)
#define Store_Word(word) {\
	    register unsigned long aux = (word);		\
	    *dest++ = (char) (aux >> 24);			\
	    *dest++ = (char) (aux >> 16);			\
	    *dest++ = (char) (aux >> 8);			\
	    *dest++ = (char) (aux);				\
	}
#define Store_DWord(word) {\
	    register unsigned long aux = (word);		\
	    *dest++ = (char) (aux >> 56);			\
	    *dest++ = (char) (aux >> 48);			\
	    *dest++ = (char) (aux >> 40);			\
	    *dest++ = (char) (aux >> 32);			\
	    *dest++ = (char) (aux >> 24);			\
	    *dest++ = (char) (aux >> 16);			\
	    *dest++ = (char) (aux >> 8);			\
	    *dest++ = (char) (aux);				\
	}
#define Store_Nat(word) {					\
	    register unsigned long aux = (word);		\
	    if (aux < 0x80) {					\
		*dest++ = (char) (aux | 0x80);			\
	    } else {						\
		*dest++ = (char) (aux >> 24);			\
		*dest++ = (char) (aux >> 16);			\
		*dest++ = (char) (aux >> 8);			\
		*dest++ = (char) (aux);				\
	    }							\
	}

typedef union {
	double	as_dbl;
#if (SIZEOF_LONG == 8)
	unsigned long as_int;
#else
	struct ieee_parts {
#ifdef WORDS_BIGENDIAN 
		unsigned mant1;
		unsigned mant0;
#else
		unsigned mant0;
		unsigned mant1;
#endif
	} as_struct;
#endif
} ieee_double;


static Tcl_Obj *
_EcReadExdr(Tcl_Interp *interp, Tcl_Channel channel, int nextch, Tcl_HashTable *string_table, uword *string_index)
{
    char buf[10];
    char *bp;
    ieee_double d;
    Tcl_Obj *obj, *elem;
    int err;
    long len, arity;

    switch(nextch)
    {
    case 'B':
	bp = buf;
	Tcl_Read_Check(1);
	Load_Byte(len);
	return Tcl_NewLongObj(len);

    case 'I':
	bp = buf;
	Tcl_Read_Check(4);
	Load_Word(len);
	return Tcl_NewLongObj(len);

#if SIZEOF_LONG == 8
    case 'J':
	bp = buf;
	Tcl_Read_Check(8);
	Load_DWord(len);
	return Tcl_NewLongObj(len);
#else
	/* not supported */
#endif

    case 'D':
	bp = buf;
	Tcl_Read_Check(8);
#if SIZEOF_LONG == 8
	Load_DWord(d.as_int);
#else
	Load_Word(d.as_struct.mant1);
	Load_Word(d.as_struct.mant0);
#endif
	return Tcl_NewDoubleObj(d.as_dbl);

    case '_':
	return Tcl_NewStringObj("_", 1);

    case 'S':
    {
	int new_entry;
	Tcl_HashEntry *entry;
	bp = buf;
	Load_Nat(Tcl_Read_Check, len);
	obj = Tcl_NewObj();
	bp = Tcl_SetByteArrayLength(obj, len);
	Tcl_Read_Check(len);
	if (string_table)
	{
	    entry = Tcl_CreateHashEntry(string_table, (char *) (*string_index), &new_entry);
	    ++(*string_index);
	    Tcl_SetHashValue(entry, (ClientData) obj);
	}
	return obj;
    }

    case 'R':
    {
	uword this_index;
	Tcl_HashEntry *entry;
	if (!string_table) return NULL;
	bp = buf;
	Load_Nat(Tcl_Read_Check, this_index);
	entry = Tcl_FindHashEntry(string_table, (char *) this_index);
	if (!entry) return NULL;
	return (Tcl_Obj *) Tcl_GetHashValue(entry);
    }

    case 'F':
	bp = buf;
	Load_Nat(Tcl_Read_Check, arity);
	obj = Tcl_NewObj();
	for (; arity >= 0; --arity)
	{
	    bp = buf;
	    Tcl_Read_Check(1);
	    elem = _EcReadExdr(interp, channel, *bp, string_table, string_index);
	    if (!elem) return NULL;
	    err = Tcl_ListObjAppendElement(interp, obj, elem);
	    if (err != TCL_OK) return NULL;
	}
	return obj;

    case '[':
	obj = Tcl_NewObj();
	for (;;)
	{
	    bp = buf;
	    Tcl_Read_Check(1);
	    elem = _EcReadExdr(interp, channel, *bp, string_table, string_index);
	    if (!elem) return NULL;
	    err = Tcl_ListObjAppendElement(interp, obj, elem);
	    if (err != TCL_OK) return NULL;
	    bp = buf;
	    Tcl_Read_Check(1);
	    if (*buf == ']')
		return obj;
	    if (*buf != '[')
		return NULL;
	}

    case ']':			/* a lone nil, not terminating a list */
	return Tcl_NewObj();

    default:
	return NULL;
    }
_error_:
    return NULL;
}


/* ec_read_exdr channel */

int
EcReadExdr(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    Tcl_Channel channel;
    Tcl_Obj *resultObj;
    uword string_index = 0;
    Tcl_HashTable string_table;
    char buf[10], *bp;
    int nextch;

    if (objc != 2)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "channel");
	return TCL_ERROR;
    }
    channel = Tcl_GetChannel(interp, Tcl_GetStringFromObj(objv[1], NULL), NULL);
    if (!channel)
    {
	Tcl_SetResult(interp, "no such channel", TCL_STATIC);
	return TCL_ERROR;
    }

    bp = buf;
    Tcl_Read_Check(EXDR_COMPRESSED_HEADER_LEN);
    if (buf[0] != 'V')
    {
	Tcl_SetResult(interp, "no exdr-term to read", TCL_STATIC);
	return TCL_ERROR;
    }
    if ((unsigned) buf[1] > (unsigned) EXDR_VERSION)
    {
	Tcl_SetResult(interp, "incompatible exdr version", TCL_STATIC);
	return TCL_ERROR;
    }
    nextch = buf[2];
    if (nextch == 'C')	/* compact-flag */
    {
	bp = buf;
	Tcl_Read_Check(1);
	Tcl_InitHashTable(&string_table, TCL_ONE_WORD_KEYS);
	resultObj = _EcReadExdr(interp, channel, *bp, &string_table, &string_index);
	Tcl_DeleteHashTable(&string_table);
    }
    else
    {
	resultObj = _EcReadExdr(interp, channel, nextch, NULL, NULL);
    }
    if (resultObj)
    {
	Tcl_SetObjResult(interp, resultObj);
	return TCL_OK;
    }
_error_:
    Tcl_SetResult(interp, "conversion error while reading exdr format", TCL_STATIC);
    return TCL_ERROR;
}


#define Buf_Check(n) { if (bp+(n) > stop) return NULL; }

static char *
_EcExdr2Tcl(Tcl_Interp *interp, char *bp, char *stop, Tcl_HashTable *string_table, uword *string_index, Tcl_Obj **result)
{
    ieee_double d;
    Tcl_Obj *elem;
    int err;
    long len, arity;

    Buf_Check(1);
    switch(*bp++)
    {
    case 'B':
	Buf_Check(1);
	Load_Byte(len);
	*result = Tcl_NewLongObj(len);
	return bp;

    case 'I':
	Buf_Check(4);
	Load_Word(len);
	*result = Tcl_NewLongObj(len);
	return bp;

#if SIZEOF_LONG == 8
    case 'J':
	Buf_Check(8);
	Load_DWord(len);
	*result = Tcl_NewLongObj(len);
	return bp;
#else
	/* not supported */
#endif

    case 'D':
	Buf_Check(8);
#if SIZEOF_LONG == 8
	Load_DWord(d.as_int);
#else
	Load_Word(d.as_struct.mant1);
	Load_Word(d.as_struct.mant0);
#endif
	*result = Tcl_NewDoubleObj(d.as_dbl);
	return bp;

    case '_':
	*result = Tcl_NewStringObj("_", 1);
	return bp;

    case 'S':
    {
	int new_entry;
	Tcl_HashEntry *entry;
	Load_Nat(Buf_Check, len);
	Buf_Check(len);
	*result = Tcl_NewByteArrayObj(bp, len);
	if (string_table)
	{
	    entry = Tcl_CreateHashEntry(string_table, (char *) (*string_index), &new_entry);
	    ++(*string_index);
	    Tcl_SetHashValue(entry, (ClientData) *result);
	}
	return bp+len;
    }

    case 'R':
    {
	uword this_index;
	Tcl_HashEntry *entry;
	if (!string_table) return NULL;
	Load_Nat(Buf_Check, this_index);
	entry = Tcl_FindHashEntry(string_table, (char *) this_index);
	if (!entry) return NULL;
	*result = (Tcl_Obj *) Tcl_GetHashValue(entry);
	return bp;
    }

    case 'F':
	Load_Nat(Buf_Check, arity);
	*result = Tcl_NewObj();
	for (; arity >= 0; --arity)
	{
	    bp = _EcExdr2Tcl(interp, bp, stop, string_table, string_index, &elem);
	    if (!bp) return NULL;
	    err = Tcl_ListObjAppendElement(interp, *result, elem);
	    if (err != TCL_OK) return NULL;
	}
	return bp;

    case '[':
	*result = Tcl_NewObj();
	for (;;)
	{
	    bp = _EcExdr2Tcl(interp, bp, stop, string_table, string_index, &elem);
	    if (!bp) return NULL;
	    err = Tcl_ListObjAppendElement(interp, *result, elem);
	    if (err != TCL_OK) return NULL;
	    Buf_Check(1);
	    switch (*bp++) {
		case ']':	return bp;
		case '[':	break;
		default:	return NULL;
	    }
	}

    case ']':			/* a lone nil, not terminating a list */
	*result = Tcl_NewObj();
	return bp;

    default:
	return NULL;
    }
}


/* ec_exdr2tcl exdr_string */

int
EcExdr2Tcl(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    Tcl_Obj *resultObj;
    char *bp, *stop;
    int len;
    uword string_index = 0;
    Tcl_HashTable string_table;

    if (objc != 2)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "exdr_string");
	return TCL_ERROR;
    }
    bp = Tcl_GetByteArrayFromObj(objv[1], &len);
    stop = bp+len;
    if (len < EXDR_COMPRESSED_HEADER_LEN)
    {
	Tcl_SetResult(interp, "ec_exdr2tcl: not exdr format (short)", TCL_STATIC);
	return TCL_ERROR;
    }
    if (*bp++ != exdr_header[0])
    {
	Tcl_SetResult(interp, "ec_exdr2tcl: not exdr format", TCL_STATIC);
	return TCL_ERROR;
    }
    if ((unsigned) *bp++ > (unsigned) exdr_header[1])
    {
	Tcl_SetResult(interp, "ec_exdr2tcl: incompatible exdr version", TCL_STATIC);
	return TCL_ERROR;
    }
    if (*bp == exdr_header[2])	/* optional compact-flag */
    {
	++bp;
	Tcl_InitHashTable(&string_table, TCL_ONE_WORD_KEYS);
	bp = _EcExdr2Tcl(interp, bp, stop, &string_table, &string_index, &resultObj);
	Tcl_DeleteHashTable(&string_table);
    }
    else
    {
	bp = _EcExdr2Tcl(interp, bp, stop, NULL, NULL, &resultObj);
    }
    if (!bp || bp != stop)
    {
	Tcl_SetResult(interp, "ec_exdr2tcl: conversion error", TCL_STATIC);
	return TCL_ERROR;
    }
    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}


/*
 * ec_tcl2exdr data ?format?
 *
 * Convert Tcl-data to EXDR term (according to format)
 */

void
Tcl_AppendToByteArray(Tcl_Obj *objPtr, char *bytes, int length, int *pos)
{
    int new_len, alloc;
    char *bp;
    new_len = *pos+length;
    bp = Tcl_GetByteArrayFromObj(objPtr, &alloc);
    if (new_len > alloc)
    {
	while (new_len > alloc)
	    alloc *= 2;
	bp = Tcl_SetByteArrayLength(objPtr, alloc);
    }
    memcpy(bp+*pos, bytes, (size_t) length);
    *pos = new_len;
}

static int
_EcTcl2Exdr(Tcl_Interp *interp,
	char **typespec,
	Tcl_Obj *obj,		/* the object to convert */
	Tcl_Obj *exdr_obj,	/* the object to append exdr data to */
	Tcl_HashTable *string_table,
	Tcl_HashTable *utf8_table,
	uword *string_index,
	int *pos)		/* next position in the resulting byte array */
{
    int i, len, res, objc;
    long n;
    ieee_double d;
    char *dest, *s, *subtype;
    char buf[10];
    Tcl_Obj **objv;

    switch (**typespec) {
    case '_':
	s = Tcl_GetStringFromObj(obj, &len);
	if (s[0] != '_' || s[1] != 0) {
	    Tcl_SetResult(interp, "ec_tcl2exdr: _ expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	Tcl_AppendToByteArray(exdr_obj, "_", 1, pos);
	++(*typespec);
	break;

    case 'S':		/* send a byte (8-bit) string */
    {
	int new_entry;
	Tcl_HashEntry *entry;
	char *hash_string;
	dest = buf;
	/* Unfortunately, Tcl hash tables cannot hash raw byte arrays, only
	 * null-terminated strings that don't contain nulls. We therefore
	 * get the string representation of the byte array and use that for
	 * hashing here. However, that has the consequence that the raw 'S'
	 * string and the corresponding 'U' string hash to the same value,
	 * even though their exdr-representation is different. That's why
	 * we need two separate hash tables string_table and utf8_table... */
	hash_string = Tcl_GetString(obj);
	entry = Tcl_CreateHashEntry(string_table, hash_string, &new_entry);
	if (new_entry)
	{
	    Tcl_SetHashValue(entry, (ClientData) (*string_index));
	    ++(*string_index);
	    s = Tcl_GetByteArrayFromObj(obj, &len);
	    Store_Byte('S');
	    Store_Nat(len);
	    Tcl_AppendToByteArray(exdr_obj, buf, dest-buf, pos);
	    Tcl_AppendToByteArray(exdr_obj, s, len, pos);
	}
	else
	{
	    Store_Byte('R');
	    Store_Nat((uword) Tcl_GetHashValue(entry));
	    Tcl_AppendToByteArray(exdr_obj, buf, dest-buf, pos);
	}
	++(*typespec);
	break;
    }

    case 'U':		/* send a UTF-8 encoded string */
    {
	int new_entry;
	Tcl_HashEntry *entry;
	dest = buf;
	s = Tcl_GetStringFromObj(obj, &len);
	entry = Tcl_CreateHashEntry(utf8_table, s, &new_entry);
	if (new_entry)
	{
	    Tcl_SetHashValue(entry, (ClientData) (*string_index));
	    ++(*string_index);
	    Store_Byte('S');
	    Store_Nat(len);
	    Tcl_AppendToByteArray(exdr_obj, buf, dest-buf, pos);
	    Tcl_AppendToByteArray(exdr_obj, s, len, pos);
	}
	else
	{
	    Store_Byte('R');
	    Store_Nat((uword) Tcl_GetHashValue(entry));
	    Tcl_AppendToByteArray(exdr_obj, buf, dest-buf, pos);
	}
	++(*typespec);
	break;
    }

    case 'I':
	res = Tcl_GetLongFromObj(interp, obj, &n);
	if (res != TCL_OK) {
	    Tcl_SetResult(interp, "ec_tcl2exdr: integer expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	dest = buf;
	if ((long)(char) n == n)
	{
	    Store_Byte('B');
	    Store_Byte((char) n);
	    Tcl_AppendToByteArray(exdr_obj, buf, 2, pos);
	}
#if SIZEOF_LONG == 8
	else if (n < -2147483648L || n > 2147483647L)
	{
	    Store_Byte('J');
	    Store_DWord(n);
	    Tcl_AppendToByteArray(exdr_obj, buf, 9, pos);
	}
#endif
	else
	{
	    Store_Byte('I');
	    Store_Word(n);
	    Tcl_AppendToByteArray(exdr_obj, buf, 5, pos);
	}
	++(*typespec);
	break;

    case 'D':
	res = Tcl_GetDoubleFromObj(interp, obj, &d.as_dbl);
	if (res != TCL_OK) {
	    Tcl_SetResult(interp, "ec_tcl2exdr: double expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	dest = buf;
	Store_Byte('D');
#if SIZEOF_LONG == 8
	Store_DWord(d.as_int);
#else
	Store_Word(d.as_struct.mant1);
	Store_Word(d.as_struct.mant0);
#endif
	Tcl_AppendToByteArray(exdr_obj, buf, 9, pos);
	++(*typespec);
	break;

    case '[':
	++(*typespec);
	res = Tcl_ListObjGetElements(interp,obj,&objc,&objv);
	if (res != TCL_OK) {
	    Tcl_SetResult(interp, "ec_tcl2exdr: list expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	for (i=0; i<objc; ++i)
	{
	    subtype = *typespec;
	    Tcl_AppendToByteArray(exdr_obj, "[", 1, pos);
	    res = _EcTcl2Exdr(interp, typespec, objv[i], exdr_obj, string_table, utf8_table, string_index, pos);
	    if (res != TCL_OK) return res;
	    if (**typespec == '*')
	    	*typespec = (i+1 < objc) ? subtype : *typespec + 1;
	}
	if (**typespec != ']')
	{
	    Tcl_SetResult(interp, "ec_tcl2exdr: list too short", TCL_STATIC);
	    return TCL_ERROR;
	}
	++(*typespec);
	Tcl_AppendToByteArray(exdr_obj, "]", 1, pos);
	break;

    case '(':
	++(*typespec);
	res = Tcl_ListObjGetElements(interp,obj,&objc,&objv);
	if (res != TCL_OK) {
	    Tcl_SetResult(interp, "ec_tcl2exdr: list expected", TCL_STATIC);
	    return TCL_ERROR;
	}
	if (objc < 1)			/* need functor at least */
	{
	    Tcl_SetResult(interp, "ec_tcl2exdr: list too short", TCL_STATIC);
	    return TCL_ERROR;
	}
	dest = buf;
	Store_Byte('F');
	Store_Nat(objc-1);
	Tcl_AppendToByteArray(exdr_obj, buf, dest-buf, pos);
	subtype = "S";
	res = _EcTcl2Exdr(interp, &subtype, objv[0], exdr_obj, string_table, utf8_table, string_index, pos);
	if (res != TCL_OK)  return res;
	for (i=1; i<objc; ++i)
	{
	    subtype = *typespec;
	    res = _EcTcl2Exdr(interp, typespec, objv[i], exdr_obj, string_table, utf8_table, string_index, pos);
	    if (res != TCL_OK)  return res;
	    if (**typespec == '*')
	    	*typespec = (i+1 < objc) ? subtype : *typespec + 1;
	}
	if (**typespec != ')')
	{
	    Tcl_SetResult(interp, "ec_tcl2exdr: list too short", TCL_STATIC);
	    return TCL_ERROR;
	}
	++(*typespec);
	break;

    default:
	Tcl_SetResult(interp, "ec_tcl2exdr: malformed format string", TCL_STATIC);
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
EcTcl2Exdr(ClientData clientdata, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
    Tcl_Obj *obj;
    Tcl_Obj *exdr_obj;
    Tcl_HashTable string_table, utf8_table;
    uword string_index = 0;
    char *typespec;
    int pos = 0;
    int res;

    if (objc < 2 || objc > 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "data ?format?");
	return TCL_ERROR;
    }
    typespec = objc == 3 ? Tcl_GetStringFromObj(objv[2], NULL) : "S";
    obj = objv[1];

    exdr_obj = Tcl_NewObj();
    Tcl_SetByteArrayLength(exdr_obj, 1000);
    Tcl_AppendToByteArray(exdr_obj, exdr_header, EXDR_COMPRESSED_HEADER_LEN, &pos);
    Tcl_InitHashTable(&string_table, TCL_STRING_KEYS);
    Tcl_InitHashTable(&utf8_table, TCL_STRING_KEYS);
    res = _EcTcl2Exdr(interp, &typespec, obj, exdr_obj, &string_table, &utf8_table, &string_index, &pos);
    Tcl_DeleteHashTable(&string_table);
    Tcl_DeleteHashTable(&utf8_table);
    if (res != TCL_OK)
    	return TCL_ERROR;
    Tcl_SetByteArrayLength(exdr_obj, pos);
    Tcl_SetObjResult(interp, exdr_obj);
    return TCL_OK;
}


/*---------------------------------------------------------------------------
 * Create the Tcl commands
 *---------------------------------------------------------------------------*/

int
Tkexdr_Init(Tcl_Interp *interp)
{
    Tcl_CreateObjCommand(interp, "ec_read_exdr", EcReadExdr,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_tcl2exdr", EcTcl2Exdr,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "ec_exdr2tcl", EcExdr2Tcl,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}



