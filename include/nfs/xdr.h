/*  $NetBSD: xdr.h,v 1.19 2000/07/17 05:00:45 matt Exp $    */

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 *  from: @(#)xdr.h 1.19 87/04/22 SMI
 *  from: @(#)xdr.h 2.2 88/07/29 4.0 RPCSRC
 * $FreeBSD: src/sys/rpc/xdr.h,v 1.1 2008/03/26 15:23:10 dfr Exp $
 */

/*
 * xdr.h, External Data Representation Serialization Routines.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#ifndef _RPC_XDR_H
#define _RPC_XDR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <lwip/inet.h> /* for ntohl/htonl */
#include <sys/types.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

/* nasty typedefs needed by RPC/XDR code */
typedef int enum_t;
typedef bool bool_t;

#define FALSE   false
#define TRUE    true

/*
 * XDR provides a conventional way for converting between C data
 * types and an external bit-string representation.  Library supplied
 * routines provide for the conversion on built-in C data types.  These
 * routines and utility routines defined here are used to help implement
 * a type encode/decode routine for each user-defined type.
 *
 * Each data type provides a single procedure which takes two arguments:
 *
 *  bool
 *  xdrproc(xdrs, argresp)
 *      XDR *xdrs;
 *      <type> *argresp;
 *
 * xdrs is an instance of a XDR handle, to which or from which the data
 * type is to be converted.  argresp is a pointer to the structure to be
 * converted.  The XDR handle contains an operation field which indicates
 * which of the operations (ENCODE, DECODE * or FREE) is to be performed.
 *
 * XDR_DECODE may allocate space if the pointer argresp is null.  This
 * data can be freed with the XDR_FREE operation.
 *
 * We write only one procedure per data type to make it easy
 * to keep the encode and decode procedures for a data type consistent.
 * In many cases the same code performs all operations on a user defined type,
 * because all the hard work is done in the component type routines.
 * decode as a series of calls on the nested data types.
 */

/*
 * Xdr operations.  XDR_ENCODE causes the type to be encoded into the
 * stream.  XDR_DECODE causes the type to be extracted from the stream.
 * XDR_FREE can be used to release the space allocated by an XDR_DECODE
 * request.
 */
enum xdr_op {
    XDR_ENCODE=0,
    XDR_DECODE=1,
    XDR_FREE=2
};

/*
 * This is the number of bytes per unit of external data.
 */
#define BYTES_PER_XDR_UNIT  (4)
#define RNDUP(x)  ((((x) + BYTES_PER_XDR_UNIT - 1) / BYTES_PER_XDR_UNIT) \
            * BYTES_PER_XDR_UNIT)

/*
 * The XDR handle.
 * Contains operation which is being applied to the stream,
 * an operations vector for the particular implementation (e.g. see xdr_mem.c),
 * and two private fields for the use of the particular implementation.
 */
typedef struct __rpc_xdr {
    enum xdr_op x_op;       /* operation; fast additional param */
    const struct xdr_ops {
        /* get a long from underlying stream */
        bool    (*x_getint32)(struct __rpc_xdr *, int32_t *);
        /* put a long to " */
        bool    (*x_putint32)(struct __rpc_xdr *, const int32_t *);
        /* get some bytes from " */
        bool    (*x_getbytes)(struct __rpc_xdr *, char *, size_t);
        /* put some bytes to " */
        bool    (*x_putbytes)(struct __rpc_xdr *, const char *, size_t);
        /* returns bytes off from beginning */
        size_t  (*x_getpostn)(struct __rpc_xdr *);
        /* lets you reposition the stream */
        bool  (*x_setpostn)(struct __rpc_xdr *, size_t);
        /* buf quick ptr to buffered data */
        int32_t *(*x_inline)(struct __rpc_xdr *, size_t);
        /* free privates of this xdr_stream */
        void    (*x_destroy)(struct __rpc_xdr *);
        //bool  (*x_control)(struct __rpc_xdr *, int, void *);
    } *x_ops;
    //void *    x_public;   /* users' data (unused) */
    void *      x_private;  /* pointer to private data */
    void *      x_base;     /* private used for position info */
    uintptr_t   x_handy;    /* extra private word */
} XDR;

/** \brief a well-known XDR struct that has x_op set to XDR_FREE
 *
 * Passed to XDR code when freeing memory allocated during deserialisation.
 */
extern XDR xdr_free;

/*
 * A xdrproc_t exists for each data type which is to be encoded or decoded.
 *
 * The second argument to the xdrproc_t is a pointer to an opaque pointer.
 * The opaque pointer generally points to a structure of the data type
 * to be decoded.  If this pointer is 0, then the type routines should
 * allocate dynamic storage of the appropriate size and return it.
 */
/*
 * XXX can't actually prototype it, because some take three args!!!
 */
typedef bool (*xdrproc_t)(XDR *, void *, ...);

/*
 * Operations defined on a XDR handle
 *
 * XDR      *xdrs;
 * int32_t  *valp;
 * char *    addr;
 * size_t    len;
 * size_t    pos;
 */
#define XDR_GETINT32(xdrs, valp)            \
    (*(xdrs)->x_ops->x_getint32)(xdrs, valp)

#define XDR_PUTINT32(xdrs, valp)            \
    (*(xdrs)->x_ops->x_putint32)(xdrs, valp)

#define XDR_GETBYTES(xdrs, addr, len)           \
    (*(xdrs)->x_ops->x_getbytes)(xdrs, addr, len)

#define XDR_PUTBYTES(xdrs, addr, len)           \
    (*(xdrs)->x_ops->x_putbytes)(xdrs, addr, len)

#define XDR_GETPOS(xdrs)                \
    (*(xdrs)->x_ops->x_getpostn)(xdrs)

#define XDR_SETPOS(xdrs, pos)               \
    (*(xdrs)->x_ops->x_setpostn)(xdrs, pos)

#define XDR_INLINE(xdrs, len)               \
    (*(xdrs)->x_ops->x_inline)(xdrs, len)

#define XDR_DESTROY(xdrs)               \
    if ((xdrs)->x_ops->x_destroy)           \
        (*(xdrs)->x_ops->x_destroy)(xdrs)

/*
 * Support struct for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * an entry with a null procedure pointer.  The xdr_union routine gets
 * the discriminant value and then searches the array of structures
 * for a matching value.  If a match is found the associated xdr routine
 * is called to handle that part of the union.  If there is
 * no match, then a default routine may be called.
 * If there is no match and no default routine it is an error.
 */
#define NULL_xdrproc_t ((xdrproc_t)0)
struct xdr_discrim {
    int value;
    xdrproc_t proc;
};

/*
 * In-line routines for fast encode/decode of primitive data types.
 * Caveat emptor: these use single memory cycles to get the
 * data from the underlying buffer, and will fail to operate
 * properly if the data is not aligned.  The standard way to use these
 * is to say:
 *  if ((buf = XDR_INLINE(xdrs, count)) == NULL)
 *      return (FALSE);
 *  <<< macro calls >>>
 * where ``count'' is the number of bytes of data occupied
 * by the primitive data types.
 *
 * N.B. and frozen for all time: each data type here uses 4 bytes
 * of external representation.
 */
#define IXDR_GET_INT32(buf)     ((int32_t)ntohl((uint32_t)*(buf)++))
#define IXDR_PUT_INT32(buf, v)      (*(buf)++ =(int32_t)htonl((uint32_t)v))

#define IXDR_GET_UINT32(buf)        ((uint32_t)IXDR_GET_INT32(buf))
#define IXDR_PUT_UINT32(buf, v)     IXDR_PUT_INT32((buf), ((int32_t)(v)))

#define IXDR_GET_BOOL(buf)      ((bool)IXDR_GET_UINT32(buf))
#define IXDR_PUT_BOOL(buf, v)       IXDR_PUT_UINT32((buf), (v))

/*
 * These are the "generic" xdr routines.
 */
extern bool xdr_void(void);
extern bool xdr_int(XDR *xdrs, int *ip);
extern bool xdr_u_int(XDR *xdrs, u_int *ip);
//extern bool xdr_int16_t(XDR *, int16_t *);
//extern bool xdr_uint16_t(XDR *, uint16_t *);
extern bool xdr_int32_t(XDR *, int32_t *);
extern bool xdr_uint32_t(XDR *, uint32_t *);
extern bool xdr_int64_t(XDR *, int64_t *);
extern bool xdr_uint64_t(XDR *, uint64_t *);
extern bool xdr_bool(XDR *, bool *);
extern bool xdr_enum(XDR *, enum_t *);
extern bool xdr_array(XDR *, char **, u_int *, u_int, u_int, xdrproc_t);
extern bool xdr_bytes(XDR *, char **, u_int *, u_int);
extern bool xdr_opaque(XDR *, char *, u_int);
extern bool xdr_string(XDR *, char **, u_int);
//extern bool xdr_union(XDR *, enum_t *, char *, const struct xdr_discrim *, xdrproc_t);
//extern bool xdr_char(XDR *, char *);
//extern bool xdr_u_char(XDR *, u_char *);
extern bool xdr_vector(XDR *, char *, u_int, u_int, xdrproc_t);
//extern bool xdr_float(XDR *, float *);
//extern bool xdr_double(XDR *, double *);
//extern bool xdr_quadruple(XDR *, long double *);
//extern bool xdr_reference(XDR *, char **, u_int, xdrproc_t);
extern bool xdr_pointer(XDR *, char **, u_int, xdrproc_t);
extern bool xdr_wrapstring(XDR *, char **);
//extern void xdr_free(xdrproc_t, void *);
//extern unsigned long xdr_sizeof(xdrproc_t func, void *data);

__END_DECLS

#endif /* !_RPC_XDR_H */
