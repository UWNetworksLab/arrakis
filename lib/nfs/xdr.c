/*  $NetBSD: xdr.c,v 1.22 2000/07/06 03:10:35 christos Exp $    */

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
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
 */

/*
 * xdr.c, Generic XDR routines implementation.
 *
 * Copyright (C) 1986, Sun Microsystems, Inc.
 *
 * These are the "generic" xdr routines used to serialize and de-serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <nfs/xdr.h>

#define mem_alloc(size)     malloc(size)
#define mem_free(p, size)   free(p)

/*
 * constants specific to the xdr "protocol"
 */
#define XDR_FALSE       ((int32_t) 0)
#define XDR_TRUE        ((int32_t) 1)
#define LASTUNSIGNED    ((u_int) 0-1)

/* XDR for freeing allocated memory. see corresponding comment in xdr.h */
XDR xdr_free = { .x_op = XDR_FREE };

/*
 * for unit alignment
 */
static const char xdr_zero[BYTES_PER_XDR_UNIT] = { 0, 0, 0, 0 };


/*
 * XDR nothing
 */
bool
xdr_void(void)
{

    return true;
}


/*
 * XDR integers
 */
bool
xdr_int(XDR *xdrs, int *ip)
{
    int32_t l;

    switch (xdrs->x_op) {

    case XDR_ENCODE:
        l = (int32_t) *ip;
        return (XDR_PUTINT32(xdrs, &l));

    case XDR_DECODE:
        if (!XDR_GETINT32(xdrs, &l)) {
            return false;
        }
        *ip = (int) l;
        return true;

    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}

/*
 * XDR unsigned integers
 */
bool
xdr_u_int(XDR *xdrs, u_int *up)
{
    uint32_t l;

    switch (xdrs->x_op) {

    case XDR_ENCODE:
        l = (uint32_t) *up;
        return (XDR_PUTINT32(xdrs, (int32_t *)&l));

    case XDR_DECODE:
        if (!XDR_GETINT32(xdrs, (int32_t *)&l)) {
            return false;
        }
        *up = (u_int) l;
        return true;

    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}

/*
 * XDR 32-bit integers
 * same as xdr_uint32_t - open coded to save a proc call!
 */
bool
xdr_int32_t(XDR *xdrs, int32_t *int32_p)
{
        switch (xdrs->x_op) {

        case XDR_ENCODE:
                return XDR_PUTINT32(xdrs, int32_p);

        case XDR_DECODE:
                return XDR_GETINT32(xdrs, int32_p);

        case XDR_FREE:
                return true;
        }
        /* NOTREACHED */
        return false;
}

/*
 * XDR unsigned 32-bit integers
 * same as xdr_int32_t - open coded to save a proc call!
 */
bool
xdr_uint32_t(XDR *xdrs, uint32_t *uint32_p)
{
    switch (xdrs->x_op) {

    case XDR_ENCODE:
        return XDR_PUTINT32(xdrs, (int32_t *)uint32_p);

    case XDR_DECODE:
                return XDR_GETINT32(xdrs, (int32_t *)uint32_p);

    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}



/*
 * XDR booleans
 */
bool
xdr_bool(XDR *xdrs, bool *bp)
{
    int32_t lb;

    switch (xdrs->x_op) {

    case XDR_ENCODE:
        lb = *bp ? XDR_TRUE : XDR_FALSE;
        return (XDR_PUTINT32(xdrs, &lb));

    case XDR_DECODE:
        if (!XDR_GETINT32(xdrs, &lb)) {
            return false;
        }
        *bp = (lb == XDR_FALSE) ? false : true;
        return true;

    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}

/*
 * XDR enumerations
 */
bool
xdr_enum(XDR *xdrs, enum_t *ep)
{
    enum sizecheck { SIZEVAL }; /* used to find the size of an enum */

    /*
     * enums are treated as ints
     */
    /* LINTED */ if (sizeof (enum sizecheck) == sizeof (int64_t)) {
        return (xdr_int64_t(xdrs, (int64_t *)(void *)ep));
    } else /* LINTED */ if (sizeof (enum sizecheck) == sizeof (int32_t)) {
        return (xdr_int32_t(xdrs, (int32_t *)(void *)ep));
    } else {
        return false;
    }
}

/*
 * XDR opaque data
 * Allows the specification of a fixed size sequence of opaque bytes.
 * cp points to the opaque object and cnt gives the byte length.
 */
bool
xdr_opaque(XDR *xdrs, char *cp, u_int cnt)
{
    u_int rndup;
    static int crud[BYTES_PER_XDR_UNIT];

    /*
     * if no data we are done
     */
    if (cnt == 0)
        return true;

    /*
     * round byte count to full xdr units
     */
    rndup = cnt % BYTES_PER_XDR_UNIT;
    if (rndup > 0)
        rndup = BYTES_PER_XDR_UNIT - rndup;

    if (xdrs->x_op == XDR_DECODE) {
        if (!XDR_GETBYTES(xdrs, cp, cnt)) {
            return false;
        }
        if (rndup == 0)
            return true;
        return (XDR_GETBYTES(xdrs, (char *)(void *)crud, rndup));
    }

    if (xdrs->x_op == XDR_ENCODE) {
        if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
            return false;
        }
        if (rndup == 0)
            return true;
        return (XDR_PUTBYTES(xdrs, xdr_zero, rndup));
    }

    if (xdrs->x_op == XDR_FREE) {
        return true;
    }

    return false;
}

/*
 * XDR counted bytes
 * *cpp is a pointer to the bytes, *sizep is the count.
 * If *cpp is NULL maxsize bytes are allocated
 */
bool
xdr_bytes(XDR *xdrs, char **cpp, u_int *sizep, u_int maxsize)
{
    char *sp = *cpp;  /* sp is the actual string pointer */
    u_int nodesize;

    /*
     * first deal with the length since xdr bytes are counted
     */
    if (! xdr_u_int(xdrs, sizep)) {
        return false;
    }
    nodesize = *sizep;
    if ((nodesize > maxsize) && (xdrs->x_op != XDR_FREE)) {
        return false;
    }

    /*
     * now deal with the actual bytes
     */
    switch (xdrs->x_op) {

    case XDR_DECODE:
        if (nodesize == 0) {
            return true;
        }
        if (sp == NULL) {
            *cpp = sp = mem_alloc(nodesize);
        }
        if (sp == NULL) {
            printf("xdr_bytes: out of memory");
            return false;
        }
        /* FALLTHROUGH */

    case XDR_ENCODE:
        return (xdr_opaque(xdrs, sp, nodesize));

    case XDR_FREE:
        if (sp != NULL) {
            mem_free(sp, nodesize);
            *cpp = NULL;
        }
        return true;
    }
    /* NOTREACHED */
    return false;
}

/*
 * XDR a descriminated union
 * Support routine for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * an entry with a null procedure pointer.  The routine gets
 * the discriminant value and then searches the array of xdrdiscrims
 * looking for that value.  It calls the procedure given in the xdrdiscrim
 * to handle the discriminant.  If there is no specific routine a default
 * routine may be called.
 * If there is no specific or default routine an error is returned.
 */
#if 0 /* not used by NFS */
bool
xdr_union(XDR *xdrs,
    enum_t *dscmp,      /* enum to decide which arm to work on */
    char *unp,              /* the union itself */
    const struct xdr_discrim *choices,  /* [value, xdr proc] for each arm */
    xdrproc_t dfault)           /* default xdr routine */
{
    enum_t dscm;

    /*
     * we deal with the discriminator;  it's an enum
     */
    if (! xdr_enum(xdrs, dscmp)) {
        return false;
    }
    dscm = *dscmp;

    /*
     * search choices for a value that matches the discriminator.
     * if we find one, execute the xdr routine for that value.
     */
    for (; choices->proc != NULL_xdrproc_t; choices++) {
        if (choices->value == dscm)
            return ((*(choices->proc))(xdrs, unp));
    }

    /*
     * no match - execute the default xdr routine if there is one
     */
    return ((dfault == NULL_xdrproc_t) ? FALSE :
        (*dfault)(xdrs, unp));
}
#endif

/*
 * Non-portable xdr primitives.
 * Care should be taken when moving these routines to new architectures.
 */


/*
 * XDR null terminated ASCII strings
 * xdr_string deals with "C strings" - arrays of bytes that are
 * terminated by a NULL character.  The parameter cpp references a
 * pointer to storage; If the pointer is null, then the necessary
 * storage is allocated.  The last parameter is the max allowed length
 * of the string as specified by a protocol.
 */
bool
xdr_string(XDR *xdrs, char **cpp, u_int maxsize)
{
    char *sp = *cpp;  /* sp is the actual string pointer */
    u_int size = 0;
    u_int nodesize;

    /*
     * first deal with the length since xdr strings are counted-strings
     */
    switch (xdrs->x_op) {
    case XDR_FREE:
        if (sp == NULL) {
            return true;    /* already free */
        }
        /* FALLTHROUGH */
    case XDR_ENCODE:
        size = strlen(sp);
        break;
    case XDR_DECODE:
        break;
    }
    if (! xdr_u_int(xdrs, &size)) {
        return false;
    }
    if (size > maxsize) {
        return false;
    }
    nodesize = size + 1;

    /*
     * now deal with the actual bytes
     */
    switch (xdrs->x_op) {

    case XDR_DECODE:
        if (nodesize == 0) {
            return true;
        }
        if (sp == NULL)
            *cpp = sp = mem_alloc(nodesize);
        if (sp == NULL) {
            printf("xdr_string: out of memory");
            return false;
        }
        sp[size] = 0;
        /* FALLTHROUGH */

    case XDR_ENCODE:
        return (xdr_opaque(xdrs, sp, size));

    case XDR_FREE:
        mem_free(sp, nodesize);
        *cpp = NULL;
        return true;
    }
    /* NOTREACHED */
    return false;
}

/* 
 * Wrapper for xdr_string that can be called directly from 
 * routines like clnt_call
 */
bool
xdr_wrapstring(XDR *xdrs, char **cpp)
{
    return xdr_string(xdrs, cpp, LASTUNSIGNED);
}

/*
 * XDR 64-bit integers
 */
bool
xdr_int64_t(XDR *xdrs, int64_t *llp)
{
    uint32_t ul[2];

    switch (xdrs->x_op) {
    case XDR_ENCODE:
        ul[0] = (uint32_t)((uint64_t)*llp >> 32) & 0xffffffff;
        ul[1] = (uint32_t)((uint64_t)*llp) & 0xffffffff;
        if (XDR_PUTINT32(xdrs, (int32_t *)&ul[0]) == FALSE)
            return false;
        return (XDR_PUTINT32(xdrs, (int32_t *)&ul[1]));
    case XDR_DECODE:
        if (XDR_GETINT32(xdrs, (int32_t *)&ul[0]) == FALSE)
            return false;
        if (XDR_GETINT32(xdrs, (int32_t *)&ul[1]) == FALSE)
            return false;
        *llp = (int64_t)
            (((uint64_t)ul[0] << 32) | ((uint64_t)ul[1]));
        return true;
    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}

/*
 * XDR unsigned 64-bit integers
 */
bool
xdr_uint64_t(XDR *xdrs, uint64_t *ullp)
{
    uint32_t ul[2];

    switch (xdrs->x_op) {
    case XDR_ENCODE:
        ul[0] = (uint32_t)(*ullp >> 32) & 0xffffffff;
        ul[1] = (uint32_t)(*ullp) & 0xffffffff;
        if (XDR_PUTINT32(xdrs, (int32_t *)&ul[0]) == FALSE)
            return false;
        return (XDR_PUTINT32(xdrs, (int32_t *)&ul[1]));
    case XDR_DECODE:
        if (XDR_GETINT32(xdrs, (int32_t *)&ul[0]) == FALSE)
            return false;
        if (XDR_GETINT32(xdrs, (int32_t *)&ul[1]) == FALSE)
            return false;
        *ullp = (uint64_t)
            (((uint64_t)ul[0] << 32) | ((uint64_t)ul[1]));
        return true;
    case XDR_FREE:
        return true;
    }
    /* NOTREACHED */
    return false;
}



/*
 * xdr_reference.c, Generic XDR routines impelmentation.
 *
 * Copyright (C) 1987, Sun Microsystems, Inc.
 *
 * These are the "non-trivial" xdr primitives used to serialize and de-serialize
 * "pointers".  See xdr.h for more info on the interface to xdr.
 */

/*
 * XDR an indirect pointer
 * xdr_reference is for recursively translating a structure that is
 * referenced by a pointer inside the structure that is currently being
 * translated.  pp references a pointer to storage. If *pp is null
 * the  necessary storage is allocated.
 * size is the sizeof the referneced structure.
 * proc is the routine to handle the referenced structure.
 */
static bool
xdr_reference(XDR *xdrs,
    char **pp,                  /* the pointer to work on */
    u_int size,                 /* size of the object pointed to */
    xdrproc_t proc)             /* xdr routine to handle the object */
{
    char *loc = *pp;
    bool bstat;

    if (loc == NULL)
        switch (xdrs->x_op) {
        case XDR_FREE:
            return true;

        case XDR_DECODE:
            *pp = loc = (char *) mem_alloc(size);
            if (loc == NULL) {
                printf("xdr_reference: out of memory");
                return false;
            }
            memset(loc, 0, size);
            break;

        case XDR_ENCODE:
            break;
    }

    bstat = (*proc)(xdrs, loc);

    if (xdrs->x_op == XDR_FREE) {
        mem_free(loc, size);
        *pp = NULL;
    }
    return bstat;
}


/*
 * xdr_pointer():
 *
 * XDR a pointer to a possibly recursive data structure. This
 * differs with xdr_reference in that it can serialize/deserialiaze
 * trees correctly.
 *
 *  What's sent is actually a union:
 *
 *  union object_pointer switch (boolean b) {
 *  case TRUE: object_data data;
 *  case FALSE: void nothing;
 *  }
 *
 * > objpp: Pointer to the pointer to the object.
 * > obj_size: size of the object.
 * > xdr_obj: routine to XDR an object.
 *
 */
bool
xdr_pointer(XDR *xdrs, char **objpp, u_int obj_size, xdrproc_t xdr_obj)
{
    bool more_data;

    more_data = (*objpp != NULL);
    if (! xdr_bool(xdrs,&more_data)) {
        return false;
    }
    if (! more_data) {
        *objpp = NULL;
        return true;
    }
    return xdr_reference(xdrs,objpp,obj_size,xdr_obj);
}


/*
 * xdr_array.c, Generic XDR routines impelmentation.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * These are the "non-trivial" xdr primitives used to serialize and de-serialize
 * arrays.  See xdr.h for more info on the interface to xdr.
 */

/*
 * XDR an array of arbitrary elements
 * *addrp is a pointer to the array, *sizep is the number of elements.
 * If addrp is NULL (*sizep * elsize) bytes are allocated.
 * elsize is the size (in bytes) of each element, and elproc is the
 * xdr procedure to call to handle each element of the array.
 */
bool
xdr_array(XDR *xdrs,
    char **addrp,               /* array pointer */
    u_int *sizep,               /* number of elements */
    u_int maxsize,              /* max numberof elements */
    u_int elsize,               /* size in bytes of each element */
    xdrproc_t elproc)           /* xdr routine to handle each element */
{
    u_int i;
    char *target = *addrp;
    u_int c;  /* the actual element count */
    bool bstat = true;
    u_int nodesize;

    /* like strings, arrays are really counted arrays */
    if (!xdr_u_int(xdrs, sizep)) {
        return false;
    }
    c = *sizep;
    if ((c > maxsize || UINT_MAX/elsize < c) &&
        (xdrs->x_op != XDR_FREE)) {
        return false;
    }
    nodesize = c * elsize;

    /*
     * if we are deserializing, we may need to allocate an array.
     * We also save time by checking for a null array if we are freeing.
     */
    if (target == NULL)
        switch (xdrs->x_op) {
        case XDR_DECODE:
            if (c == 0)
                return true;
            *addrp = target = mem_alloc(nodesize);
            if (target == NULL) {
                printf("xdr_array: out of memory");
                return false;
            }
            memset(target, 0, nodesize);
            break;

        case XDR_FREE:
            return true;

        case XDR_ENCODE:
            break;
    }

    /*
     * now we xdr each element of array
     */
    for (i = 0; (i < c) && bstat; i++) {
        bstat = (*elproc)(xdrs, target);
        target += elsize;
    }

    /*
     * the array may need freeing
     */
    if (xdrs->x_op == XDR_FREE) {
        mem_free(*addrp, nodesize);
        *addrp = NULL;
    }
    return bstat;
}

/*
 * xdr_vector():
 *
 * XDR a fixed length array. Unlike variable-length arrays,
 * the storage of fixed length arrays is static and unfreeable.
 * > basep: base of the array
 * > size: size of the array
 * > elemsize: size of each element
 * > xdr_elem: routine to XDR each element
 */
bool
xdr_vector(XDR *xdrs, char *basep, u_int nelem, u_int elemsize,
           xdrproc_t xdr_elem)
{
    u_int i;
    char *elptr;

    elptr = basep;
    for (i = 0; i < nelem; i++) {
        if (!(*xdr_elem)(xdrs, elptr)) {
            return false;
        }
        elptr += elemsize;
    }
    return true;
}
