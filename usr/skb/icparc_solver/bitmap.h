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
 * Copyright (C) 2001-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Warwick Harvey, IC-Parc
 * 
 * END LICENSE BLOCK */
/*--------------------------------------------------------------------
**
** Header file for low-level C functions implementing bitmaps.
**
** System:       ECLiPSe Constraint Logic Programming System
** Author/s:     Warwick Harvey, IC-Parc
**
** This file provides some definitions useful for C modules wishing to
** utilise the functions provided by the bitmap C module.
**
**-------------------------------------------------------------------*/


/*
** Result codes.
*/

#define RES_CHANGED	0x1	/* Domain was updated */
#define RES_SLACK	0x2	/* Bound given is not tight */
#define RES_EMPTY	0x4	/* Domain is empty */


/*
** Macros for testing result codes.
*/

#define	Result_Is_Empty(result)		(((result) & RES_EMPTY) == RES_EMPTY)
#define	Result_Is_Non_Empty(result)	(((result) & RES_EMPTY) == 0)
#define	Result_Is_Change(result)	(((result) & RES_CHANGED) == RES_CHANGED)
#define	Result_Is_Slack(result)		(((result) & RES_SLACK) == RES_SLACK)


/*
** Some fake macros to make bitmaps look a bit like their own types -
** cf. sepia.h
*/

    /* Use the string tag. */
#define	TBITMAP		TSTRG

    /* Type check. */
#define Check_Bitmap(item)	Check_String(item)

    /* Bitmap unifications. */
#define Request_Unify_Bitmap(vx,tx,vy) Request_Unify_Type(vx,tx,wptr,vy,TBITMAP)
#define Return_Unify_Bitmap(vx,tx,vy)  Return_Unify_Type(vx,tx,wptr,vy,TBITMAP)

    /* Return a bitmap or an integer. */
    /* NOTE: these clobber return variable --- use only with fresh variables. */
#define Return_Bitmap(v, t, b)			\
	{					\
            value bval;                         \
            type  btype ;                       \
            bval.wptr = b ;                     \
            btype.kernel = TBITMAP;             \
            Unify_Pw(v, t, bval, btype); \
	}
#define Return_Integer(v, t, i)			\
	{					\
	    v.ptr->val.nint = i;		\
	    v.ptr->tag.kernel = TINT;		\
	}


/*
** Function prototypes.
*/

extern	int	p_create_bitmap(value vmin, type tmin, value vmax, type tmax, value vbm, type tbm);
extern	word	create_bitmap(word min, word max, uword **bm_ptr);
extern	int	p_set_bitmap_lwb(value vbm, type tbm, value vmin, type tmin, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	set_bitmap_lwb(uword *bitmap, word min, uword **new_bm_ptr);
extern	int	p_set_bitmap_upb(value vbm, type tbm, value vmax, type tmax, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	set_bitmap_upb(uword *bitmap, word max, uword **new_bm_ptr);
extern	int	p_remove_bitmap_element(value vbm, type tbm, value vel, type tel, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	remove_bitmap_element(uword *bitmap, word el, uword **new_bm_ptr);
extern	int	p_remove_bitmap_range(value vbm, type tbm, value vlo, type tlo, value vhi, type thi, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	remove_bitmap_range(uword *bitmap, word lo0, word hi0, uword **new_bm_ptr);
extern	int	p_bitmap_intersect_into(value vbm, type tbm, value vbm2, type tbm2, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	bitmap_intersect_into(uword *bitmap, uword *bitmap2, word offset_adj, uword **new_bm_ptr);
extern	int	p_bitmap_shifted_intersect_into(value vbm, type tbm, value vbm2, type tbm2, value vshift, type tshift, value vresult, type tresult, value vnew_bm, type tnew_bm);
extern	word	bitmap_shifted_intersect_into(uword *bitmap, uword *bitmap2, word shift, uword **new_bm_ptr);
extern	int	p_bitmaps_have_non_empty_intersection(value vbm, type tbm, value vbm2, type tbm2);
extern	int	bitmaps_have_non_empty_intersection(uword *bitmap, uword *bitmap2);
extern	int	p_bitmap_union(value vbm, type tbm, value vbm2, type tbm2, value vnew_bm, type tnew_bm);
extern	word	bitmap_union(uword *bitmap, uword *bitmap2, uword **new_bm_ptr);
extern	int	p_copy_bitmap(value vbm, type tbm, value vnew_bm, type tnew_bm);
extern	void	copy_bitmap(uword *bitmap, uword **new_bm_ptr);
extern	int	p_copy_bitmap_shifted(value vbm, type tbm, value vshift, type tshift, value vnew_bm, type tnew_bm);
extern	void	copy_bitmap_shifted(uword *bitmap, word shift, uword **new_bm_ptr);
extern	int	p_bitmap_range(value vbm, type tbm, value vmin, type tmin, value vmax, type tmax);
extern	word	bitmap_range(uword *bitmap, word *min_ptr, word *max_ptr);
extern	int	p_get_bitmap_lwb(value vbm, type tbm, value vmin, type tmin);
extern	word	get_bitmap_lwb(uword *bitmap, word *min_ptr);
extern	int	p_get_bitmap_upb(value vbm, type tbm, value vmax, type tmax);
extern	word	get_bitmap_upb(uword *bitmap, word *max_ptr);
extern	int	p_next_greater_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext);
extern	word	next_greater_member(uword *bitmap, word curr, word *next_ptr);
extern	int	p_next_smaller_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext);
extern	word	next_smaller_member(uword *bitmap, word curr, word *next_ptr);
extern	int	p_next_greater_non_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext);
extern	word	next_greater_non_member(uword *bitmap, word curr, word *next_ptr);
extern	int	p_next_smaller_non_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext);
extern	word	next_smaller_non_member(uword *bitmap, word curr, word *next_ptr);
extern	int	p_bitmap_size(value vbm, type tbm, value vsize, type tsize);
extern	word	bitmap_size(uword *bitmap);
extern	int	p_bitmap_contains(value vbm, type tbm, value vel, type tel);
extern	word	bitmap_contains(uword *bitmap, word el);
extern	int	p_bitmap_contains_range(value vbm, type tbm, value vmin, type tmin, value vmax, type tmax);
extern	word	bitmap_contains_range(uword *bitmap, word min, word max);
extern	int	p_compare_bitmaps(value vres, type tres, value vbm, type tbm, value vbm2, type tbm2);
extern	word	compare_bitmaps(uword *bitmap, uword *bitmap2, word *res_ptr);

