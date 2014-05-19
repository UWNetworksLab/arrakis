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
 * Contributor(s): Warwick Harvey, IC-Parc. 
 * 
 * END LICENSE BLOCK */

/*--------------------------------------------------------------------
**
** Low-level C functions for implementing bitmaps.
**
** System:       ECLiPSe Constraint Logic Programming System
** Author/s:     Warwick Harvey, IC-Parc
**
** This file provides low-level primitives for manipulating bitmaps (e.g.
** for use in representing finite domains in IC).
**
**--------------------------------------------------------------------
**
** TODO:
**
** - For completeness, there should be a few more functions:
**   + create(+Offs, +IntBitmap, -Map)
**   + remove_range(?Map, +From, +To, -Change)
**   + subtract_from(?Map1, +Map2, -Change)
**
**-------------------------------------------------------------------*/

/*----------------------------------------------------------------------
**
** Load some needed header files.
**
*/

#include <string.h>		/* for memcpy() */

#include "external.h"
#include "bitmap.h"

/*
** Sketch of bitmap data structure to represent finite domains.
**
**	bm(
**		offset,		% offset to apply to values before
**				% converting to an index position.
**				% Note: must be multiple of word size.
**		low,		% lowest word containing non-zero bits
**		high,		% highest word containing non-zero bits
**		bits[0],
**		...
**		bits[low],
**		...
**		bits[high],
**		...
**	)
**
**	bits[0] .. bits[low-1] contain zeroes
**	bits[high+1] .. bits[(original high)] contain zeroes
**	bits[low] contains at least one set bit
**	bits[high] contains at least one set bit
**
** Actually, we get a bit lazy, and don't necessarily blank out all words
** outisde the domain on updates.
**
** In this module, `low' and `high' in variable names are generally used to
** refer to word index positions within the bitmap structure, while `min'
** and `max' are used to refer to the values being represented.
**
** *** Note:
**	Offsets are done relative to original values rather than index
**	positions in order to avoid most (unfortunately not all) problems
**	with division of negative numbers.  *Sigh*  This way, after
**	adjusting by the offset, everything within the domain is
**	non-negative and works fine.
*/

/*----------------------------------------------------------------------
**
** Define some useful constants and macros.
**
*/

    /* Bits per word. */
#define BPW		(8 * (int) sizeof(word))

    /* Word with all bits set. */
#define ALLBITS		MAX_U_WORD

    /* Word with just low bit set. */
#define LOWBIT		((word) 1)

    /* Word with just high bit set. */
#define HIGHBIT		(((word) 1) << (BPW - 1))

    /* Word with all bits set from bit n up. */
#define BitsFrom(n)	(ALLBITS << (n))

    /* Word with all bits set up to bit n. */
#define BitsTo(n)	((uword) ALLBITS >> (BPW-1-(n)))

    /* Word with just bit n set. */
#define Bit(n)		(LOWBIT << (n))


    /*
    ** Offsets into bitmap data structures.
    */

#define OFF_OFFSET	2
#define OFF_LOW		3
#define OFF_HIGH	4
#define OFF_BITS	5

#define HEADER_BYTES	(3 * sizeof(word))

    /*
    ** Bitmap access convenience macros.
    */

#define Offset(bitmap)		((bitmap)[OFF_OFFSET])
#define Low(bitmap)		((bitmap)[OFF_LOW])
#define High(bitmap)		((bitmap)[OFF_HIGH])
#define Bits(bitmap)		((bitmap) + OFF_BITS)

    /*
    ** Find the value of the minimum (maximum) element in the bitmap, given
    ** the low (high) word.
    */
#define bitmap_low_to_min(bitmap, low) \
	    ((low) * BPW + lsb(*(Bits(bitmap) + (low))))
#define bitmap_high_to_max(bitmap, high) \
	    ((high) * BPW + msb(*(Bits(bitmap) + (high))))


/*
** Some fake macros to make bitmaps look a bit like their own types -
** cf. sepia.h.
**
** Some public ones are in bitmap.h
*/

    /* Push a bitmap with the specified number of data words onto the heap. */
#define Push_Bitmap(pstruct, words)	{			\
	    pstruct = (uword *) TG;				\
	    Push_Buffer(HEADER_BYTES + words * sizeof(word))	\
	}

    /* Copy the given bitmap, keeping only the data words from low to high. */
#define Copy_Bitmap(bitmap, low, high, new_bitmap)	{	\
	    int words = (high) - (low) + 1;			\
								\
	    Push_Bitmap(new_bitmap, words);			\
	    Offset(new_bitmap) = Offset(bitmap) + (low) * BPW;	\
	    Low(new_bitmap) = 0;				\
	    High(new_bitmap) = words - 1;			\
	    memcpy(Bits(new_bitmap), Bits(bitmap) + (low),	\
			    sizeof(word) * words);		\
	}

    /*
    ** Copy the given bitmap, but only if it needs to be trailed (i.e.
    ** hasn't already been copied in this choice point).  Set `needed' based
    ** on whether the copy was done or not.
    */
#define Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, needed)	\
	if (Trail_Needed(bitmap)) {					\
	    Copy_Bitmap(bitmap, low, high, new_bitmap);			\
	    needed = 1;							\
	} else {							\
	    new_bitmap = bitmap;					\
	    needed = 0;							\
	}


/*----------------------------------------------------------------------
**
** Low-level bit-hacking functions.
**
*/

    /*
    ** bit_count(bits)
    **      Return the number of bits in the word `bits'.
    */
int
bit_count(uword bits)
{
#define MASK1   (ALLBITS / 0x03)
#define MASK2   (ALLBITS / 0x05)
#define MASK3   (ALLBITS / 0x11)

	bits = ((bits >> 1) & MASK1) + (bits & MASK1);
	bits = ((bits >> 2) & MASK2) + (bits & MASK2);
	bits = ((bits >> 4) + bits) & MASK3;
	bits = ((bits >> 8) + bits);
	bits = ((bits >> 16) + bits);
#if (SIZEOF_WORD > 4)
	bits = ((bits >> 32) + bits);
#endif
	return bits & 0xFF;	/* safe, works for word size to 128 bytes */
}


    /*
    ** lsb(bits)
    **      Returns the position of the least significant set bit in the
    **      given word.  Positions are numbered starting from 0 for the
    **      least significant bit position.  If no bits are set, then the
    **      position returned is the size of a word in bits (i.e. it's as if
    **      a set bit has been tacked on to the most significant end of the
    **      word).
    */
int
lsb(uword bits)
{
	int pos = 0;
	if (!bits) {
	    return BPW;
	}
#if (SIZEOF_WORD > 4)
	if (!(bits & 0xffffffff)) {
	    pos += 32;
	    bits >>= 32;
	}
#endif
	if (!(bits & 0xffff)) {
	    pos += 16;
	    bits >>= 16;
	}
	if (!(bits & 0xff)) {
	    pos += 8;
	    bits >>= 8;
	}
	if (!(bits & 0xf)) {
	    pos += 4;
	    bits >>= 4;
	}
	if (!(bits & 0x3)) {
	    pos += 2;
	    bits >>= 2;
	}
	if (!(bits & 0x1)) {
	    pos += 1;
	}
	return pos;
}

    /*
    ** msb(bits)
    **      Returns the position of the most significant set bit in the
    **      given word.  Positions are numbered starting from 0 for the
    **      least significant bit position.  If no bits are set, then the
    **      position returned is -1 (i.e. it's as if a set bit has been
    **      tacked on to the least significant end of the word).
    */
int
msb(uword bits)
{
	int pos = 0;
	if (!bits) {
	    return -1;
	}
#if (SIZEOF_WORD > 4)
	if (bits & 0xffffffff00000000) {
	    pos += 32;
	    bits >>= 32;
	}
#endif
	if (bits & 0xffff0000) {
	    pos += 16;
	    bits >>= 16;
	}
	if (bits & 0xff00) {
	    pos += 8;
	    bits >>= 8;
	}
	if (bits & 0xf0) {
	    pos += 4;
	    bits >>= 4;
	}
	if (bits & 0xc) {
	    pos += 2;
	    bits >>= 2;
	}
	if (bits & 0x2) {
	    pos += 1;
	}
	return pos;
}


/*----------------------------------------------------------------------
**
** Exported bitmap functions.
**
*/

    /*
    ** create_bitmap(++Min, ++Max, -Bitmap)
    **      Create a bitmap containing all elements from Min to Max.
    */
int
p_create_bitmap(value vmin, type tmin, value vmax, type tmax, value vbm, type tbm)
{
	uword	*bitmap;
	word	result;

	Check_Integer(tmin);
	Check_Integer(tmax);

	result = create_bitmap(vmin.nint, vmax.nint, &bitmap);
	Return_If_Not_Success(result);

	Return_Bitmap(vbm, tbm, bitmap);

	Succeed
}

word
create_bitmap(word min, word max, uword **bm_ptr)
{
	uword	*bitmap;
	uword	*bits_ptr;
	word	high, offset;
	word	words;

	if (max < min) {
	    Fail
	}

	/*
	** Compute the offset, making sure that the modulus of negative
	** numbers stupidity is handled correctly.
	*/
	offset = min;
	min %= BPW;
	if (min < 0) {
	    min += BPW;
	}
	offset -= min;

	max -= offset;

	high = max / BPW;
	words = high + 1;

	Push_Bitmap(bitmap, words);
	Offset(bitmap) = offset;
	Low(bitmap) = 0;
	High(bitmap) = high;

	bits_ptr = Bits(bitmap);
	if (words == 1) {
	    *bits_ptr = BitsFrom(min % BPW) & BitsTo(max % BPW);
	} else {
	    /* words >= 2 */
	    *bits_ptr = BitsFrom(min % BPW);
	    bits_ptr++;

	    while (--words > 1) {
		*bits_ptr = ALLBITS;
		bits_ptr++;
	    }

	    *bits_ptr = BitsTo(max % BPW);
	}

	*bm_ptr = bitmap;

	Succeed
}


    /*
    ** set_bitmap_lwb(+Bitmap, ++Min, -Result, -NewBitmap)
    **      Remove all elements smaller than Min from Bitmap, yielding
    **      NewBitmap.  The predicate always succeeds, with Result
    **      returning the following flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Imposed bound less than Min
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_set_bitmap_lwb(value vbm, type tbm, value vmin, type tmin,
		value vresult, type tresult, value vnew_bm, type tnew_bm)
{
	word	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tmin);

	result = set_bitmap_lwb(vbm.wptr, vmin.nint, (uword **) &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

word
set_bitmap_lwb(uword *bitmap, word min, uword **new_bm_ptr)
{
	word	*new_bitmap;
	uword	*bits_ptr;
	uword	bits;
	word	low, old_low, high, offset;
	word	result, old_min, old_max;
	int	copied;

	old_low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	min -= offset;
	old_min = bitmap_low_to_min(bitmap, old_low);
	old_max = bitmap_high_to_max(bitmap, high);

	if (old_min > old_max) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else if (min <= old_min) {
	    /* Imposed bound is weak. */
	    if (min == old_min) {
		result = 0;
	    } else {
		result = RES_SLACK;
	    }
	    new_bitmap = bitmap;
	} else if (min > old_max) {
	    /* Domain is now empty. */
	    result = RES_EMPTY + RES_CHANGED;
	    Copy_Bitmap_If_Needed(bitmap, high, high, new_bitmap, copied);
	    if (copied) {
		*Bits(new_bitmap) = 0;
	    } else {
		Low(new_bitmap) = high;
		*(Bits(new_bitmap) + high) = 0;
	    }
	} else {
	    /* Bound prunes bitmap. */
	    low = min / BPW;
	    bits_ptr = Bits(bitmap) + low;
	    bits = *bits_ptr;
	    bits &= BitsFrom(min % BPW);

	    if (!bits) {
		/* Low word now empty. */
		result = RES_CHANGED + RES_SLACK;

		do {
		    bits_ptr++;
		    low++;
		} while (!*bits_ptr);

		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (!copied) {
		    Low(new_bitmap) = low;
		}
	    } else {
		/* Check whether min has been excluded. */
		if (bits & Bit(min % BPW)) {
		    result = RES_CHANGED;
		} else {
		    result = RES_CHANGED + RES_SLACK;
		}

		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (copied) {
		    bits_ptr = Bits(new_bitmap);
		} else {
		    Low(new_bitmap) = low;
		}
		*bits_ptr = bits;
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** set_bitmap_upb(+Bitmap, ++Max, -Result, -NewBitmap)
    **      Remove all elements larger than Max from Bitmap, yielding
    **      NewBitmap.  The predicate always succeeds, with Result
    **      returning the following flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Imposed bound greater than Max
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_set_bitmap_upb(value vbm, type tbm, value vmax, type tmax,
		value vresult, type tresult, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tmax);

	result = set_bitmap_upb(vbm.wptr, vmax.nint, &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

word
set_bitmap_upb(uword *bitmap, word max, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	bits;
	word	low, high, old_high, offset;
	word	result, old_min, old_max;
	int	copied;

	low = Low(bitmap);
	old_high = High(bitmap);
	offset = Offset(bitmap);
	max -= offset;
	old_min = bitmap_low_to_min(bitmap, low);
	old_max = bitmap_high_to_max(bitmap, old_high);

	if (old_max < old_min) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else if (max >= old_max) {
	    /* Imposed bound is weak. */
	    if (max == old_max) {
		result = 0;
	    } else {
		result = RES_SLACK;
	    }
	    new_bitmap = bitmap;
	} else if (max < old_min) {
	    /* Domain is now empty. */
	    result = RES_EMPTY + RES_CHANGED;
	    Copy_Bitmap_If_Needed(bitmap, low, low, new_bitmap, copied);
	    if (copied) {
		*Bits(new_bitmap) = 0;
	    } else {
		High(new_bitmap) = low;
		*(Bits(new_bitmap) + low) = 0;
	    }
	} else {
	    /* Bound prunes bitmap. */
	    high = max / BPW;
	    bits_ptr = Bits(bitmap) + high;
	    bits = *bits_ptr;
	    bits &= BitsTo(max % BPW);

	    if (!bits) {
		/* High word now empty. */
		result = RES_CHANGED + RES_SLACK;

		do {
		    bits_ptr--;
		    high--;
		} while (!*bits_ptr);

		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (!copied) {
		    High(new_bitmap) = high;
		}
	    } else {
		/* Check whether max has been excluded. */
		if (bits & Bit(max % BPW)) {
		    result = RES_CHANGED;
		} else {
		    result = RES_CHANGED + RES_SLACK;
		}

		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (copied) {
		    bits_ptr = Bits(new_bitmap) + High(new_bitmap);
		} else {
		    High(new_bitmap) = high;
		}
		*bits_ptr = bits;
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** remove_bitmap_element(+Bitmap, ++Elem, -Result, -NewBitmap)
    **      Remove the element Elem from Bitmap, yielding NewBitmap.  The
    **      predicate always succeeds, with Result returning the following
    **      flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Elem falls outside Bitmap's range
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_remove_bitmap_element(value vbm, type tbm, value vel, type tel,
		value vresult, type tresult, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tel);

	result = remove_bitmap_element(vbm.wptr, vel.nint, &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);
	
	Succeed
}

word
remove_bitmap_element(uword *bitmap, word el, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	bits;
	word	low, high, offset, pos;
	word	result, min, max;
	int	bitmap_updated;
	int	copied;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	el -= offset;
	min = bitmap_low_to_min(bitmap, low);
	max = bitmap_high_to_max(bitmap, high);

	if (min > max) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else if (el < min || el > max) {
	    /* Excluded element is outside current domain. */
	    result = RES_SLACK;
	    new_bitmap = bitmap;
	} else if (el == min && el == max) {
	    /* Excluded element is the only element. */
	    result = RES_CHANGED + RES_EMPTY;
	    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
	    if (copied) {
		*Bits(new_bitmap) = 0;
	    } else {
		*(Bits(new_bitmap) + low) = 0;
	    }
	} else {
	    pos = el / BPW;

	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr;
	    bits &= ~Bit(el % BPW);

	    /*
	    ** Handle various special cases and determine result code.
	    */

	    bitmap_updated = 0;
	    if (el == min) {
		/* Excluded element is lower bound. */
		result = RES_CHANGED;

		if (bits == 0) {
		    do {
			bits_ptr++;
			low++;
		    } while (!*bits_ptr);

		    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		    if (!copied) {
			Low(new_bitmap) = low;
		    }
		    bitmap_updated = 1;
		}
	    } else if (el == max) {
		/* Excluded element is upper bound. */
		result = RES_CHANGED;

		if (bits == 0) {
		    do {
			bits_ptr--;
			high--;
		    } while (!*bits_ptr);

		    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		    if (!copied) {
			High(new_bitmap) = high;
		    }
		    bitmap_updated = 1;
		}
	    } else {
		if (bits == *bits_ptr) {
		    result = 0;
		    new_bitmap = bitmap;
		    bitmap_updated = 1;
		} else {
		    result = RES_CHANGED;
		}
	    }

	    /*
	    ** Default bitmap processing.
	    */

	    if (!bitmap_updated) {
		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (copied) {
		    bits_ptr = Bits(new_bitmap) + pos - low;
		}
		*bits_ptr = bits;
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** remove_bitmap_range(+Bitmap, ++Lo, ++Hi, -Result, -NewBitmap)
    **      Remove all the elements from Lo to Hi (inclusive) from Bitmap,
    **      yielding NewBitmap.  The predicate always succeeds, with Result
    **      returning the following flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Lo..Hi falls outside Bitmap's range
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_remove_bitmap_range(value vbm, type tbm, value vlo, type tlo, value vhi, type thi,
		value vresult, type tresult, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tlo);
	Check_Integer(thi);

	result = remove_bitmap_range(vbm.wptr, vlo.nint, vhi.nint, &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

word
remove_bitmap_range(uword *bitmap, word lo0, word hi0, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	bits;
	word	low, high, offset, pos, limit;
	word	result, min, max;
	int	copied;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	lo0 -= offset;
	hi0 -= offset;
	min = bitmap_low_to_min(bitmap, low);
	max = bitmap_high_to_max(bitmap, high);

	if (lo0 > hi0) {
	    /* Nothing to exclude. */
	    result = RES_SLACK;
	    new_bitmap = bitmap;
	} else if (hi0 >= max) {
	    return set_bitmap_upb(bitmap, lo0 - 1, new_bm_ptr);
	} else if (lo0 <= min) {
	    return set_bitmap_lwb(bitmap, hi0 + 1, new_bm_ptr);
	} else if (min > max) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else {
	    /* min < lo0 <= hi0 < hi */

	    pos = lo0 / BPW;
	    limit = hi0 / BPW;

	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr;

	    if (pos == limit) {
		bits &= ~(BitsFrom(lo0 % BPW) & BitsTo(hi0 % BPW));
	    } else {
		bits &= ~BitsFrom(lo0 % BPW);
	    }

	    if (bits == *bits_ptr) {
		result = 0;
		new_bitmap = bitmap;
	    } else {
		result = RES_CHANGED;
		Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
		if (copied) {
		    pos -= low;
		    limit -= low;
		    low = 0;
		    /* Don't care about high. */
		    bits_ptr = Bits(new_bitmap) + pos;
		    bitmap = new_bitmap;
		}
		*bits_ptr = bits;
	    }

	    if (pos < limit) {
		pos++;
		bits_ptr++;
		bits = *bits_ptr;

		while (pos < limit) {
		    if (bits != 0) {
			if (result != RES_CHANGED) {
			    result = RES_CHANGED;
			    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
			    if (copied) {
				pos -= low;
				limit -= low;
				low = 0;
				/* Don't care about high. */
				bits_ptr = Bits(new_bitmap) + pos;
				bitmap = new_bitmap;
			    }
			}
			*bits_ptr = 0;
		    }

		    pos++;
		    bits_ptr++;
		    bits = *bits_ptr;
		}

		bits &= ~BitsTo(hi0 % BPW);

		if (bits != *bits_ptr) {
		    if (result != RES_CHANGED) {
			result = RES_CHANGED;
			Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
			if (copied) {
			    pos -= low;
			    limit -= low;
			    low = 0;
			    /* Don't care about high. */
			    bits_ptr = Bits(new_bitmap) + pos;
			}
		    }
		    *bits_ptr = bits;
		}
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** bitmap_intersect_into(+Bitmap, +Bitmap2, -Result, -NewBitmap)
    **      Remove from Bitmap all elements which do not appear in Bitmap2,
    **      yielding NewBitmap.  The predicate always succeeds, with Result
    **      returning the following flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Bitmap's bounds were modified
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_bitmap_intersect_into(value vbm, type tbm, value vbm2, type tbm2,
		value vresult, type tresult, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Bitmap(tbm2);

	result = bitmap_intersect_into(vbm.wptr, vbm2.wptr, 0, &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

    /*
    ** Note that we allow a word-sized offset for bitmap2, since shifts of
    ** that size are efficient.  Non-word-sized offsets incur a performance
    ** penalty, and so are handled by a separate function,
    ** bitmap_shifted_intersect_into().
    */
word
bitmap_intersect_into(uword *bitmap, uword *bitmap2, word offset_adj, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	*bits_ptr2;
	uword	low_bits, high_bits, bits;
	word	low, high, offset;
	word	low2, high2, offset2;
	word	min, max;
	word	min2, max2;
	word	pos;
	word	delta;
	word	result;
	int	copied;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	low2 = Low(bitmap2);
	high2 = High(bitmap2);
	offset2 = Offset(bitmap2) + offset_adj;
	min = bitmap_low_to_min(bitmap, low);
	max = bitmap_high_to_max(bitmap, high);
	min2 = bitmap_low_to_min(bitmap2, low2);
	max2 = bitmap_high_to_max(bitmap2, high2);

	/* Add delta to convert an index for bitmap to an index for bitmap2. */
	/* This division works OK even if offset2 < offset since it is exact. */
	delta = (offset - offset2) / BPW;

	if (min > max) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else if (min2 + offset2 > max + offset || max2 + offset2 < min + offset) {
	    /* Bitmaps don't overlap, so intersection guaranteed empty. */
	    result = RES_CHANGED + RES_EMPTY;
	    Copy_Bitmap_If_Needed(bitmap, low, low, new_bitmap, copied);
	    if (copied) {
		*Bits(new_bitmap) = 0;
	    } else {
		High(new_bitmap) = low;
		*(Bits(new_bitmap) + low) = 0;
	    }
	} else {
	    /*
	    ** Note: we don't worry about setting result to RES_CHANGED for
	    ** bounds changes made here, since we test for them at the end
	    ** (in order to more easily establish whether to set RES_SLACK).
	    */

	    if (low2 - delta > low) {
		low = low2 - delta;
	    }
	    if (high2 - delta < high) {
		high = high2 - delta;
	    }

	    /* Find lowest word in result with bit set. */

	    bits_ptr = Bits(bitmap) + low;
	    low_bits = *bits_ptr;
	    bits_ptr2 = Bits(bitmap2) + low + delta;
	    low_bits &= *bits_ptr2;

	    while (!low_bits && low < high) {
		/* Low word empty. */
		bits_ptr++;
		bits_ptr2++;
		low++;
		low_bits = *bits_ptr;
		low_bits &= *bits_ptr2;
	    }

	    /* Find highest word in result with bit set. */

	    if (high > low) {
		bits_ptr = Bits(bitmap) + high;
		high_bits = *bits_ptr;
		bits_ptr2 = Bits(bitmap2) + high + delta;
		high_bits &= *bits_ptr2;

		while (!high_bits && high > low) {
		    /* High word empty. */
		    bits_ptr--;
		    bits_ptr2--;
		    high--;
		    high_bits = *bits_ptr;
		    high_bits &= *bits_ptr2;
		}
	    }

	    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
	    if (copied) {
		/*
		** Note that offset and delta may have been invalidated as well
		** as low and high.  offset is not used any more, but we need
		** to make sure we adjust delta correctly.
		*/
		delta += low - Low(new_bitmap);
		low = Low(new_bitmap);
		high = High(new_bitmap);
	    } else {
		Low(new_bitmap) = low;
		High(new_bitmap) = high;
	    }

	    bits_ptr = Bits(new_bitmap) + low;
	    if (low_bits == 0) {
		/* Bitmap now empty. */
		result = RES_CHANGED + RES_EMPTY;
		*bits_ptr = low_bits;
	    } else {
		result = 0;

		/* Update low word. */

		if (low_bits != *bits_ptr) {
		    result = RES_CHANGED;
		    *bits_ptr = low_bits;
		}

		if (high > low) {
		    /* Update intermediate words. */

		    pos = low + 1;
		    bits_ptr++;
		    bits_ptr2 = Bits(bitmap2) + pos + delta;

		    while (pos < high) {
			bits = *bits_ptr & *bits_ptr2;
			if (bits != *bits_ptr) {
			    result = RES_CHANGED;
			    *bits_ptr = bits;
			}

			pos++;
			bits_ptr++;
			bits_ptr2++;
		    }

		    /* Update high word. */

		    if (high_bits != *bits_ptr) {
			result = RES_CHANGED;
			*bits_ptr = high_bits;
		    }
		}

		/* Is it worth checking for no change and freeing new_bitmap? */

		if (bitmap_low_to_min(new_bitmap, low) > min ||
			bitmap_high_to_max(new_bitmap, high) < max) {
		    result = RES_CHANGED + RES_SLACK;
		}
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** bitmap_shifted_intersect_into(+Bitmap, +Bitmap2, ++Shift, -Result, -NewBitmap)
    **      Remove from Bitmap all elements which do not appear in Bitmap2
    **      after it has been shifted by Shift, yielding NewBitmap.  The
    **      predicate always succeeds, with Result returning the following
    **      flags as appropriate:
    **
    **      	RES_CHANGED	Bitmap was modified
    **      	RES_SLACK	Bitmap's bounds were modified
    **      	RES_EMPTY	Bitmap is now empty
    **
    **      Note that Bitmap is updated destructively, and should not be
    **      referred to again after the call to this predicate.
    */
int
p_bitmap_shifted_intersect_into(value vbm, type tbm, value vbm2, type tbm2,
		value vshift, type tshift, value vresult, type tresult,
		value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Bitmap(tbm2);
	Check_Integer(tshift);

	result = bitmap_shifted_intersect_into(vbm.wptr, vbm2.wptr,
		vshift.nint, &new_bitmap);

	Return_Integer(vresult, tresult, result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

/*
Intersect bitmap2 into bitmap, as if every element of bitmap2 has had
"shift" added to it.  "shift" is decomposed into a word-sized component
offset_adj and the (non-negative) remainder bit_adj.

          bit 32i       bit 32i+bit_adj   bit 32i+31
            lsb           word i     v    msb
            |. . . . . . . . . . . . . . . .|
    |. . . . . . . . . . . . . . . .|. . . . . . . . . . . . . . . .|
    lsb      ^ word i-offset_adj-1         word i-offset_adj      msb
       bit 32i-shift
    = 32(i-offset_adj-1) + (32-bit_adj)
*/
word
bitmap_shifted_intersect_into(uword *bitmap, uword *bitmap2, word shift, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	*bits_ptr2;
	uword	low_bits, high_bits, bits, tmp;
	word	low, high, offset;
	word	low2, high2, offset2;
	word	min, max;
	word	min2, max2;
	word	offset_adj, bit_adj;
	word	pos;
	word	delta;
	word	result;
	int	copied;

	bit_adj = shift % BPW;
	if (bit_adj == 0) {
	    /* Word-sized shift, so use the more efficient function. */
	    return bitmap_intersect_into(bitmap, bitmap2, shift, new_bm_ptr);
	}
	if (bit_adj < 0) {
	    bit_adj += BPW;
	}
	offset_adj = shift - bit_adj;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	low2 = Low(bitmap2);
	high2 = High(bitmap2);
	offset2 = Offset(bitmap2) + offset_adj;
	min = bitmap_low_to_min(bitmap, low);
	max = bitmap_high_to_max(bitmap, high);
	min2 = bitmap_low_to_min(bitmap2, low2);
	max2 = bitmap_high_to_max(bitmap2, high2);

	/* Add delta to convert an index for bitmap to an index for bitmap2. */
	/* This division works OK even if offset2 < offset since it is exact. */
	delta = (offset - offset2) / BPW;

	if (min > max) {
	    /* Old domain was empty. */
	    result = RES_EMPTY;
	    new_bitmap = bitmap;
	} else if (min2 + offset2 + bit_adj > max + offset
		|| max2 + offset2 + bit_adj < min + offset) {
	    /* Bitmaps don't overlap, so intersection guaranteed empty. */
	    result = RES_CHANGED + RES_EMPTY;
	    Copy_Bitmap_If_Needed(bitmap, low, low, new_bitmap, copied);
	    if (copied) {
		*Bits(new_bitmap) = 0;
	    } else {
		High(new_bitmap) = low;
		*(Bits(new_bitmap) + low) = 0;
	    }
	} else {
	    /*
	    ** Note: we don't worry about setting result to RES_CHANGED for
	    ** bounds changes made here, since we test for them at the end
	    ** (in order to more easily establish whether to set RES_SLACK).
	    */

	    /*
	    ** We set low and high to the lowest and highest word of bitmap
	    ** that may affect the outcome; note that the lowest word of
	    ** bitmap2 that may affect the outcome may be low + delta - 1;
	    ** note also that low + delta - 1 and high + delta may not exist
	    ** in bitmap2.
	    */

	    if (low2 - delta > low) {
		low = low2 - delta;
	    }
	    if (high2 + 1 - delta < high) {
		high = high2 + 1 - delta;
	    }

	    /* Find lowest word in result with bit set. */

	    bits_ptr = Bits(bitmap) + low;
	    low_bits = *bits_ptr;
	    bits_ptr2 = Bits(bitmap2) + low + delta;
	    /* If low2 == high2, bits_ptr2 may be out of bounds already. */
	    if (high2 < low + delta) {
		tmp = 0;
	    } else {
		tmp = (*bits_ptr2) << bit_adj;
	    }
	    /* Take into account the bits from the word below if it exists. */
	    if (low2 < low + delta) {
		tmp |= (*(bits_ptr2 - 1)) >> (BPW - bit_adj);
	    }
	    low_bits &= tmp;

	    while (!low_bits && low < high) {
		/* Low word empty. */
		bits_ptr++;
		low++;
		low_bits = *bits_ptr;
		tmp = (*bits_ptr2) >> (BPW - bit_adj);
		bits_ptr2++;
		tmp |= (*bits_ptr2) << bit_adj;
		low_bits &= tmp;
	    }

	    /* Find highest word in result with bit set. */

	    if (high > low) {
		bits_ptr = Bits(bitmap) + high;
		high_bits = *bits_ptr;
		bits_ptr2 = Bits(bitmap2) + high + delta - 1;
		tmp = (*bits_ptr2) >> (BPW - bit_adj);
		if (high2 >= high + delta) {
		    tmp |= (*(bits_ptr2 + 1)) << bit_adj;
		}
		high_bits &= tmp;

		while (!high_bits && high > low) {
		    /* High word empty. */
		    bits_ptr--;
		    high--;
		    high_bits = *bits_ptr;
		    tmp = (*bits_ptr2) << bit_adj;
		    bits_ptr2--;
		    tmp |= (*bits_ptr2) >> (BPW - bit_adj);
		    high_bits &= tmp;
		}
	    }

	    Copy_Bitmap_If_Needed(bitmap, low, high, new_bitmap, copied);
	    if (copied) {
		/*
		** Note that offset and delta may have been invalidated as well
		** as low and high.  offset is not used any more, but we need
		** to make sure we adjust delta correctly.
		*/
		delta += low - Low(new_bitmap);
		low = Low(new_bitmap);
		high = High(new_bitmap);
	    } else {
		Low(new_bitmap) = low;
		High(new_bitmap) = high;
	    }

	    bits_ptr = Bits(new_bitmap) + low;
	    if (low_bits == 0) {
		/* Bitmap now empty. */
		result = RES_CHANGED + RES_EMPTY;
		*bits_ptr = low_bits;
	    } else {
		result = 0;

		/* Update low word. */

		if (low_bits != *bits_ptr) {
		    result = RES_CHANGED;
		    *bits_ptr = low_bits;
		}

		if (high > low) {
		    /* Update intermediate words. */

		    pos = low + 1;
		    bits_ptr++;
		    bits_ptr2 = Bits(bitmap2) + pos + delta - 1;

		    while (pos < high) {
			bits = *bits_ptr;
			tmp = (*bits_ptr2) >> (BPW - bit_adj);
			bits_ptr2++;
			tmp |= (*bits_ptr2) << bit_adj;
			bits &= tmp;
			if (bits != *bits_ptr) {
			    result = RES_CHANGED;
			    *bits_ptr = bits;
			}

			pos++;
			bits_ptr++;
		    }

		    /* Update high word. */

		    if (high_bits != *bits_ptr) {
			result = RES_CHANGED;
			*bits_ptr = high_bits;
		    }
		}

		/* Is it worth checking for no change and freeing new_bitmap? */

		if (bitmap_low_to_min(new_bitmap, low) > min ||
			bitmap_high_to_max(new_bitmap, high) < max) {
		    result = RES_CHANGED + RES_SLACK;
		}
	    }
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** bitmaps_have_non_empty_intersection(+Bitmap, +Bitmap2)
    **      Test whether Bitmap and Bitmap2 have non-empty intersection.
    */

int
p_bitmaps_have_non_empty_intersection(value vbm, type tbm, value vbm2, type tbm2)
{
	Check_Bitmap(tbm);
	Check_Bitmap(tbm2);

	return bitmaps_have_non_empty_intersection(vbm.wptr, vbm2.wptr);
}

int
bitmaps_have_non_empty_intersection(uword *bitmap, uword *bitmap2)
{
	uword	*bits_ptr;
	uword	*bits_ptr2;
	uword	low_bits;
	word	low, high, offset;
	word	low2, high2, offset2;
	word	delta;

	low = Low(bitmap);
	high = High(bitmap);
	low2 = Low(bitmap2);
	high2 = High(bitmap2);

        if (low2 > high || high2 < low) {
	    /* Bitmaps don't overlap, so intersection guaranteed empty. */
	    Fail
	} 

	offset = Offset(bitmap);
	offset2 = Offset(bitmap2);

	/* Add delta to convert an index for bitmap to an index for bitmap2. */
	/* This division works OK even if offset2 < offset since it is exact. */
	delta = (offset - offset2) / BPW;

	if (low2 - delta > low) {
	    low = low2 - delta;
	}
	if (high2 - delta < high) {
	    high = high2 - delta;
	}

	/* Find a word with bit set. */

	bits_ptr = Bits(bitmap) + low;
	low_bits = *bits_ptr;
	bits_ptr2 = Bits(bitmap2) + low + delta;
	low_bits &= *bits_ptr2;

	while (!low_bits && low < high) {
	    /* Low word empty. */
	    bits_ptr++;
	    bits_ptr2++;
	    low++;
	    low_bits = *bits_ptr;
	    low_bits &= *bits_ptr2;
	}

	/* No common set bits */
	if (low_bits == 0) {
	    Fail
	}

	Succeed
}

    /*
    ** bitmap_union(+Bitmap, +Bitmap2, -NewBitmap)
    **
    **      Join all elements from Bitmap and Bitmap2, yielding
    **      NewBitmap.
    */
int
p_bitmap_union(value vbm, type tbm, value vbm2, type tbm2, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;
	word	result;

	Check_Bitmap(tbm);
	Check_Bitmap(tbm2);

	result = bitmap_union(vbm.wptr, vbm2.wptr, &new_bitmap);

	Return_If_Not_Success(result);
	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

word
bitmap_union(uword *bitmap, uword *bitmap2, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	uword	*bits_ptr;
	uword	*bits_ptr2;
	uword	*new_bits_ptr;
	word	low, high, offset;
	word	low2, high2, offset2;
	word	new_low, new_high, new_offset;
	word	min, max;
	word	min2, max2;
	word    delta, start, stop;
	word    delta2, start2, stop2;
	word	pos;
	word	result;
	word    new_words;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	low2 = Low(bitmap2);
	high2 = High(bitmap2);
	offset2 = Offset(bitmap2);
	min = offset + bitmap_low_to_min(bitmap, low);
	max = offset + bitmap_high_to_max(bitmap, high);
	min2 = offset2 + bitmap_low_to_min(bitmap2, low2);
	max2 = offset2 + bitmap_high_to_max(bitmap2, high2);


	if (min > max) {
	    /* bitmap 1 domain was empty. */
	    Copy_Bitmap(bitmap2, low2, high2, new_bitmap);
	    result = PSUCCEED;
	} else if (min2 > max2) {
	    /* bitmap 2 domain was empty. */
	    Copy_Bitmap(bitmap, low, high, new_bitmap);
	    result = PSUCCEED;
	} else {
	    /* Create a new bitmap from min(min,min2) to max(max,max2) */
	    result = create_bitmap((min<min2)?min:min2,
				   (max>max2)?max:max2,
				   &new_bitmap);
	    Return_If_Not_Success(result);

	    new_low = Low(new_bitmap);
	    new_high = High(new_bitmap);
	    new_offset = Offset(new_bitmap);
	    new_words = new_high + 1;

	    /*
            printf("low=%d, high=%d, offset=%d, min=%d, max=%d\n",
		   low, high, offset, min, max);
	    printf("low2=%d, high2=%d, offset2=%d, min2=%d, max2=%d\n",
		   low2, high2, offset2, min2, max2);
	    printf("new_low=%d, new_high=%d, new_offset=%d, new_words=%d, new_min=%d, new_max=%d\n",
		   new_low, new_high, new_offset, new_words,
		   bitmap_low_to_min(new_bitmap, new_low),
		   bitmap_high_to_max(new_bitmap, new_high));
	    */

	    /*
	    ** Copy words from bitmap and bitmap2 taking the bitunion where
	    ** appropriate.
	    */
	    delta = (new_offset - offset) / BPW;
	    delta2 = (new_offset - offset2) / BPW;
	    start = low - delta;
	    stop = high - delta;
	    start2 = low2 - delta2;
	    stop2 = high2 - delta2;

	    new_bits_ptr = Bits(new_bitmap);
	    bits_ptr = Bits(bitmap) + delta;
	    bits_ptr2 = Bits(bitmap2) + delta2;

	    /*
	    printf("start=%d, stop=%d, start2=%d, stop2=%d\n",
		   start, stop, start2, stop2);
	    */

	    /* pos is the index with new_bitmap */
	    for(pos=0; pos < new_words; pos++) {
		if ((pos >= start) && (pos <= stop)) {
		    /* copy the word from bitmap */
		    *new_bits_ptr = *bits_ptr;
		} else {
		    /* zero the memory */
		    *new_bits_ptr = 0;
		}
		/* bitwise union the words from bitmap2 */
		if ((pos >= start2) && (pos <= stop2)) {
		    /* or the word from bitmap2 */
		    *new_bits_ptr |= *bits_ptr2;
		}
		
		bits_ptr++;
		bits_ptr2++;
		new_bits_ptr++;
	    }
	    result = PSUCCEED;
	}

	*new_bm_ptr = new_bitmap;
	return result;
}


    /*
    ** copy_bitmap(+Bitmap, -NewBitmap)
    **      Make a new copy of Bitmap and return it as NewBitmap.  The
    **      predicate always succeeds.
    */
int
p_copy_bitmap(value vbm, type tbm, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;

	Check_Bitmap(tbm);

	copy_bitmap(vbm.wptr, &new_bitmap);

	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

void
copy_bitmap(uword *bitmap, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	word	low, high;

	low = Low(bitmap);
	high = High(bitmap);
	Copy_Bitmap(bitmap, low, high, new_bitmap);

	*new_bm_ptr = new_bitmap;
}


    /*
    ** copy_bitmap_shifted(+Bitmap, ++Shift, -NewBitmap)
    **      Make a new copy of Bitmap, shifted by Shift and return it as
    **      NewBitmap.
    */
int
p_copy_bitmap_shifted(value vbm, type tbm, value vshift, type tshift, value vnew_bm, type tnew_bm)
{
	uword	*new_bitmap;

	Check_Bitmap(tbm);
	Check_Integer(tshift);

	copy_bitmap_shifted(vbm.wptr, vshift.nint, &new_bitmap);

	Return_Bitmap(vnew_bm, tnew_bm, new_bitmap);

	Succeed
}

void
copy_bitmap_shifted(uword *bitmap, word shift, uword **new_bm_ptr)
{
	uword	*new_bitmap;
	word	low, high;
	word	offset_adj, bit_adj;
	word	words, i;
	uword	*bits_ptr, *new_bits_ptr;
	uword	tmp;

	low = Low(bitmap);
	high = High(bitmap);
	bit_adj = shift % BPW;
	if (bit_adj < 0) bit_adj += BPW;
	offset_adj = shift - bit_adj;

	if (bit_adj == 0) {
	    /* Easy case: we can just copy the bitmap and adjust the offset. */
	    Copy_Bitmap(bitmap, low, high, new_bitmap);
	    Offset(new_bitmap) += offset_adj;
	} else {
	    /* Have to do some bit-twiddling to shift the bitmap contents. */
	    words = high - low + 2; /* Lazy: might not need the extra word. */
	    Push_Bitmap(new_bitmap, words);
	    bits_ptr = Bits(bitmap) + low;
	    new_bits_ptr = Bits(new_bitmap);
	    *new_bits_ptr = (*bits_ptr) << bit_adj;
	    /* Low word might be empty, but then next guaranteed not. */
	    Low(new_bitmap) = (*new_bits_ptr == 0);
	    new_bits_ptr++;
	    for (i = low; i < high; i++) {
		tmp = (*bits_ptr) >> (BPW - bit_adj);
		bits_ptr++;
		tmp |= (*bits_ptr) << bit_adj;
		*new_bits_ptr = tmp;
		new_bits_ptr++;
	    }
	    *new_bits_ptr = (*bits_ptr) >> (BPW - bit_adj);
	    /* High word might be empty, but then previous guaranteed not. */
	    High(new_bitmap) = words - 1 - (*new_bits_ptr == 0);
	    Offset(new_bitmap) = Offset(bitmap) + low * BPW + offset_adj;
	}

	*new_bm_ptr = new_bitmap;
}


    /*
    ** bitmap_range(+Bitmap, -Min, -Max)
    **      Return the smallest and largest elements from Bitmap.  Fails if
    **      Bitmap is empty.
    */
int
p_bitmap_range(value vbm, type tbm, value vmin, type tmin, value vmax, type tmax)
{
	word	min, max;

	Check_Bitmap(tbm);

	if (bitmap_range(vbm.wptr, &min, &max) != 0) {
	    Fail
	}

	Return_Integer(vmin, tmin, min);
	Return_Integer(vmax, tmax, max);

	Succeed
}

word
bitmap_range(uword *bitmap, word *min_ptr, word *max_ptr)
{
	word	low, high, offset;
	word	min, max;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);

	min = bitmap_low_to_min(bitmap, low);
	max = bitmap_high_to_max(bitmap, high);

	if (min > max) {
	    return RES_EMPTY;
	} else {
	    *min_ptr = min + offset;
	    *max_ptr = max + offset;
	    return 0;
	}
}


    /*
    ** get_bitmap_lwb(+Bitmap, -Min)
    **      Return the smallest element from Bitmap.  Fails if Bitmap is
    **      empty.
    */
int
p_get_bitmap_lwb(value vbm, type tbm, value vmin, type tmin)
{
	word	min;

	Check_Bitmap(tbm);

	if (get_bitmap_lwb(vbm.wptr, &min) != 0) {
	    Fail
	}

	Return_Integer(vmin, tmin, min);

	Succeed
}

word
get_bitmap_lwb(uword *bitmap, word *min_ptr)
{
	word	low;
	word	min;

	low = Low(bitmap);

	min = bitmap_low_to_min(bitmap, low);

	if (min >= (low + 1) * BPW) {
	    return RES_EMPTY;
	} else {
	    *min_ptr = min + Offset(bitmap);
	    return 0;
	}
}


    /*
    ** get_bitmap_upb(+Bitmap, -Max)
    **      Return the largest element from Bitmap.  Fails if Bitmap is
    **      empty.
    */
int
p_get_bitmap_upb(value vbm, type tbm, value vmax, type tmax)
{
	word	max;

	Check_Bitmap(tbm);

	if (get_bitmap_upb(vbm.wptr, &max) != 0) {
	    Fail
	}

	Return_Integer(vmax, tmax, max);

	Succeed
}

word
get_bitmap_upb(uword *bitmap, word *max_ptr)
{
	word	high;
	word	max;

	high = High(bitmap);

	max = bitmap_high_to_max(bitmap, high);

	if (max < high * BPW) {
	    return RES_EMPTY;
	} else {
	    *max_ptr = max + Offset(bitmap);
	    return 0;
	}
}


    /*
    ** next_greater_member(+Bitmap, ++Curr, -Next)
    **      Return the smallest element in Bitmap which is greater than
    **      Curr.  Fails if there is no such element.
    */
int
p_next_greater_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext)
{
	word	next;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tcurr);

	result = next_greater_member(vbm.wptr, vcurr.nint, &next);
	if (Result_Is_Empty(result)) {
	    Fail
	}

	Return_Integer(vnext, tnext, next);

	Succeed
}

    /* Returns RES_EMPTY if there is no element larger than curr. */
    /* Returns RES_SLACK if next is not curr+1. */
word
next_greater_member(uword *bitmap, word curr, word *next_ptr)
{
	uword	*bits_ptr;
	uword	bits;
	word	low, high, offset, pos;
	word	next;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	next = curr - offset + 1;

	if (next < 0) {
	    /* Avoid division/modulus of a negative number problems. */
	    pos = -1;
	} else {
	    pos = next / BPW;
	}

	if (pos > high) {
	    return RES_EMPTY;
	}
	if (pos < low) {
	    pos = low;
	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr;
	} else {
	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr & BitsFrom(next % BPW);
	}

	while (!bits) {
	    if (pos >= high) {
		return RES_EMPTY;
	    }
	    pos++;
	    bits_ptr++;
	    bits = *bits_ptr;
	}

	next = pos * BPW + lsb(bits);
	next += offset;
	*next_ptr = next;

	if (next == curr + 1) {
	    return 0;
	} else {
	    /* Skipped a hole. */
	    return RES_SLACK;
	}
}


    /*
    ** next_smaller_member(+Bitmap, ++Curr, -Next)
    **      Return the largest element in Bitmap which is less than Curr.
    **      Fails if there is no such element.
    */
int
p_next_smaller_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext)
{
	word	next;
	word	result;

	Check_Bitmap(tbm);
	Check_Integer(tcurr);

	result = next_smaller_member(vbm.wptr, vcurr.nint, &next);
	if (Result_Is_Empty(result)) {
	    Fail
	}

	Return_Integer(vnext, tnext, next);

	Succeed
}

    /* Returns RES_EMPTY if there is no element smaller than curr. */
    /* Returns RES_SLACK if next is not curr-1. */
word
next_smaller_member(uword *bitmap, word curr, word *next_ptr)
{
	uword	*bits_ptr;
	uword	bits;
	word	low, high, offset, pos;
	word	next;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	next = curr - offset - 1;

	if (next < 0) {
	    /* Avoid division/modulus of a negative number problems. */
	    return RES_EMPTY;
	} else {
	    pos = next / BPW;
	}

	if (pos < low) {
	    return RES_EMPTY;
	}
	if (pos > high) {
	    pos = high;
	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr;
	} else {
	    bits_ptr = Bits(bitmap) + pos;
	    bits = *bits_ptr & BitsTo(next % BPW);
	}

	while (!bits) {
	    if (pos <= low) {
		return RES_EMPTY;
	    }
	    pos--;
	    bits_ptr--;
	    bits = *bits_ptr;
	}

	next = pos * BPW + msb(bits);
	next += offset;
	*next_ptr = next;

	if (next == curr - 1) {
	    return 0;
	} else {
	    /* Skipped a hole. */
	    return RES_SLACK;
	}
}


    /*
    ** next_greater_non_member(+Bitmap, ++Curr, -Next)
    **      Return the smallest element not in Bitmap which is greater than
    **      Curr.  Always succeeds.
    */
int
p_next_greater_non_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext)
{
	word	next;

	Check_Bitmap(tbm);
	Check_Integer(tcurr);

	next_greater_non_member(vbm.wptr, vcurr.nint, &next);

	Return_Integer(vnext, tnext, next);

	Succeed
}

    /* Returns RES_SLACK if next is not curr+1. */
word
next_greater_non_member(uword *bitmap, word curr, word *next_ptr)
{
	uword	*bits_ptr;
	uword	nobits;
	word	low, high, offset, pos;
	word	next;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	next = curr - offset + 1;

	/* Avoid division/modulus of a negative number problems. */
	if (next >= 0) {
	    pos = next / BPW;

	    if (pos >= low && pos <= high) {
		bits_ptr = Bits(bitmap) + pos;
		nobits = (~*bits_ptr) & BitsFrom(next % BPW);

		while (!nobits) {
		    if (pos >= high) {
			break;
		    }
		    pos++;
		    bits_ptr++;
		    nobits = ~*bits_ptr;
		}

		next = pos * BPW + lsb(nobits);
	    }
	}

	next += offset;
	*next_ptr = next;

	if (next == curr + 1) {
	    return 0;
	} else {
	    /* Skipped a member. */
	    return RES_SLACK;
	}
}


    /*
    ** next_smaller_non_member(+Bitmap, ++Curr, -Next)
    **      Return the largest element not in Bitmap which is less than
    **      Curr.  Always succeeds.
    */
int
p_next_smaller_non_member(value vbm, type tbm, value vcurr, type tcurr, value vnext, type tnext)
{
	word	next;

	Check_Bitmap(tbm);
	Check_Integer(tcurr);

	next_smaller_non_member(vbm.wptr, vcurr.nint, &next);

	Return_Integer(vnext, tnext, next);

	Succeed
}

    /* Returns RES_SLACK if next is not curr+1. */
word
next_smaller_non_member(uword *bitmap, word curr, word *next_ptr)
{
	uword	*bits_ptr;
	uword	nobits;
	word	low, high, offset, pos;
	word	next;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	next = curr - offset - 1;

	/* Avoid division/modulus of a negative number problems. */
	if (next >= 0) {
	    pos = next / BPW;

	    if (pos >= low && pos <= high) {
		bits_ptr = Bits(bitmap) + pos;
		nobits = (~*bits_ptr) & BitsTo(next % BPW);

		while (!nobits) {
		    if (pos <= low) {
			break;
		    }
		    pos--;
		    bits_ptr--;
		    nobits = ~*bits_ptr;
		}

		next = pos * BPW + msb(nobits);
	    }
	}

	next += offset;
	*next_ptr = next;

	if (next == curr - 1) {
	    return 0;
	} else {
	    /* Skipped a member. */
	    return RES_SLACK;
	}
}


    /*
    ** bitmap_size(+Bitmap, -Size)
    **      Return the number of elements in Bitmap (i.e. number of bits
    **      set).  Always succeeds.
    */
int
p_bitmap_size(value vbm, type tbm, value vsize, type tsize)
{
	word	size;

	Check_Bitmap(tbm);

	size = bitmap_size(vbm.wptr);

	Return_Integer(vsize, tsize, size);

	Succeed
}

word
bitmap_size(uword *bitmap)
{
	uword	*bits_ptr;
	word	high, pos;
	word	count;

	high = High(bitmap);

	pos = Low(bitmap);
	bits_ptr = Bits(bitmap) + pos;
	count = 0;

	while (pos <= high) {
	    count += bit_count(*bits_ptr);
	    pos++;
	    bits_ptr++;
	}

	return count;
}


    /*
    ** bitmap_contains(+Bitmap, ++Elem)
    **      Succeeds iff Bitmap contains the element Elem.
    */
int
p_bitmap_contains(value vbm, type tbm, value vel, type tel)
{
	Check_Bitmap(tbm);
	Check_Integer(tel);

	return bitmap_contains(vbm.wptr, vel.nint);
}

word
bitmap_contains(uword *bitmap, word el)
{
	uword	*bits_ptr;
	word	low, high, offset, pos;

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);

	el -= offset;
	if (el < 0) {
	    return PFAIL;
	}

	pos = el / BPW;

	if (pos < low || pos > high) {
	    return PFAIL;
	} else {
	    bits_ptr = Bits(bitmap) + pos;

	    return ((*bits_ptr & Bit(el % BPW)) ? PSUCCEED : PFAIL);
	}
}


    /*
    ** bitmap_contains_range(+Bitmap, ++Min, ++Max)
    **      Succeeds iff Bitmap contains every element from Min to Max,
    **      inclusive.
    */
int
p_bitmap_contains_range(value vbm, type tbm, value vmin, type tmin, value vmax, type tmax)
{
	Check_Bitmap(tbm);
	Check_Integer(tmin);
	Check_Integer(tmax);

	return bitmap_contains_range(vbm.wptr, vmin.nint, vmax.nint);
}

word
bitmap_contains_range(uword *bitmap, word min, word max)
{
	uword	*bits_ptr;
	uword	mask;
	word	low, high, offset, pos, limit;

	if (max < min) {
	    /* Empty range. */
	    return PSUCCEED;
	}

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	min -= offset;
	max -= offset;

	/* We do this check before division in case min or max are negative. */
	if (min < low * BPW || max > high * BPW + BPW - 1) {
	    /* Bitmap does not contain range. */
	    return PFAIL;
	}

	pos = min/BPW;
	limit = max/BPW;

	bits_ptr = Bits(bitmap) + pos;

	if (pos == limit) {
	    mask = BitsFrom(min % BPW) & BitsTo(max % BPW);
	    if ((*bits_ptr & mask) != mask) {
		return PFAIL;
	    }
	} else {
	    mask = BitsFrom(min % BPW);
	    if ((*bits_ptr & mask) != mask) {
		return PFAIL;
	    }

	    pos++;
	    bits_ptr++;

	    while (pos < limit) {
		if (*bits_ptr != ALLBITS) {
		    return PFAIL;
		}

		pos++;
		bits_ptr++;
	    }

	    mask = BitsTo(max % BPW);
	    if ((*bits_ptr & mask) != mask) {
		return PFAIL;
	    }
	}

	return PSUCCEED;
}


    /*
    ** compare_bitmaps(?Res, +Bitmap, +Bitmap2)
    **      Compares Bitmap and Bitmap2 to see if they are equivalent
    **      (Res = (=)), Bitmap is a subset of Bitmap2 (Res = (<)), or
    **      Bitmap is a superset of Bitmap2 (Res = (>)).  Fails if none of
    **      these conditions are true (the bitmaps are incomparable).
    */
int
p_compare_bitmaps(value vres, type tres, value vbm, type tbm, value vbm2, type tbm2)
{
	word	res;
	dident	result;

	Check_Bitmap(tbm);
	Check_Bitmap(tbm2);

	if (compare_bitmaps(vbm.wptr, vbm2.wptr, &res) == PFAIL) {
	    Fail
	}

	if (res < 0)
	    result = d_.inf0;
	else if (res > 0)
	    result = d_.sup0;
	else
	    result = d_.unify0;

	Return_Unify_Atom(vres, tres, result);
}

word
compare_bitmaps(uword *bitmap, uword *bitmap2, word *res_ptr)
{
	uword	*bits_ptr;
	uword	*bits_ptr2;
	uword	bits, bits2;
	uword	intersection;
	word	low, high, offset;
	word	low2, high2, offset2;
	word	pos;
	word	delta;
	word	res;

	if (bitmap == bitmap2)
	{
	    *res_ptr = 0;
	    return PSUCCEED;
	}

	low = Low(bitmap);
	high = High(bitmap);
	offset = Offset(bitmap);
	low2 = Low(bitmap2);
	high2 = High(bitmap2);
	offset2 = Offset(bitmap2);

	/* Add delta to convert an index for bitmap to an index for bitmap2. */
	/* This works OK even if offset2 < offset since the division is exact. */
	delta = (offset - offset2) / BPW;

	if (low + delta < low2) {
	    res = 1;
	    low = low2 - delta;
	} else if (low2 < low + delta) {
	    res = -1;
	} else {
	    res = 0;
	}

	if (high + delta > high2) {
	    if (res < 0) {
		return PFAIL;
	    }
	    res = 1;
	    high = high2 - delta;
	} else if (high2 > high + delta) {
	    if (res > 0) {
		return PFAIL;
	    }
	    res = -1;
	}
	    
	pos = low;
	bits_ptr = Bits(bitmap) + pos;
	bits_ptr2 = Bits(bitmap2) + pos + delta;

	while (pos <= high) {
	    bits = *bits_ptr;
	    bits2 = *bits_ptr2;
	    intersection = bits & bits2;

	    if (intersection != bits) {
		/* bits2 is not a superset of or equal to bits. */
		if (res < 0) {
		    return PFAIL;
		}
		/* Assume bits is a superset of bits2. */
		res = 1;
	    }
	    if (intersection != bits2) {
		/* bits is not a superset of or equal to bits2. */
		if (res > 0) {
		    return PFAIL;
		}
		res = -1;
	    }

	    pos++;
	    bits_ptr++;
	    bits_ptr2++;
	}

	*res_ptr = res;
	return PSUCCEED;
}


