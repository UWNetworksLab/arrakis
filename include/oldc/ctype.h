/*
 * Australian Public Licence B (OZPLB)
 * 
 * Version 1-0
 * 
 * Copyright (c) 2004 National ICT Australia
 * 
 * All rights reserved. 
 * 
 * Developed by: Embedded, Real-time and Operating Systems Program (ERTOS)
 *               National ICT Australia
 *               http://www.ertos.nicta.com.au
 * 
 * Permission is granted by National ICT Australia, free of charge, to
 * any person obtaining a copy of this software and any associated
 * documentation files (the "Software") to deal with the Software without
 * restriction, including (without limitation) the rights to use, copy,
 * modify, adapt, merge, publish, distribute, communicate to the public,
 * sublicense, and/or sell, lend or rent out copies of the Software, and
 * to permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimers.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimers in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of National ICT Australia, nor the names of its
 *       contributors, may be used to endorse or promote products derived
 *       from this Software without specific prior written permission.
 * 
 * EXCEPT AS EXPRESSLY STATED IN THIS LICENCE AND TO THE FULL EXTENT
 * PERMITTED BY APPLICABLE LAW, THE SOFTWARE IS PROVIDED "AS-IS", AND
 * NATIONAL ICT AUSTRALIA AND ITS CONTRIBUTORS MAKE NO REPRESENTATIONS,
 * WARRANTIES OR CONDITIONS OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO ANY REPRESENTATIONS, WARRANTIES OR CONDITIONS
 * REGARDING THE CONTENTS OR ACCURACY OF THE SOFTWARE, OR OF TITLE,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT,
 * THE ABSENCE OF LATENT OR OTHER DEFECTS, OR THE PRESENCE OR ABSENCE OF
 * ERRORS, WHETHER OR NOT DISCOVERABLE.
 * 
 * TO THE FULL EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL
 * NATIONAL ICT AUSTRALIA OR ITS CONTRIBUTORS BE LIABLE ON ANY LEGAL
 * THEORY (INCLUDING, WITHOUT LIMITATION, IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHERWISE) FOR ANY CLAIM, LOSS, DAMAGES OR OTHER
 * LIABILITY, INCLUDING (WITHOUT LIMITATION) LOSS OF PRODUCTION OR
 * OPERATION TIME, LOSS, DAMAGE OR CORRUPTION OF DATA OR RECORDS; OR LOSS
 * OF ANTICIPATED SAVINGS, OPPORTUNITY, REVENUE, PROFIT OR GOODWILL, OR
 * OTHER ECONOMIC LOSS; OR ANY SPECIAL, INCIDENTAL, INDIRECT,
 * CONSEQUENTIAL, PUNITIVE OR EXEMPLARY DAMAGES, ARISING OUT OF OR IN
 * CONNECTION WITH THIS LICENCE, THE SOFTWARE OR THE USE OF OR OTHER
 * DEALINGS WITH THE SOFTWARE, EVEN IF NATIONAL ICT AUSTRALIA OR ITS
 * CONTRIBUTORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH CLAIM, LOSS,
 * DAMAGES OR OTHER LIABILITY.
 * 
 * If applicable legislation implies representations, warranties, or
 * conditions, or imposes obligations or liability on National ICT
 * Australia or one of its contributors in respect of the Software that
 * cannot be wholly or partly excluded, restricted or modified, the
 * liability of National ICT Australia or the contributor is limited, to
 * the full extent permitted by the applicable legislation, at its
 * option, to:
 * a.  in the case of goods, any one or more of the following:
 * i.  the replacement of the goods or the supply of equivalent goods;
 * ii.  the repair of the goods;
 * iii. the payment of the cost of replacing the goods or of acquiring
 *  equivalent goods;
 * iv.  the payment of the cost of having the goods repaired; or
 * b.  in the case of services:
 * i.  the supplying of the services again; or
 * ii.  the payment of the cost of having the services supplied again.
 * 
 * The construction, validity and performance of this licence is governed
 * by the laws in force in New South Wales, Australia.
 */

/*
 Authors: Cristan Szmadja, Ben Leslie
 Description: 
  Implements character handling as per 7.4
 Status: Incomplete
  Ignores locale
*/

#ifndef _CTYPE_H_
#define	_CTYPE_H_

static inline int
iscntrl(int c)
{
	return (0 <= c && c <= '\037') || c == '\177';
}

static inline int
isgraph(int c)
{
	return ' ' < c && c < '\177';
}

static inline int
isprint(int c)
{
	return ' ' <= c && c < '\177';
}

static inline int
isdigit(int c)
{
	return '0' <= c && c <= '9';
}

static inline int
islower(int c)
{
	return 'a' <= c && c <= 'z';
}

static inline int
isspace(int c)
{
	return c == ' ' || ('\011' <= c && c <= '\015');
}

static inline int
isupper(int c)
{
	return 'A' <= c && c <= 'Z';
}

static inline int
isalpha(int c)
{
	return isupper(c) || islower(c);
}

static inline int
isalnum(int c)
{
	return isalpha(c) || isdigit(c);
}

static inline int
ispunct(int c)
{
	return isgraph(c) && !isalnum(c);
}

static inline int
isxdigit(int c)
{
	return isdigit(c) || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

static inline int
tolower(int c)
{
	return isupper(c) ? c + ('a' - 'A') : c;
}

static inline int
toupper(int c)
{
	return islower(c) ? c - ('a' - 'A') : c;
}

static inline int
isblank(int c)
{
	return c == ' ' || c == '\t';
}

static inline int
isascii(int c)
{
	return 0 <= c && c <= '\177';
}

static inline int
toascii(int c)
{
	return c & 0177;
}

static inline int
digittoint(int c)
{
	return isdigit(c) ? c - '0' : 'A' <= c
	    && c <= 'Z' ? c - 'A' : 'a' <= c && c <= 'z' ? c - 'a' : 99;
}

#endif				/* !_CTYPE_H_ */

/* XXX: Extra things needed by POSIX... need to find a correct #ifndef */

/*

int isascii(int);
int toascii(int);

#define _toupper(x)
#define _tolower(x)


*/



