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
  Author: Ben Leslie
  Created: Fri Oct  8 2004 
*/

#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdio.h>
#include <assert.h>

/**
 * Work out the numeric value of a char, assuming up to base 36
 *  
 * @param ch The character to decode
 *
 * \return Numeric value of character, or 37 on failure
 */
static inline unsigned short
char_value(char ch)
{
	if (ch >= '0' && ch <= '9') {
		return ch - '0';
	}
	if (ch >= 'a' && ch <= 'z') {
		return ch - 'a' + 10;
	}
	if (ch >= 'A' && ch <= 'Z') {
		return ch - 'A' + 10;
	}
	
	return 37;
}


long int
strtol(const char *nptr, char **endptr, int base)
{
	/*
	  Decompose input info thread parts:
	  - inital list of whitespace (as per isspace)
	  - subject sequence
	  - final string one or more unrecognized
	*/
	const char *ptr = nptr;
	bool negative = false;
	unsigned int value;
	long int return_value = 0;
	/* Remove spaces */
	while(*ptr != '\0') {
		if (! isspace(*ptr)) {
			break;
		}
		ptr++;
	}

	if (*ptr == '\0') 
		goto fail;

	/* check [+|-] */	
	if (*ptr == '+') {
		ptr++;
	} else if (*ptr == '-') {
		negative = true;
		ptr++;
	}

	if (*ptr == '\0') 
		goto fail;

	if (base == 16) {
		/* _May_ have 0x prefix */
		if (*ptr == '0') {
			ptr++;
		        if (*ptr == 'x' || *ptr == 'X') {
				ptr++;
			}
		}
	}

	/* [0(x|X)+] */
	if (base == 0) {
		/* Could be hex or octal or decimal */
		if (*ptr != '0') {
			base = 10;
		} else {
			ptr++;
			if (ptr == '\0')
				goto fail;
			if (*ptr == 'x' || *ptr == 'X') {
				base = 16;
				ptr++;
			} else {
				base = 8;
			}
		}
	}

	if (*ptr == '\0')
		goto fail;

	/* Ok, here we have a base, and we might have a valid number */
	value = char_value(*ptr);
	if (value >= base) {
		goto fail;
	} else {
		return_value = value;
		ptr++;
	}

	while (*ptr != '\0' && (value = char_value(*ptr)) < base) {
		return_value = return_value * base + value;
		ptr++;
	}

	if (endptr != NULL)
		*endptr = (char*) ptr;

	if (negative) {
		return_value *= -1;
	}

	return return_value;

	/*
	  if base is 0, then we work it out based on a couple
	  of things 
	*/
	/*
	  [+|-][0(x|X)+][0-9A-Za-z] not LL *
	*/

	/* endptr == final string */

 fail:
	if (endptr != NULL)
		*endptr = (char*) nptr;
	return 0;

}
