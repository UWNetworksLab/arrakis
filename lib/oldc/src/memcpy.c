
#include <string.h>
#include <stdint.h>


#if defined(__scc__)
#ifdef SCC_MEMCPY
/// XXX: Compile without -fPIE !

/****************************************************************************************
 * Put data into communication buffer.
 ****************************************************************************************
 *
 * Author: Stefan Lankes, Carsten Clauss
 *         Chair for Operating Systems, RWTH Aachen University
 * Date:   11/03/2010
 *
 ****************************************************************************************
 *
 * Written by the Chair for Operating Systems, RWTH Aachen University
 *
 * NO Copyright (C) 2010, Stefan Lankes, Carsten Clauss,
 * consider these trivial functions to be public domain.
 *
 * These functions are distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */

/*
 * A write access, which cache line is not present, doesn't perform (on the
 * current SCC architecture) a cache line fill. Therefore, the core writes
 * in this case directly to the memory.
 *
 * The following function copies from the on-die  memory (MPB) to the off-die
 * memory and prefetchs its destintation. Therefore, the  function avoids the
 * bad behavior of a "write miss".
 */
void *memcpy_scc1(void *dest, const void *src, size_t count)
{
	int h, i, j, k, l, m;

	__asm volatile ("cld;\n\t"
		      "1: cmpl $0, %%eax ; je 2f\n\t"
		      "movl (%%edi), %%edx\n\t"
		      "movl 0(%%esi), %%ecx\n\t"
		      "movl 4(%%esi), %%edx\n\t"
		      "movl %%ecx, 0(%%edi)\n\t"
		      "movl %%edx, 4(%%edi)\n\t"
		      "movl 8(%%esi), %%ecx\n\t"
		      "movl 12(%%esi), %%edx\n\t"
		      "movl %%ecx, 8(%%edi)\n\t"
		      "movl %%edx, 12(%%edi)\n\t"
		      "movl 16(%%esi), %%ecx\n\t"
		      "movl 20(%%esi), %%edx\n\t"
		      "movl %%ecx, 16(%%edi)\n\t"
		      "movl %%edx, 20(%%edi)\n\t"
		      "movl 24(%%esi), %%ecx\n\t"
		      "movl 28(%%esi), %%edx\n\t"
		      "movl %%ecx, 24(%%edi)\n\t"
		      "movl %%edx, 28(%%edi)\n\t"
		      "addl $32, %%esi\n\t"
		      "addl $32, %%edi\n\t"
		      "dec %%eax ; jmp 1b\n\t"
		      "2: movl %%ebx, %%ecx\n\t"
		      "movl (%%edi), %%edx\n\t"
		      "andl $31, %%ecx\n\t"
		      "rep ; movsb\n\t":"=&a" (h), "=&D"(i), "=&S"(j), "=&b"(k),
		      "=&c"(l), "=&d"(m)
		      :"0"(count / 32), "1"(dest), "2"(src),
		      "3"(count):"memory");

	return dest;
}

/*
 * If the destination is located on on-die memory (MPB), classical prefetching
 * techniques will be used to increase the performance.
 */
void *memcpy_scc2(void *dest, const void *src, size_t count)
{
	int i, j, k, l;

	/*
	 * We use the floating point registers to
	 * prefetch up to 4096 = (DCACE_SIZE (16KB) / 4) bytes.
	 */
	__asm volatile ("cmpl $63,%%ecx\n\t"
		      "jbe 1f\n\t"
		      "4: pushl %%ecx\n\t"
		      "cmpl $4096, %%ecx\n\t"
		      "jbe 2f\n\t"
		      "movl $4096,%%ecx\n\t"
		      "2: subl %%ecx,0(%%esp)\n\t"
		      "cmpl $256,%%ecx\n\t"
		      "jb 5f\n\t"
		      "pushl %%esi\n\t"
		      "pushl %%ecx\n\t"
		      ".align 4,0x90\n\t"
		      "3: movl 0(%%esi),%%eax\n\t"
		      "movl 32(%%esi),%%eax\n\t"
		      "movl 64(%%esi),%%eax\n\t"
		      "movl 96(%%esi),%%eax\n\t"
		      "movl 128(%%esi),%%eax\n\t"
		      "movl 160(%%esi),%%eax\n\t"
		      "movl 192(%%esi),%%eax\n\t"
		      "movl 224(%%esi),%%eax\n\t"
		      "addl $256,%%esi\n\t"
		      "subl $256,%%ecx\n\t"
		      "cmpl $256,%%ecx\n\t"
		      "jae 3b\n\t"
		      "popl %%ecx\n\t"
		      "popl %%esi\n\t"
		      ".align 2,0x90\n\t"
		      "5: fildq 0(%%esi)\n\t"
		      "fildq 8(%%esi)\n\t"
		      "fildq 16(%%esi)\n\t"
		      "fildq 24(%%esi)\n\t"
		      "fildq 32(%%esi)\n\t"
		      "fildq 40(%%esi)\n\t"
		      "fildq 48(%%esi)\n\t"
		      "fildq 56(%%esi)\n\t"
		      "fistpq 56(%%edi)\n\t"
		      "fistpq 48(%%edi)\n\t"
		      "fistpq 40(%%edi)\n\t"
		      "fistpq 32(%%edi)\n\t"
		      "fistpq 24(%%edi)\n\t"
		      "fistpq 16(%%edi)\n\t"
		      "fistpq 8(%%edi)\n\t"
		      "fistpq 0(%%edi)\n\t"
		      "addl $-64,%%ecx\n\t"
		      "addl $64,%%esi\n\t"
		      "addl $64,%%edi\n\t"
		      "cmpl $63,%%ecx\n\t"
		      "ja 5b\n\t"
		      "popl %%eax\n\t"
		      "addl %%eax,%%ecx\n\t"
		      "cmpl $64,%%ecx\n\t"
		      "jae 4b\n\t"
		      "1: movl %%ecx,%%eax\n\t"
		      "shrl $2,%%ecx\n\t"
		      "cld ; rep ; movsl\n\t"
		      "movl %%eax,%%ecx\n\t"
		      "andl $3,%%ecx\n\t"
		      "rep ; movsb\n\t":"=&c" (i), "=&D"(j), "=&S"(k), "=&a"(l)
		      :"0"(count), "1"(dest), "2"(src)
		      :"memory");

	return dest;
}

#endif // SCC_MEMCPY
#endif // defined(__scc__)

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
//#include <string.h>
//#include <stdint.h>

/*
 * sizeof(word) MUST BE A POWER OF TWO
 * SO THAT wmask BELOW IS ALL ONES
 */
typedef long    word;       /* "word" used for optimal copy speed */

#define wsize   sizeof(word)
#define wmask   (wsize - 1)

/*
 * Copy a block of memory, handling overlap.
 * This is the routine that actually implements
 * (the portable versions of) bcopy, memcpy, and memmove.
 */
void *
memcpy(void *dst0, const void *src0, size_t length)
{
    char        *dst;
    const char  *src;
    size_t      t;

    dst = dst0;
    src = src0;

    if (length == 0 || dst == src) {    /* nothing to do */
        goto done;
    }

    /*
     * Macros: loop-t-times; and loop-t-times, t>0
     */
#define TLOOP(s) if (t) TLOOP1(s)
#define TLOOP1(s) do { s; } while (--t)

    if ((unsigned long)dst < (unsigned long)src) {
        /*
         * Copy forward.
         */
        t = (size_t)src;    /* only need low bits */

        if ((t | (uintptr_t)dst) & wmask) {
            /*
             * Try to align operands.  This cannot be done
             * unless the low bits match.
             */
            if ((t ^ (uintptr_t)dst) & wmask || length < wsize) {
                t = length;
            } else {
                t = wsize - (t & wmask);
            }

            length -= t;
            TLOOP1(*dst++ = *src++);
        }
        /*
         * Copy whole words, then mop up any trailing bytes.
         */
        t = length / wsize;
        TLOOP(*(word *)dst = *(const word *)src; src += wsize;
            dst += wsize);
        t = length & wmask;
        TLOOP(*dst++ = *src++);
    } else {
        /*
         * Copy backwards.  Otherwise essentially the same.
         * Alignment works as before, except that it takes
         * (t&wmask) bytes to align, not wsize-(t&wmask).
         */
        src += length;
        dst += length;
        t = (uintptr_t)src;

        if ((t | (uintptr_t)dst) & wmask) {
            if ((t ^ (uintptr_t)dst) & wmask || length <= wsize) {
                t = length;
            } else {
                t &= wmask;
            }

            length -= t;
            TLOOP1(*--dst = *--src);
        }
        t = length / wsize;
        TLOOP(src -= wsize; dst -= wsize;
            *(word *)dst = *(const word *)src);
        t = length & wmask;
        TLOOP(*--dst = *--src);
    }
done:
    return (dst0);
}

