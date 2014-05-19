
// Borrowed from: http://git.musl-libc.org/cgit/musl/plain/src/time/__tm_to_time.c
//
// musl as a whole is licensed under the following standard MIT license:
// 
// Copyright © 2005-2012 Rich Felker
// 
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// 
// 
// Portions of this software are contributed or derived from software
// authored by third parties. Complete details on the copyright status of
// all code included in musl follows below:
// 
// 
// The TRE regular expression implementation (src/regex/reg* and
// src/regex/tre*) is Copyright © 2001-2008 Ville Laurikari and licensed
// under a 2-clause BSD license (license text in the source files). The
// included version has been heavily modified by Rich Felker in 2012, in
// the interests of size, simplicity, and namespace cleanliness.
// 
// Most of the math library code (src/math/* and src/complex/*) is
// Copyright © 1993,2004 Sun Microsystems or
// Copyright © 2003-2011 David Schultz or
// Copyright © 2003-2009 Steven G. Kargl or
// Copyright © 2003-2009 Bruce D. Evans or
// Copyright © 2008 Stephen L. Moshier
// and labelled as such. All have been licensed under extremely
// permissive terms. See the comments in the individual files for
// details.
// 
// The implementation of DES for crypt (src/misc/crypt_des.c) is
// Copyright © 1994 David Burren. It is licensed under a BSD license.
// 
// The implementation of blowfish crypt (src/misc/crypt_blowfish.c) was
// originally written by Solar Designer and placed into the public
// domain. The code also comes with a fallback permissive license for use
// in jurisdictions that may not recognize the public domain.
// 
// The smoothsort implementation (src/stdlib/qsort.c) is Copyright © 2011
// Valentin Ochs and is licensed under an MIT-style license.
// 
// The BSD PRNG implementation (src/prng/random.c) and XSI search API
// (src/search/*.c) functions are Copyright © 2011 Szabolcs Nagy and
// licensed under following terms: "Permission to use, copy, modify,
// and/or distribute this code for any purpose with or without fee is
// hereby granted. There is no warranty."
// 
// The x86_64 port was written by Nicholas J. Kain. Several files (crt)
// were released into the public domain; others are licensed under the
// standard MIT license terms at the top of this file. See individual
// files for their copyright status.
// 
// The mips and microblaze ports were originally written by Richard
// Pennington for use in the ellcc project. The original code was adapted
// by Rich Felker for build system and code conventions during upstream
// integration. It is licensed under the standard MIT terms.
// 
// The powerpc port was also originally written by Richard Pennington,
// and later supplemented and integrated by John Spencer. It is licensed
// under the standard MIT terms.
// 
// All other files which have no copyright comments are original works
// Copyright © 2005-2012 Rich Felker, the main author of this library.
// The decision to exclude such comments is intentional, as it should be
// possible to carry around the complete source code on tiny storage
// media. All public header files (include/*) should be treated as Public
// Domain as they intentionally contain no content which can be covered
// by copyright. Some source modules may fall in this category as well.
// If you believe that a file is so trivial that it should be in the
// Public Domain, please contact me and, if I agree, I will explicitly
// release it from copyright.
// 
// The following files are trivial, in my opinion not copyrightable in
// the first place, and hereby explicitly released to the Public Domain:
// 
// All public headers: include/*
// Startup files: crt/*


#define _GNU_SOURCE
#include <time.h>

/* C defines the rounding for division in a nonsensical way */
#define Q(a,b) ((a)>0 ? (a)/(b) : -(((b)-(a)-1)/(b)))

static time_t tm_to_time(struct tm *tm)
{
  time_t year  = tm->tm_year + -100;
  int    month = tm->tm_mon;
  int    day   = tm->tm_mday;
  int z4, z100, z400;

  /* normalize month */
  if (month >= 12) {
    year += month/12;
    month %= 12;
  } else if (month < 0) {
    year += month/12;
    month %= 12;
    if (month) {
      month += 12;
      year--;
    }
  }
  z4 = Q(year - (month < 2), 4);
  z100 = Q(z4, 25);
  z400 = Q(z100, 4);
  day += year*365 + z4 - z100 + z400 +
    month[(int []){0,31,59,90,120,151,181,212,243,273,304,335}];
  return (long long)day*86400
    + tm->tm_hour*3600 + tm->tm_min*60 + tm->tm_sec
    - -946684800; /* the dawn of time :) */
}

time_t timegm(struct tm *tm)
{
  return tm_to_time(tm);
}

#undef Q
