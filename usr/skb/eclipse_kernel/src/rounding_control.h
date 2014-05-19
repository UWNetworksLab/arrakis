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
 * Copyright (C) 2002-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
** ECLiPSe include file
**
** $Id: rounding_control.h,v 1.2 2008/07/16 23:58:32 kish_shen Exp $
**
** This file contains macro definitions and variable declarations used for
** controlling the rounding modes of the FPUs on various systems, as well as
** some other utility declarations and macros for widening results when
** rounding modes are not used.
**
** NOTE:
** Some architecture/OS/compiler combinations may require special
** compile-time flags to be specified whenever the code to be compiled
** depends on the rounding mode.  E.g. alpha_linux compiled with gcc
** requires "-mfp-rounding-mode=d" in order to use dynamic rounding modes
** (by default the standard rounding mode is statically compiled into all
** floating-point arithmetic instructions).  See the definition(s) of
** FLOAT_ROUND_FLAGS in Kernel/src/configure.in for the definitive list of
** what's required.
**
**
** PITFALLS TO WATCH FOR when using rounding modes.
**
**  1.  Unexpected extra roundings.
** 
**      The FPU may by default use more precision internally than is used
**      when a floating-point number is stored in memory (e.g. i386).  This
**      means it is possible for rounding to occur twice: once when an
**      arithmetic operation is performed, and once when the result is
**      written to memory.  This is a problem if the rounding mode is
**      changed between when a result is computed and when it is written to
**      memory.
**
**      There are two solutions to this.  One is to use gcc's -ffloat-store
**      compile flag, which makes sure floating point results are written
**      back out to memory right away.  The other is to tell the FPU to use
**      less precision (matching that of stored floats), assuming this is
**      possible.  We take the latter approach for i386_linux; note that
**      Windows (i386_nt) applications set the FPU to this mode by default,
**      so no special action is necessary.
**
**  2.  Code reordering.
**
**      Normally it is perfectly fine for compilers to reorder code which
**      has no apparent data dependency.  In particular, arithmetic
**      operations are sometimes moved past function calls if there is no
**      data dependence between the arithmetic operation and the parameters
**      of the function.  In our case though, that function call may change
**      the rounding mode, in which case moving the arithmetic operation
**      past the call changes its behaviour and must be prevented.
**
**      The (somewhat imperfect) solution to this is to declare as volatile
**      at least one input in each mathematical expression that should not
**      be reordered.  This means such expressions are not reordered (lest
**      the code moved past somehow alters the value) and any repeated
**      operations (e.g. with different rounding modes) will actually be
**      recomputed (lest the value has changed --- though in our case it's
**      not the input value which might be different, but the output value).
**      The drawback is that the generated code is less efficient than
**      necessary, since the volatile values are retrieved from memory each
**      time they are used even though the values will not have changed.
**
**
** This header file defines four macros relating to rounding.  These are:
**
** init_rounding_modes()
**	This should be called once at program start-up.  It computes the
**	values of one or more constants needed for switching or restoring
**	rounding modes and stores them in global variables for later use.
**
** set_round_up()
**	This changes the processor's rounding mode so that all results are
**	rounded towards positive infinity.
**
** set_round_down()
**	This changes the processor's rounding mode so that all results are
**	rounded towards negative infinity.
**
** restore_round_mode()
**	This restores the rounding mode of the processor to the state it was
**	in when init_rounding_modes() was called.
**
** Note that for some architectures the implementation of these macros
** assumes that the processor exception/trap mask flags are the same as they
** were for the call to init_rounding_modes().
**
** The global variables used by the above macros are defined in intervals.c.
*/

// XXX: need this on arm :-(
#if defined(__arm__)
    #define init_rounding_modes() { \
        \
        }
    #define set_round_up() { \
        \
        }
    #define set_round_down() { \
        \
        }
    #define restore_round_mode() { \
        \
        }
#endif

//asq:
#ifdef __x86_64__
#       define _FPU_SETCW
#endif

//akkourt: yap, It's ugly, but newlib does not seem to have those
#if defined(__i386__) && defined(CONFIG_NEWLIB)
#include "ieeefp.h"
/* copied from include/oldc/ieeefp.h */
#define FP_RND_FLD      0xc00   /* rounding control field */
#define FP_RND_OFF      10      /* rounding control offset */
static inline fp_rnd fpgetround(void)
{
    unsigned int cw;
    __asm volatile ("fnstcw %0" : "=m" (*&cw));
    return (fp_rnd)((cw & FP_RND_FLD) >> FP_RND_OFF);
}

static inline fp_rnd fpsetround(fp_rnd newround)
{
    fp_rnd p;
    unsigned int cw, newcw;

    __asm volatile ("fnstcw %0" : "=m" (*&cw));
    p = (fp_rnd)((cw & FP_RND_FLD) >> FP_RND_OFF);
    newcw = cw & ~FP_RND_FLD;
    newcw |= (newround << FP_RND_OFF) & FP_RND_FLD;
    __asm volatile ("fldcw %0" :: "m" (*&newcw));
    return p;
}
#endif

#ifdef HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#ifdef _WIN32

    #include <float.h>

    #ifndef EC_EXTERNAL
    #define DLLEXP __declspec(dllexport)
    #else
    #define DLLEXP __declspec(dllimport)
    #endif

    #define Declare_Rounding_Control_State \
	unsigned int ec_fpu_control_orig_;

    extern DLLEXP unsigned int ec_fpu_control_orig_;
    #define init_rounding_modes() { \
		ec_fpu_control_orig_ = _statusfp(); \
	    }
    #define set_round_up() { \
		_controlfp(_RC_UP, _MCW_RC); \
	    }
    #define set_round_down() { \
		_controlfp(_RC_DOWN, _MCW_RC); \
	    }
    #define restore_round_mode() { \
		_controlfp(ec_fpu_control_orig_, _MCW_RC); \
	    }

#elif defined(_FPU_SETCW)

#if defined(__x86_64) && defined(__SSE_MATH__)

    /* 
    ** On x86_64, gcc by default uses the SSE unit to compile floating
    ** point arithmetic, i.e. -mfpmath=sse rather than -mfpmath=387.
    ** The code in x86_64's fpu_control.h does however currently not
    ** handle this case, so we use our own.
    */

    /*
    ** Bits 14-13 of the 32-bit MXCSR Register are for rounding control:
    */

    #define Declare_Rounding_Control_State \
	unsigned int ec_fpu_control_orig_; \
	unsigned int ec_fpu_control_up_; \
	unsigned int ec_fpu_control_down_;

    extern unsigned int ec_fpu_control_orig_;
    extern unsigned int ec_fpu_control_up_;
    extern unsigned int ec_fpu_control_down_;

    #define EC_FPU_RC_MASK	0x00006000
    #define EC_FPU_RC_UP	0x00004000
    #define EC_FPU_RC_DOWN	0x00002000

    #define EC_FPU_GETCW(cw) __asm__ __volatile__ ("stmxcsr %0" : "=m" (*&cw))
    #define EC_FPU_SETCW(cw) __asm__ __volatile__ ("ldmxcsr %0" : : "m" (*&cw))

    #define init_rounding_modes() { \
		EC_FPU_GETCW(ec_fpu_control_orig_); \
		ec_fpu_control_up_ = (ec_fpu_control_orig_ & ~EC_FPU_RC_MASK) \
			| EC_FPU_RC_UP; \
		ec_fpu_control_down_ = (ec_fpu_control_orig_ & ~EC_FPU_RC_MASK) \
			| EC_FPU_RC_DOWN; \
	    }
    #define set_round_up() { \
		EC_FPU_SETCW(ec_fpu_control_up_); \
	    }
    #define set_round_down() { \
		EC_FPU_SETCW(ec_fpu_control_down_); \
	    }
    #define restore_round_mode() { \
		EC_FPU_SETCW(ec_fpu_control_orig_); \
	    }

#else

    /* e.g. i386_linux, or x86_64_linux compiled with -mfpmath=387 */

    /*
    ** When playing with rounding modes, we also set the precision to DOUBLE
    ** rather than the default EXTENDED.  This means we don't have to worry
    ** about any rounding occurring when a result is written back out to
    ** memory (at which time the rounding mode may have changed).
    **
    ** It's annoying that there are no convenient masks for rounding mode
    ** or precision...
    */

    #define Declare_Rounding_Control_State \
	fpu_control_t ec_fpu_control_orig_; \
	fpu_control_t ec_fpu_control_up_; \
	fpu_control_t ec_fpu_control_down_;

    extern fpu_control_t ec_fpu_control_orig_;
    extern fpu_control_t ec_fpu_control_up_;
    extern fpu_control_t ec_fpu_control_down_;

    /* Define some useful mask values. */
    #define EC_FPU_RC_MASK \
		(_FPU_RC_DOWN | _FPU_RC_UP | _FPU_RC_ZERO | _FPU_RC_NEAREST)
    #define EC_FPU_PRECISION_MASK \
		(_FPU_EXTENDED | _FPU_DOUBLE | _FPU_SINGLE)
    #define EC_FPU_MASK \
		(EC_FPU_RC_MASK | EC_FPU_PRECISION_MASK)

    #define init_rounding_modes() { \
		_FPU_GETCW(ec_fpu_control_orig_); \
		ec_fpu_control_up_ = ec_fpu_control_down_ = \
		    (ec_fpu_control_orig_ & ~EC_FPU_MASK) | _FPU_DOUBLE; \
		ec_fpu_control_up_ |= _FPU_RC_UP; \
		ec_fpu_control_down_ |= _FPU_RC_DOWN; \
	    }
    #define set_round_up() { \
		_FPU_SETCW(ec_fpu_control_up_); \
	    }
    #define set_round_down() { \
		_FPU_SETCW(ec_fpu_control_down_); \
	    }
    #define restore_round_mode() { \
		_FPU_SETCW(ec_fpu_control_orig_); \
	    }

#endif

#elif defined(HAVE_FPSETROUND)

    /* e.g. sparc_sunos5 */

    #include <ieeefp.h>

    #define Declare_Rounding_Control_State \
	fp_rnd ec_fpu_round_orig_;

    extern fp_rnd ec_fpu_round_orig_;

    #define init_rounding_modes() { \
		ec_fpu_round_orig_ = fpgetround(); \
	    }
    #define set_round_up() { \
		fpsetround(FP_RP); \
	    }
    #define set_round_down() { \
		fpsetround(FP_RM); \
	    }
    #define restore_round_mode() { \
		fpsetround(ec_fpu_round_orig_); \
	    }

#elif defined(__alpha__)

    /*
    ** alpha_linux does not appear to provide any way to obtain the current
    ** rounding mode from C, so we resort to assembler.
    */

    /*
    ** Explanation of Alpha FPU Control Register contents:
    ** 
    ** Bits   Meaning
    ** 63     Summary (bitwise or) of 57-52
    ** 62-60  Trap flags (don't touch)
    ** 59,58  Rounding mode: 01 is to -inf; 11 is to +inf; 10 is normal (near)
    ** 57-52  Result flags
    ** 51-47  Trap flags (don't touch)
    ** 46-0   Reserved
    */

    #define Declare_Rounding_Control_State \
	unsigned long ec_fpu_control_orig_; \
	unsigned long ec_fpu_control_up_; \
	unsigned long ec_fpu_control_down_;

    extern unsigned long ec_fpu_control_orig_;
    extern unsigned long ec_fpu_control_up_;
    extern unsigned long ec_fpu_control_down_;

    #define EC_FPU_RC_MASK	0x0C00000000000000L
    #define EC_FPU_RC_UP	0x0C00000000000000L
    #define EC_FPU_RC_DOWN	0x0400000000000000L

    #define init_rounding_modes() { \
		__asm__ ("mf_fpcr %0" : "=f" (*&ec_fpu_control_orig_)); \
		ec_fpu_control_up_ = ec_fpu_control_down_ = \
			ec_fpu_control_orig_ & ~EC_FPU_RC_MASK; \
		ec_fpu_control_up_   |= EC_FPU_RC_UP; \
		ec_fpu_control_down_ |= EC_FPU_RC_DOWN; \
	    }
    #define set_round_up() { \
		__asm__ ( \
		    "excb\n\t" \
		    "mt_fpcr %0\n\t" \
		    "excb" \
		    : : "f" (ec_fpu_control_up_) \
		); \
	    }
    #define set_round_down() { \
		__asm__ ( \
		    "excb\n\t" \
		    "mt_fpcr %0\n\t" \
		    "excb" \
		    : : "f" (ec_fpu_control_down_) \
		); \
	    }
    #define restore_round_mode() { \
		__asm__ ( \
		    "excb\n\t" \
		    "mt_fpcr %0\n\t" \
		    "excb" \
		    : : "f" (ec_fpu_control_orig_) \
		); \
	    }

#elif defined(__APPLE__) && defined(__MACH__)
/* MacOS X */
# ifndef __POWERPC__
/* assume this is Intel Mac */
/* This version uses standard C99 functions. Unfortunately we have reports
   that these functions are not defined for MacOS X 10.1, even though Apple's on-line 
   documentation claim that they are available from 10.0 onwards. There may also be issues
   with interaction with gcc's optimisation.
   Intel Macs are only publicly available from 10.5, so assume this code will work.
*/

    #include <fenv.h>

    #define Declare_Rounding_Control_State \
	int ec_fpu_round_orig_;

    extern int ec_fpu_round_orig_;

    #define init_rounding_modes() { \
                ec_fpu_round_orig_ = fegetround(); \
            }
    #define set_round_up() { \
                fesetround(FE_UPWARD); \
            }
    #define set_round_down() { \
                fesetround(FE_DOWNWARD); \
            }
    #define restore_round_mode() { \
                fesetround(ec_fpu_round_orig_); \
            }

# else  /* defined(__POWERPC__) */
/* this code is based on David K. Wittenberg's macros */

    #define Declare_Rounding_Control_State

    #define init_rounding_modes() {}

    #define set_round_up() { \
                __asm__ ("mtfsfi 7,2") ; \
            }

    #define set_round_down() { \
                __asm__ ("mtfsfi 7,3") ; \
            }

    /* assume round towards nearest is default */
    #define restore_round_mode() { \
                __asm__ ("mtfsfi 7,0") ; \
            }
# endif

#elif defined(HAVE_IEEE_FLAGS)

    /* e.g. sun4 */

    /* Last resort!  It uses strings! */

    #include <sys/ieeefp.h>
    #include <floatingpoint.h>

    #define Declare_Rounding_Control_State \
	char *ec_fpu_round_orig_;

    extern char *ec_fpu_round_orig_;

    #define init_rounding_modes() { \
		ieee_flags("get", "direction", NULL, &ec_fpu_round_orig_); \
	    }
    #define set_round_up() { \
		ieee_flags("set", "direction", "positive", NULL); \
	    }
    #define set_round_down() { \
		ieee_flags("set", "direction", "negative", NULL); \
	    }
    #define restore_round_mode() { \
		ieee_flags("set", "direction", ec_fpu_round_orig_, NULL); \
	    }

#endif


#define SAFE_ROUNDING
/* #define IEEE_ROUND_DOWN */
/* #define IEEE_INEXACT */


#define DOWN	0
#define UP	(!DOWN)

#ifdef SAFE_ROUNDING
#  ifdef IEEE_INEXACT
#    ifdef IEEE_ROUND_DOWN
#      define rinit()	result_inexact=0;
#      define down(x)	(x)
#      define up(x)	(result_inexact ? Up(x) : x)
#    else
#      define rinit()	result_inexact=0;
#      define down(x)	(result_inexact ? Down(x) : x)
#      define up(x)	(result_inexact ? Up(x) : x)
#    endif
#  else
#    ifdef IEEE_ROUND_DOWN
#      define rinit()
#      define down(x)	(x)
#      define up(x)	Up(x)
#    else
#      define rinit()
#      define down(x)	Down(x)
#      define up(x)	Up(x)
#    endif
#  endif
#else
#    define rinit()
#    define down(x)	(x)
#    define up(x)	(x)
#endif

#ifdef HAVE_NEXTAFTER
#define Up(x)		nextafter(x, HUGE_VAL)
#define Down(x)		nextafter(x, -HUGE_VAL)
#else
#define Up(x)		ec_ieee_up(x)
#define Down(x)		ec_ieee_down(x)
#endif

extern double ec_ieee_up ARGS((double));
extern double ec_ieee_down ARGS((double));

#ifdef HAVE_CEIL_NEGZERO_BUG
/* workaround for bug that incorrectly returns 0.0
   instead of -0.0 when argument is >-1.0 and <-0.0
*/
#define Ceil(x) \
  ( ceil(x) == 0.0 && x != -0.0 ? ceil(x)*x : ceil(x))

#else

#define Ceil(x) ceil(x)

#endif

