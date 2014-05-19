/**
 * \file
 * \brief Standard headers for kernel code.
 *
 * All C source in the kernel should include this file first.
 * This file should contain only definitions and prototypes that are
 * required for the majority of kernel code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __KERNEL_H
#define __KERNEL_H

#include <assert.h>
#include <stddef.h>
#include <stdio.h> // printf for debug
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/registers_arch.h>
#include <errors/errno.h>
#include <offsets.h> /* XXX */
#include <schedule.h>

extern coreid_t my_core_id;

bool arch_core_is_bsp(void);

/*
 * Utility macros.
 * FIXME: should we move these elsewhere?
 */

/** Macro to return the number of entries in a statically-allocated array. */
#define ARRAY_LENGTH(x) (sizeof(x) / sizeof((x)[0]))

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

/**
 * \brief Align block at base_addr with size n to power of two
 * alignable at its size
 *
 * Compute the highest exponent x of n so that 2^x \< n _and_
 * the base_addr is aligned at its size.
 * For example: n = 20, base_addr = 4 => size can be 1, 2 or 4.
 * Biggest possible block is 4 => x = 2
 *
 * \param n         Size of the block to split in bytes
 * \param base_addr Base address of the block to split
 *
 * \return Highest exponent (bits) for the blocksize to use next
 */

/// Computes the floor of log_2 of the given number
static inline uint8_t log2flr(uintptr_t num)
{
    uint8_t l = 0;
    uintptr_t n;
    for (n = num; n > 1; n >>= 1, l++);
    return l;
}

static inline int bitaddralign(size_t n, lpaddr_t base_addr)
{
    int exponent = sizeof(size_t) * NBBY - 1;

    if(n == 0) {
        return 0;
    }

    while ((exponent > 0) && ((base_addr % (1UL << exponent)) != 0)){
        exponent--;
    }
    return((1UL << exponent) > n ? log2flr(n) : exponent);
}

/**
 * Kernel subsystems.
 */
#define SUBSYS_STARTUP          (1 << 0)        ///< Startup
#define SUBSYS_GDB              (1 << 1)        ///< GDB stub
#define SUBSYS_APIC             (1 << 2)        ///< APIC driver
#define SUBSYS_ELF              (1 << 3)        ///< ELF64 loader
#define SUBSYS_PAGING           (1 << 4)        ///< Paging
#define SUBSYS_SYSCALL          (1 << 5)        ///< System calls
#define SUBSYS_CAPS             (1 << 6)        ///< Capabilities
#define SUBSYS_DISPATCH         (1 << 7)        ///< Scheduling and dispatch
#define SUBSYS_IO               (1 << 8)        ///< Low-level IO operations
#define SUBSYS_BMP              (1 << 9)        ///< BMP operations

/**
 * Kernel message loglevels.
 */
#define LOG_PANIC       0       ///< Panic
#define LOG_ERR         1       ///< Error
#define LOG_WARN        2       ///< Warning
#define LOG_NOTE        3       ///< Notice
#define LOG_DEBUG       4       ///< Debug

void debug_print_backtrace(void);
void panic(const char * NTS, ...)
    __attribute__((noreturn, format(printf, 1, 2)));
void breakpoint(void);
void printk(int level, const char *msg, ...)
    __attribute__ ((format(printf, 2, 3)));
int printf_nolog(const char * NTS fmt, ...)
    __attribute__ ((format(printf, 1, 2)));
void wait_cycles(uint64_t duration);
void kernel_startup_early(void);
void kernel_startup(void) __attribute__ ((noreturn));

/**
 * Command-line variable to set kernel logging level.
 */
extern int kernel_loglevel;

/**
 * Command-line variable to control which subsystems log. Bits defined
 * SUBSYS_* definitions in this file.
 */
extern int kernel_log_subsystem_mask;

/**
 * \brief Log a kernel debug message.
 *
 * Logs printf()-style debug message 'fmt' from subsystem 'subs'
 * to the default kernel console(s). Additional arguments are like
 * printf(). Whether the message is put out depends on the current
 * kernel log level, as well as on the current kernel subsystem log
 * mask.  'debug' is a macro so that the cost of marshalling the
 * arguments is avoided if the relevant debugging is disabled.
 *
 * \param _subs     Subsystem this message stems from.
 * \param _fmt      The message (printf() format string)
 */

#define debug(_subs, _fmt, ...) \
do { \
    if (((_subs) & kernel_log_subsystem_mask) && (kernel_loglevel > LOG_DEBUG)) \
        printk(LOG_DEBUG, _fmt, ## __VA_ARGS__); \
} while(0)

/**
 * command-line option for kernel timeslice in milliseconds.
 */
extern int kernel_timeslice;

/**
 * variable for gating timer interrupts.
 */
extern bool kernel_ticks_enabled;

/**
 * Current kernel epoch in number of kernel_timeslice elapsed.
 *
 * XXX AKK: shouldn't this be systime_t?
 * (It seems to count ms and not ticks anyway)
 */
extern size_t kernel_now;

extern lvaddr_t kernel_trace_buf;

extern struct capability monitor_ep;

/*
 * Variant based on Padraig Brady's implementation
 * http://www.pixelbeat.org/programming/gcc/static_assert.html
 */
#define ASSERT_CONCAT_(a, b) a##b
#define ASSERT_CONCAT(a, b) ASSERT_CONCAT_(a, b)

/* These can't be used after statements in c89. */
#ifdef __COUNTER__
  /* Microsoft compiler. */
  #define STATIC_ASSERT(e,m) \
    enum { ASSERT_CONCAT(static_assert_, __COUNTER__) = 1/(!!(e)) }
#else
  /* This can't be used twice on the same line so ensure if using in headers
   * that the headers are not included twice (by wrapping in #ifndef...#endif)
   * Note it doesn't cause an issue when used on same line of separate modules
   * compiled with gcc -combine -fwhole-program.  */
  #define STATIC_ASSERT(e,m) \
    enum { ASSERT_CONCAT(assert_line_, __LINE__) = 1/(!!(e)) }
#endif

#define STATIC_ASSERT_SIZEOF(tname,n)                   \
    STATIC_ASSERT(sizeof(tname) == n,                   \
        ASSERT_CONCAT("Size mismatch:", tname)          \
        )

#define sa_offsetof(x,y) ((size_t)(((void*)&(((x*)0)->y)) - (void*)(x*)0))

#define STATIC_ASSERT_OFFSETOF(tname, field, offset)    \
    STATIC_ASSERT(sa_offsetof(tname, field) == offset,    \
        ASSERT_CONCAT("Offset mismatch:", field)        \
        )

#endif // __KERNEL_H
