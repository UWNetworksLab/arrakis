/** \file
 * \brief Miscellaneous kernel support code.
 *
 * This file contains miscellaneous architecture-independent kernel support
 * code that doesn't belong anywhere else.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <misc.h>
#include <dispatch.h>
#include <trace/trace.h>

#define DEFAULT_LOGLEVEL        LOG_NOTE
#define DEFAULT_SUBSYSTEM_MASK  (~0L)

/**
 * Global kernel loglevel.
 */
int kernel_loglevel = DEFAULT_LOGLEVEL;

/**
 * Default kernel subsystem message mask. Determines messages of what subsystems
 * get output.
 */
int kernel_log_subsystem_mask = DEFAULT_SUBSYSTEM_MASK;

/**
 * 'true' if kernel should handle and context switch on timer ticks.
 * Pass the ticks parameter on the kernel command line if you
 * want to change this.
 */
bool kernel_ticks_enabled = true;

/**
 * The current time since kernel start in timeslices.
 */
size_t kernel_now = 0;

/**
 * \brief Print a message and halt the kernel.
 *
 * Something irrecoverably bad happened. Print a panic message, then halt.
 */
void panic(const char *msg, ...)
{
    va_list ap;
    static char buf[256];

    va_start(ap, msg);
    vsnprintf(buf, sizeof(buf), msg, ap);
    va_end(ap);

    printf("kernel %d PANIC! %.*s\n", my_core_id, (int)sizeof(buf), buf);

    breakpoint();
    halt();
}

/**
 * \brief Log a kernel message.
 *
 * Logs printf()-style message 'msg', having loglevel 'level' to the default
 * kernel console(s). Additional arguments are like printf(). Whether the
 * message is put out depends on the current kernel log level.
 *
 * \param level Loglevel of message.
 * \param msg   The message (printf() format string)
 */
void printk(int level, const char *msg, ...)
{
    if(kernel_loglevel >= level) {
        va_list ap;
        static char buf[256];

        va_start(ap, msg);
        vsnprintf(buf, sizeof(buf), msg, ap);
        va_end(ap);

        printf("kernel %d: %.*s", my_core_id, (int)sizeof(buf), buf);
    }
}

/**
 * Helper function used in the implementation of assert()
 */
#ifdef CONFIG_OLDC
void __assert(const char *exp, const char *file, const char *func, int line)
#else /* CONFIG_NEWLIB */
void __assert_func(const char *file, int line, const char *func, const char *exp)
#endif
{
    panic("kernel assertion \"%s\" failed at %s:%d", exp, file, line);
}

void
wait_cycles(uint64_t duration)
{
    uint64_t last, elapsed;

    printk(LOG_NOTE, "Waiting %" PRIu64 " cycles...\n", duration);

    last = arch_get_cycle_count();
    elapsed = 0;
    while (elapsed < duration) {
        uint64_t now = arch_get_cycle_count();
        elapsed += (now - last);
        last = now;
    }
}

/**
 * Kernel trace buffer
 */
lvaddr_t kernel_trace_buf = 0;

struct trace_application kernel_trace_boot_applications[TRACE_MAX_BOOT_APPLICATIONS];
int kernel_trace_num_boot_applications = 0;
