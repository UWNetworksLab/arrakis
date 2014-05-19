/** \file
 * \brief Architecture-independent parts of in-kernel GDB stub.
 *
 * This file implements the kernel-side GDB stubs for remote debugging when
 * running on hardware. It is loosely based on the public domain i386-stub.c
 * which is part of the GDB sources.
 *
 * The following gdb commands are supported:
 *
 *<table>
 *<tr><td>command</td><td>function</td><td>Return value</td></tr>
 *<tr><td>?</td><td>What was the last sigval ?</td><td>SNN (signal NN)</td></tr>
 *<tr><td>g</td><td>return the value of the CPU registers</td><td>hex data or ENN</td></tr>
 *<tr><td>G</td><td>set the value of the CPU registers</td><td>OK or ENN</td></tr>
 *<tr><td>p</td><td>read the value of a single CPU register</td><td>hex data or ENN</td></tr>
 *<tr><td>P</td><td>set the value of a single CPU register</td><td>OK or ENN</td></tr>
 *<tr><td>mAA..AA,LLLL</td><td>Read LLLL bytes at address AA..AA</td><td>hex data or ENN</td></tr>
 *<tr><td>MAA..AA,LLLL:</td><td>Write LLLL bytes at address AA.AA</td><td>OK or ENN</td></tr>
 *<tr><td>c</td><td>Resume at current address</td><td>SNN (signal NN)</td></tr>
 *<tr><td>cAA..AA</td><td>Continue at address AA..AA</td><td>SNN</td></tr>
 *<tr><td>s</td><td>Step one instruction</td><td>SNN</td></tr>
 *<tr><td>sAA..AA</td><td>Step one instruction from AA..AA</td><td>SNN</td></tr>
 *<tr><td>D</td><td>GDB detached -- attempt to resume</td><td>(no reply)</td></tr>
 *<tr><td>k</td><td>kill -- reboots the system</td><td>(no reply)</td></tr>
 *</table>
 *
 * All commands and responses are sent with a packet which includes a
 * checksum. A packet consists of $\<packet info\>#\<checksum\> where:
 *
 * - \<packet info\> :: characters representing the command or response
 * - \<checksum\> :: two hex digits computed as mod-256 sum of \<packet info\>
 *
 * When a packet is received, it is first acknowledged with either '+' or '-'.
 * '+' indicates a successful transfer.  '-' indicates a failed transfer.
 *
 * Example:
 * - Host: $m0,10#2a
 * - Reply: +$00010203040506070809101112131415#42
 */

/*
 * Copyright (c) 2007, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <barrelfish_kpi/cpu.h>
#include <stdio.h>
#include <string.h>
#include <serial.h>
#include <exec.h>
#include <gdb_stub.h>
#include <arch_gdb_stub.h>

/** \brief Flag. Enables debug messages from the stub itself. */
#define DEBUG_ENABLED true

/** \brief Quick-and-dirty debug message macro. */
#define DEBUG(...)                          \
    if (DEBUG_ENABLED) {                    \
        debug(SUBSYS_GDB, __VA_ARGS__);     \
    }

/** \brief Defines the maximum number of characters in in/outbound buffers.
 *
 * At least NUMREGBYTES * 2 are needed for register packets. */
#define BUFMAX 512

/** \brief Wrapper for debugger output. */
static inline void myputchar(char c)
{
    serial_debug_putchar(c);
}

/** \brief Wrapper for debugger input. */
static inline char mygetchar(void)
{
    return serial_debug_getchar();
}

/** \brief Convert an ASCII hex character to its numeric value.
 * \return -1 if the character is invalid. */
static int fromhex(char ch)
{
    if ((ch >= 'a') && (ch <= 'f')) {
        return ch - 'a' + 10;
    } else if ((ch >= '0') && (ch <= '9')) {
        return ch - '0';
    } else if ((ch >= 'A') && (ch <= 'F')) {
        return ch - 'A' + 10;
    } else {
        return -1;
    }
}

/** \brief Convert a number 0..15 to the corresponding hex character. */
static char tohex(int i)
{
    static const char hexchars[] = "0123456789abcdef";
    assert(i >= 0 && i <= 0xf);
    return hexchars[i];
}

/** \brief Parse a hex-encoded integer
 *
 * Reads a hex-encoded integer into val, stopping at the first non-hex character
 * \return first position after the end of the consumed characters
 */
static const char * NONNULL NTS parse_hex_val(const char * NONNULL NTS str,
                                              uintptr_t * NONNULL val)
{
    uintptr_t ret = 0;

    for (int pos = 0; true; pos++) {
        int value = fromhex(str[pos]);
        if (value >= 0) {
            assert(pos < sizeof(uintptr_t) * 2); // overflow
            ret = (ret << 4) | value;
        } else {
            *val = ret;
            return &str[pos];
        }
    }
}

/** \brief Read memory region into a buffer as a hex string.
 *
 * Converts the memory pointed to by mem (of length memlen) into a hex string,
 * placing the result into buf (of length buflen), and terminating with a \\0.
 * \return Number of characters written on success, negative on failure.
 * May fail if a page fault would be incurred.
 */
static int mem_to_hex(lvaddr_t mem, size_t memlen,
                      char * NONNULL COUNT(buflen) buf, size_t buflen)
{
    int bufpos = 0;
    uint8_t ch;
    int r;

    if (buflen < memlen * 2 + 1) {
        return -1; // not enough space in output buffer
    }

    for (int mempos = 0; mempos < memlen; mempos++) {
        r = gdb_arch_read_byte((uint8_t * SAFE NONNULL)TC(mem + mempos), &ch);
        if (r != 0) {
            return r;
        }

        buf[bufpos++] = tohex(ch >> 4);  // top bits of char
        buf[bufpos++] = tohex(ch & 0xf); // bottom bits of char
    }

    buf[bufpos++] = '\0';
    return bufpos;
}

/** \brief Copy data from hex string in buffer to raw memory region.
 *
 * Writes data in the provided hex string buffer into the given memory region.
 * \return zero on success, negative on failure.
 * May fail if a page fault would be incurred.
 */
static int hex_to_mem(const char * NONNULL NT COUNT(memlen * 2) buf,
                      lvaddr_t mem, size_t memlen)
{
    for (int mempos = 0; mempos < memlen; mempos++) {
        uint8_t ch;

        assert(*buf != '\0');
        ch = fromhex(*buf++) << 4;
        assert(*buf != '\0');
        ch += fromhex(*buf++);

        int r = gdb_arch_write_byte(&((uint8_t * SAFE NONNULL)TC(mem))[mempos],
                                    ch);
        if (r != 0) {
            return r;
        }
    }

    return 0;
}

/** \brief Wait to receive a valid checksummed packet from GDB.
 *
 * Scans input for the sequence $\<packet info\>#\<checksum\>. If the
 * checksum is valid, acknowledges it to the sender and returns the buffer.
 * If invalid, reports failure to the sender and waits for the retransmission.
 */
static const char * NONNULL NTS getpacket(void)
{
    static char (NT buffer)[BUFMAX];
    uint8_t checksum, xmitcsum;
    int bufpos;
    char ch = 0; /* shut up a useless deputy warning */

    while (true) {
        /* wait around for the start character, ignore all other characters */
        while (mygetchar() != '$') {}

retry:
        checksum = 0;
        bufpos = 0;

        /* now, read until a # or end of buffer is found */
        while (bufpos < BUFMAX - 1) {
            ch = mygetchar();
            if (ch == '$') {
                goto retry; // invalid
            } else if (ch == '#') {
                break; // found start of checksum
            }
            checksum += ch;
            buffer[bufpos++] = ch;
        }
        buffer[bufpos] = 0;

        if (bufpos == BUFMAX - 1) {
            DEBUG("Warning: Incoming buffer full in getpacket()");
        }

        if (ch == '#') {
            xmitcsum = fromhex(mygetchar()) << 4;
            xmitcsum += fromhex(mygetchar());

            if (checksum != xmitcsum) {
                DEBUG("bad checksum. My count=0x%x, sent=0x%x. buf='%s'",
                      checksum, xmitcsum, buffer);
                myputchar('-'); // failed checksum
            } else {
                myputchar('+'); // successful transfer
                return buffer;
            }
        }
    }
}

/** \brief Send the packet in the buffer, adding a checksum.
 *
 * Sends the packet, using the format: $\<packet info\>#\<checksum\>.
 * Loops until it receives an acknowledgement response ('+') from GDB.
 */
static void putpacket(char * NONNULL NTS buffer)
{
    do {
        uint8_t checksum = 0;
        myputchar('$');

        for (int pos = 0; buffer[pos] != '\0'; pos++) {
            char ch = buffer[pos];
            myputchar(ch);
            checksum += ch;
        }

        myputchar('#');
        myputchar(tohex(checksum >> 4));   // top bits of checksum
        myputchar(tohex(checksum & 0xf));  // bottom bits of checksum
    } while (mygetchar() != '+');
}

/** \brief Generic entry point to the GDB stub.
 *
 * Wait for a remote GDB to start talking to us, and then service its requests.
 * This function should run on a different stack the main kernel, and does not
 * return.
 * \param signal_code Unix-style signal value indicating reason for interruption
 * \param init_message Optional initial message to send to the remote GDB
 */
void gdb_stub_entry(int signal_code, char * OPT NTS init_message)
{
    const char * NONNULL NTS request;
    const char * NONNULL NTS newpos;
    static char (NT buffer)[BUFMAX];
    static bool gdb_connected = false;
    char *reply;
    uintptr_t addr, length, regnum, value;
    int r;

    /* send initial message, if gdb is listening for it */
    if (init_message && gdb_connected) {
        putpacket(init_message);
    } else if (!gdb_connected) {
        printk(LOG_NOTE, "Waiting for GDB connection...\n");
    }

    /* loop handling requests */
    while (true) {
        request = getpacket();
        gdb_connected = true;
        reply = buffer;

        switch (*request++) {

        case '?':
            /* Indicate reason target halted */
            r = snprintf(buffer, sizeof(buffer), "S%02hhx", signal_code);
            assert(r < sizeof(buffer));
            if (r >= sizeof(buffer)) {
                /* ensure termination */
                buffer[sizeof(buffer) - 1] = '\0';
            }
            break;

        case 'g':
            /* Read general registers */
            r = mem_to_hex((lvaddr_t)GDB_ARCH_REGADDR, GDB_ARCH_REGBYTES,
                           buffer, sizeof(buffer));
            if (r < 0) {
                reply = "E03";
            }
            break;

        case 'G':
            /* Write general registers */
            r = hex_to_mem(request, (lvaddr_t)GDB_ARCH_REGADDR,
                           GDB_ARCH_REGBYTES);
            if (r == 0) {
                reply = "OK";
            } else {
                reply = "E03";
            }
            break;

        case 'p':
            /* Read a single register */
            newpos = parse_hex_val(request, &regnum);
            if (newpos == request) {
                reply = "E01";
                break;
            }

            r = gdb_arch_get_register(regnum, &value);
            if (r != 0) {
                reply = "E02";
                break;
            }

            r = mem_to_hex((lvaddr_t)&value, sizeof(uintptr_t), buffer,
                           sizeof(buffer));
            if (r < 0) {
                reply = "E03";
            }
            break;

        case 'P':
            /* Write a single register */
            newpos = parse_hex_val(request, &regnum);
            if (newpos == request || *newpos != '=') {
                reply = "E01";
                break;
            }
            request = newpos + 1;
            newpos = parse_hex_val(request, &value);
            if (newpos == request) {
                reply = "E01";
                break;
            }

            r = gdb_arch_set_register(regnum, value);
            if (r == 0) {
                reply = "OK";
            } else {
                reply = "E02";
            }
            break;

        case 'm':
            /* mAA..AA,LLLL  Read LLLL bytes at address AA..AA */
            request = parse_hex_val(request, &addr);
            if (addr == 0 || *(request++) != ',') {
                reply = "E01";
                break;
            }
            request = parse_hex_val(request, &length);
            if (length == 0) {
                reply = "E01";
                break;
            }
            r = mem_to_hex(addr, length, buffer, sizeof(buffer));
            if (r < 0) {
                reply = "E03";
            }
            break;

        case 'M':
            /* MAA..AA,LLLL: Write LLLL bytes at address AA.AA return OK */
            request = parse_hex_val(request, &addr);
            if (addr == 0 || *request++ != ',') {
                reply = "E01";
                break;
            }
            request = parse_hex_val(request, &length);
            if (length == 0 || *request++ != ':') {
                reply = "E01";
                break;
            }
            r = hex_to_mem(request, addr, length);
            if (r == 0) {
                reply = "OK";
            } else {
                reply = "E03";
            }
            break;

        case 's':
            /* Single step */
            parse_hex_val(request, &addr); // try to read optional parameter
            gdb_arch_single_step(addr); // should not return
            reply = "E02";
            break;

        case 'c':
            /* Continue execution */
            parse_hex_val(request, &addr); // try to read optional parameter
            gdb_arch_continue(addr); // should not return
            reply = "E02";
            break;

        case 'D':
            /* GDB detached, try to continue */
            printk(LOG_NOTE, "GDB has detached. Attempting to continue...");
            gdb_connected = false;
            gdb_arch_continue(0);
            break;

        case 'k':
            /* Kill the program */
            printk(LOG_NOTE, "Kill requested by remote GDB.\n");
            reboot();
            break;

        default:
            /* unsupported command */
            reply = "";
            break;
        }

        putpacket(reply);
    }
}
