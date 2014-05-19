/**
 * \file
 * \brief The world's simplest serial driver.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>

#include <ixp2800_uart_dev.h>
#include <ixp2800_uart.h>

/** \brief Initialize uart  */
void ixp2800_uart_init(ixp2800_uart_t *uart, lvaddr_t base)
{
    ixp2800_uart_initialize(uart, (mackerel_addr_t) base);

    ixp2800_uart_LCR_t lcr = ixp2800_uart_LCR_default;
    
    lcr = ixp2800_uart_LCR_sb_insert(lcr, 0);
    lcr = ixp2800_uart_LCR_eps_insert(lcr, 0);
    lcr = ixp2800_uart_LCR_pen_insert(lcr, 0);
    lcr = ixp2800_uart_LCR_wls_insert(lcr, ixp2800_uart_bits8);
    lcr = ixp2800_uart_LCR_stkyp_insert(lcr, 0);
    
    ixp2800_uart_LCR_wr(uart, lcr);


    //Mask all interrupts
    ixp2800_uart_IER_t ier = ixp2800_uart_IER_default;
    
    ier = ixp2800_uart_IER_ravie_insert(ier, 0);
    ier = ixp2800_uart_IER_tie_insert(ier, 0);
    ier = ixp2800_uart_IER_rlse_insert(ier, 0);
    ier = ixp2800_uart_IER_rtoie_insert(ier, 0);
    ier = ixp2800_uart_IER_nrze_insert(ier, 0);
    ier = ixp2800_uart_IER_uue_insert(ier, 1);

    ixp2800_uart_IER_wr(uart, ier);


    //Check current baud rate setting
    
    //Set DLAB to high to access the divisor
    ixp2800_uart_LCR_dlab_wrf(uart, 1);
    
    //Reset DLAB to low
    ixp2800_uart_LCR_dlab_wrf(uart, 0);

}

/** \brief Prints a single character to the default serial port. */
void ixp2800_putchar(ixp2800_uart_t *uart, char c)
{
    while(ixp2800_uart_LSR_tdrq_rdf(uart) == 0){ } //Spin until data transmission request
    
    ixp2800_uart_THR_thr_wrf(uart, c);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char ixp2800_getchar(ixp2800_uart_t *uart)
{
    char c;

    while (ixp2800_uart_LSR_dr_rdf(uart) == 0){ } //Spin until character is ready

    c = (char) ixp2800_uart_RBR_rbr_rdf(uart);

    return c;

}
