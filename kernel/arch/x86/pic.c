/**
 * \file
 * \brief Classic 8259A PIC driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arch/x86/pic.h>
#include <dev/lpc_pic_dev.h>

/// The dual PIC
static lpc_pic_t pic;

/**
 * \brief Send end of interrupt.
 */
void pic_eoi(int irq)
{
    // Send specific end of interrupt message
    lpc_pic_ocw2_t eoi = lpc_pic_ocw2_default;
    eoi = lpc_pic_ocw2_rsleoi_insert(eoi, lpc_pic_seoi);

    if(irq < 8) {
        eoi = lpc_pic_ocw2_level_insert(eoi, irq);
        lpc_pic_master_ocw2_wr(&pic, eoi);
    } else {
        eoi = lpc_pic_ocw2_level_insert(eoi, irq - 8);
        lpc_pic_slave_ocw2_wr(&pic, eoi);
    }
}

/**
 * \brief returns true iff the PIC has an interrupt pending
 */
bool pic_have_interrupt(int irq)
{
    if(irq < 8) {
        // send read ISR command
        lpc_pic_master_ocw3_rrc_wrf(&pic, lpc_pic_read_is);
        // read ISR and check bit
        return (lpc_pic_master_ocw3rd_rd(&pic) & (1 << irq)) != 0;
    } else {
        lpc_pic_slave_ocw3_rrc_wrf(&pic, lpc_pic_read_is);
        return (lpc_pic_slave_ocw3rd_rd(&pic) & (1 << (irq -8))) != 0;
    }
}

static int mask_to_interrupt(uint8_t mask)
{
    for (int i = 0; i < 8; i++) {
        if (mask & (1 << i)) {
            return i;
        }
    }
    return -1;
}

/**
 * \brief Queries the PIC for pending interrupts
 *
 * \returns IRQ number of pending interrupt, or -1 if nothing is pending
 */
int pic_pending_interrupt(void)
{
    uint8_t isr;

    // try master first
    lpc_pic_master_ocw3_rrc_wrf(&pic, lpc_pic_read_is);
    isr = lpc_pic_master_ocw3rd_rd(&pic);
    if (isr != 0) {
        return mask_to_interrupt(isr);
    }

    // try slave
    lpc_pic_slave_ocw3_rrc_wrf(&pic, lpc_pic_read_is);
    isr = lpc_pic_slave_ocw3rd_rd(&pic);
    if (isr != 0) {
        return mask_to_interrupt(isr) + 8;
    }

    return -1;
}

/**
 * \brief Initialize 8259A.
 *
 * Initializes both master and slave 8259A in the standard cascaded
 * way (slave attached to IR line 2 of master). Sets off interrupts by
 * 32, leaving the lower 32 IRQs reserved for processor exceptions, as
 * required by protected mode. Sets all interrupts to edge
 * triggered. Finally, masks out all interrupts. If an interrupt is
 * expected by the OS, it has to be unmasked individually.
 */
void pic_init(void)
{
    // setup mackerel state
    lpc_pic_initialize(&pic, 0);

    // Setup 8259A PIC for proper protected mode interrupt delivery
    /* ICW1 */
    lpc_pic_master_icw1_ltim_wrf(&pic, 0);
    lpc_pic_slave_icw1_ltim_wrf( &pic, 0);

    /* ICW2 */
    lpc_pic_master_icw2_rawwr(&pic, 0x20); // IDT offset 0x20
    lpc_pic_slave_icw2_rawwr(&pic, 0x28);  // IDT offset 0x28

    /* ICW3 */
    lpc_pic_master_icw3_cascade_wrf(&pic, 1);
    lpc_pic_slave_icw3_slave_id_wrf(&pic, 2);

    /* ICW4 */
    lpc_pic_icw4_t icw4 = lpc_pic_icw4_default;
    icw4 = lpc_pic_icw4_aeoi_insert(icw4, 0);
    icw4 = lpc_pic_icw4_sfnm_insert(icw4, 0);
    lpc_pic_master_icw4_wr(&pic, icw4);
    lpc_pic_slave_icw4_wr(&pic, icw4);

    if (CPU_IS_M5_SIMULATOR) {
        printf("Warning: not setting elcr1 elcr2 on M5\n");
    } else {
        // Set all interrupts to be edge triggered (i.e. 0)
        lpc_pic_master_trigger_rawwr(&pic, 0);
        lpc_pic_slave_trigger_rawwr( &pic, 0);
    }

    // Mask all interrupts (except cascade IRQ 2)
    lpc_pic_slave_ocw1_wr(&pic, 0xff);
    lpc_pic_master_ocw1_wr(&pic, ~(1 << 2));
}

/**
 * \brief Enable/Disable interrupt 'irq'.
 *
 * Be careful to serialize calls to this function on a
 * multiprocessor. In general, the classic 8259A should not be used on
 * a multiprocessor.
 */
void pic_toggle_irq(int irq, bool enable)
{
    assert(irq >= 0 && irq <= 15);

    if(irq < 8) {
        // Master controller
        uint8_t mask = 1 << irq;
        uint8_t val = lpc_pic_master_ocw1_rd(&pic);

        if(enable) {
            val &= ~mask;
        } else {
            val |= mask;
        }

        lpc_pic_master_ocw1_wr(&pic, val);
    } else {
        // Slave controller
        uint8_t mask = 1 << (irq - 8);
        uint8_t val = lpc_pic_slave_ocw1_rd(&pic);

        if(enable) {
            val &= ~mask;
        } else {
            val |= mask;
        }

        lpc_pic_slave_ocw1_wr(&pic, val);
    }
}
