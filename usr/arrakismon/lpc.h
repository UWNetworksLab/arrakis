/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_H
#define LPC_H

#include <stdint.h>
#include <stdbool.h>
#include <timer/timer.h>
#include "apic.h"


/**
 * \brief Check whether an IRQ may be asserted on the VM right now.
 *
 * #irq and #irq_prio are defined if the fuction returns true. The describe
 * the current waiting interrupt and its priority.
 */
typedef bool (*lpc_virtual_irq_pending) (void *user_data, uint8_t *irq,
        uint8_t *irq_prio);

/**
 * \brief Assert an IRQ in the virtual machine.
 */
typedef void (*lpc_virtual_irq_handler) (void *user_data, uint8_t irq,
        uint8_t irq_prio);

// LPC Timer
union lpc_pit_tcw {
    struct {
        uint8_t     countdown_select        : 1;
        uint8_t     mode_select             : 3;
        uint8_t     rw_select               : 2;
        uint8_t     counter_select          : 2;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pit_sbyte {
    struct {
        uint8_t     countdown_type          : 1;
        uint8_t     mode_type               : 3;
        uint8_t     rw_mode                 : 2;
        uint8_t     count_avail             : 1;
        uint8_t     out_pin_state           : 1;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));


// NMI

union lpc_nmi_sc {
    struct {
        uint8_t     tim_cnt2_en             : 1; ///< Timer Counter 2 Enable 
        uint8_t     spkr_dat_en             : 1; ///< Speaker Data Enable
        uint8_t     pci_serr_en             : 1; ///< PCI SERR# Enable
        uint8_t     iochk_nmi_en            : 1; ///< IOCHK# NMI Enable
        uint8_t     ref_toggle              : 1; ///< Refresh Cycle Toggle
        uint8_t     tmr2_out_sts            : 1; ///< Timer Counter 2 OUT Status
        uint8_t     iochk_nmi_sts           : 1; ///< IOCHK# NMI Source Status
        uint8_t     serr_nmi_sts            : 1; ///< SERR# NMI Source Status
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));


// PIC

union lpc_pic_icw1 {
    struct {
        uint8_t     icw4_req                : 1; ///< ICW4 Write Required
        /// Single or Cascade, 0 means Cascade
        uint8_t     sigl_or_casc            : 1;
        uint8_t     adi                     : 1;
        uint8_t     edge_lvl_bank_sel       : 1; ///< Edge/Level Bank Select
        uint8_t     icw_sel                 : 1; ///< ICW/OCW Select
        uint8_t     rsvd                    : 3;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_icw2 {
    struct {
        uint8_t     intr_req_lvl            : 3; ///< Interrupt Request Level
        uint8_t     intr_vec_base_addr      : 5; ///< Interrupt Vector Base Address
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_icw3 {
    /// Master Controller
    struct {
        uint8_t     rsvd1                   : 2;
        /// Cascaded Interrupt Controller IRQ Connection
        uint8_t     casc_slave_ctrlr        : 1;
        uint8_t     rsvd2                   : 5;
    } __attribute__((packed)) um;
    /// Slave Controller
    struct {
        uint8_t     slave_id_code           : 3; ///< Slave Identification Code
        uint8_t     rsvd                    : 5;
    } __attribute__((packed)) us;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_icw4 {
    struct {
        uint8_t     microproc_mode      : 1; ///< Microprocessor Mode
        uint8_t     aeoi                : 1; ///< Automatic End of Interrupt
        uint8_t     ms_sl_mode          : 1; ///< Master/Slave in Buffered Mode
        uint8_t     buf_mode            : 1; ///< Buffered Mode
        uint8_t     sfn_mod             : 1; ///< Special Fully Nested Mode
        uint8_t     rsvd                : 3;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_ocw1 {
    struct {
        uint8_t     irq_mask            : 8; ///< Interrupt Request Mask
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_ocw2 {
    struct {
        uint8_t     irq_lvl_sel         : 3; ///< Interrupt Level Select
        uint8_t     ocw_sel             : 2; ///< OCW Select
        uint8_t     rot_eio_codes       : 3; ///< Rotate and EOI Codes
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_pic_ocw3 {
    struct {
        uint8_t     reg_read_cmd        : 2; ///< Register Read Command
        uint8_t     poll_mode           : 1; ///< Poll Mode Command
        uint8_t     ocw_sel             : 2; ///< OCW Select
        uint8_t     smm_en              : 1; ///< Enable Special Mask Mode
        uint8_t     smm                 : 1; ///< Special Mask Mode
        uint8_t     rsvd                : 1;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));


// RTC

union lpc_rtc_a {
    struct {
        uint8_t     rate_sel            : 3; ///< Rate Select
        uint8_t     div_chain_sel       : 3; ///< Division Chain Select
        uint8_t     uip                 : 1; ///< Update In Progress
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_rtc_b {
    struct {
        uint8_t     dse                 : 1; ///< Daylight Savings Enable
        uint8_t     hour_format         : 1; ///< Hour Format 
        uint8_t     data_mode           : 1; ///< Data Mode
        uint8_t     sqwe                : 1; ///< Square Wave Enable
        uint8_t     uie                 : 1; ///< Update-Ended Interrupt Enable
        uint8_t     aie                 : 1; ///< Alarm Interrupt Enable
        uint8_t     pie                 : 1; ///< Periodic Interrupt Enable
        uint8_t     set                 : 1; ///< Update Cycle Inhibit
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_rtc_c {
    struct {
        uint8_t     rsvd                : 4;
        uint8_t     uf                  : 1; ///< Update-Ended Flag
        uint8_t     af                  : 1; ///< Alarm Flag
        uint8_t     pf                  : 1; ///< Periodic Interrupt Flag
        uint8_t     irqf                : 1; ///< Interrupt Request Flag
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_rtc_d {
    struct {
        uint8_t     data_alarm          : 6; ///< Data Alarm
        uint8_t     rsvd                : 1;
        uint8_t     vrt                 : 1; ///< Valid RAM and Time Bit
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

union lpc_rtc_prim_ram {
    struct {
        uint8_t     seconds;
        uint8_t     seconds_alarm;
        uint8_t     minutes;
        uint8_t     minutes_alarm;
        uint8_t     hours;
        uint8_t     hours_alarm;
        uint8_t     day_of_week;
        uint8_t     day_of_month;
        uint8_t     month;
        uint8_t     year;
        union lpc_rtc_a A;
        union lpc_rtc_b B;
        union lpc_rtc_c C;
        union lpc_rtc_d D;
        uint8_t     ram[114];
    } __attribute__((packed)) u;
    uint8_t raw[128];
} __attribute__((packed));


// LPC main data structures

enum lpc_pit_next_byte {
    LPC_PIT_NONE = 0,
    LPC_PIT_LSB,
    LPC_PIT_MSB
};

enum lpc_pic_current_icw {
    LPC_PIC_ICW_NONE,
    LPC_PIC_ICW_2,
    LPC_PIC_ICW_3,
    LPC_PIC_ICW_4
};

enum lpc_pic_irq_state {
    LPC_PIC_IRQ_AVAIL = 0,
    LPC_PIC_IRQ_PENDING,
    LPC_PIC_IRQ_ISR
};


struct lpc {
    struct apic     *apic;  ///< The APIC to send IRQs to
    lpc_virtual_irq_handler     virq_handler;
    lpc_virtual_irq_pending     virq_pending;
    void *                      virq_user_data;
    // NMI Controller
    bool                nmi_masked;
    union lpc_nmi_sc    nmi_sc_reg; ///< NMI Status and Control Register

    // Timer
    union lpc_pit_sbyte         sbytes[3];
    enum lpc_pit_next_byte      counter_current_byte[3];
    uint16_t                    initial_count[3];
    bool                        counter_latched[3];
    uint16_t                    buffer_val[3];
    struct timer                *timer[3];

    // PIC
    // inside the array pos 0 represents the master and 1 the slave controller
    enum lpc_pic_current_icw    current_icw[2];
    union lpc_pic_icw1          icw1[2];
    union lpc_pic_icw2          icw2[2];
    union lpc_pic_icw3          icw3[2];
    union lpc_pic_icw4          icw4[2];
    union lpc_pic_ocw1          ocw1[2];
    union lpc_pic_ocw2          ocw2[2];
    union lpc_pic_ocw3          ocw3[2];
    enum lpc_pic_irq_state      irq_state[16];
    int                         current_irq;

    // RTC
    uint8_t                     rtc_prim_addr;
    union lpc_rtc_prim_ram      rtc_prim_ram;
    uint8_t                     rtc_sec_addr;
    uint8_t                     rtc_sec_ram[128];
    struct timer                *rtc_timer;
    uint64_t                    rtc_timer_elapsed;
};

int lpc_init(void);
struct lpc * lpc_new (lpc_virtual_irq_handler virq_handler,
                      lpc_virtual_irq_pending virq_pending, void *user_data,
                      struct apic *apic);
int lpc_handle_pio_read (struct lpc *l, uint16_t port, enum opsize size,
                         uint32_t *val);
int lpc_handle_pio_write (struct lpc *l, uint16_t port, enum opsize size,
                          uint32_t val);
void lpc_pic_assert_irq (struct lpc *l, uint8_t irq);
void lpc_pic_process_irqs (struct lpc *l);

void lpc_rtc_get_time_bcd (struct lpc *l, uint8_t *hour, uint8_t *min,
                           uint8_t *sec);

#endif // LPC_H
