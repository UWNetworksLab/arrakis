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

/* A lot of the information used to build these models is take from:
 * http://www.intel.com/Assets/PDF/datasheet/313082.pdf */

#include "vmkitmon.h"
#include "lpc.h"
#include <stdlib.h>


#define TCW_RW_LSB      1
#define TCW_RW_MSB      2
#define TCW_RW_LSB_MSB  3

#define LPC_PIT_MODE_ONE_SHOT       0
#define LPC_PIT_MODE_RATEN          2
#define LPC_PIT_MODE_SWAVE          3
#define LPC_PIT_TIMER0_PERIOD_NS    838

#define PIC_EOI_NSEOI              1
#define PIC_EOI_SEOI               3

#define PIT_IRQ     0
#define RTC_IRQ     8


static inline uint8_t
bcd2bin (uint8_t val)
{
    return (val & 0x0f) + (val >> 4) * 10;
}

static inline uint8_t
bin2bcd(unsigned val)
{
    assert(val < 100);
    return ((val / 10) << 4) + val % 10;
}

static inline bool
pic_irq_masked (struct lpc *l, int irq) {
    assert(irq < 16);

    // if the IRQ is comming from the slave controller check whether it is
    // masked on the first controller (IRQ2)
    if (irq > 7 && l->ocw1[0].u.irq_mask & (1 << 2)) {
        return true;
    }

    uint8_t rel_irq = irq & 0x7;
    int ctrlr = irq >> 3;
    return l->ocw1[ctrlr].u.irq_mask & (1 << rel_irq);
}

/**
 * \brief Scan all pending IRQs and send the most important one up to APIC.
 */
void
lpc_pic_process_irqs (struct lpc *l)
{
    bool pending = false;

    // check whether the CPU has an interrupt pending
    if (l->virq_pending(l->virq_user_data, NULL, NULL)) {
        assert(l->current_irq >= 0 && l->current_irq < 16);
        pending = true;
    }

    // find the most priorized IRQ
    for (int irq = 0; irq < 16; irq++) {
        // check whether this IRQ is pending
        if (l->irq_state[irq] == LPC_PIC_IRQ_PENDING) {
            assert(irq != 2);

            // check whether this irq has a higher (0 beeing high) priority as
            // a possible pending one
            if (pending && irq >= l->current_irq) {
                // we may return here since no further IRQ may have a higher prior
                return;
            }

            // check whether we have to remove a pending interrupt
            if (pending) {
                // there is an irq pending with lower priority than the current
                // one, we have to put it back into pending, it will be replaced
                // by this interrupt
                assert(l->irq_state[l->current_irq] == LPC_PIC_IRQ_ISR);
                l->irq_state[l->current_irq] = LPC_PIC_IRQ_PENDING;
            }

            // process the irq
            l->irq_state[irq] = LPC_PIC_IRQ_ISR;
            l->current_irq = irq;

            // assert the virtual interrupt to the CPU
            // find out the real irq as it is mapped through the PIC init
            int ctrlr = irq >> 3;
            assert(ctrlr == 0 || ctrlr == 1);
            assert(irq <= 7 || ctrlr == 1);
            uint8_t real_irq = (l->icw2[ctrlr].u.intr_vec_base_addr << 3) |
                               (irq & 0x7);
            /*if (irq != 0) {
                printf("PIC: assert IRQ%u (real %u), pending %u, current_irq %u\n", irq, real_irq, pending, l->current_irq);
            }*/
            l->virq_handler(l->virq_user_data, real_irq, irq);
            return;
        }
    }
}

/**
 * \brief Assert an IRQ through the PIC.
 *
 * This method asserts an IRQ to the CPU. It does not actually generate virtual
 * interrupts and instead pushed the IRQ forward to the APIC.
 */
void
lpc_pic_assert_irq (struct lpc *l, uint8_t irq)
{
    assert(irq < 16 && irq != 2);

    if (!pic_irq_masked(l, irq) && l->irq_state[irq] == LPC_PIC_IRQ_AVAIL) {
        // assert the irq
        l->irq_state[irq] = LPC_PIC_IRQ_PENDING;
    }

    lpc_pic_process_irqs(l);
}

static void
pic_eoi (struct lpc *l, int ctrlr)
{
    assert(ctrlr == 0 || ctrlr == 1);

    switch (l->ocw2[ctrlr].u.rot_eio_codes) {
    case PIC_EOI_NSEOI: {
        // start at IRQ0 for the master and IRQ8 for the slave controller
        int irq = ctrlr << 3;
        for (int i = 0; i < 8; i++, irq++) {
            if (l->irq_state[irq] == LPC_PIC_IRQ_ISR) {
                l->irq_state[irq] = LPC_PIC_IRQ_AVAIL;
                break;
            }
        }
        break;
    }
    case PIC_EOI_SEOI: {
        // irq_lvl_sel holds an number between 0 and 7 which represent the
        // corresponding IRQ on the master and slave controller
        int irq = l->ocw2[ctrlr].u.irq_lvl_sel | (ctrlr << 3);
        l->irq_state[irq] = LPC_PIC_IRQ_AVAIL;
        break;
    }
    default:
        // we do no support all EOI methods yet
        assert(!"not reached");
        break;
    }

    // after an EOI we should process the remaining IRQs
    lpc_pic_process_irqs(l);
}

static inline uint64_t
truncate_to_opsize (enum opsize size, uint64_t val)
{
    switch (size) {
        case OPSIZE_8:
            return val & 0xff;
        case OPSIZE_16:
            return val & 0xffff;
        case OPSIZE_32:
            return val & 0xffffffff;
        case OPSIZE_64:
            return val;
    }

    assert(!"not reached");
    return 0;
}

static inline void
cycle_counter_access_byte (struct lpc *l, int reg)
{
    // check whether we have to cycle the byte pointer
    if (l->sbytes[reg].u.rw_mode == TCW_RW_LSB_MSB) {
        switch (l->counter_current_byte[reg]) {
        case LPC_PIT_LSB:
            l->counter_current_byte[reg] = LPC_PIT_MSB;
            break;
        case LPC_PIT_MSB:
            l->counter_current_byte[reg] = LPC_PIT_LSB;
            break;
        case LPC_PIT_NONE:
            assert(!"not reached");
        }
    }
}

#if 0
static inline uint32_t
timer_countdown_reg_read (struct lpc *l, int reg)
{
    uint32_t ret;

    assert(l->counters_next_byte[reg] != LPC_PIT_NONE);

    // write the requested byte
    if (l->counters_next_byte[reg] != LPC_PIT_LSB) {
        ret = l->counters[reg] & 0xff;
    } else if (l->counters_next_byte[reg] != LPC_PIT_MSB) {
        ret = (l->counters[reg] >> 8) & 0xff;
    }

    cycle_counter_access_byte(l, reg);

    return ret;
}
#endif

static inline void
pit_write_current_byte (struct lpc *l, int reg, uint16_t src, uint8_t *dest)
{
    // write the requested byte
    if (l->counter_current_byte[reg] == LPC_PIT_LSB) {
        *dest = src;
    } else if (l->counter_current_byte[reg] == LPC_PIT_MSB) {
        *dest = src >> 8;
    } else {
        assert(!"not reached");
    }
}

static inline uint8_t
pit_counter_read (struct lpc *l, int reg)
{
    assert(reg == 0 || reg == 2);
    assert(l->counter_current_byte[reg] != LPC_PIT_NONE);

    uint16_t val;

    // read the current value
    if (reg == 2 && !l->nmi_sc_reg.u.tim_cnt2_en) {
        // if we are dealing with counter 2 and it is disabled we just return
        // the initial value
        val = l->initial_count[2];
    } else {
        // otherwise we read the value from the real counter

        // if the counter is latched then its value has been stored in the buffer
        // otherwhise we take it directly from the counter
        if (l->counter_latched[reg]) {
            // the value was read before
            val = l->buffer_val[reg];
        } else {
            val = (timer_remaining(l->timer[reg]) * 1000 /
                  LPC_PIT_TIMER0_PERIOD_NS) % l->initial_count[reg];
        }
    }

    uint8_t ret_val;
    if (l->counter_current_byte[reg] == LPC_PIT_LSB) {
        ret_val = val;
    } else {
        ret_val = val >> 8;
        // reset a possible latch command
        l->counter_latched[reg] = false;
    }
    cycle_counter_access_byte(l, reg);

    return ret_val;
}

/**
 * \brief Handles writes to the ICW or OCW registers of both PICs.
 *
 * \param ctrlr     0 inidicates the master and 1 the slave controller
 * \param port      the relative port (0 or 1) based at 0x20 or 0xa0 accessed
 */
static inline uint8_t
pic_icw_ocw_read (struct lpc *l, int ctrlr, uint16_t port)
{
    assert(ctrlr == 0 || ctrlr == 1);
    assert(port == 0 || port == 1);

    switch (port) {
    case 0:
        // ICW1, OCW2, OCW3 are write only
        assert(!"not reached");
        break;
    case 1:
        return l->ocw1[ctrlr].raw;
    }

    assert(!"not reached");
    return 0;
}

int
lpc_handle_pio_read (struct lpc *l, uint16_t port, enum opsize size,
                     uint32_t *val)
{
    switch (port) {
    // PIC
    case 0x20:
    case 0x21:
        *val = pic_icw_ocw_read(l, 0, port - 0x20);
        return HANDLER_ERR_OK;
    case 0xa0:
    case 0xa1:
        *val = pic_icw_ocw_read(l, 1, port - 0xa0);
        return HANDLER_ERR_OK;

    // RTC
    case 0x70:
    case 0x72:
    case 0x74:
    case 0x76:
        assert(!"these io-ports should not be read");
        break;
    // primary ram bank access
    case 0x71:
    case 0x75:
        if (l->rtc_prim_addr < 128) {
            *val = l->rtc_prim_ram.raw[l->rtc_prim_addr];
            // clear register C on read
            if (l->rtc_prim_addr == 0xc) {
                l->rtc_prim_ram.u.C.raw = 0;
            }
        }
        return HANDLER_ERR_OK;
    // second ram bank access
    case 0x73:
    case 0x77:
        if (l->rtc_sec_addr < 128) {
            *val = l->rtc_sec_ram[l->rtc_sec_addr];
        }
        return HANDLER_ERR_OK;;

    // NMI
    case 0x61:
        // pass the out pin state of PIT counter 2
        if (l->nmi_sc_reg.u.tim_cnt2_en) {
            l->nmi_sc_reg.u.tmr2_out_sts = l->sbytes[2].u.out_pin_state;
        } else {
            l->nmi_sc_reg.u.tmr2_out_sts = 0;
        }

        *val = l->nmi_sc_reg.raw;
        return HANDLER_ERR_OK;

    // Timer
    case 0x41:
        assert(!"not reached");
        break;
    case 0x40:
    case 0x42:
        *val = pit_counter_read(l, port - 0x40);
        return HANDLER_ERR_OK;
    case 0x43:
        *val = 0;
        return HANDLER_ERR_OK;

    default:
        printf("lpc: Unhandled read access to port %x\n", port);
        return HANDLER_ERR_UNHANDELED;
    }

    return -1;
}

static void
rtc_timer_callback (struct timer *t, void *data)
{
    struct lpc *l = data;

    // some restrictions
    // all uniplemented features
    assert(l->rtc_prim_ram.u.B.u.data_mode == 0);
    assert(l->rtc_prim_ram.u.B.u.hour_format == 1);
    assert(l->rtc_prim_ram.u.B.u.aie == 0);
    assert(l->rtc_prim_ram.u.B.u.pie == 0);
    assert(l->rtc_prim_ram.u.B.u.set == 0);
    assert(l->rtc_prim_ram.u.B.u.sqwe == 0);

    uint8_t sec = bcd2bin(l->rtc_prim_ram.u.seconds);
    uint8_t min = bcd2bin(l->rtc_prim_ram.u.minutes);
    uint8_t hour = bcd2bin(l->rtc_prim_ram.u.hours);
    uint8_t dow = bcd2bin(l->rtc_prim_ram.u.day_of_week);
    uint8_t dom = bcd2bin(l->rtc_prim_ram.u.day_of_month);
    uint8_t mon = bcd2bin(l->rtc_prim_ram.u.month);
    uint8_t year = bcd2bin(l->rtc_prim_ram.u.year);

    // increment RTC by one second
    sec++;
    if (sec >= 60) {
        sec = 0;
        min++;
    }
    if (min >= 60) {
        min = 0;
        hour++;
    }
    if (hour >= 24) {
        hour = 0;
        dow++;
        dom++;
    }
    if (dow > 7) {
        dow = 0;
    }
    // FIXME: simply wrong!
    if (dom > 30) {
        dom = 0;
        mon++;
    }
    if (mon >= 12) {
        mon = 0;
        year++;
    }

    l->rtc_prim_ram.u.seconds = bin2bcd(sec);
    l->rtc_prim_ram.u.minutes = bin2bcd(min);
    l->rtc_prim_ram.u.hours = bin2bcd(hour);
    l->rtc_prim_ram.u.day_of_week = bin2bcd(dow);
    l->rtc_prim_ram.u.day_of_month = bin2bcd(dom);
    l->rtc_prim_ram.u.month = bin2bcd(mon);
    l->rtc_prim_ram.u.year = bin2bcd(year);

    // trigger update interrupt if desired
    if (l->rtc_prim_ram.u.B.u.uie) {
        l->rtc_prim_ram.u.C.u.uf = 1;
        lpc_pic_assert_irq(l, RTC_IRQ);
    }

}

static void
handle_counter_timer (struct timer *t, void *user_data)
{
    struct lpc *l = user_data;

    int reg;
    if (t == l->timer[0]) {
        reg = 0;
        // assert the IRQ
        lpc_pic_assert_irq(l, PIT_IRQ);
    } else if (t == l->timer[2]) {
        reg = 2;
    } else {
        assert(!"timer callback for unknown counter!");
        return;
    }

    switch (l->sbytes[reg].u.mode_type) {
    case LPC_PIT_MODE_ONE_SHOT:
        // set the out pin to high
        l->sbytes[reg].u.out_pin_state = 1;
        // destroy the timer
        timer_destroy(t);
        l->timer[reg] = NULL;
        break;

    case LPC_PIT_MODE_SWAVE:
        // cycle between zero and one every time the counter rolls
        // over its initial value
        l->sbytes[reg].u.out_pin_state = !l->sbytes[reg].u.out_pin_state;
        break;

    case LPC_PIT_MODE_RATEN:
        // we do not set the out pin to one for one cycle as it would be on
        // real hardware
        // the running timer is peridic therefore we do not need to do anything
        // here
        break;
    }
}

static inline void
timer_countdown_reg_write (struct lpc *l, int reg, uint8_t val)
{
    uint16_t buf = l->initial_count[reg];

    assert(l->counter_current_byte[reg] != LPC_PIT_NONE);

    // write the requested byte
    if (l->counter_current_byte[reg] == LPC_PIT_LSB) {
        l->initial_count[reg] = (buf & 0xff00) | val;
    } else if (l->counter_current_byte[reg] == LPC_PIT_MSB) {
        l->initial_count[reg] = (val << 8) | (buf & 0xff);
    }

    // actions to be done at the end of a counter config cycle
    // in LSB MSB mode this is the case after MSB has been written in all other
    // modes this true after all writes
    // TCW_RW_LSB_MSB ==> LPC_PIT_MSB
    if (l->sbytes[reg].u.rw_mode != TCW_RW_LSB_MSB ||
        l->counter_current_byte[reg] == LPC_PIT_MSB) {
        // counter 0 asserts an interrupt so start a timer on this event
        if (reg == 0 || reg == 2) {
            // destroy a possible running timer before creating a new one
            if (l->timer[reg] != NULL) {
                timer_destroy(l->timer[reg]);
            }

            // set up the new timer
            bool periodic = l->sbytes[reg].u.mode_type != LPC_PIT_MODE_ONE_SHOT;

            // start the PIT timer
            l->timer[reg] = timer_create((uint64_t)l->initial_count[reg] *
                                         LPC_PIT_TIMER0_PERIOD_NS / 1000,
                                         periodic, handle_counter_timer, l);
            // only start timer 2 if it is enabled
            if (!(reg == 2 && !l->nmi_sc_reg.u.tim_cnt2_en)) {
                timer_start(l->timer[reg]);
            }
        }
        // other counters do not assert an IRQ so just pass on the config
        else {
            //lpc_timer_config(reg, l->sbytes[reg].u.mode_type,
            //                 l->initial_count[reg]);
        }
    }

    cycle_counter_access_byte(l, reg);
}

/**
 * \brief Handles writes to the ICW or OCW registers of both PICs.
 *
 * \param ctrlr     0 inidicates the master and 1 the slave controller
 * \param port      the relative port (0 or 1) based at 0x20 or 0xa0 accessed
 */
static inline void
pic_icw_ocw_write (struct lpc *l, int ctrlr, uint16_t port, uint8_t val)
{
    assert(ctrlr == 0 || ctrlr == 1);
    assert(port == 0 || port == 1);

    switch (port) {
    case 0:
        // check whether the IWC cycle should start
        if (val & 0x10) {
            // ICW cycle
            l->icw1[ctrlr].raw = val;
            assert(l->icw1[ctrlr].u.sigl_or_casc == 0);
            assert(l->icw1[ctrlr].u.icw_sel != 0);
            // start the cycle
            l->current_icw[ctrlr] = LPC_PIC_ICW_2;
        } else {
            // OCW write
            // bit 3 indicates which control word shall be written
            if (val & 0x8) {
                // OCW 3
                l->ocw3[ctrlr].raw = val;
                assert(l->ocw3[ctrlr].u.ocw_sel == 1);
            } else {
                // OCW 2
                l->ocw2[ctrlr].raw = val;
                assert(l->ocw2[ctrlr].u.ocw_sel == 0);
                pic_eoi(l, ctrlr);
            }
        }
        break;
    case 1:
        // check whether we are in IWC mode
        if (l->current_icw[ctrlr] != LPC_PIC_ICW_NONE) {
            switch (l->current_icw[ctrlr]) {
            case LPC_PIC_ICW_2:
                l->icw2[ctrlr].raw = val;
                assert(l->icw2[ctrlr].u.intr_req_lvl == 0);
                l->current_icw[ctrlr] = LPC_PIC_ICW_3;
                break;
            case LPC_PIC_ICW_3:
                l->icw3[ctrlr].raw = val;
                if (ctrlr == 0) {
                    assert(l->icw3[ctrlr].um.casc_slave_ctrlr == 1);
                }
                if (l->icw1[ctrlr].u.icw4_req) {
                    l->current_icw[ctrlr] = LPC_PIC_ICW_4;
                } else {
                    l->current_icw[ctrlr] = LPC_PIC_ICW_NONE;
                }
                break;
            case LPC_PIC_ICW_4:
                l->icw4[ctrlr].raw = val;
                assert(l->icw4[ctrlr].u.microproc_mode != 0);
                assert(!l->icw4[ctrlr].u.buf_mode);
                assert(!l->icw4[ctrlr].u.sfn_mod);
                // AEOI not supported yet
                assert(!l->icw4[ctrlr].u.aeoi);
                l->current_icw[ctrlr] = LPC_PIC_ICW_NONE;
                break;
            default:
                assert(!"not reached");
                break;
            }
        }
        // OCW1 write
        else {
            l->ocw1[ctrlr].raw = val;
        }
        break;
    default:
        assert(!"not reached");
    }
}

int
lpc_handle_pio_write (struct lpc *l, uint16_t port, enum opsize size,
                      uint32_t val)
{
    switch (port) {
    // PIC
    case 0x20:
    case 0x21:
        pic_icw_ocw_write(l, 0, port - 0x20, val);
        return HANDLER_ERR_OK;
    case 0xa0:
    case 0xa1:
        pic_icw_ocw_write(l, 1, port - 0xa0, val);
        return HANDLER_ERR_OK;

    //  RTC
    case 0x70:
    case 0x74:
        if ((val >> 7) & 1) {
            l->nmi_masked = true;
        } else {
            l->nmi_masked = false;
        }
        l->rtc_prim_addr = val & 0x7f;
        return HANDLER_ERR_OK;
    case 0x71:
    case 0x75:
        if (l->rtc_prim_addr < 128) {
            l->rtc_prim_ram.raw[l->rtc_prim_addr] = val;
        }
        return HANDLER_ERR_OK;
    case 0x72:
    case 0x76:
        l->rtc_sec_addr = val;
        return HANDLER_ERR_OK;
    case 0x73:
    case 0x77:
        if (l->rtc_sec_addr < 128) {
            l->rtc_sec_ram[l->rtc_sec_addr] = val;
        }
        return HANDLER_ERR_OK;

    // NMI Controller
    case 0x61:
        // only the first 4 bits are writable
        l->nmi_sc_reg.raw |= val & 0xf;
        // start/stop the counter 2 if required
        if (l->timer[2]) {
            if (l->nmi_sc_reg.u.tim_cnt2_en && !timer_is_running(l->timer[2])) {
                timer_start(l->timer[2]);
            } else if (!l->nmi_sc_reg.u.tim_cnt2_en
                       && timer_is_running(l->timer[2])) {
                timer_stop(l->timer[2]);
            }
        }
        return HANDLER_ERR_OK;

    // Timer
    // Write to the countdown registers
    case 0x40:
        timer_countdown_reg_write(l, 0, val);
        return HANDLER_ERR_OK;
    case 0x41:
        timer_countdown_reg_write(l, 1, val);
        return HANDLER_ERR_OK;
    case 0x42:
        timer_countdown_reg_write(l, 2, val);
        return HANDLER_ERR_OK;
    // Write to the timer control register (TCW)
    case 0x43: {
        union lpc_pit_tcw tcw = { .raw = val };

        // check for some unimplemented stuff
        if (tcw.u.countdown_select != 0) {
            printf("lpc timer: only binary countdown is supported\n");
            return HANDLER_ERR_FATAL;
        }
        // not all modes are implemented
        assert(tcw.u.mode_select == LPC_PIT_MODE_ONE_SHOT ||
               tcw.u.mode_select == LPC_PIT_MODE_RATEN);
        if (tcw.u.counter_select == 3) {
            printf("lpc timer: read back command not implemented\n");
            return HANDLER_ERR_FATAL;
        }

        // we do not support counter 1
        assert(tcw.u.counter_select != 1);

        // check for latch command
        if (tcw.u.rw_select == 0) {
            l->counter_latched[tcw.u.counter_select] = true;
            l->buffer_val[tcw.u.counter_select] =
                (timer_remaining(l->timer[tcw.u.counter_select]) *
                1000 / LPC_PIT_TIMER0_PERIOD_NS) %
                l->initial_count[tcw.u.counter_select];
            return HANDLER_ERR_OK;
        }

        l->sbytes[tcw.u.counter_select].raw = (uint8_t)val;
        l->sbytes[tcw.u.counter_select].u.count_avail = 0;
        l->sbytes[tcw.u.counter_select].u.out_pin_state = 0;

        // set the access mode
        switch (tcw.u.rw_select) {
        case TCW_RW_LSB_MSB:
            l->counter_current_byte[tcw.u.counter_select] = LPC_PIT_LSB;
            break;
        // we only support LSB MSB mode atm
        default:
            assert(!"not reached");
        }

        return HANDLER_ERR_OK;
    }

    default:
        printf("lpc: Unhandled write access to port %x\n", port);
        return HANDLER_ERR_UNHANDELED;
    }

    return -1;
}

struct lpc *
lpc_new (lpc_virtual_irq_handler virq_handler,
         lpc_virtual_irq_pending virq_pending, void *user_data,
         struct apic *apic)
{
    struct lpc *ret = calloc(1, sizeof(struct lpc));

    ret->virq_handler = virq_handler;
    ret->virq_pending = virq_pending;
    ret->virq_user_data = user_data;
    ret->apic = apic;

    ret->current_irq = -1;

    // RTC init
    ret->rtc_prim_ram.u.B.u.hour_format = 1;
    // init some time
    ret->rtc_prim_ram.u.day_of_month = bin2bcd(29);
    ret->rtc_prim_ram.u.month = bin2bcd(8);
    ret->rtc_prim_ram.u.year = bin2bcd(9);
    // start the RTC timer to to be called every second
    ret->rtc_timer = timer_create(1000000, true, rtc_timer_callback, ret);
    timer_start(ret->rtc_timer);

    return ret;
}

void
lpc_rtc_get_time_bcd (struct lpc *l, uint8_t *hour, uint8_t *min, uint8_t *sec)
{
    if (hour != NULL) {
        *hour = l->rtc_prim_ram.u.hours;
    }
    if (min != NULL) {
        *min = l->rtc_prim_ram.u.minutes;
    }
    if (sec != NULL) {
        *sec = l->rtc_prim_ram.u.seconds;
    }
}
