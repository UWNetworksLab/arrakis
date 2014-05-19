/*
 * Copyright (c) 2007, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/pl011_uart_dev.h>
#include <dev/arm_icp_pic0_dev.h>
#include <dev/arm_icp_pit_dev.h>

#include <pl011_uart.h>
#include <serial.h>
#include <arm_hal.h>

uint32_t hal_get_board_id(void)
{
    return 0x113;
}

uint8_t hal_get_cpu_id(void)
{
    return 0;
}

bool hal_cpu_is_bsp(void)
{
    return true;
}

//
// Interrupt controller
//

static arm_icp_pic0_source_un pic_primary_irqs;
static arm_icp_pic0_t pic;

void pic_init(void)
{
    static const uintptr_t PIC_BASE = 0x14000000;

    arm_icp_pic0_source_t sources = {
        .SOFTINT  = 1, .UARTINT0  = 1, .UARTINT1  = 1, .KBDINT    = 1,
        .MOUSEINT = 1, .TIMERINT0 = 1, .TIMERINT1 = 1, .TIMERINT2 = 1,
        .RTCINT   = 1, .LM_LLINT0 = 1, .LM_LLINT1 = 1, .CLCDCINT  = 1,
        .MMCIINT0 = 1, .MMCIINT1  = 1, .AACIINT   = 1, .CPPLDINT  = 1,
        .ETH_INT  = 1, .TS_PENINT = 1
    };
    pic_primary_irqs.val = sources;

    lvaddr_t pic_base = paging_map_device(PIC_BASE, 0x00100000);
    arm_icp_pic0_initialize(&pic, (mackerel_addr_t)pic_base);

    pic_disable_all_irqs();
}

void pic_set_irq_enabled(uint32_t irq, bool en)
{
    uint32_t m = 1u << irq;
    if (irq < 32 && (pic_primary_irqs.raw & m) == m) {
        if (en) {
            m |= arm_icp_pic0_PIC_IRQ_ENABLESET_rd_raw(&pic);
            arm_icp_pic0_PIC_IRQ_ENABLESET_wr_raw(&pic, m);
        }
        else {
            arm_icp_pic0_PIC_IRQ_ENABLECLR_wr_raw(&pic, m);
        }
    }
    else {
        panic("Unknown IRQ source %"PRIu32, irq);
    }
}

void pic_disable_all_irqs(void)
{
    arm_icp_pic0_PIC_IRQ_ENABLECLR_wr_raw(&pic, pic_primary_irqs.raw);
}

uint32_t pic_get_active_irq(void)
{
    uint32_t status = arm_icp_pic0_PIC_IRQ_STATUS_rd_raw(&pic);
    uint32_t irq;

    for (irq = 0; irq < 32; irq++) {
        if (0 != (status & (1u << irq))) {
            return irq;
        }
    }
    return ~0ul;
}

void pic_ack_irq(uint32_t irq)
{
    // From the ARM specs it looks as if just clearing the interrupt at the
    // peripheral will clear the interrupt. No explicit EOI.
}

//
// Kernel timer and tsc
//

static const uintptr_t PIT_BASE = 0x13000000;
static const uint32_t  PIT_IRQ  = 6;

static arm_icp_pit_t pit;

static lvaddr_t pit_map_resources(void)
{
    static lvaddr_t timer_base = 0;
    if (timer_base == 0) {
        timer_base = paging_map_device(PIT_BASE, 0x100000);
    }
    return timer_base;
}

void pit_init(uint32_t tick_hz)
{
    // PIT uses timer 1 (hardcoded to 1MHz)
    arm_icp_pit_LOAD_t load = { .value = 1000000 / tick_hz };
    arm_icp_pit_CONTROL_t control = {
        .oneshot = 0, .timer32 = 1, .prescale = arm_icp_pit_none,
        .int_enable = 0, .mode = arm_icp_pit_reload, .enable = 0
    };

    lvaddr_t timer_base = pit_map_resources();

    arm_icp_pit_initialize(&pit, (mackerel_addr_t)(timer_base + 0x100));
    arm_icp_pit_LOAD_wr(&pit, load);
    arm_icp_pit_CONTROL_wr(&pit, control);

    pic_set_irq_enabled(PIT_IRQ, 1);
}

void pit_start(void)
{
    arm_icp_pit_CONTROL_t control = arm_icp_pit_CONTROL_rd(&pit);
    control.int_enable = 1;
    control.enable = 1;
    arm_icp_pit_CONTROL_wr(&pit, control);
}

bool pit_handle_irq(uint32_t irq)
{
    if (PIT_IRQ == irq) {
        arm_icp_pit_INTCLR_wr_raw(&pit, ~0ul);
        return 1;
    }
    else {
        return 0;
    }
}

void pit_mask_irq(bool masked)
{
    arm_icp_pit_CONTROL_t control = arm_icp_pit_CONTROL_rd(&pit);
    if (masked) {
        control.int_enable = 0;
    }
    else {
        control.int_enable = 1;
    }
    arm_icp_pit_CONTROL_wr(&pit, control);

    if (masked) {
        // Clear interrupt if pending.
        pit_handle_irq(PIT_IRQ);
    }
}

//
// TSC uses timer 0 (assuming 40MHz for QEMU)
//
static const uint32_t tsc_hz = 40000000;
static arm_icp_pit_t tsc;

void tsc_init(void)
{
    arm_icp_pit_LOAD_t load = { .value = ~0ul };
    arm_icp_pit_CONTROL_t control = {
        .oneshot = 0, .timer32 = 1, .prescale = arm_icp_pit_none,
        .int_enable = 0, .mode = arm_icp_pit_reload, .enable = 1
    };
    pit_map_resources();

    arm_icp_pit_initialize(&tsc, (mackerel_addr_t)pit_map_resources());
    arm_icp_pit_LOAD_wr(&tsc, load);
    arm_icp_pit_CONTROL_wr(&tsc, control);
}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return ~arm_icp_pit_CURRENT_rd_raw(&tsc);
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;
}

//
// Serial console and debugger interfaces
//

#define NUM_PORTS 2
unsigned serial_console_port = 0;
unsigned serial_debug_port = 1;
const unsigned serial_num_physical_ports = NUM_PORTS;


#define UART0_VBASE		0xE0009000
#define UART0_SECTION_OFFSET	0x9000
#define UART_DEVICE_BYTES	0x4c
#define UART_MAPPING_DIFF	0x1000

static pl011_uart_t ports[NUM_PORTS];

errval_t serial_init(unsigned port)
{
    if (port < NUM_PORTS) {
        assert(ports[port].base == 0);

        lvaddr_t base = paging_map_device(0x16000000ul + port * 0x01000000,
                                          0x00100000);
        pl011_uart_init(&ports[port], base);
        return SYS_ERR_OK;
    }
    else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}
errval_t serial_early_init(unsigned port)
{
    return SYS_ERR_OK; // Unused
}

void serial_putchar(unsigned port, char c) 
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    pl011_putchar(&ports[port], c);
};

char serial_getchar(unsigned port)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    return pl011_getchar(&ports[port]);
};
