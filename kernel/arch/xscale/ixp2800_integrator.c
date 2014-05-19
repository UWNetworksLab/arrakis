/*
 * Copyright (c) 2007, 2009, 2011, 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/ixp2800_uart_dev.h>
#include <dev/ixp2800_icp_pic0_dev.h>
#include <dev/ixp2800_icp_pit_dev.h>

#include <ixp2800_uart.h>
#include <serial.h>
#include <arm_hal.h>

/** \brief Return hard coded board id  */
uint32_t hal_get_board_id(void)
{
    return 0x318;
}

/** \brief Return hard coded cpu id  */
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

static ixp2800_icp_pic0_source_t pic_primary_irqs;
static ixp2800_icp_pic0_t pic;


/** \brief Initialize interrupt controller. Disable all irqs.  */
void pic_init(void)
{
    //Base address for programmable interrupt controller
    static const uintptr_t PIC_BASE = 0xd6000000;

    //Enable irq for timer, uart and error sum (or of all interrupts)
    pic_primary_irqs = ixp2800_icp_pic0_source_default;
    pic_primary_irqs = ixp2800_icp_pic0_source_TIMER_UFLW_insert(pic_primary_irqs, 1);
    pic_primary_irqs = ixp2800_icp_pic0_source_UART_INT_insert(pic_primary_irqs, 1);
    pic_primary_irqs = ixp2800_icp_pic0_source_ERROR_SUM_insert(pic_primary_irqs, 1);

    //Map to virtual memory and initialize
    lvaddr_t pic_base = paging_map_device(PIC_BASE, 0x00100000);
    ixp2800_icp_pic0_initialize(&pic, (mackerel_addr_t)pic_base);
    
    pic_disable_all_irqs();
}

/** \brief Enable or disable certain irq. If irq not found on system --> panic
 */
void pic_set_irq_enabled(uint32_t irq, bool en)
{
    uint32_t m = 1u << irq; //Contains 1 at position of irq
    if (irq < 32 && (pic_primary_irqs & m) == m) {
        //Only matches if the irq has been configured in pic_init()
        //Hacky implementation: pic_primary_irqs is not of type uint, but will be used as uint
        if(en){
            m |= ixp2800_icp_pic0_IRQ_ENABLE_rd(&pic); //Keep already enabled irqs enabled
            ixp2800_icp_pic0_IRQ_ENABLE_SET_wr(&pic, m);
        }else{
            ixp2800_icp_pic0_IRQ_ENABLE_CLR_wr(&pic, m);
        }
    } else {
        panic("Unknown IRQ source %"PRIu32, irq);
    }
}

/** \brief Disable all irqs  */
void pic_disable_all_irqs(void)
{
    ixp2800_icp_pic0_IRQ_ENABLE_CLR_wr(&pic, 0xffffffff);
    //    ixp2800_icp_pic0_IRQ_ENABLE_CLR_wr(&pic, pic_primary_irqs);
}

/** \brief Return first active irq  */
uint32_t pic_get_active_irq(void)
{
    uint32_t status = ixp2800_icp_pic0_IRQ_STATUS_rd(&pic);

    uint32_t irq;
    for (irq = 0; irq < 32; irq++) {
        if (0 != (status & (1u << irq))) {
            return irq; //Return first active irq
        }
    }
    return ~0ul; //No irq active
}

/** \brief NOP. On ARM platform, interrupt request cleared when handling interrupt on peripheral */
void pic_ack_irq(uint32_t irq)
{
    // From the ARM specs it looks as if just clearing the interrupt at the
    // peripheral will clear the interrupt. No explicit EOI.
}



//
// Kernel timer and tsc
//

static const uintptr_t PIT_BASE = 0xc0020000;
static const uint32_t  PIT_IRQ  = 4;

static ixp2800_icp_pit_t pit;

/** \brief Map timer device ONCE to virtual memory  */
static lvaddr_t pit_map_resources(void)
{
    static lvaddr_t timer_base = 0; //Initialized once by the compiler
    if (timer_base == 0) {
        timer_base = paging_map_device(PIT_BASE, 0x100000) + (PIT_BASE & 0x000ffffful); //Base mapping + offset
    }

    return timer_base;
}

/** \brief Initialize timer. Get timer interrupt every 1/<function_argument>  */
void pit_init(uint32_t tick_hz)
{
    // XXX: Disable for the moment
    return;

    //P.144 Intel Hardware reference manual
    //Timer has 50 Hz

    //Setup initial timer value
    ixp2800_icp_pit_LOADING_t load = ixp2800_icp_pit_LOADING_default;
    load = ixp2800_icp_pit_LOADING_CLV_insert(load, 50000000 / tick_hz); //(tick_hz == 1) ==> 1 interrupt / s

    //Disable timer and set divisor
    ixp2800_icp_pit_CONTROL_t ctrl = ixp2800_icp_pit_CONTROL_default;
    ctrl = ixp2800_icp_pit_CONTROL_ACT_insert(ctrl, 0);
    ctrl = ixp2800_icp_pit_CONTROL_PSS_insert(ctrl, ixp2800_icp_pit_div16);

    //Map virtual address for this device
    lvaddr_t timer_base = pit_map_resources();

    //Initialize device and write parameters to device
    ixp2800_icp_pit_initialize(&pit, (mackerel_addr_t) timer_base);
    ixp2800_icp_pit_LOADING_wr(&pit, load);
    ixp2800_icp_pit_CONTROL_wr(&pit, ctrl);
    
    //Enable interrupts
    pic_set_irq_enabled(PIT_IRQ, 1);
}

/** \brief Start timer */
void pit_start(void)
{
    ixp2800_icp_pit_CONTROL_ACT_wrf(&pit, 1);
}

/** \brief Clear interrupt and return true if irq originated from pit, else return false */
bool pit_handle_irq(uint32_t irq)
{
    if (PIT_IRQ == irq) {
        ixp2800_icp_pit_CLEAR_ICL_wrf(&pit, 1); //IRQ request comes from pit
        return true;
    }
    else {
        return false;
    }
}

/*
 * \brief Mask and handle timer interrupt.
 * There seems no possibility to disable interrupts directly on the timer device, therefore deactivate timer
 * at pic. Clear interrupt if pending.
 */
void pit_mask_irq(bool masked)
{
    if (masked) {
        pic_set_irq_enabled(PIT_IRQ, 0);
        pit_handle_irq(PIT_IRQ);
    }
    else {
        pic_set_irq_enabled(PIT_IRQ, 1); //Enable timer interrupts
    }
}

//
// TSC uses timer 0 (assuming 40MHz for QEMU)
//
static const uint32_t tsc_hz = 40000000;


/** \brief Initialize time stamp counter */
void tsc_init(void)
{
    //Currently not implemented
}

uint32_t tsc_read(void)
{
    //Currently not implemented
    return 0;
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;     //Currently not implemented
}



//
// Serial console and debugger interfaces
//

#define NUM_PORTS 2
unsigned serial_console_port = 0;
unsigned serial_debug_port = 1;
const unsigned serial_num_physical_ports = NUM_PORTS;

static ixp2800_uart_t ports[2];

errval_t serial_init(unsigned port)
{
    if (port < NUM_PORTS) {
        assert(ports[port].base == 0);
        lvaddr_t base = paging_map_device(0xc0030000ul + port * 0x01000000, 0x00100000);
        ixp2800_uart_init(&ports[port], base + 0x30000);
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}
errval_t serial_early_init(unsigned port)
{
    return SYS_ERR_OK; // Unused currently
}

void serial_putchar(unsigned port, char c) 
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    ixp2800_putchar(&ports[port], c);
};

char serial_getchar(unsigned port)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    return ixp2800_getchar(&ports[port]);
};
