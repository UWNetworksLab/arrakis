/*
 * Copyright (c) 2007, 2009, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/pl011_uart_dev.h>
#include <dev/pl130_gic_dev.h>
#include <dev/sp804_pit_dev.h>
#include <dev/cortex_a9_pit_dev.h>
#include <dev/arm_icp_pit_dev.h>
#include <dev/a9scu_dev.h>

#include <pl011_uart.h>
#include <serial.h>
#include <arm_hal.h>
#include <cp15.h>
#include <io.h>
#include <gic.h>

//hardcoded bc gem5 doesn't set board id in ID_Register
#define VEXPRESS_ELT_BOARD_ID		0x8e0
uint32_t hal_get_board_id(void)
{
    return VEXPRESS_ELT_BOARD_ID;
}

uint8_t hal_get_cpu_id(void)
{
    return cp15_get_cpu_id();
}

//Gem5 ensures that cpu0 is always BSP, this probably also holds in general
bool hal_cpu_is_bsp(void)
{
    return cp15_get_cpu_id() == 0;
}

// clock rate hardcoded to 1GHz
static uint32_t tsc_hz = 1000000000;

//
// Interrupt controller
//

// RealView.py l342
#define GIC_BASE    0x2C000000 // l342
#define DIST_OFFSET 0x1000 // l342
#define CPU_OFFSET  0x2000 // l342

void gic_map_and_init(pl130_gic_t *gic)
{
    lvaddr_t gic_base = paging_map_device(GIC_BASE, ARM_L1_SECTION_BYTES);
    pl130_gic_initialize(gic, (mackerel_addr_t)gic_base + DIST_OFFSET,
            (mackerel_addr_t)gic_base + CPU_OFFSET);
}

//
// Kernel timer and tsc
//


#define PIT_BASE    0x1C100000 // RealView.py l344
#define PIT0_OFFSET 0x10000 // RealView.py l344f

// difference between two PITs
#define PIT_DIFF    0x10000 // from gem5/src/dev/arm/RealView.py l344 f

#define PIT0_IRQ    34 // from gem5/src/dev/arm/RealView.py l344
#define PIT1_IRQ    35 // from gem5/src/dev/arm/RealView.py l345

#define PIT0_ID     0
#define PIT1_ID     1

static sp804_pit_t pit0;
static sp804_pit_t pit1;

static lvaddr_t pit_map_resources(void)
{
    static lvaddr_t timer_base = 0;
    if (timer_base == 0) {
        timer_base = paging_map_device(PIT_BASE, ARM_L1_SECTION_BYTES);
    }
    return timer_base;
}

static void pit_config(uint32_t timeslice, uint8_t pit_id)
{
	sp804_pit_t *pit;
	if(pit_id == PIT0_ID)
		pit = &pit0;
	else if(pit_id == PIT1_ID)
		pit = &pit1;
	else
		panic("Unsupported PIT ID: %"PRIu32, pit_id);

	// PIT timer
	uint32_t load1 = timeslice * tsc_hz / 1000;
	uint32_t load2 = timeslice * tsc_hz / 1000;

	sp804_pit_Timer1Load_wr(pit, load1);
	sp804_pit_Timer2Load_wr(pit, load2);

	//configure timer 1
	sp804_pit_Timer1Control_one_shot_wrf(pit, 0);
	sp804_pit_Timer1Control_timer_size_wrf(pit, sp804_pit_size_32bit);
	sp804_pit_Timer1Control_timer_pre_wrf(pit, sp804_pit_prescale0);
	sp804_pit_Timer1Control_int_enable_wrf(pit, 0);
	sp804_pit_Timer1Control_timer_mode_wrf(pit, sp804_pit_periodic);
	sp804_pit_Timer1Control_timer_en_wrf(pit, 0);

	//configure timer 2
	sp804_pit_Timer2Control_one_shot_wrf(pit, 0);
	sp804_pit_Timer2Control_timer_size_wrf(pit, sp804_pit_size_32bit);
	sp804_pit_Timer2Control_timer_pre_wrf(pit, sp804_pit_prescale0);
	sp804_pit_Timer2Control_int_enable_wrf(pit, 0);
	sp804_pit_Timer2Control_timer_mode_wrf(pit, sp804_pit_periodic);
	sp804_pit_Timer2Control_timer_en_wrf(pit, 0);

	// enable PIT interrupt
	uint32_t int_id = pit_id ? PIT1_IRQ : PIT0_IRQ;
	gic_enable_interrupt(int_id, 0x1, 0xf, 0x1, 0);

}

void pit_init(uint32_t timeslice, uint8_t pit_id)
{
    sp804_pit_t *pit;
	if(pit_id == PIT0_ID)
		pit = &pit0;
	else if(pit_id == PIT1_ID)
		pit = &pit1;
	else
		panic("Unsupported PIT ID: %"PRIu32, pit_id);

    lvaddr_t timer_base = pit_map_resources();

	sp804_pit_initialize(pit, (mackerel_addr_t)(timer_base + PIT0_OFFSET + pit_id*PIT_DIFF));

	// if we are BSP we also config the values of the PIT
	if(hal_cpu_is_bsp())
	{
		pit_config(timeslice, pit_id);
	}
    //gic_set_irq_enabled(PIT_IRQ, 1);
}

void pit_start(uint8_t pit_id)
{
	 sp804_pit_t *pit;
	 if(pit_id == PIT0_ID)
		 pit = &pit0;
	 else if(pit_id == PIT1_ID)
		 pit = &pit1;
	 else
		 panic("Unsupported PIT ID: %"PRIu32, pit_id);


	sp804_pit_Timer1Control_int_enable_wrf(pit, 1);
	sp804_pit_Timer1Control_timer_en_wrf(pit, 1);
}

bool pit_handle_irq(uint32_t irq)
{
	if (PIT0_IRQ == irq)
	{
        sp804_pit_Timer1IntClr_wr(&pit0, ~0ul);
        gic_ack_irq(irq);
        return 1;
    }
	else if(PIT1_IRQ == irq)
	{
		sp804_pit_Timer1IntClr_wr(&pit1, ~0ul);
		gic_ack_irq(irq);
		return 1;
	}
    else {
        return 0;
    }
}

void pit_mask_irq(bool masked, uint8_t pit_id)
{
	 sp804_pit_t *pit;
	 if(pit_id == PIT0_ID)
		 pit = &pit0;
	 else if(pit_id == PIT1_ID)
		 pit = &pit1;
	 else
		 panic("Unsupported PIT ID: %"PRIu32, pit_id);

    if (masked) {
        sp804_pit_Timer1Control_int_enable_wrf(pit, 0);
    }
    else {
        sp804_pit_Timer1Control_int_enable_wrf(pit, 1);
    }

    if (masked) {
        // Clear interrupt if pending.
        pit_handle_irq(pit_id ? PIT1_IRQ : PIT0_IRQ);
    }
}

//
// TSC uses cpu private timer
//

#define TSC_BASE 	0x2C000000
#define TSC_OFFSET	0x80000

static cortex_a9_pit_t tsc;

void tsc_init(void)
{
    lvaddr_t timer_base = paging_map_device(TSC_BASE, ARM_L1_SECTION_BYTES);

    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)timer_base+TSC_OFFSET);

    // write load
    uint32_t load = ~0ul;
    cortex_a9_pit_TimerLoad_wr(&tsc, load);

    //configure tsc
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);

}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return ~cortex_a9_pit_TimerCounter_rd(&tsc);
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;
}


//
// Snoop Control Unit
//

#define SCU_BASE	0xE0200000

static a9scu_t scu;

void scu_enable(void)
{
    lvaddr_t scu_base = paging_map_device(TSC_BASE, ARM_L1_SECTION_BYTES);

    a9scu_initialize(&scu, (mackerel_addr_t)scu_base);

    //enable SCU
    a9scu_SCUControl_t ctrl_reg = a9scu_SCUControl_rd(&scu);
    ctrl_reg |= 0x1;
    a9scu_SCUControl_wr(&scu, ctrl_reg);
    //(should invalidate d-cache here)
}

int scu_get_core_count(void)
{
	return a9scu_SCUConfig_cpu_number_rdf(&scu);
}

//
// Sys Flag Register
//

// RealView.py: realview_io.pio_addr = 0x1C010000 l341
// gem5script.py: flags_addr is set to +0x30 in l141
#define SYSFLAGSET_BASE         0x1C000000 // needs to be section aligned
#define SYSFLAGSET_OFFSET       0x00010030

static lvaddr_t sysflagset_base = 0;
void write_sysflags_reg(uint32_t regval)
{
    if (sysflagset_base == 0) {

        printf("SYSFLAGSET_BASE is at 0x%x\n", SYSFLAGSET_BASE);
        sysflagset_base = paging_map_device(SYSFLAGSET_BASE,
                                            ARM_L1_SECTION_BYTES);

        printf(".. mapped to 0x%"PRIxLVADDR"\n", sysflagset_base);
    }

    lvaddr_t sysflags = sysflagset_base + SYSFLAGSET_OFFSET;
    printf(".. using address 0x%p\n", sysflags);

    writel(regval, (char *) sysflags);
}
