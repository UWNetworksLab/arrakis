/**
 * \file
 * \brief Intel 64 local APIC driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arch/x86/apic.h>
#include <arch/x86/start_aps.h>
#include <paging_kernel_arch.h>
#include <x86.h>

#include <dev/ia32_dev.h>
#include <dev/xapic_dev.h>

#define APIC_BASE_ADDRESS_MASK  0x000ffffffffff000
#define APIC_PAGE_SIZE          4096
#define APIC_DEFAULT_PRIORITY   0x0000

/**
 * The kernel's APIC ID.
 */
uint8_t apic_id;

/**
 * True iff we're on the BSP
 */
bool apic_bsp = true;

// global pointers used in init_ap.S
extern uint64_t x86_64_start_ap;
extern uint64_t x86_64_init_ap_wait;
extern uint64_t x86_32_start_ap;
extern uint64_t x86_32_init_ap_wait;

static xapic_t apic;

/**
 * \brief Initializes the local APIC timer.
 *
 * \param masked        Mask interrupt
 * \param periodic      Periodic or one-shot
 */
void apic_timer_init(bool masked, bool periodic)
{
    xapic_lvt_timer_t t = xapic_lvt_timer_initial;
    t = xapic_lvt_timer_vector_insert(t, APIC_TIMER_INTERRUPT_VECTOR);
    t = xapic_lvt_timer_mask_insert(t, masked ? xapic_masked : xapic_not_masked );
    t = xapic_lvt_timer_mode_insert(t, periodic ? xapic_periodic : xapic_one_shot);
    xapic_lvt_timer_wr(&apic, t);
}

/**
 * \brief Initializes performance counter overflow interrupt
 *
 */
void apic_perfcnt_init(void)
{
    // Activate use of APIC performance counter interrupts
    xapic_lvt_mon_t t = xapic_lvt_perf_cnt_initial;
    t = xapic_lvt_mon_vector_insert(t, APIC_PERFORMANCE_INTERRUPT_VECTOR);
    t = xapic_lvt_mon_msgType_insert(t, 0);
    t = xapic_lvt_mon_mask_insert(t, 0);
    xapic_lvt_perf_cnt_wr(&apic, t);
}
/**
 * \brief Initializes performance counter overflow interrupt
 *
 */
void apic_perfcnt_stop(void)
{
    // Deactivate use of APIC performance counter interrupts
    // Activate use of APIC performance counter interrupts
    xapic_lvt_mon_t t = xapic_lvt_perf_cnt_initial;
    t = xapic_lvt_mon_vector_insert(t, APIC_PERFORMANCE_INTERRUPT_VECTOR);
    t = xapic_lvt_mon_msgType_insert(t, 0);
    t = xapic_lvt_mon_mask_insert(t, 1);
    xapic_lvt_perf_cnt_wr(&apic, t);
}

void apic_timer_set_count(uint32_t count)
{
    xapic_init_count_wr(&apic, count);
}

uint32_t apic_timer_get_count(void)
{
    return xapic_cur_count_rd(&apic);
}

void apic_timer_set_divide(xapic_divide_t divide)
{
    xapic_dcr_div_val_wrf(&apic, divide);
}

/** \brief This function initializes the local APIC.
    This function initialzes the local APIC by writes to the memory mapped
    registers and by enabling it in the MSR.
*/
void apic_init(void)
{
    //pointer to a variable used as pseudo-lock to synchronize the BSP
    //and the AP which gets enabled
#if defined (__x86_64__)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_64_init_ap_wait - ((lpaddr_t)&x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);
#elif defined (__i386__)
#       if !defined(__scc__)
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t)&x86_32_init_ap_wait - ((lpaddr_t)&x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);
#       endif
#else
#error "Architecture not supported"
#endif

#if !defined(__scc__)
    ia32_apic_base_t apic_base_msr = ia32_apic_base_rd(NULL);
    lpaddr_t apic_phys = ((lpaddr_t)apic_base_msr) & APIC_BASE_ADDRESS_MASK;
    lvaddr_t apic_base = paging_map_device(apic_phys, APIC_PAGE_SIZE);
#else
    lpaddr_t apic_phys = (lpaddr_t)0xfee00000;
    lvaddr_t apic_base = paging_map_device((lpaddr_t)0xfee00000, APIC_PAGE_SIZE);
#endif

    if(apic_base == 0) {
        panic("apic_init(): could not map APIC registers");
    }

    debug(SUBSYS_APIC, "Accessing APIC at 0x%"PRIxLPADDR" / 0x%"PRIxLVADDR"\n",
          apic_phys, apic_base);
    xapic_initialize(&apic, (void *)apic_base);

#if !defined(__scc__)
    apic_id = apic_get_id();
    debug(SUBSYS_APIC, "APIC ID=%hhu\n", apic_id);
    if (ia32_apic_base_bsp_extract(apic_base_msr)) {
        debug(SUBSYS_APIC, "APIC: bootstrap processor\n");
        apic_bsp = true;
    } else {
        debug(SUBSYS_APIC, "APIC: application processor\n");
        apic_bsp = false;
        *ap_wait = AP_STARTED;
    }
#endif

    // initialize spurious interrupt register
    // no focus, software enabled
    {
	xapic_svr_t t = xapic_svr_initial; 
	t = xapic_svr_vector_insert(t, APIC_SPURIOUS_INTERRUPT_VECTOR);
	t = xapic_svr_enable_insert(t, 1);
	t = xapic_svr_focus_insert( t, 1);
	xapic_svr_wr(&apic, t);
    }

    // Task priority
    {
	xapic_priority_t t = xapic_tpr_initial;
	t = xapic_priority_sub_class_insert(t, APIC_DEFAULT_PRIORITY);
	t = xapic_priority_priority_insert( t, APIC_DEFAULT_PRIORITY);
	xapic_tpr_wr(&apic, t);
    }

    //LVT timer register
    //set vector number and disable reception of this interrupt
    {
	xapic_lvt_timer_t t = xapic_lvt_timer_initial;
	t = xapic_lvt_timer_vector_insert(t, APIC_TIMER_INTERRUPT_VECTOR);
	t = xapic_lvt_timer_mask_insert(  t, xapic_masked);
	xapic_lvt_timer_wr(&apic, t);
    }

    //thermal sensor
    {
	xapic_lvt_mon_t t = xapic_lvt_thermal_initial;
	t = xapic_lvt_timer_vector_insert(t, APIC_THERMAL_INTERRUPT_VECTOR);
	t = xapic_lvt_timer_mask_insert(  t, xapic_masked);
	xapic_lvt_thermal_wr(&apic, t);
    }

#if defined(__scc__)
    //LINT0: inter-core interrupt
    //generate fixed int
    {
	xapic_lvt_lint_t t = xapic_lvt_lint0_initial;
	t = xapic_lvt_lint_vector_insert(   t, APIC_INTER_CORE_VECTOR);
	t = xapic_lvt_lint_dlv_mode_insert( t, xapic_fixed);
	t = xapic_lvt_lint_trig_mode_insert(t, xapic_edge);
	t = xapic_lvt_lint_mask_insert(     t, xapic_not_masked);
	xapic_lvt_lint0_wr(&apic, t);

	//LINT1: usually used to generate an NMI
	//generate device interrupt
	t = xapic_lvt_lint1_initial;
	t = xapic_lvt_lint_vector_insert(   t, 32);
	t = xapic_lvt_lint_dlv_mode_insert( t, xapic_fixed);
	t = xapic_lvt_lint_trig_mode_insert(t, xapic_edge);
	t = xapic_lvt_lint_mask_insert(     t, xapic_not_masked);
	xapic_lvt_lint1_wr(&apic, t);
    }
#else
    //LINT0: external interrupts, delivered by the 8259 PIC
    //generate extInt as if INTR pin were activated
    //disabled (we use IOAPICs exclusively)
    {
	xapic_lvt_lint_t t = xapic_lvt_lint0_initial;
	t = xapic_lvt_lint_vector_insert(   t, 0);
	t = xapic_lvt_lint_dlv_mode_insert( t, xapic_extint);
	t = xapic_lvt_lint_trig_mode_insert(t, xapic_edge);
	t = xapic_lvt_lint_mask_insert(     t, xapic_masked);
	xapic_lvt_lint0_wr(&apic, t);

	//LINT1: usually used to generate an NMI
	//generate NMI
	//disabled (FIXME?)
	t = xapic_lvt_lint1_initial;
	t = xapic_lvt_lint_vector_insert(   t, 0);
	t = xapic_lvt_lint_dlv_mode_insert( t, xapic_extint); //xapic_nmi,
	t = xapic_lvt_lint_trig_mode_insert(t, xapic_edge);
	t = xapic_lvt_lint_mask_insert(     t, xapic_masked);
	xapic_lvt_lint1_wr(&apic, t);
    }
#endif

    //error interrupt register
    {
	xapic_lvt_err_t t = xapic_lvt_err_initial;
	t = xapic_lvt_err_vector_insert(t, APIC_ERROR_INTERRUPT_VECTOR);
	t = xapic_lvt_err_mask_insert(  t, xapic_not_masked);
	xapic_lvt_err_wr(&apic, t);
    }

#if 0 // this doesn't seem necessary, and causes problems on non-AMD HW -AB
    //we need to enable this bit in the AMD case, but not in the Intel case.
    if (CPU_IS_M5_SIMULATOR) {
        printf("Warning: not setting xapic_eacr_wr on M5\n");
    } else {
        //    if (strcmp(cpuid_res.cpu_vendor,"AuthenticAMD")==0) {
        xapic_eacr_iern_wrf(&apic, 1);
        //    }
    }
#endif

#if !defined(__scc__)
    // enable the thing, if it wasn't already!
    if (!(ia32_apic_base_global_extract(apic_base_msr))) {
        apic_base_msr = ia32_apic_base_global_insert(apic_base_msr, 1);
        ia32_apic_base_wr(NULL,apic_base_msr);
    }
#endif
}

/** \brief This function sends an IPI
    Two INIT IPIs can be sent, assert or de-assert INIT IPIs. This function is
    called by either apic_send_init_assert or apic_send_init_deassert.
    apic_init() has to be called once before using this function,
    because this function doesn't map the physical page of the APIC
    registers another time.
    \param int_cmd_1 The structure containing the bits for interrupt
           command register 1
    \param destination The destination for the INIT IPI
    \param wait Should we wait for delivery?
*/

static void apic_send_ipi( xapic_icr_lo_t cmd, uint8_t destination, bool wait)
{
    //clear the previous error, if nobody was interested before.
    //otherwise it isn't possible to send another IPI
    xapic_esr_rawwr(&apic,0);
    xapic_esr_rawwr(&apic,0);

    //send the IPI
    xapic_icr_hi_t t = xapic_icr_hi_initial;
    t = xapic_icr_hi_dest_insert(t, destination);
    xapic_icr_hi_rawwr(&apic, t);
    xapic_icr_lo_rawwr(&apic, cmd);

    // Wait for delivery
    while( wait && xapic_icr_lo_dlv_stat_rdf(&apic) );
}

/** \brief Send an INIT IPI (assert mode)
    This function sends an INIT IPI to the destination processor
    or the processors included in the shorthand in assert mode.
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_init_assert(uint8_t destination, uint8_t destination_shorthand)
{
    xapic_icr_lo_t t = xapic_icr_lo_initial;
    t = xapic_icr_lo_vector_insert(   t, 0);
    t = xapic_icr_lo_dlv_mode_insert( t, xapic_init);
    t = xapic_icr_lo_dst_mode_insert( t, xapic_dst_phys);
    t = xapic_icr_lo_level_insert(    t, xapic_lvl_set);
    t = xapic_icr_lo_trig_mode_insert(t, xapic_level);
    t = xapic_icr_lo_dst_short_insert(t, destination_shorthand);
    apic_send_ipi( t, destination, true );
}

/** \brief Send an INIT IPI (de-assert mode)
    This function sends an INIT IPI to all processors. INIT IPIs in
    de-assert mode are always sent to all processors, regardless of the
    destination value and the destination shorthand value.
*/

void apic_send_init_deassert(void)
{
    xapic_icr_lo_t t = xapic_icr_lo_initial;
    t = xapic_icr_lo_vector_insert(   t, 0);
    t = xapic_icr_lo_dlv_mode_insert( t, xapic_init);
    t = xapic_icr_lo_dst_mode_insert( t, xapic_dst_phys);
    t = xapic_icr_lo_level_insert(    t, xapic_lvl_clr);
    t = xapic_icr_lo_trig_mode_insert(t, xapic_level);
    t = xapic_icr_lo_dst_short_insert(t, xapic_all_inc);
    apic_send_ipi( t, 0, true );
}


/** \brief Send a Start-Up IPI

    This function sends a Start-Up IPI to the destination processor
    or the processors included in the shorthand.
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_start_up(uint8_t destination,
                        uint8_t destination_shorthand,
                        uint8_t realmode_startpage)
{
    xapic_icr_lo_t t = xapic_icr_lo_initial;
    t = xapic_icr_lo_vector_insert(   t, realmode_startpage);
    t = xapic_icr_lo_dlv_mode_insert( t, xapic_startup);
    t = xapic_icr_lo_dst_mode_insert( t, xapic_dst_phys);
    t = xapic_icr_lo_level_insert(    t, xapic_lvl_set);
    t = xapic_icr_lo_trig_mode_insert(t, xapic_edge);
    t = xapic_icr_lo_dst_short_insert(t, destination_shorthand);
    apic_send_ipi( t, destination, true );
}


/** \brief Send an standard IPI to the definded target APIC
    This function sends an standard IPI to the target APIC
    \param destination The destination, if shorthand = xapic_none
    \param destination_shorthand THe shorthand of the destination which may be
           xapic_none, xapic_self, xapic_all_inc or xapic_all_exc
*/

void apic_send_std_ipi(uint8_t destination,
                       uint8_t destination_shorthand,
                       uint8_t vector)
{
    xapic_icr_lo_t t = xapic_icr_lo_initial;
    t = xapic_icr_lo_vector_insert(   t, vector);
    t = xapic_icr_lo_dlv_mode_insert( t, xapic_fixed);
    t = xapic_icr_lo_dst_mode_insert( t, xapic_dst_phys);
    t = xapic_icr_lo_level_insert(    t, xapic_lvl_clr);
    t = xapic_icr_lo_trig_mode_insert(t, xapic_edge);
    t = xapic_icr_lo_dst_short_insert(t, destination_shorthand);
    apic_send_ipi( t, destination, false );
}

/** \brief This function sends an EOI to the local APIC
    An End-Of-Interrupt is sent to the local APIC. This function should be
    called at the end of an interrupt handler.
*/
void apic_eoi(void)
{
    //has to be 0 for future compability
    xapic_eoi_wr(&apic,0);
}

/** \brief This function sends a specific EOI to the local APIC
    A specific End-Of-Interrupt is sent to the local APIC. This function should
    be called at the end of an interrupt handler.
*/
void apic_seoi(uint8_t int_nr)
{
    xapic_seoi_vector_wrf(&apic, int_nr);
}

uint8_t apic_get_id(void)
{
    return xapic_id_id_rdf(&apic);
}

/**
 * Mask the timer interrupt.
 */
void apic_mask_timer(void)
{
    xapic_lvt_timer_mask_wrf(&apic, xapic_masked);
}

/**
 * Unmask the timer interrupt.
 */
void apic_unmask_timer(void)
{
    xapic_lvt_timer_mask_wrf(&apic, xapic_not_masked);
}

bool arch_core_is_bsp(void)
{
    return apic_is_bsp();
}

xapic_esr_t apic_get_esr(void)
{
    // 10.5.3: Have to write to ESR before reading it
    xapic_esr_rawwr(&apic, 0);
    return xapic_esr_rd(&apic);
}
