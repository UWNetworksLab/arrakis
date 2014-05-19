/**
 * \file
 * \brief OMAP44xx MMC Controller driver implementation
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>

#include <driverkit/driverkit.h>
#include <arch/arm/omap44xx/device_registers.h>

#include <dev/omap/omap44xx_mmchs1_dev.h>

#include "mmchs.h"

#define DBUF_SIZE (64*1024)
static char dbuf[DBUF_SIZE];

static omap44xx_mmchs1_t mmchs;

static void mmchs_soft_reset(void)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    omap44xx_mmchs1_mmchs_sysconfig_softreset_wrf(&mmchs, 0x1);
    while (omap44xx_mmchs1_mmchs_sysstatus_resetdone_rdf(&mmchs) != 0x1);

    MMCHS_DEBUG("%s:%d: sysctl reset\n", __FUNCTION__, __LINE__);
    omap44xx_mmchs1_mmchs_sysctl_sra_wrf(&mmchs, 0x1);
    while (omap44xx_mmchs1_mmchs_sysctl_sra_rdf(&mmchs) != 0x0);
}

static void set_hardware_capabilities(void)
{
    omap44xx_mmchs1_mmchs_capa_vs18_wrf(&mmchs, 0x1);
    omap44xx_mmchs1_mmchs_capa_vs30_wrf(&mmchs, 0x1);
}

static void set_wake_up_configuration(void)
{
    omap44xx_mmchs1_mmchs_sysconfig_enawakeup_wrf(&mmchs, 0x1);
    omap44xx_mmchs1_mmchs_hctl_iwe_wrf(&mmchs, 0x1);
}

/**
 * \see TRM rev Z, Section 24.5.1.2.1.7.2
 */
static void change_clock_frequency_to_fit_protocol(uint32_t clock)
{
    omap44xx_mmchs1_mmchs_sysctl_cen_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_sysctl_clkd_wrf(&mmchs, clock);

    MMCHS_DEBUG("%s:%d: Wait until clock is stable.\n", __FUNCTION__, __LINE__);
    while (omap44xx_mmchs1_mmchs_sysctl_ics_rdf(&mmchs) != 0x1);

    omap44xx_mmchs1_mmchs_sysctl_cen_wrf(&mmchs, 0x1);
}

static void mmc_host_and_bus_configuration(void)
{
    omap44xx_mmchs1_mmchs_con_od_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_con_dw8_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_con_ceata_wrf(&mmchs, 0x0);

    omap44xx_mmchs1_mmchs_hctl_sdvs_wrf(&mmchs, 0x6);
    omap44xx_mmchs1_mmchs_hctl_sdbp_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_hctl_dtw_wrf(&mmchs, 0x0);

    omap44xx_mmchs1_mmchs_sysctl_cen_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_sysctl_ice_wrf(&mmchs, 0x1);

    MMCHS_DEBUG("%s:%d: clksel = %u\n", __FUNCTION__, __LINE__, cm2_get_hsmmc1_base_clock());
    change_clock_frequency_to_fit_protocol(0x258);

    MMCHS_DEBUG("%s:%d: Wait until internal clock is stable.\n", __FUNCTION__, __LINE__);
    while (omap44xx_mmchs1_mmchs_sysctl_ics_rdf(&mmchs) != 0x1);

    omap44xx_mmchs1_mmchs_hctl_sdbp_wrf(&mmchs, 0x1);
    assert(omap44xx_mmchs1_mmchs_hctl_sdbp_rdf(&mmchs) == 0x1);

    // Pessimistic settings, we want to have this thing always ON for testing
    omap44xx_mmchs1_mmchs_sysconfig_clockactivity_wrf(&mmchs, 0x3);
    omap44xx_mmchs1_mmchs_sysconfig_standbymode_wrf(&mmchs, 0x1);
    omap44xx_mmchs1_mmchs_sysconfig_sidlemode_wrf(&mmchs, 0x1);
    omap44xx_mmchs1_mmchs_sysconfig_autoidle_wrf(&mmchs, 0x0);
}

/**
 * \see TRM rev Z, Section 24.5.1.2.1.1.1
 */
static void cmd_line_reset(void)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);

    omap44xx_mmchs1_mmchs_sysctl_src_wrf(&mmchs, 0x1);
    while (omap44xx_mmchs1_mmchs_sysctl_src_rdf(&mmchs) != 0x1);
    while (omap44xx_mmchs1_mmchs_sysctl_src_rdf(&mmchs) != 0x0);
}

/**
 * \see TRM rev Z, Section 24.5.1.2.1.2.1
 */
static void dat_line_reset(void)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);

    omap44xx_mmchs1_mmchs_sysctl_srd_wrf(&mmchs, 0x1);
    while (omap44xx_mmchs1_mmchs_sysctl_srd_rdf(&mmchs) != 0x1);
    while (omap44xx_mmchs1_mmchs_sysctl_srd_rdf(&mmchs) != 0x0);
}


// TODO(gz): Got this from old mmchs code, its ugly and should
// go away with a proper sleep functionality
volatile uint32_t dummy = 0;
static void wait_msec(long msec)
{
    int i = 0, sum = 0;
    long end = (1200000 * msec / 8);

    // Cannot use volatile variables in loop
    while (++i < end)  {
        sum += i + end;
    }

    dummy += sum;
}


/**
 * \see TRM rev Z, Section 24.5.1.2.1.7.1
 */
static void send_command(omap44xx_mmchs1_indx_status_t cmd, uint32_t arg)
{
    MMCHS_DEBUG("%s:%d: cmd = 0x%x arg=0x%x\n", __FUNCTION__, __LINE__, cmd, arg);

    MMCHS_DEBUG("%s:%d: Wait until command line is free.\n", __FUNCTION__, __LINE__);
    while (omap44xx_mmchs1_mmchs_pstate_cmdi_rdf(&mmchs) != 0x0);
    MMCHS_DEBUG("%s:%d: \n", __FUNCTION__, __LINE__);

    omap44xx_mmchs1_mmchs_stat_rawwr(&mmchs, ~0x0);

    // Only for MMC cards
    omap44xx_mmchs1_mmchs_con_mit_wrf(&mmchs, 0x0);
    omap44xx_mmchs1_mmchs_con_str_wrf(&mmchs, 0x0);

    omap44xx_mmchs1_mmchs_csre_rawwr(&mmchs, 0x0);

    omap44xx_mmchs1_mmchs_blk_blen_wrf(&mmchs, 512);
    omap44xx_mmchs1_mmchs_blk_nblk_wrf(&mmchs, 0x1);

    omap44xx_mmchs1_mmchs_sysctl_dto_wrf(&mmchs, 0xE); // omapconf

    omap44xx_mmchs1_mmchs_arg_rawwr(&mmchs, arg);

    // Enable interrupts
    omap44xx_mmchs1_mmchs_ie_rawwr(&mmchs, ~0x0);
    //omap44xx_mmchs1_mmchs_ise_rawwr(&mmchs, ~0x0);

    omap44xx_mmchs1_mmchs_cmd_t cmdreg = omap44xx_mmchs1_mmchs_cmd_default;

    // see TRM rev Z, Table 24-4
    // and Physical Layer Simplified Spec 3.01, Section 4.7.4
    switch (cmd) {
        // R2
    case omap44xx_mmchs1_INDX_2:
    case omap44xx_mmchs1_INDX_9:
        cmdreg = omap44xx_mmchs1_mmchs_cmd_rsp_type_insert(cmdreg, 0x1);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_ccce_insert(cmdreg, 0x1);
        break;
        // R1, R6, R5, R7
    case omap44xx_mmchs1_INDX_17:
        cmdreg = omap44xx_mmchs1_mmchs_cmd_ddir_insert(cmdreg, 0x1);
    case omap44xx_mmchs1_INDX_24:
        cmdreg = omap44xx_mmchs1_mmchs_cmd_dp_insert(cmdreg, 0x1);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_acen_insert(cmdreg, 0x1);
        // Fallthrough desired!
    case omap44xx_mmchs1_INDX_0:
    case omap44xx_mmchs1_INDX_3:
    case omap44xx_mmchs1_INDX_5:
    case omap44xx_mmchs1_INDX_8:
    case omap44xx_mmchs1_INDX_16:
    case omap44xx_mmchs1_INDX_41:
    case omap44xx_mmchs1_INDX_55:
        cmdreg = omap44xx_mmchs1_mmchs_cmd_rsp_type_insert(cmdreg, 0x2);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_ccce_insert(cmdreg, 0x1);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_cice_insert(cmdreg, 0x1);
        break;
        // R1b, R5b
    case omap44xx_mmchs1_INDX_7:
    case omap44xx_mmchs1_INDX_12:
        cmdreg = omap44xx_mmchs1_mmchs_cmd_rsp_type_insert(cmdreg, 0x3);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_ccce_insert(cmdreg, 0x1);
        cmdreg = omap44xx_mmchs1_mmchs_cmd_cice_insert(cmdreg, 0x1);
        break;
    default:
        assert(!"Unsupported command\n");
        break;
    }

    cmdreg = omap44xx_mmchs1_mmchs_cmd_indx_insert(cmdreg, cmd);
    omap44xx_mmchs1_mmchs_cmd_wr(&mmchs, cmdreg);

    MMCHS_DEBUG("%s:%d: Wait until mmchs_stat.cc == 0x1\n", __FUNCTION__, __LINE__);
    uint32_t cc = 0;
    size_t i = 0;
    do {
        uint32_t cto = omap44xx_mmchs1_mmchs_stat_cto_rdf(&mmchs);
        uint32_t ccrc = omap44xx_mmchs1_mmchs_stat_ccrc_rdf(&mmchs);
        cc = omap44xx_mmchs1_mmchs_stat_cc_rdf(&mmchs);

        if (cto == 0x1 && ccrc == 0x1) {
            MMCHS_DEBUG("%s:%d: cto = 1 ccrc = 1: Conflict on cmd line.\n", __FUNCTION__, __LINE__);
            cmd_line_reset();
            return;
        }
        if (cto == 0x1 && ccrc == 0x0) {
            MMCHS_DEBUG("%s:%d: cto = 1 ccrc = 0: Abort.\n", __FUNCTION__, __LINE__);
            cmd_line_reset();
            return;
        }

        if (i++ > 1000) {
            omap44xx_mmchs1_mmchs_stat_pr(dbuf, DBUF_SIZE, &mmchs);
            MMCHS_DEBUG("%s:%d: %s\n", __FUNCTION__, __LINE__, dbuf);
            USER_PANIC("Command not Ackd?");
        }
        wait_msec(1);
    } while (cc != 0x1);


    /*omap44xx_mmchs1_mmchs_pstate_pr(dbuf, DBUF_SIZE, &mmchs);
    MMCHS_DEBUG("%s:%d: \n%s\n", __FUNCTION__, __LINE__, dbuf);
    MMCHS_DEBUG("%s:%d: mmchs_rsp10 = 0x%x\n", __FUNCTION__, __LINE__,
           omap44xx_mmchs1_mmchs_rsp10_rd(&mmchs));
    MMCHS_DEBUG("%s:%d: mmchs_rsp32 = 0x%x\n", __FUNCTION__, __LINE__,
           omap44xx_mmchs1_mmchs_rsp32_rd(&mmchs));
    MMCHS_DEBUG("%s:%d: mmchs_rsp54 = 0x%x\n", __FUNCTION__, __LINE__,
           omap44xx_mmchs1_mmchs_rsp54_rd(&mmchs));
    MMCHS_DEBUG("%s:%d: mmchs_rsp76 = 0x%x\n", __FUNCTION__, __LINE__,
           omap44xx_mmchs1_mmchs_rsp76_rd(&mmchs));*/


    uint32_t resp_type = omap44xx_mmchs1_mmchs_cmd_rsp_type_rdf(&mmchs);
    if (resp_type == 0x0) {
        MMCHS_DEBUG("%s:%d: No response.\n", __FUNCTION__, __LINE__);
        return;
    }
}


/**
 * \see TRM rev Z, Figure 24-38
 */
static void mmchs_identify_card(void)
{
    // Module Initialization is done in mmchs_init()
    omap44xx_mmchs1_mmchs_ie_rawwr(&mmchs, 0xFFFFFFFF);

    omap44xx_mmchs1_mmchs_con_init_wrf(&mmchs, 0x1);

    omap44xx_mmchs1_mmchs_cmd_rawwr(&mmchs, 0x0);
    while (omap44xx_mmchs1_mmchs_stat_cc_rdf(&mmchs) != 0x1);

    wait_msec(10);
    omap44xx_mmchs1_mmchs_stat_cc_wrf(&mmchs, 0x1);

    omap44xx_mmchs1_mmchs_cmd_rawwr(&mmchs, 0x0);
    while (omap44xx_mmchs1_mmchs_stat_cc_rdf(&mmchs) != 0x1);

    omap44xx_mmchs1_mmchs_stat_cc_wrf(&mmchs, 0x1);
    omap44xx_mmchs1_mmchs_con_init_wrf(&mmchs, 0x0);

    //omap44xx_mmchs1_mmchs_stat_rawwr(&mmchs, 0xFFFFFFFF);

    omap44xx_mmchs1_mmchs_hctl_sdbp_wrf(&mmchs, 0x1);
    change_clock_frequency_to_fit_protocol(0xF0UL);

    send_command(0, 0x0);

    uint32_t arg8 = (0x1  << 8) | 0b10101010;
    send_command(8, arg8);
    assert(omap44xx_mmchs1_mmchs_stat_cc_rdf(&mmchs) == 0x1);

    send_command(55, 0x0);
    uint32_t ocrdata = 0;
    do {
        send_command(55, 0x0);
        MMCHS_DEBUG("%s:%d: ACMD41\n", __FUNCTION__, __LINE__);
        uint32_t arg41 = 0x1 << 30 | ocrdata;
        send_command(41, arg41);
        wait_msec(10);
        ocrdata = omap44xx_mmchs1_mmchs_rsp10_rd(&mmchs);
    } while ((omap44xx_mmchs1_mmchs_rsp10_rd(&mmchs) & (1 << 31)) == 0);

    MMCHS_DEBUG("%s:%d: CMD2\n", __FUNCTION__, __LINE__);
    send_command(2, 0x0);

    MMCHS_DEBUG("%s:%d: CMD3\n", __FUNCTION__, __LINE__);
    send_command(3, 0x1);
    uint32_t rca = omap44xx_mmchs1_mmchs_rsp10_rd(&mmchs) >> 16;
    MMCHS_DEBUG("Status: 0x%X\n", rca & 0xFFFF);
    MMCHS_DEBUG("RCA: 0x%X\n", (rca >> 16) & 0xFFFF);

    MMCHS_DEBUG("%s:%d: CMD9\n", __FUNCTION__, __LINE__);
    send_command(9, rca << 16);

    MMCHS_DEBUG("%s:%d: CMD7\n", __FUNCTION__, __LINE__);
    send_command(7, rca << 16);

    MMCHS_DEBUG("%s:%d: CMD16\n", __FUNCTION__, __LINE__);
    send_command(16, 512);
}

static errval_t complete_card_transaction(void)
{
    size_t i = 0;
    do {
        if ( omap44xx_mmchs1_mmchs_stat_tc_rdf(&mmchs) == 0x1 )  {
            //send_command(12, 0);
            return SYS_ERR_OK; // Fine as long as we support only finite transfers
        } else {
            bool deb = omap44xx_mmchs1_mmchs_stat_deb_rdf(&mmchs);
            bool dcrc = omap44xx_mmchs1_mmchs_stat_dcrc_rdf(&mmchs);
            bool dto = omap44xx_mmchs1_mmchs_stat_dto_rdf(&mmchs);

            if (deb || dcrc || dto) {
                MMCHS_DEBUG("%s:%d: Error interrupt during transfer: deb=%d dcrc=%d dto=%d.\n",
                            __FUNCTION__, __LINE__, deb, dcrc, dto);
                dat_line_reset();
                return MMC_ERR_TRANSFER;
            }
        }

        wait_msec(10);
    } while (i++ < 1000);

    MMCHS_DEBUG("%s:%d: No transfer complete interrupt?\n", __FUNCTION__, __LINE__);
    return MMC_ERR_TRANSFER;
}

/**
 * \brief Reads a 512-byte block on the card.
 *
 * \param block_nr Index number of block to read.
 * \param buffer Non-null buffer with a size of at least 512 bytes.
 *
 * \retval SYS_ERR_OK Block successfully written in buffer.
 * \retval MMC_ERR_TRANSFER Error interrupt or no transfer complete interrupt.
 * \retval MMC_ERR_READ_READY Card not ready to read.
 */
errval_t mmchs_read_block(size_t block_nr, void *buffer)
{
    MMCHS_DEBUG("%s:%d: Wait for free data lines.\n", __FUNCTION__, __LINE__);
    while (omap44xx_mmchs1_mmchs_pstate_dati_rdf(&mmchs) != 0x0);

    // Send data command
    send_command(17, block_nr);
    // TODO(gz): Check for errors

    for (size_t i = 0; i < (omap44xx_mmchs1_mmchs_blk_blen_rdf(&mmchs) + 3) / 4; i++) {
        size_t timeout = 1000;
        while (omap44xx_mmchs1_mmchs_stat_brr_rdf(&mmchs) == 0x0 && timeout--) {
            wait_msec(1);
        }
        if (timeout == 0) {
            return MMC_ERR_READ_READY;
        }

        ((uint32_t *) buffer)[i] = omap44xx_mmchs1_mmchs_data_rd(&mmchs);
    }

    return complete_card_transaction();
}

/**
 * \brief Write a 512-byte block in the card.
 *
 * \param block_nr Index number of block to write.
 * \param buffer Data to write (must be at least 512 bytes in size).
 *
 * \retval SYS_ERR_OK Block written to card.
 * \retval MMC_ERR_TRANSFER Error interrupt or no transfer complete interrupt.
 * \retval MMC_ERR_WRITE_READY Card not ready to write.
 */
errval_t mmchs_write_block(size_t block_nr, void *buffer)
{
    MMCHS_DEBUG("%s:%d: Wait for free data lines.\n", __FUNCTION__, __LINE__);
    size_t timeout = 1000;
    while (omap44xx_mmchs1_mmchs_pstate_dati_rdf(&mmchs) != 0x0 && timeout--) {
        wait_msec(1);
    }
    if (timeout == 0) {
        return MMC_ERR_WRITE_READY;
    }

    // Send data command
    send_command(24, block_nr);
    // TODO(gz): Check for errors

    for (size_t i = 0; i < (omap44xx_mmchs1_mmchs_blk_blen_rdf(&mmchs) + 3) / 4; i++) {
        while (omap44xx_mmchs1_mmchs_stat_bwr_rdf(&mmchs) == 0x0);
        omap44xx_mmchs1_mmchs_data_wr(&mmchs, ((uint32_t *) buffer)[i]);
    }

    return complete_card_transaction();
}

/**
 * MMC Initialization
 *
 * \see TRM rev Z, 24.5.1.1.2
 */
void mmchs_init(void)
{
    lvaddr_t mmchs_vaddr;
    errval_t err = map_device_register(OMAP44XX_MMCHS1, 0x1000, &mmchs_vaddr);
    assert(err_is_ok(err));

    omap44xx_mmchs1_initialize(&mmchs, (mackerel_addr_t)mmchs_vaddr);

    mmchs_soft_reset();
    set_hardware_capabilities();
    set_wake_up_configuration();
    mmc_host_and_bus_configuration();

    mmchs_identify_card();
}
