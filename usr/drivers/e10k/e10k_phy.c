/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdbool.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include "sleep.h"
#include "e10k.h"

//#define DEBUG(x...) printf("e10k: " x)
#define DEBUG(x...) do {} while (0)


/** Acquire SWFW semaphore */
static bool e10k_swfwsem_acquire(e10k_t* d)
{
    while (e10k_swsm_smbi_rdf(d) != 0); // TODO: Timeout
    e10k_swsm_swesmbi_wrf(d, 1);
    while (e10k_swsm_swesmbi_rdf(d) == 0); // TODO: Timeout
    return true;
}

/** Release SWFW semaphore */
static void e10k_swfwsem_release(e10k_t* d)
{
    e10k_swsm_swesmbi_wrf(d, 0);
    e10k_swsm_smbi_wrf(d, 0);
}


static bool e10k_swfwlock_phy(e10k_t* d)
{
    bool good = false;

    // See 10.5.4
again:
    if (!e10k_swfwsem_acquire(d)) {
        return false;
    }

    if ((e10k_status_lan_id_rdf(d) == 0) &&
        (e10k_swfw_sync_sw_physm0_rdf(d) == 0) &&
        (e10k_swfw_sync_fw_physm0_rdf(d) == 0))
    {
        e10k_swfw_sync_sw_physm0_wrf(d, 1);
        good = true;
    } else if ((e10k_swfw_sync_sw_physm1_rdf(d) == 0) &&
        (e10k_swfw_sync_fw_physm1_rdf(d) == 0))
    {
        e10k_swfw_sync_sw_physm1_wrf(d, 1);
        good = true;
    }
    e10k_swfwsem_release(d);

    if (!good) {
        DEBUG("Failed, try again\n");
        milli_sleep(20);
        goto again;
    }

    return true;
}

static bool e10k_swfwunlock_phy(e10k_t* d)
{
    if (!e10k_swfwsem_acquire(d)) {
        return false;
    }

    if (e10k_status_lan_id_rdf(d) == 0) {
        e10k_swfw_sync_sw_physm0_wrf(d, 0);
    } else {
        e10k_swfw_sync_sw_physm1_wrf(d, 0);
    }

    e10k_swfwsem_release(d);

    // Technically this is only necessary in the case that the semaphore is
    // acquired again.
    milli_sleep(10);
    return true;
}

#if 0
static bool e10k_mdi_command(uint16_t mdi, uint8_t dev, uint8_t phy,
    e10k_mdi_opcode_t op)
{
    e10k_msca_t msca = e10k_msca_default;

    msca = e10k_msca_mdiadd_insert(msca, mdi);
    msca = e10k_msca_devadd_insert(msca, dev);
    msca = e10k_msca_phyadd_insert(msca, phy);
    msca = e10k_msca_opcode_insert(msca, op);
    msca = e10k_msca_stcode_insert(msca, e10k_new_proto);
    msca = e10k_msca_mdicmd_insert(msca, 1);


    // Issue command
    e10k_msca_wr(d, msca);

    // Wait for completion
    while (e10k_msca_mdicmd_rdf(d) != 0); // TODO: Timeout

    return true;
}

static bool e10k_phy_readreg(uint16_t mdi, uint8_t dev, uint8_t phy,
    uint16_t* value)
{
    bool success = true;

    if (!e10k_swfwlock_phy()) {
        return false;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_addr_cycle)) {
        success = false;
        goto out;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_read_op)) {
        success = false;
        goto out;
    }
    *value = e10k_msrwd_mdirddata_rdf(d);

out:
    e10k_swfwunlock_phy();
    return success;
}

static bool e10k_phy_writereg(uint16_t mdi, uint8_t dev, uint8_t phy,
    uint16_t value)
{
    bool success = true;

    if (!e10k_swfwlock_phy()) {
        return false;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_addr_cycle)) {
        success = false;
        goto out;
    }

    e10k_msrwd_mdiwrdata_wrf(d, value);
    if (!e10k_mdi_command(mdi, dev, phy, e10k_read_op)) {
        success = false;
        goto out;
    }

out:
    e10k_swfwunlock_phy();
    return success;
}

static bool e10k_phy_validate_address(uint8_t phy)
{
    bool success;
    uint16_t value;
    uint16_t v;

    /* IXGBE_MDIO_PHY_ID_HIGH */ /* IXGBE_MDIO_PMA_PMD_DEV_TYPE */
    success = e10k_phy_readreg(0x2, 0x1, phy, &value);
    if (!success) {
        DEBUG("Error reading\n");
    }

    uint8_t i;

    for (i = 0; i < 32; i++) {
        e10k_phy_readreg(i, 0x1, phy, &v);
        DEBUG("    [%x][%x] %x\n", phy, i, v);
    }

    return success && (value != 0xFFFF) && (value != 0x0);
}

static bool e10k_phy_read_id(uint8_t phy, uint16_t* id)
{
    /* IXGBE_MDIO_PHY_ID_HIGH */ /* IXGBE_MDIO_PMA_PMD_DEV_TYPE */
    return e10k_phy_readreg(0x2, 0x1, phy, id);
}

static bool e10k_phy_identify(void)
{
    uint8_t i;
    uint16_t id;
    for (i = 0; i < 32; i++) {
        if (e10k_phy_validate_address(i)) {
            if (!e10k_phy_read_id(i, &id)) {
                return false;
            }
            DEBUG("Found PHY addr=%x id=%x\n", i, id);
            return true;
        }
    }
    return false;
}
#endif

static uint16_t e10k_eeprom_read(e10k_t* d, uint16_t offset)
{
    e10k_eerd_t eerd = e10k_eerd_default;

    eerd = e10k_eerd_start_insert(eerd, 1);
    eerd = e10k_eerd_addr_insert(eerd, offset);
    e10k_eerd_wr(d, eerd);

    while (e10k_eerd_done_rdf(d) == 0); // TODO: Timeout

    return e10k_eerd_data_rdf(d);
}

void e10k_phy_init(e10k_t* d)
{
    /* IXGBE_PHY_INIT_OFFSET_NL */
    uint16_t list_offset;
    uint16_t data_offset = 0x0;
    uint16_t data_value;
    uint16_t sfp_id;
    uint16_t sfp_type = 0x4; /* SPF_DA_CORE1 */
    e10k_autoc_t autoc;

    list_offset = e10k_eeprom_read(d, 0x002B);
    DEBUG("list_offset=%x\n", list_offset);
    if ((list_offset == 0x0) || (list_offset == 0xFFFF)) {
        return;
    }

    list_offset++;

    sfp_id = e10k_eeprom_read(d, list_offset);
    DEBUG("sfp_id = %x\n", sfp_id);
    while (sfp_id != 0xFFFF) {
        if (sfp_id == sfp_type) {
            list_offset++;
            data_offset = e10k_eeprom_read(d, list_offset);
            if ((data_offset == 0x0) || (data_offset == 0xFFFF)) {
                DEBUG("sfp init failed\n");
                return;
            } else {
                break;
            }
        } else {
            list_offset += 2;
            sfp_id = e10k_eeprom_read(d, list_offset);
        }
        list_offset++;
    }

    if (sfp_id == 0xFFFF) {
        DEBUG("sfp init failed\n");
        return;
    }

    DEBUG("data_offset=%x\n", data_offset);

    e10k_swfwlock_phy(d);
    data_value = e10k_eeprom_read(d, ++data_offset);
    while (data_value != 0xFFFF) {
        DEBUG(" v=%x\n", data_value);
        e10k_corectl_wr(d, data_value);
        data_value = e10k_eeprom_read(d, ++data_offset);
    }
    e10k_swfwunlock_phy(d);

    milli_sleep(50);


    autoc = e10k_autoc_rd(d);
    autoc = e10k_autoc_lms_insert(autoc, 0x0);
    autoc = e10k_autoc_restart_an_insert(autoc, 1);
    e10k_autoc_wr(d, autoc);
    while (e10k_anlp1_anas_rdf(d) != 0); // TODO: Timeout


    autoc = e10k_autoc_rd(d);
    autoc = e10k_autoc_lms_insert(autoc, e10k_l10g_sfi);
    autoc = e10k_autoc_restart_an_insert(autoc, 1);
    e10k_autoc_wr(d, autoc);
    while (e10k_autoc_restart_an_rdf(d) != 0); // TODO: Timeout

    DEBUG("PHY init done\n");
}


