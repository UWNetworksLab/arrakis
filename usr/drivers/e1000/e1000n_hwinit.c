/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000_hwinit.c
 *
 *  Created on: Feb 12, 2013
 *      Author: mao
 *
 * Much referencing has been done against iPXE - Open Source Boot Firmware
 * and the Linux kernel.
 */
#include "e1000n.h"
#include "e1000n_hwinit.h"


/*****************************************************************
 * PHY
 *
 ****************************************************************/
/*****************************************************************
 * Writes a value to a PHY register
 *
 *****************************************************************/
/* TODO */

/***************************************************************************
 * Is EEPROM NVM or FLASH.
 *
 * Returns true if EEPROM is of NVM type, else false.
 *
 ****************************************************************************/
static bool e1000_is_onboard_nvm_eeprom(e1000_device_t *dev)
{
    if (dev->mac_type == e1000_82573) {
        e1000_eecd_t eecd = e1000_eecd_rd(dev->device);
        /* Isolate bits 15 & 16 */
        uint8_t eecd_flash = ((eecd >> 15) & 0x03);

        /* If both bits are set, device is Flash type */
        if (eecd_flash == 0x03) {
            return false;
        }
    }

    return true;
}

/*****************************************************************
 * Read e1000 EEPROM.
 *
 * TODO: Fix semaphore support and eeprom release on devices that
 *       need this.
 *
 * dev     - device to read eeprom from.
 * offset  - eeprom offset.
 * data    - returns data read.
 * Returns:  0 on success, 1 on timeout and 2 if no eeprom.
 ****************************************************************/
static errval_t e1000_read_eeprom(e1000_device_t *dev, uint64_t offset,
                                  uint16_t *data)
{
    int timeout = 1000;

    /* Make shore there are no direct access requests on
     * devices that support this.
     */
    if (dev->mac_type != e1000_82544) {
        e1000_eecd_ee_req_wrf(dev->device, 1);
    }

    while (!e1000_eecd_ee_gnt_rdf(dev->device)) {
        usec_delay(1000);
    }

    /* EEPROM present */
    // TODO(gz): Why does e1000 82574 have ee_pres == 0?
    if (e1000_eecd_ee_pres_rdf(dev->device) ||
            dev->mac_type == e1000_82574) {
        e1000_eerd_ms_t eerd_ms = 0;
        e1000_eerd_nm_t eerd_nm = 0;

        switch (dev->mac_type) {
        case e1000_82571:
        case e1000_82572:
        case e1000_82573:
        case e1000_82574:
        case e1000_82575:
            /* These devices have SPI or Microwire EEPROMs */
            eerd_ms = e1000_eerd_ms_start_insert(eerd_ms, 1);
            eerd_ms = e1000_eerd_ms_addr_insert(eerd_ms, offset);
            e1000_eerd_ms_wr(dev->device, eerd_ms);

            while (e1000_eerd_ms_done_rdf(dev->device) == 0 && 0 < timeout--) {
                usec_delay(1000);
            }

            *data = e1000_eerd_ms_data_rdf(dev->device);
            break;
        default:
            /* These devices have standard EEPROMs */
            eerd_nm = e1000_eerd_nm_start_insert(eerd_nm, 1);
            eerd_nm = e1000_eerd_nm_addr_insert(eerd_nm, offset);
            e1000_eerd_nm_wr(dev->device, eerd_nm);

            while (e1000_eerd_ms_done_rdf(dev->device) == 0 && 0 < timeout--) {
                usec_delay(1000);
            }

            *data = e1000_eerd_nm_data_rdf(dev->device);
            break;
        }
    } else {
        E1000_DEBUG("No EEPROM pressent.\n");
        e1000_eecd_ee_req_wrf(dev->device, 0);
        return -1;
    }

    if (timeout) {
        e1000_eecd_ee_req_wrf(dev->device, 0);
        return 0; /* Success */
    }

    E1000_DEBUG("EEPROM read timed out\n");

    e1000_eecd_ee_req_wrf(dev->device, 0);
    return 1;
}

/*****************************************************************
 * Check for EEPROM Auto Read bit done.
 *
 ****************************************************************/
static errval_t e1000_get_auto_rd_done(e1000_device_t *dev)
{
    uint16_t data;
    errval_t err;

    err = e1000_read_eeprom(dev, 0, &data);

    /* PHY configuration from NVM just starts after EECD_AUTO_RD sets to high.
     * Need to wait for PHY configuration completion before accessing NVM
     * and PHY. */
    if (dev->mac_type == e1000_82573) {
        usec_delay(2500);
    }

    return err;
}

/******************************************************************************
 * Reads the adapter's MAC address from the EEPROM and inverts the LSB for the
 * second function of dual function devices
 *
 *****************************************************************************/
static errval_t e1000_read_mac_addr(e1000_device_t *dev, uint8_t *mac_addr)
{
    uint16_t offset;
    uint16_t eeprom_data, i;
    e1000_status_t status;

    for (i = 0; i < MAC_ADDRESS_LEN; i += 2) {
        offset = i >> 1;
        if (e1000_read_eeprom(dev, offset, &eeprom_data) != 0) {
            return 1;
        }
        mac_addr[i] = (uint8_t) (eeprom_data & 0x00FF);
        mac_addr[i + 1] = (uint8_t) (eeprom_data >> 8);
    }

    switch (dev->mac_type) {
    default:
        break;
    case e1000_82546:
    case e1000_82546_rev_3:
    case e1000_82571:
        /* test LAN ID to see if we need to modify the MAC from EEPROM */
        status = e1000_status_rd(dev->device);
        if (e1000_status_func_id_extract(status) == e1000_lan_b) {
            mac_addr[5] ^= e1000_lan_b_mask;
        }
        break;
    }

    return 0;
}

/*****************************************************************
 * Reset the device and disable interrupts.
 *
 ****************************************************************/
static int e1000_reset(e1000_device_t *dev)
{
    errval_t err = 0;
    int timeout;

    /* disable interrupts */
    e1000_imc_rawwr(dev->device, 0xffffffff);

    /* disable receive and transmit */
    e1000_rctl_rawwr(dev->device, 0);
    e1000_tctl_rawwr(dev->device, 0);

    /* Delay to allow outstanding PCI transactions to complete before
     * reseting the device */
    usec_delay(1000);

    /* Exit from GIO management mode */
    if (dev->mac_type == e1000_82571 || dev->mac_type == e1000_82563
            || dev->mac_type == e1000_82573) {
        E1000_DEBUG("Disabling GIO management.\n");

        e1000_ctrl_gio_md_wrf(dev->device, 1);

        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_ctrl_gio_md_rdf(dev->device) && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to disable GIO management.\n");
            // return -1;
        }
    }

    /* Must reset PHY before reseting the MAC */
    if (dev->mac_type == e1000_82541 || dev->mac_type == e1000_82547) {
        e1000_ctrl_phy_rst_wrf(dev->device, 1);
    }

    /* Must acquire MDIO ownership before MAC reset
     * Ownership defaults to firmware after a reset */
    if (dev->mac_type == e1000_82573) {
        timeout = 1000;
        do {
            e1000_extcnf_ctrl_mdio_swown_wrf(dev->device, 1);
            usec_delay(200);
        } while (e1000_extcnf_ctrl_mdio_swown_rdf(dev->device) == 0
                 && 0 < timeout--);
    }

    E1000_DEBUG("Resetting device.\n");

    switch (dev->mac_type) {
    case e1000_82545_rev_3:
    case e1000_82546_rev_3:
        /* Reset is performed on a shadow of the control register
         * Where is this mentioned?
         */
        e1000_ctrldup_rst_wrf(dev->device, 1);
        break;
    case e1000_82540:
    case e1000_82541:
    case e1000_82541_rev_2:
    case e1000_82544:
    case e1000_82545:
    case e1000_82546:
        /* These controllers can't ack the 64-bit write when issuing the
         * reset, so use IO-mapping as a workaround to issue the reset
         * We don't support IO-mapped writing yet */
    default:
        e1000_ctrl_rst_wrf(dev->device, 1);

        /* Wait for reset to clear */
        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_ctrl_rst_rdf(dev->device) != 0 && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to reset device.\n");
        }
        break;
    }

    /* After MAC reset, force reload of EEPROM to restore power-on settings to
     * device.  Later controllers reload the EEPROM automatically, so just wait
     * for reload to complete.
     */
    switch (dev->mac_type) {
    case e1000_82542:
    case e1000_82543:
    case e1000_82544:
        e1000_ctrlext_ee_rst_wrf(dev->device, 1);
        /* Wait for EEPROM reload */
        usec_delay(2000);
        break;
    case e1000_82541:
    case e1000_82541_rev_2:
    case e1000_82547:
    case e1000_82547_rev_2:
        /* Wait for EEPROM reload */
        usec_delay(20000);
        break;
    case e1000_82573:
        if (e1000_is_onboard_nvm_eeprom(dev) == false) {
            usec_delay(100);
            e1000_ctrlext_ee_rst_wrf(dev->device, 1);
        }

        err = e1000_get_auto_rd_done(dev);
        break;
    default:
        err = e1000_get_auto_rd_done(dev);

        break;
    }

    if (err) {
        E1000_DEBUG("Auto read by HW from EEPROM did not complete.\n");
    }

    /* Disable HW ARPs on ASF enabled adapters */
    if (dev->mac_type >= e1000_82540 && dev->mac_type <= e1000_82547_rev_2) {
        e1000_manc_arp_req_en_wrf(dev->device, 0);
    }

    if (dev->mac_type == e1000_82541 || dev->mac_type == e1000_82547) {
        /* Configure activity LED after PHY reset */
        e1000_ledctl_t ledctl;

        // TODO:
//      e1000_phy_init_script(dev);

        /* I guess this is not realy needed to setup card LEDs */
        ledctl = e1000_ledctl_rd(dev->device);
        ledctl &= IGP_ACTIVITY_LED_MASK;
        ledctl = e1000_ledctl_led0_mode_insert(ledctl, 0x2);
        ledctl = e1000_ledctl_led3_mode_insert(ledctl, 0x3);
        e1000_ledctl_wr(dev->device, ledctl);
    }

    /* disable interrupts */
    e1000_imc_rawwr(dev->device, 0xffffffff);

    /* clear any pending interrupts */
    e1000_icr_rd(dev->device);

    return 0;
}

/*****************************************************************
 * Get media type.
 *
 ****************************************************************/
static void e1000_set_media_type(e1000_device_t *dev)
{
    e1000_status_t status;

    if (dev->mac_type != e1000_82543) {
        dev->tbi_combaility = false;
    }

    switch (dev->device_id) {
    case E1000_DEVICE_82545GM_SERDES:
    case E1000_DEVICE_82546GB_SERDES:
    case E1000_DEVICE_82571EB_SERDES:
    case E1000_DEVICE_82571EB_SERDES_DUAL:
    case E1000_DEVICE_82571EB_SERDES_QUAD:
    case E1000_DEVICE_82572EI_SERDES:
        dev->media_type = e1000_media_type_serdes;
        break;
    default:
        switch (dev->mac_type) {
            /*
             * According to: 1.3.7 Additional Ethernet Controller Features
             */
        case e1000_82546:
        case e1000_82545:
            dev->media_type = e1000_media_type_serdes;
            break;
        case e1000_82542:
            dev->media_type = e1000_media_type_fiber;
            break;
        case e1000_82573:
            /* The STATUS.tbimode bit is reserved or reused for the this
             * device.
             */
            dev->media_type = e1000_media_type_copper;
            break;
        default:
            status = e1000_status_rd(dev->device);
            if (e1000_status_tbimode_extract(status)) {
                dev->media_type = e1000_media_type_fiber;
                dev->tbi_combaility = false;
            } else {
                dev->media_type = e1000_media_type_copper;
            }
            break;
        }
        break;
    }
}

/*****************************************************************
 * Check link connection status.
 *
 ****************************************************************/
bool e1000_check_link_up(e1000_device_t *dev)
{
    e1000_status_t status = e1000_status_rd(dev->device);

    if (e1000_status_lu_extract(status)) {
        return true;
    }

    return false;
}

/*****************************************************************
 * Setup link auto-negotiation.
 *
 ****************************************************************/
bool e1000_auto_negotiate_link(e1000_device_t *dev)
{
    bool link_up = false;

    e1000_ctrlext_t ctrlext = e1000_ctrlext_rd(dev->device);
    if (e1000_ctrlext_link_mode_extract(ctrlext) == e1000_serdes) {
        E1000_DEBUG("Auto-negotiation: serdes mode");
        int timeout = 4000;
        e1000_txcw_ane_wrf(dev->device, 1);
        e1000_ctrl_lrst_wrf(dev->device, 1);

        while (e1000_rxcw_anc_rdf(dev->device) == 0 && 0 < timeout--) {
            usec_delay(10);
        }

        if (timeout > 0) {
            link_up = true;
        }

        if (!link_up) {
            e1000_txcw_ane_wrf(dev->device, 0);
        }
    } else {
        int timeout = 4000;

        // XXX: find out which cards really need this?
        if (dev->mac_type < e1000_82571) {
            e1000_ctrl_asde_wrf(dev->device, 1);
        }

        while (e1000_check_link_up(dev) == false && 0 < timeout--) {
            usec_delay(10);
        }

        link_up = e1000_check_link_up(dev);
    }

    E1000_DEBUG("Auto-negotiate link status: %s\n", e1000_check_link_up(dev) ? "link-up" : "link-down");
    return link_up;
}

/*****************************************************************
 * Set RX buffer size and enable receive unit.
 *
 ****************************************************************/
static void e1000_set_rxbsize(e1000_device_t *dev, e1000_rx_bsize_t rx_bsize)
{
    uint8_t bsize;
    uint8_t bsex;
    e1000_rctl_t rctl;

    switch (rx_bsize) {
    case bsize_16384:
        bsize = 0x1;
        bsex = 1;
        break;
    case bsize_8192:
        bsize = 0x2;
        bsex = 1;
        break;
    case bsize_4096:
        bsize = 0x3;
        bsex = 1;
        break;
    case bsize_2048:
        bsize = 0x0;
        bsex = 0;
        break;
    case bsize_1024:
        bsize = 0x1;
        bsex = 0;
        break;
    case bsize_512:
        bsize = 0x2;
        bsex = 0;
        break;
    case bsize_256:
    default:
        bsize = 0x3;
        bsex = 0;
        break;
    }

    rctl = e1000_rctl_rd(dev->device);
    rctl = e1000_rctl_bsize_insert(rctl, bsize);
    rctl = e1000_rctl_bsex_insert(rctl, bsex);
    rctl = e1000_rctl_bam_insert(rctl, 1);
    e1000_rctl_wr(dev->device, rctl);

    e1000_rctl_en_wrf(dev->device, 1);
}

/*****************************************************************
 * Set serial interface mode.
 *
 ****************************************************************/
static void e1000_set_serial_interface_mode(e1000_device_t *dev)
{
    e1000_ctrlext_t ctrlext = e1000_ctrlext_rd(dev->device);

    if (dev->mac_type == e1000_82544) {
        assert(!"XXX: How do we set these ones up?");
        return;
    }

    if (dev->mac_type == e1000_82573) {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_l82573);
    }
    else if (dev->media_type == e1000_media_type_serdes) {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_serdes);
    }
    else {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_glci);
    }
    /* write serial interface mode */
    e1000_ctrlext_wr(dev->device, ctrlext);
}

/*****************************************************************
 * Set Transmit Inter Packet Gap (TIPG)
 *
 ****************************************************************/
static void e1000_set_tipg(e1000_device_t *dev)
{
    e1000_tipg_t tipg = 0;

    if ((dev->mac_type <= e1000_82547_rev_2)
            && (dev->media_type == e1000_media_type_fiber
                || dev->media_type == e1000_media_type_serdes)) {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_FIBER);
    } else {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_COPPER);
    }

    switch (dev->mac_type) {
    case e1000_82542:
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT);
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82542_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82542_TIPG_IPGR2);
        break;
    case e1000_82575:
    case e1000_82576:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82575_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82575_TIPG_IPGR2);
        break;
    default:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82543_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82543_TIPG_IPGR2);
        break;
    }

    e1000_tipg_wr(dev->device, tipg);
}

/*****************************************************************
 * Configure card transmit
 *
 ****************************************************************/
static void e1000_configure_tx(e1000_device_t *dev)
{
    // TODO: configure_tx
    if (dev->mac_type >= e1000_82571 && dev->mac_type < e1000_82575) {
        /* Reset delay timers after every interrupt */
        e1000_ctrlext_int_tca_wrf(dev->device, 1);
    }

    /* Set Transmit Inter-Packet Gap (TIPG)*/
    e1000_set_tipg(dev);
}

/*****************************************************************
 * Configure device receive
 *
 ****************************************************************/
static void e1000_configure_rx(e1000_device_t *dev)
{
    /* set buffer size and enable receive unit */
    e1000_set_rxbsize(dev, dev->rx_bsize);
}

/*****************************************************************
 * Initialize the hardware
 *
 ****************************************************************/
void e1000_hwinit(e1000_device_t *dev, struct device_mem *bar_info,
                  int nr_allocated_bars, volatile struct tx_desc **transmit_ring,
                  volatile union rx_desc **receive_ring, int receive_buffers,
                  int transmit_buffers, uint8_t *mac_addr,
                  bool user_mac_addr, bool use_interrupt)
{
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    struct capref frame;
    errval_t err;

    E1000_DEBUG("Initializing network device.\n");

    if (nr_allocated_bars < 1) {
        E1000_PRINT_ERROR("Error: Not enough PCI bars allocated. Can not initialize network device.\n");
        exit(1);
    }

    err = map_device(&bar_info[0]);
    if (err_is_fail(err)) {
        E1000_PRINT_ERROR("Error: map_device failed. Can not initialize network device.\n");
        exit(err);
    }

    e1000_initialize(dev->device, (void *) bar_info[0].vaddr);

    E1000_DEBUG("Setting media type.\n");
    e1000_set_media_type(dev);

    err = e1000_reset(dev);
    if (err) {
        exit(1);
    }

    E1000_DEBUG("Deasserting PHY reset.\n");
    e1000_ctrl_phy_rst_wrf(dev->device, 0);

    E1000_DEBUG("Setting serial interface mode.\n");
    e1000_set_serial_interface_mode(dev);

    E1000_DEBUG("Auto negotiating link.\n");
    if (!e1000_auto_negotiate_link(dev)) {
        E1000_DEBUG("Auto negotiating link failed, force link-up in driver.\n");
        e1000_ctrl_slu_wrf(dev->device, 0x1);
        //set full-duplex
        e1000_ctrl_fd_wrf(dev->device, 0x1);
        e1000_ctrl_speed_wrf(dev->device, e1000_status_speed_rdf(dev->device));
    }

    /* set flow control */
    e1000_fcal_wr(dev->device, 0);
    e1000_fcah_wr(dev->device, 0);
    e1000_fct_wr(dev->device, 0);

    /* initialize statistic counters */
    for (int i = 0; i < e1000_statsregs_length; i++) {
        e1000_statsregs_rd(dev->device, i);
    }

    /* --------------------- MAC address setup --------------------- */
    E1000_DEBUG("Setting up MAC address.\n");

    /* is a valid MAC already present? */
    /* This will always return false due to hardware/software reset */
    bool mac_present = e1000_rah_av_rdf(dev->device, 0);

    if (user_mac_addr || !mac_present) {
        uint16_t mac_word0, mac_word1, mac_word2;
        e1000_rah_t rah = 0;

        if (user_mac_addr == false)
            /* read MAC from EEPROM */
        {
            e1000_read_mac_addr(dev, mac_addr);
        }

        mac_word0 = (((uint16_t) mac_addr[1]) << 8) | mac_addr[0];
        mac_word1 = (((uint16_t) mac_addr[3]) << 8) | mac_addr[2];
        mac_word2 = (((uint16_t) mac_addr[5]) << 8) | mac_addr[4];

        if (user_mac_addr == false && mac_word0 == 0 && mac_word1 == 0
                && mac_word2 == 0) {
            E1000_PRINT_ERROR("Error: Failed to read MAC address from EEPROM.\n");
            E1000_PRINT_ERROR("Try setting it manually. Use -h for help.\n");
            exit(1);
        }

        /* program card's address with MAC */
        e1000_rah_wr(dev->device, 0, 0);
        e1000_ral_wr(dev->device, 0, (mac_word0 | ((mac_word1) << 16)));
        rah = e1000_rah_rah_insert(rah, mac_word2);
        rah = e1000_rah_av_insert(rah, 1);
        e1000_rah_wr(dev->device, 0, rah);
    }

    /* cache MAC for stack to see */
    uint64_t mac_hi = e1000_rah_rah_rdf(dev->device, 0);
    uint64_t mac = e1000_ral_rd(dev->device, 0) + (mac_hi << 32);
    mac_addr[0] = mac & 0xff;
    mac_addr[1] = (mac >> 8) & 0xff;
    mac_addr[2] = (mac >> 16) & 0xff;
    mac_addr[3] = (mac >> 24) & 0xff;
    mac_addr[4] = (mac >> 32) & 0xff;
    mac_addr[5] = (mac >> 40) & 0xff;

    E1000_DEBUG("MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
                mac_addr[0], mac_addr[1], mac_addr[2], mac_addr[3], mac_addr[4], mac_addr[5]);

    /* clear all other filers (clear high-to-low (13.4.3)) */
    for (int i = 1; i < e1000_ral_length; i++) {
        e1000_rah_wr(dev->device, i, 0);
        e1000_ral_wr(dev->device, i, 0);
    }

    /* clear MTA table */
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(dev->device, i, 0);
    }

    /* --------------------- receive setup --------------------- */
    /* receive descriptor control */
    if (dev->mac_type == e1000_82575 || dev->mac_type == e1000_82576) {
        e1000_rxdctl_82575_t rxdctl = 0;

        rxdctl = e1000_rxdctl_82575_enable_insert(rxdctl, 1);
        rxdctl = e1000_rxdctl_82575_wthresh_insert(rxdctl, 1);
        e1000_rxdctl_82575_wr(dev->device, 0, rxdctl);
    }
    else {
        e1000_rxdctl_t rxdctl = 0;

        rxdctl = e1000_rxdctl_gran_insert(rxdctl, 1);
        rxdctl = e1000_rxdctl_wthresh_insert(rxdctl, 1);
        e1000_rxdctl_wr(dev->device, 0, rxdctl);

        e1000_rfctl_exsten_wrf(dev->device, 0);
    }

    /* Allocate and map frame for receive ring */
    *receive_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    sizeof(union rx_desc) * receive_buffers, &frame);

    if (*receive_ring == NULL) {
        E1000_PRINT_ERROR("Error: Failed to allocate map frame.\n");
        exit(1);
    }

    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        E1000_PRINT_ERROR("Error: Failed to invoke frame identify.\n");
        exit(1);
    }

    /* tell card where receive ring is */
    e1000_rdbal_wr(dev->device, 0, frameid.base & 0xffffffff);
    e1000_rdbah_wr(dev->device, 0, (frameid.base >> 32) & 0xffffffff);
    e1000_rdlen_len_wrf(dev->device, 0, (receive_buffers / 8));

    /* Initialize receive head and tail pointers */
    e1000_rdh_wr(dev->device, 0, 0);
    e1000_rdt_wr(dev->device, 0, 0);

    e1000_configure_rx(dev);

    /* --------------------- transmit setup --------------------- */
    if (dev->mac_type == e1000_82575 || dev->mac_type == e1000_82576) {
        e1000_txdctl_82575_t txdctl = 0;
        txdctl = e1000_txdctl_82575_enable_insert(txdctl, 1);
        txdctl = e1000_txdctl_82575_priority_insert(txdctl, 1);
        e1000_txdctl_82575_wr(dev->device, 0, txdctl);
    }
    else {
        e1000_txdctl_t txdctl = 0;
        txdctl = e1000_txdctl_gran_insert(txdctl, 1);
        e1000_txdctl_wr(dev->device, 0, txdctl);
    }

    /* allocate and map frame for transmit ring */
    *transmit_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                                     sizeof(struct tx_desc) * transmit_buffers, &frame);

    *transmit_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                                     sizeof(union rx_desc) * transmit_buffers, &frame);

    if (*transmit_ring == NULL) {
        E1000_PRINT_ERROR("Error: Failed to allocate map frame.\n");
        exit(1);
    }

    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        E1000_PRINT_ERROR("Error: Failed to invoke frame identify.\n");
        exit(1);
    }

    /* tell card about our transmit ring */
    e1000_tdbal_wr(dev->device, 0, frameid.base & 0xffffffff);
    e1000_tdbah_wr(dev->device, 0, frameid.base >> 32);
    e1000_tdlen_len_wrf(dev->device, 0, (transmit_buffers / 8));
    e1000_tdh_wr(dev->device, 0, 0);
    e1000_tdt_wr(dev->device, 0, 0);

    e1000_configure_tx(dev);

    /* enable transmit */
    {
        e1000_tctl_t tctl = 0;

        tctl = e1000_tctl_ct_insert(tctl, 0x10);
        tctl = e1000_tctl_en_insert(tctl, 1);
        tctl = e1000_tctl_psp_insert(tctl, 1);
        tctl = e1000_tctl_bst_insert(tctl, 0x40);
        e1000_tctl_wr(dev->device, tctl);
    }

    /* Enable interrupt throttling rate.
     *
     * The optimal performance setting for this register is very system and
     * configuration specific. A initial suggested range is 651-5580 (28Bh - 15CCh).
     * The value 0 will disable interrupt throttling
     */
    if (dev->mac_type == e1000_82575 || dev->mac_type == e1000_82576) {
        e1000_eitr_interval_wrf(dev->device, 0, 5580);
        //e1000_eitr_interval_wrf(dev->device, 0, 10);
    }
    else {
        e1000_itr_interval_wrf(dev->device, 5580);
        //e1000_itr_interval_wrf(dev->device, 10);
    }

    /* Enable interrupts */
    if (use_interrupt) {
        e1000_intreg_t intreg = 0;

        intreg = e1000_intreg_lsc_insert(intreg, 1);
        intreg = e1000_intreg_rxt0_insert(intreg, 1);
        e1000_ims_wr(dev->device, intreg);
    }
}

