/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include "ahcid.h"
#include <barrelfish/waitset.h>
#include <barrelfish/syscalls.h>
#include <barrelfish/nameservice_client.h>
#include <pci/pci.h>
#include <if/ahci_mgmt_defs.h>
#include <skb/skb.h>
#include <ahci/sata_fis.h>
#include <ahci/ahci_dma_pool.h>
#include "ahci_hba_dev.h"
#include <ata_identify_dev.h>
#include "ahcid_debug.h"

#define PCI_DEVICE_ICH9R_82801IR 0x2922
#define PCI_DEVICE_ICH10_82801JI 0x3a22
#define PCI_DEVICE_SB7x0 0x4390 // also SB8x0 SB9x0

static ahci_hba_t controller;
static uint8_t num_ports = -1;
static struct ahcid_port_info **ports;

static struct ahci_mgmt_binding **port_bindings;

static char *my_service_name = "ahcid";

static struct device_mem hbabar;

#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
    static bool initialized = false;
#endif


static void rx_list_call(struct ahci_mgmt_binding *b)
{
    AHCID_DEBUG("got list call\n");
    uint8_t port;
    uint8_t current_num_ports = (num_ports == -1 ? 0 : num_ports);
    uint8_t next_port = 0;
    uint8_t *port_ids = malloc(current_num_ports);
    for (port = 0; port < current_num_ports; port++) {
        if (ports[port] && ports[port]->status == AHCID_PORT_STATUS_IDLE) {
            port_ids[next_port++] = port;
        }
        else if (ports[port]) {
            AHCID_DEBUG("skipping port %u with status %d\n",
                    (unsigned int)port, (int)ports[port]->status);
        }
    }
    AHCID_DEBUG("responding to list call with %u port_ids\n", (unsigned int)next_port);
    ahci_mgmt_list_response__tx(b, MKCLOSURE(free, port_ids), port_ids, next_port);
}

static void rx_identify_call(struct ahci_mgmt_binding *b, uint8_t port_id)
{
    AHCID_DEBUG("got identify call\n");
    if (ports[port_id] && ports[port_id]->status == AHCID_PORT_STATUS_IDLE) {
        AHCID_DEBUG("responding...\n");
        ahci_mgmt_identify_response__tx(b, NOP_CONT,
                (uint8_t*)ports[port_id]->identify_data, 512);
    }
    else {
        AHCID_DEBUG("not responding...\n");
    }
}

static void rx_open_call(struct ahci_mgmt_binding *b, uint8_t port_id)
{
    AHCID_DEBUG("got open call\n");
    if (ports[port_id] == NULL) {
        ahci_mgmt_open_response__tx(b, NOP_CONT, AHCI_ERR_PORT_INVALID,
                NULL_CAP, 0, 0);
        return;
    }
    if (ports[port_id]->binding != NULL) {
        ahci_mgmt_open_response__tx(b, NOP_CONT, AHCI_ERR_PORT_BUSY,
                NULL_CAP, 0, 0);
        return;
    }
    ports[port_id]->binding = b;
    // find correct frame cap for port. should be a single cap as long as FRAME_SIZE > 0x80b.
    uint32_t cap_size = hbabar.bytes / hbabar.nr_caps;
    uint32_t offset = ahcid_port_offset(port_id);
    int cap_index = 0;
    while (offset >= cap_size) { cap_index++; offset -= cap_size; }

    ahci_mgmt_open_response__tx(b, NOP_CONT, SYS_ERR_OK,
            hbabar.frame_cap[cap_index], offset, ahci_hba_cap_rd(&controller));
}

static void rx_close_call(struct ahci_mgmt_binding *b, uint8_t port_id)
{
    AHCID_DEBUG("got close call\n");
    if (ports[port_id] == NULL) {
        ahci_mgmt_close_response__tx(b, NOP_CONT, AHCI_ERR_PORT_INVALID);
        return;
    }
    errval_t retval = AHCI_ERR_PORT_MISMATCH;
    if (ports[port_id]->binding == b) {
        ports[port_id]->binding = NULL;
        retval = SYS_ERR_OK;
    }
    ahci_mgmt_close_response__tx(b, NOP_CONT, retval);
    return;
}

static struct ahci_mgmt_rx_vtbl rx_vtbl = {
    .list_call = rx_list_call,
    .list_response = NULL,
    .identify_call = rx_identify_call,
    .identify_response = NULL,
    .open_call = rx_open_call,
    .open_response = NULL,
    .close_call = rx_close_call,
    .close_response = NULL,
    .command_completed = NULL,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    AHCID_DEBUG("service exported at iref %u\n", iref);

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct ahci_mgmt_binding *b)
{
    AHCID_DEBUG("got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void do_identify(struct ahcid_port_info *port)
{
    errval_t r;

    // allocate frame for command table
    struct ahci_dma_region *command_table;
    r = ahci_dma_region_alloc(0x90, &command_table);
    assert(err_is_ok(r));

    // write command into command list
    ahci_port_chdr_t commandlist = (ahci_port_chdr_t)port->command_list->vaddr;
    memset(commandlist, 0, ahci_port_chdr_size);

#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
    char buf[4096];
    ahci_port_chdr_prtval(buf, 4096, commandlist);
    AHCID_DEBUG("Command Header: \n");
    puts(buf);
#endif

    // allocate dma region for identify result
    struct ahci_dma_region **prd = calloc(1, sizeof(struct ahci_dma_region*));
    r = ahci_dma_region_alloc(512, prd);
    assert(err_is_ok(r));

    memset((*prd)->vaddr, 0, 512);
    port->prdts = prd;
    port->prdt_count = 1;

    ahci_port_chdr_w_insert(commandlist, 0); // dma on receive
    ahci_port_chdr_a_insert(commandlist, 0);
    ahci_port_chdr_cfl_insert(commandlist, 5);

    // set dma region for identify result
    ahci_port_chdr_prdtl_insert(commandlist, 1); // we use 1 PRD
    ahci_port_chdr_ctba_insert(commandlist, (uint32_t)command_table->paddr);
    ahci_port_chdr_ctbau_insert(commandlist, (uint32_t)(command_table->paddr>>32));


    //char buf[4096];
#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
    ahci_port_chdr_prtval(buf, 4096, commandlist);
    AHCID_DEBUG("Command Header: \n");
    puts(buf);
#endif

    // fill command table
    memset(command_table->vaddr, 0, 0x90);

    // FIS
    struct sata_fis_reg_h2d *fis = command_table->vaddr;
    fis->type = SATA_FIS_TYPE_H2D;
    fis->command = 0xEC /* ATA_CMD_IDENTIFY */;
    fis->device = 0;
    fis->specialstuff = 0x80 /* command */;

    // place PRD into PRDT which is at offset 0x80
    uint32_t *prdt_entry = command_table->vaddr + 0x80;
    prdt_entry[0] = (*prd)->paddr;
    prdt_entry[3] = 511; // again this +1 thingy

    // ensure command processing is on
    ahci_port_cmd_t cmd = ahci_port_cmd_rd(&port->port);
    assert(ahci_port_cmd_cr_extract(cmd) == 1);

    // issue command in slot 0
    ahci_port_ci_wr(&port->port, 1);

    AHCID_DEBUG("Issued IDENTIFY command\n");

    port->status = AHCID_PORT_STATUS_IDENTIFY_PENDING;
}


static void ahci_init(struct device_mem *bar_info, int nr_allocated_bars)
{
    // re-usable vars
    int i;
    char buf[4096];
    errval_t r;

    // registers
    ahci_hba_ghc_t ghc;

    // find BAR and map device data
    AHCID_DEBUG("nr_allocated_bars = %d\n", nr_allocated_bars);
    assert(nr_allocated_bars >= 1);
    // Although the AHCI specification requires the AHCI memory region to be in
    // BAR 5 (BAR 0 to 4 are used for legacy IDE mode) the QEMU AHCI emulation
    // incorrectly uses BAR 0.  Because of this, ahcid consults both BAR 0 and
    // BAR 5 to find the HBA's memory mapped I/O region.
    if (nr_allocated_bars == 1) { // handle QEMU
        memcpy(&hbabar, &bar_info[0], sizeof(struct device_mem));
    } else if (nr_allocated_bars == 6) { // handle HW (AHCI in BAR 5)
        memcpy(&hbabar, &bar_info[5], sizeof(struct device_mem));
    } else {
        AHCID_DEBUG("strange device... not supported\n");
        abort();
    }

    map_device(&hbabar);
    ahci_hba_initialize(&controller, (void *)(hbabar.vaddr));
    AHCID_DEBUG("accessing conf regs starting at %p\n",
            (void *)(hbabar.vaddr));
    AHCID_DEBUG("physical address of conf regs: %p\n",
            (void *)(hbabar.paddr));

    // first of all, reset the device for fun and profit
    ghc = ahci_hba_ghc_rd(&controller);
    ghc = ahci_hba_ghc_hr_insert(ghc, 1);
    AHCID_DEBUG("Resetting HBA (setting ghc = %x)...\n", ghc);
    ahci_hba_ghc_wr(&controller, ghc);

    // spec: "the device shall reset this bit to '0' " so we will wait
    while (1) {
        ghc = ahci_hba_ghc_rd(&controller);
        if (ahci_hba_ghc_hr_extract(ghc) == 0) {
            AHCID_DEBUG("reset done\n");
            break;
        }
        sys_yield(CPTR_NULL);
    }

    // set HBA into AHCI mode
    AHCID_DEBUG("Setting controller into AHCI mode\n");
    ghc = ahci_hba_ghc_rd(&controller);
    ghc = ahci_hba_ghc_ae_insert(ghc, 1);
    ahci_hba_ghc_wr(&controller, ghc);

    // disable interrupts for the moment during setup
    ghc = ahci_hba_ghc_rd(&controller);
    if (ahci_hba_ghc_ie_extract(ghc) == 1) {
        AHCID_DEBUG("Interrupts are enabled. Disabling them during setup\n");
        ghc = ahci_hba_ghc_ie_insert(ghc, 1);
        ahci_hba_ghc_wr(&controller, ghc);
    } else {
        AHCID_DEBUG("Interrupts are disabled (as expected after a reset)\n");
    }

    // get number of ports the HBA supports
    // AHCI spec 1.3, section 3.1.1; bits 0-4 specify number of ports, 0 being 1 port
    num_ports = ahci_hba_cap_np_extract(ahci_hba_cap_rd(&controller)) + 1;
    AHCID_DEBUG("HBA supports %d ports\n", num_ports);

    ports = calloc(num_ports, sizeof(struct ahcid_port_info*));
    port_bindings = calloc(num_ports, sizeof(struct ahci_mgmt_binding*));

    // enable interrupts again
    ghc = ahci_hba_ghc_rd(&controller);
    ghc = ahci_hba_ghc_ie_insert(ghc, 1);
    AHCID_DEBUG("Enabling HBA Interrupts\n");
    ahci_hba_ghc_wr(&controller, ghc);

    // init dma pool
    r = ahci_dma_pool_init(1024 * 1024);

    // initialize ports
    r = ahcid_ports_init(ports, num_ports, ahci_hba_pi_rd(&controller),
            (void *)(hbabar.vaddr));

    for (i = 0; i < num_ports; i++) {
        if (ports[i] != NULL) {
            // read port state
            ahci_port_ssts_t ssts = ahci_port_ssts_rd(&ports[i]->port);
            if (ahci_port_ssts_det_extract(ssts) == ahci_port_detect) {
                // we have a device
                ahci_port_speed_prtval(buf, 4096, ahci_port_ssts_spd_extract(ssts));
                AHCID_DEBUG("Device detected! Port: %d Type: %s\n", i, buf);

                // enable all interrupts of this port
                ahci_port_ie_wr(&ports[i]->port, -1);

                // Clear PxSERR
                ahci_port_serr_wr(&ports[i]->port, -1);

                // wait until device is ready
                ahci_port_tfd_pr(buf, 4096, &ports[i]->port);
                AHCID_DEBUG("TFD: \n%s\n", buf);
                AHCID_DEBUG("Waiting for device to become ready\n");
                uint32_t taskfile;
                while (1) {
                    taskfile = ahci_port_tfd_rd(&ports[i]->port);
                    // 7: BSY, 3: DRQ, 0: ERR
                    if ((ahci_port_tfd_sts_extract(taskfile) & 0x89) == 0) {
                        AHCID_DEBUG("Device ready\n");
                        break;
                    }
                }

                // start running commands
                ahci_port_cmd_t cmd = ahci_port_cmd_rd(&ports[i]->port);
                cmd = ahci_port_cmd_st_insert(cmd, 1);
                ahci_port_cmd_wr(&ports[i]->port, cmd);

                // send identify command to device
                do_identify(ports[i]);
            }
        }
    }

    // export ahci management interface
    ahci_mgmt_export(NULL /* state pointer for connect/export callbacks */,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);

#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
    initialized = true;
#endif

}

static void ahci_interrupt_handler(void *arg)
{
    uint32_t hba_irq_state = ahci_hba_is_rd(&controller);

    if (hba_irq_state == 0) {
#ifdef AHCI_SERVICE_DEBUG
        static uint32_t verb_count = 0;
        verb_count++;
        if (verb_count % 1 == 0) {
            AHCID_DEBUG("Ignoring foreign interrupt\n");
            AHCID_DEBUG("Port 0 Interrupt State: %"PRIu32"\n",
                    ahci_port_is_rd(&ports[0]->port));
            AHCID_DEBUG("Port 0 Command Issue state: %"PRIu32"\n",
                    ahci_port_ci_rd(&ports[0]->port));
        }
#endif
        // just ignore foreign interrupts
        return;
    }

    bool interrupted_ports[32] = {false};
    ahci_port_is_t interrupt_state[32] = {0};

    int i = 0;
    for (; i < 32; i++) { // check which ports have irq flag set
        if ((hba_irq_state & (1 << i)) && ports[i]) {
            AHCID_DEBUG("Interrupt for port %d\n", i);

            ahci_port_is_t port_state = ahci_port_is_rd(&ports[i]->port);

            // clear interrupts for port asap. (this is what FreeBSD also does)
            ahci_port_is_wr(&ports[i]->port, port_state);

            // process IDENTIFY response: QEMU sends D2H Register FIS, Spec.
            // says device should send a PIO Setup FIS. We handle both for now :)
            if ((ahci_port_is_dhrs_extract(port_state) == 1 ||
                        ahci_port_is_pss_extract(port_state)) &&
                    ports[i]->status == AHCID_PORT_STATUS_IDENTIFY_PENDING) {
                // we got a d2h register or pio setup fis
                uint8_t *d2hr_fis = ports[i]->receive_fis->vaddr +
                    0x40 /* D2H Register FIS offset in rFIS */;
                uint8_t *pss_fis = ports[i]->receive_fis->vaddr +
                    0x20 /* D2H Register FIS offset in rFIS */;
                uint8_t *fis;
                if (ahci_port_is_dhrs_extract(port_state)) {
                    // QEMU case: QEMU sends a D2H Register FIS as answer to
                    // IDENTIFY Device (0xEC).
                    // This is *NOT* according to AT Attachement 8, Sect. 7.16
                    // IDENTIFY DEVICE which specifies PIO Data-in.
                    // This does not change anything in handling the actual
                    // IDENTIFY data, which is still stored in the indicated
                    // DMA region.
                    fis = d2hr_fis;
                    if (fis[0] != SATA_FIS_TYPE_D2H) {
                        AHCID_DEBUG("Interrupt signalled D2H Reg FIS but the"
                                " FIS has another type (0x%02x)\n", fis[0]);
                        continue; // handle next port
                    }
                }
                else if (ahci_port_is_pss_extract(port_state)) {
                    // ATA-8 compliant case. Real hardware seems to do this.
                    // PIO Setup FIS should mean that we need to receive data
                    // afterwards.
                    // However the AHCI HBA handles that for us and copies the
                    // received data into the DMA region specified in the
                    // IDENTIFY command.
                    fis = pss_fis;
                    if (fis[0] != SATA_FIS_TYPE_PIO) {
                        AHCID_DEBUG("Interrupt signalled PIO Setup FIS but the"
                                " FIS has another type (0x%02x)\n", fis[0]);
                        continue; // handle next port
                    }
                }

                int identify_received = 1;
                if (ports[i]->prdts != NULL && ports[i]->prdt_count == 1) {
                    AHCID_DEBUG("coping data from IDENTIFY result on %d\n", i);
                    memcpy(ports[i]->identify_data, ports[i]->prdts[0]->vaddr, 512);
                    ata_identify_t identify;
                    ata_identify_initialize(&identify,
                            (char *)ports[i]->identify_data);
#ifdef AHCI_SERVICE_DEBUG
                    char buf[32768];
                    ata_identify_pr(buf, 32768, &identify);
                    puts(buf);
#endif
                }
                else {
                    AHCID_DEBUG("PRDTL structures not set up for port %d while"
                            " processing IDENTIFY response.\n", i);
                    identify_received = 0;
                }

                // uninitialize port
                //  clear interrupts
                ahci_port_is_wr(&ports[i]->port, -1);
                //  disable all interrupts of this port
                AHCID_DEBUG("disabling interrupts for %d\n", i);
                ahci_port_ie_wr(&ports[i]->port, 0);
                //  stop running commands
                AHCID_DEBUG("stopping %d\n", i);
                ahci_port_cmd_t cmd = ahci_port_cmd_rd(&ports[i]->port);
                cmd = ahci_port_cmd_st_insert(cmd, 0);
                ahci_port_cmd_wr(&ports[i]->port, cmd);
                // free CL, rFIS
                AHCID_DEBUG("clearing clb and fb for %d\n", i);
                ahci_port_clb_wr(&ports[i]->port, 0);
                ahci_port_fb_wr(&ports[i]->port, 0);
                AHCID_DEBUG("freeing memory for %d\n", i);
                ahci_dma_region_free(ports[i]->command_list);
                ahci_dma_region_free(ports[i]->receive_fis);
                ahci_dma_region_free(ports[i]->prdts[0]);
                free(ports[i]->prdts);
                // add port to skb
                // TODO: true unique port_ids, more info?
                if (identify_received) {
                    skb_add_fact("ahci_device(%d).", i);
                    ports[i]->status = AHCID_PORT_STATUS_IDLE;
                    AHCID_DEBUG("Processing IDENTIFY from port %d complete\n", i);
                }
                else {
                    ports[i]->status = AHCID_PORT_STATUS_UNINITIALIZED;
                }
                continue;
            }
#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
            else if (ports[i]->status == AHCID_PORT_STATUS_IDENTIFY_PENDING) {
                AHCID_DEBUG("received unknown interrupt while waiting for"
                        " IDENTIFY D2H Reg FIS\n");
                char buf[2048];
                ahci_port_is_prtval(buf, 2048, port_state);
                AHCID_DEBUG("Port Status:\n");
                puts(buf);
            }
#endif
            if (ports[i]->binding != NULL) {
                interrupted_ports[i] = true;
                interrupt_state[i] = port_state;
            } else {
                AHCID_DEBUG("no client registered for port %d\n", i);
            }
        }
    }

    // clear interrupts for host controller
    ahci_hba_is_wr(&controller, (uint32_t)-1);

    // deliver messages to clients
    for (i = 0; i < 32; i++) {
        if (!interrupted_ports[i]) continue;

        AHCID_DEBUG("Port %d Interrupt State: 0x%"PRIx32"\n",
                i, interrupt_state[i]);
        AHCID_DEBUG("Port %d Command Issue state: 0x%"PRIx32"\n",
                i, ahci_port_ci_rd(&ports[i]->port));

        ahci_mgmt_command_completed__tx(ports[i]->binding, NOP_CONT,
                i, interrupt_state[i]);
    }
}

static void polling_loop(void)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
#if defined(AHCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
        if (controller.b != NULL) {
            ahci_interrupt_handler(0);
        }
#endif
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}

int main(int argc, char **argv)
{
    int r;

    AHCID_DEBUG("starting\n");

    //connect to the SKB
    AHCID_DEBUG("connecting to the SKB...\n");
    skb_client_connect();
    AHCID_DEBUG("connected.\n");

    r = pci_client_connect();
    assert(err_is_ok(r));
    AHCID_DEBUG("connected to pci\n");

    if (argc >= 3) {
        AHCID_DEBUG("got %s as vendor_id:device_id\n", argv[2]);
        uint64_t vendor_id, device_id;
        vendor_id = strtol(argv[2], NULL, 16);
        device_id = strtol(argv[2]+5, NULL, 16);
        r = pci_register_driver_irq(ahci_init, PCI_CLASS_MASS_STORAGE,
                PCI_SUB_SATA, PCI_DONT_CARE, vendor_id,
                device_id,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                ahci_interrupt_handler, NULL);
        if (err_is_fail(r)) {
            printf("couldn't register device %04"PRIx64":%04"PRIx64": %s\n", vendor_id,
                    device_id, err_getstring(r));
            return 1;
        }
        printf("ahcid: registered device %04"PRIx64":%04"PRIx64"\n", vendor_id, device_id);
    } else {
        // fall-back: try some known AHCI devices
        r = pci_register_driver_irq(ahci_init, PCI_CLASS_MASS_STORAGE,
                PCI_SUB_SATA, PCI_DONT_CARE, PCI_VENDOR_INTEL,
                PCI_DEVICE_ICH9R_82801IR /* QEMU ICH9R AHCI Controller */,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                ahci_interrupt_handler, NULL);
        if (err_is_ok(r)) {
            printf("ahcid: found QEMU ICH9R controller\n");
            goto finish;
        }
        printf("ahcid: did not find QEMU ICH9R controller\n");

        r = pci_register_driver_irq(ahci_init, PCI_CLASS_MASS_STORAGE,
                PCI_SUB_SATA, PCI_DONT_CARE,
                PCI_VENDOR_INTEL,
                PCI_DEVICE_ICH10_82801JI /* 82801JI (ICH10 Family) AHCI Controller */,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                ahci_interrupt_handler, NULL);
        if (err_is_ok(r)) {
            printf("ahcid: found Sun Microsystems ICH10 Family controller\n");
            goto finish;
        }
        printf("ahcid: did not find Sun Microsystems ICH10 Family controller\n");

        r = pci_register_driver_irq(ahci_init, PCI_CLASS_MASS_STORAGE,
                PCI_SUB_SATA, PCI_DONT_CARE,
                PCI_VENDOR_ATI,
                PCI_DEVICE_SB7x0 /* ATI SB7x0/SB8x0/SB9x0 SATA controller (IDE mode) */,
                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                ahci_interrupt_handler, NULL);
        if (err_is_ok(r)) {
            printf("ahcid: found ATI Technologies Inc. SB7x0/8x0/9x0 controller\n");
            goto finish;
        }
        printf("ahcid: did not find ATI Technologies Inc. SB7x0/8x0/9x0 controller\n");
        printf("ahcid: \ndid not find any supported AHCI controller\naborting...");
        return 1;
    }

finish:
    AHCID_DEBUG("registered driver: retval=%d\n", r);

    polling_loop();
}
