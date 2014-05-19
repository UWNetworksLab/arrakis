#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#ifdef __arm__
#include <if/monitor_blocking_rpcclient_defs.h>
#endif

#include "kaluga.h"

extern char **environ;

#ifdef __x86__
errval_t default_start_function(coreid_t where, struct module_info* mi,
        char* record)
{
    assert(mi != NULL);
    errval_t err = SYS_ERR_OK;

    if (is_started(mi)) {
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    if (!is_auto_driver(mi)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    // Construct additional command line arguments containing pci-id.
    // We need one extra entry for the new argument.
    uint64_t vendor_id, device_id;
    char **argv = mi->argv;
    bool cleanup = false;
    err = oct_read(record, "_ { vendor: %d, device_id: %d }",
            &vendor_id, &device_id);
    if (err_is_ok(err)) {
        // We assume that we're starting a device if the query above succeeds
        // and therefore append the pci vendor and device id to the argument
        // list.
        argv = malloc((mi->argc+1) * sizeof(char *));
        memcpy(argv, mi->argv, mi->argc * sizeof(char *));
        char *pci_id  = malloc(10);
        // Make sure pci vendor and device id fit into our argument
        assert(vendor_id < 0x9999 && device_id < 0x9999);
        snprintf(pci_id, 10, "%04"PRIx64":%04"PRIx64, vendor_id, device_id);
        argv[mi->argc] = pci_id;
        mi->argc += 1;
        argv[mi->argc] = NULL;
        cleanup = true;
    }
    err = spawn_program(where, mi->path, argv,
            environ, 0, &mi->did);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }
    if (cleanup) {
        // alloc'd string is the last of our array
        free(argv[mi->argc-1]);
        free(argv);
    }

    return err;
}
#endif

errval_t start_networking(coreid_t core, struct module_info* driver,
        char* record)
{
    assert(driver != NULL);
    errval_t err = SYS_ERR_OK;

    if (is_started(driver)) {
        return KALUGA_ERR_DRIVER_ALREADY_STARTED;
    }

    if (!is_auto_driver(driver)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    struct module_info* netd = find_module("netd");
    if (netd == NULL || !is_auto_driver(netd)) {
        KALUGA_DEBUG("netd not found or not declared as auto.");
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    struct module_info* ngd_mng = find_module("NGD_mng");
    if (ngd_mng == NULL || !is_auto_driver(ngd_mng)) {
        KALUGA_DEBUG("NGD_mng not found or not declared as auto.");
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    err = spawn_program(core, driver->path, driver->argv + 1, environ, 0,
            &driver->did);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", driver->path);
        return err;
    }

    // XXX: Manually add cardname (overwrite first (auto) argument)
    // +Weird convention, e1000n binary but cardname=e1000
    char* cardname =
            strcmp(driver->binary, "e1000n") == 0 ? "e1000" : driver->binary;

    size_t name_len = strlen("cardname=") + strlen(cardname) + 1;
    char* card_argument = malloc(name_len);
    sprintf(card_argument, "cardname=%s", cardname);
    printf("############# starting network with argiments %s\n", card_argument);

    // Spawn netd and ngd_mng
    netd->argv[0] = card_argument;
    err = spawn_program(core, netd->path, netd->argv, environ, 0, &netd->did);

    ngd_mng->argv[0] = card_argument;
    err = spawn_program(core, ngd_mng->path, ngd_mng->argv, environ, 0,
            &ngd_mng->did);

    free(card_argument);
    return err;
}

/* errval_t start_usb_manager(void) */
/* { */

/*     struct module_info* driver = find_module("usb_manager"); */
/*     if (driver == NULL || !is_auto_driver(driver)) { */
/*         KALUGA_DEBUG("NGD_mng not found or not declared as auto."); */
/*         return KALUGA_ERR_DRIVER_NOT_AUTO; */
/*     } */

/*     debug_printf("doing pandaboard related setup...\n"); */
/*     errval_t err; */

/*     struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client(); */
/*     assert(cl != NULL); */

/*     // Request I/O Cap */
/*     struct capref requested_caps; */
/*     errval_t error_code; */

/*     err = cl->vtbl.get_io_cap(cl, &requested_caps, &error_code); */
/*     assert(err_is_ok(err) && err_is_ok(error_code)); */

/*     // Copy into correct slot */

/*     struct capref device_range_cap = NULL_CAP; */

/*     err = slot_alloc(&device_range_cap); */
/*     if (err_is_fail(err)) { */
/*         printf("slot alloc failed. Step 1\n"); */
/*         return (err); */
/*     } */
/*     struct capref tiler_cap = NULL_CAP; */

/*     err = slot_alloc(&tiler_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 1\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(device_range_cap, requested_caps, ObjType_DevFrame, 29); */

/*     struct capref l3_ocm_ram = NULL_CAP; */

/*     err = slot_alloc(&l3_ocm_ram); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 2\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(l3_ocm_ram, device_range_cap, ObjType_DevFrame, 26); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "failed to retype the dev cap. Step 3\n"); */
/*         return (err); */
/*     } */

/*     struct capref l3_config_registers_cap; */
/*     err = slot_alloc(&l3_config_registers_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot alloc failed. Step 4\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_domains_cap; */
/*     err = slot_alloc(&l4_domains_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 5\n"); */
/*         return (err); */
/*     } */

/*     struct capref emif_registers_cap; */
/*     err = slot_alloc(&emif_registers_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 6\n"); */
/*         return (err); */
/*     } */

/*     struct capref gpmc_iss_cap; */
/*     err = slot_alloc(&gpmc_iss_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 7\n"); */
/*         return (err); */
/*     } */

/*     struct capref l3_emu_m3_sgx_cap; */
/*     err = slot_alloc(&l3_emu_m3_sgx_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 8\n"); */
/*         return (err); */
/*     } */

/*     struct capref display_iva_cap; */
/*     err = slot_alloc(&display_iva_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 9\n"); */
/*         return (err); */
/*     } */
/*     struct capref tmp_cap = display_iva_cap; */
/*     tmp_cap.slot++; */
/*     cap_delete(tmp_cap); */

/*     struct capref l4_PER_domain_cap; */
/*     err = slot_alloc(&l4_PER_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 12\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_ABE_domain_cap; */
/*     err = slot_alloc(&l4_ABE_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 11\n"); */
/*         return (err); */
/*     } */

/*     struct capref l4_CFG_domain_cap; */
/*     err = slot_alloc(&l4_CFG_domain_cap); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "slot_alloc failed. Step 12\n"); */
/*         return (err); */
/*     } */

/*     err = cap_retype(l4_PER_domain_cap, l4_domains_cap, ObjType_DevFrame, 24); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "failed to retype the cap. Step 13\n"); */
/*         return (err); */
/*     } */
/*     tmp_cap = l4_CFG_domain_cap; */
/*     tmp_cap.slot++; */
/*     cap_delete(tmp_cap); */

/*     struct frame_identity frameid;  // = {        0,        0    }; */

/*     err = invoke_frame_identify(l4_CFG_domain_cap, &frameid); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "could not identify the frame. Step 14\n"); */
/*     } */

/*     // get the 32 bit */
/*     uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base)); */
/*     uint32_t size2 = frameid.bits; */

/*     /\* the L4 CFG domain cap must have address 0x4A000000 *\/ */
/*     assert(last == 0x4a000000); */

/*     /\* the size of the L4 CFG domain is 16k *\/ */
/*     assert(((1 << size2) / 1024) == (16 * 1024)); */

/* #define USB_SUBSYSTEM_L4_OFFSET 0x00062000 */
/* //#define USB_OHCI_OFFSET         (0x000A9000-USB_SUBSYSTEM_L4_OFFSET) */
/* #define USB_OHCI_OFFSET         0x00002800 */
/* #define USB_EHCI_OFFSET         0x00002C00 */

/* #define USB_ARM_EHCI_IRQ 109 */

/*     uint32_t tmp = (uint32_t) USB_EHCI_OFFSET + USB_SUBSYSTEM_L4_OFFSET; */

/*     char buf[255]; */
/*     uint8_t offset = 0; */
/*     driver->cmdargs = buf; */
/*     driver->argc = 3; */
/*     driver->argv[0] = driver->cmdargs+0; */

/*     snprintf(buf+offset, 255-offset, "ehci\0"); */
/*     offset += strlen(driver->argv[0])+1; */
/*     driver->argv[1] = driver->cmdargs+offset; */
/*     snprintf(buf+offset, 255-offset, "%u\0", tmp); */
/*     offset += strlen(driver->argv[1])+1; */
/*             driver->argv[2] = driver->cmdargs+offset; */
/*     snprintf(buf+offset, 255-offset, "%u\0", USB_ARM_EHCI_IRQ); */

/*     err = SYS_ERR_OK; */
/*     coreid_t core = 0; */

/*     if (is_started(driver)) { */
/*         debug_printf("driver is already started..."); */
/*         return KALUGA_ERR_DRIVER_ALREADY_STARTED; */
/*     } */


/*     err = spawn_program_with_caps(core, driver->path, driver->argv, environ, */
/*             NULL_CAP, l4_CFG_domain_cap, 0, &driver->did); */
/*     if (err_is_fail(err)) { */
/*         DEBUG_ERR(err, "Spawning %s failed.", driver->path); */
/*         return err; */
/*     } */

/*     return err; */
/* } */
