#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>

#include <usb/usb.h>

#include <if/usb_driver_defs.h>
#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb_controller.h>
#include <usb_request.h>
#include <usb_device.h>
#include <usb_transfer.h>
#include <usb_driver.h>

/*
 * ========================================================================
 * Flounder callbacks and service connect handling
 * ========================================================================
 */

/**
 * struct representing the state of a new USB driver connection
 */
struct usb_manager_connect_state {
    struct usb_manager_binding *b;      ///< the usb_manager_binding struct
    struct usb_driver_binding *driver;  ///< the usb drivers service
    void *desc;                         ///< configuration descriptor
    uint32_t length;                    ///< length of the descirptor
    usb_error_t error;                  ///< the outcome of the initial setup
    iref_t driver_iref;                 ///< the drivers iref
};

/**
 * \brief callback for USB driver binding
 *
 * \param st the supplied state
 * \param err the outcome of the binding process
 * \param b the driver binding
 *
 * This function is the last step in the setup procedure and frees up the state
 */
static void usb_driver_bind_cb(void *st, errval_t err,
        struct usb_driver_binding *b)
{
    USB_DEBUG_IDC("usb_driver_bind_cb\n");

    if (err_is_fail(err)) {
        USB_DEBUG("driver binding failed..\n");
    }

    struct usb_manager_connect_state *cs = st;

    cs->driver = b;
    struct usb_device *dev = cs->b->st;
    dev->usb_driver_binding = b;

    free(cs->desc);
    free(cs);
}

/*
 * \brief callback for successful sent replies to the connect rpc
 *
 * \param a the state of the connection call
 *
 * This function binds to the USB driver iref
 */
static void usb_driver_connect_cb(void *a)
{
    USB_DEBUG_IDC("usb_driver_connect_cb->binding...\n");
    struct usb_manager_connect_state *st = a;
    errval_t err = usb_driver_bind(st->driver_iref, usb_driver_bind_cb, st,
            get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "usb driver bind failed");
        usb_device_free(st->b->st, 0);
        free(st->desc);
        free(st);
    }
}

/*
 * \brief sends the response to the connect rpc
 *
 * \param a the connection state
 */
static void usb_driver_connect_response(void *a)
{
    errval_t err;
    struct usb_manager_connect_state *st = a;

    struct event_closure txcont = MKCONT(usb_driver_connect_cb, st);

    err = usb_manager_connect_response__tx(st->b, txcont, st->error, st->desc,
            st->length);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // try to resend
            txcont = MKCONT(usb_driver_connect_response, st);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to register send");
            }
        } else {
            // error
            DEBUG_ERR(err, "error while seniding driver connect response");
            /* free the device */
            usb_device_free(st->b->st, 0);
            free(st->desc);
            free(st);
        }
    }
}

/**
 * \brief this function handles connections of new USB device driver processes
 *
 * \param bind  the binding which we received the connect cal
 * \param driver_iref the iref of the usb driver
 * \param init_config the initial configuration to set the device
 *
 * This function associates the USB device with an flounder binding. Further
 * the usb manager connects to the usb drivers service for notifications
 */
static void usb_rx_connect_call(struct usb_manager_binding *bind,
        iref_t driver_iref, uint16_t init_config)
{
    struct usb_manager_connect_state *st;

    st = malloc(sizeof(struct usb_manager_connect_state));

    if (st == NULL) {
        USER_PANIC("cannot reply, out of memory!");
    }

    st->b = bind;
    st->driver_iref = driver_iref;

    // associate the bindings with the usb device
    usb_driver_connected(bind, st->driver, init_config);

    if (bind->st == NULL) {
        /* if the connection fails, there will not be an association */
        debug_printf("ERROR: no state associated..\n");
        st->error = USB_ERR_IOERROR;
        usb_driver_connect_response(st);
        return;
    }

    /*
     * all went fine so the binding state pointer is now refering to the
     * usb device and we can setup the reply
     */

    struct usb_device *dev = bind->st;

    /* we reply with the initial configuration descriptor */
    st->length = sizeof((dev->device_desc)) + dev->config_desc_size;
    st->desc = malloc(st->length);

    memcpy(st->desc, &(dev->device_desc), sizeof((dev->device_desc)));
    memcpy(st->desc + sizeof((dev->device_desc)), dev->config_desc,
            dev->config_desc_size);

    st->error = USB_ERR_OK;

    // send response
    usb_driver_connect_response(st);
}

/// the receive function handles
static struct usb_manager_rx_vtbl usb_manager_handle_fn = {
    .request_read_call = usb_rx_request_read_call,
    .request_write_call = usb_rx_request_write_call,
    .request_call = usb_rx_request_call,
    .connect_call = usb_rx_connect_call,
    .transfer_setup_call = usb_rx_transfer_setup_call,
    .transfer_unsetup_call = usb_rx_transfer_unsetup_call,
    .transfer_start_call = usb_rx_transfer_start_call,
    .transfer_stop_call = usb_rx_transfer_stop_call,
    .transfer_status_call = usb_rx_transfer_status_call,
    .transfer_state_call = usb_rx_transfer_state_call,
    .transfer_clear_stall_call = usb_rx_transfer_clear_stall_call,
};

/**
 * \brief this function sets the receive handlers for a newly connected
 *        USB driver process
 *
 * \param st the state (currently NULL)
 * \param b  the binding of the new connection
 */
static errval_t service_connected_cb(void *st, struct usb_manager_binding *b)
{
    USB_DEBUG_IDC("service_connected_cb(): Setting handler functions.\n");

    b->rx_vtbl = usb_manager_handle_fn;

    return (SYS_ERR_OK);
}

/// state variable for the usb manager service
static volatile uint8_t usb_manager_service_exported = 0;

/**
 * \brief call back function for the export of the USB manager service
 *
 * \param st   the supplied state (currently NULL)
 * \param err  the outcome of the service export
 * \param iref the iref which the service is associated with
 *
 * NOTE: the usb manager blocks untill the service is exported and the
 *       and registered with the name service.
 */
static void service_exported_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "service export failed.");
    }

    err = nameservice_register(USB_MANAGER_SERVICE, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "registration with name server failed");
    }

    usb_manager_service_exported = 1;

}

#if 0
/**
 * \brief this function maps the supplied device capability in our memory
 *
 * The capability is expected to be in the argcn slot of the rootcn. The
 * spawning domain has to ensure that the needed capability is at the right
 * location.
 *
 * XXX: Maybe it would be better to move the caps into the inheritcn slot
 *      to support one device capability per host controller.
 */
static uintptr_t map_device_cap(void)
{
    errval_t err;

    struct capref dev_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity frameid;

    err = invoke_frame_identify(dev_cap, &frameid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame.\n");
        return (0);
    }

    void *ret_addr = NULL;
    size_t size = (1UL << frameid.bits); /* bytes */

    err = vspace_map_one_frame_attr(&ret_addr, size, dev_cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to create a vspace mapping.\n");
        return (0);
    }

    return ((uintptr_t) ret_addr);
}
#endif
/*
 * =========================================================================
 * ARM and PandaBoard specific functions
 * =========================================================================
 */
#if __pandaboard__

// the offset into the supplied capability
#define USB_CAPABILITY_OFFSET (0x00000C00)

// the EHCI interrupt number on the pandaboard
#define USB_ARM_EHCI_IRQ 109

/**
 * \brief this is a quick check function if everything is all right
 *
 * NOTE: there is just one specific setting possible on the PandaBoard
 *       EHCI controller, with the specific capability and the specific offset
 *       This function checks if these values match to ensure functionality
 *       If you change something with the startup of the USB manager domain
 *       you may need to change the values in this function!
 */
static usb_error_t pandaboard_checkup(uintptr_t base, int argc, char *argv[])
{
    USB_DEBUG("performing pandaboard integrity check.\n");

    /* checking the host controller type */
    if (strcmp(argv[0], "ehci")) {
        debug_printf("wrong host controller type: %s\n", argv[0]);
        return (USB_ERR_INVAL);
    }

    /* checking the memory offset */
    if (strtoul(argv[1], NULL, 10) != ((uint32_t) USB_CAPABILITY_OFFSET)) {
        debug_printf("wrong offset!: %x (%s)\n", strtoul(argv[1], NULL, 10),
                argv[1]);
        return (USB_ERR_INVAL);
    }

    /* checking the IRQ number */
    if (strtoul(argv[2], NULL, 10) != USB_ARM_EHCI_IRQ) {
        debug_printf("wrong interrupt number: %s, %x", argv[2],
                strtoul(argv[2], NULL, 10));
        return (USB_ERR_INVAL);
    }

    /*
     * here we read some values from the ULPI register of the PandaBoards
     * additional ULPI interface on the EHCI controller.
     *
     * The request are forwarded to the external ULPI receiver on the
     * PandaBoard.
     *
     * NOTE: Not every EHCI controller has those register!
     */

    uint32_t tmp = USB_CAPABILITY_OFFSET + (uint32_t) base;
    printf("address of ehci base = %p\n", tmp);

    /*
     * This request reads the debug register of the ULPI receiver. The values
     * returned are the line state. If the returned value is 0x1 this means
     * there is connection i.e. the USB hub on the PandaBoard is reachable.
     */
    *((volatile uint32_t*) (tmp + 0x00A4)) = (uint32_t) ((0x15 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));

    /* wait till the request is done */
    while (*((volatile uint32_t*) (tmp + 0x00A4)) & (1 << 31)) {
        printf("%c", 0xE);

    }

    /* compare the result */
    if (!(*(((volatile uint32_t*) (tmp + 0x00A4))) & 0x1)) {
        return (USB_ERR_INVAL);
    }

    /*
     * This request reads out the low part of the vendor id from the ULPI
     * receiver on the PandaBoard. This should be 0x24.
     *
     * XXX: Assuming that all the Pandaboards have the same ULPI receiver
     */
    *((volatile uint32_t*) (tmp + 0x00A4)) = (uint32_t) ((0x00 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));

    /* wait till request is done */
    while (*((volatile uint32_t*) (tmp + 0x00A4)) & (1 << 31)) {
        printf("%c", 0xE);
    }

    /* compare the values */
    if (0x24 != ((*((volatile uint32_t*) (tmp + 0x00A4))) & 0xFF)) {
        return (USB_ERR_INVAL);
    }

    return (USB_ERR_OK);
}

#endif /* __arm__ */

/*
 * ========================================================================
 * MAIN
 * ========================================================================
 */

/*
 * \brief   main function of the usb manager
 *
 * The USB manager must be called with very the necessary arguments and supplied
 * with the needed capability to access the device registers.
 *
 * On x86:
 * On ARM:
 */
int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("USB Manager started.\n");

    /* starting the usb manager service */
    err = usb_manager_export(NULL, service_exported_cb, service_connected_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to start the usb manager service.");
        return (err);
    }

    /* wait till the service is exported */
    while (!usb_manager_service_exported) {
        event_dispatch(get_default_waitset());
    }

    /* map de device capability into our address space */
    uintptr_t base; // = map_device_cap();
    // TODO: Change when new API is ready!
    err = map_device_register(0x4A064000, 0x1000, &base);
    assert(err_is_ok(err));

    if (base == 0) {
        USER_PANIC("failed to map the device capability");
    }

    /* the default tuple size is 2, since on x86 the interrupts can be routed */
    uint8_t arg_tuple_size = 2;

#if __arm__

    /* ARM / PandaBoard related setup and checks */

    if (pandaboard_checkup(base, argc, argv) != USB_ERR_OK) {
        USER_PANIC("Pandaboard checkup failed!\n");
    }

    /*
     * the argument tuple size must be 3, i.e. the host usb manager expects
     * [host-controller offset interrupt] as arguments, because the interrupts
     * are fixed and cannot be allocated as we like.
     */
    arg_tuple_size = 3;

    /* checking the command line parameter count */
    if (argc != 3) {
        debug_printf("Usage: usb_manager [host-controller offset interrupt]\n");
    }

    uint32_t irq = strtoul(argv[2], NULL, 10);

    /*
     * setting up interrupt handler for the EHCI interrupt
     * XXX: this should be done for each host controller eventually...
     */
    err = inthandler_setup_arm(usb_hc_intr_handler, NULL, irq);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to enable interrupt. Step 16.\n");
    }

#else
    if (argc == 0 || argc % 2) {
        debug_printf("Usage: usb_manager [host-controller offset]\n");
    }
    uint32_t intr_vector;
    err = inthandler_setup(usb_hc_intr_handler, NULL,
            uint32_t &intr_vector);
    /* TODO: register interrupt routing.. */
#endif

    usb_error_t uerr = USB_ERR_INVAL;

    /*
     * start initializing the host controllers supplied by the arguments
     * XXX: Currently just one
     */
    for (uint16_t i = 0; i < argc; i += arg_tuple_size) {

        /* allocate the general host controller */
        uerr = USB_ERR_INVAL;
        usb_host_controller_t *hc = malloc(sizeof(*hc));
        memset(hc, 0, sizeof(*hc));

        uintptr_t controller_base = base + strtoul(argv[i + 1], NULL, 10);

        /* -------------------------------------------------------------------
         * EHCI Controller
         * -------------------------------------------------------------------
         */
        if (strcmp(argv[i], "ehci") == 0) {
            uerr = usb_hc_init(hc, USB_EHCI, controller_base);
        }

        /* -------------------------------------------------------------------
         * OHCI Controller
         * -------------------------------------------------------------------
         */
        if (strcmp(argv[i], "ohci") == 0) {
            uerr = usb_hc_init(hc, USB_OHCI, controller_base);
        }

        /* -------------------------------------------------------------------
         * UHCI Controller
         * -------------------------------------------------------------------
         */
        if (strcmp(argv[i], "uhci") == 0) {
            uerr = usb_hc_init(hc, USB_UHCI, controller_base);
        }

        /* -------------------------------------------------------------------
         * XHCI Controller
         * -------------------------------------------------------------------
         */
        if (strcmp(argv[i], "xhci") == 0) {
            uerr = usb_hc_init(hc, USB_XHCI, controller_base);
        }

        if (uerr != USB_ERR_OK && hc != NULL) {
            free(hc);
            continue;
        }
    }

    messages_handler_loop();
}
