/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define UHCI_SERVICE_DEBUG 1
#define NO_PCI_IO_HANDLING 1

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <pci/pci.h>
#include <dev/uhci_dev.h>
#include <uhci_debug.h>
#include <string.h>



// #define PCI_DEVICE_82731_UHCI 0x7112
#define PCI_DEVICE_82731_UHCI 0x7020

#define UHCI_PORTBASE 0xc020
#define UHCI_PORTEND 0xc03f
#define UHCI_IRQ 9

static const uint16_t device_ids[] = {
    0x7020,
    0x7112, 
    0x0000 // Sentinel
};

static struct device_mem bar;
static struct uhci_t   dev;

static void uhci_legacy_init(void)
{
    uint16_t portbase = UHCI_PORTBASE; // Why isn't this from a BAR

    UHCI_DEBUG("accessing conf regs starting at %" PRIx16 "\n", portbase);
    uhci_initialize(&dev, portbase);

    {
	char buf[4096];
	uhci_pr(buf, 4095, &dev);
	printf("%s", buf);
    }

    // Now initialize the controller.
    

}



static void uhci_init(struct device_mem *bar_info, int nr_allocated_bars)
{
    int i;
    uint16_t portbase;

    UHCI_DEBUG("nr_allocated_bars = %d\n", nr_allocated_bars);
    for(i=0; i < nr_allocated_bars; i++) {
	struct device_mem *b = &bar_info[i];
	UHCI_DEBUG("Bar %d: type %d\n", i, b->type);
	UHCI_DEBUG(" vaddr=%p, paddr=%"PRIx64"\n", b->vaddr, b->paddr);
    }

    if (nr_allocated_bars != 1) { 
	UHCI_DEBUG("Warning: more than 1 bar (ignoring others)\n");
    }
    memcpy(&bar, &bar_info[0], sizeof(&bar));

    portbase = UHCI_PORTBASE; // Why isn't this from a BAR

    UHCI_DEBUG("accessing conf regs starting at %" PRIx16 "\n", portbase);
    uhci_initialize(&dev, portbase);

    {
	char buf[4096];
	uhci_pr(buf, 4095, &dev);
	printf("%s", buf);
    }

    // Now initialize the controller.
    

}

static void uhci_interrupt_handler(void *arg)
{
    UHCI_DEBUG("interrupt!\n");
}

int main(int argc, char **argv)
{
    int r;

    UHCI_DEBUG("starting\n");

    // UHCI_DEBUG("connecting to the SKB...\n");
    // skb_client_connect();
    // UHCI_DEBUG("connected to SKB\n");

    UHCI_DEBUG("connecting to PCI...\n");
    r = pci_client_connect();
    assert(err_is_ok(r));
    UHCI_DEBUG("connected to PCI\n");

    r = pci_register_legacy_driver_irq(uhci_legacy_init, 
				       UHCI_PORTBASE,
				       UHCI_PORTEND,
				       UHCI_IRQ, 
				       uhci_interrupt_handler,
				       NULL );
    if (err_is_ok(r)) {
	UHCI_DEBUG("uhci[legacy]: found controller.");
	goto finish;
    }

    int i;
    for( i=0; device_ids[i] != 0; i++) {
	r = pci_register_driver_irq(uhci_init,	// Init. fn
				    PCI_CLASS_SERIAL,	// Class
				    PCI_SUB_USB,	// Subclass
				    PCI_IF_USB_UHCI,	// Prog. if
				    PCI_VENDOR_INTEL,	// Vendor ID
				    device_ids[i],	// Device ID
				    PCI_DONT_CARE,	// Bus
				    PCI_DONT_CARE,	// Dev
				    PCI_DONT_CARE,	// Fun
				    uhci_interrupt_handler,	// IRQ handler
				    NULL		// IRQ argument
				    );
	if (err_is_ok(r)) {
	    UHCI_DEBUG("uhci: found controller %" PRIx16 ":%" PRIx16 ".\n", 
		       PCI_VENDOR_INTEL, device_ids[i]);
	    goto finish;
	}
	printf("uhci: no device ID %" PRIx16 " found.\n", device_ids[i]);
    }

    printf("uhci: no devices recognized; giving up.");
    return 1;

finish:
    UHCI_DEBUG("registered driver: retval=%d; now polling...\n", r);

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}
