#include <barrelfish/barrelfish.h>
#include "serial.h"
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>

#include <dev/omap/omap44xx_uart3_dev.h>
#include <arch/arm/omap44xx/device_registers.h>

//
// Serial console and debugger interfaces
//
#define NUM_PORTS 4

#define UART_IRQ (32+74)


unsigned int serial_console_port = 2;
unsigned int serial_debug_port = 2;
unsigned const int serial_num_physical_ports = NUM_PORTS;


lpaddr_t uart_base[NUM_PORTS] = {
    OMAP44XX_MAP_L4_PER_UART1,
    OMAP44XX_MAP_L4_PER_UART2,
    OMAP44XX_MAP_L4_PER_UART3,
    OMAP44XX_MAP_L4_PER_UART4
};

size_t uart_size[NUM_PORTS] = {
    OMAP44XX_MAP_L4_PER_UART1_SIZE,
    OMAP44XX_MAP_L4_PER_UART2_SIZE,
    OMAP44XX_MAP_L4_PER_UART3_SIZE,
    OMAP44XX_MAP_L4_PER_UART4_SIZE
};


static omap44xx_uart3_t ports[NUM_PORTS];
static bool uart_initialized[NUM_PORTS];

static void serial_poll(omap44xx_uart3_t *uart)
{
    // Read while we can
    while(omap44xx_uart3_lsr_rx_fifo_e_rdf(uart)) {
        char c = omap44xx_uart3_rhr_rhr_rdf(uart);
        serial_input(&c, 1);
    }
}

static void serial_interrupt(void *arg)
{
    // We better be initialized!
    assert(uart_initialized[serial_console_port]);
    // XXX: this is probably not correct for all ports!
    uint16_t port = *(uint16_t*)arg;
    omap44xx_uart3_t *uart = &ports[port];

    // get type
    omap44xx_uart3_iir_t iir =
        omap44xx_uart3_iir_rd(uart);

    if (omap44xx_uart3_iir_it_pending_extract(iir) == 0) {
        omap44xx_uart3_it_type_status_t it_type = omap44xx_uart3_iir_it_type_extract(iir);
        switch(it_type) {
            case omap44xx_uart3_it_modem:
                (void) omap44xx_uart3_msr_rd(uart);
                break;
            case omap44xx_uart3_it_rxtimeout:
            case omap44xx_uart3_it_rhr:
                serial_poll(uart);
                break;
            default:
                debug_printf("serial_interrupt: unhandled irq: %d\n", it_type);
                break;
        }
    }
}


static bool convert_rx_simple(uint8_t *trig)
{
    switch(*trig) {
        case 8:
            *trig = 0;
            return true;
        case 16:
            *trig = 1;
            return true;
        case 56:
            *trig = 2;
            return true;
        case 60:
            *trig = 3;
            return true;
        default:
            return false;
    }
}
static bool convert_tx_simple(uint8_t *trig)
{
    switch(*trig) {
        case 8:
            *trig = 0;
            return true;
        case 16:
            *trig = 1;
            return true;
        case 32:
            *trig = 2;
            return true;
        case 56:
            *trig = 3;
            return true;
        default:
            return false;
    }
}

/*
 * Initialzie OMAP UART with interrupt
 * UART TRM 23.3
 */
static void omap44xx_uart3_init(omap44xx_uart3_t *uart, lvaddr_t base)
{
    // XXX: test this with other values
    // rx and tx FIFO threshold values (1 -- 63)
    uint8_t rx_trig = 1; // amount of characters in fifo
    uint8_t tx_trig = 63; // amount of free spaces in fifo
    bool need_rx_1b = convert_rx_simple(&rx_trig);
    bool need_tx_1b = convert_tx_simple(&tx_trig);

    omap44xx_uart3_initialize(uart, (mackerel_addr_t) base);
    // do soft reset -- not the best idea if we rely on the same UART for
    // debug output
    //omap44xx_uart3_sysc_softreset_wrf(uart, 0x1);
    //while (!omap44xx_uart3_syss_resetdone_rdf(uart)); // poll for reset completion

    // configure FIFOs according to TRM (section 25.3.5.1.1.2)
    //1 switch to config mode B (access to efr register): set lcr to 0xbf
    omap44xx_uart3_lcr_t old_lcr = omap44xx_uart3_lcr_rd(uart);
    omap44xx_uart3_lcr_wr(uart, 0xbf);
    // 1.1 disable baud clock so we can write to FCR[0] and FCR[3]
    omap44xx_uart3_dll_clock_lsb_wrf(uart, 0x0);
    omap44xx_uart3_dlh_clock_msb_wrf(uart, 0x0);
    //2 enable register submode tlr to access tlr register
    omap44xx_uart3_enhanced_en_status_t old_enhanced_en =
        omap44xx_uart3_efr_enhanced_en_rdf(uart);
    omap44xx_uart3_efr_enhanced_en_wrf(uart, 1);
    //3 switch to config mode A (access to mcr register): set lcr to: 0x80
    omap44xx_uart3_lcr_wr(uart, 0x80);
    //4 enable register submode tlr to access tlr register
    omap44xx_uart3_tcr_tlr_status_t old_tcr_tlr =
        omap44xx_uart3_mcr_tcr_tlr_rdf(uart);
    omap44xx_uart3_mcr_tcr_tlr_wrf(uart, 1);
    //5 enable FIFO, load FIFO triggers, load DMA mode (part1)
    omap44xx_uart3_fcr_t fcr = omap44xx_uart3_fcr_default;
    // set trigger lvls of rx and tx FIFOs (defined above)
    fcr = omap44xx_uart3_fcr_rx_fifo_trig_insert(fcr, rx_trig&0x3);
    fcr = omap44xx_uart3_fcr_tx_fifo_trig_insert(fcr, tx_trig&0x3);
    fcr = omap44xx_uart3_fcr_dma_mode_insert(fcr, 0); // no DMA
    fcr = omap44xx_uart3_fcr_fifo_en_insert(fcr, 0); // enable FIFOs
    omap44xx_uart3_fcr_wr(uart, fcr);
    //6 switch to config mode B
    omap44xx_uart3_lcr_wr(uart, 0xbf);
    //7 load FIFO triggers (DMA only?, part2)
    omap44xx_uart3_tlr_t tlr = omap44xx_uart3_tlr_default;
    omap44xx_uart3_tlr_rx_fifo_trig_dma_insert(tlr, rx_trig>>2);
    omap44xx_uart3_tlr_tx_fifo_trig_dma_insert(tlr, tx_trig>>2);
    omap44xx_uart3_tlr_wr(uart, tlr);
    //8 load new FIFO triggers & new DMA mode (part3)
    omap44xx_uart3_scr_t scr = omap44xx_uart3_scr_default;
    // make FIFO trigger levels byte granularity
    // --> lvl = { *_fifo_trig_dma : *_fifo_trig }
    scr = omap44xx_uart3_scr_rx_trig_granu1_insert(scr, need_rx_1b);
    scr = omap44xx_uart3_scr_tx_trig_granu1_insert(scr, need_tx_1b);
    scr = omap44xx_uart3_scr_dma_mode_2_insert(scr, 0); // no DMA
    scr = omap44xx_uart3_scr_dma_mode_ctl_insert(scr, 0); // shouldn't matter w/ DMA off
    omap44xx_uart3_scr_wr(uart, scr);
    //8b clear fifo queues
    omap44xx_uart3_fcr_rx_fifo_clear_wrf(uart, 1);
    omap44xx_uart3_fcr_tx_fifo_clear_wrf(uart, 1);
    //9 restore enhanced_en
    omap44xx_uart3_efr_enhanced_en_wrf(uart, old_enhanced_en);
    //10 switch to config mode A
    omap44xx_uart3_lcr_wr(uart, 0x80);
    //11 restore TCR_TLR
    omap44xx_uart3_mcr_tcr_tlr_wrf(uart, old_tcr_tlr);
    //12 restore LCR
    omap44xx_uart3_lcr_wr(uart, old_lcr);

    // configure protocol, baud rate and irq according to trm (section
    // 23.3.5.1.1.3)
    //1 disable UART access to DLL and DLH regs
    omap44xx_uart3_mdr1_mode_select_wrf(uart, 0x7);
    //2 switch to register config mode B
    omap44xx_uart3_lcr_wr(uart, 0xbf);
    //3 enable access to IER bit field
    old_enhanced_en = omap44xx_uart3_efr_enhanced_en_rdf(uart);
    omap44xx_uart3_efr_enhanced_en_wrf(uart, 1);
    //4 switch to reg operational mode to access IER register
    omap44xx_uart3_lcr_wr(uart, 0);
    //5 clear IER register
    omap44xx_uart3_ier_wr(uart, 0);
    //6 config mode B
    omap44xx_uart3_lcr_wr(uart, 0xbf);
    //7 new divisor value --> 115200 baud == 0x00, 0x1A (dlh, dll)
    omap44xx_uart3_dll_clock_lsb_wrf(uart, 0x1a);
    omap44xx_uart3_dlh_clock_msb_wrf(uart, 0x0);
    //8 register operational mode
    omap44xx_uart3_lcr_wr(uart, 0);
    //9 load irq config --> only rhr irq for now
    omap44xx_uart3_ier_rhr_it_wrf(uart, 1);
    //10 register config mode B
    omap44xx_uart3_lcr_wr(uart, 0xbf);
    //11 restore efr.enhanced_en
    omap44xx_uart3_efr_enhanced_en_wrf(uart, old_enhanced_en);
    //12 load protocol formatting --> 8N1
    omap44xx_uart3_lcr_t lcr = omap44xx_uart3_lcr_default;
    lcr = omap44xx_uart3_lcr_parity_en_insert(lcr, 0);       // No parity
    lcr = omap44xx_uart3_lcr_nb_stop_insert(lcr, 0);         // 1 stop bit
    lcr = omap44xx_uart3_lcr_char_length_insert(lcr, omap44xx_uart3_cl8); // 8 data bits
    omap44xx_uart3_lcr_wr(uart, lcr);
    //13 load UART mode
    omap44xx_uart3_mdr1_mode_select_wrf(uart, 0x0);
    // DONE
}


static errval_t real_init(unsigned port)
{
    if (port >= NUM_PORTS) {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
    if (uart_initialized[port]) {
        printf("omap serial_init[%d]: already initialized; skipping.\n", port);
        return SYS_ERR_OK;
    }

    // XXX: TODO: figure this out --> kaluga magic?
    errval_t err;
    lvaddr_t vbase;
    err = map_device_register(uart_base[port], uart_size[port], &vbase);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "map_device_register failed\n");
        return err;
    }
    assert(vbase);

    // paging_map_device returns an address pointing to the beginning of
    // a section, need to add the offset for within the section again
    debug_printf("omap serial_init base = 0x%"PRIxLVADDR"\n", vbase);
    omap44xx_uart3_init(&ports[port], vbase);
    uart_initialized[port] = true;
    debug_printf("omap serial_init[%d]: done.\n", port);
    return SYS_ERR_OK;
}

errval_t serial_init(uint16_t portbase, uint8_t irq)
{
    // ARM: we ignore the irq argument and use the portbase as UART port
    // number.
    if (portbase > NUM_PORTS) {
        debug_printf("don't have serial port #%d... exiting\n", portbase);
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
    // initialize hardware
    errval_t err = real_init(portbase);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "serial_init failed\n");
        return -1;
    }

    // register interrupt
    uint16_t *pb = malloc(sizeof(uint16_t));
    *pb = portbase;
    err = inthandler_setup_arm(serial_interrupt, pb, UART_IRQ);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    // offer service now we're up
    start_service();
    return SYS_ERR_OK;
}

/** output a single character */
static void serial_putchar(unsigned port, char c)
{
    assert(port <= NUM_PORTS);
    omap44xx_uart3_t *uart = &ports[port];

    // Wait until FIFO can hold more characters
    while (!omap44xx_uart3_lsr_tx_fifo_e_rdf(uart));
    // Write character
    omap44xx_uart3_thr_thr_wrf(uart, c);
}

/** write string to serial port */
void serial_write(char *c, size_t len)
{
    for (int i = 0; i < len; i++) {
        serial_putchar(serial_console_port, c[i]);
    }
}

