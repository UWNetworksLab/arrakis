/*
 * \brief Driver for face detection on OMAP 4460.
 *
 * \see OMAP TRM rev Z
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>

#include "fdif.h"

#include <dev/omap/omap44xx_cam_prm_dev.h>
#include <dev/omap/omap44xx_cam_cm2_dev.h>
#include <dev/omap/omap44xx_fdif_dev.h>

#define FDIF_IRQ (32+69)

#define PRINT_BUFFER_SIZE (1024*1024)
static char printbuf[PRINT_BUFFER_SIZE];

static omap44xx_cam_prm_t dev;
static omap44xx_fdif_t devfdif;
static omap44xx_cam_cm2_t devclk;

extern struct gimage lena_image;

static void manage_clocks(void)
{
    FDIF_DEBUG("Enable the clocks in domain CD_CAM\n");

    // Clock domain CAM
    lvaddr_t vbase;
    errval_t err;
    err = map_device_register(0x4A009000, 4096, &vbase);
    assert(err_is_ok(err));

    omap44xx_cam_cm2_initialize(&devclk, (mackerel_addr_t)vbase);
    //omap44xx_cam_cm2_cm_cam_staticdep_l3_1_statdep_wrf(&devclk, 0x1);
    //omap44xx_cam_cm2_cm_cam_staticdep_memif_statdep_wrf(&devclk, 0x1);
    //omap44xx_cam_cm2_cm_cam_staticdep_ivahd_statdep_wrf(&devclk, 0x1);

    // Explicit enable && Force SW wakeup
    omap44xx_cam_cm2_cm_cam_fdif_clkctrl_modulemode_wrf(&devclk, 0x2);
    omap44xx_cam_cm2_cm_cam_clkstctrl_clktrctrl_wrf(&devclk, 0x2);
}

static void manage_power(void)
{
    FDIF_DEBUG("Power-on the PD_CAM domain for fdif\n");

    // Power domain CAM
    lvaddr_t vbase;
    errval_t err;
    err = map_device_register(0x4A307000, 4096, &vbase);
    assert(err_is_ok(err));

    omap44xx_cam_prm_initialize(&dev, (mackerel_addr_t)vbase);
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&dev, 0x3);

    FDIF_DEBUG("%s:%d: Power OFF -> ON\n", __FUNCTION__, __LINE__);
    while (omap44xx_cam_prm_pm_cam_pwrstst_powerstatest_rdf(&dev)
            != 0x3);
    FDIF_DEBUG("%s:%d: Power is on.\n", __FUNCTION__, __LINE__);
}

static void read_result(void)
{
    printf("Face detection completed:\n");
    printf("Read the results...\n");

    int faces = omap44xx_fdif_fd_dnum_dnum_rdf(&devfdif);
    printf("Faces found: %d\n", faces);
    //omap44xx_fdif_pr(printbuf, PRINT_BUFFER_SIZE, &devfdif);
    //printf("%s\n", printbuf);

    for (int i = 0; i < faces; i++) {
        printf("Face %d:\n", i);
        int x = omap44xx_fdif_fd_centerx_centerx_rdf(&devfdif, i);
        int y = omap44xx_fdif_fd_centery_centery_rdf(&devfdif, i);
        printf("Position (X,Y): %d %d\n", x, y);

        int size = omap44xx_fdif_fd_confsize_size_rdf(&devfdif, i);
        int confidence = omap44xx_fdif_fd_confsize_conf_rdf(&devfdif, i);
        int angle = omap44xx_fdif_fd_angle_angle_rdf(&devfdif, i);
        printf("Size: %d Confidence: %d Angle: %d\n", size, confidence, angle);
    }
}

/*
 * \brief Interrupt handler for "finish" interrupt
 */
static void irq_handler(void *args)
{
    read_result();

    omap44xx_fdif_fdif_ctrl_pr(printbuf, PRINT_BUFFER_SIZE, &devfdif);
    printf("%s\n", printbuf);

    omap44xx_cam_cm2_pr(printbuf, PRINT_BUFFER_SIZE, &devclk);
    printf("%s\n", printbuf);

    omap44xx_cam_prm_pr(printbuf, PRINT_BUFFER_SIZE, &dev);
    printf("%s\n", printbuf);


    omap44xx_fdif_fdif_irqstatus_finish_irq_wrf(&devfdif, 2, 1);

    // Go in Standby Mode
    // Again, module seems to go in standby just fine after its done
    // processing.
    //printf("%s:%d: go in standby\n", __FUNCTION__, __LINE__);
    //omap44xx_fdif_fdif_ctrl_mstandby_wrf(&devfdif, 0x1);
    //while(omap44xx_fdif_fdif_ctrl_mstandby_hdshk_rdf(&devfdif) != 0x0);

    // Disable Module Clocks
    omap44xx_cam_cm2_cm_cam_clkstctrl_clktrctrl_wrf(&devclk, 0x1);
    omap44xx_cam_cm2_cm_cam_fdif_clkctrl_modulemode_wrf(&devclk, 0x0);

    // Going Powermode ON-INACTIVE
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&dev, 0x2);
    FDIF_DEBUG("%s:%d: Power ON -> ON-INACTIVE\n", __FUNCTION__, __LINE__);
    while (omap44xx_cam_prm_pm_cam_pwrstst_powerstatest_rdf(&dev)
            != 0x2);

    // Going Powermode ON-INACTIVE -> OFF
    // Must use lowpoerstatechange for that
    FDIF_DEBUG("%s:%d: Power ON-INACTIVE -> OFF\n", __FUNCTION__, __LINE__);
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&dev, 0x0);
    omap44xx_cam_prm_pm_cam_pwrstctrl_lowpowerstatechange_wrf(&dev, 0x1);
    while (omap44xx_cam_prm_pm_cam_pwrstctrl_lowpowerstatechange_rdf(&dev)
            != 0x0);

    omap44xx_cam_cm2_pr(printbuf, PRINT_BUFFER_SIZE, &devclk);
    FDIF_DEBUG("%s\n", printbuf);

    omap44xx_cam_prm_pr(printbuf, PRINT_BUFFER_SIZE, &dev);
    FDIF_DEBUG("%s\n", printbuf);
}

/*
 * \brief Enable in poll-based mode
 *
 * \see OMAP TRM 9.4.1.2.1.1 Main Sequence – FDIF Polling Method
 */
static void __attribute__((__unused__)) enable_poll_mode(void)
{
    omap44xx_fdif_fd_ctrl_run_wrf(&devfdif, 0x1);

    FDIF_DEBUG("%s:%d: Waiting until fdif is done...\n", __FUNCTION__, __LINE__);
    while (omap44xx_fdif_fd_ctrl_finish_rdf(&devfdif) != 0x1);

    read_result();
}


/*
 * \brief Enable in interrupt-based mode
 *
 * See OMAP TRM 9.4.1.2.1.2 Main Sequence – FDIF Interrupt Method
 */
static void enable_irq_mode(void)
{
    errval_t err;

    omap44xx_fdif_fdif_irqenable_set_finish_irq_wrf(&devfdif, 2, 1);

    // Register interrupt handler in kernel
    err = inthandler_setup_arm(irq_handler, NULL, FDIF_IRQ);
    assert(err_is_ok(err));

    omap44xx_fdif_fd_ctrl_run_wrf(&devfdif, 0x1);

    while (1) {
        event_dispatch(get_default_waitset());
    }
}

/*
 * \brief Set image parameters
 *
 * See OMAP TRM 9.4.1.2.1.3 Subsequence – Set Image Parameters
 */
static void set_image_params(genpaddr_t picaddr, genpaddr_t wkaddr)
{
    // make sure 5 least significant bits are 0!
    omap44xx_fdif_fdif_picaddr_wr(&devfdif, picaddr);
    omap44xx_fdif_fdif_wkaddr_wr(&devfdif, wkaddr);

    omap44xx_fdif_fd_dcond_min_wrf(&devfdif, 0x0); // 40 pixel
    omap44xx_fdif_fd_dcond_dir_wrf(&devfdif, 0x0); // up?

    omap44xx_fdif_fd_startx_startx_wrf(&devfdif, 0x0);
    omap44xx_fdif_fd_starty_starty_wrf(&devfdif, 0x0);

    omap44xx_fdif_fd_sizex_sizex_wrf(&devfdif, 0x140); // TODO
    omap44xx_fdif_fd_sizey_sizey_wrf(&devfdif, 0xf0); // TODO
    omap44xx_fdif_fd_lhit_lhit_wrf(&devfdif, 0x5);
}

/*
 * \brief Face detection on OMAP4460
 *
 * We support poll-based mode as well as interrupt-based mode. The
 * default setting is to use polling for its simplicity. If interrups is
 * given as command line argument, interrupts will be used instead.
 */
int main(int argc, char **argv)
{
    size_t img_size = 320 * 240 * 8; // 75 KB
    size_t working_size = img_size; // 51.25 KB is enough
    size_t retbytes;
    void *workarea;
    uint8_t *image;
    errval_t err;
    lpaddr_t vbase;

    // Face detect Module
    err = map_device_register(0x4A10A000, 4096, &vbase);
    assert(err_is_ok(err));
    omap44xx_fdif_initialize(&devfdif, (mackerel_addr_t)vbase);

    FDIF_DEBUG("FDIF Global Initialization\n");

    manage_clocks();
    manage_power();

    omap44xx_fdif_fdif_sysconfig_softreset_wrf(&devfdif, 1);
    while (omap44xx_fdif_fdif_sysconfig_softreset_rdf(&devfdif) != 0);

    omap44xx_fdif_fdif_sysconfig_pr(printbuf, PRINT_BUFFER_SIZE, &devfdif);
    printf("%s\n", printbuf);

    omap44xx_fdif_fdif_sysconfig_idlemode_wrf(&devfdif, 0x2);
    omap44xx_fdif_fdif_sysconfig_standbymode_wrf(&devfdif, 0x2);

    omap44xx_fdif_fdif_sysconfig_pr(printbuf, PRINT_BUFFER_SIZE, &devfdif);
    printf("%s\n", printbuf);

    omap44xx_fdif_fdif_ctrl_max_tags_wrf(&devfdif, 0xA);

    struct capref img_cap;
    struct capref workarea_cap;

    err = frame_alloc(&img_cap, img_size, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= img_size);

    err = frame_alloc(&workarea_cap, working_size, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= working_size);

    // Map space for image (as non-cacheable)
    err = vspace_map_one_frame_attr((void **)&image, img_size, img_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    assert(err_is_ok(err));
    err = vspace_map_one_frame(&workarea, working_size, workarea_cap,
                               NULL, NULL);
    assert(err_is_ok(err));

    FDIF_DEBUG("Image: width:%d height:%d bytes/pixel:%d\n",
               lena_image.width, lena_image.height, lena_image.bytes_per_pixel);
    for (int i = 0; i < lena_image.width * lena_image.height; i += 1) {
        image[i] = lena_image.pixel_data[i];
    }

    struct frame_identity ret;
    err = invoke_frame_identify(img_cap, &ret);
    assert (err_is_ok(err));

    struct frame_identity wkret;
    err = invoke_frame_identify(workarea_cap, &wkret);
    assert (err_is_ok(err));

    set_image_params(ret.base, wkret.base);

    // The following will set cm_cam_fdif_clkctrl_stdbysy to 0x0 (not in standby)
    // but it is apparently not needed (because we are in smart standby and
    // wake-up with magic as soon as we need it...?)
    //printf("%s:%d: // Wake up from standby\n", __FUNCTION__, __LINE__);
    //omap44xx_fdif_fdif_ctrl_mstandby_wrf(&devfdif, 0x0);
    //while(omap44xx_fdif_fdif_ctrl_mstandby_hdshk_rdf(&devfdif) != 0x1);

#if defined(FDIF_IRQ)
    enable_irq_mode();
#else
    enable_poll_mode();
#endif

    return 0;
}
