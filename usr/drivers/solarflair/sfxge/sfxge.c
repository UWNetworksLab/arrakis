/*-
 * Copyright (c) 2010-2011 Solarflare Communications, Inc.
 * All rights reserved.
 *
 * This software was developed in part by Philip Paeps under contract for
 * Solarflare Communications, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include "sfxge.h"

//#include "common/efx.h"
//#include "sfxge_rx.h"


// pravin: called from device_attach (sfxge_attach)
static errval_t
sfxge_create(struct sfxge_softc *sc)
{
    efx_nic_t *enp;
    errval_t err = SYS_ERR_OK;
    int error = 0;

    // Create the common code nic object.
    //mtx_init(&sc->enp_lock, "sfxge_nic", NULL, MTX_DEF);
    if ((error = efx_nic_create(sc->family, (efsys_identifier_t *)sc,
        &sc->bar, &sc->enp_lock, &enp)) != 0) {
        printf("efx_nic_create failed\n");
        abort();
    }

    sc->enp = enp;

    // Initialize MCDI to talk to the microcontroller.
    err = sfxge_mcdi_init(sc);
    if (err_is_fail(err)) {
        printf("sfxge_mcdi_init failed\n");
        abort();
    }

    // Probe the NIC and build the configuration data area.
    int status = efx_nic_probe(enp);
    if (status != 0) {
        printf("efx_nic_probe failed\n");
        abort();
    }

    // Initialize the NVRAM.
    if ((error = efx_nvram_init(enp)) != 0) {
        printf("efx_nvram_init failed\n");
        abort();
    }

    // Initialize the VPD.
    if ((error = efx_vpd_init(enp)) != 0) {
        printf("efx_vpd_init failed\n");
        abort();
    }

    // Reset the NIC.
    if ((error = efx_nic_reset(enp)) != 0) {
        printf("efx_nic_reset failed\n");
        abort();
    }

    return err;
} // end function: sfxge_create

int main(int argc, char *argv[])
{
    printf("starting sfxge driver\n");
    struct sfxge_softc *sc = NULL; // FIXME: allocate memory for this struct

    errval_t err = sfxge_create(sc);
    if (err_is_fail(err)) {
        printf("sfxge_create failed \n");
    }
    printf("sfxge_create done\n");
    return 0;
} // end function: main

