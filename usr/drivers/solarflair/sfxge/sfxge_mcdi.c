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

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <string.h>


#include "common/efx.h"
#include "common/efx_mcdi.h"
#include "common/efx_regs_mcdi.h"


#include "sfxge.h"

#define    SFXGE_MCDI_POLL_INTERVAL_MIN 10        /* 10us in 1us units */
#define    SFXGE_MCDI_POLL_INTERVAL_MAX 100000    /* 100ms in 1us units */
#define    SFXGE_MCDI_WATCHDOG_INTERVAL 10000000    /* 10s in 1us units */

#if 0
/* Acquire exclusive access to MCDI for the duration of a request. */
static void
sfxge_mcdi_acquire(struct sfxge_mcdi *mcdi)
{

	mtx_lock(&mcdi->lock);
	KASSERT(mcdi->state != SFXGE_MCDI_UNINITIALIZED,
	    ("MCDI not initialized"));

	while (mcdi->state != SFXGE_MCDI_INITIALIZED)
		(void)cv_wait_sig(&mcdi->cv, &mcdi->lock);
	mcdi->state = SFXGE_MCDI_BUSY;

	mtx_unlock(&mcdi->lock);
}
#endif // 0

errval_t
sfxge_mcdi_init(struct sfxge_softc *sc)
{
    efx_nic_t *enp;
    struct sfxge_mcdi *mcdi;
    efx_mcdi_transport_t *emtp;
    errval_t rc = SYS_ERR_OK;

    enp = sc->enp;
    mcdi = &sc->mcdi;
    emtp = &mcdi->transport;
    // "MCDI already initialized"
    assert(mcdi->state == SFXGE_MCDI_UNINITIALIZED);

//   mtx_init(&mcdi->lock, "sfxge_mcdi", NULL, MTX_DEF);

    mcdi->state = SFXGE_MCDI_INITIALIZED;

    emtp->emt_context = sc;
/*
    emtp->emt_execute = sfxge_mcdi_execute;
    emtp->emt_ev_cpl = sfxge_mcdi_ev_cpl;
    emtp->emt_exception = sfxge_mcdi_exception;
*/

//    cv_init(&mcdi->cv, "sfxge_mcdi");


    if ((rc = efx_mcdi_init(enp, emtp)) != 0) {
        printf("efx_mcdi_init failed! \n");
        goto fail;
    }

    return (0);

fail:
//    mtx_destroy(&mcdi->lock);
    mcdi->state = SFXGE_MCDI_UNINITIALIZED;
    return (rc);
}

void
sfxge_mcdi_fini(struct sfxge_softc *sc)
{
    struct sfxge_mcdi *mcdi;
    efx_nic_t *enp;
    efx_mcdi_transport_t *emtp;

    enp = sc->enp;
    mcdi = &sc->mcdi;
    emtp = &mcdi->transport;

    // mtx_lock(&mcdi->lock);

    // "MCDI not initialized"
    assert(mcdi->state == SFXGE_MCDI_INITIALIZED);

    efx_mcdi_fini(enp);
    memset(emtp, 0, sizeof(*emtp));
    //bzero(emtp, sizeof(*emtp));

//    cv_destroy(&mcdi->cv);
//    mtx_unlock(&mcdi->lock);

//    mtx_destroy(&mcdi->lock);
}

