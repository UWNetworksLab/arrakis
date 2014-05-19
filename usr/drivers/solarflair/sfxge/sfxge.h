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

#ifndef _SFXGE_H
#define _SFXGE_H

#include "common/efx.h"

enum sfxge_mcdi_state {
    SFXGE_MCDI_UNINITIALIZED = 0,
    SFXGE_MCDI_INITIALIZED,
    SFXGE_MCDI_BUSY,
    SFXGE_MCDI_COMPLETED
};

struct sfxge_mcdi {
    enum sfxge_mcdi_state    state;
    efx_mcdi_transport_t    transport;
};


enum sfxge_softc_state {
    SFXGE_UNINITIALIZED = 0,
    SFXGE_INITIALIZED,
    SFXGE_REGISTERED,
    SFXGE_STARTED
};

struct sfxge_softc {
    enum sfxge_softc_state      init_state;
    efx_family_t                family;
    efx_nic_t                   *enp;
    struct mtx                  enp_lock;
    efsys_bar_t                 bar;
    struct sfxge_mcdi           mcdi;
};


errval_t sfxge_mcdi_init(struct sfxge_softc *sc);

void sfxge_mcdi_fini(struct sfxge_softc *sc);

#endif /* _SFXGE_H */

