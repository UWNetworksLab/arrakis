/**
 * \file
 * \brief Resource control.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_RESOURCE_CTRL_H
#define LIBBARRELFISH_RESOURCE_CTRL_H

#include <sys/cdefs.h>

__BEGIN_DECLS

errval_t rsrc_manifest(const char *manifest, rsrcid_t *id);
errval_t rsrc_join(rsrcid_t id);
errval_t rsrc_phase(rsrcid_t id, uint32_t phase);

__END_DECLS

#endif
