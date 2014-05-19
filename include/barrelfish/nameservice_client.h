/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_NAMESERVICE_CLIENT_H
#define BARRELFISH_NAMESERVICE_CLIENT_H

#include <sys/cdefs.h>

__BEGIN_DECLS

errval_t nameservice_lookup(const char *iface, iref_t *retiref);
errval_t nameservice_blocking_lookup(const char *iface, iref_t *retiref);
errval_t nameservice_register(const char *iface, iref_t iref);
errval_t nameservice_client_blocking_bind(void);

errval_t nameservice_get_capability(const char *key, struct capref *retcap);
errval_t nameservice_put_capability(const char *key, struct capref cap);
errval_t nameservice_remove_capability(const char *key);

__END_DECLS

#endif // BARRELFISH_NAMESERVICE_CLIENT_H
