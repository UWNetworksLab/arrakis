/**
 * \file
 * \brief File cache for HTTP server
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HTTP_CACHE_H
#define HTTP_CACHE_H
#include "webserver_session.h"
err_t http_cache_init (struct ip_addr server, const char *path,
                     void (*callback)(void));
err_t http_cache_lookup (const char *name, struct http_conn *cs);
long decrement_buff_holder_ref (struct buff_holder *bh);
long decrement_reference (struct http_conn *cs);
#endif // HTTP_CACHE_H
