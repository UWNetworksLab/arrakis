/**
 * \file
 * \brief XDR implementation using LWIP PBuf structures
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _XDR_PBUF_H
#define _XDR_PBUF_H

struct pbuf;
bool xdr_pbuf_create_send(XDR *xdr, size_t size);
void xdr_pbuf_create_recv(XDR *xdr, struct pbuf *pbuf);

#endif
