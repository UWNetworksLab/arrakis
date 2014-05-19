/**
 * \file
 * \brief Prototypes for use by flounder-generated stubs
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_LMP_H
#define __FLOUNDER_SUPPORT_LMP_H

#include <sys/cdefs.h>

__BEGIN_DECLS

struct lmp_chan;
struct lmp_recv_msg;

errval_t flounder_stub_lmp_send_string(struct lmp_chan *chan,
                                       lmp_send_flags_t flags,
                                       const char *str,
                                       size_t *pos, size_t *len);
errval_t flounder_stub_lmp_recv_string(struct lmp_recv_msg *msg, char **str,
                                       size_t *pos, size_t *len);

errval_t flounder_stub_lmp_send_buf(struct lmp_chan *chan,
                                    lmp_send_flags_t flags, const void *buf,
                                    size_t len, size_t *pos);
errval_t flounder_stub_lmp_recv_buf(struct lmp_recv_msg *msg, void **buf,
                                    size_t *len, size_t *pos);

__END_DECLS

#endif // __FLOUNDER_SUPPORT_LMP_H
