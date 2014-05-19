/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef STORAGE_VSIC_H
#define STORAGE_VSIC_H

// Roundup s to the next multiple of m
#define STORAGE_ROUNDUP(s, m) \
  ((s) % (m) != 0 ? (s) + ((m) - ((s) % (m))) : (s))

// Round to VSIC block size
#define STORAGE_VSIC_ROUND(vsic, s) \
  STORAGE_ROUNDUP(s, (vsic)->blocksize)

struct storage_vsic;
struct storage_vsa;

struct storage_vsic_ops {
    errval_t (*write)(struct storage_vsic *vsic, struct storage_vsa *vsa,
                      off_t offset, size_t size, void *buffer);
    errval_t (*read)(struct storage_vsic *vsic, struct storage_vsa *vsa,
                     off_t offset, size_t size, void *buffer);
    errval_t (*flush)(struct storage_vsic *vsic, struct storage_vsa *vsa);
    errval_t (*flush2)(struct storage_vsic *vsic, struct storage_vsa *vsa, void *handle);
    errval_t (*wait)(struct storage_vsic *vsic);
    errval_t (*poll)(struct storage_vsic *vsic, void **handle);
};

struct storage_vsic {
    struct storage_vsic_ops     ops;
    void                        *data;
    size_t			blocksize;
};

// XXX: Remove once we support multiple backend drivers
errval_t storage_vsic_driver_init(int argc, const char **argv,
				  struct storage_vsic *vsic);

#endif
