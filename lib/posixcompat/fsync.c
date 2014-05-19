/*
 * Copyright (c) 2011, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <barrelfish/barrelfish.h>
#include <vfs/fdtab.h>
#include <vfs/vfs.h>
#include "posixcompat.h"

int fsync(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    switch(e->type) {
    case FDTAB_TYPE_FILE:
      {
	errval_t err = vfs_flush((vfs_handle_t)e->handle);
	if(err_is_fail(err)) {
	  return -1;
	}
      }
      break;

    default:
      errno = EINVAL;
      return -1;
    }

    return 0;
}
