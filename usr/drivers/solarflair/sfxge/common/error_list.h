/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _ERROR_LIST_H
#define _ERROR_LIST_H

enum error_list {
    ENOENT = -100,
    EINTR,
    EACCES,
    EBUSY,
    EINVAL,
    EDEADLK,
    ENOTSUP,
    ETIMEDOUT,
    EAGAIN,
    ENOSPC,
    EIO,
    EMSGSIZE,
    ENOMEM,
    EFBIG,
    EFAULT,
    EEXIST,
};

#endif // _ERROR_LIST_H

