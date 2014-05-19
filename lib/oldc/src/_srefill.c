/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdio_file.h>

#include "local.h"

/**
 * \brief Refill a file's input buffer. Allocates the buffer if it doesn't exist yet.
 *
 * \param f     Pointer to file to refill input buffer for.
 *
 * \return 0 on success, EOF on end of file, 1 on other error.
 */
int __srefill(FILE *f)
{
    if(feof(f)) {
        return EOF;
    }

    // Allocate buffer if it doesn't exist
    if(f->rbuffer == NULL) {
        f->rbuffer = malloc(BUFSIZ);

        if(f->rbuffer == NULL) {
            // Out of memory
            return 1;
        }

        f->rbuf_size = BUFSIZ;
    } else {
        // Advance file position according to old buffer position
        f->current_pos += (uintptr_t)(f->rbuf_pos - f->rbuffer);
    }

    // Reset buffer
    f->rbuf_pos = f->rbuffer;
    f->rbuf_valid = f->read_fn(f->rbuffer, f->current_pos, BUFSIZ, f->handle);

    return 0;
}
