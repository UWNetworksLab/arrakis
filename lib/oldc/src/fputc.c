/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdio_file.h>

/*
 * XXX (C99): An output stream is not currently flushed when input is
 * read from an input stream attached to a terminal. This is because
 * we can't currently know what's a terminal and what isn't.
 */

int
fputc(int c, FILE *stream)
{
    lock_stream(stream);

    if(stream->buffer == NULL) {
        stream->buffer = malloc(stream->buf_size);
	if (stream->buffer == NULL)
	    return EOF;
        stream->buf_pos = 0;
        stream->buf_allocated = 1;
    }

    stream->buffer[stream->buf_pos++] = c;

    if(stream->buffering_mode == _IONBF
       || stream->buf_pos == stream->buf_size
       || (stream->buffering_mode == _IOLBF && (c == '\n' || c == '\r'))) {
        unlock_stream(stream);
        if(fflush(stream) != 0) {
            return EOF;
        }
    } else {
        unlock_stream(stream);
    }

    return c;
}
