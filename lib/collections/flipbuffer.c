/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <collections/flipbuffer.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

/**
 * \brief Append data to front buffer.
 *
 * \param fbuf   Flip buffer to append data to.
 * \param data   Data to append. Don't have to be \0-terminated.
 * \param length Length of data to append.
 */
void collections_fbuf_append(struct collections_fbuf *fbuf, const void *data,
                             size_t length)
{
    if (data == NULL || length == 0) {
        return;
    }

    if (fbuf->front == NULL) {
        // allocate a new front buffer
        fbuf->front = malloc(length);
        assert(fbuf->front != NULL);
        memcpy(fbuf->front, data, length);
        fbuf->frontlen = length;
    } else {
        // append to existing buffer
        fbuf->front = realloc(fbuf->front, fbuf->frontlen + length);
        assert(fbuf->front != NULL);
        memcpy(fbuf->front + fbuf->frontlen, data, length);
        fbuf->frontlen += length;
    }
}

/**
 * \brief Create a new flip buffer.
 *
 * \param fbuf   Pointer to a pointer to a flip buffer. Filled-in by function.
 */
void collections_fbuf_create(struct collections_fbuf **fbuf)
{
    *fbuf = malloc(sizeof(struct collections_fbuf));
    assert(*fbuf != NULL);

    (*fbuf)->front = NULL;
    (*fbuf)->frontlen = 0;
    (*fbuf)->back = NULL;
    (*fbuf)->backlen = 0;
}

/**
 * \brief Flip the buffer, i.e. change the role of the front and back buffer.
 *
 * \param fbuf   Flip buffer to flip.
 */
void collections_fbuf_flip(struct collections_fbuf *fbuf)
{
    char *tmpbuf;
    size_t tmplen;

    tmpbuf = fbuf->front;
    tmplen = fbuf->frontlen;

    fbuf->front = fbuf->back;
    fbuf->frontlen = fbuf->backlen;

    fbuf->back = tmpbuf;
    fbuf->backlen = tmplen;
}

/**
 * \brief Empty the front buffer
 *
 * \param fbuf   Flip buffer to operate on.
 */
void collections_fbuf_free(struct collections_fbuf *fbuf)
{
    if (fbuf->front != NULL) {
        free(fbuf->front);
        fbuf->front = NULL;
    }
    fbuf->frontlen = 0;
}

/**
 * \brief Retrieve the data from the front buffer.
 *
 * \param fbuf   Flip buffer to get data from.
 * \return       Content of the front buffer. Not \0 terminated.
 */
void *collections_fbuf_get_data(struct collections_fbuf *fbuf)
{
    return fbuf->front;
}

/**
 * \brief Retrieve the length of the data of the front buffer.
 *
 * \param fbuf   Flip buffer to get length of data from.
 * \return       Length of the data in the front buffer.
 */
size_t collections_fbuf_get_length(struct collections_fbuf *fbuf)
{
    return fbuf->frontlen;
}

/**
 * \brief Is the front buffer empty?
 *
 * \param fbuf   Flip buffer to check.
 * \return       Whether or not the front buffer contains any data?
 */
bool collections_fbuf_is_empty(struct collections_fbuf *fbuf)
{
    if (fbuf->front == NULL || fbuf->frontlen == 0) {
        return true;
    } else {
        return false;
    }
}

/**
 * \brief Is the back buffer empty?
 *
 * \param fbuf   Flip buffer to check.
 * \return       Whether or not the back buffer contains any data?
 */
bool collections_fbuf_other_is_empty(struct collections_fbuf *fbuf)
{
    if (fbuf->back == NULL || fbuf->backlen == 0) {
        return true;
    } else {
        return false;
    }
}


/**
 * \brief Empty the back buffer
 *
 * \param fbuf   Flip buffer to operate on.
 */
void collections_fbuf_other_free(struct collections_fbuf *fbuf)
{
    if (fbuf->back != NULL) {
        free(fbuf->back);
        fbuf->back = NULL;
    }
    fbuf->backlen = 0;
}

/**
 * \brief Free all memory associated with the flip buffer.
 */
void collections_fbuf_release(struct collections_fbuf *fbuf)
{
    if (fbuf == NULL) {
        return;
    }

    collections_fbuf_free(fbuf);
    collections_fbuf_other_free(fbuf);

    free(fbuf);
}
