/**
 * \file
 * \brief Set of default filters for terminal client library.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <term/client/default_filters.h>

/* internal functions */
static bool is_ctrlhat(char c);

/**
 * \brief Translate carriage return (\r) to line feed (\n).
 *
 * \param data   in:  characters before translation
 *               out: characters after tnaslation
 * \param length in:  length before translation
 *               out: length after translation
 */
void term_filter_cr2lf(char **data, size_t *length)
{
    assert((data != NULL) && (*data != NULL));
    assert(length != NULL);

    for (int i = 0; i < *length; i++) {
        if ((*data)[i] == '\r') {
            (*data)[i] = '\n';
        }
    }
}

/**
 * \brief Translate ASCII control characters (ASCII characters in the range
 *        0x00 - 0x1f), other than ASCII TAB, ASCII NL, ASCII CR and ASCII BS to
 *        ^X, where X is the character formed by adding 0x40 to the control
 *        character.
 *
 * \param data   in:  characters before translation
 *               out: characters after tnaslation
 * \param length in:  length before translation
 *               out: length after translation
 */
void term_filter_ctrlhat(char **data, size_t *length)
{
    size_t additional_length = 0;
    size_t new_length = 0;
    char *new_data = NULL;

    assert((data != NULL) && (*data != NULL));
    assert(length != NULL);

    /* calculate new lenght */
    for (int i = 0; i < *length; i++) {
        if (is_ctrlhat((*data)[i])) {
            additional_length++;
        }
    }

    /* if no translation necessary, bail out */
    if (additional_length == 0) {
        return;
    }
    new_length = *length + additional_length;

    /* allocate space for translated data */
    new_data = malloc(sizeof(char) * new_length);
    assert(new_data != NULL);

    /* translate characters */
    char *c = *data;
    for (int i = 0; i < new_length; ) {
        if (is_ctrlhat(*c)) {
            new_data[i++] = '^';
            new_data[i++] = (*c) + 0x40;
        } else {
            new_data[i++] = *c;
        }
    }

    free(*data);
    *data = new_data;
    *length = new_length;
}

/**
 * \brief Translate line feed (\n) to carriage return plus line feed (\r\n).
 * \param data   in:  characters before translation
 *               out: characters after tnaslation
 * \param length in:  length before translation
 *               out: length after translation
 */
void term_filter_lf2crlf(char **data, size_t *length)
{
    size_t additional_length = 0;
    size_t new_length = 0;
    char *new_data = NULL;

    assert((data != NULL) && (*data != NULL));
    assert(length != NULL);

    /* calculate new length */
    for (int i = 0; i < *length; i++) {
        if ((*data)[i] == '\n') {
            additional_length++;
        }
    }

    /* if no translation necessary, bail out */
    if (additional_length == 0) {
        return;
    }
    new_length = *length + additional_length;

    /* allocate space for translated data */
    new_data = malloc(sizeof(char) * new_length);
    assert(new_data != NULL);

    /* translate characters */
    char *c = *data;
    for (int i = 0; i < new_length; ) {
        if (*c == '\n') {
            new_data[i++] = '\r';
        }
        new_data[i++] = *c;
        c++;
    }

    free(*data);
    *data = new_data;
    *length = new_length;
}

/**
 * \privatesection
 * Internal functions.
 */
static bool is_ctrlhat(char c)
{
    if ((c <= 0x1f) && (c != '\n') && (c != '\t') && (c != '\r') && (c != '\b')) {
        return true;
    } else {
        return false;
    }
}
