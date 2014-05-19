/**
 * \file
 * \brief SKB library functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include "skb_debug.h"

#define BUFFER_SIZE (16384 * 2)
#define OUTPUT_SIZE (16384 * 2)

/* XXX: The following static chars make the skb connection not thread
   safe and we probably don't want to put them in the per dispatcher
   corestate as they are so big. */
static char buffer[BUFFER_SIZE];
static char output[OUTPUT_SIZE];
static char error_output[OUTPUT_SIZE];
static int error_code;

int skb_read_error_code(void)
{
    return error_code;
}

char *skb_get_output(void)
{
    return (output);
}

char *skb_get_error_output(void)
{
    return (error_output);
}

errval_t skb_execute(char *goal)
{
    int32_t error;
    char *result, *error_out;
    errval_t err = skb_evaluate(goal, &result, &error_out, &error);
    if (err_is_fail(err)) {
        return err_push(err, SKB_ERR_EVALUATE);
    }
    error_code = error;
    strncpy(output, result, OUTPUT_SIZE);
    strncpy(error_output, error_out, OUTPUT_SIZE);
    free(result);
    free(error_out);
    if (error != 0) {
        return err_push(err, SKB_ERR_EXECUTION);
    }
    return err;
}

errval_t skb_add_fact(char *fmt, ...)
{
    va_list va_l;
    va_start(va_l, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, va_l);
    buffer[BUFFER_SIZE - 1] = 0;
    int len = strlen(buffer);

    char *dot = strrchr(buffer, '.');
    if (dot != 0) {
        *dot = 0;
    }

    SKB_DEBUG("skb_add_fact(): %s\n", buffer);
    snprintf(buffer + len, BUFFER_SIZE - len,
             "assert(%s).", buffer);
    errval_t err = skb_execute(buffer + len);
    va_end(va_l);
    return err;
}

int skb_execute_query(char *fmt, ...)
{
    va_list va_l;
    va_start(va_l, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, va_l);
    va_end(va_l);
    buffer[BUFFER_SIZE - 1] = 0;

    char *dot = strrchr(buffer, '.');
    if (dot != 0) {
        *dot = 0;
    }

    return skb_execute(buffer);
}

static inline int count_expected_conversions(char *s, int len)
{
    int expected_conversions = 0;
    //count the number of single occurences of '%' to calculate the expected
    //number of conversions made by sscanf
    for (int i = 0; i < len; i++) {
        if ((s[i] == '%') && 
            (((i + 1 < len) && (s[i + 1] != '%')) ||
            (i + 1 >= len))) {
            expected_conversions++;
        }
    }
    return (expected_conversions);
}

errval_t skb_read_output_at(char *out, char *fmt, ...)
{
    errval_t r;
    va_list va_l;
    va_start(va_l, fmt);
    r = skb_vread_output_at(out, fmt, va_l);
    va_end(va_l);
    return(r);
}

errval_t skb_vread_output_at(char *out, char *fmt, va_list va_l)
{

    int expected_conversions = 0;
    int nr_conversions;
    int fmtlen = strlen(fmt);
    expected_conversions = count_expected_conversions(fmt, fmtlen);

    nr_conversions = vsscanf(out, fmt, va_l);
    if (nr_conversions != expected_conversions) {
        SKB_DEBUG("skb_vread_output_at(): Could not convert the SKB's result\n");
        SKB_DEBUG("SKB returned: %s\nSKB error: %s\n", skb_get_output(),
                skb_get_error_output());
        return SKB_ERR_CONVERSION_ERROR;
    }
    return SYS_ERR_OK;
}


errval_t skb_read_output(char *fmt, ...)
{
    errval_t r;
    va_list va_l;
    va_start(va_l, fmt);
    r = skb_vread_output_at(skb_get_output(), fmt, va_l);
    va_end(va_l);
    return r;
}


void skb_read_list_init_offset(struct list_parser_status *status, char *s,
                               int offset)
{
    status->s = s + offset;
    status->conv_ptr = s;
    status->len = strlen(s);
    status->element_name[0] = 0;
    status->expected_conversions = -1;
}

void skb_read_list_init(struct list_parser_status *status)
{
    skb_read_list_init_offset(status, skb_get_output(), 0);
}

bool skb_read_list(struct list_parser_status *status, char *fmt, ...)
{
    va_list va_l;
    va_start(va_l, fmt);

    int nr_conversions;
    int lpar = 0;
    int fmtlen = strlen(fmt);
    if (status->element_name[0] == 0) {
        for (lpar = 0; lpar < fmtlen; lpar++) {
            if (fmt[lpar] == '(') {
                break;
            }
        }
        strncpy(status->element_name, fmt,
                (lpar < ELEMENT_NAME_BUF_SIZE) ? lpar : ELEMENT_NAME_BUF_SIZE);
        status->element_name[
                (lpar < ELEMENT_NAME_BUF_SIZE) ? lpar : ELEMENT_NAME_BUF_SIZE
                            ] = 0;
        status->element_len = lpar;
    }
    if (status->expected_conversions == -1) {
        status->expected_conversions =
            count_expected_conversions(fmt, fmtlen);
    }


    //iterate over all buselements
    while (status->conv_ptr < status->s + status->len) {
        // search the beginning of the next buselement
        while ((status->conv_ptr < status->s + status->len) &&
               (strncmp(status->conv_ptr, status->element_name,
                        status->element_len)) != 0) {
                    status->conv_ptr++;
        }
        //convert the string to single elements and numbers
        nr_conversions = vsscanf(status->conv_ptr, fmt, va_l);
        va_end(va_l);
        status->conv_ptr++;
        if (nr_conversions != status->expected_conversions) {
            return false;
        }
        return true;
    }
    return false;
}
