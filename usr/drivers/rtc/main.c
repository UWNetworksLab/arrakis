/**
 * \file
 * \brief Real Time Clock driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include "rtc.h"

static struct chips_msg *handler(void *st, struct chips_msg *msg)
{
    uintptr_t msgtype;
    int r;
    struct chips_msg *reply;
    struct rtc_time time;

    r = msg->f->unmarshall_word(msg, &msgtype);
    if (r != 0) {
        return NULL;
    }

    switch (msgtype) {
    case 0:     // get current real time clock time
        rtc_read(&time);
        reply = msg->conn->f->create_msg(msg->conn);
        reply->f->marshall_word(reply, ((uintptr_t)time.hr << 16) |
                                       ((uintptr_t)time.min << 8) |
                                       (uintptr_t)time.sec);
        msg->conn->f->send(reply);
        break;
    default:
        printf("rtc: Bogus request message recieved\n");
        break;
    }

    return NULL;
}

int main(int argc, char *argv[])
{
    int r;

    // Connect to chips
    struct chips_context *context = chips_get_context();
    r = context->init("rtc");
    assert(r == 0);

    r = chips_listen_and_register_singleton(NULL, handler, context, "rtc",
                                            NULL, NULL);
    assert(r == 0);

    // Stick around waiting for input
    thread_exit();
    assert(!"thread_exit returned");
    return EXIT_FAILURE;
}
