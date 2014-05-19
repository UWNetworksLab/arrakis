/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() */
#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <if/test_defs.h>

static const char *my_service_name = "idctest";

static const char *longstr = ""
  "Far out in the uncharted backwaters of the unfashionable end of the\n"
  "western spiral arm of the Galaxy lies a small unregarded yellow sun.\n"
  "\n"
  "Orbiting this at a distance of roughly ninety-two million miles is an\n"
  "utterly insignificant little blue green planet whose ape- descended life\n"
  "forms are so amazingly primitive that they still think digital watches\n"
  "are a pretty neat idea.\n"
  "\n"
  "This planet has - or rather had - a problem, which was this: most of the\n"
  "people on it were unhappy for pretty much of the time. Many solutions\n"
  "were suggested for this problem, but most of these were largely concerned\n"
  "with the movements of small green pieces of paper, which is odd because\n"
  "on the whole it wasn't the small green pieces of paper that were unhappy.\n"
  "\n"
  "And so the problem remained; lots of the people were mean, and most\n"
  "of them were miserable, even the ones with digital watches.\n"
  "\n"
  "Many were increasingly of the opinion that they'd all made a big mistake\n"
  "in coming down from the trees in the first place. And some said that\n"
  "even the trees had been a bad move, and that no one should ever have\n"
  "left the oceans.\n"
  "\n"
  "And then, one Thursday, nearly two thousand years after one man had\n"
  "been nailed to a tree for saying how great it would be to be nice to people\n"
  "for a change, one girl sitting on her own in a small cafe in Rickmansworth\n"
  "suddenly realized what it was that had been going wrong all this time,\n"
  "and she finally knew how the world could be made a good and happy\n"
  "place. This time it was right, it would work, and no one would have to\n"
  "get nailed to anything.\n"
  "\n"
  "Sadly, however, before she could get to a phone to tell anyone- about it,\n"
  "a terribly stupid catastrophe occurred, and the idea was lost forever.\n"
  "\n"
  "This is her story.";

static const char *shortstr = "Hello, world!";

/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void rx_basic(struct test_binding *b, uint32_t arg)
{
    printf("rx_basic %"PRIu32"\n", arg);
}

static void rx_str(struct test_binding *b, uint32_t arg, char *s)
{
    printf("rx_str %"PRIu32" '%s'\n", arg, s);
    free(s);
}

static void rx_caps(struct test_binding *b, uint32_t arg,
                    struct capref cap1, struct capref cap2)
{
    char buf1[256], buf2[256];
    debug_print_cap_at_capref(buf1, sizeof(buf1), cap1);
    debug_print_cap_at_capref(buf2, sizeof(buf2), cap2);
    buf1[sizeof(buf1) - 1] = '\0';
    buf2[sizeof(buf2) - 1] = '\0';
    printf("rx_caps %"PRIu32" [%s] [%s]\n", arg, buf1, buf2);
}

static void rx_buf(struct test_binding *b, uint8_t *buf, size_t buflen)
{
    printf("rx_buf (%zu bytes)\n", buflen);
    free(buf);
}

static struct test_rx_vtbl rx_vtbl = {
    .basic = rx_basic,
    .str = rx_str,
    .caps = rx_caps,
    .buf = rx_buf,
};

/* ------------------------------ CLIENT ------------------------------ */

struct client_state {
    struct test_binding *binding;
    int nextmsg;
    char *str;
    struct capref cap1, cap2;
};

// send the next message in our sequence
static void send_cont(void *arg)
{
    struct client_state *myst = arg;
    struct test_binding *b = myst->binding;
    struct event_closure txcont = MKCONT(send_cont, myst);
    errval_t err;

    printf("client sending msg %d\n", myst->nextmsg);

    switch(myst->nextmsg) {
    case 0:
        err = test_basic__tx(b, txcont, 7);
        break;

    case 1:
        // send a static string
        err = test_str__tx(b, txcont, 9, shortstr);
        break;

    case 2:
        // send a "dynamically allocated" string
        myst->str = strdup(shortstr);
        err = test_str__tx(b, txcont, 37, myst->str);
        break;

    case 3:
        // deallocate the string we sent in the previous message
        free(myst->str);

        // send a long string
        err = test_str__tx(b, txcont, 42, longstr);
        break;

    case 4:
        // create some caps to send (assume it all works)
        err = frame_alloc(&myst->cap1, BASE_PAGE_SIZE, NULL);
        assert(err_is_ok(err));

        err = slot_alloc(&myst->cap2);
        assert(err_is_ok(err));

        err = vnode_create(myst->cap2, ObjType_VNode_x86_64_ptable);
        assert(err_is_ok(err));

        err = test_caps__tx(b, txcont, 69, myst->cap1, myst->cap2);
        break;

    case 5:
        // delete the caps, now they've been sent
        err = cap_destroy(myst->cap1);
        assert(err_is_ok(err));

        err = cap_destroy(myst->cap2);
        assert(err_is_ok(err));

        // send a "buffer"
        err = test_buf__tx(b, txcont, (uint8_t *)longstr, strlen(longstr));
        break;

    case 6:
        // here is where we would deallocate the buffer, if it wasn't static
        printf("client all done!\n");
        return;

    default:
        err = LIB_ERR_NOT_IMPLEMENTED; // TODO: Make meaningful
        assert(!"shouldn't happen");
    }

    if (err_is_ok(err)) {
        myst->nextmsg++;
    } else {
        DEBUG_ERR(err, "error sending message %d", myst->nextmsg);

        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            assert(!"binding is busy for tx?!");

            // this error should never happen to us, because we serialise all
            // sends with the continuation, however it may happen in another
            // situation if the binding is already busy sending. in this case,
            // the user can use something like:

            struct waitset *ws = get_default_waitset();
            err = b->register_send(b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        }

        abort();
    }
}

static void bind_cb(void *st, errval_t err, struct test_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // construct local per-binding state
    struct client_state *myst = malloc(sizeof(struct client_state));
    assert(myst != NULL);
    myst->nextmsg = 0;
    myst->binding = b;
    b->st = myst;

    // start sending stuff to the service
    send_cont(myst);
}

static void start_client(void)
{
    iref_t iref;
    errval_t err;

    printf("client looking up '%s' in name service...\n", my_service_name);
    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    printf("client binding to %"PRIuIREF"...\n", iref);
    err = test_bind(iref, bind_cb, NULL /* state pointer for bind_cb */,
                    get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* ------------------------------ SERVER ------------------------------ */

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    printf("service exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct test_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void start_server(void)
{
    errval_t err;

    err = test_export(NULL /* state pointer for connect/export callbacks */,
                      export_cb, connect_cb, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{
    errval_t err;

    if (argc == 2 && strcmp(argv[1], "client") == 0) {
        start_client();
    } else if (argc == 2 && strcmp(argv[1], "server") == 0) {
        start_server();
    } else {
        printf("Usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return EXIT_FAILURE;
}
