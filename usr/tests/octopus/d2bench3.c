/**
 * \file
 * \brief Benchmark publish / subscribe throughput.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <octopus/octopus.h>
#include <skb/skb.h>

#define MAX_SEND 100000

static void start_publisher(void) {

	for (size_t i=0; i < MAX_SEND; i++) {
		errval_t err = oct_publish("msg_1 { val: %d }", 1);
		if (err_is_fail(err)) { DEBUG_ERR(err, "publish"); exit(1); }
	}

}

static void handle_msg(uint64_t id, char* msg, void* state)
{
	free(msg);
}

static void start_subscriber() {
	errval_t err = oct_subscribe(handle_msg, NULL, &id, "_ { val: 1 }");
	if (err_is_fail(err)) { DEBUG_ERR(err, "subscribe"); exit(1); }
}

int main(int argc, char** argv)
{
    oct_init();
    bench_init();

    start_client();
    if (0) start_subscriber();

    return EXIT_SUCCESS;
}

