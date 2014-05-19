/**
 * \brief Simple SKB; implements some basic functionality of Octopus to provide
 * a nameservice for architectures that are not yet ready to run ECLiPSe (like ARM)
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <unistd.h>

#include <barrelfish/barrelfish.h>
#include <octopus_server/init.h>

int main(int argc, char**argv)
{
	errval_t err = oct_server_init();
	if (err_is_fail(err)) {
		USER_PANIC_ERR(err, "octopus init failed?");
	}

    messages_handler_loop();

    return EXIT_FAILURE;
}
