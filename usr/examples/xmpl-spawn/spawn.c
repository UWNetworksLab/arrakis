/** \file
 *  \brief Example spawn application
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#define MAXPATH 256

int main(int argc, char *argv[])
{
    errval_t err;
    coreid_t mycore = disp_get_core_id();

    debug_printf("This is xmpl-spawn on core %d\n", mycore);

    int num_spawns = -1;
    int num_cores = -1;
    if (argc == 3) {
        num_spawns = atoi(argv[1]);
        num_cores = atoi(argv[2]);

        char path[MAXPATH];
        snprintf(path, MAXPATH, "examples/%s", argv[0]);
        argv[1] = NULL;

        domainid_t new_domain = -1;

        coreid_t core = 0;
        for (int i = 0; i < num_spawns; i++, core++) {
            core %= num_cores; 
            /*
              Signature for spawn_program is:

              errval_t spawn_program(coreid_t coreid, const char *path,
              	char *const argv[], char *const envp[],
                spawn_flags_t flags, domainid_t *ret_domainid)
            */
            err = spawn_program(core, path, argv, NULL, 0, &new_domain);
            
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed spawn %d on core %d", i, core);
            } else {
                debug_printf("program %d on core %d spawned "
                             "with domain id %d\n", i, core, new_domain);
            }
        }
    } else {
        debug_printf("not spawning any programs\n");
    }

    debug_printf("Finished\n");
}
