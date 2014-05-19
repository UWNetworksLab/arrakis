/**
 * \file
 * \brief angler - terminal and session initialization manager
 *
 * Each instance of angler starts one new session. The first argument to
 * angler determines the terminal to associate with the session. The second,
 * the type of the terminal.
 *
 * Did you know that the angler angles for other types of fish?
 * The angler is sometimes also called sea-devil.
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

#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <angler/angler.h>

/**
 * The shell to start.
 */
#define SHELL "fish"

static void start_shell(struct capref session_id, char *terminal_type)
{
    errval_t err;
    coreid_t my_core_id = disp_get_core_id();

    char *shell = SHELL;

    /* setup argv of the shell */
    char *shell_argv[2];
    shell_argv[0] = SHELL;
    shell_argv[1] = NULL;

    /* setup environment of the shell */
    setenv("TERM", terminal_type, 1);

    /* inherit the session capability */
    struct capref inheritcn_cap;
    err = alloc_inheritcn_with_sidcap(&inheritcn_cap, session_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error allocating inherit CNode with session cap.");
    }

    /* spawn shell on the same core */
    extern char **environ;
    err = spawn_program_with_caps(my_core_id, shell, shell_argv, environ,
                                  inheritcn_cap, NULL_CAP, SPAWN_NEW_DOMAIN,
                                  NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error spawning shell.");
    }
}

static void print_usage(char *prog_name)
{
    fprintf(stderr, "%s: Wrong number of arguments. Expecting two arguments.\n",
            prog_name);
    fprintf(stderr, "Usage: %s <terminal> <terminal type>\n", prog_name);
    fprintf(stderr, "\n");
    fprintf(stderr, "\tterminal -  serial0.terminal|console0.terminal|...\n");
    fprintf(stderr, "\tterminal type - xterm|...\n");
}

int main(int argc, char **argv)
{
    errval_t err;
    struct capref session_id;

    /* argument processing */
    if (argc < 3) {
        print_usage(argv[0]);
        return EXIT_FAILURE;
    }
    char *terminal = argv[1];
    char *terminal_type = argv[2];

    /* start new session */
    err = angler_new_session(terminal, &session_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error starting session.");
    }

    /* spawn shell */
    start_shell(session_id, terminal_type);

    return EXIT_SUCCESS;
}
