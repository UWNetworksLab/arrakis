/**
 * \file
 * \brief startd spawn functions
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() in string.h */
#include <string.h>
#include <stdio.h>


#include <barrelfish/barrelfish.h>

#include <barrelfish/spawn_client.h>
#include <spawndomain/spawndomain.h>
#include <dist/barrier.h>

#include "internal.h"

extern char **environ;

static const char *get_shortname(const char *start, const char *nameend,
                                 size_t *namelen)
{

    // find the short name by searching back for the last / before the args
    const char *shortname = nameend;
    while (shortname >= start && *shortname != '/') {
        shortname--;
    }
    if (shortname != start) {
        shortname++;
    }

    // where's the end of the basename? (ie. ignoring beehive's | suffix)
    const char *basenameend = memchr(shortname, '|', nameend - shortname);
    if (basenameend == NULL) {
        basenameend = nameend;
    }

    *namelen = basenameend - shortname;
    return shortname;
}

static void set_local_bindings(void)
{
    ram_alloc_set(NULL);
}


struct spawn_info {
    int argc;
    char *argv[MAX_CMDLINE_ARGS + 1];
    char *name;
    char *shortname;
    size_t shortnamelen;
    char *cmdargs;
};


/*
   read the next line of the bootmodule, and return info about what to
   spawn in *si
   return:
   1: line read succesfully
   0: end of file reached
   -1: error
*/
static int prepare_spawn(size_t *bmpos, struct spawn_info *si)
{
    assert(bmpos != NULL);
    assert(si != NULL);

    const char *bootmodules = gbootmodules;

    // find the start/end of the next line
    const char *start = &bootmodules[*bmpos];
    const char *end = strchr(start, '\n');
    if (end == NULL) {
        return 0;
    } else {
        *bmpos = end - bootmodules + 1;
    }

    // ignore arguments for name comparison
    const char *args = memchr(start, ' ', end - start);

    // where's the end of the full name?
    const char *nameend = args == NULL ? end : args;

    si->shortname = (char *)get_shortname(start, nameend, &si->shortnamelen);

    si->cmdargs = malloc(end - si->shortname + 1);
    if (si->cmdargs == NULL) {
        return -1;
    }
    si->name = malloc(nameend - start + 1);
    if (si->name == NULL) {
        free(si->cmdargs);
        return -1;
    }

    /* Get the command line arguments of the domain: args plus shortname */
    memcpy(si->cmdargs, si->shortname, end - si->shortname);
    si->cmdargs[end - si->shortname] = '\0';
    si->argc = spawn_tokenize_cmdargs(si->cmdargs, si->argv,
                                      ARRAY_LENGTH(si->argv));
    if (si->argc >= MAX_CMDLINE_ARGS) {
        free(si->cmdargs);
        free(si->name);
        return -1;
    }

    /* grab a copy of the full name as a separate string */
    memcpy(si->name, start, nameend - start);
    si->name[nameend - start] = '\0';

    return 1;
}


void spawn_dist_domains(void)
{
    struct spawn_info si;
    size_t bmpos = 0;
    errval_t err;
    int r;

    coreid_t my_coreid = disp_get_core_id();

    while (true) {

        r = prepare_spawn(&bmpos, &si);
        if (r == 0) {
            return;
        } else if (r == -1) {
            DEBUG_ERR(STARTD_ERR_BOOTMODULES,
                      "failed to read bootmodules entry");
        }

        /* Only spawn special dist-serv modules */
        if (si.argc >= 2 && strcmp(si.argv[1], "dist-serv") == 0) {

            coreid_t coreid;
            int extra_args;

            // get core id
            if (si.argc >= 3 && strncmp(si.argv[2], "core=", 5) == 0) {

                char *p = strchr(si.argv[2], '=');
                assert(p != NULL);
                coreid = strtol(p + 1, NULL, 10);
                extra_args = 2;

            } else {
                coreid = my_coreid;
                extra_args = 1;
            }

            // discard 'dist-serv' and 'core=x' argument
            for (int i = 1; i <= si.argc - extra_args; i++) {
                si.argv[i] = si.argv[i+extra_args];
            }
            si.argc--;

            debug_printf("starting dist-serv %s on core %d\n", si.name, coreid);

            domainid_t new_domain;
            err = spawn_program(coreid, si.name, si.argv, environ,
                                0, &new_domain);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "spawn of %s failed", si.name);
                continue;
            }

            char c = si.shortname[si.shortnamelen];
            si.shortname[si.shortnamelen] = '\0';

            // wait until fully started
            err = nsb_wait_ready(si.shortname);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "nsb_wait_ready on %s failed", si.shortname);
            }

            si.shortname[si.shortnamelen] = c;

            // HACK:  make sure we use the local versions of a service if
            // it was started. Really there needs to be a mechanism for that
            // service to signal us and others to do this once it has started
            // up.
            set_local_bindings();
        }

        free(si.cmdargs);
        free(si.name);
    }
}

void spawn_arrakis_domains(void)
{
    struct spawn_info si;
    size_t bmpos = 0;
    errval_t err;
    int r;

    coreid_t my_coreid = disp_get_core_id();

    while (true) {

        r = prepare_spawn(&bmpos, &si);
        if (r == 0) {
            return;
        } else if (r == -1) {
            DEBUG_ERR(STARTD_ERR_BOOTMODULES,
                      "failed to read bootmodules entry");
        }

        /* Only spawn special arrakis modules */
        if (si.argc >= 2 && strcmp(si.argv[1], "arrakis") == 0) {

            coreid_t coreid;
            int extra_args;

            // get core id
            if (si.argc >= 3 && strncmp(si.argv[2], "core=", 5) == 0) {

                char *p = strchr(si.argv[2], '=');
                assert(p != NULL);
                coreid = strtol(p + 1, NULL, 10);
                extra_args = 2;

            } else {
                coreid = my_coreid;
                extra_args = 1;
            }

            // discard 'dist-serv' and 'core=x' argument
            for (int i = 1; i <= si.argc - extra_args; i++) {
                si.argv[i] = si.argv[i+extra_args];
            }
            si.argc--;

            debug_printf("starting arrakis domain %s on core %d\n", si.name, coreid);

            domainid_t new_domain;
            err = spawn_arrakis_program(coreid, si.name, si.argv, environ,
					NULL_CAP, NULL_CAP, 0, &new_domain);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "spawn of %s failed", si.name);
                continue;
            }
        }

        free(si.cmdargs);
        free(si.name);
    }
}

void spawn_app_domains(void)
{
    struct spawn_info si;
    size_t bmpos = 0;
    errval_t err;
    int r;

    coreid_t my_coreid = disp_get_core_id();

    while (true) {

        bool spawn_here = true;

        r = prepare_spawn(&bmpos, &si);
        if (r == 0) {
            return;
        } else if (r == -1) {
            DEBUG_ERR(STARTD_ERR_BOOTMODULES,
                      "failed to read bootmodules entry");
        }

        /* Do not spawn special domains */
        if (strncmp(si.shortname, "init", si.shortnamelen) == 0
            || strncmp(si.shortname, "cpu", si.shortnamelen) == 0
                // Adding following condition for cases like "cpu_omap44xx"
            || strncmp(si.shortname, "cpu", strlen("cpu")) == 0
            || strncmp(si.shortname, "monitor", si.shortnamelen) == 0
            || strncmp(si.shortname, "mem_serv", si.shortnamelen) == 0
        ) {
            spawn_here = false;
        }

        /* Do not spawn special boot modules, dist-serv modules
           or nospawn modules */
        if (si.argc >= 2 && (strcmp(si.argv[1], "boot") == 0
                          || strcmp(si.argv[1], "dist-serv") == 0
                          || strcmp(si.argv[1], "nospawn") == 0
                          || strcmp(si.argv[1], "arrakis") == 0
                          || strcmp(si.argv[1], "auto") == 0)) {
            spawn_here = false;
        }

        if (spawn_here) {

            coreid_t coreid;

            // get core id
            if (si.argc >= 2 && strncmp(si.argv[1], "core=", 5) == 0) {

                char *p = strchr(si.argv[1], '=');
                assert(p != NULL);

                p++;
                while(*p != '\0') {
                    int id_from = strtol(p, (char **)&p, 10), id_to = id_from;
                    if(*p == '-') {
                        p++;
                        id_to = strtol(p, (char **)&p, 10);
                    }
                    assert(*p == ',' || *p == '\0');
                    if(*p != '\0') {
                        p++;
                    }

                    /* coreid = strtol(p + 1, NULL, 10); */
                    // discard 'core=x' argument
                    for (int i = 1; i < si.argc; i++) {
                        si.argv[i] = si.argv[i+1];
                    }
                    si.argc--;

                    for(int i = id_from; i <= id_to; i++) {
                        debug_printf("starting app %s on core %d\n",
                                si.name, i);

                        domainid_t new_domain;
                        err = spawn_program(i, si.name, si.argv, environ,
                                            0, &new_domain);
                        if (err_is_fail(err)) {
                            DEBUG_ERR(err, "spawn of %s failed", si.name);
                        }
                    }
                }
            } else {
                coreid = my_coreid;

                debug_printf("starting app %s on core %d\n", si.name, coreid);

                domainid_t new_domain;
                err = spawn_program(coreid, si.name, si.argv, environ,
                                    0, &new_domain);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "spawn of %s failed", si.name);
                }
            }
        }

        free(si.cmdargs);
        free(si.name);
    }

}

void spawn_bootscript_domains(void)
{
    errval_t err;
    coreid_t my_coreid = disp_get_core_id();
    char *argv[256], *name;

    // open bootmodules file and read it in
    FILE *f = fopen("/bootscript", "r");
    if(f == NULL) {
        printf("No bootscript\n");
        return;
    }
    char line[1024];
    while(fgets(line, 1024, f) != NULL) {
        int argc;

        // ignore comments (#) and empty lines
        if (line[0] == '#' || line[0] == '\n') {
            continue;
        }

        argv[0] = strtok(line, " \n");
        name = argv[0];
        for(argc = 1;; argc++) {
            argv[argc] = strtok(NULL, " \n");
            if(argv[argc] == NULL) {
                break;
            }
        }

        // get core id
        if (argc >= 2 && strncmp(argv[1], "core=", 5) == 0) {
            char *p = strchr(argv[1], '=');
            assert(p != NULL);

            p++;
            while(*p != '\0') {
                int id_from = strtol(p, (char **)&p, 10), id_to = id_from;
                if(*p == '-') {
                    p++;
                    id_to = strtol(p, (char **)&p, 10);
                }
                assert(*p == ',' || *p == '\0');
                if(*p != '\0') {
                    p++;
                }

                /* coreid = strtol(p + 1, NULL, 10); */
                // discard 'core=x' argument
                for (int i = 1; i < argc; i++) {
                    argv[i] = argv[i+1];
                }
                argc--;

                for(int i = id_from; i <= id_to; i++) {
                    debug_printf("starting app %s on core %d\n", name, i);

                    domainid_t new_domain;
                    err = spawn_program(i, name, argv, environ,
                                        0, &new_domain);
                    if (err_is_fail(err)) {
                        DEBUG_ERR(err, "spawn of %s failed", name);
                    }
                }
            }
        } else {
            debug_printf("starting app %s on core %d\n", name, my_coreid);

            domainid_t new_domain;
            err = spawn_program(my_coreid, name, argv, environ,
                                0, &new_domain);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "spawn of %s failed", name);
            }
        }
    }

    fclose(f);
}
