/**
 * \file
 * \brief Client for interacting with the spawn daemon on each core
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/cpu_arch.h>
#include <if/spawn_rpcclient_defs.h>
#include <if/arrakis_rpcclient_defs.h>
#include <vfs/vfs_path.h>

extern char **environ;

struct spawn_bind_retst {
    errval_t err;
    struct spawn_binding *b;
    bool present;
};

struct arrakis_bind_retst {
    errval_t err;
    struct arrakis_binding *b;
    bool present;
};

static void spawn_bind_cont(void *st, errval_t err, struct spawn_binding *b)
{
    struct spawn_bind_retst *retst = st;
    assert(retst != NULL);
    assert(!retst->present);
    retst->err = err;
    retst->b = b;
    retst->present = true;
}

static void arrakis_bind_cont(void *st, errval_t err, struct arrakis_binding *b)
{
    struct arrakis_bind_retst *retst = st;
    assert(retst != NULL);
    assert(!retst->present);
    retst->err = err;
    retst->b = b;
    retst->present = true;
}

static struct spawn_binding *spawn_b = NULL;

static errval_t bind_client(coreid_t coreid)
{
    struct spawn_rpc_client *cl;
    errval_t err = SYS_ERR_OK;

    // do we have a spawn client connection for this core?
    assert(coreid < MAX_CPUS);
    cl = get_spawn_rpc_client(coreid);
    if (cl == NULL) {
        char namebuf[16];
        snprintf(namebuf, sizeof(namebuf), "spawn.%u", coreid);
        namebuf[sizeof(namebuf) - 1] = '\0';

        iref_t iref;
        err = nameservice_lookup(namebuf, &iref);
        if (err_is_fail(err)) {
            //DEBUG_ERR(err, "spawn daemon on core %u not found\n", coreid);
            return err;
        }

       // initiate bind
        struct spawn_bind_retst bindst = { .present = false };
        err = spawn_bind(iref, spawn_bind_cont, &bindst, get_default_waitset(),
                         IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawn_bind failed");
            return err;
        }

        // XXX: block for bind completion
        while (!bindst.present) {
            messages_wait_and_handle_next();
        }

        if(err_is_fail(bindst.err)) {
            return bindst.err;
        }

        spawn_b = bindst.b;

        cl = malloc(sizeof(struct spawn_rpc_client));
        if (cl == NULL) {
            return err_push(err, LIB_ERR_MALLOC_FAIL);
        }

        err = spawn_rpc_client_init(cl, bindst.b);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawn_rpc_client_init failed");
            return err;
       }

        set_spawn_rpc_client(coreid, cl);
    }

    return err;
}


/**
 * \brief Request the spawn daemon on a specific core to spawn a program
 *
 * \param coreid        Core ID on which to spawn the program
 * \param path          Absolute path in the file system to an executable image
 *                      suitable for the given core
 * \param argv          Command-line arguments, NULL-terminated
 * \param envp          Optional environment, NULL-terminated
 *                      (pass NULL to inherit)
 * \param inheritcn_cap Cap to a CNode containing capabilities to be inherited
 * \param argcn_cap     Cap to a CNode containing capabilities passed as
 *                      arguments
 * \param flags         Flags to spawn
 * \param ret_domainid  If non-NULL, filled in with domain ID of program
 *
 * \bug flags are currently ignored
 */
errval_t spawn_program_with_caps(coreid_t coreid, const char *path,
                                 char *const argv[], char *const envp[],
                                 struct capref inheritcn_cap,
                                 struct capref argcn_cap, spawn_flags_t flags,
                                 domainid_t *ret_domainid)
{
    struct spawn_rpc_client *cl;
    errval_t err, msgerr;

    // default to copying our environment
    if (envp == NULL) {
        envp = environ;
    }

    // do we have a spawn client connection for this core?
    assert(coreid < MAX_CPUS);
    cl = get_spawn_rpc_client(coreid);
    if (cl == NULL) {
        char namebuf[16];
        snprintf(namebuf, sizeof(namebuf), "spawn.%u", coreid);
        namebuf[sizeof(namebuf) - 1] = '\0';

        iref_t iref;
        err = nameservice_blocking_lookup(namebuf, &iref);
        if (err_is_fail(err)) {
            //DEBUG_ERR(err, "spawn daemon on core %u not found\n", coreid);
            return err;
        }

        // initiate bind
        struct spawn_bind_retst bindst = { .present = false };
        err = spawn_bind(iref, spawn_bind_cont, &bindst, get_default_waitset(),
                         IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn_bind failed");
        }

        // XXX: block for bind completion
        while (!bindst.present) {
            messages_wait_and_handle_next();
        }

        if(err_is_fail(bindst.err)) {
            USER_PANIC_ERR(bindst.err, "asynchronous error during spawn_bind");
        }
        assert(bindst.b != NULL);

        cl = malloc(sizeof(struct spawn_rpc_client));
        assert(cl != NULL);

        err = spawn_rpc_client_init(cl, bindst.b);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "spawn_rpc_client_init failed");
        }

        set_spawn_rpc_client(coreid, cl);
    }

    // construct argument "string"
    // \0-separated strings in contiguous character buffer
    // this is needed, as flounder can't send variable-length arrays of strings
    size_t argstrlen = 0;
    for (int i = 0; argv[i] != NULL; i++) {
        argstrlen += strlen(argv[i]) + 1;
    }

    char argstr[argstrlen];
    size_t argstrpos = 0;
    for (int i = 0; argv[i] != NULL; i++) {
        strcpy(&argstr[argstrpos], argv[i]);
        argstrpos += strlen(argv[i]);
        argstr[argstrpos++] = '\0';
    }
    assert(argstrpos == argstrlen);

    // repeat for environment
    size_t envstrlen = 0;
    for (int i = 0; envp[i] != NULL; i++) {
        envstrlen += strlen(envp[i]) + 1;
    }

    char envstr[envstrlen];
    size_t envstrpos = 0;
    for (int i = 0; envp[i] != NULL; i++) {
        strcpy(&envstr[envstrpos], envp[i]);
        envstrpos += strlen(envp[i]);
        envstr[envstrpos++] = '\0';
    }
    assert(envstrpos == envstrlen);


    domainid_t domain_id;

    // make an unqualified path absolute using the $PATH variable
    // TODO: implement search (currently assumes PATH is a single directory)
    char *searchpath = getenv("PATH");
    if (searchpath == NULL) {
        searchpath = VFS_PATH_SEP_STR; // XXX: just put it in the root
    }
    size_t buflen = strlen(path) + strlen(searchpath) + 2;
    char pathbuf[buflen];
    if (path[0] != VFS_PATH_SEP) {
        snprintf(pathbuf, buflen, "%s%c%s", searchpath, VFS_PATH_SEP, path);
        pathbuf[buflen - 1] = '\0';
        //vfs_path_normalise(pathbuf);
        path = pathbuf;
    }

    if (capref_is_null(inheritcn_cap) && capref_is_null(argcn_cap)) {
        err = cl->vtbl.spawn_domain(cl, path, argstr, argstrlen,
                                    envstr, envstrlen,
                                    &msgerr, &domain_id);
    } else {
        err = cl->vtbl.spawn_domain_with_caps(cl, path, argstr, argstrlen,
                                              envstr, envstrlen, inheritcn_cap,
                                              argcn_cap, &msgerr, &domain_id);
    }
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending spawn request");
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }

    if (ret_domainid != NULL) {
        *ret_domainid = domain_id;
    }

    return msgerr;
}

errval_t spawn_arrakis_program(coreid_t coreid, const char *path,
                               char *const argv[], char *const envp[],
                               struct capref inheritcn_cap,
                               struct capref argcn_cap, spawn_flags_t flags,
                               domainid_t *ret_domainid)
{
    struct arrakis_rpc_client *cl;
    errval_t err, msgerr;

    // default to copying our environment
    if (envp == NULL) {
        envp = environ;
    }

    // do we have a arrakis client connection for this core?
    assert(coreid < MAX_CPUS);
    cl = get_arrakis_rpc_client(coreid);
    if (cl == NULL) {
        char namebuf[16];
        snprintf(namebuf, sizeof(namebuf), "arrakis.%u", coreid);
        namebuf[sizeof(namebuf) - 1] = '\0';

        iref_t iref;
        err = nameservice_blocking_lookup(namebuf, &iref);
        if (err_is_fail(err)) {
            //DEBUG_ERR(err, "arrakis daemon on core %u not found\n", coreid);
            return err;
        }

        // initiate bind
        struct arrakis_bind_retst bindst = { .present = false };
        err = arrakis_bind(iref, arrakis_bind_cont, &bindst, get_default_waitset(),
                           IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "arrakis_bind failed");
        }

        // XXX: block for bind completion
        while (!bindst.present) {
            messages_wait_and_handle_next();
        }

        if(err_is_fail(bindst.err)) {
            USER_PANIC_ERR(bindst.err, "asynchronous error during arrakis_bind");
        }
        assert(bindst.b != NULL);

        cl = malloc(sizeof(struct arrakis_rpc_client));
        assert(cl != NULL);

        err = arrakis_rpc_client_init(cl, bindst.b);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "arrakis_rpc_client_init failed");
        }

        set_arrakis_rpc_client(coreid, cl);
    }

    // construct argument "string"
    // \0-separated strings in contiguous character buffer
    // this is needed, as flounder can't send variable-length arrays of strings
    size_t argstrlen = 0;
    for (int i = 0; argv[i] != NULL; i++) {
        argstrlen += strlen(argv[i]) + 1;
    }

    char argstr[argstrlen];
    size_t argstrpos = 0;
    for (int i = 0; argv[i] != NULL; i++) {
        strcpy(&argstr[argstrpos], argv[i]);
        argstrpos += strlen(argv[i]);
        argstr[argstrpos++] = '\0';
    }
    assert(argstrpos == argstrlen);

    // repeat for environment
    size_t envstrlen = 0;
    for (int i = 0; envp[i] != NULL; i++) {
        envstrlen += strlen(envp[i]) + 1;
    }

    char envstr[envstrlen];
    size_t envstrpos = 0;
    for (int i = 0; envp[i] != NULL; i++) {
        strcpy(&envstr[envstrpos], envp[i]);
        envstrpos += strlen(envp[i]);
        envstr[envstrpos++] = '\0';
    }
    assert(envstrpos == envstrlen);


    domainid_t domain_id;

    // make an unqualified path absolute using the $PATH variable
    // TODO: implement search (currently assumes PATH is a single directory)
    char *searchpath = getenv("PATH");
    if (searchpath == NULL) {
        searchpath = VFS_PATH_SEP_STR; // XXX: just put it in the root
    }
    size_t buflen = strlen(path) + strlen(searchpath) + 2;
    char pathbuf[buflen];
    if (path[0] != VFS_PATH_SEP) {
        snprintf(pathbuf, buflen, "%s%c%s", searchpath, VFS_PATH_SEP, path);
        pathbuf[buflen - 1] = '\0';
        //vfs_path_normalise(pathbuf);
        path = pathbuf;
    }

    err = cl->vtbl.spawn_arrakis_domain(cl, path, argstr, argstrlen,
                                        envstr, envstrlen, &msgerr, &domain_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending arrakis request");
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }

    if (ret_domainid != NULL) {
        *ret_domainid = domain_id;
    }

    return msgerr;
}


/**
 * \brief Request the spawn daemon on a specific core to spawn a program
 *
 * \param coreid Core ID on which to spawn the program
 * \param path   Absolute path in the file system to an executable image
 *                        suitable for the given core
 * \param argv   Command-line arguments, NULL-terminated
 * \param envp   Optional environment, NULL-terminated (pass NULL to inherit)
 * \param flags  Flags to spawn
 * \param ret_domainid If non-NULL, filled in with domain ID of program
 *
 * \bug flags are currently ignored
 */
errval_t spawn_program(coreid_t coreid, const char *path,
                             char *const argv[], char *const envp[],
                             spawn_flags_t flags, domainid_t *ret_domainid)
{
    return spawn_program_with_caps(coreid, path, argv, envp, NULL_CAP,
                                   NULL_CAP, flags, ret_domainid);
}    



/**
 * \brief Request a program be spawned on all cores in the system
 *
 * \param same_core Iff false, don't spawn on the same core as the caller
 * \param path   Absolute path in the file system to an executable image
 *                        suitable for the given core
 * \param argv   Command-line arguments, NULL-terminated
 * \param envp   Optional environment, NULL-terminated (pass NULL to inherit)
 * \param flags  Flags to spawn
 * \param ret_domainid If non-NULL, filled in with domain ID of program
 *
 * \note This function is for legacy compatibility with existing benchmark/test
 *    code, and SHOULD NOT BE USED IN NEW CODE UNLESS YOU HAVE A GOOD REASON!
 *    It doesn't make much sense from a scalability perspective, and is
 *    probably useless on a heterogeneous system.
 */
errval_t spawn_program_on_all_cores(bool same_core, const char *path,
                                    char *const argv[], char *const envp[],
                                    spawn_flags_t flags, domainid_t *ret_domainid)
{
    errval_t err;

    // TODO: handle flags, domain ID

    // FIXME: world's most broken implementation...
    for (coreid_t c = 0; c < MAX_CPUS; c++) {
        if (!same_core && c == disp_get_core_id()) {
            continue;
        }

        // Check first whether the spawnd exists
        char namebuf[16];
        snprintf(namebuf, sizeof(namebuf), "spawn.%u", c);
        namebuf[sizeof(namebuf) - 1] = '\0';

        iref_t iref;
        err = nameservice_lookup(namebuf, &iref);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NAMESERVICE_UNKNOWN_NAME) {
                continue; // no spawn daemon on this core
            }
            //DEBUG_ERR(err, "spawn daemon on core %u not found\n", coreid);
            return err;
        }

        err = spawn_program(c, path, argv, envp, flags, NULL);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NAMESERVICE_UNKNOWN_NAME) {
                continue; // no spawn daemon on this core
            } else {
                DEBUG_ERR(err, "error spawning %s on core %u\n", path, c);
                return err;
            }
        }
    }

    return SYS_ERR_OK;
}

errval_t spawn_rpc_client(coreid_t coreid, struct spawn_rpc_client **ret_client)
{
    errval_t err = bind_client(coreid);
    if(err_is_fail(err)) {
        return err;
    }

    *ret_client = get_spawn_rpc_client(coreid);
    return SYS_ERR_OK;
}

/**
 * \brief Kill a domain.
 */
errval_t spawn_kill(domainid_t domainid)
{
    errval_t err, reterr;

    err = bind_client(disp_get_core_id());
    if (err_is_fail(err)) {
        return err;
    }
    struct spawn_rpc_client *cl = get_spawn_rpc_client(disp_get_core_id());
    assert(cl != NULL);

    err = cl->vtbl.kill(cl, domainid, &reterr);
    if(err_is_fail(err)) {
        return err;
    }

    return reterr;
}

/**
 * \brief Exit this domain.
 */
errval_t spawn_exit(uint8_t exitcode)
{
    errval_t err;

    err = bind_client(disp_get_core_id());
    if (err_is_fail(err)) {
        return err;
    }
    struct spawn_rpc_client *cl = get_spawn_rpc_client(disp_get_core_id());
    assert(cl != NULL);

    err = cl->vtbl.exit(cl, disp_get_domain_id(), exitcode);
    if(err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Wait for spawned proccess to exit on core.
 */
errval_t spawn_wait_coreid(coreid_t coreid, domainid_t domainid, uint8_t *exitcode, bool nohang)
{
    return spawn_wait_core(disp_get_core_id(), domainid, exitcode, nohang);
}

/**
 * \brief Wait for the termination of a domain on a remote core.
 */
errval_t spawn_wait_core(coreid_t coreid, domainid_t domainid,
                         uint8_t *exitcode, bool nohang)
{
    errval_t err, reterr;

    err = bind_client(coreid);
    if (err_is_fail(err)) {
        return err;
    }
    struct spawn_rpc_client *cl = get_spawn_rpc_client(coreid);
    assert(cl != NULL);

    err = cl->vtbl.wait(cl, domainid, nohang, exitcode, &reterr);
    if(err_is_fail(err)) {
        return err;
    }

    return reterr;
}

/**
 * \brief Wait for spawned proccess to exit on current core.
 */
errval_t spawn_wait(domainid_t domainid, uint8_t *exitcode, bool nohang)
{
    return spawn_wait_coreid(disp_get_core_id(), domainid, exitcode, nohang);
}

/**
 * \brief Get the list of domains for ps like implementation
 */
errval_t spawn_get_domain_list(uint8_t **domains, size_t *len)
{
    errval_t err;

    struct spawn_rpc_client *cl;
    err = spawn_rpc_client(disp_get_core_id(), &cl);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn_rpc_client");
    }
    assert(cl != NULL);

    err = cl->vtbl.get_domainlist(cl, domains, len);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_domainlist");
    }

    return SYS_ERR_OK;
}

/**
 * \brief Get the status of a domain for ps like implementation
 */
errval_t spawn_get_status(uint8_t domain, struct spawn_ps_entry *pse,
                          char **argbuf, size_t *arglen, errval_t *reterr)
{
    errval_t err;

    struct spawn_rpc_client *cl;
    err = spawn_rpc_client(disp_get_core_id(), &cl);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn_rpc_client");
    }
    assert(cl != NULL);

    err = cl->vtbl.status(cl, domain, (spawn_ps_entry_t*)pse, argbuf, arglen, reterr);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "status");
    }

    return SYS_ERR_OK;
}

/**
 * \brief Utility function to create an inherit cnode and copy fdcap into it.
 *
 * \param inheritcn_capp Pointer to capref, filled-in with location of inheritcn
 *                       capability.
 * \param fdcap          fdcap to copy into inherit cnode.
 */
errval_t alloc_inheritcn_with_fdcap(struct capref *inheritcn_capp,
                                    struct capref fdcap)
{
    errval_t err;

    // construct inherit CNode
    struct cnoderef inheritcn;
    err = cnode_create(inheritcn_capp, &inheritcn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    if (capref_is_null(fdcap)) {
        return SYS_ERR_OK;
    }

    // copy fdcap to inherit Cnode
    struct capref dest = {
        .cnode = inheritcn,
        .slot  = INHERITCN_SLOT_FDSPAGE
    };
    err = cap_copy(dest, fdcap);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Utility function to create an inherit cnode and copy session
 *        capability into it.
 *
 * \param inheritcn_capp Pointer to capref, filled-in with location of inheritcn
 *                       capability.
 * \param sidcap         sidcap to copy into inherit cnode.
 */
errval_t alloc_inheritcn_with_sidcap(struct capref *inheritcn_capp,
                                    struct capref sidcap)
{
    errval_t err;

    // construct inherit CNode
    struct cnoderef inheritcn;
    err = cnode_create(inheritcn_capp, &inheritcn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    if (capref_is_null(sidcap)) {
        return SYS_ERR_OK;
    }

    // copy fdcap to inherit Cnode
    struct capref dest = {
        .cnode = inheritcn,
        .slot  = INHERITCN_SLOT_SESSIONID
    };
    err = cap_copy(dest, sidcap);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
