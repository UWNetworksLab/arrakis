/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // strdup() (for oldc)

#include <assert.h>
#include <string.h>
#include <pwd.h>

#include <barrelfish/barrelfish.h>

#include "posixcompat.h"
#include "userdb.h"

/*
 * Cursor in the userdb.
 */
static unsigned int db_cursor = 0;

/*
 * Posix states that the return value from getpwent, getpwnam and getpwuid may
 * point to static area. Conformant applications therefore don't free the
 * memory returned by these functions. To prevent a memory leak, we also use
 * a static memory area.
 */
static struct passwd user;

static void copy_passwd(struct passwd *dest, struct passwd *src)
{
    assert(dest != NULL);

    dest->pw_name   = strdup(src->pw_name);
    dest->pw_passwd = strdup(src->pw_passwd);
    dest->pw_uid    = src->pw_uid;
    dest->pw_gid    = src->pw_gid;
    dest->pw_gecos  = strdup(src->pw_gecos);
    dest->pw_dir    = strdup(src->pw_dir);
    dest->pw_shell  = strdup(src->pw_shell);

    assert(dest->pw_name   != NULL);
    assert(dest->pw_passwd != NULL);
    assert(dest->pw_gecos  != NULL);
    assert(dest->pw_dir    != NULL);
    assert(dest->pw_shell  != NULL);
}

/**
 * \brief Return struct passwd of current user of the user database.
 *
 * When called the first time returns the struct passwd of the first user listed
 * in the user database. Each successive call returns the next user in the
 * user database.
 */
struct passwd *getpwent(void)
{
    struct passwd *ret = NULL;

    if (db_cursor < ARRAY_LENGTH(userdb)) {
        copy_passwd(&user, &userdb[db_cursor++]);
        ret = &user;
        POSIXCOMPAT_DEBUG("getpwent(): returning user \"%s\"\n",
                          ret->pw_name);
    } else {
        ret = NULL;
        POSIXCOMPAT_DEBUG("getpwent(): no more users\n");
    }

    return ret;
}

/**
 * \brief Rewind user database.
 */
void setpwent(void)
{
    db_cursor = 0;
    POSIXCOMPAT_DEBUG("setpwent(): rewind user database\n");
}

/**
 * \brief Close user database.
 */
void endpwent(void)
{
    db_cursor = 0;
    POSIXCOMPAT_DEBUG("endpwent(): rewind user database\n");
}
