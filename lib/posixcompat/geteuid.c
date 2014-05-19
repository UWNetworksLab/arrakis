/*
 * Copyright (c) 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include "posixcompat.h"
#include "userdb.h"

static struct passwd *dummyuser = &userdb[0];

uid_t geteuid(void)
{
    POSIXCOMPAT_DEBUG("geteuid(): returning %d\n", dummyuser->pw_uid);
    return dummyuser->pw_uid;
}

uid_t getuid(void)
{
    POSIXCOMPAT_DEBUG("getuid(): returning %d\n", dummyuser->pw_uid);
    return dummyuser->pw_uid;
}

struct passwd *getpwuid(uid_t uid)
{
    struct passwd* user = NULL;

    setpwent();
    while ((user = getpwent()) != NULL) {
        if (user->pw_uid == uid) {
            POSIXCOMPAT_DEBUG("getpwuid(%d): returning user \"%s\"\n",
                              uid, user->pw_name);
            return user;
        }
    }
    endpwent();

    // No matching user found
    POSIXCOMPAT_DEBUG("getpwuid(%d): no user found\n", uid);
    return NULL;
}

/**
 * \brief Get the effective group ID.
 */
gid_t getegid(void)
{
    POSIXCOMPAT_DEBUG("getegid(): returning %d\n", dummyuser->pw_gid);
    return dummyuser->pw_gid;
}

/**
 * \brief Get the real group ID.
 */
gid_t getgid(void)
{
    POSIXCOMPAT_DEBUG("getgid(): returning %d\n", dummyuser->pw_gid);
    return dummyuser->pw_gid;
}

/**
 * \brief Set the effective group ID.
 */
int setegid(gid_t gid)
{
    assert(!"NYI");
    return -1;
}

/**
 * \brief Set-group-ID.
 */
int setgid(gid_t gid)
{
    assert(!"NYI");
    return -1;
}

struct passwd *getpwnam(const char *name)
{
    struct passwd* user = NULL;

    setpwent();
    while ((user = getpwent()) != NULL) {
        if (strcmp(user->pw_name, name) == 0) {
            POSIXCOMPAT_DEBUG("getpwnam(%s): returning user \"%d\"\n",
                              name, user->pw_uid);
            return user;
        }
    }
    endpwent();

    // No matching user found
    POSIXCOMPAT_DEBUG("getpwnam(%s): no user found\n", name);
    return NULL;
}

/**
 * \brief Set effective user ID.
 */
int seteuid(uid_t uid)
{
    POSIXCOMPAT_DEBUG("seteuid(%d): nothing changed", uid);
    return 0;
}

/**
 * \brief Set user ID.
 */
int setuid(uid_t uid)
{
    POSIXCOMPAT_DEBUG("setuid(%d): nothing changed", uid);
    return 0;
}
