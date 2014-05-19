/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#define _USE_XOPEN
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

/* XXX: for MAX_ENVIRON_VARS */
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/init.h>

// statically initialise environ to a sane empty environment array
// this may be changed at domain startup if an environment is present
static char *empty_environ[128] = { NULL };
char **environ = empty_environ;

char *getenv(const char *name)
{
    assert(environ != NULL);
    assert(name != NULL);

    size_t namelen = strlen(name);

    for (int i = 0; environ[i] != NULL; i++) {
        char *sep = strchr(environ[i], '=');
        if (sep != NULL && sep - environ[i] == namelen
            && strncmp(name, environ[i], namelen) == 0) {
            return sep + 1;
        }
    }

    return NULL;
}

int setenv(const char *name, const char *value, int overwrite)
{
    assert(environ != NULL);
    assert(value != NULL);

    if (name == NULL || *name == '\0' || strchr(name, '=') != NULL) {
        errno = EINVAL;
        return -1;
    }

    size_t namelen = strlen(name);

    // search for matching entry in the environment
    int i;
    for (i = 0; environ[i] != NULL; i++) {
        char *sep = strchr(environ[i], '=');
        if (sep != NULL && sep - environ[i] == namelen
            && strncmp(name, environ[i], namelen) == 0) {
            // found an existing entry
            if (overwrite) {
                break;
            } else {
                return 0;
            }
        }
    }

    if (environ[i] != NULL) {
        // FIXME: this might be an entry we need to free, or it might live in
        // the fixed arguments page. we can't tell here, so must leak it
    } else if (i + 1 > MAX_ENVIRON_VARS) {
        errno = ENOMEM;
        return -1;
    } else {
        environ[i + 1] = NULL;
    }

    char *newentry = malloc(namelen + strlen(value) + 2);
    assert(newentry != NULL);

    strcpy(newentry, name);
    newentry[namelen] = '=';
    strcpy(&newentry[namelen + 1], value);

    environ[i] = newentry;

    return 0;
}

int unsetenv(const char *name)
{
    assert(environ != NULL);

    if (name == NULL || *name == '\0' || strchr(name, '=') != NULL) {
        errno = EINVAL;
        return -1;
    }

    size_t namelen = strlen(name);

    // search for matching entry in the environment
    for (int i = 0; environ[i] != NULL; i++) {
        char *sep = strchr(environ[i], '=');
        if (sep != NULL && sep - environ[i] == namelen
            && strncmp(name, environ[i], namelen) == 0) {
            // unset the existing entry
            // XXX: Currently done by replacing with empty string. Could
            // reclaim memory here.
            environ[i][0] = '\0';
            break;
        }
    }

    return 0;
}

int putenv(char *string)
{
    assert(environ != NULL);

    if (string == NULL || *string == '\0' || strchr(string, '=') == NULL) {
        errno = EINVAL;
        return -1;
    }

    size_t namelen = strchr(string, '=') - string;

    // search for matching entry in the environment
    int i;
    for (i = 0; environ[i] != NULL; i++) {
        char *sep = strchr(environ[i], '=');
        if (sep != NULL && sep - environ[i] == namelen
            && strncmp(string, environ[i], namelen) == 0) {
            // found an existing entry
            break;
        }
    }

    if (environ[i] != NULL) {
        // FIXME: this might be an entry we need to free, or it might live in
        // the fixed arguments page. we can't tell here, so must leak it
    } else if (i + 1 > MAX_ENVIRON_VARS) {
        errno = ENOMEM;
        return -1;
    } else {
        environ[i + 1] = NULL;
    }

    environ[i] = string;
    return 0;
}
