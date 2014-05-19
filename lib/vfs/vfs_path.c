/**
 * \file
 * \brief VFS-related path manipulation
 */

/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>

/**
 * \brief Normalise the given path, in-place
 *
 * Modifies the provided path, performing the following substitutions:
 * 1. successive separators are suppressed: A//B -> A/B
 * 2. the self-referential directory element is suppressed: A/./B -> A/B
 * 3. references to the parent directory are collapsed where possible:
 *       A/foo/../B -> A/B
 * 4. a trailing / is elided
 */
void vfs_path_normalise(char *path)
{
    bool seensep = false, emitted_dotdot = false;
    size_t outpos = 0;
    size_t origlen;

#ifndef NDEBUG
    origlen = strlen(path);
#endif

    for (size_t inpos = 0; path[inpos] != '\0'; inpos++) {
        assert(outpos <= inpos);
        assert(outpos <= origlen);
        if (path[inpos] == VFS_PATH_SEP) { // is this a separator?
            if (!seensep) { // swallow consecutive separators
                path[outpos++] = VFS_PATH_SEP;
                seensep = true;
            }
        } else if (seensep || inpos == 0) { // new element of the path
            if (path[inpos] == '.' &&
                (path[inpos + 1] == VFS_PATH_SEP || path[inpos + 1] == '\0')) {
                // "./" or "." at end of input
                continue;
            } else if (path[inpos] == '.' && path[inpos + 1] == '.' &&
                (path[inpos + 2] == VFS_PATH_SEP || path[inpos + 2] == '\0')) {
                // "../" or ".." at end of input
                // can we consume something we previous emitted?
                if (emitted_dotdot || outpos <= 1) {
                    // nothing to consume, continue anyway
                    emitted_dotdot = true;
                    path[outpos++] = path[inpos];
                    seensep = false;
                } else {
                    // consume previously emitted element, by searching back
                    outpos--;
                    while (outpos > 0 && path[outpos - 1] != VFS_PATH_SEP) {
                        outpos--;
                    }

                    // stop if this is the end of the input string
                    if (path[inpos + 2] == '\0') {
                        break;
                    } else {
                        // discard "../" from input
                        inpos += 2;
                        seensep = outpos > 0;
                    }
                }
            } else { // start of new element that isn't . or .. -- just emit it
                emitted_dotdot = seensep = false;
                path[outpos++] = path[inpos];
            }
        } else {
            // normal case: emit the char
            seensep = false;
            path[outpos++] = path[inpos];
        }
    }

    // remove a trailing / if present, and not the only thing in the path
    if (outpos > 1 && path[outpos - 1] == VFS_PATH_SEP) {
        outpos--;
    }

    // terminate
    path[outpos] = '\0';
}

/**
 * \brief Generate an absolute path, given the current directory
 *
 * This function constructs an absolute path, given the current directory
 * and a path which may be relative or absolute (starts with '/').
 *
 * \returns Path in malloc'ed buffer, which must be freed by the caller
 */
char *vfs_path_mkabsolute(const char *cwd, const char *path)
{
    char *ret;

    if (path[0] == VFS_PATH_SEP) {
        // path is already absolute, just copy it
        ret = strdup(path);
        assert(ret != NULL);
    } else {
        // allocate a buffer, and concatenate the cwd and relative path
        // ... this could be more efficient, if we cared
        ret = malloc(strlen(cwd) + strlen(path) + 2);
        assert(ret != NULL);

        strcpy(ret, cwd);
        strcat(ret, VFS_PATH_SEP_STR);
        strcat(ret, path);
    }

    // normalise the result, in either case
    vfs_path_normalise(ret);

    // optional: to save space after normalising
    // realloc(ret, strlen(ret) + 1);

    return ret;
}

/**
 * \brief Shorthand version of vfs_path_mkabsolute() that "knows" the current directory.
 *
 * \returns Path in malloc'ed buffer, which must be freed by caller.
 */
char *vfs_path_mkabs(const char *path)
{
    char *cwd = getenv("PWD");

    if(cwd == NULL) {
        return strdup(path);
    }

    return vfs_path_mkabsolute(cwd, path);
}
