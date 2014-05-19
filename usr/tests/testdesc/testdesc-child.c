/** \file
 *  \brief inheritance of file descriptors
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/init.h>

#include "testdesc.h"


static void print_file_fd(void *handle)
{
    vfs_handle_t *vh = *(vfs_handle_t**)handle;

    printf("reading from: %p\n", handle);
    printf("FILE\n\thandle: %p\n", vh);
}

static void print_unixsock_fd(void *handle)
{
    struct _unix_socket *ush = (struct _unix_socket *)handle;

    printf("reading from: %p\n", handle);
    printf("UNIX socket\n\ttype: %x protocol: %x\n\tpassive: %d nonblkng: %d\n",
           ush->type, ush->protocol, ush->passive, ush->nonblocking);
}

static errval_t get_inherited_fds(void) 
{
    errval_t err;

    /* Map the FD buffer into our address space.
     * It stays there since the FD data structures will remain in there and be 
     * referenced from the FD table.
     */
    struct capref frame = {
        .cnode = cnode_task,
        .slot = TASKCN_SLOT_FDSPAGE,
    };

    void *fdspg;
    err = vspace_map_one_frame(&fdspg, FDS_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_FDSPG_TO_SELF);
    }

    /* Set up to read the table */
    char *p = fdspg;
    printf("fds at: %p\n", p);

    int num_fds = *((int*)p);
    printf("num fds: %d\n", num_fds);

    struct fd_store *fd;
    p += sizeof(int);
    fd = (struct fd_store*)p;
    p += (sizeof(struct fd_store)*num_fds);

    /* Process all the FDs passed in the buffer */
    int i;
    for (i = 0; i < num_fds; i++, fd++) {

        /* add each to our fd table - replacing any fds already there */
        struct fdtab_entry fde;
        fde.type = fd->type;
        fde.handle = fd->handle;

        if (fdtab_get(fd->num)->type != FDTAB_TYPE_AVAILABLE) {
            fdtab_free(fd->num);
        }
        fdtab_alloc_from(&fde, fd->num);

        /* print out some info about the FD */

        char *s = "";
        switch (fd->type) {
        case FDTAB_TYPE_AVAILABLE:
            s = "available";
            break;
        case FDTAB_TYPE_FILE:
            s = "file";
            break;
        case FDTAB_TYPE_UNIX_SOCKET:
            s = "unix socket";
            break;
        case FDTAB_TYPE_STDIN:
            s = "stdin";
            break;
        case FDTAB_TYPE_STDOUT:
            s = "stdout";
            break;
        case FDTAB_TYPE_STDERR:
            s = "stderr";
            break;
        case FDTAB_TYPE_LWIP_SOCKET:
            s = "lwip socket";
            break;
        case FDTAB_TYPE_EPOLL_INSTANCE:
            s = "epoll instance";
            break;
        case FDTAB_TYPE_PTM:
            s = "pseudo-terminal master";
            break;
        case FDTAB_TYPE_PTS:
            s = "pseudo-terminal slave";
            break;
        }
        printf("fd_store %d: num: %d, type: %d:%s handle: %p\n", 
               i, fd->num, fd->type, s, fd->handle);

        switch (fd->type) {
        case FDTAB_TYPE_FILE:
            print_file_fd((void*)(p + (genpaddr_t)fd->handle));
            break;
        case FDTAB_TYPE_UNIX_SOCKET:
            print_unixsock_fd((void*)(p + (genpaddr_t)fd->handle));
            break;
        default:
            printf("[no handle data]\n");
            break;
        }

    }

    return SYS_ERR_OK;

}

int main(int argc, char *argv[]) 
{
    errval_t err;

    printf("Child. will print out file descriptors\n");

    /* Inherit all the FDs sent over */
    err = get_inherited_fds();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not get inherited FDs\n");
        return EXIT_FAILURE;
    }

    /* print out all of our FDs */
    int i;
    struct fdtab_entry *fde;
    for (i = MIN_FD; i < MAX_FD; i++) {
        fde = fdtab_get(i);
        if (fde->type != FDTAB_TYPE_AVAILABLE) {
            printf("fd[%d]: type: %d, handle: %p\n", i, fde->type, fde->handle);
        }
    }

    return EXIT_SUCCESS;
}
