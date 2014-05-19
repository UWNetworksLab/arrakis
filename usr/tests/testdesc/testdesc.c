/** \file
 *  \brief inheritance of file descriptors
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include "testdesc.h"

/* Copying the actual handles is hard.
 * We could try a deep copy, but really we should come up with a serialised
 * format for each type.  It also involves implementing the underlying
 * resources such that the descriptors can actually be used in the new 
 * dispatcher.
 */

static size_t copy_file_fd(void *dest, genpaddr_t offset, struct fd_store *fds)
{ 
    size_t size = sizeof(void *);

    printf("FILE\n\thandle: %p\n", fds->handle);

    /* This following isn't correct at all - we're just copying the value of 
     * the pointer, which is useless in the new dispatcher */

    printf("copying %zu bytes from %p to %p\n", size, &fds->handle, dest);
    memcpy(dest, &fds->handle, size);
    fds->handle = (void*)(offset);
    printf("fd %d fixed handle is: %p\n", fds->num, fds->handle);

    return size;
}


static size_t copy_unixsock_fd(void *dest, genpaddr_t offset, 
                                 struct fd_store *fds)
{
    // shallow copy. doesn't really help us.

    struct _unix_socket *ush;
    size_t size;

    ush = fds->handle;
    printf("adding UNIX socket (%p)\n\ttype: %x protocol: %x\n\tpassive: %d "
           "nonblkng: %d\n",
           fds->handle, ush->type, ush->protocol, ush->passive, ush->nonblocking);
    size = sizeof(struct _unix_socket);

    printf("copying %zu bytes from %p to %p\n", size, fds->handle, dest);
    memcpy(dest, fds->handle, size);
    fds->handle = (void*)(offset);
    printf("fd %d fixed handle is: %p\n", fds->num, fds->handle);

    return size;
}


/**
 * \brief Setup inherited file descriptors
 *
 */
static errval_t spawn_setup_fds(struct capref *frame)
{
    errval_t err;
    void *fdspg;

    // Create frame (actually multiple pages) for fds
    err = frame_alloc(frame, FDS_SIZE, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_FDSPG);
    }

    // map it in so we can write to it
    err = vspace_map_one_frame(&fdspg, FDS_SIZE, *frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_FDSPG_TO_SELF);
    }

    /* Layout of FD page:
     * int num_fds
     * struct fd_store fdtab[num_fds]
     * uint8_t buf[] // store of actual handle data.  entries in fdtab above 
     *               // point here (relative offset from the beginning of buf).
     * TODO: add the actual handle data!
     */
    int *num_fds = (int *)fdspg;
    *num_fds = 0;
    struct fd_store *fdtab = (struct fd_store *)(num_fds + 1);
    
    /* first copy all the fd table entries */
    struct fdtab_entry *fde;
    struct fd_store *fds;
    int i;
    for (i = MIN_FD; i < MAX_FD; i++) {
        fde = fdtab_get(i);
        if (fde->type != FDTAB_TYPE_AVAILABLE) {
            fds = &fdtab[*num_fds];
            fds->num = i;
            fds->type = fde->type;
            fds->handle = fde->handle;
            printf("added fd %d to fdtabs[%d]: %p as fd_store (%p: num: %d, "
                    "type: %d, (unfixed)handle: %p)\n",
                    i, *num_fds, &fdtab[*num_fds], fds, fds->num, fds->type, 
                    fds->handle);
            (*num_fds)++;
        }
    }

    /* then copy all the handle data to the buffer */
    char *buf = (char *)&fdtab[*num_fds];
    char *dest = buf;
    genpaddr_t offset;
    size_t size;
    for (i = 0; i < *num_fds; i++) {
        fds =  &fdtab[i];
        offset = (genpaddr_t)(dest - buf);
        switch (fds->type) {
        case FDTAB_TYPE_FILE:
            size = copy_file_fd(dest, offset, fds); 
            break;
        case FDTAB_TYPE_UNIX_SOCKET:
            size = copy_unixsock_fd(dest, offset, fds);
            break;
        default:
            // nothing to copy
            size = 0;
            break;
        }
        dest += size;
    }

    // unmap frame
    err = vspace_unmap(fdspg);

    return err;
}


static errval_t spawn_child(struct capref fdcap)

{
    errval_t err;

    char *argv[2] = { "testdesc-child", NULL };

    domainid_t new_domain = -1;

    coreid_t core = 0;

    // allocate inheritcn
    struct capref inheritcn_cap;
    err = alloc_inheritcn_with_fdcap(&inheritcn_cap, fdcap);
    
    err = spawn_program_with_caps(core, argv[0], argv, NULL, inheritcn_cap,
                                  NULL_CAP, SPAWN_NEW_DOMAIN, &new_domain);
            
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawn on core %d", core);
        return err;
    }

    return SYS_ERR_OK;

}

int main(int argc, char *argv[]) 
{
    errval_t err;

    printf("Test inheritance of file descriptors\n");

    // create some file handles
    int fd = open("test file", O_CREAT);
    printf("opened a file with fd: %d\n", fd);

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    printf("opened a socket with fd: %d\n", fd);    

    struct capref fdcap;
    err = spawn_setup_fds(&fdcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not setup fds!\n");
        return EXIT_FAILURE;
    }
            
    err = spawn_child(fdcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not spawn child!\n");
        return EXIT_FAILURE;
    }
        
    return EXIT_SUCCESS;
}
