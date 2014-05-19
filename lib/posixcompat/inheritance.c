/*
 * Copyright (c) 2011, ETH Zurich.
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
#include <posixcompat.h>
#include <lwip/sock_serialise.h>
#include <vfs/fdtab.h>

struct fd_store {
    int num;
    enum fdtab_type	type;
    void *handle;
    int fd;
};

struct posixcompat_sockinfo {
};

/* Copying the actual handles is hard.
 * We could try a deep copy, but really we should come up with a serialised
 * format for each type.  It also involves implementing the underlying
 * resources such that the descriptors can actually be used in the new 
 * dispatcher.
 */

static size_t copy_file_fd(void *dest, genpaddr_t offset, struct fd_store *fds)
{ 
    assert(!"NYI");
    return 0;
}

static void debug_uipaddr_print(u32_t addr) 
{
    debug_printf("%"U16_F".%"U16_F".%"U16_F".%"U16_F"\n", 
                 (u16_t)((ntohl(addr) >> 24) & 0xff),
                 (u16_t)((ntohl(addr) >> 16) & 0xff),
                 (u16_t)((ntohl(addr) >> 8) & 0xff),
                 (u16_t)(ntohl(addr) & 0xff));
}

static void debug_ipaddr_print(struct ip_addr *ipaddr) {
    u32_t addr = ipaddr->addr;
    debug_uipaddr_print(addr);

}

static size_t copy_lwip_fd(void *dest, genpaddr_t offset, struct fd_store *fds)
{ 
    size_t size = sizeof(struct lwip_sockinfo);

    struct lwip_sockinfo si;
    lwip_serialise_sock(fds->fd, &si);

    printf("LWIP socket\n\tfd: %d\n", fds->fd);
    debug_printf("local port and ip: %u\n", si.local_port);
    debug_ipaddr_print(&si.local_ip);
    debug_printf("remote port and ip: %u\n", si.remote_port);
    debug_ipaddr_print(&si.remote_ip);

    printf("copying %zu bytes from %p to %p\n", size, &fds->handle, dest);
    memcpy(dest, &si, size);
    fds->handle = (void *)(uintptr_t)(offset);
    printf("fd %d fixed handle is: %d\n", fds->num, fds->fd);

    return size;
}

static size_t copy_unixsock_fd(void *dest, genpaddr_t offset, 
                                 struct fd_store *fds)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief Setup inherited file descriptors
 *
 */
errval_t spawn_setup_fds(struct capref *frame, int rfd)
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
    int i = 0;
    for (i = MIN_FD; i < MAX_FD; i++) {
        fde = fdtab_get(i);
        if (fde->type == FDTAB_TYPE_LWIP_SOCKET) {
            fds = &fdtab[*num_fds];
            fds->num = i;
            fds->type = fde->type;
            fds->handle = fde->handle;
            fds->fd = fde->fd;
            // Mark the inherited fd accordingly
            fde->inherited = 1;
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
        case FDTAB_TYPE_LWIP_SOCKET:
            size = copy_lwip_fd(dest, offset, fds);
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

errval_t posixcompat_unpack_fds(void)
{
    errval_t err;

    /* Map the FD buffer into our address space. */
    struct capref frame = {
        .cnode = cnode_task,
        .slot = TASKCN_SLOT_FDSPAGE,
    };

    void *fdspg;
    err = vspace_map_one_frame(&fdspg, FDS_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MAP_FDSPG_TO_SELF);
    }

    int *num_fds = (int *)fdspg;
    struct fd_store *fdtab = (struct fd_store *)(num_fds + 1);
    char *buf = (char *)&fdtab[*num_fds];

    /* first copy all the fd table entries */
    int i;
    for (i = 0; i < *num_fds; i++) {
        struct fd_store *fds = &fdtab[i];
        struct fdtab_entry fde;

        assert(fds->type == FDTAB_TYPE_LWIP_SOCKET);

        fde.type = FDTAB_TYPE_LWIP_SOCKET;
        fde.fd = lwip_socket(AF_INET, SOCK_STREAM, 0);

        struct lwip_sockinfo *si = (struct lwip_sockinfo *)(buf + (uintptr_t)fds->handle);
        lwip_deserialise_sock(fde.fd, si);
        debug_printf("local port and ip: %u\n", si->local_port);
        debug_ipaddr_print(&si->local_ip);
        debug_printf("remote port and ip: %u\n", si->remote_port);
        debug_ipaddr_print(&si->remote_ip);

        int fd = fdtab_alloc_from(&fde, fds->num);
        assert(fd == fds->num);

        printf("restored fd %d from fdtabs[%d]: %p as fd_store (%p: num: %d, "
               "type: %d, (unfixed)handle: %p)\n",
               fds->num, i, &fdtab[i], fds, fds->num, fds->type,
               fds->handle);

    }

    // unmap frame
    err = vspace_unmap(fdspg);

    return err;
}
