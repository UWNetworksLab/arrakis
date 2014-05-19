/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <aio.h>
#include <string.h>
#include <errors/errno.h>
#include <storage/vsic.h>
#include <storage/vsa.h>

#define MAX_CBS         10

struct file_vsic {
    struct aiocb cb[MAX_CBS];
    const struct aiocb *cb_list[MAX_CBS];
};

static struct aiocb *get_aiocb(struct file_vsic *vsic)
{
    for(int i = 0; i < MAX_CBS; i++) {
        if(vsic->cb_list[i] == NULL) {
            vsic->cb_list[i] = &vsic->cb[i];
            return &vsic->cb[i];
        }
    }

    return NULL;
}

static errval_t vsic_write(struct storage_vsic *vsic, struct storage_vsa *vsa,
                           off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    struct file_vsic *mydata = vsic->data;
    struct aiocb *cb = get_aiocb(mydata);
    assert(cb != NULL);

    cb->aio_fildes = vsa->fd;
    cb->aio_offset = offset;
    cb->aio_buf = buffer;
    cb->aio_nbytes = size;

    int r = aio_write(cb);
    assert(r == 0);

    return SYS_ERR_OK;
}

static errval_t vsic_read(struct storage_vsic *vsic, struct storage_vsa *vsa,
                          off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    struct file_vsic *mydata = vsic->data;
    struct aiocb *cb = get_aiocb(mydata);
    assert(cb != NULL);

    cb->aio_fildes = vsa->fd;
    cb->aio_offset = offset;
    cb->aio_buf = buffer;
    cb->aio_nbytes = size;

    int r = aio_read(cb);
    assert(r == 0);

    return SYS_ERR_OK;
}

static errval_t vsic_flush(struct storage_vsic *vsic, struct storage_vsa *vsa)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    struct file_vsic *mydata = vsic->data;
    struct aiocb *cb = get_aiocb(mydata);
    assert(cb != NULL);

    cb->aio_fildes = vsa->fd;
    int r = aio_fsync(O_SYNC, cb);
    assert(r == 0);

    return SYS_ERR_OK;
}

static errval_t vsic_wait(struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    struct file_vsic *mydata = vsic->data;

    for(;;) {
        int entries = 0;

        for(int i = 0; i < MAX_CBS; i++) {
            if(mydata->cb_list[i] != NULL) {
                int err = aio_error(mydata->cb_list[i]);

                if(err == 0) {
                    int status = aio_return((struct aiocb *)mydata->cb_list[i]);
                    /* printf("Status: %zd\n", status); */

                    // Completed successfully
                    mydata->cb_list[i] = NULL;

                    if(status == 0) {
                        return VFS_ERR_EOF;
                    }
                } else if(err == EINPROGRESS) {
                    entries++;
                } else {
		  // Error! Report it.
		  printf("Error: %s\n", strerror(err));
		  return VFS_ERR_IN_READ;
                }
            }
        }

        if(entries == 0) {
            break;
        }

        int r = aio_suspend(mydata->cb_list, MAX_CBS, NULL);
        assert(r == 0);
    }

    return SYS_ERR_OK;
}

static struct storage_vsic_ops file_ops = {
    .write = vsic_write,
    .read = vsic_read,
    .flush = vsic_flush,
    .wait = vsic_wait,
};

static errval_t file_vsic_alloc(struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    struct file_vsic *mydata = malloc(sizeof(struct file_vsic));
    assert(mydata != NULL);
    memset(mydata, 0, sizeof(struct file_vsic));

    // Init VSIC data structures
    vsic->ops = file_ops;
    vsic->data = mydata;
    vsic->blocksize = 512;	// XXX: Determine from drive?

    return SYS_ERR_OK;
}

errval_t storage_vsic_driver_init(int argc, const char **argv,
				  struct storage_vsic *vsic)
{
    return file_vsic_alloc(vsic);
}

/* errval_t storage_vsa_alloc(struct storage_vsa *vsa, size_t size) */
/* { */
/*     static int vsa_num = 0; */
/*     char filename[32]; */

/*     snprintf(filename, 32, "%u.vsa", vsa_num++); */
/*     vsa->fd = open(filename, O_RDWR | O_CREAT | O_TRUNC, 0644); */
/*     assert(vsa->fd != -1); */

/*     return SYS_ERR_OK; */
/* } */

errval_t storage_vsa_acquire(struct storage_vsa *vsa, const char *name,
			     size_t size)
{
    char filename[32];

    snprintf(filename, 32, "%s.vsa", name);
    vsa->fd = open(filename, O_RDWR | O_CREAT, 0644);
    assert(vsa->fd != -1);

    return SYS_ERR_OK;
}

errval_t storage_vsa_resize(struct storage_vsa *vsa, size_t size)
{
    // No-op
    return SYS_ERR_OK;
}
