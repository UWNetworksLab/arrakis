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
#include <string.h>
#include <errors/errno.h>
#include <storage/vsic.h>
#include <storage/vsa.h>

//#define DO_ACTUAL_IO

#define MAX_CBS         1000

#define CPU_FREQ        2200            // MHz
#define READ_LATENCY    (1 * CPU_FREQ)  // us
#define WRITE_LATENCY   (15 * CPU_FREQ) // us
#define FLUSH_LATENCY   (0 * CPU_FREQ)  // us

enum ramcb_cmd {
    CMD_READ,
    CMD_WRITE,
    CMD_FLUSH
};

struct ramcb {
    uint64_t execute_time;
    off_t offset;
    void *buf;
    size_t nbytes;
    void *handle;
    enum ramcb_cmd cmd;
};

struct ram_vsic {
    void *vsa_area;
    struct ramcb cb[MAX_CBS];
    const struct ramcb *cb_list[MAX_CBS];
};

#ifndef BARRELFISH
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

static struct ramcb *get_ramcb(struct ram_vsic *vsic)
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
    struct ram_vsic *mydata = vsic->data;
    struct ramcb *cb = get_ramcb(mydata);
    assert(cb != NULL);

#ifdef DO_ACTUAL_IO
    memcpy(mydata->vsa_area + offset, buffer, size);
#endif

    cb->execute_time = rdtsc() + WRITE_LATENCY;
    cb->offset = offset;
    cb->buf = buffer;
    cb->nbytes = size;
    cb->cmd = CMD_WRITE;
    cb->handle = NULL;

    return SYS_ERR_OK;
}

static errval_t vsic_read(struct storage_vsic *vsic, struct storage_vsa *vsa,
                          off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    struct ram_vsic *mydata = vsic->data;
    struct ramcb *cb = get_ramcb(mydata);
    assert(cb != NULL);

    cb->execute_time = rdtsc() + READ_LATENCY;
    cb->offset = offset;
    cb->buf = buffer;
    cb->nbytes = size;
    cb->cmd = CMD_READ;
    cb->handle = NULL;

    return SYS_ERR_OK;
}

static errval_t vsic_flush(struct storage_vsic *vsic, struct storage_vsa *vsa)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    struct ram_vsic *mydata = vsic->data;
    struct ramcb *cb = get_ramcb(mydata);
    assert(cb != NULL);

    cb->execute_time = rdtsc() + FLUSH_LATENCY;
    cb->cmd = CMD_FLUSH;
    cb->handle = NULL;

    return SYS_ERR_OK;
}

static errval_t vsic_flush2(struct storage_vsic *vsic, struct storage_vsa *vsa, void *handle)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    struct ram_vsic *mydata = vsic->data;
    struct ramcb *cb = get_ramcb(mydata);
    assert(cb != NULL);

    cb->execute_time = rdtsc() + FLUSH_LATENCY;
    cb->cmd = CMD_FLUSH;
    cb->handle = handle;

    return SYS_ERR_OK;
}

static errval_t vsic_poll(struct storage_vsic *vsic, void **handle)
{
    assert(vsic != NULL);
    struct ram_vsic *mydata = vsic->data;

    for(int i = 0; i < MAX_CBS; i++) {
      if(mydata->cb_list[i] != NULL) {
	const struct ramcb *cb = mydata->cb_list[i];

	if(rdtsc() >= cb->execute_time) {
#ifdef DO_ACTUAL_IO
	  if(cb->cmd == CMD_READ) {
	    memcpy(cb->buf, mydata->vsa_area + cb->offset, cb->size);
	  }
#endif

	  // Completed successfully
	  mydata->cb_list[i] = NULL;
	  *handle = cb->handle;
	  return SYS_ERR_OK;
	}
      }
    }

    return FLOUNDER_ERR_TX_BUSY;
}

static errval_t vsic_wait(struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    struct ram_vsic *mydata = vsic->data;

    for(;;) {
        int entries = 0;

        for(int i = 0; i < MAX_CBS; i++) {
            if(mydata->cb_list[i] != NULL) {
                const struct ramcb *cb = mydata->cb_list[i];

                if(rdtsc() < cb->execute_time) {
                    // Still in progress
                    entries++;
                } else {
#ifdef DO_ACTUAL_IO
                    if(cb->cmd == CMD_READ) {
                        memcpy(cb->buf, mydata->vsa_area + cb->offset, cb->size);
                    }
#endif

                    // Completed successfully
                    mydata->cb_list[i] = NULL;
                }
            }
        }

        if(entries == 0) {
            break;
        }

        // TODO: Might want to sleep
    }

    return SYS_ERR_OK;
}

static struct storage_vsic_ops file_ops = {
    .write = vsic_write,
    .read = vsic_read,
    .flush = vsic_flush,
    .wait = vsic_wait,
    .poll = vsic_poll,
    .flush2 = vsic_flush2,
};

errval_t storage_vsic_driver_init(int argc, const char **argv,
				  struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    struct ram_vsic *mydata = malloc(sizeof(struct ram_vsic));
    assert(mydata != NULL);
    memset(mydata, 0, sizeof(struct ram_vsic));

    mydata->vsa_area = malloc(10 * 1024 * 1024);
    assert(mydata->vsa_area != NULL);

    // Init VSIC data structures
    vsic->ops = file_ops;
    vsic->data = mydata;
    vsic->blocksize = 512;	// XXX: Determine from drive?

    return SYS_ERR_OK;
}

errval_t storage_vsa_acquire(struct storage_vsa *vsa, const char *name,
			     size_t size)
{
    return SYS_ERR_OK;
}

#if 0
errval_t storage_vsa_resize(struct storage_vsa *vsa, size_t size)
{
    // No-op
    return SYS_ERR_OK;
}
#endif
