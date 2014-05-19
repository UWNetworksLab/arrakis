/**
 * \file
 * \brief ramfs service
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/vregion.h>
#include <if/trivfs_defs.h>
#include <if/monitor_defs.h>

#include "ramfs.h"

#define SERVICE_NAME    "ramfs"

#define FHTAB_SIZE_BITS 8
#define FHTAB_SIZE_MASK ((1U << FHTAB_SIZE_BITS) - 1)
#define FHTAB_LEN       (1U << FHTAB_SIZE_BITS)
#define FH_BITS         (sizeof(trivfs_fh_t) * NBBY)
#define FHGEN_BITS      (FH_BITS - FHTAB_SIZE_BITS)

#define NULL_FH         ((trivfs_fh_t)-1u)

struct msgq_elem {
    enum trivfs_msg_enum msgnum;
    union trivfs_arg_union a;
    struct dirent *dirent;
    struct msgq_elem *next;
};

struct client_state {
    struct dirent *root;
    struct dirent *fhtab[FHTAB_LEN];
    unsigned fhindex, fhgen;
    struct msgq_elem *qstart, *qend; ///< queue of pending replies
    struct bulk_transfer_slave bulk;
    struct vregion *bulk_vregion;
};

static void qrunner(void *);

/* ------------------------------------------------------------------------- */

static void client_state_init(struct client_state *st, struct dirent *root)
{
    st->root = root;
    memset(st->fhtab, 0, sizeof(st->fhtab));
    st->fhindex = 0;
    st->fhgen = 0;
    st->qstart = st->qend = NULL;
    st->bulk_vregion = NULL;
}

static trivfs_fh_t fh_set(struct client_state *st, struct dirent *d)
{
    // use the next index slot
    st->fhtab[st->fhindex] = d;
    ramfs_incref(d);

    // construct fh: generation and index
    trivfs_fh_t fh = ((trivfs_fh_t)st->fhgen << FHTAB_SIZE_BITS) | st->fhindex;

    // update index (and generation if needed)
    if (++st->fhindex == FHTAB_LEN) {
        st->fhindex = 0;
        st->fhgen++;
    }

    return fh;
}

static struct dirent *fh_get(struct client_state *st, trivfs_fh_t fh)
{
    // unpack fh
    unsigned gen = fh >> FHTAB_SIZE_BITS;
    unsigned idx = fh & FHTAB_SIZE_MASK;

    // check if it's still valid
    struct dirent *e = NULL;
    if ((gen == st->fhgen && idx < st->fhindex)
        || (gen == st->fhgen - 1 && idx >= st->fhindex)) {
        e = st->fhtab[idx];
    }

    if (e == NULL) {
        return NULL; // invalid or stale
    }

    if (ramfs_islive(e)) {
        return e; // valid
    }

    // has been deleted
    st->fhtab[idx] = NULL;
    ramfs_decref(e);
    return NULL;
}

/* ------------------------------------------------------------------------- */

static void cleanup(struct trivfs_binding *b)
{
    struct client_state *st = b->st;

    // TODO: disconnect client, destroy local state
    USER_PANIC("NYI: client cleanup");
    if (st->bulk_vregion != NULL) {
        vregion_destroy(st->bulk_vregion);
    }
    free(st);
}

static void txcont(void *arg)
{
    struct dirent *e = arg;
    assert(e != NULL);
    ramfs_decref(e);
}

static bool queue_is_empty(struct client_state *st)
{
    return st->qstart == NULL;
}

static void msg_enqueue(struct client_state *st, struct trivfs_binding *b,
                        struct msgq_elem *e)
{
    errval_t err;

    e->next = NULL;
    if (st->qstart == NULL) {
        assert(st->qend == NULL);
        st->qstart = st->qend = e;

        err = b->register_send(b, b->waitset, MKCONT(qrunner,b));
        assert(err_is_ok(err));
    } else {
        assert(st->qend != NULL);
        assert(st->qend->next == NULL);
        st->qend->next = e;
        st->qend = e;
    }
}

static struct msgq_elem *msg_dequeue(struct client_state *st)
{
    if (st->qstart == NULL) {
        assert(st->qend == NULL);
        return NULL;
    } else {
        struct msgq_elem *e = st->qstart;
        st->qstart = e->next;
        if (st->qstart == NULL) {
            assert(st->qend == e);
            st->qend = NULL;
        }
        return e;
    }
}

static void qrunner(void *arg)
{
    struct trivfs_binding *b = arg;
    struct client_state *st = b->st;
    struct msgq_elem *e = msg_dequeue(st);
    errval_t err;

    if (e == NULL) {
        return;
    }

    switch (e->msgnum) {
    case trivfs_bulk_init_response__msgnum:
        err = b->tx_vtbl.bulk_init_response(b, NOP_CONT,
                                            e->a.bulk_init_response.err);
        break;

    case trivfs_getroot_response__msgnum:
        err = b->tx_vtbl.getroot_response(b, NOP_CONT,
                                          e->a.getroot_response.rootfh);
        break;

    case trivfs_readdir_response__msgnum:
        err = b->tx_vtbl.readdir_response(b, e->dirent
                                                ? MKCONT(txcont, e->dirent)
                                                : NOP_CONT,
                                          e->a.readdir_response.err,
                                          e->a.readdir_response.name,
                                          e->a.readdir_response.isdir,
                                          e->a.readdir_response.size);
        if (e->dirent != NULL && err_is_fail(err)) {
            ramfs_decref(e->dirent);
        }
        break;

    case trivfs_lookup_response__msgnum:
        err = b->tx_vtbl.lookup_response(b, NOP_CONT,
                                         e->a.lookup_response.err,
                                         e->a.lookup_response.fh,
                                         e->a.lookup_response.isdir);
        break;

    case trivfs_getattr_response__msgnum:
        err = b->tx_vtbl.getattr_response(b, NOP_CONT,
                                          e->a.getattr_response.err,
                                          e->a.getattr_response.isdir,
                                          e->a.getattr_response.size);
        break;

    case trivfs_read_response__msgnum:
        err = b->tx_vtbl.read_response(b, e->dirent
                                            ? MKCONT(txcont, e->dirent)
                                            : NOP_CONT,
                                       e->a.read_response.err,
                                       e->a.read_response.data,
                                       e->a.read_response.retlen);
        if (e->dirent != NULL && err_is_fail(err)
            && err_is_ok(e->a.read_response.err)) {
            ramfs_decref(e->dirent);
        }
        break;

    case trivfs_write_response__msgnum:
        err = b->tx_vtbl.write_response(b, NOP_CONT,
                                        e->a.write_response.err);
        break;

    case trivfs_read_bulk_response__msgnum:
        err = b->tx_vtbl.read_bulk_response(b, NOP_CONT,
                                            e->a.read_bulk_response.err,
                                            e->a.read_bulk_response.retlen);
        break;

    case trivfs_write_bulk_response__msgnum:
        err = b->tx_vtbl.write_bulk_response(b, NOP_CONT,
                                             e->a.write_bulk_response.err);
        break;

    case trivfs_truncate_response__msgnum:
        err = b->tx_vtbl.truncate_response(b, NOP_CONT,
                                           e->a.truncate_response.err);
        break;

    case trivfs_create_response__msgnum:
        err = b->tx_vtbl.create_response(b, NOP_CONT,
                                         e->a.create_response.err,
                                         e->a.create_response.fh);
        break;

    case trivfs_mkdir_response__msgnum:
        err = b->tx_vtbl.mkdir_response(b, NOP_CONT,
                                        e->a.mkdir_response.err,
                                        e->a.mkdir_response.fh);
        break;

    case trivfs_delete_response__msgnum:
        err = b->tx_vtbl.delete_response(b, NOP_CONT,
                                         e->a.delete_response.err);
        break;

    default:
        USER_PANIC("unhandled msgnum in queue: %d", e->msgnum);
    }

    if (err_is_fail(err)) {
        assert(err_no(err) != FLOUNDER_ERR_TX_BUSY);
        DEBUG_ERR(err, "error sending reply");
        cleanup(b);
    }

    if (e->next != NULL) {
        err = b->register_send(b, b->waitset, MKCONT(qrunner,b));
        assert(err_is_ok(err));
    }
    free(e);
}

/* ------------------------------------------------------------------------- */

static void ramfs_bulk_init(struct trivfs_binding *b, struct capref shared_frame)
{
    struct client_state *st = b->st;
    errval_t err, reterr = SYS_ERR_OK;

    if (st->bulk_vregion != NULL) {
        reterr = FS_ERR_BULK_ALREADY_INIT;
        cap_destroy(shared_frame);
        goto reply;
    }

    // Determine size of frame
    struct frame_identity frameid;
    err = invoke_frame_identify(shared_frame, &frameid);
    if (err_is_fail(err)) {
        reterr = err_push(err, LIB_ERR_FRAME_IDENTIFY);
        cap_destroy(shared_frame);
        goto reply;
    }

    size_t bulk_size = ((size_t)1) << frameid.bits;

    // Map the frame in local memory
    void *bulk_pool;
    err = vspace_map_one_frame_attr(&bulk_pool, bulk_size, shared_frame,
                                    VREGION_FLAGS_READ_WRITE_MPB, NULL,
                                    &st->bulk_vregion);
    if (err_is_fail(err)) {
        cap_destroy(shared_frame);
        reterr = err_push(err, LIB_ERR_VSPACE_MAP);
        goto reply;
    }
    assert(bulk_pool != NULL);
    assert(st->bulk_vregion != NULL);

    // Init the bulk transfer library
    err = bulk_slave_init(bulk_pool, bulk_size, &st->bulk);
    assert(err_is_ok(err));

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.bulk_init_response(b, NOP_CONT, reterr);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *e = malloc(sizeof(struct msgq_elem));
    assert(e != NULL);
    e->msgnum = trivfs_bulk_init_response__msgnum;
    e->a.bulk_init_response.err = reterr;
    e->dirent = NULL;
    msg_enqueue(st, b, e);
}

static void getroot(struct trivfs_binding *b)
{
    struct client_state *st = b->st;
    trivfs_fh_t fh = fh_set(st, st->root);
    errval_t err;

    if (queue_is_empty(st)) {
        err = b->tx_vtbl.getroot_response(b, NOP_CONT, fh);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *e = malloc(sizeof(struct msgq_elem));
    assert(e != NULL);
    e->msgnum = trivfs_getroot_response__msgnum;
    e->a.getroot_response.rootfh = fh;
    e->dirent = NULL;
    msg_enqueue(st, b, e);
}

static void readdir(struct trivfs_binding *b, trivfs_fh_t dir, uint32_t idx)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    const char *name = NULL;
    bool isdir = false;
    trivfs_fsize_t size = 0;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    struct dirent *e = NULL;
    err = ramfs_readdir(d, idx, &e);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    } else if (e == NULL) {
        reterr = FS_ERR_INDEX_BOUNDS;
        goto reply;
    }

    ramfs_incref(e);
    name = ramfs_get_name(e);
    isdir = ramfs_isdir(e);
    size = ramfs_get_size(e);

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.readdir_response(b, err_is_ok(reterr)
                                                ? MKCONT(txcont, e) : NOP_CONT,
                                          reterr, name, isdir, size);
	if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            if (err_is_ok(reterr)) {
                ramfs_decref(e);
            }
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }
    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_readdir_response__msgnum;
    q->a.readdir_response.err = reterr;
    q->a.readdir_response.name = (char *)name;
    q->a.readdir_response.isdir = isdir;
    q->a.readdir_response.size = size;
    q->dirent = err_is_ok(reterr) ? e : NULL;
    msg_enqueue(st, b, q);
   
}

static void lookup(struct trivfs_binding *b, trivfs_fh_t dir, char *name)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    trivfs_fh_t retfh = NULL_FH;
    bool isdir = false;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    if (name == NULL) {
        reterr = FS_ERR_NOTFOUND;
        goto reply;
    }

    struct dirent *e = NULL;
    err = ramfs_lookup(d, name, &e);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    } else if (e == NULL) {
        reterr = FS_ERR_INDEX_BOUNDS;
        goto reply;
    }

    retfh = fh_set(st, e);
    isdir = ramfs_isdir(e);

reply:
    free(name);
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.lookup_response(b, NOP_CONT, reterr, retfh, isdir);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_lookup_response__msgnum;
    q->a.lookup_response.err = reterr;
    q->a.lookup_response.fh = retfh;
    q->a.lookup_response.isdir = isdir;
    msg_enqueue(st, b, q);
}

static void getattr(struct trivfs_binding *b, trivfs_fh_t fh)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    bool isdir = false;
    trivfs_fsize_t size = 0;

    struct dirent *e = fh_get(st, fh);
    if (e == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    isdir = ramfs_isdir(e);
    size = ramfs_get_size(e);

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.getattr_response(b, NOP_CONT, reterr, isdir, size);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_getattr_response__msgnum;
    q->a.getattr_response.err = reterr;
    q->a.getattr_response.isdir = isdir;
    q->a.getattr_response.size = size;
    msg_enqueue(st, b, q);
}

static void read(struct trivfs_binding *b, trivfs_fh_t fh,
                 trivfs_offset_t offset, trivfs_fsize_t maxlen)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    uint8_t *buf = NULL;
    size_t len = 0;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    err = ramfs_read(f, offset, &buf, &len);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    if (len > maxlen) {
        len = maxlen;
    }

    ramfs_incref(f);

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.read_response(b, err_is_ok(reterr)
                                            ? MKCONT(txcont, f) : NOP_CONT,
                                       reterr, buf, len);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            if (err_is_ok(reterr)) {
                ramfs_decref(f);
            }
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_read_response__msgnum;
    q->a.read_response.err = reterr;
    q->a.read_response.data = buf;
    q->a.read_response.retlen = len;
    q->dirent = err_is_ok(reterr) ? f : NULL;
    msg_enqueue(st, b, q);
}

static void write(struct trivfs_binding *b, trivfs_fh_t fh,
                 trivfs_offset_t offset, uint8_t *data, size_t len)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    uint8_t *buf;

    err = ramfs_grow(f, offset, len, &buf);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    memcpy(buf, data, len);

reply:
    free(data);
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.write_response(b, NOP_CONT, reterr);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_write_response__msgnum;
    q->a.write_response.err = reterr;
    msg_enqueue(st, b, q);
}

static void read_bulk(struct trivfs_binding *b, trivfs_fh_t fh,
                      trivfs_offset_t offset, trivfs_fsize_t maxlen,
                      trivfs_bulkid_t bulkid)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    uint8_t *ramfsbuf = NULL;
    size_t len = 0;

    if (st->bulk_vregion == NULL) {
        reterr = FS_ERR_BULK_NOT_INIT;
        goto reply;
    }

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    err = ramfs_read(f, offset, &ramfsbuf, &len);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    // determine local address of bulk buffer
    size_t bulk_size;
    void *bulkbuf = bulk_slave_buf_get_mem(&st->bulk, bulkid, &bulk_size);

    // limit max len to size of bulk buffer
    if (maxlen > bulk_size) {
        maxlen = bulk_size;
    }

    // limit read len to maxlen
    if (len > maxlen) {
        len = maxlen;
    }

    // copy data to bulk buffer
    memcpy(bulkbuf, ramfsbuf, len);

    // prepare bulk buffer for reply
    bulk_slave_prepare_send(&st->bulk, bulkid);

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.read_bulk_response(b, NOP_CONT, reterr, len);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_read_bulk_response__msgnum;
    q->a.read_bulk_response.err = reterr;
    q->a.read_bulk_response.retlen = len;
    msg_enqueue(st, b, q);
}

static void write_bulk(struct trivfs_binding *b, trivfs_fh_t fh,
                       trivfs_offset_t offset, trivfs_fsize_t len,
                       trivfs_bulkid_t bulkid)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    if (st->bulk_vregion == NULL) {
        reterr = FS_ERR_BULK_NOT_INIT;
        goto reply;
    }

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    // determine local address of bulk buffer
    size_t maxlen;
    void *bulkbuf = bulk_slave_buf_get_mem(&st->bulk, bulkid, &maxlen);

    // limit len to size of bulk buffer
    if (len > maxlen) {
        len = maxlen;
    }

    uint8_t *ramfsbuf;
    err = ramfs_grow(f, offset, len, &ramfsbuf);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    bulk_slave_prepare_recv(&st->bulk, bulkid);

    memcpy(ramfsbuf, bulkbuf, len);

reply:
    if (queue_is_empty(st)) {
        err = b->tx_vtbl.write_bulk_response(b, NOP_CONT, reterr);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_write_bulk_response__msgnum;
    q->a.write_bulk_response.err = reterr;
    msg_enqueue(st, b, q);
}

static void truncate(struct trivfs_binding *b, trivfs_fh_t fh,
                     trivfs_fsize_t newsize)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        reterr = FS_ERR_INVALID_FH;
    } else {
        reterr = ramfs_resize(f, newsize);
    }

    if (queue_is_empty(st)) {
        err = b->tx_vtbl.truncate_response(b, NOP_CONT, reterr);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_truncate_response__msgnum;
    q->a.truncate_response.err = reterr;
    msg_enqueue(st, b, q);
}

static void create(struct trivfs_binding *b, trivfs_fh_t dir, char *name)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    trivfs_fh_t fh = NULL_FH;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    if (name == NULL) {
        reterr = FS_ERR_EXISTS; // XXX
        goto reply;
    }

    struct dirent *newf;
    err = ramfs_create(d, name, &newf);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    fh = fh_set(st, newf);

reply:
    if (err_is_fail(reterr)) {
        free(name);
    }

    if (queue_is_empty(st)) {
        err = b->tx_vtbl.create_response(b, NOP_CONT, reterr, fh);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_create_response__msgnum;
    q->a.create_response.err = reterr;
    q->a.create_response.fh = fh;
    msg_enqueue(st, b, q);
}

static void mkdir(struct trivfs_binding *b, trivfs_fh_t dir, char *name)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    trivfs_fh_t fh = NULL_FH;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        reterr = FS_ERR_INVALID_FH;
        goto reply;
    }

    if (name == NULL) {
        reterr = FS_ERR_EXISTS; // XXX
        goto reply;
    }

    struct dirent *newd;
    err = ramfs_mkdir(d, name, &newd);
    if (err_is_fail(err)) {
        reterr = err;
        goto reply;
    }

    fh = fh_set(st, newd);

reply:
    if (err_is_fail(reterr)) {
        free(name);
    }

    if (queue_is_empty(st)) {
        err = b->tx_vtbl.mkdir_response(b, NOP_CONT, reterr, fh);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_mkdir_response__msgnum;
    q->a.mkdir_response.err = reterr;
    q->a.mkdir_response.fh = fh;
    msg_enqueue(st, b, q);
}

static void delete(struct trivfs_binding *b, trivfs_fh_t fh)
{
    errval_t err, reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *d = fh_get(st, fh);
    if (d == NULL) {
        reterr = FS_ERR_INVALID_FH;
    } else {
        reterr = ramfs_delete(d);
    }

    if (queue_is_empty(st)) {
        err = b->tx_vtbl.delete_response(b, NOP_CONT, reterr);
        if (err_is_ok(err)) {
            return;
        } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
            DEBUG_ERR(err, "error sending reply");
            cleanup(b);
            return;
        }
    }

    // enqueue in send queue
    struct msgq_elem *q = malloc(sizeof(struct msgq_elem));
    assert(q != NULL);
    q->msgnum = trivfs_delete_response__msgnum;
    q->a.delete_response.err = reterr;
    msg_enqueue(st, b, q);
}

/* ------------------------------------------------------------------------- */

static struct trivfs_rx_vtbl rx_vtbl = {
    .bulk_init_call = ramfs_bulk_init,
    .getroot_call = getroot,
    .readdir_call = readdir,
    .lookup_call = lookup,
    .getattr_call = getattr,
    .read_call = read,
    .write_call = write,
    .read_bulk_call = read_bulk,
    .write_bulk_call = write_bulk,
    .truncate_call = truncate,
    .create_call = create,
    .mkdir_call = mkdir,
    .delete_call = delete,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_ramfs_iref_request(mb, NOP_CONT, iref);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send set_ramfs_iref_request to monitor");
    }
}

static errval_t connect_cb(void *st, struct trivfs_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // init state
    struct client_state *bst = malloc(sizeof(struct client_state));
    assert(bst != NULL);
    client_state_init(bst, st);
    b->st = bst;

    return SYS_ERR_OK;
}

errval_t start_service(struct dirent *root)
{
    // Offer the fs service
    return trivfs_export(root, export_cb, connect_cb, get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
}
