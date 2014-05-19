/**
 * \file
 * \brief NFS client
 */

/*
 * Copyright (c) 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <nfs/nfs.h>
#include "nfs_debug.h"
#include "rpc.h"
#include "portmap_rpc.h"


static err_t portmap_lookup(struct nfs_client *client, u_int prog, u_int vers);

/// What state are we at in initialising this mount?
enum nfs_mount_state {
    NFS_INIT_GETPORT_MOUNT, ///< 1. consult portmap for mountd port
    NFS_INIT_GETPORT_NFS,   ///< 2. consult portmap for nfsd port
    NFS_INIT_MOUNT,         ///< 3. call mountd to perform mount
    NFS_INIT_COMPLETE       ///< 4. complete
};

/// Per-instance NFS client data
struct nfs_client {
    struct rpc_client rpc_client;       ///< RPC client state (XXX: must be first)
    enum nfs_mount_state mount_state;   ///< State in mounting
    nfs_mount_callback_t mount_callback;///< Callback function when mount completes
    void *mount_cbarg;                  ///< Arg to #mount_callback
    const char *mount_path;             ///< Path on server to be mounted
    uint16_t mount_port, nfs_port;      ///< UDP ports for mountd and nfsd
};


/// Called for portmap and mount RPC replies during the mount process
/// Implements state machine for mount process
static void mount_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                void *arg2, uint32_t replystat,
                                uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    uint32_t port;
    mountstat3 mountstat;
    struct nfs_fh3 fh = { .data_len = 0, .data_val = NULL };
    err_t r;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("RPC failed while mounting in state %d:"
               "replystat = %"PRIu32", acceptstat = %"PRIu32"\n",
               client->mount_state, replystat, acceptstat);
        goto error;
    }

    switch (client->mount_state) {
    case NFS_INIT_GETPORT_MOUNT:
        rb = xdr_uint32_t(xdr, &port);
        assert(rb);
        if (!rb) {
            goto error;
        }
        client->mount_port = port;

        // lookup NFS port
        r = portmap_lookup(client, NFS_PROGRAM, NFS_V3);
        assert(r == ERR_OK);
        if (r != ERR_OK) {
            goto error;
        }
        client->mount_state = NFS_INIT_GETPORT_NFS;
        break;

    case NFS_INIT_GETPORT_NFS:
        rb = xdr_uint32_t(xdr, &port);
        assert(rb);
        client->nfs_port = port;

        // do mount call
        r = rpc_call(&client->rpc_client, client->mount_port, MOUNT_PROGRAM,
                    MOUNT_V3, MOUNTPROC3_MNT, (xdrproc_t) xdr_dirpath,
                    (void *)&client->mount_path,
                    RNDUP(strlen(client->mount_path)) + BYTES_PER_XDR_UNIT,
                    mount_reply_handler, NULL, NULL);
        assert(r == ERR_OK);
        if (r != ERR_OK) {
            goto error;
        }
        client->mount_state = NFS_INIT_MOUNT;
        break;

    case NFS_INIT_MOUNT:
        rb = xdr_mountstat3(xdr, &mountstat);
        assert(rb);
        if (!rb) {
            goto error;
        }

        client->mount_state = NFS_INIT_COMPLETE;
        if (mountstat == MNT3_OK) {
            rb = xdr_nfs_fh3(xdr, &fh);
            assert(rb);
            if (!rb) {
                goto error;
            }
        }

        // mount complete
        client->mount_callback(client->mount_cbarg, client, mountstat, fh);
        break;

    default:
        assert(!"invalid state");
    }

    return;

error:
    client->mount_callback(client->mount_cbarg, NULL, -1, fh);
    nfs_destroy(client);
}



/// Initiates a portmap GETPORT call, calling mount_reply_handler with the reply
static err_t portmap_lookup(struct nfs_client *client, u_int prog, u_int vers)
{
    struct mapping mount_map = {
        .prog = prog,
        .vers = vers,
        .prot = IPPROTO_UDP,
        .port = 0 /* ignored */
    };

    NFSDEBUGPRINT("portmap_lookup: portmap_lookup calling rpc_call\n");
    err_t err = rpc_call(&client->rpc_client, PMAP_PORT, PMAP_PROG, PMAP_VERS,
                    PMAPPROC_GETPORT, (xdrproc_t) xdr_mapping, &mount_map,
                    sizeof(mount_map), mount_reply_handler, NULL, NULL);
    NFSDEBUGPRINT("portmap_lookup: portmap_lookup done with rpc_call returned %d \n",
            err);
    return  err;
}

/** \brief Initiate an NFS mount operation
 *
 * \param server IP address of NFSv3 server
 * \param path Path on server to mount
 * \param callback Callback function to call when mount completes or fails
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns nfs_client instance pointer on success, or NULL on error. If this
 *   call succeeds, the returned client instance must be freed by a later call
 *   to nfs_destroy().
 */
struct nfs_client *nfs_mount(struct ip_addr server, const char *path,
                             nfs_mount_callback_t callback, void *cbarg)
{
    struct nfs_client *client;

    client = malloc(sizeof(struct nfs_client));
    if (client == NULL) {
        return NULL;
    }
    NFSDEBUGPRINT("nfs_mount: calling rpc_init\n");
    err_t r = rpc_init(&client->rpc_client, server);
    if (r != ERR_OK) {
        free(client);
        return NULL;
    }

    NFSDEBUGPRINT("nfs_mount: rpc_init done\n");
    client->mount_path = path;
    client->mount_state = NFS_INIT_GETPORT_MOUNT;
    client->mount_callback = callback;
    client->mount_cbarg = cbarg;

    NFSDEBUGPRINT("nfs_mount: calling portmap_lookup\n");
    r = portmap_lookup(client, MOUNT_PROGRAM, MOUNT_V3);
    NFSDEBUGPRINT("nfs_mount: portmap_lookup done \n");
    if (r != ERR_OK) {
        nfs_destroy(client);
        return NULL;
    }
    return client;
}


void nfs_copyfh(struct nfs_fh3 *dest, struct nfs_fh3 src)
{
    dest->data_len = src.data_len;
    dest->data_val = malloc(src.data_len);
    assert(dest->data_val != NULL);
    memcpy(dest->data_val, src.data_val, src.data_len);
}

void nfs_freefh(struct nfs_fh3 fh)
{
    free(fh.data_val);
}

/// RPC callback for getattr replies
static void getattr_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                  void *arg2, uint32_t replystat,
                                  uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_getattr_callback_t callback = (nfs_getattr_callback_t)arg1;
    GETATTR3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Getattr failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_GETATTR3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_GETATTR3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS getattr operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for directory to stat
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_getattr(struct nfs_client *client, struct nfs_fh3 fh,
                  nfs_getattr_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct GETATTR3args args = {
        .object = fh,
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_GETATTR, (xdrproc_t) xdr_GETATTR3args,
                    &args, sizeof(args) + RNDUP(fh.data_len),
                    getattr_reply_handler, callback, cbarg);
}


/// RPC callback for getattr replies
static void setattr_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                  void *arg2, uint32_t replystat,
                                  uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_setattr_callback_t callback = (nfs_setattr_callback_t)arg1;
    SETATTR3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Setattr failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_SETATTR3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_SETATTR3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS setattr operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for directory which attributes are to be modified
 * \param new New attributes for directory
 * \param guarded TODO
 * \param ctime TODO
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_setattr(struct nfs_client *client, struct nfs_fh3 fh,
                  sattr3 new_attributes, bool guarded,
                  nfs_setattr_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);
//    struct nfstime3 time_null;

    struct SETATTR3args args = {
        .object = fh,
        .new_attributes = new_attributes,
        .guard = {
            .check = guarded ? TRUE : FALSE,
            //.sattrguard3_u.obj_ctime = time_null,
        },
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_SETATTR, (xdrproc_t) xdr_SETATTR3args,
                    &args, sizeof(args) + RNDUP(fh.data_len),
                    setattr_reply_handler, callback, cbarg);
}


/// RPC callback for readdir replies
static void readdir_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                  void *arg2, uint32_t replystat,
                                  uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_readdir_callback_t callback = (nfs_readdir_callback_t)arg1;
    READDIR3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Readdir failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_READDIR3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_READDIR3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS readdir operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for directory to read
 * \param cookie Cookie from a previous call, or NFS_READDIR_COOKIE for a new call
 * \param cookieverf Cookie verifier from a previous call, or NFS_READDIR_COOKIEVERF
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_readdir(struct nfs_client *client, struct nfs_fh3 fh,
                  cookie3 cookie, cookieverf3 cookieverf,
                  nfs_readdir_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct READDIR3args args = {
        .dir = fh,
        .cookie = cookie,
        .cookieverf = { 0 },
        .count = (uint32_t)-1,
    };

    if (cookieverf != NULL) {
        memcpy(args.cookieverf, cookieverf, sizeof(cookieverf3));
    }
    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_READDIR, (xdrproc_t) xdr_READDIR3args,
                    &args, sizeof(args) + RNDUP(fh.data_len),
                    readdir_reply_handler, callback, cbarg);
}


/// RPC callback for readdirplus replies
static void readdirplus_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                  void *arg2, uint32_t replystat,
                                  uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_readdirplus_callback_t callback = (nfs_readdirplus_callback_t)arg1;
    READDIRPLUS3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Readdirplus failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_READDIRPLUS3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_READDIRPLUS3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS readdirplus operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for directory to read
 * \param cookie Cookie from a previous call, or NFS_READDIR_COOKIE for a new call
 * \param cookieverf Cookie verifier from a previous call, or NFS_READDIR_COOKIEVERF
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_readdirplus(struct nfs_client *client, struct nfs_fh3 fh,
                      cookie3 cookie, cookieverf3 cookieverf,
                      nfs_readdirplus_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct READDIRPLUS3args args = {
        .dir = fh,
        .cookie = cookie,
        .cookieverf = { 0 },
        .dircount = (uint32_t)-1,
        .maxcount = (uint32_t)-1,
    };

    if (cookieverf != NULL) {
        memcpy(args.cookieverf, cookieverf, sizeof(cookieverf3));
    }

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_READDIRPLUS,
                    (xdrproc_t) xdr_READDIRPLUS3args, &args,
                    sizeof(args) + RNDUP(fh.data_len),
                    readdirplus_reply_handler, callback, cbarg);
}


/// RPC callback for lookup replies
static void lookup_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                 void *arg2, uint32_t replystat,
                                 uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_lookup_callback_t callback = (nfs_lookup_callback_t)arg1;
    LOOKUP3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Lookup failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_LOOKUP3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_LOOKUP3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS lookup operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param dirfh Filehandle for directory to lookup
 * \param name Name to lookup
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_lookup(struct nfs_client *client, struct nfs_fh3 dirfh,
                 const char *name, nfs_lookup_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct LOOKUP3args args = {
        .what = {
            .dir = dirfh,
            .name = (char *)name
        }
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_LOOKUP, (xdrproc_t) xdr_LOOKUP3args,
                    &args, 2 * BYTES_PER_XDR_UNIT + RNDUP(dirfh.data_len)
                        + RNDUP(strlen(name)),
                    lookup_reply_handler, callback, cbarg);
}


/// RPC callback for access replies
static void access_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                 void *arg2, uint32_t replystat,
                                 uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_access_callback_t callback = (nfs_access_callback_t)arg1;
    ACCESS3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Access failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_ACCESS3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_ACCESS3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS access operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for file/object to check access to
 * \param access Rights to check for
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_access(struct nfs_client *client, struct nfs_fh3 fh, uint32_t access,
                 nfs_access_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct ACCESS3args args = {
        .object = fh,
        .access = access,
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_ACCESS, (xdrproc_t) xdr_ACCESS3args,
                    &args, sizeof(args) + RNDUP(fh.data_len),
                    access_reply_handler, callback, cbarg);
}


/// RPC callback for read replies
static void read_reply_handler(struct rpc_client *rpc_client, void *arg1,
                               void *arg2, uint32_t replystat,
                               uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_read_callback_t callback = (nfs_read_callback_t)arg1;
    READ3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Read failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_READ3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_READ3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS read operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for file to read
 * \param offset Offset from start of file to read from
 * \param count Maximum number of bytes to read
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_read(struct nfs_client *client, struct nfs_fh3 fh, offset3 offset,
               count3 count, nfs_read_callback_t callback, void *cbarg)
{
    NFSDEBUGPRINT("nfs read called on offset %"PRIu32" and size %d\n",
                      (uint32_t)offset, count);
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct READ3args args = {
        .file = fh,
        .offset = offset,
        .count = count
    };

    err_t errval = rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_READ, (xdrproc_t) xdr_READ3args,
                    &args, sizeof(args) + RNDUP(fh.data_len),
                    read_reply_handler, callback, cbarg);

    return errval;
}


/// RPC callback for write replies
static void write_reply_handler(struct rpc_client *rpc_client, void *arg1,
                               void *arg2, uint32_t replystat,
                               uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_write_callback_t callback = (nfs_write_callback_t)arg1;
    WRITE3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Write failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_WRITE3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_WRITE3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS write operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param fh Filehandle for file to write
 * \param offset Offset from start of file to write from
 * \param data Pointer to data to write
 * \param count Number of bytes of data to write
 * \param stable Specifies when the server may commit data to stable storage
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_write(struct nfs_client *client, struct nfs_fh3 fh, offset3 offset,
                const void *data, count3 count, stable_how stable,
                nfs_write_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct WRITE3args args = {
        .file = fh,
        .offset = offset,
        .count = count,
        .stable = stable,
        .data = {
            .data_len = count,
            .data_val = (void *)data, /* XXX: discarding const */
        }
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_WRITE, (xdrproc_t) xdr_WRITE3args,
                    &args, sizeof(args) + RNDUP(fh.data_len) + RNDUP(count),
                    write_reply_handler, callback, cbarg);
}


/// RPC callback for create replies
static void create_reply_handler(struct rpc_client *rpc_client, void *arg1,
                               void *arg2, uint32_t replystat,
                               uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_create_callback_t callback = (nfs_create_callback_t)arg1;
    CREATE3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Create failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_CREATE3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_CREATE3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS create operation (unchecked or guarded)
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param dir Filehandle for directory in which to create file
 * \param name Name of file to create
 * \param guarded True iff the operation should fail if the file already exists
 * \param attributes Initial attributes for the file
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \todo Exclusive create will be implemented by a different call.
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_create(struct nfs_client *client, struct nfs_fh3 dir,
                 const char *name, bool guarded, sattr3 attributes,
                 nfs_create_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct CREATE3args args = {
        .where = {
            .dir = dir,
            .name = (char *)name,
        },
        .how = {
            .mode = guarded ? GUARDED : UNCHECKED,
            .createhow3_u.obj_attributes = attributes,
        },
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_CREATE, (xdrproc_t) xdr_CREATE3args, &args,
                    sizeof(args) + RNDUP(dir.data_len) + RNDUP(strlen(name)),
                    create_reply_handler, callback, cbarg);
}


/// RPC callback for mkdir replies
static void mkdir_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                void *arg2, uint32_t replystat,
                                uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_mkdir_callback_t callback = (nfs_mkdir_callback_t)arg1;
    MKDIR3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Mkdir failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_MKDIR3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_MKDIR3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS mkdir operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param dir Filehandle for directory in which to create directory
 * \param name Name of directory to create
 * \param attributes Initial attributes for the directory
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_mkdir(struct nfs_client *client, struct nfs_fh3 dir, const char *name,
                sattr3 attributes, nfs_mkdir_callback_t callback, void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct MKDIR3args args = {
        .where = {
            .dir = dir,
            .name = (char *)name,
        },
        .attributes = attributes,
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_MKDIR, (xdrproc_t) xdr_MKDIR3args, &args,
                    sizeof(args) + RNDUP(dir.data_len) + RNDUP(strlen(name)),
                    mkdir_reply_handler, callback, cbarg);
}


/// RPC callback for remove replies
static void remove_reply_handler(struct rpc_client *rpc_client, void *arg1,
                                 void *arg2, uint32_t replystat,
                                 uint32_t acceptstat, XDR *xdr)
{
    struct nfs_client *client = (void *)rpc_client;
    nfs_remove_callback_t callback = (nfs_remove_callback_t)arg1;
    REMOVE3res result;
    bool rb;

    if (replystat != RPC_MSG_ACCEPTED || acceptstat != RPC_SUCCESS) {
        printf("Remove failed\n");
        callback(arg2, client, NULL);
    } else {
        memset(&result, 0, sizeof(result));
        rb = xdr_REMOVE3res(xdr, &result);
        assert(rb);
        if (rb) {
            callback(arg2, client, &result);
        } else {
            /* free partial results if the xdr fails */
            xdr_REMOVE3res(&xdr_free, &result);
            callback(arg2, client, NULL);
        }
    }
}

/** \brief Initiate an NFS remove operation
 *
 * \param client NFS client pointer, which has completed the mount process
 * \param dir Filehandle for directory in which to remove file
 * \param name Name of file to remove
 * \param callback Callback function to call when operation returns
 * \param cbarg Opaque argument word passed to callback function
 *
 * \returns ERR_OK on success, error code on failure
 */
err_t nfs_remove(struct nfs_client *client, struct nfs_fh3 dir,
                 const char *name, nfs_remove_callback_t callback,
                 void *cbarg)
{
    assert(client->mount_state == NFS_INIT_COMPLETE);

    struct REMOVE3args args = {
        .object = {
            .dir = dir,
            .name = (char *)name,
        }
    };

    return rpc_call(&client->rpc_client, client->nfs_port, NFS_PROGRAM,
                    NFS_V3, NFSPROC3_REMOVE, (xdrproc_t) xdr_REMOVE3args, &args,
                    sizeof(args) + RNDUP(dir.data_len) + RNDUP(strlen(name)),
                    remove_reply_handler, callback, cbarg);
}


/**
 * \brief Reclaim memory and terminate any outstanding operations
 */
void nfs_destroy(struct nfs_client *client)
{
    rpc_destroy(&client->rpc_client);
    free(client);
}


errval_t nfsstat_to_errval(enum nfsstat3 s)
{
    switch(s) {
    case NFS3_OK: return SYS_ERR_OK;
    case NFS3ERR_PERM: return NFS_ERR_PERM;
    case NFS3ERR_NOENT: return NFS_ERR_NOENT;
    case NFS3ERR_IO: return NFS_ERR_IO;
    case NFS3ERR_NXIO: return NFS_ERR_NXIO;
    case NFS3ERR_ACCES: return NFS_ERR_ACCES;
    case NFS3ERR_EXIST: return NFS_ERR_EXIST;
    case NFS3ERR_XDEV: return NFS_ERR_XDEV;
    case NFS3ERR_NODEV: return NFS_ERR_NODEV;
    case NFS3ERR_NOTDIR: return NFS_ERR_NOTDIR;
    case NFS3ERR_ISDIR: return NFS_ERR_ISDIR;
    case NFS3ERR_INVAL: return NFS_ERR_INVAL;
    case NFS3ERR_FBIG: return NFS_ERR_FBIG;
    case NFS3ERR_NOSPC: return NFS_ERR_NOSPC;
    case NFS3ERR_ROFS: return NFS_ERR_ROFS;
    case NFS3ERR_MLINK: return NFS_ERR_MLINK;
    case NFS3ERR_NAMETOOLONG: return NFS_ERR_NAMETOOLONG;
    case NFS3ERR_NOTEMPTY: return NFS_ERR_NOTEMPTY;
    case NFS3ERR_DQUOT: return NFS_ERR_DQUOT;
    case NFS3ERR_STALE: return NFS_ERR_STALE;
    case NFS3ERR_REMOTE: return NFS_ERR_REMOTE;
    case NFS3ERR_BADHANDLE: return NFS_ERR_BADHANDLE;
    case NFS3ERR_NOT_SYNC: return NFS_ERR_NOT_SYNC;
    case NFS3ERR_BAD_COOKIE: return NFS_ERR_BAD_COOKIE;
    case NFS3ERR_NOTSUPP: return NFS_ERR_NOTSUPP;
    case NFS3ERR_TOOSMALL: return NFS_ERR_TOOSMALL;
    case NFS3ERR_SERVERFAULT: return NFS_ERR_SERVERFAULT;
    case NFS3ERR_BADTYPE: return NFS_ERR_BADTYPE;
    case NFS3ERR_JUKEBOX: return NFS_ERR_JUKEBOX;
    default: return NFS_ERR_TRANSPORT; // XXX: unknown
    }
}

errval_t mountstat_to_errval(enum mountstat3 s)
{
    switch(s) {
    case MNT3_OK: return SYS_ERR_OK;
    case MNT3ERR_PERM: return NFS_ERR_MNT_PERM;
    case MNT3ERR_NOENT: return NFS_ERR_MNT_NOENT;
    case MNT3ERR_IO: return NFS_ERR_MNT_IO;
    case MNT3ERR_ACCES: return NFS_ERR_MNT_ACCES;
    case MNT3ERR_NOTDIR: return NFS_ERR_MNT_NOTDIR;
    case MNT3ERR_INVAL: return NFS_ERR_MNT_INVAL;
    case MNT3ERR_NAMETOOLONG: return NFS_ERR_MNT_NAMETOOLONG;
    case MNT3ERR_NOTSUPP: return NFS_ERR_MNT_NOTSUPP;
    case MNT3ERR_SERVERFAULT: return NFS_ERR_MNT_SERVERFAULT;
    default: return NFS_ERR_TRANSPORT; // XXX: unknown
    }
}
