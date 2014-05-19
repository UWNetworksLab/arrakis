/**
 * \file
 * \brief NFS client definitions
 */

/*
 * Copyright (c) 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_NFS_H
#define BARRELFISH_NFS_H

#include <sys/cdefs.h>

#include <lwip/err.h> // for err_t
#include <lwip/ip_addr.h> // for struct ip_addr
#include <errors/errno.h>

#include <nfs/xdr.h>
#include <nfs/mount_rpc.h>
#include <nfs/nfs_rpc.h>

__BEGIN_DECLS

#define NFS_READDIR_COOKIE 0        ///< initial cookie for readdir
#define NFS_READDIR_COOKIEVERF NULL ///< initial cookie verifier for readder

#define NULL_NFS_FH ((struct nfs_fh3) { /*data_len*/ 0, /*data_val*/ NULL })

struct nfs_client;

/**
 * \brief Callback function for mount operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_mount()
 * \param client NFS client instance
 * \param mountstat Mount status (MNT3_OK on success)
 * \param fhandle File handle for mount point (only valid if mountstat==MNT3_OK)
 *
 * The memory referred to by #fhandle, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_mount_callback_t)(void *arg, struct nfs_client *client,
                                     enum mountstat3 mountstat,
                                     struct nfs_fh3 fhandle);

/**
 * \brief Callback function for getattr operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_getattr()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_getattr_callback_t)(void *arg, struct nfs_client *client,
                                       GETATTR3res *result);

/**
 * \brief Callback function for setattr operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_setattr()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_setattr_callback_t)(void *arg, struct nfs_client *client,
                                       SETATTR3res *result);


/**
 * \brief Callback function for readdir operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_readdir()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_readdir_callback_t)(void *arg, struct nfs_client *client,
                                       READDIR3res *result);

/**
 * \brief Callback function for readdirplus operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_readdirplus()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_readdirplus_callback_t)(void *arg, struct nfs_client *client,
                                           READDIRPLUS3res *result);

/**
 * \brief Callback function for lookup operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_lookup()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_lookup_callback_t)(void *arg, struct nfs_client *client,
                                      LOOKUP3res *result);

/**
 * \brief Callback function for access operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_access()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_access_callback_t)(void *arg, struct nfs_client *client,
                                      ACCESS3res *result);

/**
 * \brief Callback function for read operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_read()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_read_callback_t)(void *arg, struct nfs_client *client,
                                    READ3res *result);

/**
 * \brief Callback function for write operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_write()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_write_callback_t)(void *arg, struct nfs_client *client,
                                     WRITE3res *result);

/**
 * \brief Callback function for create operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_create()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_create_callback_t)(void *arg, struct nfs_client *client,
                                      CREATE3res *result);

/**
 * \brief Callback function for mkdir operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_mkdir()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_mkdir_callback_t)(void *arg, struct nfs_client *client,
                                     MKDIR3res *result);

/**
 * \brief Callback function for remove operation
 *
 * \param arg Opaque argument pointer, as provided to nfs_remove()
 * \param client NFS client instance
 * \param result Result pointer, or NULL on error
 *
 * The memory referred to by #result, if any, is now the property of the callee,
 * and must be freed by the appropriate XDR free operations.
 */
typedef void (*nfs_remove_callback_t)(void *arg, struct nfs_client *client,
                                      REMOVE3res *result);

struct nfs_client *nfs_mount(struct ip_addr server, const char *path,
                             nfs_mount_callback_t callback, void *cbarg);
err_t nfs_getattr(struct nfs_client *client, struct nfs_fh3 fh,
                  nfs_getattr_callback_t callback, void *cbarg);
err_t nfs_setattr(struct nfs_client *client, struct nfs_fh3 fh,
                  sattr3 new_attributes, bool guarded,
                  nfs_setattr_callback_t callback, void *cbarg);
err_t nfs_readdir(struct nfs_client *client, struct nfs_fh3 fh,
                  cookie3 cookie, cookieverf3 cookieverf,
                  nfs_readdir_callback_t callback, void *cbarg);
err_t nfs_readdirplus(struct nfs_client *client, struct nfs_fh3 fh,
                      cookie3 cookie, cookieverf3 cookieverf,
                      nfs_readdirplus_callback_t callback, void *cbarg);
err_t nfs_lookup(struct nfs_client *client, struct nfs_fh3 dirfh,
                 const char *name, nfs_lookup_callback_t callback, void *cbarg);
err_t nfs_access(struct nfs_client *client, struct nfs_fh3 fh, uint32_t access,
                 nfs_access_callback_t callback, void *cbarg);
err_t nfs_read(struct nfs_client *client, struct nfs_fh3 fh, offset3 offset,
               count3 count, nfs_read_callback_t callback, void *cbarg);
err_t nfs_write(struct nfs_client *client, struct nfs_fh3 fh, offset3 offset,
                const void *data, count3 count, stable_how stable,
                nfs_write_callback_t callback, void *cbarg);
err_t nfs_create(struct nfs_client *client, struct nfs_fh3 dir,
                 const char *name, bool guarded, sattr3 attributes,
                 nfs_create_callback_t callback, void *cbarg);
err_t nfs_mkdir(struct nfs_client *client, struct nfs_fh3 dir, const char *name,
                sattr3 attributes, nfs_mkdir_callback_t callback, void *cbarg);
err_t nfs_remove(struct nfs_client *client, struct nfs_fh3 dir,
                 const char *name, nfs_remove_callback_t callback,
                 void *cbarg);
void nfs_destroy(struct nfs_client *client);

void nfs_copyfh(struct nfs_fh3 *dest, struct nfs_fh3 src);
void nfs_freefh(struct nfs_fh3 fh);


errval_t nfsstat_to_errval(enum nfsstat3 s);
errval_t mountstat_to_errval(enum mountstat3 s);

__END_DECLS

#endif // BARRELFISH_NFS_H
