/*
 * Copyright (c) 2009, 2010, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_BLOCKDEVFS_H
#define VFS_BLOCKDEVFS_H

#ifndef VFS_BLK_DEBUG
#if defined(VFS_DEBUG) || defined(GLOBAL_DEBUG)
#define VFS_BLK_DEBUG(x...) printf("vfs_blockdevfs: " x)
#else
#define VFS_BLK_DEBUG(x...) ((void)0)
#endif
#endif // VFS_DEBUG

struct blockdev_entry {
    struct blockdev_entry *prev;
    struct blockdev_entry *next;

    char *path;
    size_t size;
    int type;
    void *backend_handle;
    bool open;
};

void blockdev_append_entry(struct blockdev_entry *entry);

enum blockdev_backend_types
{
    blockdev_backend_type_ahci,
    blockdev_backend_type_ata,
    blockdev_backend_type_megaraid,
};


// AHCI (direct libahci)
errval_t blockdevfs_ahci_init(void);
errval_t blockdevfs_ahci_open(void *handle);
errval_t blockdevfs_ahci_close(void *handle);
errval_t blockdevfs_ahci_read(void *handle, size_t pos, void *buffer,
		size_t bytes, size_t *bytes_read);
errval_t blockdevfs_ahci_write(void *handle, size_t pos, const void *buffer,
		size_t bytes, size_t *bytes_written);
errval_t blockdevfs_ahci_flush(void *handle);
// AHCI (using Flounder-AHCI)
errval_t blockdevfs_ata_init(void);
errval_t blockdevfs_ata_open(void *handle);
errval_t blockdevfs_ata_close(void *handle);
errval_t blockdevfs_ata_read(void *handle, size_t pos, void *buffer,
		size_t bytes, size_t *bytes_read);
errval_t blockdevfs_ata_write(void *handle, size_t pos, const void *buffer,
		size_t bytes, size_t *bytes_written);
errval_t blockdevfs_ata_flush(void *handle);
// MegaRAID
errval_t blockdevfs_megaraid_init(void);
errval_t blockdevfs_megaraid_open(void *handle);
errval_t blockdevfs_megaraid_close(void *handle);
errval_t blockdevfs_megaraid_read(void *handle, size_t pos, void *buffer,
		size_t bytes, size_t *bytes_read);
errval_t blockdevfs_megaraid_write(void *handle, size_t pos, const void *buffer,
		size_t bytes, size_t *bytes_written);
errval_t blockdevfs_megaraid_flush(void *handle);
#endif
