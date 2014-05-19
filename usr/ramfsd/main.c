/**
 * \file
 * \brief ramfs service
 */

/*
 * Copyright (c) 2010, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdio.h>
#include <string.h>
#include <zlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <if/trivfs_defs.h>
#include <spawndomain/spawndomain.h>
#include "ramfs.h"
#include <cpiobin.h>

#define BOOTSCRIPT_FILE_NAME "bootmodules"

static errval_t write_directory(struct dirent *root, const char *path);
static errval_t write_file(struct dirent *root, const char *path, uint8_t *data,
                           size_t len);

/* -------------------- CODE TO UNPACK RAMFS IMAGES  ------------------------ */

static int cpio_entry_handler(int n, const cpio_generic_header_t *h, void *arg)
{
    struct dirent *root = arg;
    errval_t err;

    if(CPIO_MODE_FILE == (h->mode & CPIO_MODE_FILE_TYPE_MASK)) {
        err = write_file(root, h->name, (void *)h->data, h->datasize);
    } else if(CPIO_MODE_DIRECTORY == (h->mode & CPIO_MODE_FILE_TYPE_MASK)) {
        err = write_directory(root, h->name);
    } else {
        return 0;
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error writing file to ramfs");
    }

    return 0;
}

static errval_t unpack_cpio(struct dirent *root, void *data, size_t len)
{
    if (!cpio_archive_valid(data, len)) {
        USER_PANIC("invalid CPIO archive");
    }

    cpio_generic_header_t h;
    cpio_visit(data, len, cpio_entry_handler, &h, root);
    return SYS_ERR_OK;
}

static errval_t unpack_cpiogz(struct dirent *root, void *data, size_t len)
{
    /* decompress the image to a local buffer */
    static uint8_t chunk[16384];
    z_stream d_stream; /* decompression stream */
    errval_t err;
    int zerr;

    size_t outbuflen = 16384, outbufpos = 0;
    uint8_t *outbuf = malloc(outbuflen);
    assert(outbuf != NULL);

    memset(&d_stream, 0, sizeof(d_stream));
    d_stream.next_in  = data;
    d_stream.avail_in = len;

    zerr = inflateInit2(&d_stream, MAX_WBITS + 32);
    assert(zerr == Z_OK);

    do {
        do {
            d_stream.avail_out = sizeof(chunk);
            d_stream.next_out = chunk;

            zerr = inflate(&d_stream, Z_NO_FLUSH);
            if (zerr < 0 || zerr == Z_NEED_DICT) {
                debug_printf("zlib error %d: %s\n", zerr, d_stream.msg);
                abort(); // FIXME
            }

            size_t outchunklen = sizeof(chunk) - d_stream.avail_out;
            while (outchunklen + outbufpos > outbuflen) {
                // XXX: inefficient: grow output buffer, copying everything
                outbuflen *= 2;
                outbuf = realloc(outbuf, outbuflen);
                assert(outbuf != NULL);
            }
            memcpy(&outbuf[outbufpos], chunk, outchunklen);
            outbufpos += outchunklen;
        } while(d_stream.avail_out == 0);
        assert(zerr == Z_STREAM_END);
    } while (zerr != Z_STREAM_END);

    zerr = inflateEnd(&d_stream);
    assert(zerr == Z_OK);

    assert(outbufpos == d_stream.total_out);
    err = unpack_cpio(root, outbuf, outbufpos);
    free(outbuf);
    return err;
}

/* ---------------------- POPULATION FROM BOOTINFO  ------------------------- */

static errval_t write_directory(struct dirent *root, const char *path)
{
    errval_t err;

    assert(path != NULL);
    assert(root != NULL);

    // walk path, creating/locating directories as we go
    struct dirent *d = root;
    while (true) {
        // remove any leading /
        while (path[0] == '/') {
            path++;
        }

        char *nextsep = strchr(path, '/');

        // no more /, we have the directory name
        if (nextsep == NULL) {
            break;
        }

        // extract dirname, advance path
        size_t namelen = nextsep - path;
        char *dirname = malloc(namelen + 1);
        assert(dirname != NULL);
        memcpy(dirname, path, namelen);
        dirname[namelen] = '\0';
        path += namelen;

        // does this directory exist?
        struct dirent *next;
        err = ramfs_lookup(d, dirname, &next);
        if (err_is_ok(err)) {
            free(dirname);
            d = next;
            continue;
        } else if (err_no(err) != FS_ERR_NOTFOUND) {
            free(dirname);
            return err;
        }

        // create the directory
        err = ramfs_mkdir(d, dirname, &next);
        if (err_is_fail(err)) {
            free(dirname);
            return err;
        }
        d = next;
    }

    // create the last-level directory
    char *path_copy = strdup(path);
    assert(path_copy != NULL);
    err = ramfs_mkdir(d, path_copy, NULL);
    if (err_is_fail(err)) {
        free(path_copy);
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t write_file(struct dirent *root, const char *path, uint8_t *data,
                           size_t len)
{
    errval_t err;

    assert(path != NULL);
    assert(root != NULL);

    // walk path, creating/locating directories as we go
    struct dirent *d = root;
    while (true) {
        // remove any leading /
        while (path[0] == '/') {
            path++;
        }

        char *nextsep = strchr(path, '/');

        // no more /, we have the filename
        if (nextsep == NULL) {
            break;
        }

        // extract dirname, advance path
        size_t namelen = nextsep - path;
        char *dirname = malloc(namelen + 1);
        assert(dirname != NULL);
        memcpy(dirname, path, namelen);
        dirname[namelen] = '\0';
        path += namelen;

        // does this directory exist?
        struct dirent *next;
        err = ramfs_lookup(d, dirname, &next);
        if (err_is_ok(err)) {
            free(dirname);
            d = next;
            continue;
        } else if (err_no(err) != FS_ERR_NOTFOUND) {
            free(dirname);
            return err;
        }

        // create the directory
        err = ramfs_mkdir(d, dirname, &next);
        if (err_is_fail(err)) {
            free(dirname);
            return err;
        }
        d = next;
    }

    // create the file!
    struct dirent *f;
    char *path_copy = strdup(path);
    assert(path_copy != NULL);
    err = ramfs_create(d, path_copy, &f);
    if (err_is_fail(err)) {
        free(path_copy);
        return err;
    }

    // allocate storage
    uint8_t *buf;
    err = ramfs_grow(f, 0, len, &buf);
    if (err_is_fail(err)) {
        ramfs_delete(f);
        return err;
    }

    // copy the payload
    memcpy(buf, data, len);

    return SYS_ERR_OK;
}

static errval_t append_to_file(struct dirent *f, const char *str)
{
    assert(!ramfs_isdir(f));
    size_t pos = ramfs_get_size(f);
    size_t len = strlen(str);
    errval_t err;

    // allocate storage
    uint8_t *buf;
    err = ramfs_grow(f, pos, len + 1, &buf);
    if (err_is_fail(err)) {
        return err;
    }

    // copy the payload
    memcpy(buf, str, len);

    // terminate with a \n
    buf[len] = '\n';

    return SYS_ERR_OK;
}

/* maybe this should be in libc -- apparently it's a GNU extension */
static void *memrchr(const void *p, int c, size_t n)
{
    const char *s = &((const char *)p)[n - 1];

    while (n--) {
        if (*s == c) {
            return (void *) s;
        }
        s--;
    }

    return NULL;
}


// try to remove the 'irrelevant' prefix of a multiboot path
// eg. "/username/blerg/x86_64/sbin/foo..." becomes "/x86_64/sbin/foo..."
static const char *remove_prefix(const char *path)
{
    static char *theprefix;
    static size_t theprefixlen;

    // don't know anything
    if (theprefix == NULL) {
        // walk forward until we see '/sbin/'
        char *sbin = strstr(path, "/sbin/");
        if (sbin == NULL || sbin == path) {
            return path; // give up
        }

        // walk backward one path element (the architecture)
        char *prevsep = memrchr(path, '/', sbin - path);
        if (prevsep == NULL) {
            return path; // give up
        }

        // store copy of prefix for future use
        theprefixlen = prevsep - path;
        theprefix = malloc(theprefixlen + 1);
        assert(theprefix != NULL);
        memcpy(theprefix, path, theprefixlen);
        theprefix[theprefixlen] = '\0';

        return prevsep;
    } else if (strncmp(theprefix, path, theprefixlen) == 0) {
        // check if stored prefix matches
        return &path[theprefixlen];
    } else { // no match?
        debug_printf("remove_prefix(): '%s' doesn't match stored prefix '%s'\n",
                     path, theprefix);
        return path;
    }
}

static errval_t getimage(struct mem_region *region, size_t *retlen,
                         lvaddr_t *retdata)
{
    // map in the real module (FIXME: leaking it afterwards)
    size_t len;
    errval_t err = spawn_map_module(region, &len, retdata, NULL);
    if (err_is_ok(err)) {
        assert(len >= region->mrmod_size);
        *retlen = region->mrmod_size;
    }
    return err;
}

static void populate_multiboot(struct dirent *root, struct bootinfo *bi)
{
    lvaddr_t data;
    size_t len;
    errval_t err;

    assert(root != NULL);

    // create bootscript file
    struct dirent *bootscript_f;
    err = ramfs_create(root, BOOTSCRIPT_FILE_NAME, &bootscript_f);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error creating bootscript file");
    }

    debug_printf("pre-populating from boot image...\n");

    for (int i = 0; i < bi->regions_length; i++) {
        struct mem_region *region = &bi->regions[i];
        if (region->mr_type != RegionType_Module) { /* Not of module type */
            continue;
        }

        const char *name = remove_prefix(multiboot_module_name(region));

        // is this a ramfs image we should unpack?
        if (strstr(name, "_ramfs.cpio.gz") != NULL) {
            debug_printf("unpacking Gzipped CPIO %s\n", name);
            err = spawn_map_module(region, &len, &data, NULL);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in spawn_map_module");
            }

            err = unpack_cpiogz(root, (void *)data, len);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error unpacking ramfs image");
            }

            // TODO: unmap
        } else if (strstr(name, "_ramfs.cpio") != NULL) {
            debug_printf("unpacking CPIO %s\n", name);
            err = spawn_map_module(region, &len, &data, NULL);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in spawn_map_module");
            }

            err = unpack_cpio(root, (void *)data, len);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error unpacking ramfs image");
            }

            // TODO: unmap
        } else {
            // map the image
            err = getimage(region, &len, &data);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in getimage");
            }

            // copy to ramfs
            err = write_file(root, name, (void *)data, len);
            if (err_is_fail(err)) {
                if (err_no(err) == FS_ERR_EXISTS) {
                    debug_printf("%s already exists, skipping it\n", name);
                } else {
                    USER_PANIC_ERR(err, "error in write_file");
                }
            }

            // TODO: unmap

            // append line to bootscript
            const char *args = remove_prefix(multiboot_module_rawstring(region));
            char *line = NULL;

            // Prepend a '/' if path is relative
            if(args[0] != '/') {
                line = calloc(strlen(args) + 2, 1);
                line[0] = '/';
                strcat(line, args);
                args = line;
            }

            err = append_to_file(bootscript_f, args);

            // Free temporary buffer if allocated
            if(line != NULL) {
                free(line);
            }

            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error appending to bootscript");
            }
        }
    }

    debug_printf("ready\n");
}

// Get the bootinfo and map it in.
static errval_t map_bootinfo(struct bootinfo **bootinfo)
{
    errval_t err, msgerr;

    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    struct capref bootinfo_frame;
    size_t bootinfo_size;

    msgerr = cl->vtbl.get_bootinfo(cl, &err, &bootinfo_frame, &bootinfo_size);
    if (err_is_fail(msgerr)) {
        err = msgerr;
    }
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed in get_bootinfo");
        return err;
    }

    err = vspace_map_one_frame((void**)bootinfo, bootinfo_size, bootinfo_frame,
                               NULL, NULL);
    assert(err_is_ok(err));

    return err;
}

static void multiboot_cap_reply(struct monitor_binding *st, struct capref cap,
                                errval_t msgerr)
{
    errval_t err;
    static cslot_t multiboot_slots = 0;

    // All multiboot caps received
    if (err_is_fail(msgerr)) {
        // Request bootinfo frame
        struct bootinfo *bi;
        err = map_bootinfo(&bi);
        assert(err_is_ok(err));

        // Init ramfs
        struct dirent *root = ramfs_init();

        // Populate it with contents of multiboot
        populate_multiboot(root, bi);

        // Start the service
        err = start_service(root);
        assert(err_is_ok(err));
        return;
    }

    // Move the cap into the multiboot cnode
    struct capref dest = {
        .cnode = cnode_module,
        .slot  = multiboot_slots++,
    };
    err = cap_copy(dest, cap);
    assert(err_is_ok(err));
    err = cap_destroy(cap);
    assert(err_is_ok(err));

    err = st->tx_vtbl.multiboot_cap_request(st, NOP_CONT, multiboot_slots);
    assert(err_is_ok(err));
}

static void bootstrap(void)
{
    errval_t err;

    /* Create the module cnode */
    struct capref modulecn_cap = {
        .cnode = cnode_root,
        .slot  = ROOTCN_SLOT_MODULECN,
    };
    err = cnode_create_raw(modulecn_cap, NULL,
                           ((cslot_t)1 << MODULECN_SIZE_BITS), NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cnode_create_raw failed");
        abort();
    }

    // XXX: Set reply handler
    struct monitor_binding *st = get_monitor_binding();
    st->rx_vtbl.multiboot_cap_reply = multiboot_cap_reply;

    // Make first multiboot cap request
    err = st->tx_vtbl.multiboot_cap_request(st, NOP_CONT, 0);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    // Request multiboot caps and bootinfo from monitor
    bootstrap();

    messages_handler_loop();
    return 0;
}
