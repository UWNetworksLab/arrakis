/**
 * \file
 * \brief fish - The Barrelfish shell
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish/debug.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/terminal.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <skb/skb.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <if/pixels_defs.h>

#define MAX_LINE        512
#define BOOTSCRIPT_NAME "/init.fish"

#define ENTRIES(array)  (sizeof(array) / sizeof(array[0]))

extern char **environ;
static struct cmd *find_command(const char *name);
static int makeargs(char *cmdline, char *argv[]);

typedef int (*Command)(int argc, char *argv[]);

static int spawnpixels(int argc, char *argv[]);

struct cmd {
    const char  *name;
    Command     cmd;
    const char  *usage;
};

static char *cwd;

static int help(int argc, char *argv[]);

static int execute_program(coreid_t coreid, int argc, char *argv[],
                           domainid_t *retdomainid)
{
    vfs_handle_t vh;
    errval_t err;

    // if the name contains a directory separator, assume it is relative to PWD
    char *prog = argv[0];
    if (strchr(argv[0], VFS_PATH_SEP) != NULL) {
        prog = vfs_path_mkabsolute(cwd, argv[0]);

        // check it exists
        err = vfs_open(prog, &vh);
        if (err_is_fail(err)) {
            printf("%s: file not found: %s\n", prog, err_getstring(err));
            free(prog);
            return -1;
        }
        vfs_close(vh);
    }

    assert(retdomainid != NULL);

    argv[argc] = NULL;
    err = spawn_program(coreid, prog, argv, NULL, SPAWN_NEW_DOMAIN,
                        retdomainid);

    if (prog != argv[0]) {
        free(prog);
    }

    if (err_is_fail(err)) {
        printf("%s: error spawning: %s\n", argv[0], err_getstring(err));
        DEBUG_ERR(err, "Spawning Error\n");
        return -1;
    }

    return 0;
}

static int quit(int argc, char *argv[])
{
    exit(0);
    assert(!"exit() returned");
    return 255;
}

static int print_cspace(int argc, char *argv[])
{
    debug_my_cspace();
    return 0;
}

static int setenvcmd(int argc, char *argv[])
{
    if (argc <= 1) {
        printf("Usage: %s [name=value]...\n", argv[0]);
        return -1;
    }

    for (int i=1; i < argc; i++) {
        char *sep = strchr(argv[i], '=');
        char *value = "";
        if (sep != NULL) {
            *sep = '\0'; // XXX: modify arg inplace
            value = sep + 1;
        }
        int r = setenv(argv[i], value, 1);
        if (r != 0) {
            fprintf(stderr, "Error: setenv(%s, %s) failed\n", argv[i], value);
            return r;
        }
    }

    return 0;
}

static int printenv(int argc, char *argv[])
{
    if (argc <= 1) {
        for (int i=0; environ[i] != NULL; i++) {
            printf("%s\n", environ[i]);
        }
    } else {
        for (int i = 1; i < argc; i++) {
            char *val = getenv(argv[i]);
            if (val) {
                printf("%s\n", val);
            }
        }
    }

    return 0;
}

static bool pixels_started = false;
static bool pixels_inited = false;
static int pixels_connected = 0;
#define NUM_PIXELS 16
static struct pixels_binding my_pixels_bindings[NUM_PIXELS];

static int acks = 0;

static void pixels_ack(struct pixels_binding *cl)
{
    acks--;
}

static struct pixels_rx_vtbl pixels_vtbl = {
    .ack = pixels_ack
};

static void my_pixels_bind_cb(void *st, errval_t err, struct pixels_binding *b)
{
    struct pixels_binding *pb = (struct pixels_binding *)st;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    pb->rx_vtbl = pixels_vtbl;
    pixels_connected++;
}

static void pixels_init(void)
{
    // ensure pixels is up
    if (!pixels_started) {
        printf("Starting pixels...\n");
        spawnpixels(0, NULL);
    }

    pixels_connected = 0;

    for (int core = 0; core < NUM_PIXELS; core ++) {
        char name[16];
        iref_t serv_iref;
        errval_t err;

        sprintf(name, "pixels.%d", core);

        /* Connect to the server */
        err = nameservice_blocking_lookup(name, &serv_iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to lookup server");
            exit(EXIT_FAILURE);
        }

        if (serv_iref == 0) {
            DEBUG_ERR(err, "failed to get a valid iref back from lookup");
            exit(EXIT_FAILURE);
        }

        err = pixels_bind(serv_iref,
                my_pixels_bind_cb,
                &my_pixels_bindings[core],
                get_default_waitset(),
                IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "bind request to pixels server failed immediately");
            exit(EXIT_FAILURE);
        }
    }

    while (pixels_connected < NUM_PIXELS)
        messages_wait_and_handle_next();

    printf("connected to pixels server\n");
    pixels_inited = true;
}

static const char *scroller = "Barrelfish posse in full effect!!!   ";

static char c64map(char c) {
    if ('A' <= c && c <= 'Z') {
        return 65 + c-'A';
    } else if ('a' <= c && c <= 'z') {
        return 1 + c-'a';
    } else if (c == ' ') {
        return 32;
    } else if (c == '!') {
        return 33;
    }
    else {return 32;}
}

extern const char font[];

#define RENDER_WIDTH 48
#define PIXEL_WIDTH 100000
#define FRAMES 10

static int demo(int argc, char *argv[])
{
    int core;
    int pixwidth = PIXEL_WIDTH;
    int frames = FRAMES;

    if (!pixels_inited) pixels_init();

    if (argc == 3) {
        pixwidth = atoi(argv[1]);
        frames = atoi(argv[2]);
    }
    int width = 8 * strlen(scroller);

    for (int x = 0; x < width - RENDER_WIDTH; x++) {

        // Repeat each frame a few times to slow down scrolling!
        for (int f = 0; f < frames; f++) {
        trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 1);
        for(int i = 0; i < RENDER_WIDTH; i++) {

            int xpos = (x + i)%width;
            char ascii = scroller[xpos >> 3];
            char c64char = c64map(ascii);
            int xsub = xpos & 7;

            acks = 0;
            for (core = 0 ;core < 8; core++) {
                unsigned char bits = font[c64char*8 + (7-core)];

                if (bits & (1<<(7-xsub)) ) {

                    my_pixels_bindings[core+2].tx_vtbl.display(&my_pixels_bindings[core+2], NOP_CONT, pixwidth);
                    acks++;
                }
            }

            uint64_t now = rdtsc();

            while (acks) {
                messages_wait_and_handle_next();
            }
            while (rdtsc() - now < pixwidth) ;
        }

        trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 0);
        }
    }
    return 0;
}

static int oncore(int argc, char *argv[])
{
    if(argc < 3) {
        printf("Usage: %s <core id> <program> [args]\n", argv[0]);
        return 1;
    }

    int core = atoi(argv[1]);

    argc -= 2;
    argv += 2;

    domainid_t domain_id;
    int ret = execute_program(core, argc, argv, &domain_id);

    // TODO: do something with domain_id

    return ret;
}

static int spawnpixels(int argc, char *argv[])
{
    errval_t err;

    /* Spawn on all cores */
    char *spawnargv[] = {"pixels", NULL};
    err = spawn_program_on_all_cores(true, spawnargv[0], spawnargv, NULL,
                                     SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error spawning other core");
    }
    pixels_started = true;
    printf("Done\n");

    return 0;
}

static int reset(int argc, char *argv[])
{
    printf("RESET NOT SUPPORTED ON ARM\n");
        return 0;
}

static int poweroff(int argc, char *argv[])
{
    printf("POWER OFF NOT SUPPORTED ON ARM\n");
    return 0;
}

static int ps(int argc, char *argv[])
{
    uint8_t *domains;
    size_t len;
    errval_t err;

    err = spawn_get_domain_list(&domains, &len);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawn_get_domain_list");
        return EXIT_FAILURE;
    }

    printf("DOMAINID\tSTAT\tCOMMAND\n");
    for(size_t i = 0; i < len; i++) {
        struct spawn_ps_entry pse;
        char *argbuf, status;
        size_t arglen;
        errval_t reterr;

        err = spawn_get_status(domains[i], &pse, &argbuf, &arglen, &reterr);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawn_get_status");
            return EXIT_FAILURE;
        }
        if(err_is_fail(reterr)) {
            if(err_no(reterr) == SPAWN_ERR_DOMAIN_NOTFOUND) {
                return reterr;
            }
            DEBUG_ERR(err, "status");
            return EXIT_FAILURE;
        }

        switch(pse.status) {
        case 0:
            status = 'R';
            break;

        case 1:
            status = 'Z';
            break;

        default:
            status = '?';
            break;
        }

        printf("%-8u\t%c\t", domains[i], status);
        size_t pos = 0;
        for(int p = 0; pos < arglen && p < MAX_CMDLINE_ARGS;) {
            printf("%s ", &argbuf[pos]);
            char *end = memchr(&argbuf[pos], '\0', arglen - pos);
            assert(end != NULL);
            pos = end - argbuf + 1;
        }
        printf("\n");

        free(argbuf);
    }

    free(domains);
    return EXIT_SUCCESS;
}

static int skb(int argc, char *argv[])
{
    static bool init = false;

    if(argc < 2) {
        printf("Usage: %s <program>\n", argv[0]);
        return 1;
    }

    if(!init) {
        skb_client_connect();
        init = true;
    }

    char *result = NULL, *str_err = NULL;
    int32_t int_err;

    skb_evaluate(argv[1], &result, &str_err, &int_err);

    if (int_err != 0 || (str_err != NULL && str_err[0] != '\0')) {
        printf("SKB error returned: %"PRIu32" %s\n", int_err, str_err);
    } else {
        printf("SKB returned: %s\n", result);
    }

    free(result);
    free(str_err);

    return 0;
}

static int mount(int argc, char *argv[])
{
    if (argc != 3) {
        printf("Usage: %s MOUNTPOINT URI\n", argv[0]);
        return 1;
    }

    char *path = vfs_path_mkabsolute(cwd, argv[1]);
    errval_t err = vfs_mount(path, argv[2]);
    free(path);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "in vfs_mount %s %s", argv[1], argv[2]);
        return 1;
    }
    return 0;
}

static int cat(int argc, char *argv[])
{
    if(argc < 2) {
        printf("Usage: %s [file...]\n", argv[0]);
        return 1;
    }

    uint8_t buf[1024];
    size_t size;
    vfs_handle_t vh;
    errval_t err;
    int ret = 0;

    for (int i = 1; i < argc; i++) {
        char *path = vfs_path_mkabsolute(cwd, argv[i]);
        err = vfs_open(path, &vh);
        free(path);
        if (err_is_fail(err)) {
            printf("%s: file not found\n", argv[i]);
            ret = 1;
            continue;
        }

        do {
            err = vfs_read(vh, buf, sizeof(buf), &size);
            if (err_is_fail(err)) {
                // XXX: Close any files that might be open
                DEBUG_ERR(err, "error reading file");
                return 1;
            }

            fwrite(buf, 1, size, stdout);
        } while(size > 0);

        err = vfs_close(vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

static int cat2(int argc, char *argv[])
{
    errval_t err;
    char *path;
    int ret;

    if(argc < 3) {
        printf("Usage: %s [input-files...] output-file\n", argv[0]);
        return 1;
    }

    /* Open output file creating it if it does not exist */
    path = vfs_path_mkabsolute(cwd, argv[argc - 1]);
    vfs_handle_t output_vh;
    err = vfs_create(path, &output_vh);
    free(path);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error opening output file");
        return 1;
    }

    /* Open input files, read buffer and write to output file */
    for (int i = 1; i < argc - 1; i++) {
        uint8_t buf[1024];
        size_t size;
        vfs_handle_t input_vh;
        path = vfs_path_mkabsolute(cwd, argv[i]);
        err = vfs_open(path, &input_vh);
        free(path);
        if (err_is_fail(err)) {
            printf("%s: file not found\n", argv[i]);
            ret = 1;
            continue;
        }

        do {
            err = vfs_read(input_vh, buf, sizeof(buf), &size);
            if (err_is_fail(err)) {
                // XXX: Close any files that might be open
                DEBUG_ERR(err, "error reading file");
                return 1;
            }

            size_t output_size;
            err = vfs_write(output_vh, buf, size, &output_size);
            if (err_is_fail(err)) {
                // XXX: Close any files that might be open
                DEBUG_ERR(err, "error writing to output file");
                return 1;
            }
            if (output_size != size) {
                printf("Wanted to write %zu but only wrote %zu, aborting\n",
                       size, output_size);
                // XXX: Close any files that might be open
                return 1;
            }
        } while(size > 0);

        err = vfs_close(input_vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    err = vfs_close(output_vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "in vfs_close");
    }
    return ret;
}

static int cp(int argc, char *argv[])
{
    if(argc != 3) {
        printf("Usage: %s src dest\n", argv[0]);
        return 1;
    }

    static uint8_t buf[32768];
    size_t rsize, wsize;
    vfs_handle_t src = NULL, dst = NULL;
    errval_t err;
    int ret = 0;

    char *path = vfs_path_mkabsolute(cwd, argv[1]);
    err = vfs_open(path, &src);
    free(path);
    if (err_is_fail(err)) {
        printf("%s: %s\n", argv[1], err_getstring(err));
        return 1;
    }

    path = vfs_path_mkabsolute(cwd, argv[2]);
    err = vfs_create(path, &dst);
    free(path);
    if (err_is_fail(err)) {
        printf("%s: %s\n", argv[2], err_getstring(err));
        ret = 1;
        goto out;
    }

    err = vfs_truncate(dst, 0);
    if (err_is_fail(err)) {
        printf("truncate %s: %s\n", argv[2], err_getstring(err));
        ret = 1;
        goto out;
    }

    do {
        err = vfs_read(src, buf, sizeof(buf), &rsize);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error reading file");
            ret = 1;
            goto out;
        }

        size_t wpos = 0;
        while (wpos < rsize) {
            err = vfs_write(dst, &buf[wpos], rsize - wpos, &wsize);
            if (err_is_fail(err) || wsize == 0) {
                DEBUG_ERR(err, "error writing file");
                ret = 1;
                goto out;
            }
            wpos += wsize;
        }
    } while(rsize > 0);

out:
    if (src != NULL) {
        err = vfs_close(src);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    if (dst != NULL) {
        err = vfs_close(dst);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

static int dd(int argc, char *argv[])
{
    // parse options
    char *source = NULL;
    char *target = NULL;

    vfs_handle_t source_vh = NULL;
    vfs_handle_t target_vh = NULL;

    size_t blocksize = 512;
    size_t count = 0;
    size_t skip = 0;
    size_t seek = 0;

    size_t rsize = 0;
    size_t wsize = 0;
    size_t blocks_written = 0;

    size_t total_bytes_read = 0;
    size_t total_bytes_written = 0;

    size_t progress = 0;

    errval_t err;
    int ret = 0;

    for (int i = 1; i < argc; i++)
    {
        if (!strncmp(argv[i], "bs=", 3))
            blocksize = atoi(argv[i] + 3);

        else if (!strncmp(argv[i], "count=", 6))
            count = atoi(argv[i] + 6);

        else if (!strncmp(argv[i], "skip=", 5))
            skip = atoi(argv[i] + 5);

        else if (!strncmp(argv[i], "seek=", 5))
            seek = atoi(argv[i] + 5);

        else if (!strncmp(argv[i], "if=", 3))
            source = (argv[i] + 3);

        else if (!strncmp(argv[i], "of=", 3))
            target = (argv[i] + 3);
        else if (!strncmp(argv[i], "progress", 8))
            progress = 1;
    }

    size_t one_per_cent = (blocksize * count) / 100;

    printf("from: %s to: %s bs=%zd count=%zd seek=%zd skip=%zd\n", source, target, blocksize, count, seek, skip);

    if (source != NULL)
    {
        char *path = vfs_path_mkabsolute(cwd, source);
        err = vfs_open(path, &source_vh);
        free(path);
        if (err_is_fail(err)) {
            printf("%s: %s\n", source, err_getstring(err));
            return 1;
        }

        if (skip != 0)
        {
            // TODO: skip
        }
    }

    if (target != NULL)
    {
        char *path = vfs_path_mkabsolute(cwd, target);
        err = vfs_create(path, &target_vh);
        free(path);
        if (err_is_fail(err)) {
            // close source handle
            if (source_vh != NULL)
                vfs_close(source_vh);
            printf("%s: %s\n", target, err_getstring(err));
            return 1;
        }

        if (seek != 0)
        {
            // TODO: seek
        }
    }

    uint8_t * buffer = malloc(blocksize);

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    //printf("ticks per millisec: %" PRIu64 "\n", tscperms);
    uint64_t start = rdtsc();
#endif

    if (buffer == NULL)
    {
        ret = 2;
        printf("failed to allocate buffer of size %zd\n", blocksize);
        goto out;
    }

    do
    {
        //printf("copying block\n");
        size_t read_bytes = 0;
        do {
            err = vfs_read(source_vh, buffer, blocksize, &rsize);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error reading file");
                ret = 1;
                goto out;
            }

            total_bytes_read += rsize;
            read_bytes += rsize;

            size_t wpos = 0;
            while (wpos < rsize) {
                if (wpos > 0)
                    printf("was unable to write the whole chunk of size %zd. Now at pos: %zd of buffer\n", rsize, wpos);

                err = vfs_write(target_vh, &buffer[wpos], rsize - wpos, &wsize);
                if (err_is_fail(err) || wsize == 0) {
                    DEBUG_ERR(err, "error writing file");
                    ret = 1;
                    goto out;
                }
                wpos += wsize;
                total_bytes_written += wsize;
            }
        } while(read_bytes < blocksize);

        blocks_written++;

        if (progress && one_per_cent && total_bytes_written % one_per_cent == 0) {
            printf(".");
        }

        //printf("block successfully copied. read: %zd. blocks written: %zd\n", rsize, blocks_written);
    } while (rsize > 0 && !(count > 0 && blocks_written >= count));

    if (progress) printf("\n");

out:
    if (buffer != NULL)
        free(buffer);

    if (source_vh != NULL) {
        err = vfs_close(source_vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    if (target_vh != NULL) {
        err = vfs_close(target_vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t stop = rdtsc();
    uint64_t elapsed_msecs = ((stop - start) / tscperms);
    double elapsed_secs = (double)elapsed_msecs/1000.0;

    printf("start: %" PRIu64 " stop: %" PRIu64 "\n", start, stop);

    double kbps = ((double)total_bytes_written / 1024.0) / elapsed_secs;

    printf("%zd bytes read. %zd bytes written. %f s, %f kB/s\n", total_bytes_read, total_bytes_written, elapsed_secs, kbps);
#else
    printf("%zd bytes read. %zd bytes written.\n", total_bytes_read, total_bytes_written);
#endif

    return ret;
}

static int touch(int argc, char *argv[])
{
    if(argc < 2) {
        printf("Usage: %s [file...]\n", argv[0]);
        return 1;
    }

    vfs_handle_t vh;
    errval_t err;
    int ret = 0;

    for (int i = 1; i < argc; i++) {
        char *path = vfs_path_mkabsolute(cwd, argv[i]);
        err = vfs_create(path, &vh);
        free(path);
        if (err_is_fail(err)) {
            printf("%s: %s\n", argv[i], err_getstring(err));
            DEBUG_ERR(err, "vfs_create failed");
            ret = 1;
            continue;
        }

        err = vfs_close(vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

static char vfs_type_char(enum vfs_filetype type)
{
    switch(type) {
    case VFS_FILE:
        return '-';
    case VFS_DIRECTORY:
        return 'd';
    default:
        return '?';
    }
}

static int ls(int argc, char *argv[])
{
    errval_t err;
    int ret = 0;

    // XXX: cheat and assume we have some extra space wherever argv lives
    if (argc <= 1) {
        argv[1] = cwd;
        argc = 2;
    }

    for (int i = 1; i < argc; i++) {
        vfs_handle_t vh;
        char *path = vfs_path_mkabsolute(cwd, argv[i]);
        err = vfs_opendir(path, &vh);
        free(path);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_opendir %s", argv[i]);
            printf("%s: not found\n", argv[i]);
            ret = 1;
            continue;
        }

        if (i > 1) {
            printf("\n");
        }
        printf("%s:\n", argv[i]);

        do {
            struct vfs_fileinfo info;
            char *name;
            err = vfs_dir_read_next(vh, &name, &info);
            if (err_is_ok(err)) {
                printf("%8zu %c %s\n", info.size, vfs_type_char(info.type),
                       name);
                free(name);
            }
        } while(err_is_ok(err));

        err = vfs_closedir(vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_closedir");
        }
    }

    return ret;
}

static int mkdir(int argc, char *argv[])
{
    if(argc != 2) {
        printf("Usage: %s dir\n", argv[0]);
        return 1;
    }

    char *path = vfs_path_mkabsolute(cwd, argv[1]);
    errval_t err = vfs_mkdir(path);
    free(path);
    if (err_is_fail(err)) {
        printf("%s\n", err_getstring(err));
        return 1;
    } else {
        return 0;
    }
}

static int rmdir(int argc, char *argv[])
{
    if(argc != 2) {
        printf("Usage: %s dir\n", argv[0]);
        return 1;
    }

    char *path = vfs_path_mkabsolute(cwd, argv[1]);
    errval_t err = vfs_rmdir(path);
    free(path);
    if (err_is_fail(err)) {
        printf("%s\n", err_getstring(err));
        return 1;
    } else {
        return 0;
    }
}

static int rm(int argc, char *argv[])
{
    if(argc < 2) {
        printf("Usage: %s file...\n", argv[0]);
        return 1;
    }

    int ret = 0;

    for (int i = 1; i < argc; i++) {
        char *path = vfs_path_mkabsolute(cwd, argv[i]);
        errval_t err = vfs_remove(path);
        free(path);
        if (err_is_fail(err)) {
            printf("%s: %s\n", argv[i], err_getstring(err));
            ret = 1;
        }
    }

    return ret;
}

static int cd(int argc, char *argv[])
{
    errval_t err;

    if (argc != 2) {
        printf("Usage: %s DIR\n", argv[0]);
        return 1;
    }

    char *newcwd = vfs_path_mkabsolute(cwd, argv[1]);

    // ensure directory exists, by attempting to open it
    vfs_handle_t dh;
    err = vfs_opendir(newcwd, &dh);
    if (err_is_fail(err)) {
        printf("cd to %s (-> %s) failed: %s\n",
               argv[1], newcwd, err_getstring(err));
        free(newcwd);
        return 1;
    }
    vfs_closedir(dh);

    // ok!
    free(cwd);
    cwd = newcwd;

    return 0;
}

static int pwd(int argc, char *argv[])
{
    printf("%s\n", cwd);
    return 0;
}

static int mnfs(int argc, char *argv[])
{
    char *args1[2] = { "mkdir", "/nfs" };
    mkdir(2, args1);
    char *args2[3] = { "mount", "/nfs", "nfs://10.110.4.4/local/nfs" };
    return mount(3, args2);
}

/// Open file(s) with a list of commands and execute them
static int src(int argc, char *argv[])
{
    if (argc < 2) {
        printf("Usage: %s file...\n", argv[0]);
    }

    int ret = 0;
    for (int i = 1; i < argc; i++) {
        char *path = vfs_path_mkabsolute(cwd, argv[i]);
        FILE *f = fopen(path, "r");
        if (!f) {
            printf("File %s not found\n", path);
            ret = 1;
            continue;
        }
        printf("Executing file %s\n", path);

        // Read one line at a time, make args out of it and execute it
        while (true) {
            char buf[1024] = "\0";
            char *p = fgets(buf, 1024, f);
            if (!p) {
                break;
            }

            char *q = strrchr(p, '\n');
            *q = '\0';
            char *cmdstr = strdup(p);
            char *cmd_argv[64];
            int cmd_argc = makeargs(cmdstr, cmd_argv);
            if (cmd_argc == 0) {
                continue;
            }

            struct cmd *cmd = find_command(cmd_argv[0]);
            if (!cmd) {
            } else {
                printf("running command: %s\n", p);
                int r = cmd->cmd(cmd_argc, cmd_argv);
                if (r != 0) {
                    printf("running command %s failed errorcode %d\n", p, r);
                }
            }
            free(cmdstr);
        }
        free(path);
    }
    return ret;
}

static int freecmd(int argc, char *argv[])
{
    struct mem_rpc_client *mc = get_mem_client();
    assert(mc != NULL);
    errval_t err;
    genpaddr_t available, total;

    err = ram_available(&available, &total);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "available");
        return EXIT_FAILURE;
    }

    printf("Free memory: %" PRIuGENPADDR " bytes\n", available);
    printf("Total memory: %" PRIuGENPADDR " bytes\n", total);

    return EXIT_SUCCESS;
}

static struct cmd commands[] = {
    {"help", help, "Output usage information about given shell command"},
    {"print_cspace", print_cspace, "Debug print-out of my cspace"},
    {"quit", quit, "Quit the shell"},
    {"ps", ps, "List running processes"},
    {"demo", demo, "Run barrelfish demo"},
    {"pixels", spawnpixels, "Spawn pixels on all cores"},
    {"mnfs", mnfs, "Mount script for NFS on emmentaler"},
    {"oncore", oncore, "Start program on specified core"},
    {"reset", reset, "Reset machine"},
    {"poweroff", poweroff, "Power down machine"},
    {"skb", skb, "Send command to system knowledge base"},
    {"mount", mount, "Mount file system"},
    {"ls", ls, "List directory contents"},
    {"cd", cd, "Change working directory"},
    {"pwd", pwd, "Print current working directory"},
    {"touch", touch, "Create an empty file"},
    {"cat", cat, "Print the contents of file(s)"},
    {"cat2", cat2, "Print the contents of file(s) into another file"},
    {"dd", dd, "copy stuff"},
    {"cp", cp, "Copy files"},
    {"rm", rm, "Remove files"},
    {"mkdir", mkdir, "Create a new directory"},
    {"rmdir", rmdir, "Remove an existing directory"},
    {"setenv", setenvcmd, "Set environment variables"},
    {"src", src, "Execute the list of commands in a file"},
    {"printenv", printenv, "Display environment variables"},
    {"free", freecmd, "Display amount of free memory in the system"},
};

static void getline(char *input, size_t size)
{
    int i = 0, in;

    do {
        in = getchar();
        if (in == '\b' || in == 0x7f /* DEL */) {
            if (i > 0) {
                i--;
                putchar('\b'); // FIXME: this kinda works on my terminal
                putchar(' ');
                putchar('\b');
                //puts("\033[1X"); // XXX: suitable for xterm
            }
        } else if (in != '\n' && i < size - 1) {
            input[i++] = in;
        }
        fflush(stdout);
    } while(in != '\n');
    assert(i < size);
    input[i] = '\0';
    putchar('\n');
    fflush(stdout);
}

static struct cmd *find_command(const char *name)
{
    for(int i = 0; i < ENTRIES(commands); i++) {
        struct cmd *cmd = &commands[i];

        if(strcmp(name, cmd->name) == 0) {
            return cmd;
        }
    }

    return NULL;
}

static int help(int argc, char *argv[])
{
    struct cmd *cmd;

    if(argc == 1) {
        printf("available commands:\n");
        for (int i=0; i < ENTRIES(commands); i++) {
            printf("%-15s", commands[i].name);
            if (((i + 1) % 5) == 0) {
                printf("\n");
            }
        }
        printf("\n");
        return 0;
    }

    if((cmd = find_command(argv[1])) != NULL) {
        printf("%s: %s\n", argv[1], cmd->usage);
        return 0;
    } else {
        printf("%s: %s: command not found\n", argv[0], argv[1]);
        return 1;
    }
}

static int makeargs(char *cmdline, char *argv[])
{
    char *p = cmdline;
    bool inquote = false;
    int argc = 0;

    if (p == NULL) {
        return 0;
    }

    while (*p == ' ') {
        p++;
    }

    if (*p == '\0') {
        return 0;
    }

    for(argv[argc++] = p; *p != '\0'; p++) {
        if (*p == '"') {
            inquote = !inquote;
            *p = ' '; // mega-kludge!
        } else if (*p == ' ' && !inquote) {
            *p++ = '\0';
            // Skip any redundant whitespace
            while(*p == ' ') {
                p++;
            }
            if (*p != '\0') {
                argv[argc++] = p;
                if (*p == '"') {
                    inquote = true;
                    *p = ' '; // mega-kludge
                }
            }
        }
    }

    return argc;
}

static uint8_t wait_domain_id(domainid_t domainid)
{
    uint8_t exitcode;
    errval_t err = spawn_wait(domainid, &exitcode, false);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn_wait");
    }
    return exitcode;
}

static void runbootscript(void)
{
    char cmdstr[1024];
    snprintf(cmdstr, 1024,"sh %s", BOOTSCRIPT_NAME);
    char *cmd_argv[64];
    int cmd_argc = makeargs(cmdstr, cmd_argv);
    int ret = src(cmd_argc, cmd_argv);
    if (ret != 0) {
        snprintf(cmdstr, 1024, "help");
        cmd_argc = makeargs(cmdstr, cmd_argv);
        help(cmd_argc, cmd_argv);
    }
}


int main(int argc, const char *argv[])
{
    char        input[MAX_LINE];
    int         exitcode = 0;
    bool        is_bootscript = true;
    coreid_t my_core_id = disp_get_core_id();

    vfs_init();

    cwd = strdup("/");

    printf("fish v0.2 -- pleased to meet you!\n");

    // run canned pre-boot commands
    if(is_bootscript) {
        runbootscript();
    }

    for(;;) {
        int             cmd_argc;
        char            *cmd_argv[64];      // Support a max of 64 cmd args
        struct cmd      *cmd;

        printf("> ");
        fflush(stdout);
        getline(input, MAX_LINE);
        cmd_argc = makeargs(input, cmd_argv);

        /* check for trailing '&' (== run in background) */
        bool wait = true;
        if (cmd_argc > 0) {
            size_t len = strlen(cmd_argv[cmd_argc - 1]);
            if (len > 0 && cmd_argv[cmd_argc - 1][len - 1] == '&') {
                wait = false;
                // remove '&' character from args
                if (len == 1) {
                    cmd_argc--;
                } else {
                    cmd_argv[cmd_argc - 1][len - 1] = '\0';
                }
            }
        }

        if (cmd_argc == 0) {
            continue;
        } else if ((cmd = find_command(cmd_argv[0])) != NULL) {
            exitcode = cmd->cmd(cmd_argc, cmd_argv);
        } else {
            // Try loading a program off disk if VFS is initialized
            domainid_t domain_id;
            exitcode = execute_program(my_core_id, cmd_argc, cmd_argv, &domain_id);

            // wait if it succeeds
            if(exitcode == 0 && wait) {
                exitcode = wait_domain_id(domain_id);
                char exitstr[128];
                snprintf(exitstr, 128, "%u", exitcode);
                int r = setenv("EXITCODE", exitstr, 1);
                assert(r == 0);
            }
        }
    }
}
