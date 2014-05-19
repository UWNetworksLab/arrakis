/**
 * \file
 * \brief ahci benchmarks
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <limits.h>
#include <trace/trace.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>

#define ENTRIES(array)  (sizeof(array) / sizeof(array[0]))

static const char *cwd = "/";

static int mount(const char *path, const char *uri)
{
    char *apath = vfs_path_mkabsolute(cwd, path);
    errval_t err = vfs_mount(apath, uri);
    free(apath);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "in vfs_mount %s %s", path, uri);
        return 1;
    }
    return 0;
}

static double
dd_bench(char *source, char *target, size_t blocksize, size_t count)
{
    vfs_handle_t source_vh = NULL;
    vfs_handle_t target_vh = NULL;

    size_t blocks_written = 0;

    size_t total_bytes_read = 0;
    size_t total_bytes_written = 0;

    errval_t err;
    double ret = 0;
    double kbps = 0.0;

    err = vfs_open(source, &source_vh);
    if (err_is_fail(err)) {
        printf("%s: %s (%ld)\n", source, err_getstring(err), err);
        return -1;
    }

    err = vfs_create(target, &target_vh);
    if (err_is_fail(err)) {
        // close source handle
        if (source_vh != NULL)
            vfs_close(source_vh);
        printf("%s: %s (%ld)\n", target, err_getstring(err), err);
        return -1;
    }

    uint8_t *buffer = malloc(blocksize);

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    uint64_t start = rdtsc();
#endif

    if (buffer == NULL) {
        ret = -2;
        printf("failed to allocate buffer of size %zd\n", blocksize);
        goto out;
    }

    size_t rsize, wsize;
    do {
        err = vfs_read(source_vh, buffer, blocksize, &rsize);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error reading file");
            ret = -1;
            goto out;
        }

        total_bytes_read += rsize;

        size_t wpos = 0;
        while (wpos < rsize) {
            if (wpos > 0)
                printf("was unable to write the whole chunk of size %zd. Now at pos: %zd of buffer\n", rsize, wpos);

            err = vfs_write(target_vh, &buffer[wpos], rsize - wpos, &wsize);
            if (err_is_fail(err) || wsize == 0) {
                DEBUG_ERR(err, "error writing file");
                ret = -1;
                goto out;
            }
            wpos += wsize;
            total_bytes_written += wsize;
        }

        blocks_written++;

    } while (rsize > 0 && !(count > 0 && blocks_written >= count));

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

    kbps = ((double)total_bytes_written / 1024.0) / elapsed_secs;

    if (ret == 0)
        return kbps;
    else
        return ret;
#else
    return ret;
#endif
}

struct bench_res {
    double read;
    double write;
};

static errval_t
fill_bench(char *target, uint8_t *buffer, size_t blocksize, size_t count, struct bench_res *result)
{
    vfs_handle_t target_vh = NULL;

    size_t blocks_written = 0;
    size_t blocks_read = 0;

    size_t total_bytes_read = 0;
    size_t total_bytes_written = 0;

    uint64_t start = 0, stop = 0;
    errval_t err;

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));
#endif

    errval_t ret = 0;

    err = vfs_open(target, &target_vh);
    if (err_is_fail(err)) {
        printf("%s: %s (%ld)\n", target, err_getstring(err), err);
        return err;
    }

#if defined(__x86_64__) || defined(__i386__)
    start = rdtsc();
#endif

    if (buffer == NULL) {
        ret = -2;
        printf("failed to allocate buffer of size %zd\n", blocksize);
        goto out;
    }

    size_t wsize;
    do {
        // initialize buffer
        for (size_t i = 0; i < blocksize; i += sizeof(size_t))
            *((size_t *)(buffer + i)) = blocks_written;

        // write to file
        size_t wpos = 0;
        while (wpos < blocksize) {
            if (wpos > 0)
                printf("was unable to write the whole chunk of size %zd. Now at pos: %zd of buffer\n", blocksize, wpos);

            err = vfs_write(target_vh, &buffer[wpos], blocksize - wpos, &wsize);
            if (err_is_fail(err) || wsize == 0) {
                DEBUG_ERR(err, "error writing file");
                ret = err;
                goto out;
            }
            wpos += wsize;
            total_bytes_written += wsize;
        }

        blocks_written++;

    } while (blocksize > 0 && !(count > 0 && blocks_written >= count));

    err = vfs_close(target_vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "in vfs_close");
        goto out;
    }


#if defined(__x86_64__) || defined(__i386__)
    stop = rdtsc();
    {
        uint64_t elapsed_msecs = ((stop - start) / tscperms);
        double elapsed_secs = (double)elapsed_msecs/1000.0;

        result->write = ((double)total_bytes_written / 1024.0) / elapsed_secs;
	printf("%lf\n", result->write);
    }

#endif
    err = vfs_open(target, &target_vh);
    if (err_is_fail(err)) {
        printf("%s: %s (%ld)\n", target, err_getstring(err), err);
        goto out;
    }
    err = vfs_seek(target_vh, VFS_SEEK_SET, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "seeking failed");
        ret = err;
        goto out;
    }
#if defined(__x86_64__) || defined(__i386__)
    start = rdtsc();
#endif

    size_t rsize;
    do {
        // read
        size_t rpos = 0;
        while (rpos < blocksize) {
            if (rpos > 0)
                printf("was unable to read whole chunk of size %zd. Now at pos: %zd of buffer\n", blocksize, rpos);

            err = vfs_read(target_vh, &buffer[rpos], blocksize - rpos, &rsize);
            if (err_is_fail(err) || wsize == 0) {
                DEBUG_ERR(err, "error reading file");
                ret = err;
                goto out;
            }
            rpos += rsize;
            total_bytes_read += rsize;
        }

        // verify data
        for (size_t i = 0; i < blocksize; i += sizeof(size_t)) {
            if (*((size_t *)(buffer + i)) != blocks_read) {
                printf("Verification failed! Block %zd, value %zd\n", blocks_read, *((size_t *)(buffer + i)) );
                ret = err;
                goto out;
            }
        }

        blocks_read++;
    } while (blocksize > 0 && !(count > 0 && blocks_read >= count));

out:
    if (target_vh != NULL) {
        err = vfs_close(target_vh);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
            ret = err;
        }
    }

#if defined(__x86_64__) || defined(__i386__)
    stop = rdtsc();
    {
        uint64_t elapsed_msecs = ((stop - start) / tscperms);
        double elapsed_secs = (double)elapsed_msecs/1000.0;

        result->read = ((double)total_bytes_read / 1024.0) / elapsed_secs;
	printf("%lf\n", result->read);
    }
#endif

    return ret;
}



static int
ahci_benchmark(int argc, char *argv[])
{
    if (argc != 3) {
        printf("usage: %s <source> <target>\nsource and target must be absolute paths!\n", argv[0]);
        return 1;
    }

    char *source = argv[1];
    char *target = argv[2];

    printf("Running dd disk test\n");

    size_t blocksize, count = 16;
    dd_bench(source, target, 131072, 16);


    for (blocksize = 131072; blocksize >= 512; blocksize /= 2, count *= 2) {
        double r1, r2, r3;
        r1 = dd_bench(source, target, blocksize, count);
        r2 = dd_bench(source, target, blocksize, count);
        r3 = dd_bench(source, target, blocksize, count);
        double kbps = (r1 + r2 + r3) / 3;
        printf("%zd\t%.3lf\n", blocksize, kbps);
    }

    printf("dd disk test completed\n");

    return 0;
}

static int 
ahci_fillbench(int argc, char *argv[])
{
    if (argc != 2) {
        printf("usage: %s <target>\ntarget must be an absolute path!\n", argv[0]);
        return 1;
    }

    char *target = argv[1];

    printf("Running fill disk test\n");

    size_t blocksize, count = 256;
    struct bench_res dummy;
    uint8_t *buffer = malloc(131072);
    fill_bench(target, buffer, 131072, count, &dummy);

    printf("bs\tread\tvar\twrite\tvar\n");

    for (blocksize = 131072; blocksize >= 512; blocksize /= 2, count *= 2) {
        struct bench_res r1, r2, r3;
        fill_bench(target, buffer, blocksize, count, &r1);
        fill_bench(target, buffer, blocksize, count, &r2);
        fill_bench(target, buffer, blocksize, count, &r3);
        double write_kbps = (r1.write + r2.write + r3.write) / 3;
        double read_kbps = (r1.read + r2.read + r3.read) / 3;

        double write_var = (write_kbps - r1.write) * (write_kbps - r1.write);
        write_var += (write_kbps - r2.write) * (write_kbps - r2.write);
        write_var += (write_kbps - r3.write) * (write_kbps - r3.write);

        write_var /= 3;

        double read_var = (read_kbps - r1.read) * (read_kbps - r1.read);
        read_var += (read_kbps - r2.read) * (read_kbps - r2.read);
        read_var += (read_kbps - r3.read) * (read_kbps - r3.read);

        read_var /= 3;

        printf("%zd\t%.3lf\t%.3lf\t%.3lf\t%.3lf\n", blocksize, read_kbps, read_var, write_kbps, write_var);
    }

    free(buffer);
    printf("fill disk test completed\n");

    return 0;

}

static int
shuffle_file(int argc, char *argv[])
{
    if (argc != 6) {
        printf("Usage: %s <file> <filesize> <blocksize> <count> <seed>\n", argv[0]);

        return 0;
    }

    size_t filesize = atoi(argv[2]);
    size_t blocksize = atoi(argv[3]);
    size_t count = atoi(argv[4]);
    size_t randval = atoi(argv[5]);
    size_t randbit;
    char * filename = argv[1];

    size_t rsize = 0;
    size_t wsize = 0;

    int ret = 0;


#define RAND_NEXT do {\
    randbit = ((randval >> 0) ^ (randval >> 3)) & 1;\
    randval = (randval >> 1) | (randbit << (sizeof(size_t) * CHAR_BIT - 1));\
} while (0)

#define RAND_BLOCK ((randval % (filesize / blocksize)) * blocksize)

    if (filesize % blocksize != 0) {
        printf("Please spcifiy the filesize as a multiple of blocksize\n");
        return 0;
    }

    uint8_t * buffer = malloc(blocksize);
    
    if (buffer == NULL) {
        printf("failed to allocate buffer of size %zd\n", blocksize);
        return 1;
    }

    errval_t err;
    vfs_handle_t f = NULL;
    char *path = vfs_path_mkabsolute(cwd, filename);
    err = vfs_open(path, &f);

    if (err_is_fail(err)) {
        printf("%s: %s\n", path, err_getstring(err));
        return 1;
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    //printf("ticks per millisec: %" PRIu64 "\n", tscperms);
    uint64_t start = rdtsc();
#endif

    size_t count2 = count;
    while (count2--) {
        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);
        
        err = vfs_read(f, buffer, blocksize, &rsize);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error reading file");
            ret = 1;
            goto out;
        }

        assert(rsize == blocksize);

        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);

        err = vfs_write(f, buffer, blocksize, &wsize);
        if (err_is_fail(err) || wsize == 0) {
            DEBUG_ERR(err, "error writing file");
            ret = 1;
            goto out;
        }

        assert(wsize == blocksize);
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t stop = rdtsc();
    uint64_t elapsed_msecs = ((stop - start) / tscperms);
    double elapsed_secs = (double)elapsed_msecs/1000.0;

    printf("start: %" PRIu64 " stop: %" PRIu64 "\n", start, stop);

    double kbps = ((double)(count * blocksize) / 1024.0) / elapsed_secs;

    printf("%zu bytes read. %zu bytes written. %f s, %f kB/s\n", count * blocksize, count * blocksize, elapsed_secs, kbps); 
#endif

out:
    if (buffer != NULL)
        free(buffer);
    
    if (f != NULL) {
        err = vfs_close(f);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

static int
rand_bench(char *target, uint8_t *buffer, size_t filesize, size_t blocksize, size_t count, size_t randval, struct bench_res *result)
{
    size_t randbit;

    size_t rsize = 0;
    size_t wsize = 0;

    int ret = 0;


#define RAND_NEXT do {\
    randbit = ((randval >> 0) ^ (randval >> 3)) & 1;\
    randval = (randval >> 1) | (randbit << (sizeof(size_t) * CHAR_BIT - 1));\
} while (0)

#define RAND_BLOCK ((randval % (filesize / blocksize)) * blocksize)

    if (filesize % blocksize != 0) {
        printf("Please spcifiy the filesize as a multiple of blocksize\n");
        return 0;
    }

    errval_t err;
    vfs_handle_t f = NULL;
    char *path = vfs_path_mkabsolute(cwd, target);
    err = vfs_open(path, &f);

    if (err_is_fail(err)) {
        printf("%s: %s\n", path, err_getstring(err));
        return 1;
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    //printf("ticks per millisec: %" PRIu64 "\n", tscperms);
    uint64_t start = rdtsc();
#endif

    size_t count2 = count;
    while (count2--) {
        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);

        err = vfs_read(f, buffer, blocksize, &rsize);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error reading file");
            ret = 1;
            goto out;
        }

        assert(rsize == blocksize);
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t stop = rdtsc();
    uint64_t elapsed_msecs = ((stop - start) / tscperms);
    double elapsed_secs = (double)elapsed_msecs/1000.0;

    result->read = ((double)(count * blocksize) / 1024.0) / elapsed_secs;

    start = rdtsc();
#endif

    count2 = count;
    while(count2--) {
        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);

        err = vfs_write(f, buffer, blocksize, &wsize);
        if (err_is_fail(err) || wsize == 0) {
            DEBUG_ERR(err, "error writing file");
            ret = 1;
            goto out;
        }

        assert(wsize == blocksize);
        vfs_flush(f);
    }

#if defined(__x86_64__) || defined(__i386__)
    stop = rdtsc();
    elapsed_msecs = ((stop - start) / tscperms);
    elapsed_secs = (double)elapsed_msecs/1000.0;

    result->write = ((double)(count * blocksize) / 1024.0) / elapsed_secs;
#endif

out:
    if (f != NULL) {
        err = vfs_close(f);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

#define FILESIZE (1024 * 1024 * 1024)
static int
ahci_randbench(int argc, char *argv[])
{
    if (argc != 2) {
        printf("usage: %s <target>\ntarget must be an absolute path!\n", argv[0]);
        return 1;
    }

    char *target = argv[1];

    printf("Running random disk test\n");

    size_t blocksize, count = 256;
    uint8_t *buffer = malloc(131072);

    printf("bs\tread\tvar\twrite\tvar\n");

    for (blocksize = 131072; blocksize >= 512; blocksize /= 2) {
        struct bench_res r1, r2, r3;
        rand_bench(target, buffer, FILESIZE, blocksize, count, 9147, &r1);
        rand_bench(target, buffer, FILESIZE, blocksize, count, 26447, &r2);
        rand_bench(target, buffer, FILESIZE, blocksize, count, 5109, &r3);
        double read_kbps = (r1.read + r2.read + r3.read) / 3;
        double read_var = (read_kbps - r1.read) * (read_kbps - r1.read);
        read_var += (read_kbps - r2.read) * (read_kbps - r2.read);
        read_var += (read_kbps - r3.read) * (read_kbps - r3.read);
        read_var /= 3;

        double write_kbps = (r1.write + r2.write + r3.write) / 3;
        double write_var = (write_kbps - r1.write) * (write_kbps - r1.write);
        write_var += (write_kbps - r2.write) * (write_kbps - r2.write);
        write_var += (write_kbps - r3.write) * (write_kbps - r3.write);
        write_var /= 3;

        printf("%zd\t%.3lf\t%.3lf\t%.3lf\t%.3lf\n", blocksize, read_kbps, read_var, write_kbps, write_var);
    }

    free(buffer);
    printf("random disk test completed\n");

    return 0;

}

static int
rand_bench_time(char *target, uint8_t *buffer, size_t filesize, size_t blocksize, size_t count, size_t randval, struct bench_res *result)
{
    size_t randbit;

    size_t rsize = 0;
    size_t wsize = 0;

    int ret = 0;


#define RAND_NEXT do {\
    randbit = ((randval >> 0) ^ (randval >> 3)) & 1;\
    randval = (randval >> 1) | (randbit << (sizeof(size_t) * CHAR_BIT - 1));\
} while (0)

#define RAND_BLOCK ((randval % (filesize / blocksize)) * blocksize)

    if (filesize % blocksize != 0) {
        printf("Please spcifiy the filesize as a multiple of blocksize\n");
        return 0;
    }

    errval_t err;
    vfs_handle_t f = NULL;
    char *path = vfs_path_mkabsolute(cwd, target);
    err = vfs_open(path, &f);

    if (err_is_fail(err)) {
        printf("%s: %s\n", path, err_getstring(err));
        return 1;
    }

#if defined(__x86_64__) || defined(__i386__)
    uint64_t tscperms;
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    //printf("ticks per millisec: %" PRIu64 "\n", tscperms);
    uint64_t start = rdtsc();
#endif

    size_t count2 = count;
    while (count2--) {
        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);

        err = vfs_read(f, buffer, blocksize, &rsize);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error reading file");
            ret = 1;
            goto out;
        }

        assert(rsize == blocksize);
    }
#if defined(__x86_64__) || defined(__i386__)
    uint64_t stop = rdtsc();
    uint64_t elapsed_msecs = ((stop - start) / tscperms);
    double elapsed_secs = (double)elapsed_msecs/1000.0;

    result->read = elapsed_secs;

    start = rdtsc();
#endif

    count2 = count;
    while(count2--) {
        RAND_NEXT;
        vfs_seek(f, VFS_SEEK_SET, RAND_BLOCK);

        err = vfs_write(f, buffer, blocksize, &wsize);
        if (err_is_fail(err) || wsize == 0) {
            DEBUG_ERR(err, "error writing file");
            ret = 1;
            goto out;
        }

        assert(wsize == blocksize);
        vfs_flush(f);
    }

#if defined(__x86_64__) || defined(__i386__)
    stop = rdtsc();
    elapsed_msecs = ((stop - start) / tscperms);
    elapsed_secs = (double)elapsed_msecs/1000.0;

    result->write = elapsed_secs;
#endif

out:
    if (f != NULL) {
        err = vfs_close(f);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

#define FILESIZE (1024 * 1024 * 1024)
static int
ahci_randbench_time(int argc, char *argv[])
{
    if (argc != 2) {
        printf("usage: %s <target>\ntarget must be an absolute path!\n", argv[0]);
        return 1;
    }

    char *target = argv[1];

    printf("Running random disk test\n");

    size_t blocksize, count = 256;
    uint8_t *buffer = malloc(131072);

    printf("bs\tread\tvar\n");

    for (blocksize = 131072; blocksize >= 512; blocksize /= 2) {
        struct bench_res r1, r2, r3;
        rand_bench_time(target, buffer, FILESIZE, blocksize, count, 9147, &r1);
        rand_bench_time(target, buffer, FILESIZE, blocksize, count, 26447, &r2);
        rand_bench_time(target, buffer, FILESIZE, blocksize, count, 5109, &r3);
        r1.read /= count;
        r2.read /= count;
        r3.read /= count;
        r1.write /= count;
        r2.write /= count;
        r3.write /= count;
        double read_sec = (r1.read + r2.read + r3.read) / 3;
        double read_var = (read_sec - r1.read) * (read_sec - r1.read);
        read_var += (read_sec - r2.read) * (read_sec - r2.read);
        read_var += (read_sec - r3.read) * (read_sec - r3.read);
        read_var /= 3;
        double write_sec = (r1.write + r2.write + r3.write) / 3;
        double write_var = (write_sec - r1.write) * (write_sec - r1.write);
        write_var += (write_sec - r2.write) * (write_sec - r2.write);
        write_var += (write_sec - r3.write) * (write_sec - r3.write);
        write_var /= 3;

        printf("%zd\t%.3lf\t%.3lf\t%.3lf\t%.3lf\n", blocksize, read_sec, read_var, write_sec, write_var);
    }

    free(buffer);
    printf("random disk test completed\n");

    return 0;

}

static int
ahci_read_write(int argc, char *argv[])
{
    if (argc != 4) {
        printf("usage: %s <block device> <blocksize> <count>\n", argv[0]);
        return 1;
    }

    char *dev = argv[1];
    size_t blocksize = atoi(argv[2]);
    size_t count = atoi(argv[3]);
    printf("malloc buf\n");
    uint8_t *buf = malloc(blocksize);
    int ret = 0;

    errval_t err;
    vfs_handle_t f = NULL;
    char *path = vfs_path_mkabsolute(cwd, dev);
    printf("open\n");
    err = vfs_open(path, &f);

    if (err_is_fail(err)) {
        printf("%s: %s\n", path, err_getstring(err));
        ret = 1;
        goto out;
    }

    for (int i = 0; i < count; i++) {
        size_t read = 0;
        size_t rsize;
        while(read < blocksize) {
            printf("read\n");
            err = vfs_read(f, &buf[read], blocksize, &rsize);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error reading file");
                ret = 1;
                goto out;
            }
            read += rsize;
        }

        size_t written = 0;
        size_t wsize;
        while(written < blocksize) {
            printf("write\n");
            err = vfs_write(f, &buf[written], blocksize, &wsize);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error writing file");
                ret = 1;
                goto out;
            }
            written += wsize;
        }
    }

out:
    if (buf)
        printf("free\n");
        free(buf);
    if (f) {
        printf("close\n");
        err = vfs_close(f);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
        }
    }

    return ret;
}

typedef int (*Command)(int argc, char *argv[]);
struct cmd {
    const char  *name;
    Command     cmd;
    const char  *usage;
};

static struct cmd commands[] = {
    { "benchmark", ahci_benchmark, "benchmark ahci" },
    { "fillbench", ahci_fillbench, "benchmark ahci sequential" },
    { "randbench", ahci_randbench, "benchmark ahci random (throughput)" },
    { "randbench_time", ahci_randbench_time, "benchmark ahci random (response time)" },
    { "read_write", ahci_read_write, "read from block device and write back" },
    { "shuffle_file", shuffle_file, "Shuffle a file around" },
};

#define NUM_COMMANDS (sizeof(commands)/sizeof(commands[0]))

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

static void usage(char *progname)
{
    printf("%s <mountpoint> <mount uri> <command> <command_args...>\n", progname);
    for (int i = 0; i < NUM_COMMANDS; i++) {
        printf("\t%s: %s\n", commands[i].name, commands[i].usage);
    }
    printf("To get detailed usage informations for the different benchmarks\n"
           "run the command without additional arguments.\n\n");
}

int main(int argc, char *argv[])
{
    // mount
    if (argc < 4) {
        usage(argv[0]);
        return 1;
    }
    // argv[1] mountpoint, argv[2] mount uri
    if(mount(argv[1], argv[2]))
        return 1;

    // find command
    struct cmd *cmd;
    if ((cmd = find_command(argv[3])) == NULL)
        return 1;

    return cmd->cmd(argc-3, argv+3);
}
