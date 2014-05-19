/**
 * \brief Simple benchmark for vfs performance.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <vfs/vfs.h>

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

//#define FILENAME    "/tmpfile"
#define FILENAME    "/nfs/fuchsr/tmpfile"

static void single_run(int32_t chunksize, int32_t repetitions)
{
    errval_t err;
    vfs_handle_t handle;

    // create file
    err = vfs_create(FILENAME, &handle);
    assert(err_is_ok(err));

    // create chunk containing arbitraty data
    uint8_t *chunk = malloc(chunksize);
    assert(chunk != NULL);

    // start time
    printf("Start run with chunksize: %" PRId32 ", repetitions: %" PRId32
           "\n", chunksize, repetitions);
    cycles_t start_cycles = bench_tsc();

    size_t written = 0;
    for (int32_t i = 0; i < repetitions; i++) {
        err = vfs_write(handle, chunk, chunksize, &written);
        assert(err_is_ok(err));
        assert(written == chunksize);
    }
    err = vfs_close(handle);
    assert(err_is_ok(err));

    // end time
    cycles_t end_cycles = bench_tsc();

    // evaluation
    cycles_t cycles = end_cycles - start_cycles;
    uint64_t ms = bench_tsc_to_ms(cycles);
    double sec = (double) ms / 1000.0;
    int64_t bytes_written = chunksize * repetitions;
    double kibibytes_written = (double) bytes_written / 1024.0;
    double mebibytes_written = (double) bytes_written / (1024.0 * 1024.0);
    double kibps = kibibytes_written / sec;
    printf("%" PRId64 " bytes (%.1f KiB, %.1f MiB) written in %" PRIuCYCLES
           " cycles (%" PRIu64 " ms, %.1f s) -> %.1f KiB/s\n", bytes_written,
           kibibytes_written, mebibytes_written, cycles, ms, sec, kibps);

    // cleanup
    free(chunk);
    err = vfs_remove(FILENAME);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    errval_t err;

    // initialization
    vfs_init();
    bench_init();

    // mount nfs
    err = vfs_mkdir("/nfs");
    assert(err_is_ok(err));
    err = vfs_mount("/nfs", "nfs://10.110.4.4/local/nfs");
    assert(err_is_ok(err));

    // argument processing
    if (argc == 3) {
        printf("Started vfs_bench in command-line mode\n");

        int32_t chunksize = atol(argv[1]);
        int32_t repetitions = atol(argv[2]);

        single_run(chunksize, repetitions);
    } else {
        printf("Started vfs_bench.\n");

        for (int32_t i = 1; i < 20; i++) {
            single_run(4096, i * 2000);
        }
    }

    //err = vfs_unmount("/nfs"); // unmount is NYI
    //assert(err_is_ok(err));
    err = vfs_rmdir("/nfs");
    assert(err_is_ok(err));

    return 0;
}
