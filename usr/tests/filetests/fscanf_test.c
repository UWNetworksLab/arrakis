/** \file
 *  \brief Simple test to check functionality of fscanf
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <errno.h>

int main(int argc, char *argv[])
{
    errval_t err;

    vfs_init();
    
    err = vfs_mkdir("/filetests");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_mkdir failed");
    }

    /* Create a file with a lot of data */
    FILE *fh = fopen("/filetests/fscanf_test.dat", "w");
    if (!fh) {
        USER_PANIC("fopen failed (%d)", errno);
    }

    // Write various datatypes
    fprintf(fh, "a %d\n", 1234);
    fprintf(fh, "b %x\n", 1234);
    fprintf(fh, "c %ld\n", 1234567890UL);

    fclose(fh);

    /* Read out the data in chunks */
    fh = fopen("/filetests/fscanf_test.dat", "r");
    if (!fh) {
        USER_PANIC("fopen failed (%d)", errno);
    }

    // Read the various datatypes
    int val, r;
    r = fscanf(fh, "a %d\n", &val);
    assert(r == 1);
    if(val != 1234) {
        USER_PANIC("fscanf read wrong value %d != 1234", val);
    }
    r = fscanf(fh, "b %x\n", &val);
    assert(r == 1);
    if(val != 1234) {
        USER_PANIC("fscanf read wrong value %d != 1234", val);
    }
    long lval;
    r = fscanf(fh, "c %ld\n", &lval);
    assert(r == 1);
    if(lval != 1234567890UL) {
        USER_PANIC("fscanf read wrong value %ld != 1234567890", lval);
    }

    fclose(fh);
    printf("client done\n");
    return 0;
}
