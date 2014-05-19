/** \file
 *  \brief Simple test to check functionality of fread
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

/* ------------------------------ MAIN ------------------------------ */

#define AMOUNT 100000

int main(int argc, char *argv[])
{
    errval_t err;

    vfs_init();
    
    err = vfs_mkdir("/filetests");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_mkdir failed");
    }

    /* Create a file with a lot of data */
    FILE *fh = fopen("/filetests/fread_test.dat", "w");
    if (!fh) {
        USER_PANIC("fopen failed");
    }

    for (int i = 0; i < AMOUNT; i++) {
        fprintf(fh, "h");
    }
    fclose(fh);

    /* Read out the data in chunks */
    fh = fopen("/filetests/fread_test.dat", "r");
    if (!fh) {
        USER_PANIC("fopen failed");
    }
    char *ptr = malloc(AMOUNT);
    assert(ptr);

    size_t size = fread(ptr, 10, 1, fh);
    if (size != 10) {
        USER_PANIC("fread did not read full amount");
    }

    size = fread(ptr, AMOUNT - 10, 1, fh);
    if (size != AMOUNT - 10) {
        USER_PANIC("fread did not read full amount");
    }

    size = fread(ptr, AMOUNT, 1, fh);
    if (size != 0) {
        USER_PANIC("fread did not read full amount");
    }

    printf("client done\n");
    return 0;
}
