/** \file
 *  \brief Test FAT32 file system implementation
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>


static void walk_dir(char* path)
{
    printf("%s:%d: path=%s\n", __FUNCTION__, __LINE__, path);
    vfs_handle_t dhandle;
    struct vfs_fileinfo info;
    
    errval_t err = vfs_opendir(path, &dhandle);
    assert(err_is_ok(err));

    char* name;
    while(err_is_ok(vfs_dir_read_next(dhandle, &name, &info))) {

        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
            continue;
        char* newpath = malloc(snprintf(NULL, 0, "%s/%s", path, name) + 1);
        sprintf(newpath, "%s/%s", path, name);

        switch (info.type) {
        case VFS_DIRECTORY:
            printf("%s:%d: Found directory %s\n", __FUNCTION__, __LINE__, newpath);
            walk_dir(newpath);
            break;
        
        case VFS_FILE:
            printf("%s:%d: Found file %s\n", __FUNCTION__, __LINE__, newpath);
            vfs_handle_t fhandle;

            err = vfs_open(newpath, &fhandle);
            char* buf = malloc(info.size);
            size_t bytes_read = 0;
            err = vfs_read(fhandle, buf, info.size, &bytes_read);
            assert(err_is_ok(err));
            assert(bytes_read == info.size);
            printf("%s:%d: File content is (bytes=%d):\n%s\n", 
                   __FUNCTION__, __LINE__, bytes_read, buf);

            free(buf);
            break;
        }
        free(name);
        free(newpath);
    }

    err = vfs_closedir(dhandle);
    assert(err_is_ok(err));   
}

int main(int argc, char *argv[])
{
    vfs_init();
    vfs_mkdir("/fat");
    
    errval_t err = vfs_mount("/fat", "fat32://0+0");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_fat_mount failed");
    }

    walk_dir("/fat");

    return 0;
}
