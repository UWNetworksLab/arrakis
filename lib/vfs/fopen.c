/*
 * Australian Public Licence B (OZPLB)
 * 
 * Version 1-0
 * 
 * Copyright (c) 2004 National ICT Australia
 * 
 * All rights reserved. 
 * 
 * Developed by: Embedded, Real-time and Operating Systems Program (ERTOS)
 *               National ICT Australia
 *               http://www.ertos.nicta.com.au
 * 
 * Permission is granted by National ICT Australia, free of charge, to
 * any person obtaining a copy of this software and any associated
 * documentation files (the "Software") to deal with the Software without
 * restriction, including (without limitation) the rights to use, copy,
 * modify, adapt, merge, publish, distribute, communicate to the public,
 * sublicense, and/or sell, lend or rent out copies of the Software, and
 * to permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimers.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimers in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of National ICT Australia, nor the names of its
 *       contributors, may be used to endorse or promote products derived
 *       from this Software without specific prior written permission.
 * 
 * EXCEPT AS EXPRESSLY STATED IN THIS LICENCE AND TO THE FULL EXTENT
 * PERMITTED BY APPLICABLE LAW, THE SOFTWARE IS PROVIDED "AS-IS", AND
 * NATIONAL ICT AUSTRALIA AND ITS CONTRIBUTORS MAKE NO REPRESENTATIONS,
 * WARRANTIES OR CONDITIONS OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO ANY REPRESENTATIONS, WARRANTIES OR CONDITIONS
 * REGARDING THE CONTENTS OR ACCURACY OF THE SOFTWARE, OR OF TITLE,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT,
 * THE ABSENCE OF LATENT OR OTHER DEFECTS, OR THE PRESENCE OR ABSENCE OF
 * ERRORS, WHETHER OR NOT DISCOVERABLE.
 * 
 * TO THE FULL EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL
 * NATIONAL ICT AUSTRALIA OR ITS CONTRIBUTORS BE LIABLE ON ANY LEGAL
 * THEORY (INCLUDING, WITHOUT LIMITATION, IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHERWISE) FOR ANY CLAIM, LOSS, DAMAGES OR OTHER
 * LIABILITY, INCLUDING (WITHOUT LIMITATION) LOSS OF PRODUCTION OR
 * OPERATION TIME, LOSS, DAMAGE OR CORRUPTION OF DATA OR RECORDS; OR LOSS
 * OF ANTICIPATED SAVINGS, OPPORTUNITY, REVENUE, PROFIT OR GOODWILL, OR
 * OTHER ECONOMIC LOSS; OR ANY SPECIAL, INCIDENTAL, INDIRECT,
 * CONSEQUENTIAL, PUNITIVE OR EXEMPLARY DAMAGES, ARISING OUT OF OR IN
 * CONNECTION WITH THIS LICENCE, THE SOFTWARE OR THE USE OF OR OTHER
 * DEALINGS WITH THE SOFTWARE, EVEN IF NATIONAL ICT AUSTRALIA OR ITS
 * CONTRIBUTORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH CLAIM, LOSS,
 * DAMAGES OR OTHER LIABILITY.
 * 
 * If applicable legislation implies representations, warranties, or
 * conditions, or imposes obligations or liability on National ICT
 * Australia or one of its contributors in respect of the Software that
 * cannot be wholly or partly excluded, restricted or modified, the
 * liability of National ICT Australia or the contributor is limited, to
 * the full extent permitted by the applicable legislation, at its
 * option, to:
 * a.  in the case of goods, any one or more of the following:
 * i.  the replacement of the goods or the supply of equivalent goods;
 * ii.  the repair of the goods;
 * iii. the payment of the cost of replacing the goods or of acquiring
 *  equivalent goods;
 * iv.  the payment of the cost of having the goods repaired; or
 * b.  in the case of services:
 * i.  the supplying of the services again; or
 * ii.  the payment of the cost of having the services supplied again.
 * 
 * The construction, validity and performance of this licence is governed
 * by the laws in force in New South Wales, Australia.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/* VFS support for fopen() and friends in oldc / newlib */

#define _USE_XOPEN // for strdup()
#include <string.h>
#include <stdio.h>
#include <stdio_file.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <errno.h>
#include "vfs_backends.h"
#include <vfs/vfs_fd.h>

static long int
barrelfish_eof(void *handle)
{
    /* contrary to what its name may imply, the purpose of this function is to
     * return the size of the file (see the fseek() implementation) */

    struct vfs_fileinfo info;
    errval_t err;

    err = vfs_stat(handle, &info);
    if (err_is_ok(err)) {
        return info.size;
    } else {
        DEBUG_ERR(err, "error in vfs_stat");
        // how do we return an error?
        return 0;
    }
}

static size_t
barrelfish_read(void *data, long int position, size_t count, void *handle)
{
    size_t readcount;
    errval_t err;

    err = vfs_seek(handle, VFS_SEEK_SET, position);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning only: vfs_seek failed");
    }

    err = vfs_read(handle, data, count, &readcount);
    if (err_is_ok(err)) {
        return readcount;
    } else {
        return 0;
    }
}

static size_t
barrelfish_write(void *data, long int position, size_t count, void *handle)
{
    size_t writecount;
    errval_t err;

    err = vfs_seek(handle, VFS_SEEK_SET, position);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Warning only: vfs_seek failed");
    }

    err = vfs_write(handle, data, count, &writecount);
    if (err_is_ok(err)) {
        return writecount;
    } else {
        return 0;
    }
}

static int
barrelfish_close(void *handle)
{
    errval_t err = vfs_close(handle);
    return err_is_ok(err) ? 0 : EOF;
}

 __attribute__((unused)) static struct __file *
barrelfish_oldc_fopen(const char *fname, const char *mode)
{
    struct __file *newfile;
    vfs_handle_t vh;
    errval_t err;

    char *abspath = vfs_path_mkabs(fname);
    assert(abspath != NULL);
    
    //debug_printf("fopen('%s', '%s')\n", fname, mode);

    if (mode[0] == 'r') { // read: file must exist
        err = vfs_open(abspath, &vh);
    } else { // write or append
        err = vfs_create(abspath, &vh);
    }
    if (err_is_fail(err)) {
        if (err_no(err) != FS_ERR_NOTFOUND) {
            DEBUG_ERR(err, "vfs_open failed in fopen('%s'): %s\n", fname, abspath);
        } else {
            errno = ENOENT;
        }
        free(abspath);
        return NULL;
    }
    free(abspath);

    if (mode[0] == 'a') {
        // append: seek to end of file
        err = vfs_seek(vh, VFS_SEEK_END, 0);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "vfs_seek");
        }
    }

    newfile = malloc(sizeof(struct __file));
    if (newfile == NULL) {
        vfs_close(vh);
        return NULL;
    }

    newfile->write_fn = barrelfish_write;
    newfile->read_fn = barrelfish_read;
    newfile->close_fn = barrelfish_close;
    newfile->eof_fn = barrelfish_eof;

    err = vfs_tell(vh, &newfile->current_pos);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_tell");
    }

    newfile->handle = vh;
    newfile->buffering_mode = _IOFBF;
    newfile->buffer = NULL;
    newfile->buf_pos = 0;
    /* newfile->buf_size = 65536; */
    //    newfile->buf_size = 262144;
    newfile->buf_size = (16 * 1024 * 1024);
    newfile->unget_pos = 0;
    newfile->eof = 0;
    newfile->error = 0;
    newfile->rbuffer = NULL;
    newfile->rbuf_pos = NULL;
    newfile->rbuf_size = 0;
    newfile->rbuf_valid = 0;

    thread_mutex_init(&newfile->mutex);

    return newfile;
}

#ifdef CONFIG_OLDC
extern struct __file *(*_oldc_fopen_func)(const char *fname, const char *prot);
#endif

#ifdef CONFIG_NEWLIB
typedef int   fsopen_fn_t(const char *, int);
typedef int   fsread_fn_t(int, void *buf, size_t);
typedef int   fswrite_fn_t(int, const void *, size_t);
typedef int   fsclose_fn_t(int);
typedef off_t fslseek_fn_t(int, off_t, int);
void
newlib_register_fsops__(fsopen_fn_t *open_fn,
                        fsread_fn_t *read_fn,
                        fswrite_fn_t *write_fn,
                        fsclose_fn_t *close_fn,
                        fslseek_fn_t *lseek_fn);
#endif

void vfs_fopen_init(void)
{
    /* set the function pointer that oldc libc calls through */
    #ifdef CONFIG_OLDC
    _oldc_fopen_func = barrelfish_oldc_fopen;
    #endif

    #ifdef CONFIG_NEWLIB
    newlib_register_fsops__(vfsfd_open, vfsfd_read, vfsfd_write,
                            vfsfd_close, vfsfd_lseek);
    #endif

    // Initialize working directory if not set already
    int r = setenv("PWD", "/", 0);
    assert(r == 0);
}
