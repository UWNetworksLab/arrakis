#include <reent.h>
#include <_syslist.h>

#include <sys/types.h> /* for off_t */

/* defined in barrelfish/debug.h */
void debug_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Some targets provides their own versions of this functions.  Those
   targets should define REENTRANT_SYSCALLS_PROVIDED in TARGET_CFLAGS.  */

typedef int   fsopen_fn_t(const char *, int);
typedef int   fsread_fn_t(int, void *buf, size_t);
typedef int   fswrite_fn_t(int, const void *, size_t);
typedef int   fsclose_fn_t(int);
typedef off_t fslseek_fn_t(int, off_t, int);

#define FAIL_FN() \
do { \
    debug_printf("***** %s:%s() called. Something is probably wrong! Maybe " \
                 "you forgot to call vfs_init().\n", __FILE__,__FUNCTION__); \
    return -1; \
} while (0)

static int open_fail(const char *pathname, int flags)      { FAIL_FN(); }
static int read_fail(int fd, void *buf, size_t len)        { FAIL_FN(); }
static int write_fail(int fd, const void *buf, size_t len) { FAIL_FN(); }
static int close_fail(int fd)                              { FAIL_FN(); }
static off_t lseek_fail(int fd, off_t off, int whence)     { FAIL_FN(); }

static struct {
    fsopen_fn_t  *open;
    fsread_fn_t  *read;
    fswrite_fn_t *write;
    fsclose_fn_t *close;
    fslseek_fn_t *lseek;
} fs_ops = {
    .open = open_fail,
    .read = read_fail,
    .write = write_fail,
    .close = close_fail,
    .lseek = lseek_fail
};

void
newlib_register_fsops__(fsopen_fn_t *open_fn,
                        fsread_fn_t *read_fn,
                        fswrite_fn_t *write_fn,
                        fsclose_fn_t *close_fn,
                        fslseek_fn_t *lseek_fn)
{
    fs_ops.open  = open_fn  ? open_fn  : open_fail;
    fs_ops.read  = read_fn  ? read_fn  : read_fail;
    fs_ops.write = write_fn ? write_fn : write_fail;
    fs_ops.close = close_fn ? close_fn : close_fail;
    fs_ops.lseek = lseek_fn ? lseek_fn : lseek_fail;
}


#ifdef _REENT_ONLY
#ifndef REENTRANT_SYSCALLS_PROVIDED
#define REENTRANT_SYSCALLS_PROVIDED
#endif
#endif

#ifndef REENTRANT_SYSCALLS_PROVIDED

/* We use the errno variable used by the system dependent layer.  */
#undef errno
extern int errno;

ssize_t
static xwrite(int fd, const void *buf, size_t count)
{
    return fs_ops.write(fd, buf, count);
}

_ssize_t
_DEFUN (_write_r, (ptr, fd, buf, cnt),
     struct _reent *ptr _AND
     int fd _AND
     _CONST _PTR buf _AND
     size_t cnt)
{
  _ssize_t ret;

  errno = 0;
  if ((ret = (_ssize_t)xwrite (fd, buf, cnt)) == -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}

static ssize_t
xread(int fd, void *buf, size_t count)
{
    return fs_ops.read(fd, buf, count);
}

_ssize_t
_DEFUN (_read_r, (ptr, fd, buf, cnt),
     struct _reent *ptr _AND
     int fd _AND
     _PTR buf _AND
     size_t cnt)
{
  _ssize_t ret;


  errno = 0;
  if ((ret = (_ssize_t)xread (fd, buf, cnt)) == -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}


static int
xclose(int fd)
{
    return fs_ops.close(fd);
}

int
_DEFUN(_close_r, (ptr, fd),
     struct _reent *ptr _AND
     int fd)
{
  int ret;

  errno = 0;
  if ((ret = xclose (fd)) == -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}


static int
xopen(const char *pathname, int flags, ...)
{
    return fs_ops.open(pathname, flags);
}

int
_DEFUN (_open_r, (ptr, file, flags, mode),
     struct _reent *ptr _AND
     _CONST char *file _AND
     int flags _AND
     int mode)
{
  int ret;

  errno = 0;
  if ((ret = xopen (file, flags, mode)) == -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}

static off_t
xlseek(int fd, off_t offset, int whence)
{
    return fs_ops.lseek(fd, offset, whence);
}

_off_t
_DEFUN (_lseek_r, (ptr, fd, pos, whence),
     struct _reent *ptr _AND
     int fd _AND
     _off_t pos _AND
     int whence)
{
  _off_t ret;

  errno = 0;
  if ((ret = xlseek (fd, pos, whence)) == (_off_t) -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}

/* we don't provide an fstat function.  It's only use seems to be in:
 * stdio/makebuf.c:59, which we don't really care about. Programs can use libvfs
 * or libposixcompat for this functionality  */
static off_t
xfstat(int fd, void *stat)
{
    return -1;
}

int
_fstat_r (struct _reent *ptr, int fd, struct stat *pstat)
{
  int ret;

  errno = 0;
  if ((ret = xfstat (fd, pstat)) == -1 && errno != 0)
    ptr->_errno = errno;
  return ret;
}

#endif /* ! defined (REENTRANT_SYSCALLS_PROVIDED) */
