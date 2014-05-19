#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

size_t (*_libc_terminal_read_func)(char *, size_t);
size_t (*_libc_terminal_write_func)(const char *, size_t);

void (*_libc_assert_func)(const char *, const char *, const char *, int);

typedef void *(*morecore_alloc_func_t)(size_t bytes, size_t *retbytes);
morecore_alloc_func_t sys_morecore_alloc;

typedef void (*morecore_free_func_t)(void *base, size_t bytes);
morecore_free_func_t sys_morecore_free;

int system(const char *cmd)
{
	return -1;
}

clock_t times(struct tms *buf)
{
  return -1;
}

clock_t _times_r(struct _reent *r, struct tms *buf)
{
  return -1;
}

int _getpid_r(struct _reent *r)
{
    assert(!"NYI");
}

int _stat_r(struct _reent *r, const char *path, struct stat *buf)
{
    assert(!"NYI");
}

void (*_libc_exit_func)(int) __attribute__((noreturn));
void _Exit(int status)
{
    _libc_exit_func(status);
}


/* copied from oldc sys-barrelfish */
extern void sys_print(const char *, size_t);

void abort(void)
{
    // Do not use stderr here.  It relies on too much to be working.
    sys_print("Aborted\n", 8);
    exit(EXIT_FAILURE);

    // Can't assert() here (would re-enter abort())
    sys_print("FATAL: exit() returned in abort()!\n", 36);
    for(;;);
}


/* this is very trivial to do, so copy it from posixcompat */
int _isatty_r(void *unused, int fd)
{
    return fd >=0 && fd <= 2;
}
