#define _GNU_SOURCE
#include <dlfcn.h>         // dlsym(), dlopen() [OS X]
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <pci/devids.h>
#include <errors/errno.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
/* #include <fcntl.h> */
#include "linux_defs.h"
#include <storage/vsic.h>
#include <storage/vsa.h>

static int myfd = -1;
static off_t mypos = 0;
static FILE *myfile = NULL;
static struct storage_vsic vsic;
static struct storage_vsa vsa;

int open64(const char *pathname, int flags, mode_t mode)
{
  typedef int (*func_t)(const char *pathname, int flags, mode_t mode);
  static func_t original = NULL;

  if(original == NULL) {
    original = (func_t)dlsym(RTLD_NEXT, __func__);
    assert(original != NULL);
  }

  if(!strcmp(pathname, "appendonly.aof")) {
    printf("open64('%s', %d) called\n", pathname, flags);
    /* int result = original(pathname, flags, mode); */
    /* myfd = result; */
    /* return result; */
    static const char *argv[2] = { "megaraid", "0x37c000" };
    errval_t err = storage_vsic_driver_init(2, argv, &vsic);
    assert(err_is_ok(err));
    err = storage_vsa_acquire(&vsa, "0", 1 * 1024 * 1024);
    assert(err_is_ok(err));
    myfd = 100;
    return myfd;
  } else {
    int result = original(pathname, flags, mode);
    return result;
  }
}

FILE *fopen64(const char *path, const char *mode)
{
  typedef FILE *(*func_t)(const char *path, const char *mode);
  static func_t original = NULL;

  if(original == NULL) {
    original = (func_t)dlsym(RTLD_NEXT, __func__);
    assert(original != NULL);
  }

  if(!strcmp(path, "appendonly.aof")) {
    printf("fopen64('%s', '%s') called from %p\n", path, mode,
	   __builtin_return_address(0));
    FILE *result = original(path, mode);
    myfile = result;
    return result;
  } else {
    FILE *result = original(path, mode);
    return result;
  }
}

#define BUF_SIZE	65536

void *user_alloc(size_t size, uintptr_t *paddr);

ssize_t write(int fd, const void *buf, size_t count)
{
  if(fd == myfd) {
    static uint8_t *mybuf = NULL;

    if(mybuf == NULL) {
      lpaddr_t paddr;
      mybuf = user_alloc(BUF_SIZE, &paddr);
      assert(mybuf != NULL);
      memset(mybuf, 0, BUF_SIZE);
    }

    memcpy(mybuf, buf, count);

    size_t mycount = STORAGE_VSIC_ROUND(&vsic, count);
    /* printf("write on my fd called, count = %zu, mycount = %zu\n", count, mycount); */
    errval_t err = vsic.ops.write(&vsic, &vsa, mypos, mycount, (void *)mybuf);
    assert(err_is_ok(err));
    mypos += mycount;
    return count;
  } else {
    typedef ssize_t (*func_t)(int fd, const void *buf, size_t count);
    static func_t original = NULL;

    if(original == NULL) {
      original = (func_t)dlsym(RTLD_NEXT, __func__);
      assert(original != NULL);
    }

    return original(fd, buf, count);
  }
}

int fdatasync(int fd)
{
  if(fd == myfd) {
    /* printf("fdatasync on my fd called\n"); */
    errval_t err = vsic.ops.flush(&vsic, &vsa);
    assert(err_is_ok(err));
    err = vsic.ops.wait(&vsic);
    assert(err_is_ok(err));
    return 0;
  } else {
    typedef ssize_t (*func_t)(int fd);
    static func_t original = NULL;

    if(original == NULL) {
      original = (func_t)dlsym(RTLD_NEXT, __func__);
      assert(original != NULL);
    }

    return original(fd);
  }
}
