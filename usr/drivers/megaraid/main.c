#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include <errors/errno.h>
#include <storage/storage.h>

static struct storage_vsic vsic;
static struct storage_vsa vsa;

#define BUF_SIZE	1024
#define ITERATIONS	10000

#ifndef BARRELFISH
void *user_alloc(size_t size, uintptr_t *paddr);
#endif

static uint64_t timestamps1[ITERATIONS], timestamps2[ITERATIONS];

#ifndef BARRELFISH
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

int main(int argc, const char *argv[])
{
  errval_t err = storage_vsic_driver_init(argc, argv, &vsic);
  assert(err_is_ok(err));
  err = storage_vsa_acquire(&vsa, "0", 1 * 1024 * 1024);
  assert(err_is_ok(err));

#ifndef BARRELFISH
  uintptr_t paddr;
  uint8_t *buf = user_alloc(BUF_SIZE, &paddr);
#else
  uint8_t *buf = malloc(BUF_SIZE);
#endif
  assert(buf != NULL);

  memset(buf, 0, BUF_SIZE);

  for(int i = 0; i < ITERATIONS; i++) {
    /* printf("Iteration %u...\n", i); */
    uint64_t begin = rdtsc();
    err = vsic.ops.write(&vsic, &vsa, 0, BUF_SIZE, buf);
    assert(err_is_ok(err));
    err = vsic.ops.flush(&vsic, &vsa);
    assert(err_is_ok(err));
    uint64_t start = rdtsc();
    err = vsic.ops.wait(&vsic);
    assert(err_is_ok(err));
    timestamps2[i] = rdtsc() - start;
    timestamps1[i] = start - begin;
  }

  printf("# write size = %u\n", BUF_SIZE);
  for(int i = 0; i < ITERATIONS; i++) {
      printf("%" PRIu64 "\t%" PRIu64 "\n", timestamps1[i], timestamps2[i]);
  }

  return 0;
}
