#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

#define MEASUREMENTS    1000000

int main(int argc, char *argv[])
{
    char a[1024], b[1024];
    uint64_t sum = 0;

    for(int i = 0; i < MEASUREMENTS; i++) {
        uint64_t start = rdtsc();
        memcpy(a, b, 1024);
        uint64_t end = rdtsc();
        sum += end - start;
    }

    printf("avg duration: %" PRIu64 " cycles\n", sum / MEASUREMENTS);

  /* for(;;); */
  return 0;
}
