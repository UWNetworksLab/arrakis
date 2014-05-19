#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>



#define GROESSI (64*1024*1024)


int main(int c, char **argv)
{
    unsigned char *intarray = (unsigned char*)malloc(GROESSI);
    uint64_t *data = (uint64_t*)malloc(sizeof(uint64_t) * GROESSI);

    for (int i = 0; i < GROESSI; i++) {
        intarray[i] = 0;
    }
//    uint64_t start = rdtsc();
    for (int i = 0; i < GROESSI; i++) {
        uint64_t start = rdtsc();
        intarray[i] = 0;
        uint64_t end = rdtsc();
        data[i] = end - start;
    }
//    uint64_t end = rdtsc();
//    printf("zykla: %lu\n", end - start);
    printf("\n**************start***********\n\n");
    for (int i = 0; i < GROESSI; i++) {
        printf("###RES%d\t%lu\n",i, data[i]);
    }
    printf("\n**************stop***********\n\n");
    return 0;
}

