#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/types.h>
#include <barrelfish/cap_predicates.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>

bool debug_all_the_things = false;

#define DEBUG_ALL_THE_THINGS(...) \
    do { \
        if (debug_all_the_things) \
            printf(__VA_ARGS__); \
    } while(0)


#define CAP_COUNT 8
#define BASE_BITS 12
struct cte caps[CAP_COUNT];
extern struct cte *mdb_root;
static inline void setup(void) {
    memset(caps, 0, CAP_COUNT*sizeof(struct cte));
    for (size_t i = 0; i < CAP_COUNT; i++) {
        struct capability *cap = &caps[i].cap;
        cap->type = ObjType_PhysAddr;
        cap->rights = CAPRIGHTS_ALLRIGHTS;
        cap->u.ram.base = 0x0;
        cap->u.ram.bits = BASE_BITS + i;
        mdb_insert(&caps[i]);
    }
}

int main(int argc, char *argv[])
{
    setup();

    struct cte devframe;
    struct capability *cap = &devframe.cap;
    cap->type = ObjType_DevFrame;
    cap->rights = CAPRIGHTS_ALLRIGHTS;
    cap->u.devframe.base = 0x0;
    cap->u.devframe.bits = BASE_BITS;
    mdb_insert(&devframe);

    struct cte *retcte;
    mdb_find_cap_for_address(0x0, &retcte);

    if (retcte != &devframe) {
        printf("devframe@%p\n", &devframe);
        printf("  retcap@%p\n", retcte);
        if (retcte) {
            printf("retcap:   (%d) 0x%lx %zd\n", retcte->cap.type, get_address(&retcte->cap), get_size(&retcte->cap));
        }
        printf("devframe: (%d) 0x%lx %zd\n", devframe.cap.type, get_address(&devframe.cap), get_size(&devframe.cap));
        mdb_dump_all_the_things();
    }

    return 0;
}
