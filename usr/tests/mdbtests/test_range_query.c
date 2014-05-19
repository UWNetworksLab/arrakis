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


#define RUNS 5000
#define MIN_RANGES 30
#define MAX_RANGES 100
#define MAX_ADDR_BITS 20
#define MAX_ADDR (1<<MAX_ADDR_BITS)
#define QUERY_COUNT 20

struct node {
    genvaddr_t address;
    size_t size;
    int tiebreak;
};

static inline size_t randrange(size_t begin, size_t end)
{
    return begin + rand() / (RAND_MAX / (end - begin + 1) + 1);
}

static void
get_ranges(size_t count, uint8_t max_addr_bits, struct cte *out)
{
    size_t gencount = 0;
    size_t sizebits;
    size_t size;
    genvaddr_t max_addr = 1ULL<<max_addr_bits;
    while (gencount < count) {
        sizebits = randrange(1,max_addr_bits-2);
        size = 1ULL<<sizebits;
        genvaddr_t begin = randrange(max_addr/10, max_addr-max_addr/4);
        genvaddr_t end = begin + size;
        if (end > max_addr) {
            continue;
        }
        bool valid = true;
        for (int j = 0; j < gencount; j++) {
            genvaddr_t r_addr = get_address(&(out[j].cap));
            size_t r_size = get_size(&(out[j].cap));
            genvaddr_t r_end = r_addr + r_size;
            if (begin < r_addr && end > r_addr && end < r_end) {
                valid = false;
                break;
            }
            if (begin > r_addr && begin < r_end && end > r_end) {
                valid = false;
                break;
            }
        }
        if (valid) {
            memset(&out[gencount], 0, sizeof(struct cte));
            out[gencount].cap.type = ObjType_RAM;
            out[gencount].cap.rights = CAPRIGHTS_ALLRIGHTS;
            out[gencount].cap.u.ram = (struct RAM) { .base = begin, .bits = sizebits };
            gencount++;
        }
    }
}

enum query_type {
    NONE,
    CONTAINS,
    INNER,
    EXC,
};

struct query {
    genvaddr_t begin;
    size_t size;
    struct cte *target;
};

static inline size_t min_count(size_t *counts, size_t countcount)
{
    size_t min = (size_t)-1;
    for (int i = 0; i < countcount; i++) {
        if (counts[i] < min)
            min = counts[i];
    }
    return min;
}

static void
get_overlap_queries(struct cte *ranges, size_t range_count, size_t count, genvaddr_t max_addr, struct query *out)
{
    size_t gencount[4] = { 0 };

    DEBUG_ALL_THE_THINGS("get_overlap_queries() count = %zd, gencount = [ %zd %zd %zd %zd ]\n",
            count, gencount[0], gencount[1], gencount[2], gencount[3]);

    while (min_count(gencount, 4) < count) {
        genvaddr_t begin = randrange(0, max_addr);
        genvaddr_t end = randrange(0, max_addr);
        if (begin == end) {
            continue;
        }
        if (begin > end) {
            begin ^= end;
            end ^= begin;
            begin ^= end;
        }
        enum query_type res = NONE;
        struct cte *target = NULL;
        for (int i = 0; i < range_count; i++) {
            struct cte *r = &ranges[i];
            genvaddr_t r_begin = get_address(&r->cap);
            size_t r_size = get_size(&r->cap);
            genvaddr_t r_end = r_begin + r_size;
            DEBUG_ALL_THE_THINGS("beg = 0x%"PRIxGENVADDR", end = 0x%"PRIxGENVADDR", r_beg = 0x%"PRIxGENVADDR", r_end = 0x%"PRIxGENVADDR"\n", begin, end, r_begin, r_end);
            if (r_end <= begin) {
                DEBUG_ALL_THE_THINGS("r_end <= beg\n");
                // do nothing
            }
            else if (r_begin <= begin && r_end >= end) {
                DEBUG_ALL_THE_THINGS("CONTAINS\n");
                if (res < CONTAINS) {
                    res = CONTAINS;
                    target = r;
                }
                else if (res == CONTAINS && compare_caps(&target->cap, &r->cap, true) < 0) {
                    target = r;
                }
            }
            else if (r_begin >= end) {
                DEBUG_ALL_THE_THINGS("r_beg >= end\n");
                // do nothing
            }
            else if ((r_begin >= begin && r_end < end) ||
                     (r_begin > begin && r_end <= end))
            {
                DEBUG_ALL_THE_THINGS("INNER\n");
                if (res < INNER) {
                    res = INNER;
                    target = r;
                }
                else if (res == INNER && compare_caps(&target->cap, &r->cap, true) > 0) {
                    target = r;
                }
            }
            else if ((r_begin < begin && r_end > begin && r_end < end) ||
                     (r_begin > begin && r_begin < end && r_end > end))
            {
                DEBUG_ALL_THE_THINGS("EXC\n");
                if (res < EXC) {
                    res = EXC;
                    target = r;
                }
                else if (res == EXC) {
                    if (r_begin < begin && target->cap.u.ram.base > begin) {
                        res = EXC;
                        target = r;
                    }
                    else if ((target->cap.u.ram.base < begin) == (r_begin < begin)) {
                        if (compare_caps(&r->cap, &target->cap, true) > 0) {
                            res = EXC;
                            target = r;
                        }
                    }
                }
            }
            else {
                printf("strange query: %zd - %zd (%zd - %zd)\n", begin, end, r_begin, r_end);
                USER_PANIC("huh?\n");
            }
        }
        if (gencount[res] < count) {
            size_t index_ = res * count + gencount[res]++;
            out[index_] = (struct query) { .begin = begin, .size = end - begin, .target = target };
        }
    }
}

__attribute__((unused))
static void dump_ranges(struct cte *ranges, size_t count)
{
    for (int i = 0; i < count; i++) {
        printf("address = %"PRIxGENVADDR"\nsize=%d\n",
                ranges[i].cap.u.ram.base, ranges[i].cap.u.ram.bits);
    }
}

extern struct cte *mdb_root;
int main(int argc, char *argv[])
{
    for (int run = 0; run < RUNS; run++) {
        putchar('-'); fflush(stdout);
        size_t count = randrange(MIN_RANGES, MAX_RANGES);
        struct cte ranges[count];
        get_ranges(count, MAX_ADDR_BITS, ranges);
        //dump_ranges(ranges, count);
        set_init_mapping(ranges, count);
        //mdb_dump_all_the_things();
        struct query queries[4][QUERY_COUNT];
        DEBUG_ALL_THE_THINGS("generating overlap queries\n");
        get_overlap_queries(ranges, count, QUERY_COUNT, MAX_ADDR, &queries[0][0]);
        DEBUG_ALL_THE_THINGS("finished generating overlap queries\n");
        for (int i = 0; i < 4*QUERY_COUNT; i++) {
            int qtype = i / QUERY_COUNT;
            int qindex = i % QUERY_COUNT;
            struct query *q = &queries[qtype][qindex];
            int result;
            struct cte *retcap;
            errval_t r = mdb_find_range(get_type_root(ObjType_RAM), q->begin, q->size, MDB_RANGE_FOUND_PARTIAL, &retcap, &result);
            if (err_is_fail(r)) {
                printf("begin = 0x%"PRIxGENVADDR", size = %zd, type = %d\n", q->begin, q->size, qtype);
                USER_PANIC("mdb_find_range returned with error %d\n", r);
            }
            if (result != qtype) {
                printf("Query: address = 0x%"PRIxGENVADDR", size = %zu\n", q->begin, q->size);
                if (retcap) {
                    printf("Result: address = 0x%"PRIxGENVADDR", size = %"PRIuGENSIZE"\n",
                            get_address(&retcap->cap),
                            get_size(&retcap->cap));
                }
                USER_PANIC("mdb_find_range returned %d (expected %d)\n", result, qtype);
            }
            if (retcap != q->target) {
                printf("Query: address = 0x%"PRIxGENVADDR", size = %zu\n", q->begin, q->size);
                USER_PANIC("mdb_find_range returned cap (.base = 0x%"
                        PRIxGENVADDR", .bits = %"PRIu8") (expected (.base = 0x%"
                        PRIxGENVADDR", .bits = %"PRIu8"))\n",
                        retcap->cap.u.ram.base, retcap->cap.u.ram.bits,
                        q->target->cap.u.ram.base, q->target->cap.u.ram.bits);
            }
        }
        // empty tree
        memset(ranges, 0, count * sizeof(struct cte));
        mdb_root = NULL;
    }
    return 0;
}
