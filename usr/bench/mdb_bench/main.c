#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/types.h>
#include <barrelfish/cap_predicates.h>
#include <bench/bench.h>

#include "mdb_bench.h"

static void test(char *base, size_t size, size_t runs, reset_fn reset, measure_fn measure, const char *name)
{
    assert(size % sizeof(struct cte) == 0);
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size/sizeof(struct cte);

    int run, skipped;
    for (run = skipped = 0;
         run < runs && (skipped < runs/10 || skipped < run*2);
         run++)
    {
        // reset MDB
        reset(base, size);

        // perform measurement
        cycles_t val = measure(ctes, num_caps);

        if (!val) {
            // measurement skipped
            printf("%s: skipping\n", name);
            skipped++;
            run--;
            continue;
        }

        // output
        printf("%s: %"PRIu64"/%lu\n", name, val - bench_tscoverhead(), num_caps);
    }

    if (run < runs) {
        printf("%s: skipped too many, aborting\n", name);
    }
}

static void
reset_and_dump(char *base, size_t size, size_t runs, reset_fn reset, const char *name)
{
    assert(size % sizeof(struct cte) == 0);
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size/sizeof(struct cte);

    for (size_t run = 0; run < runs; run++) {
        // reset MDB
        reset(base, size);

        for (size_t i = 0; i < num_caps; i++) {
            INS(&ctes[i]);
        }

        // dump entries
        for (size_t i = 0; i < num_caps; i++) {
            struct cte *cte = &ctes[i];
            struct capability *curr = &cte->cap;
            assert(curr->type == ObjType_RAM);
            printf("%s/%zu:dump:%zu: 0x%08"PRIxGENPADDR"/%"PRIu8" %c%c%c\n",
                   name, num_caps, run, curr->u.ram.base, curr->u.ram.bits,
                   (HASCOP(cte) ? 'c' : '.'), (HASANC(cte) ? 'a' : '.'),
                   (HASDESC(cte) ? 'd' : '.'));
        }
    }
}

static void usage(const char *program)
{
    printf("usage: %s [runs=NUM]"
           " [size=NUM] [count=NUM]"
           " [logsize=NUM] [logcount=NUM]"
           " [reset=NAME] [measure=NAME]\n\n", program);
    printf("\truns defaults to 100\n");
    printf("\tlogsize defaults to 20\n");
    printf("\tcount = size/sizeof(struct cte)\n\n");
    printf("resetters:\n");
    for (int i = 0; reset_opts[i].name; i++) {
        printf("\t%s\n", reset_opts[i].name);
    }
    printf("\n");
    printf("measures:\n");
    for (int i = 0; measure_opts[i].name; i++) {
        printf("\t%s\n", measure_opts[i].name);
    }
    printf("\tdump\n");
    printf("\n");
}

int main(int argc, char* argv[])
{

    size_t size_wanted = 1<<20;
    size_t runs = 100;
    struct reset_opt *reset = NULL;
    struct measure_opt *measure = NULL;
    bool dump = false;

    assert(argc>0);
    if (argc == 1) {
        usage(argv[0]);
        return 0;
    }

    bool args_ok = true;

    for (int arg = 1; arg < argc; arg++) {
        if (strcmp(argv[arg], "help") == 0
            || strcmp(argv[arg], "--help") == 0
            || strcmp(argv[arg], "-h") == 0)
        {
            usage(argv[0]);
            return 0;
        }
        if (strncmp(argv[arg], "size=", 5) == 0) {
            size_wanted = atol(argv[arg]+5);
        }
        if (strncmp(argv[arg], "logsize=", 8) == 0) {
            size_t logsize = atol(argv[arg]+8);
            if (logsize > 31) {
                printf("ERROR: logsize too big\n");
                args_ok = false;
            }
            else {
                size_wanted = 1 << logsize;
            }
        }
        else if (strncmp(argv[arg], "count=", 6) == 0) {
            size_wanted = atol(argv[arg]+6)*sizeof(struct cte);
        }
        else if (strncmp(argv[arg], "logcount=", 9) == 0) {
            size_t logcount = atol(argv[arg]+9);
            if (logcount > (31-OBJBITS_CTE)) {
                printf("ERROR: logcount too big\n");
                args_ok = false;
            }
            else {
                size_wanted = (1 << logcount)*sizeof(struct cte);
            }
        }
        else if (strncmp(argv[arg], "runs=", 5) == 0) {
            runs = atol(argv[arg]+5);
        }
        else if (strncmp(argv[arg], "reset=", 6) == 0) {
            char *name = argv[arg]+6;
            int i;
            for (i = 0; reset_opts[i].name; i++) {
                if (strcmp(reset_opts[i].name, name) == 0) {
                    reset = &reset_opts[i];
                    break;
                }
            }
            if (!reset_opts[i].name) {
                args_ok = false;
                printf("ERROR: unkown reset \"%s\"\n", name);
            }
        }
        else if (strncmp(argv[arg], "measure=", 8) == 0) {
            char *name = argv[arg]+8;
            if (strcmp(name, "dump") == 0) {
                measure = NULL;
                dump = true;
            }
            else {
                int i;
                for (i = 0; measure_opts[i].name; i++) {
                    if (strcmp(measure_opts[i].name, name) == 0) {
                        measure = &measure_opts[i];
                        break;
                    }
                }

                if (measure_opts[i].name) {
                    dump = false;
                }
                else {
                    args_ok = false;
                    printf("ERROR: unkown measure \"%s\"\n", name);
                }
            }
        }
        else {
            args_ok = false;
            printf("ERROR: unkown argument %s\n", argv[arg]);
        }
    }
    if (!args_ok) {
        usage(argv[0]);
        return 1;
    }

    assert(size_wanted > 0);
    assert(runs > 0);
    assert(reset);
    assert(measure || dump);

    errval_t err;
    struct capref frame;
    size_t size;
    err = frame_alloc(&frame, size_wanted, &size);
    assert_err(err, "alloc");
    assert(size >= size_wanted);
    printf("got %lu bytes\n", size);

    struct memobj *m;
    struct vregion *v;
    void *addr;

    err = vspace_map_one_frame(&addr, size, frame, &m, &v);
    assert_err(err, "map");

    if (dump) {
        reset_and_dump(addr, size_wanted, runs, reset->fn, reset->name);
    }
    else {
        bench_init();

        char *bench_name = malloc(strlen(reset->name)+strlen(measure->name)+2);
        strcpy(bench_name, reset->name);
        strcat(bench_name, ":");
        strcat(bench_name, measure->name);
        test(addr, size_wanted, runs, reset->fn, measure->fn, bench_name);

        free(bench_name);
    }

    printf("client done\n");

    vregion_destroy(v);
    cap_destroy(frame);

    return 0;
}
