#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/types.h>
#include <barrelfish/cap_predicates.h>
#include <bench/bench.h>

#include "mdb_bench.h"

static void clear_mdb(char *base, size_t size)
{
    RESET_ROOT();
    memset(base, 0, size);
}

static void reset_mdb_seq_1b_ram(char *base, size_t size)
{
    clear_mdb(base, size);

    // generate 1-byte ctes corresponding to index
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size / sizeof(struct cte);
    for (int i = 0; i < num_caps; i++) {
        struct RAM ram = {
            .base = i,
            .bits = 0,
        };
        struct capability cap = {
            .type = ObjType_RAM,
            .rights = CAPRIGHTS_ALLRIGHTS,
            .u.ram = ram,
        };
        ctes[i].cap = cap;
    }
}

static void reset_mdb_random_nat_ram(char *base, size_t size)
{
    clear_mdb(base, size);

    // generate naturally-aligned RAM caps
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size / sizeof(struct cte);
    for (int i = 0; i < num_caps; i++) {
        int bits = rand() % 16;
        struct RAM ram = {
            .base = ((uint32_t)rand())<<bits,
            .bits = bits,
        };
        struct capability cap = {
            .type = ObjType_RAM,
            .rights = CAPRIGHTS_ALLRIGHTS,
            .u.ram = ram,
        };
        ctes[i].cap = cap;
    }
}

static void reset_mdb_propszrand_nat_ram(char *base, size_t size)
{
    clear_mdb(base, size);

    // generate naturally-aligned RAM caps
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size / sizeof(struct cte);

    size_t size_bits = 0;
    while ((1<<size_bits) < size) { size_bits++; }
    size_bits += 2;

    for (int i = 0; i < num_caps; i++) {
        genpaddr_t capbase = rand() % (1<<(size_bits+2));
        int capbits = 0;
        while (capbase & (1<<(size_bits+1))) { capbase <<= 1; capbits++; }

        struct RAM ram = {
            .base = capbase & ((1<<size_bits)-1),
            .bits = capbits,
        };
        struct capability cap = {
            .type = ObjType_RAM,
            .rights = CAPRIGHTS_ALLRIGHTS,
            .u.ram = ram,
        };
        ctes[i].cap = cap;
    }
}

static void reset_mdb_szprob_cp_nat_ram(char *base, size_t size)
{
    clear_mdb(base, size);

    // generate naturally-aligned RAM caps
    struct cte *ctes = (struct cte*)base;
    size_t num_caps = size / sizeof(struct cte);

    size_t size_bits = 0;
    while ((1<<size_bits) < size) { size_bits++; }
    size_bits += 2;

    for (int i = 0; i < num_caps; i++) {
        if (i > 1 && (rand() % 10) == 0) {
            // force a copy

            // randomly select source cap, not assuming i is power-of-two
            size_t pos, mod = 1;
            while (mod < i) { mod <<= 1; }
            do {
                pos = rand() % mod;
            } while (pos >= i);
            struct cte *cte = &ctes[pos];

            // make cpoy
            ctes[i].cap = cte->cap;
        }
        else {
            // generate caps with P[capbits=x]=(1<<(max_bits-x-1))/(1<<max_bits)

            genpaddr_t capbase = rand() % (1<<(size_bits+2));
            int capbits = 0;
            while (capbase & (1<<(size_bits+1))) { capbase <<= 1; capbits++; }

            struct RAM ram = {
                .base = capbase & ((1<<size_bits)-1),
                .bits = capbits,
            };
            struct capability cap = {
                .type = ObjType_RAM,
                .rights = CAPRIGHTS_ALLRIGHTS,
                .u.ram = ram,
            };
            ctes[i].cap = cap;
        }
    }
}

struct reset_opt reset_opts[] = {
    { "random_nat_ram", reset_mdb_random_nat_ram, },
    { "propszrand_nat_ram", reset_mdb_propszrand_nat_ram, },
    { "seq_1b_ram", reset_mdb_seq_1b_ram, },
    { "szprob_cp_nat_ram", reset_mdb_szprob_cp_nat_ram, },
    { NULL, NULL, },
};
