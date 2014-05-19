#ifndef MDB_BENCH_H
#define MDB_BENCH_H

#ifdef OLD_MDB
#include "old_mdb.h"
#define RESET_ROOT() do { old_start = NULL; } while(0)
#define PRED(c) old_mdb_predecessor(c)
#define NEXT(c) old_mdb_successor(c)
#define INS(c) old_mdb_insert(c)
#define REM(c) old_mdb_remove(c)
#define HASCOP(c) old_mdb_has_copies(c)
#define HASANC(c) old_mdb_has_ancestors(c)
#define HASDESC(c) old_mdb_has_descendants(c)
#else
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
extern struct cte *mdb_root;
#define RESET_ROOT() do { mdb_root = NULL; } while(0)
#define PRED(c) mdb_predecessor(c)
#define NEXT(c) mdb_successor(c)
#define INS(c) mdb_insert(c)
#define REM(c) mdb_remove(c)
#define HASCOP(c) has_copies(c)
#define HASANC(c) has_ancestors(c)
#define HASDESC(c) has_descendants(c)
#endif

#define assert_err(err, msg) do { \
    if (err_is_fail(err)) \
        USER_PANIC_ERR(err, msg); \
} while (0)

typedef void (*reset_fn)(char*,size_t);
typedef cycles_t (*measure_fn)(struct cte*,size_t);

struct reset_opt { char *name; reset_fn fn; };
struct measure_opt { char *name; measure_fn fn; };

extern struct reset_opt reset_opts[];
extern struct measure_opt measure_opts[];

#endif
