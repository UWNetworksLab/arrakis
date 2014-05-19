#include <mdb/mdb_tree.h>
#include <mdb/mdb.h>
#include <cap_predicates.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/capbits.h>
#include <capabilities.h>
#include <assert.h>
#include <stdio.h>
#include "old_mdb.h"

#ifdef N
#undef N
#endif
#define N(cte) (&(cte)->mdbnode)
#ifdef C
#undef C
#endif
#define C(cte) (&(cte)->cap)

struct cte *old_start;

void old_mdb_insert(struct cte *cte)
{
    if (!old_start) {
        old_start = cte;
        N(cte)->left = cte;
        N(cte)->right = cte;
        return;
    }

    struct cte *curr = old_start;
    while (true) {
        int cmp = compare_caps(C(curr), C(cte), true);
        assert(cmp != 0); // cte should not be in mdb yet
        if (cmp >= 0) {
            break;
        }
        curr = N(curr)->right;
        if (curr == old_start) {
            break;
        }
    }

    N(cte)->left = N(curr)->left;
    N(cte)->right = curr;

    N(N(cte)->left)->right = cte;
    N(N(cte)->right)->left = cte;
}

void old_mdb_remove(struct cte *cte)
{
    if (N(cte)->right == cte) {
        assert(cte == old_start);
        old_start = N(cte)->right = N(cte)->left = NULL;
        return;
    }
    if (cte == old_start) {
        old_start = N(cte)->right;
    }

    N(N(cte)->right)->left = N(cte)->left;
    N(N(cte)->left)->right = N(cte)->right;

    N(cte)->right = N(cte)->left = NULL;
}

struct cte* old_mdb_predecessor(struct cte *cte)
{
    return N(cte)->left;
}

struct cte* old_mdb_successor(struct cte *cte)
{
    return N(cte)->right;
}

bool old_mdb_has_copies(struct cte *cte)
{
    return (N(cte)->left != cte)
        && (is_copy(C(N(cte)->left), C(cte))
            || is_copy(C(N(cte)->right), C(cte)));
}

bool old_mdb_has_descendants(struct cte *cte)
{
    for (struct cte *curr = N(cte)->right;
         curr != cte;
         curr = N(curr)->right)
    {
        if (!is_copy(C(cte), C(curr))) {
            return is_ancestor(C(curr), C(cte));
        }
    }
    return false;
}

bool old_mdb_has_ancestors(struct cte *cte)
{
    for (struct cte *curr = N(cte)->left;
         curr != cte;
         curr = N(curr)->left)
    {
        int cmp = compare_caps(C(curr), C(cte), true);
        if (cmp >= 0) {
            return false;
        }
        if (is_ancestor(C(cte), C(curr))) {
            return true;
        }
    }
    return false;
}
