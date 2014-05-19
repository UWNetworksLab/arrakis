#include <mdb/mdb_tree.h>
#include <mdb/mdb.h>
#include <cap_predicates.h>
#include <barrelfish_kpi/capabilities.h>
#include <capabilities.h>
#include <assert.h>
#include <stdio.h>

#ifndef MIN
#define MIN(a, b) ((a)<(b)?(a):(b))
#endif
#ifndef MAX
#define MAX(a, b) ((a)>(b)?(a):(b))
#endif

#ifdef N
#undef N
#endif
#define N(cte) (&(cte)->mdbnode)
#ifdef C
#undef C
#endif
#define C(cte) (&(cte)->cap)

// PP switch to change behaviour if invariants fail
#ifdef MDB_FAIL_INVARIANTS
// on failure, dump mdb and terminate
static void
mdb_dump_and_fail(struct cte *cte, int failure)
{
    mdb_dump(cte, 0);
    printf("failed on cte %p with failure %d\n", cte, failure);
    // XXX: what is "proper" way to always terminate?
    assert(false);
}
#define MDB_RET_INVARIANT(cte, failure) mdb_dump_and_fail(cte, failure)
#else
#define MDB_RET_INVARIANT(cte, failure) return failure
#endif

// PP switch to toggle checking of invariants by default
#ifdef MDB_RECHECK_INVARIANTS
#define CHECK_INVARIANTS(cte) mdb_check_subtree_invariants(cte)
#else
#define CHECK_INVARIANTS(cte) ((void)0)
#endif

// printf tracing and entry/exit invariant checking
#ifdef MDB_TRACE
#define MDB_TRACE_ENTER(valid_cte, args_fmt, ...) do { \
    printf("enter %s(" args_fmt ")\n", __func__, __VA_ARGS__); \
    CHECK_INVARIANTS(valid_cte); \
} while (0)
#define MDB_TRACE_LEAVE_SUB(valid_cte) do { \
    CHECK_INVARIANTS(valid_cte); \
    printf("leave %s\n", __func__); \
    return; \
} while (0)
#define MDB_TRACE_LEAVE_SUB_RET(ret_fmt, ret, valid_cte) do { \
    CHECK_INVARIANTS(valid_cte); \
    printf("leave %s->" ret_fmt "\n", __func__, (ret)); \
    return (ret); \
} while (0)
#else
#define MDB_TRACE_ENTER(valid_cte, args_fmt, ...) CHECK_INVARIANTS(valid_cte)
#define MDB_TRACE_LEAVE_SUB(valid_cte) do { \
    CHECK_INVARIANTS(valid_cte); \
    return; \
} while (0)
#define MDB_TRACE_LEAVE_SUB_RET(ret_fmt, ret, valid_cte) do { \
    CHECK_INVARIANTS(valid_cte); \
    return (ret); \
} while (0)
#endif

struct cte *mdb_root = NULL;

/*
 * Debug printing.
 */

void
mdb_dump_all_the_things(void)
{
    mdb_dump(mdb_root, 0);
}

void
mdb_dump(struct cte *cte, int indent)
{
    // Print a tree with root on the left, the smallest element at the top and the
    // largest at the bottom.

    /* make an indent buffer */
    char indent_buff[indent+2];
    for (int i=0; i < indent+1; i++) {
        indent_buff[i]='\t';
    }
    indent_buff[indent+1] = '\0';

    if (!cte) {
        printf("NULL{}\n");
        return;
    }

    struct mdbnode *node = N(cte);

    if (node->left) {
        if (node->left == cte) {
            printf("%sSELF!!!!\n", indent_buff);
        }
        else {
            mdb_dump(node->left, indent+1);
        }
    }

    printf("%s%p{left=%p,right=%p,end=0x%08"PRIxGENPADDR",end_root=%"PRIu8","
            "level=%"PRIu8",address=0x%08"PRIxGENPADDR",size=0x%08"PRIx64","
            "type=%"PRIu8",remote_rels=%d}\n",
            indent_buff,
            cte, node->left, node->right, node->end, node->end_root,
            node->level, get_address(C(cte)), get_size(C(cte)),
            (uint8_t)C(cte)->type, N(cte)->remote_relations);

    if (node->right) {
        if (node->right == cte) {
            printf("%sSELF!!!!\n", indent_buff);
        }
        else {
            mdb_dump(node->right, indent+1);
        }
    }
}


/*
 * Invariant checking.
 */

static int
mdb_check_subtree_invariants(struct cte *cte)
{
    if (!cte) {
        return MDB_INVARIANT_OK;
    }
    assert(C(cte)->type != 0);

    int err;
    struct mdbnode *node = N(cte);

    if (node->level > 0 && !(node->left && node->right)) {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_BOTHCHILDREN);
    }
    if (node->left && !(N(node->left)->level < node->level)) {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_LEFT_LEVEL_LESS);
    }
    if (node->right && !(N(node->right)->level <= node->level)) {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_RIGHT_LEVEL_LEQ);
    }
    if (node->right && N(node->right)->right &&
        !(N(N(node->right)->right)->level < node->level))
    {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS);
    }
    if (node->right && N(node->right)->left &&
        !(N(N(node->right)->left)->level < node->level))
    {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS);
    }

    // build expected end root for current node
    mdb_root_t expected_end_root = get_type_root(C(cte)->type);
    if (node->left) {
        expected_end_root = MAX(expected_end_root, N(node->left)->end_root);
    }
    if (node->right) {
        expected_end_root = MAX(expected_end_root, N(node->right)->end_root);
    }
    if (node->end_root != expected_end_root) {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_END_IS_MAX);
    }

    // build expected end for current node. this is complex because the root
    // acts as an address prefix, so only ends where the corresponding root is
    // the maximum may be considered.
    genpaddr_t expected_end = 0;
    if (get_type_root(C(cte)->type) == node->end_root) {
        // only consider current cte's end if its root is node->end_root
        expected_end = get_address(C(cte))+get_size(C(cte));
    }
    if (node->left && N(node->left)->end_root == node->end_root) {
        // only consider left child end if its end_root is node->end_root
        expected_end = MAX(expected_end, N(node->left)->end);
    }
    if (node->right && N(node->right)->end_root == node->end_root) {
        // only consider right child end if its end_root is node->end_root
        expected_end = MAX(expected_end, N(node->right)->end);
    }
    if (node->end != expected_end) {
        MDB_RET_INVARIANT(cte, MDB_INVARIANT_END_IS_MAX);
    }

    if (node->left) {
        assert(node->left != cte);
        if (compare_caps(C(node->left), C(cte), true) >= 0) {
            MDB_RET_INVARIANT(cte, MDB_INVARIANT_LEFT_SMALLER);
        }
        err = mdb_check_subtree_invariants(node->left);
        if (err) {
            return err;
        }
    }

    if (node->right) {
        assert(node->right != cte);
        if (compare_caps(C(node->right), C(cte), true) <= 0) {
            MDB_RET_INVARIANT(cte, MDB_INVARIANT_RIGHT_GREATER);
        }
        err = mdb_check_subtree_invariants(node->right);
        if (err) {
            return err;
        }
    }

    return MDB_INVARIANT_OK;
}

int
mdb_check_invariants(void)
{
    int res = mdb_check_subtree_invariants(mdb_root);
    if (res != 0) {
        printf("mdb_check_invariants() -> %d\n", res);
    }
    return res;
}

/*
 * General internal helpers.
 */

static void
mdb_update_end(struct cte *cte)
{
    if (!cte) {
        return;
    }
    struct mdbnode *node = N(cte);

    // build end root for current node
    mdb_root_t end_root = get_type_root(C(cte)->type);
    if (node->left) {
        end_root = MAX(end_root, N(node->left)->end_root);
    }
    if (node->right) {
        end_root = MAX(end_root, N(node->right)->end_root);
    }
    node->end_root = end_root;

    // build end address for current node. this is complex because the root
    // acts as an address prefix, so only ends where the corresponding root is
    // the maximum may be considered.
    genpaddr_t end = 0;
    if (get_type_root(C(cte)->type) == node->end_root) {
        // only consider current cte's end if its root is node->end_root
        end = get_address(C(cte))+get_size(C(cte));
    }
    if (node->left && N(node->left)->end_root == node->end_root) {
        // only consider left child end if its end_root is node->end_root
        end = MAX(end, N(node->left)->end);
    }
    if (node->right && N(node->right)->end_root == node->end_root) {
        // only consider right child end if its end_root is node->end_root
        end = MAX(end, N(node->right)->end);
    }
    node->end = end;
}

static struct cte*
mdb_skew(struct cte *node)
{
    /* transform invalid state
     *
     *               |
     *      |L|<---|T|
     *     /   \      \
     *   |A|   |B|    |R|
     *
     * to valid equivalent state
     *
     *       |
     *      |L|--->|T|
     *     /      /   \
     *   |A|    |B|   |R|
     *
     */
    if (!node || !N(node)->left) {
        return node;
    }
    else if (N(node)->level == N(N(node)->left)->level) {
        struct cte *left = N(node)->left;
        N(node)->left = N(left)->right;
        N(left)->right = node;
        mdb_update_end(node);
        mdb_update_end(left);
        // need to update mdb_root
        if (mdb_root == node) {
            mdb_root = left;
        }
        return left;
    }
    else {
        return node;
    }
}

static struct cte*
mdb_split(struct cte *node)
{
    /* transform invalid state
     *
     *       |
     *      |T|--->|R|-->|X|
     *     /      /
     *   |A|    |B|
     *
     * to valid equivalent state
     *
     *             |
     *            |R|
     *           /   \
     *         |T|   |X|
     *        /   \
     *      |A|   |B|
     *
     */
    if (!node || !N(node)->right || !N(N(node)->right)->right) {
        return node;
    }
    else if (N(node)->level == N(N(N(node)->right)->right)->level) {
        struct cte *right = N(node)->right;
        N(node)->right = N(right)->left;
        N(right)->left = node;
        N(right)->level += 1;
        mdb_update_end(node);
        mdb_update_end(right);
        // need to update mdb_root
        if (mdb_root == node) {
            mdb_root = right;
        }
        return right;
    }
    else {
        return node;
    }
}

static void
mdb_decrease_level(struct cte *node)
{
    assert(node);

    mdb_level_t expected;
    if (!N(node)->left || !N(node)->right) {
        expected = 0;
    }
    else {
        expected = MIN(N(N(node)->left)->level, N(N(node)->right)->level) + 1;
    }

    if (expected < N(node)->level) {
        N(node)->level = expected;
        if (N(node)->right && expected < N(N(node)->right)->level) {
            N(N(node)->right)->level = expected;
        }
    }
}

static struct cte*
mdb_rebalance(struct cte *node)
{
    assert(node);
    mdb_update_end(node);
    mdb_decrease_level(node);
    node = mdb_skew(node);
    N(node)->right = mdb_skew(N(node)->right);
    if (N(node)->right) {
        N(N(node)->right)->right = mdb_skew(N(N(node)->right)->right);
    }
    node = mdb_split(node);
    N(node)->right = mdb_split(N(node)->right);
    return node;
}

static bool
mdb_is_child(struct cte *child, struct cte *parent)
{
    if (!parent) {
        return mdb_root == child;
    }
    else {
        return N(parent)->left == child || N(parent)->right == child;
    }
}

static bool
mdb_is_inside(genpaddr_t outer_begin, genpaddr_t outer_end,
              genpaddr_t inner_begin, genpaddr_t inner_end)
{
    return
        (inner_begin >= outer_begin && inner_end < outer_end) ||
        (inner_begin > outer_begin && inner_end <= outer_end);
}

/*
 * Operations and operation-specific helpers.
 */

static errval_t
mdb_sub_insert(struct cte *new_node, struct cte **current)
{
    errval_t err;
    assert(new_node);
    assert(current);
    MDB_TRACE_ENTER(*current, "%p, %p (*%p)", new_node, *current, current);

    struct cte *current_ = *current;

    if (!current_) {
        // we've reached an empty leaf, insert here
        *current = new_node;
        mdb_update_end(new_node);
        return SYS_ERR_OK;
    }

    int compare = compare_caps(C(new_node), C(current_), true);
    if (compare < 0) {
        // new_node < current
        err = mdb_sub_insert(new_node, &N(current_)->left);
        if (err_is_fail(err)) {
            return err;
        }
    }
    else if (compare > 0) {
        // new_node > current
        err = mdb_sub_insert(new_node, &N(current_)->right);
        if (err_is_fail(err)) {
            return err;
        }
    }
    else {
        return CAPS_ERR_MDB_DUPLICATE_ENTRY;
    }

    mdb_update_end(current_);
    current_ = mdb_skew(current_);
    current_ = mdb_split(current_);
    *current = current_;

    err = SYS_ERR_OK;
    MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, current_);
}

errval_t
mdb_insert(struct cte *new_node)
{
    MDB_TRACE_ENTER(mdb_root, "%p", new_node);
    errval_t ret = mdb_sub_insert(new_node, &mdb_root);
    MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, ret, mdb_root);
}

static void
mdb_exchange_child(struct cte *first, struct cte *first_parent,
                   struct cte *second)
{
    assert(mdb_is_child(first, first_parent));

    if (!first_parent) {
        mdb_root = second;
    }
    else if (N(first_parent)->left == first) {
        N(first_parent)->left = second;
    }
    else if (N(first_parent)->right == first) {
        N(first_parent)->right = second;
    }
    else {
        assert(!"first is not child of first_parent");
    }
}

static void
mdb_exchange_nodes(struct cte *first, struct cte *first_parent,
                   struct cte *second, struct cte *second_parent)
{
    struct cte *tmp_node;
    mdb_level_t tmp_level;

    mdb_exchange_child(first, first_parent, second);
    mdb_exchange_child(second, second_parent, first);

    tmp_node = N(first)->left;
    N(first)->left = N(second)->left;
    N(second)->left = tmp_node;

    tmp_node = N(first)->right;
    N(first)->right = N(second)->right;
    N(second)->right = tmp_node;

    tmp_level = N(first)->level;
    N(first)->level = N(second)->level;
    N(second)->level = tmp_level;

    mdb_update_end(first);
    mdb_update_end(second);
}

static void
mdb_exchange_remove(struct cte *target, struct cte *target_parent,
                    struct cte **current, struct cte *parent,
                    int dir, struct cte **ret_target)
{
    assert(current);
    MDB_TRACE_ENTER(*current, "%p, %p, %p (*%p), %p, %d", target, target_parent, *current, current, parent, dir);
    assert(target);
    assert(*current);
    assert(parent);
    assert(C(target)->type != 0);
    assert(C(*current)->type != 0);
    assert(C(parent)->type != 0);
    assert(ret_target);
    assert(!*ret_target);
    assert(dir != 0);
    assert(compare_caps(C(target), C(*current), true) != 0);
    assert(mdb_is_child(target, target_parent));
    assert(mdb_is_child(*current, parent));

    struct cte *current_ = *current;

    if (dir > 0) {
        if (parent == target) {
            assert(N(parent)->left == current_);
        }
        else {
            assert(N(parent)->right == current_);
        }

        if (N(current_)->right) {
            mdb_exchange_remove(target, target_parent, &N(current_)->right,
                                current_, dir, ret_target);
        }
    }
    else if (dir < 0) {
        if (parent == target) {
            assert(N(parent)->right == current_);
        }
        else {
            assert(N(parent)->left == current_);
        }

        if (N(current_)->left) {
            mdb_exchange_remove(target, target_parent, &N(current_)->left,
                                current_, dir, ret_target);
        }
        else if (N(current_)->right) {
            // right is null, left non-null -> current is level 0 node with
            // horizontal right link, and is also the successor of the target.
            // in this case, exchange current and current right, then current
            // (at its new position) and the target.
            struct cte *new_current = N(current_)->right;
            mdb_exchange_nodes(current_, parent, N(current_)->right, current_);
            mdb_exchange_nodes(target, target_parent, current_, new_current);
            // "current" is now located where the target was, further up in the
            // tree. "new_current" is the node where current was. "target" is
            // where current->right was, and is a leaf, so can be dropped.
            assert(N(new_current)->right == target);
            N(new_current)->right = NULL;
            *ret_target = current_;
            *current = new_current;
            MDB_TRACE_LEAVE_SUB(NULL);
        }
    }

    if (*ret_target) {
        // implies we recursed further down to find a leaf. need to rebalance.
        current_ = mdb_rebalance(current_);
        *current = current_;
        MDB_TRACE_LEAVE_SUB(current_);
    }
    else {
        //printf("found leaf %p\n", current_);
        // found successor/predecessor leaf, exchange with target
        assert(!N(current_)->right && !N(current_)->left);
        mdb_exchange_nodes(target, target_parent, current_, parent);

        // "current" is now where target was, so set as ret_target
        *ret_target = current_;
        // target would be the new current, but we're removing it, so set
        // current to null. This also sets parent's corresponding child to
        // null by recursion.
        *current = NULL;
        MDB_TRACE_LEAVE_SUB(NULL);
    }
}

static errval_t
mdb_subtree_remove(struct cte *target, struct cte **current, struct cte *parent)
{
    assert(current);
    MDB_TRACE_ENTER(*current, "%p, %p (*%p), %p", target, *current, current, parent);

    errval_t err;
    struct cte *current_ = *current;
    if (!current_) {
        err = CAPS_ERR_MDB_ENTRY_NOTFOUND;
        MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, current_);
    }

    int compare = compare_caps(C(target), C(current_), true);
    if (compare > 0) {
        err = mdb_subtree_remove(target, &N(current_)->right, current_);
        if (err != SYS_ERR_OK) {
            MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, current_);
            return err;
        }
    }
    else if (compare < 0) {
        err = mdb_subtree_remove(target, &N(current_)->left, current_);
        if (err != SYS_ERR_OK) {
            MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, current_);
        }
    }
    else {
        assert(current_ == target);
        if (!N(current_)->left && !N(current_)->right) {
            // target is leaf, just remove
            *current = NULL;
            err = SYS_ERR_OK;
            MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, NULL);
        }
        else if (!N(current_)->left) {
            // move to right child then go left (dir=-1)
            struct cte *new_current = NULL;
            struct cte *new_right = N(current_)->right;
            mdb_exchange_remove(target, parent, &new_right, current_, -1,
                                &new_current);
            assert(new_current);
            current_ = new_current;
            N(current_)->right = new_right;
        }
        else {
            // move to left child then go right (dir=1)
            struct cte *new_current = NULL;
            struct cte *new_left = N(current_)->left;
            mdb_exchange_remove(target, parent, &new_left, current_, 1,
                                &new_current);
            assert(new_current);
            current_ = new_current;
            N(current_)->left = new_left;
        }
    }

    // rebalance after remove from subtree
    current_ = mdb_rebalance(current_);
    *current = current_;

    assert(C(target)->type != 0);
    assert(!*current || C(*current)->type != 0);

    err = SYS_ERR_OK;
    MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, current_);
}

errval_t
mdb_remove(struct cte *target)
{
    MDB_TRACE_ENTER(mdb_root, "%p", target);
    errval_t err = mdb_subtree_remove(target, &mdb_root, NULL);
    MDB_TRACE_LEAVE_SUB_RET("%"PRIuPTR, err, mdb_root);
}

/*
 * Queries on the ordering.
 */

static struct cte*
mdb_sub_find_equal(struct capability *cap, struct cte *current)
{
    if (!current) {
        return NULL;
    }
    int compare = compare_caps(cap, C(current), false);
    if (compare < 0) {
        // current is gt key, look for smaller node
        return mdb_sub_find_equal(cap, N(current)->left);
    }
    else if (compare > 0) {
        // current is lt key, attempt to find bigger current
        return mdb_sub_find_equal(cap, N(current)->right);
    }
    else {
        return current;
    }
}

struct cte*
mdb_find_equal(struct capability *cap)
{
    return mdb_sub_find_equal(cap, mdb_root);
}

static struct cte*
mdb_sub_find_less(struct capability *cap, struct cte *current, bool equal_ok,
                  bool tiebreak)
{
    if (!current) {
        return NULL;
    }
    int compare = compare_caps(cap, C(current), tiebreak);
    if (compare < 0) {
        // current is gt key, look for smaller node
        return mdb_sub_find_less(cap, N(current)->left, equal_ok, tiebreak);
    }
    else if (compare > 0) {
        // current is lt key, attempt to find bigger current
        struct cte *res = mdb_sub_find_less(cap, N(current)->right, equal_ok,
                                            tiebreak);
        if (res) {
            return res;
        }
        // bigger child exceeded key
        return current;
    }
    else {
        // found equal element
        if (equal_ok) {
            return current;
        }
        else {
            // look for smaller node
            return mdb_sub_find_less(cap, N(current)->left, equal_ok,
                                     tiebreak);
        }
    }
}

struct cte*
mdb_find_less(struct capability *cap, bool equal_ok)
{
    return mdb_sub_find_less(cap, mdb_root, equal_ok, false);
}

static struct cte*
mdb_sub_find_greater(struct capability *cap, struct cte *current,
                     bool equal_ok, bool tiebreak)
{
    if (!current) {
        return NULL;
    }
    int compare = compare_caps(cap, C(current), tiebreak);
    if (compare < 0) {
        // current is gt key, attempt to find smaller node
        struct cte *res = mdb_sub_find_greater(cap, N(current)->left, equal_ok,
                                               tiebreak);
        if (res) {
            return res;
        }
        // smaller was lte key
        return current;
    }
    else if (compare > 0) {
        // current is lte key, look for greater node
        return mdb_sub_find_greater(cap, N(current)->right, equal_ok,
                                    tiebreak);
    }
    else {
        // found equal element
        if (equal_ok) {
            return current;
        }
        else {
            // look for greater node
            return mdb_sub_find_greater(cap, N(current)->right, equal_ok,
                                        tiebreak);
        }
    }
}

struct cte*
mdb_find_greater(struct capability *cap, bool equal_ok)
{
    return mdb_sub_find_greater(cap, mdb_root, equal_ok, false);
}

struct cte*
mdb_predecessor(struct cte *current)
{
    struct mdbnode *node = N(current);
    if (node->left) {
        // if possible, look just at children
        current = node->left;
        while ((node = N(current))->right) {
            current = node->right;
        }
        return current;
    }
    // XXX: in lieu of a parent pointer that can be used to traverse upwards,
    // we have to perform a search through the tree from the root. This makes
    // "predecessor" into a O(log(n)) operation, instead of the expected O(1).
    return mdb_sub_find_less(C(current), mdb_root, false, true);
}

struct cte*
mdb_successor(struct cte *current)
{
    struct mdbnode *node = N(current);
    if (node->right) {
        // if possible, look just at children
        current = node->right;
        while ((node = N(current))->left) {
            current = node->left;
        }
        return current;
    }
    // XXX: in lieu of a parent pointer that can be used to traverse upwards,
    // we have perform a search through the tree from the root. This makes
    // "successor" into a O(log(n)) operation, instead of the expected O(1).
    return mdb_sub_find_greater(C(current), mdb_root, false, true);
}

/*
 * The range query.
 */

static struct cte*
mdb_choose_surrounding(genpaddr_t address, size_t size, struct cte *first,
                       struct cte *second)
{
    assert(first);
    assert(second);
    assert(get_type_root(C(first)->type) == get_type_root(C(second)->type));
#ifndef NDEBUG
    genpaddr_t beg = address, end = address + size;
    genpaddr_t fst_beg = get_address(C(first));
    genpaddr_t snd_beg = get_address(C(second));
    genpaddr_t fst_end = fst_beg + get_size(C(first));
    genpaddr_t snd_end = snd_beg + get_size(C(second));
    assert(fst_beg <= beg && fst_end >= end);
    assert(snd_beg <= beg && snd_end >= end);
#endif

    if (compare_caps(C(first), C(second), true) >= 0) {
        return first;
    }
    else {
        return second;
    }
}

static struct cte*
mdb_choose_inner(genpaddr_t address, size_t size, struct cte *first,
                 struct cte *second)
{
    assert(first);
    assert(second);
    assert(get_type_root(C(first)->type) == get_type_root(C(second)->type));
#ifndef NDEBUG
    genpaddr_t end = address + size;
    genpaddr_t fst_beg = get_address(C(first));
    genpaddr_t snd_beg = get_address(C(second));
    genpaddr_t fst_end = fst_beg + get_size(C(first));
    genpaddr_t snd_end = snd_beg + get_size(C(second));
    assert(mdb_is_inside(address, end, fst_beg, fst_end));
    assert(mdb_is_inside(address, end, snd_beg, snd_end));
#endif

    if (compare_caps(C(first), C(second), true) <= 0) {
        return first;
    }
    else {
        return second;
    }
}

static struct cte*
mdb_choose_partial(genpaddr_t address, size_t size, struct cte *first,
                   struct cte *second)
{
    assert(first);
    assert(second);
    assert(get_type_root(C(first)->type) == get_type_root(C(second)->type));
    genpaddr_t beg = address;
    genpaddr_t fst_beg = get_address(C(first));
    genpaddr_t snd_beg = get_address(C(second));
#ifndef NDEBUG
    genpaddr_t end = address + size;
    genpaddr_t fst_end = fst_beg + get_size(C(first));
    genpaddr_t snd_end = snd_beg + get_size(C(second));
    assert(fst_beg < end);
    assert(snd_beg < end);
    assert(fst_end > beg);
    assert(snd_end > beg);
    assert(fst_beg != beg);
    assert(snd_beg != beg);
    assert(fst_end != end);
    assert(snd_end != end);
    assert((fst_beg < beg) == (fst_end < end));
    assert((snd_beg < beg) == (snd_end < end));
#endif

    if (fst_beg < beg && snd_beg > beg) {
        return first;
    }
    else if (snd_beg < beg && fst_beg > beg) {
        return second;
    }
    else {
        if (compare_caps(C(first), C(second), true) >= 0) {
            return first;
        }
        else {
            return second;
        }
    }
}

static int
mdb_sub_find_range(mdb_root_t root, genpaddr_t address, size_t size,
                   int max_precision, struct cte *current,
                   /*out*/ struct cte **ret_node);

static void
mdb_sub_find_range_merge(mdb_root_t root, genpaddr_t address, size_t size,
                         int max_precision, struct cte *sub,
                         /*inout*/ int *ret, /*inout*/ struct cte **result)
{
    assert(sub);
    assert(ret);
    assert(result);
    assert(max_precision >= 0);
    assert(*ret <= max_precision);

    struct cte *sub_result = NULL;
    int sub_ret = mdb_sub_find_range(root, address, size, max_precision, sub,
                                     &sub_result);
    if (sub_ret > max_precision) {
        *result = NULL;
        *ret = sub_ret;
    }
    else if (sub_ret > *ret) {
        *result = sub_result;
        *ret = sub_ret;
    }
    else if (sub_ret == *ret) {
        switch (sub_ret) {
        case MDB_RANGE_NOT_FOUND:
            break;
        case MDB_RANGE_FOUND_SURROUNDING:
            *result = mdb_choose_surrounding(address, size, *result, sub_result);
            break;
        case MDB_RANGE_FOUND_INNER:
            *result = mdb_choose_inner(address, size, *result, sub_result);
            break;
        case MDB_RANGE_FOUND_PARTIAL:
            *result = mdb_choose_partial(address, size, *result, sub_result);
            break;
        default:
            assert(!"Unhandled enum value for mdb_find_range result");
            break;
        }
    }
    // else ret > sub_ret, keep ret & result as is
}

static int
mdb_sub_find_range(mdb_root_t root, genpaddr_t address, size_t size,
                   int max_precision, struct cte *current,
                   /*out*/ struct cte **ret_node)
{
    assert(max_precision >= 0);
    assert(ret_node);

    if (!current) {
        *ret_node = NULL;
        return MDB_RANGE_NOT_FOUND;
    }

    if (N(current)->end_root < root) {
        *ret_node = NULL;
        return MDB_RANGE_NOT_FOUND;
    }
    if (N(current)->end_root == root && N(current)->end <= address) {
        *ret_node = NULL;
        return MDB_RANGE_NOT_FOUND;
    }

    mdb_root_t current_root = get_type_root(C(current)->type);

    struct cte *result = NULL;
    int ret = MDB_RANGE_NOT_FOUND;

    genpaddr_t current_address = get_address(C(current));
    genpaddr_t current_end = current_address + get_size(C(current));
    genpaddr_t search_end = address + size;

    if (current_root == root) {

        if (ret < MDB_RANGE_FOUND_PARTIAL &&
            current_address > address &&
            current_address < search_end &&
            current_end > search_end)
        {
            result = current;
            ret = MDB_RANGE_FOUND_PARTIAL;
        }
        if (ret < MDB_RANGE_FOUND_PARTIAL &&
            current_end > address &&
            current_end < search_end &&
            current_address < address)
        {
            result = current;
            ret = MDB_RANGE_FOUND_PARTIAL;
        }
        if (ret < MDB_RANGE_FOUND_INNER &&
            mdb_is_inside(address, search_end, current_address, current_end))
        {
            result = current;
            ret = MDB_RANGE_FOUND_INNER;
        }
        if (ret < MDB_RANGE_FOUND_SURROUNDING &&
            current_address <= address &&
            // exclude 0-length match with curaddr==addr
            current_address < search_end &&
            current_end >= search_end &&
            // exclude 0-length match with currend==addr
            current_end > address)
        {
            result = current;
            ret = MDB_RANGE_FOUND_SURROUNDING;
        }
        if (ret > max_precision) {
            *ret_node = NULL;
            return ret;
        }
    }

    if (N(current)->left) {
        mdb_sub_find_range_merge(root, address, size, max_precision,
                                 N(current)->left, /*inout*/&ret,
                                 /*inout*/&result);
        if (ret > max_precision) {
            *ret_node = NULL;
            return ret;
        }
    }

    if (N(current)->right && root >= current_root &&
        (search_end > current_address || (search_end == current_address && size == 0))) {
        mdb_sub_find_range_merge(root, address, size, max_precision,
                                 N(current)->right, /*inout*/&ret,
                                 /*inout*/&result);
        if (ret > max_precision) {
            *ret_node = NULL;
            return ret;
        }
    }

    *ret_node = result;
    return ret;

}

errval_t
mdb_find_range(mdb_root_t root, genpaddr_t address, gensize_t size,
               int max_result, /*out*/ struct cte **ret_node,
               /*out*/ int *result)
{
    if (max_result < MDB_RANGE_NOT_FOUND ||
        max_result > MDB_RANGE_FOUND_PARTIAL)
    {
        return CAPS_ERR_INVALID_ARGS;
    }
    if (max_result > MDB_RANGE_NOT_FOUND && !ret_node) {
        return CAPS_ERR_INVALID_ARGS;
    }
    if (!result) {
        return CAPS_ERR_INVALID_ARGS;
    }

    struct cte *alt_ret_node;
    if (!ret_node) {
        ret_node = &alt_ret_node;
    }

    *result = mdb_sub_find_range(root, address, size, max_result, mdb_root, ret_node);
    return SYS_ERR_OK;
}

errval_t
mdb_find_cap_for_address(genpaddr_t address, struct cte **ret_node)
{
    int result;
    errval_t err;
    // query for size 1 to get the smallest cap that includes the byte at the
    // given address
    err = mdb_find_range(get_type_root(ObjType_RAM), address,
                         1, MDB_RANGE_FOUND_SURROUNDING, ret_node, &result);
    if (err_is_fail(err)) {
        return err;
    }
    if (result != MDB_RANGE_FOUND_SURROUNDING) {
        return SYS_ERR_CAP_NOT_FOUND;
    }
    return SYS_ERR_OK;
}
