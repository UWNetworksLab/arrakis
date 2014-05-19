/**
 * \file
 * \brief Simple coreset implementation
 *
 * Specifies a set of cores
 *
 * \bug Currently only supports a maximum of 64 cores.
 * Since we do not have a sensible way of sending structs over flounder,
 * this provides an API to convert a coreset to coremask which
 * can be sent over flounder. This limits coreset to 64 cores.
 */

/*
 * Copyright (c) 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/coreset.h>
#include <string.h>

#define CORESET_BITS_PER_WORD (sizeof(uintptr_t) * NBBY)
#define CORESET_WORDS DIVIDE_ROUND_UP(MAX_COREID, CORESET_BITS_PER_WORD)

struct coreset {
    uintptr_t bits[CORESET_WORDS];
};

/**
 * \brief Covert a coreset to coremask
 *
 * \param set     The coreset to convert
 * \param mask    Use to return the mask
 *
 * \bug This is used to support sending coreset over flounder.
 * This should be removed once we can sensibly send structs over flounder
 */
errval_t coreset_to_coremask(struct coreset *set, coremask_t *mask)
{
    memset(mask->bits, 0, sizeof(mask->bits));

    for (int i = 0; i < MAX_COREID; i++) {
        if (coreset_test(set, i)) {
            mask->bits[i % _COREMASK_BITS_PER_WORD]
                |= (_coremask_word_t)1 << (i / _COREMASK_BITS_PER_WORD);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Covert a coremask to coreset
 *
 * \param mask    The mask to covert
 * \param set     The returned set
 *
 * The returned set is allocated using #coreset_new and should be destroyed
 *
 * \bug This is used to support sending coreset over flounder.
 * This should be removed once we can sensibly send structs over flounder
 */
errval_t coreset_from_coremask(coremask_t mask, struct coreset **set)
{
    errval_t err;

    err = coreset_new(set);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CORESET_NEW);
    }

    for (int i = 0; i < MAX_COREID; i++) {
        if (mask.bits[i % _COREMASK_BITS_PER_WORD]
            & (_coremask_word_t)1 << (i / _COREMASK_BITS_PER_WORD)) {
            coreset_add(*set, i);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Initialize a coreset
 *
 * \param set     Used to return the allocated coreset
 */
errval_t coreset_new(struct coreset **retset)
{
    // Allocate
    struct coreset *set = malloc(sizeof(struct coreset));
    if (!set) {
        return LIB_ERR_MALLOC_FAIL;
    }

    // Initialize
    memset(set->bits, 0, sizeof(set->bits));

    *retset = set;
    return SYS_ERR_OK;
}

/**
 * \brief Destroy a coreset
 *
 * \param set     The coreset to destroy. It cannot be used anymore
 */
void coreset_destroy(struct coreset *set)
{
    free(set);
}

/**
 * \brief Add a core to the coreset
 *
 * \param set    The coreset in which to add
 * \param id     The core to add
 *
 */
errval_t coreset_add(struct coreset *set, coreid_t id)
{
    assert(id < MAX_COREID);
    size_t nword = id / CORESET_BITS_PER_WORD;
    size_t nbit = id % CORESET_BITS_PER_WORD;

    set->bits[nword] |= (uintptr_t)1 << nbit;

    return SYS_ERR_OK;
}

/**
 * \brief Remove a core from the coreset
 *
 * \param set    The coreset in which to remove
 * \param id     The core to remove
 */
errval_t coreset_remove(struct coreset *set, coreid_t id)
{
    assert(id < MAX_COREID);
    size_t nword = id / CORESET_BITS_PER_WORD;
    size_t nbit = id % CORESET_BITS_PER_WORD;

    set->bits[nword] &= ~((uintptr_t)1 << nbit);

    return SYS_ERR_OK;
}

/**
 * \brief Test if a core is specified in a coreset
 *
 * \param set    The coreset in which to test
 * \param id     The core to test for
 */
bool coreset_test(struct coreset *set, coreid_t id)
{
    assert(id < MAX_COREID);
    size_t nword = id / CORESET_BITS_PER_WORD;
    size_t nbit = id % CORESET_BITS_PER_WORD;

    return (set->bits[nword] & ((uintptr_t)1 << nbit)) != 0;
}

/**
 * \brief Get the next item in the set
 *
 * \param set    The coreset
 * \param token  Token to maintain internal state
 * \param id     Pointer to return the coreid_t
 *
 * Iterates over all elements in #set.  #CORESET_INIT_TOKEN is used
 * for the #token the first time the function is called. The function
 * updates the token for subsequent calls.
 *
 * Use this as an alternative to #coreset_iterate if you want control
 * over exactly when the next element in the set is retrived.
 */
errval_t coreset_get_next(struct coreset *set, coreset_token_t *token,
                          coreid_t *id)
{
    // Token indicates which element the use wants
    coreid_t elem  = *token;
    (*token)++;

    // Count up in the set till the desired element is found

    // FIXME: this is stupidly inefficient in the common case.
    // Couldn't we store the last core ID in the token? -AB

    coreid_t count = 0;
    coreid_t i;
    for (i = 0; i < MAX_COREID; i++) {
        if (coreset_test(set, i)) {
            if (count == elem) {
                *id = i;
                return SYS_ERR_OK;
            }
            count++;
        }
    }

    // The desired element was not found
    return LIB_ERR_CORESET_GET_NEXT_DONE;
}

/**
 * \brief Iterate over all cores in the coreset
 *
 * \param set    The coreset to iterate over
 * \param st     Some user supplied state
 * \param func   The function pointer to use
 *
 * This will call #func on every core that exists in the
 * coreset in ascending order.
 * If #func ever returns a non-success errval_t, then this will terminate early
 * and return that errval_t to user.
 */
errval_t coreset_iterate(struct coreset *set, void *st,
                         coreset_iterator_fn func)
{
    for (int i = 0; i < MAX_COREID; i++) {
        if (coreset_test(set, i)) {
            errval_t err = func(st, i);
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief The iterator function for coreset_count
 */
static errval_t count_iterator(void *st, coreid_t id)
{
    coreid_t *count = st;
    (*count)++;
    return SYS_ERR_OK;
}

/**
 * \brief Count the number of cores in the coreset
 *
 * \param set    The coreset to count
 * \param count  Return the count of cores
 */
coreid_t coreset_count(struct coreset *set)
{
    errval_t err;

    coreid_t count = 0;

    err = coreset_iterate(set, &count, count_iterator);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "coreset_iterate should not fail");
    }

    return count;
}
