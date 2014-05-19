/**
 * \file
 * \brief Skip list implementation used for attribute index.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include "skiplist.h"

#define SKIP_LEVEL_PROBABILITY 0.5
// With probability 0.5 use log2(n) as MAX_LEVEL (n = amount of elements
// to be expected for index). 2**14 = 16384
#define MAX_LEVEL 14 


/**
 * \brief This code reads the cycle counter
 *
 * We use this to get some randomness for the skip list algorithm.
 **/
static inline uint64_t get_cycle_counter(void)
{
#if defined(__x86_64__)
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
#else
    return 0xdead;
#endif
}

/**
 * \brief Random number generator
 *
 * Pseudo-random number generator using Multiply-with-carry
 * method invented by George Marsaglia.
 *
 * \note This is a hack since posixcompat random does not seem
 * to work properly.
 */
static uint32_t my_random(void)
{
    static bool seeded = false;
    static uint32_t m_z = 0xdeadbeef;
    static uint32_t m_w = 0;
    if (!seeded) {
        m_w = (uint32_t) get_cycle_counter();
        seeded = true;
    }

    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);

    return (m_z << 16) + m_w;  // 32-bit result
}

/**
 * \return Random number between [0.0, 1.0]
 */
static inline float frand(void)
{
    return ((float) (my_random() & 0xffffffff)) / 0xffffffff;
}

/**
 * \brief Generates a random level number.
 *
 * Distribution (hopefully) for returned value:
 * 0 => 50%
 * 1 => 25%
 * 2 => 12.5%
 * ...
 *
 * \return A random number between 0 and MAX_LEVEL
 */
static inline size_t random_level(void)
{
    size_t level = 0;
    while(frand() < SKIP_LEVEL_PROBABILITY && level < MAX_LEVEL) {
        level++;
    }

    return level;
}

/**
 * \brief Create a new skip list element.
 */
static errval_t new_node(struct skip_node** sn, void* element, size_t level)
{
    *sn = malloc(sizeof(struct skip_node));
    if (*sn == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    (*sn)->forward = calloc(level+1, sizeof(struct skip_node*));
    if ((*sn)->forward == NULL) {
        free(*sn);
        *sn = NULL;

        return LIB_ERR_MALLOC_FAIL;
    }

    (*sn)->element = element;

    return SYS_ERR_OK;
}

/**
 * \brief Create a skip set.
 */
errval_t skip_create_list(struct skip_list** ss)
{
    *ss = malloc(sizeof(struct skip_list));
    if (*ss == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    errval_t err = new_node(&(*ss)->header, NULL, MAX_LEVEL);
    if (err_is_ok(err)) {
        (*ss)->level = 0;
        (*ss)->entries = 0;
    }
    else {
        free(*ss);
    }

    return err;
}

/**
 * \brief Print elements in set.
 *
 * Used for debugging.
 */
void skip_print_list(struct skip_list* ss)
{
    struct skip_node* cur = ss->header->forward[0];

    printf("{");
    while(cur != NULL) {
        printf("%s", cur->element);
        cur = cur->forward[0];
        if(cur != NULL)
            printf(",");
    }

    printf("}\n");
}

/**
 * \brief Checks if a element is in the list.
 *
 * \param ss Skip list
 * \param elem The element to find.
 *
 * \retval true Element found.
 * \retval false Element not found.
 */
bool skip_contains(struct skip_list* ss, char* elem) {
    assert(ss != NULL);
    assert(elem != NULL);

    struct skip_node* cur = ss->header;
    for (int64_t i = ss->level; i >= 0; i--) {
        while(cur->forward[i] != NULL && strcmp(cur->forward[i]->element, elem) < 0) {
            cur = cur->forward[i];
        }
    }
    cur = cur->forward[0];


    if (cur != NULL && strcmp(cur->element, elem) == 0) {
        return true;
    }

    return false;
}

/**
 * \brief Insert element in list.
 *
 * \note The skip list is actually a set. So
 * duplicates will be ignored.
 *
 * \param ss The list where we insert.
 * \param to_insert The element we'd like to insert.
 */
void skip_insert(struct skip_list* ss, char* to_insert) {
    assert(to_insert != NULL);

    struct skip_node* cur = ss->header;
    struct skip_node* update[MAX_LEVEL + 1] = { NULL };

    // Find element in table and save previous pointer which
    // needs to be modified on all levels
    for(int64_t i = ss->level; i >= 0; i--) {
        while(cur->forward[i] != NULL && strcmp(cur->forward[i]->element, to_insert) < 0) {
            cur = cur->forward[i];
        }
        update[i] = cur;
    }
    cur = cur->forward[0];

    // Need to insert a new node in list
    if(cur == NULL || strcmp(cur->element, to_insert) != 0) {
        size_t level = random_level();
        // In case we hit a new level cap, make sure to update
        // the header node
        if(level > ss->level) {
            for(size_t i = ss->level+1; i <= level; i++) {
                update[i] = ss->header;
            }
            ss->level = level;
        }

        // Insert new node
        errval_t err = new_node(&cur, to_insert, level);
        assert(err_is_ok(err)); // XXX

        // update next & prev pointer
        for(size_t i=0; i <= level; i++) {
            cur->forward[i] = update[i]->forward[i];
            update[i]->forward[i] = cur;
        }

        ss->entries++; // maintain list count
    }
}

/**
 * \brief Remoeves an element for the skip list.
 *
 * \param ss List to search for the element.
 * \param to_delete The element to remove.
 */
char* skip_delete(struct skip_list* ss, char* to_delete) {
    struct skip_node* cur = ss->header;
    struct skip_node* update[MAX_LEVEL + 1] = { NULL };

    for(int64_t i = ss->level; i >= 0; i--) {
        while(cur->forward[i] != NULL && strcmp(cur->forward[i]->element, to_delete) < 0) {
            cur = cur->forward[i];
        }
        update[i] = cur;
    }
    cur = cur->forward[0];

    if(strcmp(cur->element, to_delete) == 0) {
        // update prev->next pointer
        for(size_t i=0; i <= ss->level; i++) {
            if(update[i]->forward[i] != cur)
                break;
            update[i]->forward[i] = cur->forward[i];
        }
        char* to_return = cur->element;
        free(cur->forward);
        free(cur);

        while(ss->level > 0 && ss->header->forward[ss->level] == NULL) {
            ss->level--;
        }

        ss->entries--;
        return to_return;
    }

    return NULL; // element not found
}

static int compare_entry_count(const void* s1, const void* s2) {

    struct skip_list* list1 = *(struct skip_list* const*) s1;
    struct skip_list* list2 = *(struct skip_list* const*) s2;

    return list1->entries - list2->entries;
}

/**
 * \brief Compute the intersection of multiple sets on-the-fly by
 * repeated calls to this function.
 *
 * \param sets Array of pointer to the lists we want to intersect.
 * \param set_count How many lists we want to intersect.
 * \param next Null in case we want to start a fresh intersect
 * computation. Otherwise provide the last returned element
 * here to continue computing subsequent intersection elements.
 *
 */
char* skip_intersect(struct skip_list** sets, size_t set_count, char* next)
{
    static struct skip_node** state = NULL;
    if (next == NULL) {
        free(state);
        state = calloc(set_count, sizeof(struct skip_node*));

        // Improvement: Sort skip lists based on their amount of stored elements
        // saves some time doing actual intersection in case
        // the first lists have the most entries...
        qsort(sets, set_count, sizeof(struct skip_list*), compare_entry_count);

        for (size_t i = 0; i < set_count; i++) {
            state[i] = sets[i]->header;
        }
    }

    char* to_intersect = NULL;
    while(state[0]->forward[0] != NULL) {
        to_intersect = state[0]->forward[0]->element;

        // Check if to_intersect is contained in all other lists
        for (size_t j=1; j < set_count; j++) {
            struct skip_list* set = sets[j];
            struct skip_node* cur = state[j];

            for(int64_t k = set->level; k >= 0; k--) {
                while(cur->forward[k] != NULL && strcmp(cur->forward[k]->element, to_intersect) < 0) {
                    cur = cur->forward[k];
                }
            }
            cur = cur->forward[0];

            if (cur == NULL) {
                // reached the end of a list no more intersections possible
                goto no_more_possible;
            }
            else if (strcmp(cur->element, to_intersect) != 0) {
                // no match, continue with next element
                to_intersect = NULL;
                break;
            }
            else {
                // continue checking other sets
            }
        }

        state[0] = state[0]->forward[0];
        if (to_intersect != NULL) {
            // we found a intersection
            return to_intersect;
        }
    }


no_more_possible:
    return NULL;
}

#ifdef TEST_SKIPLIST
int main() {

    struct skip_set* ss = NULL;
    errval_t err = make_skipset(&ss);
    insert(ss, "a");
    insert(ss, "c");
    insert(ss, "b");
    insert(ss, "d");
    insert(ss, "aaa");

    struct skip_set* ss2 = NULL;
    err = make_skipset(&ss2);
    insert(ss2, "a");
    insert(ss2, "c");
    insert(ss2, "x");
    insert(ss2, "wer");
    insert(ss2, "12312");
    insert(ss2, "aaa");

    struct skip_set* ss3 = NULL;
    err = make_skipset(&ss3);
    insert(ss3, "asdf");
    insert(ss3, "c");
    insert(ss3, "x");
    insert(ss3, "wer");
    insert(ss3, "12312");
    insert(ss3, "aa");

    struct skip_set* sets[3] = { ss, ss2, ss3 };

    char* next = NULL;
    while( (next = skip_intersect(sets, 3, next)) != NULL) {
       printf("next is:%s\n", next);
    }

    while( (next = skip_intersect(sets, 3, next)) != NULL) {
       printf("next is:%s\n", next);
    }

    return 0;
}
#endif
