/**
 * \file
 * \brief VT-d domain management
 */

/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VTD_DOMAINS_H
#define VTD_DOMAINS_H

struct vtd_unit;

struct vtd_domain {
    // x86-64 VNode capability for the domain pagetable structure
    struct capref   pml4;
    // physical address associated with capability
    genpaddr_t      pt_gp;
    uint16_t        did;
    struct vtd_domain *  prev;
    struct vtd_domain *  next;
    // Pointer to the list of remapping hardware units
    struct vtd_unit *    units;
};

// A domain list is a sorted and bounded doubly-linked list with
// min_did and max_did specifying the bounds
struct vtd_domain_list {
    struct vtd_domain *head;
    struct vtd_domain *tail;
    int min_did;
    int max_did;
};

// Returns true if the domain list is empty, else false.
static inline int domain_list_empty(struct vtd_domain_list *lst)
{
    return (lst->head == NULL);
}

static inline struct vtd_domain_list *vtd_new_domain_list(void)
{
    struct vtd_domain_list *new_list;
    new_list = (struct vtd_domain_list *)malloc(sizeof(struct vtd_domain_list));
    assert(new_list != NULL);
    new_list->head = NULL;
    new_list->tail = NULL;
    new_list->min_did = 0;
    new_list->max_did = 0;
    return new_list;
}
 
static inline struct vtd_domain *vtd_new_domain(int did, genpaddr_t pt, struct capref pml4, struct vtd_unit *units)
{
    struct vtd_domain *new_domain = (struct vtd_domain *)malloc(sizeof(struct vtd_domain));
    assert(new_domain != NULL);
    new_domain->did   = did;
    new_domain->pt_gp = pt;
    new_domain->pml4  = pml4;
    new_domain->next  = NULL;
    new_domain->prev  = NULL;
    new_domain->units = units;
    return new_domain;
}

// Inserts newd as the new head of lst, a domain list with a size > 1
static inline void vtd_insert_new_head(struct vtd_domain *newd, struct vtd_domain_list *lst)
{
    newd->next = lst->head;
    lst->head->prev = newd;
    lst->head = newd;
}

// Inserts newd as the new tail of lst, a domain list with a size > 1
static inline void vtd_insert_new_tail(struct vtd_domain_list *lst, struct vtd_domain *newd)
{
    lst->tail->next = newd;
    newd->prev = lst->tail;
    lst->tail = newd;
}

// Inserts the domain newd between prevd and nextd.
static inline void vtd_insert_between(struct vtd_domain *prevd, struct vtd_domain *newd, struct vtd_domain *nextd)
{
    newd->next = nextd;
    newd->prev = prevd;
    prevd->next = newd;
    nextd->prev = newd;
}

// Inserts the domain newd before the domain nextd in the domain list doms.
static inline void vtd_insert_domain(struct vtd_domain *newd, struct vtd_domain *nextd, struct vtd_domain_list *doms) {
    assert(newd != NULL);
    if (domain_list_empty(doms)) {           // empty
        doms->head = newd;
	doms->tail = newd;
    } else if (newd->did == doms->min_did) { // new head
        vtd_insert_new_head(newd, doms);
    } else if (nextd == NULL) {              // new tail
        vtd_insert_new_tail(doms, newd);
    } else {                                 // between 
        vtd_insert_between(nextd->prev, newd, nextd);
    }
}

// Deletes the head of lst, a domain list with a size > 1.
static inline void vtd_delete_head(struct vtd_domain_list *lst) 
{
    struct vtd_domain *old_head = lst->head;
    lst->head->next->prev = NULL;
    lst->head = lst->head->next;
    free(old_head);
}

// Deletes the tail of lst, a domain list with a size > 1.
static inline void vtd_delete_tail(struct vtd_domain_list *lst)
{
    struct vtd_domain *old_tail = lst->tail;
    lst->tail->prev->next = NULL;
    lst->tail = lst->tail->prev;
    free(old_tail);
}

// Deletes the domain d between the domains prevd and nextd.
static inline void vtd_delete_between(struct vtd_domain *prevd, struct vtd_domain *d, struct vtd_domain *nextd)
{
    struct vtd_domain *temp = d;
    prevd->next = nextd;
    nextd->prev = prevd;
    free(temp);
}

// Deletes a list of domains consisting of just a single element.
static inline void vtd_delete_single(struct vtd_domain_list *lst) {
    free(lst->head);
    lst->head = NULL;
    lst->tail = NULL;
}

// Deletes the domain d from the non-empty list of domains lst. 
static inline void vtd_delete_domain(struct vtd_domain *d, struct vtd_domain_list *lst)
{
    assert(d != NULL);
    assert(lst != NULL);
    if (lst->head == lst->tail) {
	vtd_delete_single(lst);
    } else if (d == lst->head) {
        vtd_delete_head(lst);
    } else if (d == lst->tail) {
        vtd_delete_tail(lst);
    } else {
        vtd_delete_between(d->prev, d, d->next);
    }
}

#endif //VTD_DOMAINS_H
