/* Copyright (c) 2007-2009, Stanford University
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of Stanford University nor the names of its 
*       contributors may be used to endorse or promote products derived from 
*       this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY STANFORD UNIVERSITY ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL STANFORD UNIVERSITY BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

#ifndef LIST_H
#define LIST_H

/* doubly linked circular list with dummy node */
typedef struct list_ent list_ent;
typedef struct list list;

struct list_ent{
    list_ent    *li_next;
    list_ent    *li_prev;
};

struct list {
    list_ent    lst_list;
};

#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((size_t)&((TYPE *) 0)->MEMBER)
#endif

#define list_entry(LIST_ELEM, STRUCT, MEMBER)         \
        ((STRUCT *) ((uint8_t *) &(LIST_ELEM)->li_next    \
                        - offsetof (STRUCT, MEMBER.li_next)))

static inline void list_init(list* l)
{
    l->lst_list.li_next = &l->lst_list;
    l->lst_list.li_prev = &l->lst_list;
}

static inline void list_ent_init(list_ent* li)
{
    li->li_next = NULL;
    li->li_prev = NULL;
}

static inline list_ent* list_peek_head(list* l)
{
    if(&l->lst_list != l->lst_list.li_next)
        return l->lst_list.li_next;
    else
        return NULL;
}

static inline list_ent* list_peek_tail(list* l)
{
    if(&l->lst_list != l->lst_list.li_prev)
        return l->lst_list.li_prev;
    else
        return NULL;
}

static inline int list_is_empty(list* l)
{
    return (&l->lst_list == l->lst_list.li_next);
}

static inline void list_add_head(list* l, list_ent* toadd)
{
    list_ent*    ltop = &l->lst_list;
    toadd->li_prev = ltop;
    toadd->li_next = ltop->li_next;
    ltop->li_next->li_prev = toadd;
    ltop->li_next = toadd;
}

static inline void list_add_tail(list* l, list_ent* toadd)
{
    list_ent*    ltop = &l->lst_list;
    toadd->li_next = ltop;
    toadd->li_prev = ltop->li_prev;
    ltop->li_prev->li_next = toadd;
    ltop->li_prev = toadd;
}

static inline void list_insert_after(list_ent* chain, list_ent* toadd)
{
    toadd->li_prev = chain;
    toadd->li_next = chain->li_next;
    chain->li_next->li_prev = toadd;
    chain->li_next = toadd;
}

static inline void list_insert_before(list_ent* chain, list_ent* toadd)
{
    toadd->li_next = chain;
    toadd->li_prev = chain->li_prev;
    chain->li_prev->li_next = toadd;
    chain->li_prev = toadd;
}

static inline void list_remove(list_ent* elem)
{
    elem->li_next->li_prev = elem->li_prev;
    elem->li_prev->li_next = elem->li_next;
    elem->li_prev = NULL;
    elem->li_next = NULL;
}

static inline list_ent* list_remove_head(list* l)
{
    if(l->lst_list.li_next != &l->lst_list){
        list_ent    *head;
        head = l->lst_list.li_next;
        list_remove(head);
        return head;
    }else
        return NULL;
}

static inline list_ent* list_remove_tail(list* l)
{
    if(l->lst_list.li_prev != &l->lst_list){
        list_ent    *tail;
        tail = l->lst_list.li_prev;
        list_remove(l->lst_list.li_prev);
        return tail;
    }else
        return NULL;
}

static inline void list_clear(list* l)
{
    l->lst_list.li_next = &l->lst_list;
    l->lst_list.li_prev = &l->lst_list;
}

#define list_from(x, y)        for(; y != &((x)->lst_list); y = y->li_next)
#define list_for_all(x, y)    for(y = (x)->lst_list.li_next; y != &((x)->lst_list); y = y->li_next)
#define list_for_all_safe(x,y,z)\
    for(y = (x)->lst_list.li_next, z = y->li_next; y != &((x)->lst_list); y = z, z = y->li_next)
#define list_for_all_backwards(x, y) for(y = (x)->lst_list.li_prev; y != &((x)->lst_list); y = y->li_prev)
#define list_for_all_backwards_safe(x,y,z)\
    for(y = (x)->lst_list.li_prev, z = y->li_prev; y != &((x)->lst_list); y = z, z = y->li_prev)

#endif
