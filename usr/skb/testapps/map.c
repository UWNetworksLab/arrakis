/**
 * \file
 * \brief Computes a matrix multiplication. Tries to do that in a cache-aware way.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <skb/skb.h>
// #include <if/monitor_defs.h>
#include <barrelfish/dispatch.h>
#include <if/skb_map_defs.h>


//define that if you want to use the cache aware list allocation
#define CACHE_AWARE

//how many cores does this application support?
#define MAXCORES 32


static struct skb_map_client_response *mycl;
static struct skb_map_client_response * clients[32];
static int available_cores[MAXCORES];
static int available_valid = 0;

//prototypes
static void spawnmyself(bool bsp);
static void map_init_server(void);
static void map_init_client(uint64_t core_id);



#ifdef CACHE_AWARE
static int l1_line_size;

static void get_l1_line_size(void)
{
    //because the information might not yet be added to the SKB...
    for (int i = 0; i < 3; i++) {
        skb_execute("cache(_,_,1,data,_,_,LineSize,_),write(output,LineSize).");
        while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
        if (skb_read_error_code() == 0) {
            break;
        }
        thread_yield();
    }
    if (skb_read_error_code() == 0) {
        l1_line_size = atoi(skb_get_output());
    } else {
        printf("\nerror code = %d\n", skb_read_error_code());
        printf("\nerror output = %s\n", skb_get_error_output());
        l1_line_size = 64;
        printf("\nassuming %d bytes\n", l1_line_size);
    }
    printf("\ncache line size is %d bytes.\n", l1_line_size);
}

//************************* memory management **********************************
struct memmmgt {
    struct capref frame;
    void *va;
    uint64_t bitmap[];
};

static struct capref mmgtframe;
static void *mmgtva;
static uint64_t nrbitmapfields;
static uint64_t structsize;
static uint64_t nrstructs;

//1 bit corresponds to a memory block of 1 L1 cache line
static int init_mem(void)
{
    int r;

    r = frame_alloc(&mmgtframe, BASE_PAGE_SIZE, NULL);
    if (r != 0) {
        return (r);
    }
    mmgtva = vspace_map(mmgtframe, 0, BASE_PAGE_SIZE, NULL, NULL);
    if (mmgtva == 0) {
        return (-1);
    }
    nrbitmapfields = (BASE_PAGE_SIZE / l1_line_size) / 64;
    structsize = nrbitmapfields + sizeof(struct memmmgt);
    int adjust = structsize % l1_line_size;
    if (adjust != 0) {
        structsize += (l1_line_size - adjust);
    }
    nrstructs = BASE_PAGE_SIZE / structsize;
    memset(mmgtva, 0, BASE_PAGE_SIZE);
    printf("\nstructsize = %lu, #structs = %lu\n", structsize, nrstructs);
    return 0;
}

static void *alloc_mem(int size)
{
    int r;

    int nr_blocks = size / l1_line_size + ((size % l1_line_size) != 0);
    int mask = (1 << nr_blocks) - 1;

    for (int i = 0; i < nrstructs; i++) {
        struct memmmgt *tmp = (struct memmmgt*)(i * structsize + mmgtva);
        if (tmp->va == 0) {
            r = frame_alloc(&(tmp->frame), BASE_PAGE_SIZE, NULL);
            if (r != 0) {
                return (0);
            }
            tmp->va = vspace_map(tmp->frame, 0, BASE_PAGE_SIZE, NULL, NULL);
            if (tmp->va == 0) {
                return (0);
            }
        }
        for (int j = 0; j < nrbitmapfields; j++) {
            for (int k = 0; k < (64 - nr_blocks); k++) {
                if ((tmp->bitmap[j] & (mask << k)) == 0) {
                    tmp->bitmap[j] |= (mask << k);
                    return (tmp->va + ((j * 64) + k) * l1_line_size);
                }
            }
        }
    }
    return (0);
}

static uint64_t get_offset_from_page(void *va)
{
    for (int i = 0; i < nrstructs; i++) {
        struct memmmgt *tmp = (struct memmmgt*)(i * structsize + mmgtva);
        if ((va >= tmp->va) && (va <= tmp->va)) {
            return (va - tmp->va);
        }
    }
//page not found... (shouldn't happen...
//does not matter, then the allocation strategy will not be optimal,
//but will still work
    return (0); 
}

static int *alloc_list(int size)
{
    int eff_size = sizeof(int) * (size + 1);
    int adjust = eff_size % l1_line_size;
    if (adjust != 0) {
        eff_size += (l1_line_size - adjust);
    }
    assert((eff_size % l1_line_size) == 0);
    int *tmp = (int*)alloc_mem(eff_size);
    printf("\ntmp = %p\n", tmp);
    assert(tmp != 0);
    assert(((vaddr_t)tmp % l1_line_size) == 0);
    tmp[0] = size;
    return (tmp);
}

static void free_list(int *list)
{
    printf("\nXXX: free_list does nothing.\n");
}
#else
static int *alloc_list(int size)
{
    int *tmp = (int*)malloc(size * sizeof(int));
    assert(tmp != 0);
    tmp[0] = size;
    return (tmp);
}
static void free_list(int *list)
{
    free(list);
}
#endif

//*********************************************

//**************** thread allocation plan **************************************
int nr_threads;
struct threadalloc {
    uint8_t core_id;
    vaddr_t start, end;
};

struct threadalloc ta[MAXCORES];

//**************** parsing the thread results from the SKB *********************
static void threadplan(paddr_t startaddress, paddr_t endaddress)
{
    printf("\nmap: requesting threadplan...\n");
    uint64_t nr_threads, start, end;
    uint32_t core_id;
    char query[128];

    if (available_valid == 0) {
        return;
    }
    char threadlist[128];
    sprintf(threadlist, "%d", available_cores[0]);
    for (int i = 1; i < available_valid; i++) {
        sprintf(threadlist,"%s,%d", threadlist, available_cores[i]);
    }
    printf("\nthreadlist = %s\n", threadlist);
    sprintf(query, "allocthread([%s], %lu, %lu, L), length(L,Len),"
            "write(ouput,Len), write(output,L).",
            threadlist, startaddress, endaddress);
    skb_execute(query);
    while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
    printf("\nerror code = %d\n", skb_read_error_code());
    printf("\noutput = %s\n", skb_get_output());
    printf("\nerror output = %s\n", skb_get_error_output());

    char *output = skb_get_output();
//    sscanf(output,"%lu",&nr_threads);
    nr_threads = atoi(output);
    printf("\nshould allocate %lu threads.\n", nr_threads);
    for (int i = 0; i < nr_threads; i++) {
        while (!((output[0] >= '0') && (output[0] <= '9'))) output++;
//        sscanf(output, "range(%u, %lu, %lu)", &core_id, &start, &end);
        core_id = atoi(output);
        while (!((output[0] >= '0') && (output[0] <= '9'))) output++;
        start = atoi(output);
        while (!((output[0] >= '0') && (output[0] <= '9'))) output++;
        end = atoi(output);
        printf("thread on core %u, range [%lu, %lu]\n", core_id, start, end);
        output++;
    }
}


//*********************************************

static void map(int(*f)(int),int *list, int **newlist)
{
    int *tmp = alloc_list(list[0]);
    *newlist = tmp;
    for (int i = 0; i < list[0]; i++) {
        tmp[i + 1] = f(list[i + 1]);
    }
}

static void print_list(int *list)
{
    printf("[");
    for (int i = 0; i < list[0]; i++) {
        printf("%d,", list[i + 1]);
    }
    printf("]\n");
}



//********************************* user functions *****************************
static int sqr_list(int in)
{
    return (in * in);
}

typedef int(*f)(int);
static f functions[] = {sqr_list};

static uint64_t core_id;

int main(int argc, char **argv)
{
    printf("map: connecting to the SKB...\n");
    skb_client_connect();
    printf("map: connected.\n");

    skb_create_buffer();

    core_id = disp_get_core_id();
    printf("\nmap: running one core %lu\n", core_id);
    printf("\nmap %lu: argc = %d\n", core_id, argc);


//get the L1 cache line size and initialize the memory management accordingly
#ifdef CACHE_AWARE
    get_l1_line_size();
    init_mem();
#endif
    if (argc == 1) { //this is the bootstrap copy of the domain
        map_init_server();
        spawnmyself(true);
    } else {
        printf("\nmap %lu: waiting for requests from the main domain\n", core_id);
        map_init_client(core_id);
        mycl->f->initialized(mycl, core_id);
        messages_handler_loop();
    }

    ////wait a bit for the cloned domains to come up...
    //messages_wait_and_handle_next();
    messages_handler_loop();

    threadplan(0, 64);

//allocate a list at cache line boundaries. Fill it with values and apply
//a square function on every element using map. Print the newly allocated result
//list.
    int *list1 = alloc_list(17);
    for (int i = 0; i < 17; i++) {
        list1[1 + i] = i;
    }
    print_list(list1);
    int *list2;
    uint64_t offset = get_offset_from_page(list1);
    offset = offset;
    map(functions[0], list1, &list2);
    print_list(list2);

    free_list(list1);
    free_list(list2);

    messages_handler_loop();
    return 0;
}








//*********** everything needed to spawn myself to all other cores *************

/* Generic buffer set */
static void set_generic_buf_reply(struct monitor_client_response *st, errval_t msgerr)
{
    assert(err_is_ok(msgerr));
    errval_t err;
    printf("\nmap %lu: set_generic_buf_reply\n", core_id);
    /* Spawn self on all cores except the current */
    err = st->call_vtbl->spawn_domain_request
        (st, "map", "map spawned", 0, 2);
    assert(err_is_ok(err));
}

/* All clients spawned */
static void spawn_domain_reply(struct monitor_client_response *st, errval_t err){}

static void spawnmyself(bool bsp)
{
    errval_t err;

    printf("\nmap %lu: bsp = %d\n", core_id, bsp);
    /* Set handlers */
    get_monitor_closure()->f->spawn_domain_reply = spawn_domain_reply;
    get_monitor_closure()->f->set_generic_buf_reply = set_generic_buf_reply;
    if (bsp) { /* bsp */
        /* Set generic buffer channel with the monitor
           and spawn bcast on additional cores */
        err = monitor_client_set_generic_buf();
        assert(err_is_ok(err));
    }
}



//***************** handlers ****************************************************

static void skb_map_share_page_c(struct skb_map_client_response *cc,
                               struct capref cap, uint64_t pagenumber)
{

}
static void skb_map_share_page_s(struct skb_map_service_response *cc,
                               struct capref cap, uint64_t pagenumber)
{

}

static void skb_map_mapfunction_c(struct skb_map_client_response *cc, uint64_t functionnr,
                                uint64_t startaddress, uint64_t endaddress)
{
}
static void skb_map_mapfunction_s(struct skb_map_service_response *cc, uint64_t functionnr,
                                uint64_t startaddress, uint64_t endaddress)
{
}

static void skb_map_finished_c(struct skb_map_client_response *cc, uint64_t core_id)
{}
static void skb_map_finished_s(struct skb_map_service_response *cc, uint64_t core_id)
{}

static void skb_map_initialized_c(struct skb_map_client_response *cc, uint64_t coreid)
{
    printf("\nmap: got connection from %lu\n", coreid);
    clients[coreid] = cc;
    available_cores[available_valid++] = coreid;
}

static void skb_map_initialized_s(struct skb_map_service_response *cc, uint64_t coreid)
{
/*
    printf("\nmap: got connection from %lu\n", coreid);
    clients[coreid] = cc;
    available_cores[available_valid++] = coreid;
*/
}





//***** everything needed to connect as client to the server domain *******
bool skb_map_connected = false;
static void client_disconnect_handler(struct skb_map_client_response *st)
{
}


static void client_connection_service_logic(struct skb_map_client_response *st)
{
    skb_map_connected = true;
    mycl = st;
}

static void start_client(struct chips_context *context, uint64_t core_id)
{
    iref_t iref;

    errval_t e = chips_blocking_lookup(context, "skb_map", &iref);
    if (err_is_fail(e)) {
        fprintf(stderr, "map %lu: could not connect to the main map domain.\n"
                "Terminating.\n", core_id);
        abort();
    }

    assert(iref != 0);


    static struct skb_map_client_response_vtbl crv = {
        .sharepage = skb_map_share_page_c,
        .mapfunction = skb_map_mapfunction_c,
        .mapfinished = skb_map_finished_c,
        .initialized = skb_map_initialized_c,
        ._disconnect = client_disconnect_handler,
        ._connected = client_connection_service_logic
    };

    static struct skb_map_client_response cr = {
        .f = &crv
    };

    skb_map_connect(iref, &cr, 0);
}

static void map_init_client(uint64_t core_id)
{
    struct chips_context *context = chips_get_context();
    context->init();
    start_client(context, core_id);
    while (!skb_map_connected) {
        messages_wait_and_handle_next();
    }
}



//***** everything needed to create the server part on all other domains *******
//connection prototypes
static void listen_cb(struct skb_map_service *st, iref_t iref);
static void skb_map_disconnect_handler(struct skb_map_service_response *cl);
static errval_t connection_service_logic(struct skb_map_service_response *cl);


struct client_closure {
};

struct listen_closure {
} lc;

struct skb_map_server_call_vtbl skb_map_call_vtbl =
    {
        .sharepage = skb_map_share_page_s,
        .mapfunction = skb_map_mapfunction_s,
        .mapfinished = skb_map_finished_s,
        .initialized = skb_map_initialized_s,
        ._listening = listen_cb,
        ._disconnect = skb_map_disconnect_handler,
        ._connected = connection_service_logic,
    };
struct skb_map_service skb_map_service = 
    {
        .f = &skb_map_call_vtbl,
        .st = &lc
    };

static errval_t connection_service_logic(struct skb_map_service_response *cl)
{
    struct client_closure *cc = (struct client_closure*)
        malloc(sizeof(struct client_closure));

    cl->st = cc;

    return 0;
}

static void skb_map_disconnect_handler(struct skb_map_service_response *cl)
{
/*
    struct client_closure *cc = 
        (struct client_closure *) cl->st;

    free(cc);
    cc = 0;
    free(closure);
    closure = 0;
*/
}

//called when listen suceeds
static void listen_cb(struct skb_map_service *st, iref_t iref)
{
    assert(iref != 0); //if iref == 0, listen failed.
    struct chips_context *context = chips_get_context();
    context->register_service("skb_map", iref, NULL, NULL);
}

static void map_init_server(void)
{
    int r;

    struct chips_context *context = chips_get_context();
    context->init();
    r = skb_map_listen(&skb_map_service);
    assert(r == 0);
}
