/**
 * \file
 * \brief NFS-populated file cache for HTTP server
 *
 * This very stupid "cache" assumes that:
 *  All regular files in a hardcoded NFS mount point are cached at startup
 *  The contents of the cache never change nor expire
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <nfs/nfs.h>
#include <lwip/init.h>
#include <lwip/ip_addr.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <timer/timer.h>
#include <contmng/netbench.h>
#include "webserver_network.h"
#include "webserver_debug.h"
#include "webserver_session.h"
#include "http_cache.h"

/* NOTE: enable following #def if you want cache to be preloaded */
#define PRELOAD_WEB_CACHE 1

/* Enable tracing only when it is globally enabled */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
//#define ENABLE_WEB_TRACING 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

//#define MAX_NFS_READ       14000
#define MAX_NFS_READ      1330 /* 14000 */ /* to avoid packet reassembly inside driver */

/* Maximum staleness allowed */
#define MAX_STALENESS ((cycles_t)9000000)

static void (*init_callback)(void);


struct http_cache_entry {
    int                 valid;      /* flag for validity of the data */
    char                *name;      /* name of the cached file */
    size_t              copied;     /* how much data is copied? */
    int                 loading;    /* flag indicating if data is loading */
    struct buff_holder  *hbuff;      /* holder for buffer */
    struct nfs_fh3      file_handle;    /* for NFS purpose */
    struct http_conn *conn;     /* list of connections waiting for data */
    struct http_conn *last;        /* for quick insertions at end */
    struct http_cache_entry *next;   /* for linked list */
};

/* global states */
static struct nfs_fh3 nfs_root_fh;  /* reference to the root dir of NFS */
static struct nfs_client *my_nfs_client; /* The NFS client */

static struct http_cache_entry *cache_table = NULL; /*root cached entry array */
static struct http_cache_entry *error_cache = NULL; /* cache entry for error */


#ifdef PRELOAD_WEB_CACHE
/* Initial cache loading state variables */
static bool cache_loading_phase = false;
static bool readdir_complete = false;
static int cache_lookups_started = 0;
static int cache_loaded_counter = 0;
static int cache_loading_probs = 0;
static void handle_cache_load_done(void);
#endif // PRELOAD_WEB_CACHE

// Variables for time measurement for performance
static uint64_t last_ts = 0;

/* allocate the buffer and initialize it. */
static struct buff_holder *allocate_buff_holder (size_t len)
{
    struct buff_holder *result = NULL;
    result = (struct buff_holder *) malloc (sizeof (struct buff_holder));
    assert (result != NULL );
    memset (result, 0, sizeof(struct buff_holder));
    if ( len > 0) {
        result->data = malloc (len);
        assert (result->data != NULL);
    }
    /* NOTE: 0 is valid length and used by error_cache */
    result->len = len;
    result->r_counter = 1; /* initiating ref_counter to 1, and using it as ref
            for free */
    return result;
} /* end function: allocate_buff_holder */

/* increments and returns the incremented value of the ref_counter for bh */
static long increment_buff_holder_ref (struct buff_holder *bh)
{
    ++bh->r_counter;
    return (bh->r_counter);
} /* end Function: increment_buff_holder_ref */

/* Decrements value of the ref_counter for bh
    if r_counter reaches zero then free all the memory */
long decrement_buff_holder_ref (struct buff_holder *bh)
{
    if (bh == NULL) {
        return 0;
    }

    --bh->r_counter;
    if (bh->r_counter > 0) {
        return (bh->r_counter);
    }

    if (bh->len > 0 && bh->data != NULL) {
        free (bh->data);
    }
    free (bh);
    return 0;
} /* end Function: increment_buff_holder_ref */


/* allocates the memory for the cacheline */
static struct http_cache_entry * cache_entry_allocate (void)
{
    struct http_cache_entry *e;
    e = (struct http_cache_entry *)
        malloc (sizeof(struct http_cache_entry));
    assert (e != NULL );
    memset (e, 0, sizeof(struct http_cache_entry));
    return e;
}

/* Function create_404_page_cache  creates error_cache entry,
    this entry will be used to reply when file is not found */
static void create_404_page_cache (void)
{
    error_cache = cache_entry_allocate();
    error_cache->hbuff = allocate_buff_holder (0);
    increment_buff_holder_ref (error_cache->hbuff);
    error_cache->valid = 1;
} /* end function: create_404_page_cache */

/* adds the http connection cs at the end of the list of connections waiting
    for the arrival of the data on this cacheline e. */
static void add_connection(struct http_cache_entry *e, struct http_conn *cs)
{
    assert (e != NULL);
    assert (cs != NULL);
    DEBUGPRINT ("%d: adding conn to waiting list of cacheline [%s]\n",
        cs->request_no, e->name);
    increment_http_conn_reference (cs);
    /* maintaining linked list with O(c) time and space complexity */
    if (e->conn == NULL) { /* list is empty, this is first element */
        assert(e->last == NULL); /* NOTE: just for safety, not actually needed*/
        e->conn = cs;
        e->last = cs;
        return;
    }
    /* adding element at end of the list */
    e->last->next = cs;
    e->last = cs;
} /* end function: add_connection */

/* Finds the cacheline associated with given name
 if no cacheline exists, it will create one,
 copy name as the key for cacheline */
static struct http_cache_entry *find_cacheline (const char *name)
{
    struct http_cache_entry *e;
    int l;
    // FIXME: simple linear search
    for (e = cache_table; e != NULL; e = e->next) {
        if (strcmp(name, e->name) == 0) {
            DEBUGPRINT ("cache-hit for [%s] == [%s]\n", name, e->name);
            return e;
        }
    } /* end for : for each cacheline */
    /* create new cacheline */
    e = cache_entry_allocate();
    /* copying the filename */
    l = strlen (name);
    e->name = (char *)malloc(sizeof(char)*(l+1));
    assert(e->name != NULL);
    strcpy(e->name, name);
    DEBUGPRINT ("cache-miss for [%s] so, created [%s]\n", name, e->name);
    e->next = cache_table;
    cache_table = e;
    return e;
} /* end function: find_cacheline */

static void delete_cacheline_from_cachelist (struct http_cache_entry *target)
{
    struct http_cache_entry *prev;
    struct http_cache_entry *e;

    if (cache_table == target){
        cache_table = target->next;
        return;
    }

    // FIXME: simple linear search
    prev = NULL;
    for (e = cache_table; e != NULL; e = e->next) {
        if (e == target) {
            prev->next = e->next;
            return;
        }
        prev = e;
    } /* end for : for each cacheline */
} /* end function: delete_cacheline_from_cachelist */


static void trigger_callback (struct http_conn *cs, struct http_cache_entry *e)
{

    if ( cs->mark_invalid ){
        /* This http_conn is not good anymore (most probably, it received RST)*/
        DEBUGPRINT ("%d: ERROR: callback triggered on invalid conn\n",
            cs->request_no );
        return;
    }

    /* making the callback */
    cs->hbuff = e->hbuff;
    /* increasing the reference */
    increment_buff_holder_ref (cs->hbuff);
    cs->callback(cs);
} /* end function: trigger_callback */


/* send callback for each pending http_conn */
static void handle_pending_list(struct http_cache_entry *e)
{
    struct http_conn *cs = NULL;
    struct http_conn *next_cs = NULL;
    if(e == NULL) {
    	printf("ERROR: handle_pending_list: null passed for cache entry\n");
    	return;
    }
    if(e->conn == NULL){
//    	printf("ERROR: handle_pending_list: no one waiting\n");
    	return;
    }
    cs = e->conn;
    while (cs != NULL) {
    	e->conn = cs->next;
        trigger_callback(cs, e);
        next_cs = cs->next;
        /* Copying the next of cs as following function can release the cs. */
        decrement_http_conn_reference(cs);
        cs = next_cs;
    }

    /* clear the list attached to the cache */
    e->conn = NULL;
    e->last = NULL;
} /* end function : handle_pending_list */


static void read_callback (void *arg, struct nfs_client *client,
                          READ3res *result)
{
    struct http_cache_entry *e = arg;
    assert( e != NULL);

    assert (result != NULL);
    assert (result->status == NFS3_OK);
    READ3resok *res = &result->READ3res_u.resok;
    assert(res->count == res->data.data_len);

    assert (e->hbuff != NULL);
    assert (e->hbuff->data != NULL );

    assert (e->hbuff->len >= e->copied + res->data.data_len);
    memcpy (e->hbuff->data + e->copied, res->data.data_val, res->data.data_len);
    e->copied += res->data.data_len;

    DEBUGPRINT ("got response of len %d, filesize %lu for file %s\n",
    			res->data.data_len, e->copied, e->name);

    // free arguments
    xdr_READ3res(&xdr_free, result);

    if (!res->eof) {
        // more data to come, read the next chunk
        err_t err = nfs_read(client, e->file_handle, e->copied, MAX_NFS_READ,
                        read_callback, e);
        assert(err == ERR_OK);
        return;
    }

    /* This is the end-of-file, so deal with it. */
    e->valid = 1;
    e->loading = 0;
    decrement_buff_holder_ref (e->hbuff);

#ifdef PRELOAD_WEB_CACHE
    if (!cache_loading_phase) {
        handle_pending_list (e); /* done! */
        return;
    }

    /* This is cache loading going on... */
    printf("Copied %zu bytes for file [%s] of length: %zu\n",
            e->copied, e->name, e->hbuff->len);
    ++cache_loaded_counter;
    handle_cache_load_done();

#else // PRELOAD_WEB_CACHE
    handle_pending_list(e); /* done! */
#endif // PRELOAD_WEB_CACHE
}


static void lookup_callback (void *arg, struct nfs_client *client,
                            LOOKUP3res *result)
{
    LOOKUP3resok *resok = &result->LOOKUP3res_u.resok;
    err_t r;
    struct http_cache_entry *e = arg;

    DEBUGPRINT ("inside lookup_callback_file for file %s\n", e->name);
    assert(result != NULL );

    /* initiate a read for every regular file */
    if ( result->status == NFS3_OK &&
        resok->obj_attributes.attributes_follow &&
        resok->obj_attributes.post_op_attr_u.attributes.type == NF3REG) {

    	/* FIXME: check if the file is recently modified or not. */
		// resok->obj_attributes.post_op_attr_u.attributes.ctime;


        DEBUGPRINT("Copying %s of size %lu\n", e->name,
                    resok->obj_attributes.post_op_attr_u.attributes.size );

        /* Store the nfs directory handle in current_session (cs) */
        nfs_copyfh (&e->file_handle, resok->object);
        /* GLOBAL: Storing the global reference for cache entry */
        /* NOTE: this memory is freed in reset_cache_entry() */

        /* Allocate memory for holding the file-content */
        /* Allocate the buff_holder */
        e->hbuff = allocate_buff_holder(
            resok->obj_attributes.post_op_attr_u.attributes.size );
        /* NOTE: this memory will be freed by decrement_buff_holder_ref */

        /* increment buff_holder reference */
        increment_buff_holder_ref (e->hbuff);

        e->hbuff->len = resok->obj_attributes.post_op_attr_u.attributes.size;

        /* Set the size of the how much data is read till now. */
        e->copied = 0;

        r = nfs_read (client, e->file_handle, 0, MAX_NFS_READ,
                read_callback, e);
        assert (r == ERR_OK);

        // free arguments
        xdr_LOOKUP3res(&xdr_free, result);
        return;
    }

    /* Most probably the file does not exist */
    DEBUGPRINT ("Error: file [%s] does not exist, or wrong type\n", e->name);

#ifdef PRELOAD_WEB_CACHE
    if (cache_loading_phase){
    	++cache_loading_probs;
    	handle_cache_load_done();
    	return;
    }
#endif // PRELOAD_WEB_CACHE

    if (e->conn == NULL) {
    	/* No connection is waiting for this loading */
    	return;
    }

    /*	as file does not exist, send all the http_conns to error page. */
    error_cache->conn = e->conn;
    error_cache->last = e->last;
    handle_pending_list (error_cache); /* done! */

    /* free this cache entry as it is pointing to invalid page */
    e->conn = NULL;
    e->last = NULL;
    delete_cacheline_from_cachelist (e);
    if (e->name != NULL ) free(e->name);
    free(e);

    // free arguments
    xdr_LOOKUP3res(&xdr_free, result);
    return;
} /* end function: lookup_callback_file */

static err_t async_load_cache_entry(struct http_cache_entry *e)
{
    err_t r;
    assert(e != NULL);

    // FIXME: currently only works for files in root directory.
    // Do lookup for given filename in root dir
    DEBUGPRINT ("pageloading starting with nfs_lookup\n");
    r = nfs_lookup(my_nfs_client, nfs_root_fh, e->name,
                lookup_callback, e);
    assert(r == ERR_OK);
    return ERR_OK;
} /* end function : async_load_cache_entry */


err_t http_cache_lookup (const char *name, struct http_conn *cs)
{
    struct http_cache_entry *e;
    assert(cs != NULL);

    e = find_cacheline(name);
    if (e->valid == 1) {
        /* fresh matching cache-entry found */
        DEBUGPRINT ("%d: Fresh cache-entry, returning page [%s]\n",
                cs->request_no, name);
        trigger_callback (cs, e);
        return ERR_OK;
    } /* end if: valid cacheline */

    /* data not in cache */
    /* add this connection to the list of waiting on cacheline */
    add_connection (e, cs);

    /* Check if no-one is loading the data */
    if (e->loading == 0 ) {
        e->loading = 1;
        DEBUGPRINT ("%d: starting the page loading\n", cs->request_no);
        async_load_cache_entry(e);
    } /* end if: no-one else is loading the data */
    else {
        DEBUGPRINT ("%d: someone else is loading the page, so just waiting\n",
            cs->request_no);
    }

    return ERR_OK;
} /* end function: http_cache_lookup */




static void cache_timeout_event (struct timer *timer, void *arg)
{
    struct http_cache_entry *cache = arg;
    struct http_cache_entry *e;

    DEBUGPRINT ("CACHE_TIMEOUT: marking all cache entries for reload\n");
    for (e = cache; e != NULL; e = e->next) {
        if (e->valid == 1) {
            /* this if is to avoid any complications that might come because
                of getting cache_timeout when cacheline was in loading */
            e->valid = 0;
            e->loading = 0;

            decrement_buff_holder_ref (e->hbuff); /* Decrement buffer ref.  */
            e->hbuff = NULL;    /* loosing the pointer to buff */
        } /* end if: if cacheline is valid  */
    } /* end for : for each cacheline */
    DEBUGPRINT ("CACHE_TIMEOUT: Done with invalidating the cache entries\n");
} /* end function : cache_timeout_event */


#ifdef PRELOAD_WEB_CACHE

static void readdir_callback(void *arg, struct nfs_client *client,
                             READDIR3res *result)
{
    READDIR3resok *resok = &result->READDIR3res_u.resok;
    struct http_cache_entry *ce;
    entry3 *last = NULL;
    err_t r;

    DEBUGPRINT ("readdir_callback came in\n");
    assert(result != NULL && result->status == NFS3_OK);

    // FIXME: start here the measurement of file loading time

    // FIXME: Start the trace
#if ENABLE_WEB_TRACING
    printf("Starting tracing\n");

    errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_NNET,
                                    TRACE_EVENT_NNET_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_NNET,
                                    TRACE_EVENT_NNET_STOP, 0), 0);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_control failed");
    }
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_START, 0);
#else // ENABLE_WEB_TRACING
    printf("Tracing not enabled\n");
#endif // ENABLE_WEB_TRACING


    last_ts = rdtsc();
//    lwip_benchmark_control(1, BMS_START_REQUEST, 0, 0);
    // initiate a lookup for every entry
    for (entry3 *e = resok->reply.entries; e != NULL; e = e->nextentry) {
        if (strlen(e->name) < 3 ) {
            printf("ignoring the file %s\n", e->name);
            continue;
        }
        ++cache_lookups_started;
        printf("Loading the file %s\n", e->name);
        ce = find_cacheline(e->name);
        ce->loading = 1;
        async_load_cache_entry(ce);
        e->name = NULL; // prevent freeing by XDR
        last = e;

        // continue handling events till this page is not completly loaded.

/*
        struct waitset *ws = get_default_waitset();
        errval_t err;

        DEBUGPRINT ("looping for events inside loop of readdir_callback\n");
        while (ce->valid != 1) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }
        DEBUGPRINT ("readdir_callback: %s file loaded\n", e->name);
*/

    }

    /* more in the directory: repeat call */
    if (!resok->reply.eof) {
        assert(last != NULL);
        r = nfs_readdir(client, nfs_root_fh, last->cookie,
                        resok->cookieverf, readdir_callback, NULL);
        assert(r == ERR_OK);
    } else {
        readdir_complete = true;
        handle_cache_load_done();
    }

    // free arguments
    xdr_READDIR3res(&xdr_free, result);
}



static bool are_all_pages_loaded(void)
{
	if (readdir_complete == false ) {
		return false;
	}
	return (cache_lookups_started == (cache_loaded_counter + cache_loading_probs));
}

static void handle_cache_load_done(void)
{
	if (!are_all_pages_loaded()) {
		return;
	}
    DEBUGPRINT("initial_cache_load: entire cache loaded done\n");
    cache_loading_phase = false;

    // lwip_benchmark_control(1, BMS_STOP_REQUEST, 0, 0);
    //
    // Report the cache loading time

    uint64_t total_loading_time = rdtsc() - last_ts;
    printf("Cache loading time %"PU", %"PRIu64"\n",
            in_seconds(total_loading_time), total_loading_time);

//    lwip_print_interesting_stats();

    /* stop the trace. */
#if ENABLE_WEB_TRACING
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_STOP, 0);

    char *trace_buf_area = malloc(CONSOLE_DUMP_BUFLEN);
    assert(trace_buf_area);
    size_t used_bytes = 0;
    trace_dump_core(trace_buf_area, CONSOLE_DUMP_BUFLEN, &used_bytes, NULL,
            disp_get_core_id(),  true, true);

    printf("\n%s\n", "dump trac buffers: Start");
    printf("\n%s\n", trace_buf_area);
    printf("\n%s\n", "dump trac buffers: Stop");
    trace_reset_all();
//    abort();

    printf("Cache loading time %"PU", %"PRIu64"\n",
            in_seconds(total_loading_time), total_loading_time);

#endif // ENABLE_WEB_TRACING


    /* continue with the web-server initialization. */
    init_callback(); /* do remaining initialization! */
}

static void initial_cache_load(struct nfs_client *client)
{
    err_t r;
	cache_loading_phase = true;
	cache_lookups_started = 0;
	cache_loaded_counter = 0;
	cache_loading_probs = 0;

	//my_nfs_client
    r = nfs_readdir(client, nfs_root_fh, NFS_READDIR_COOKIE,
                         NFS_READDIR_COOKIEVERF, readdir_callback, NULL);
    assert(r == ERR_OK);
}

#endif // PRELOAD_WEB_CACHE


static void mount_callback(void *arg, struct nfs_client *client,
                 enum mountstat3 mountstat, struct nfs_fh3 fhandle)
{
    assert(mountstat == MNT3_OK);
    nfs_root_fh = fhandle; /* GLOBAL: assigning root of filesystem handle */
#ifdef PRELOAD_WEB_CACHE
    DEBUGPRINT ("nfs_mount successful, loading initial cache.\n");
    initial_cache_load(client);	/* Initial load of files for cache. */
#else //     PRELOAD_WEB_CACHE
    DEBUGPRINT ("nfs_mount successful\n");
    init_callback(); /* done! */
#endif // PRELOAD_WEB_CACHE
} /* end function: mount_callback */

err_t http_cache_init(struct ip_addr server, const char *path,
                     void (*callback)(void))
{
    struct timer *cache_timer;      /* timer for triggering cache timeouts */
    init_callback = callback;

    DEBUGPRINT ("nfs_mount calling.\n");
    my_nfs_client = nfs_mount(server, path, mount_callback, NULL);
    DEBUGPRINT ("nfs_mount calling done.\n");

    assert(my_nfs_client != NULL);
    /* creating the empty cache */
    cache_table = NULL;
    create_404_page_cache();


    cache_timer = timer_create(MAX_STALENESS, true, cache_timeout_event,
            cache_table);
    assert (cache_timer != NULL);
    if (cache_timer == NULL) {
        printf ("http_cache_init failed in timer_create\n");
        return ERR_MEM;
    }
    timer_start(cache_timer);
    DEBUGPRINT ("http_cache_init done\n");
    return ERR_OK;
} /* end function: http_cache_init */


