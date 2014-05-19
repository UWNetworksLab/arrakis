/*
 * threadalloc.c
 *
 *  Created on: Mar 21, 2011
 *      Author: scadrian
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <skb/skb.h>
#include <unistd.h>

#include <barrelfish/curdispatcher_arch.h>
#include <arch/x86/barrelfish/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon_amd.h>

#define GROESSI (128*1024*1024)

static volatile int nr_available_cores = 1;
static volatile int nr_spanned_cores = 0;

static volatile int nr_active_threads = 0;
static volatile int nr_finished_threads = 0;
static volatile uint64_t end = 0;
static volatile uint64_t start = 0;

static struct thread_mutex m;
static volatile spinlock_t s = 0;

static volatile uint64_t tmp_increments = 0;
static spinlock_t stmp = 0;
static struct thread_mutex mtmp;

static struct thread_cond all_done_condition;
static struct thread_mutex all_done_mutex;

struct addr {
    uint8_t *startaddr;
    int size;
};

#define NR_ALLOCATED_THREADS 22
//#define BUFFER_SIZE (1024 * 1024 * 1024)
#define BUFFER_SIZE (4 * 1024 * 1024)

static struct addr as[NR_ALLOCATED_THREADS];

static volatile int totalincrements = 0;
static volatile int value_per_thread = 1000000;
static int increment_tmp_var(void *arg)
{
    for (uint64_t i = 0; i < value_per_thread; i++) {
//        acquire_spinlock(&stmp);
        thread_mutex_lock(&mtmp);
        tmp_increments++;
//        if (tmp_increments % 10000 == 0) {
//            printf("tmp_increments: %lu\n", tmp_increments);
//        }
        if (tmp_increments == totalincrements) {
            thread_mutex_lock(&all_done_mutex);
            thread_cond_signal(&all_done_condition);
            thread_mutex_unlock(&all_done_mutex);
        }
        thread_mutex_unlock(&mtmp);
//        release_spinlock(&stmp);
    }
    return 0;
}
static void thread_done(void)
{
//    thread_mutex_lock(&m);
    acquire_spinlock(&s);
    nr_finished_threads++;
//    if (nr_finished_threads == nr_active_threads) {
//        end = rdtsc();
//    }
//    thread_mutex_unlock(&m);
    release_spinlock(&s);
}
static void spanned_cb(void *arg, errval_t reterr)
{
    assert(err_is_ok(reterr));
    nr_spanned_cores++;
    if (nr_spanned_cores == nr_available_cores - 1) {
        thread_mutex_lock(&all_done_mutex);
        thread_cond_signal(&all_done_condition);
        thread_mutex_unlock(&all_done_mutex);
    }
//    printf("nr_spanned_cores = %d\n", nr_spanned_cores);
}

static int create_skb_channel(void *arg)
{
    errval_t err = skb_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connection to the SKB from thread failed. Terminating.\n");
        return (1);
    }
    printf("SKB channel on core %"PRIuCOREID" created\n", disp_get_core_id());
    thread_done();
    return (0);
}

static int initialize_memory(void *arg)
{
    struct addr *ptr = (struct addr*)arg;
    for (int i = 0; i < ptr->size; i++) {
        ptr->startaddr[i] = 0;
    }
    thread_done();
    return 0;
}

static int initialize_number(void *arg)
{
    struct addr *ptr = (struct addr*)arg;
    for (int i = 0; i < ptr->size; i++) {
        ptr->startaddr[i] = i;
    }
    thread_done();
    return 0;
}

static int sqr(void *arg)
{
    struct addr *ptr = (struct addr*)arg;
    for (int i = 0; i < ptr->size; i++) {
        ptr->startaddr[i] = ptr->startaddr[i] * ptr->startaddr[i];
    }
    thread_done();
    return 0;
}

//static int writemem(void *arg)
//{
//    printf("very first program:\n");
//    unsigned char *intarray = (unsigned char *)arg;
//
//    for (int i = 0; i < GROESSI; i++) {
//        intarray[i] = 0;
//    }
//    start = rdtsc();
//    for (int i = 0; i < GROESSI; i++) {
//        intarray[i] = 0;
//    }
//    end = rdtsc();
//    printf("zykla: %lu\n", end - start);
//    printf("done.\n");
//    return 0;
//}


static int writemem(void *arg)
{
    printf("very first program:\n");
    unsigned char *intarray = (unsigned char *)arg;
    uint64_t *data = (uint64_t*)malloc(sizeof(uint64_t) * GROESSI);

    for (int i = 0; i < GROESSI; i++) {
        intarray[i] = 0;
    }
    for (int i = 0; i < GROESSI; i++) {
        start = rdtsc();
        intarray[i] = 0;
        end = rdtsc();
        data[i] = end - start;
    }
    printf("\n**************start***********\n\n");
    for (int i = 0; i < GROESSI; i++) {
        printf("###RES%d\t%lu\n",i, data[i]);
    }
    printf("\n**************stop***********\n\n");

//    printf("zykla: %lu\n", end - start);
    printf("done.\n");
    return 0;
}


int main(int argc, char **argv)
{
    errval_t err;
    
    printf("%s starting up...\n", argv[0]);
    
    err = skb_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connection to the SKB failed. Terminating.\n");
        return (0);
    }

    iref_t iref;
    err = nameservice_blocking_lookup("datagatherer_done", &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }
    
    err = skb_execute("available_nr_cores(Nr),write(nrcores(Nr)).");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not determine the number of cores."
                "Stay with one core...\n");
    }
    err = skb_read_output("nrcores(%d)", &nr_available_cores);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not determine the number of cores <2>."
                "Stay with one core...\n");
    }
    printf("nr available cores: %d\n", nr_available_cores);
    
    coreid_t my_core_id = disp_get_core_id();
    
    int curr_core_nr = (my_core_id + 1) % nr_available_cores;
    while (curr_core_nr != my_core_id) {
//    while (curr_core_nr != ((my_core_id + 11)%nr_available_cores)) {
        err = domain_new_dispatcher(curr_core_nr, spanned_cb, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain\n");
        }
        assert(err_is_ok(err));
//        printf("new thread on core %d\n", curr_core_nr);
        curr_core_nr = (curr_core_nr + 1) % nr_available_cores;
    }

    printf("ack\n");
    // we need to wait until the domain got spanned to all other cores
    // this means that we need to wait for n-1 callbacks
    thread_mutex_init(&all_done_mutex);
    thread_cond_init(&all_done_condition);

    printf("wait for the domain to be spanned to all other cores...\n");
    thread_mutex_lock(&all_done_mutex);
    thread_cond_wait(&all_done_condition, &all_done_mutex);
    thread_mutex_unlock(&all_done_mutex);
    printf("wait for the domain to be spanned: signalled.\n");



    printf("spanned to all cores!\n");
    unsigned char *intarray = (unsigned char*)malloc(GROESSI);
    printf("allocated.\n");


    curr_core_nr = (my_core_id + 1) % nr_available_cores;
    err = domain_thread_create_on(curr_core_nr, writemem, intarray);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not create thread on core\n");
    }

    // test: two threads that increment the same variable,
    // each one increments it by 100. the result should be
    // 200

    thread_mutex_init(&mtmp);
    
    thread_mutex_init(&all_done_mutex);
    thread_cond_init(&all_done_condition);
    
    curr_core_nr = (my_core_id + 1) % nr_available_cores;
    err = domain_thread_create_on(curr_core_nr, increment_tmp_var, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not create thread on core\n");
    }
    curr_core_nr = (my_core_id) % nr_available_cores;
    err = domain_thread_create_on(curr_core_nr, increment_tmp_var, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not create thread on core\n");
    }

    totalincrements = value_per_thread * 2;
    printf("set totalincrements to %d\n", totalincrements);
//    thread_mutex_lock(&all_done_mutex);
//    thread_cond_wait(&all_done_condition, &all_done_mutex);
//    thread_mutex_unlock(&all_done_mutex);
    printf("signalled, wait now in while loop.\n");
    while(tmp_increments < totalincrements);
//    for (int i = 0; i < 5000; i++) {
//        printf(".");
//    }
    printf("var = %ld\n", tmp_increments);
    
    // each dispatcher on every core except the original core needs
    // to create a channel to the SKB, otherwise it can't use the SKB
//    nr_active_threads = nr_available_cores - 1;
    nr_active_threads = nr_spanned_cores;
    nr_finished_threads = 0;
    thread_mutex_init(&m);
    s = 0;
    curr_core_nr = (my_core_id + 1) % nr_available_cores;
    while (curr_core_nr != my_core_id) {
        err = domain_thread_create_on(curr_core_nr, create_skb_channel, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not create thread on core\n");
            break;
        }
        curr_core_nr = (curr_core_nr + 1) % nr_available_cores;
    }
    while (nr_finished_threads != nr_active_threads);
    printf("\nall SKB channels created\n");


    void *ptr = sbrk(BUFFER_SIZE);
    printf("address = %p\n", ptr);
    void *saddr = ptr;
    for (int i = 0; i < NR_ALLOCATED_THREADS; i++) {
        as[i].startaddr = saddr;
        as[i].size = BUFFER_SIZE / NR_ALLOCATED_THREADS;
        saddr += (BUFFER_SIZE / NR_ALLOCATED_THREADS);
        printf("start %d: %p, size %d: %d\n", i, as[i].startaddr, i, as[i].size);
    }
    
    thread_cond_init(&all_done_condition);

    end = 0;
    dispatcher_handle_t my_dispatcher = curdispatcher();
//    uint64_t event = EVENT_AMD_L2_REQUESTS;
    uint64_t event = EVENT_AMD_L2_CACHE_MISSES;
    uint64_t umask = UMASK_AMD_L2_REQUEST_DC_FILL;
    int counter = 0;
    perfmon_setup(my_dispatcher, counter, event, umask, false);

    static struct addr as_master;
    as_master.size = BUFFER_SIZE;
    as_master.startaddr = ptr;

    start = rdtsc();
    initialize_memory(&as_master);
    end = rdtsc();
    printf("master thread: %lu cycles\n", end - start);

    start = rdtsc();

    nr_active_threads = NR_ALLOCATED_THREADS;
    nr_finished_threads = 0;
    thread_mutex_init(&m);
    s = 0;
    curr_core_nr = (my_core_id + 1) % nr_available_cores;

    uint64_t start_l2_cache_misses = rdpmc(0);
    for (int i = 0; i < NR_ALLOCATED_THREADS; i++) {
        err = domain_thread_create_on(curr_core_nr, initialize_memory, &as[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not create thread on core\n");
        }
        curr_core_nr = (curr_core_nr + 1) % nr_available_cores;
    }
    while (nr_active_threads != nr_finished_threads);
    uint64_t stop_l2_cache_misses = rdpmc(0);
    printf("Round 0: %lu Cache requests\n", stop_l2_cache_misses - start_l2_cache_misses);
    end = rdtsc();
    printf("round 0: %lu cycles\n", end - start);


    start = rdtsc();

    nr_active_threads = NR_ALLOCATED_THREADS;
    nr_finished_threads = 0;
    thread_mutex_init(&m);
    s = 0;

    curr_core_nr = (my_core_id + 1) % nr_available_cores;
    start_l2_cache_misses = rdpmc(0);
    for (int i = 0; i < NR_ALLOCATED_THREADS; i++) {
        err = domain_thread_create_on(curr_core_nr, initialize_number, &as[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not create thread on core\n");
        }
        curr_core_nr = (curr_core_nr + 1) % nr_available_cores;
    }
    while (nr_active_threads != nr_finished_threads);
    stop_l2_cache_misses = rdpmc(0);
    printf("Round 0: %lu Cache requests\n", stop_l2_cache_misses - start_l2_cache_misses);
    end = rdtsc();
    printf("round 0: %lu cycles\n", end - start);


    start = rdtsc();

    nr_active_threads = NR_ALLOCATED_THREADS;
    nr_finished_threads = 0;
    thread_mutex_init(&m);
    s = 0;

    curr_core_nr = (my_core_id + 1) % nr_available_cores;
    start_l2_cache_misses = rdpmc(0);
    for (int i = 0; i < NR_ALLOCATED_THREADS; i++) {
        err = domain_thread_create_on(curr_core_nr, sqr, &as[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not create thread on core\n");
        }
        curr_core_nr = (curr_core_nr + 1) % nr_available_cores;
    }
    while (nr_active_threads != nr_finished_threads);
    stop_l2_cache_misses = rdpmc(0);
    printf("Round 0: %lu Cache requests\n", stop_l2_cache_misses - start_l2_cache_misses);



    
    end = rdtsc();
    printf("Start = %lu, end = %lu, Time: %lu\n", start, end, end - start);

    struct waitset *ws = get_default_waitset();
//    while (nr_spanned_cores != nr_available_cores - 1) {
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
while(1);
}
