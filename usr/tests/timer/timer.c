/**
 * \file
 * \brief A simple test for checking if lpc_timer works
 * It tests periodic timers implemented by timer library.
 * test_A: When no arguments are given, then timer_test will run test_A
 * which starts two periodic timers and stops them after 100 callbacks
 * from each of them.
 *
 * test_B: When there are command line arguments given, then timer_test
 * will run test_B.  This test registers three periodic timer and stops
 * when 100 callbacks are received from all three timers.
 *
 * It is advised to run both test_A and test_B at same time.  It can be
 * done by inserting following lines into the menu.lst
 * module	/x86_64/sbin/lpc_timer core=1
 * module	/x86_64/sbin/timer_test core=2
 * module	/x86_64/sbin/timer_test core=3 B
 *
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <timer/timer.h>

/* Real life example */
//#define DHCP_FINE_TIMER 	20000000  /* client_1 callback_1 */
//#define DHCP_CORSE_TIMER 	60000000  /* client_1 callback_2 */

//#define TCP_INTERVAL 	250000 		/* client_2 callback_1 */
//#define RPC_INTERVAL 	5000  		/* client_2 callback_2 */
//#define CACHE_INTERVAL 	9000000		/* client_2 callback_3 */

#define SMALL_INTERVAL 		200000  /* client_1 callback_1 */
#define LARGE_INTERVAL 		700000  /* client_1 callback_2 */

#define MEDIAM_INTERVAL 	450000 		/* client_2 callback_1 */
#define SMALLEST_INTERVAL 	11000  		/* client_2 callback_2 */
#define LARGEST_INTERVAL 	900000		/* client_2 callback_3 */

#define MAX_LEN 512
struct client_data{
	struct timer *timer;
	char *client_name;
	char *timer_name;
	long interval;
	long counter;
	long limit;
	int is_stopped;
};

static int stopped_counter = 0;

//#define DHCP_FINE_TIMER 	200000  /* client_1 callback_1 */
//#define DHCP_CORSE_TIMER 	600000  /* client_1 callback_2 */


//#define TCP_INTERVAL 	250000 		/* client_2 callback_1 */
//#define RPC_INTERVAL 	5000  		/* client_2 callback_2 */
//#define CACHE_INTERVAL 	9000000		/* client_2 callback_3 */

static int l_debug = 0;

static int is_stopped(struct client_data *cl)
{
	if(l_debug) printf("is_stopped called %s -> %s %ld callback counter %ld\n",
			cl->client_name, cl->timer_name, cl->interval, cl->counter);

	if(cl->is_stopped){
		return cl->is_stopped;
	}

	if(cl->counter >= cl->limit){
		printf("Stopping %s -> %s %ld callback counter %ld reached it's limit %ld.\n",
				cl->client_name, cl->timer_name, cl->interval,
				cl->counter, cl->limit);
		timer_stop(cl->timer);
//		timer_destroy(cl->timer);
//		printf("### timer destroyed\n");
		cl->is_stopped = 1;
		++stopped_counter;
//		printf("#### %s: stop_counter = %d\n", cl->client_name, stopped_counter);
		return cl->is_stopped;
	}

	return cl->is_stopped;

} /* end function : is_running */

static void callback(struct timer *timer, void *data)
{
	struct client_data *cl = (struct client_data *)data;
	assert(cl != NULL);
	cl->counter = cl->counter + 1;

	if(l_debug) printf("%s -> %s %ld callback counter %ld\n",
    		cl->client_name, cl->timer_name, cl->interval, cl->counter);

} /* end function: callback */

static void setup_and_start_timer(struct client_data *ptr, char *c_name,
		char *t_name, long interval, long limit)
{
    ptr->client_name = c_name;
    ptr->timer_name = t_name;
    ptr->interval = interval;
    ptr->counter = 0;
    ptr->limit = limit;
    ptr->is_stopped = 0;
    ptr->timer = timer_create(interval, true, callback, ptr);
    timer_start(ptr->timer);
}

static void client_A(void)
{
    errval_t err;
    char client_name[512];
    snprintf(client_name, sizeof(client_name), "Client_A_%d",
    		disp_get_core_id());
    printf("%s: client A test running\n", client_name);

    err = timer_init();
    if (err_is_fail(err)) {
    	printf("%s: timer_init failed\n", client_name);
        USER_PANIC_ERR(err, "timer_init failed");
    }

    struct client_data for_t1;
    setup_and_start_timer(&for_t1, client_name, "SMALL_INTERVAL",
    		SMALL_INTERVAL, 100);

    struct client_data for_t2;
    setup_and_start_timer(&for_t2, client_name, "LARGE_INTERVAL",
    		LARGE_INTERVAL, 100);

    int r1, r2;
    while(1){
		r1 = is_stopped(&for_t1);
		r2 = is_stopped(&for_t2);
		if(r1 && r2){
			return;
		}
		messages_wait_and_handle_next();
	} /* end while: */
}


static void client_B(void)
{
    errval_t err;
    char client_name[512];
    snprintf(client_name, sizeof(client_name), "Client_B_%d",
    		disp_get_core_id());
    printf("%s: client B test running\n", client_name);

    err = timer_init();
    if (err_is_fail(err)) {
    	printf("%s: timer_init failed\n", client_name);
        USER_PANIC_ERR(err, "timer_init failed");
    }

    struct client_data for_t1;
    setup_and_start_timer(&for_t1, client_name,
    		"MEDIAM_INTERVAL", MEDIAM_INTERVAL, 100);

    struct client_data for_t2;
    setup_and_start_timer(&for_t2, client_name,
    		"SMALLEST_INTERVAL", SMALLEST_INTERVAL, 100);

    struct client_data for_t3;
    setup_and_start_timer(&for_t3, client_name,
    		"LARGEST_INTERVAL", LARGEST_INTERVAL, 100);

    int r1, r2, r3;
    while(1){

    	r1 = is_stopped(&for_t1);
		r2 = is_stopped(&for_t2);
		r3 = is_stopped(&for_t3);

		if(r1 && r2 && r3){
			return;
		}
		messages_wait_and_handle_next();
	} /* end while: */
}


int main(int argc, char *argv[])
{
    stopped_counter = 0;
	printf("For core %d, argc = %d\n", disp_get_core_id(), argc);
	if(argc == 1) {
		client_A();
		printf("Done with test for client_A\n");
	} else {
		client_B();
		printf("Done with test for client_B\n");
	}
	while(stopped_counter < 2){
		messages_wait_and_handle_next();
	}
    return 0;
}
