/** \file
 *  \brief Hello World application
 */

/*
 * Copyright (c) 2010, 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <arch/x86/barrelfish_kpi/perfmon.h>
#include <barrelfish/lmp_endpoints.h>

#define max(x,y) ((x) > (y) ? (x) : (y))

// Store result
// current size: 8 bytes
struct perfmon_data_buffer {
    uint64_t ip;
    char name[PERFMON_DISP_NAME_LEN];
};
#define PF_DATA_MAX 10000
// Name of the domain to measure (cropped to length 8)
#define PF_DOMAIN "monitor"
static struct perfmon_data_buffer pf_data[PF_DATA_MAX];
static uint64_t pf_data_ptr = 0;

// Endpoint for interrupt handlers
static struct capref epcap;
static struct lmp_endpoint *ep;

static uint64_t overflow_ctr;
static uint64_t max_outstanding;

/*
 * \brief Performance counter overflow handler
 *
 * Whenever the performance counter overflows the current instruction pointer
 * along with the name of the running task is send as payload to this endpoint.
 * The data is buffered. Therefore, this handler might be able to retrieve
 * several messages from the buffer as soon as it is scheduled.
 */
static void overflow_handler(void *args)
{
    errval_t err;
    uint64_t this_outstanding = 0;
    static int first_call = 4;

    // Retrieve the payload
    do {
        struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
        err = lmp_endpoint_recv(ep, &msg.buf, NULL);

        if (err_is_ok(err)) {

            this_outstanding++;

            struct perfmon_overflow_data data;
            memcpy(&data, &msg.words, sizeof(data));
            
            char res[PERFMON_DISP_NAME_LEN+1];
            memset(res, '\0', PERFMON_DISP_NAME_LEN+1);
            
            strncpy(res, data.name, PERFMON_DISP_NAME_LEN);

            overflow_ctr++;

            // Future work
            // domain id from dispatcher
            // spawnd to translate the id to name
            // see: ps in shell

            // Store if comming from sk_server task .. 
            if(pf_data_ptr<PF_DATA_MAX) {

                pf_data[pf_data_ptr] = (struct perfmon_data_buffer) { .ip = data.ip };
                memcpy(pf_data[pf_data_ptr].name, res, PERFMON_DISP_NAME_LEN);
                /* printf("PerfMon: %" PRIx64 " %.*s\n",  */
                /*        pf_data[pf_data_ptr].ip, (int)PERFMON_DISP_NAME_LEN, pf_data[pf_data_ptr].name); */
                pf_data_ptr++;

                /* printf("pf_data_ptr = %" PRIu64 "\n", pf_data_ptr); */

                if(pf_data_ptr==PF_DATA_MAX) {

                    // Stop performance monitoring
                    invoke_perfmon_deactivate(cap_perfmon);

                    if(first_call > 0) {
                        first_call--;
                        printf("Countdown: %u. Go start your experiment!\n", first_call);
                    } else {
                        // Print result .. 
                        printf("Perfmon Overflow data START\n");
                        for(uint64_t i=0; i<PF_DATA_MAX; i++) {
                            printf("PerfMon: %" PRIx64 " %.*s\n", 
                                   pf_data[i].ip, (int)PERFMON_DISP_NAME_LEN, pf_data[i].name);
                        }
                        // Don't change this line, as that will break 
                        // the harness test "perfmontest"
                        printf("Perfmon Overflow data END\n");
                    }

                    err = invoke_perfmon_activate(cap_perfmon,
                                                  0x3c,   // Event to monitor
                                                  0,   // UMASK
                                                  false,      // Kernel
                                                  0,      // Counter ID
                                                  0, // number of events to cause overflow
                                                  get_cap_addr(epcap));
                    assert(err_is_ok(err));
                    pf_data_ptr = 0;
                    overflow_ctr = 0;
                    max_outstanding = 0;
                }
            }

        } else if (err_no(err) == LIB_ERR_NO_LMP_MSG) {

            max_outstanding = max(max_outstanding, this_outstanding);

        } else {

            DEBUG_ERR(err, "Cannot read payload in performance counter "
                      "overflow \n");
        }

    } while(err_is_ok(err));

    // Need to re-register .. 
    struct event_closure cl = {
        .handler = overflow_handler
    };
    err = lmp_endpoint_register(ep, get_default_waitset(), cl);
    assert(err_is_ok(err));
    
}

/*
 * \brief Initialize endpoint for performance monitor overflow notifications.
 */
static void interrupt_endpoint_init(void)
{
    errval_t err;

    /* Register notification endpoint with kernel */
    err = endpoint_create(250 * 12, &epcap, &ep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed creating endpoint");
    }

    // Register endpoint. This should set up the endpoint.
    struct event_closure cl = {
        .handler = overflow_handler
    };
    err = lmp_endpoint_register(ep, get_default_waitset(), cl);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[]) 
{
    errval_t err;
    overflow_ctr = 0;
    max_outstanding = 0;

    // Setup the endpoint for performance counter overflows
    interrupt_endpoint_init();

    // Starting performance monitoring.
    // Assuming, that the capability has been given to us by init>monitor>spawn
    err = invoke_perfmon_activate(cap_perfmon,
                                  0x3c,   // Event to monitor
                                  0,   // UMASK
                                  false,      // Kernel
                                  0,      // Counter ID
                                  0, // number of events to cause overflow
                                  get_cap_addr(epcap));
                                  /* 120000, // number of events to cause overflow */
    // SAMPLE events to monitor for recent AMD machines.
    // 0x7e  L2 cache misses
    // 0x76  CPU clocks not halt
    // SAMPLE events to monitor for Intel machines.
    // 0x3C  Unhalted core cycles (umask = 0)

    assert(err_is_ok(err));
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "invoke_perfmon_activate");
    }

    // Wait for overflows
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Performance counter overflow handler died");
            break;
        }
    }

    // do never terminate
    assert(false);
}

