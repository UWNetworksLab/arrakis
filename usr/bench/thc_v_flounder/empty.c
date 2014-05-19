/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() */
#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <if/bench_defs.h>
#include <if/bench_thc.h>
#include <thc/thc.h>
#include <bench/bench.h>

/* ------------------------------ COMMON ------------------------------ */

// Real
#define ITERATIONS 10000
#define SKIP        9000

// Simulation
//#define ITERATIONS 100
//#define SKIP        90

#define NUM_CONFIG    13
#define NUM_REP        1

#define CONFIG_IS_THC_CLIENT(_x)      ((_x)&1)
#define CONFIG_IS_FLOUNDER_CLIENT(_x) (!((_x)&1))
#define CONFIG_IS_THC_SERVER(_x)      ((_x)&2)
#define CONFIG_IS_FLOUNDER_SERVER(_x) (!((_x)&2))
#define CONFIG_IS_SEQUENTIAL1(_x)     (((_x)&12) == 0)
#define CONFIG_IS_SEQUENTIAL2(_x)     (((_x)&12) == 4)
#define CONFIG_IS_PIPELINED2(_x)      (((_x)&12) == 8)
#define CONFIG_IS_PIPELINED1(_x)      (((_x)&12) == 12)

#define CONFIG_IS_PIPELINED(_x) (CONFIG_IS_PIPELINED1(_x) || CONFIG_IS_PIPELINED2(_x))
#define CONFIG_IS_1CHAN(_x)     (CONFIG_IS_SEQUENTIAL1(_x) || CONFIG_IS_PIPELINED1(_x))
#define CONFIG_IS_2CHAN(_x)     (CONFIG_IS_SEQUENTIAL2(_x) || CONFIG_IS_PIPELINED2(_x))

// Mode:Client<->Server
//
// s1 => sequential using 1 channel
// s2 => sequential using 2 channels
// p2 => pipelined using 2 channels
//
// . => Flounder      t => THC

static char *config_name[NUM_CONFIG] 
= {"s1:.<->.",
   "s1:t<->.",
   "s1:.<->t",
   "s1:t<->t",
   "s2:.<->.",
   "s2:t<->.",
   "s2:.<->t",
   "s2:t<->t",
   "p2:.<->.",
   "p2:t<->.",
   "p2:.<->t",
   "p2:t<->t",
   "p1:.<->."};
                    
static const char *my_service_name = "thc_v_flounder";
static const char *kind;
static int config;

// IDC bindings allocated at start of day.  We do not use these 
// directly: instead, we use these priv_b_* bindings to initialize 
// the b_* bindings.  This lets us switch between 1-channel and 
// 2-channel implementations.

static struct bench_binding *priv_b_c2s;
static struct bench_binding *priv_b_s2c;

// IDC bindings for use in direct-to-Flounder code.  
//
// c2s => binding used for sending messages from client to server
// s2c => binding used for sending messages from server to client

static struct bench_binding *b_c2s;
static struct bench_binding *b_s2c;

/* ------------------------------ CLIENT ------------------------------ */

static cycles_t timings[ITERATIONS];
static cycles_t total_timing;

// THC client code

static void client_thc(struct bench_thc_client_binding_t *cl) {
  assert(CONFIG_IS_THC_CLIENT(config));
  for (int i = 0; i < SKIP; i ++) {
    cycles_t start = bench_tsc();
    errval_t err = cl->send.fsb_empty_request(cl);
    assert(err_is_ok(err));
    err = cl->recv.fsb_empty_reply(cl);
    assert(err_is_ok(err));
    timings[i] = (bench_tsc() - start);
  }

  cycles_t loop_start = bench_tsc();
  
  for (int i = SKIP; i < ITERATIONS; i ++) {
    cycles_t start = bench_tsc();
    cl->send.fsb_empty_request(cl);
    cl->recv.fsb_empty_reply(cl);
    timings[i] = (bench_tsc() - start);
  }

  total_timing = bench_tsc()-loop_start;
}

static void client_thc_pipeline(struct bench_thc_client_binding_t *cl) {
  assert(CONFIG_IS_THC_CLIENT(config));
  cycles_t loop_start;

  DO_FINISH({
      ASYNC({
          for (int i = 0; i < SKIP; i++) {
            timings[i] = bench_tsc();
            cl->send.fsb_empty_request(cl);
          }

          loop_start = bench_tsc();

          for (int i = SKIP; i < ITERATIONS; i++) {
            timings[i] = bench_tsc();
            cl->send.fsb_empty_request(cl);
          }
        });

      for (int i = 0; i < SKIP; i ++) {
        cl->recv.fsb_empty_reply(cl);
        timings[i] = (bench_tsc() - timings[i]);
      }

      for (int i = SKIP; i < ITERATIONS; i ++) {
        cl->recv.fsb_empty_reply(cl);
        timings[i] = (bench_tsc() - timings[i]);
      }

    });

  total_timing = bench_tsc()-loop_start;
}

// Direct-to-flounder client code

static int tx_count; 
static int rx_count; 
static void *old_fn;
static thc_sem_t flounder_test_done_sem;

static void client_handler(struct bench_binding *b) {
  assert(CONFIG_IS_FLOUNDER_CLIENT(config));
  assert(!CONFIG_IS_PIPELINED(config));
  assert(b == b_s2c);
  timings[rx_count] = bench_tsc()-timings[rx_count];
  rx_count++;
  if (rx_count == ITERATIONS) {
    // Done
    total_timing = bench_tsc() - total_timing;
    b_s2c->rx_vtbl.fsb_empty_reply = old_fn;
    thc_sem_v(&flounder_test_done_sem);
  } else {
    // Send next request
    cycles_t t = bench_tsc();
    tx_count++;
    if (tx_count == SKIP) {
      total_timing = t;
    }
    timings[tx_count] = t;
    errval_t err = b_c2s->tx_vtbl.fsb_empty_request(b_c2s,
                                                    NOP_CONT);
    if (err_is_fail(err)) {
      DEBUG_ERR(err, "Client send failed\n");
      abort();
    }
  }
}

static void client_flounder(void) {
  assert(CONFIG_IS_FLOUNDER_CLIENT(config));
  assert(!CONFIG_IS_PIPELINED(config));
  old_fn = b_s2c->rx_vtbl.fsb_empty_reply;
  b_s2c->rx_vtbl.fsb_empty_reply = client_handler;
  tx_count = rx_count = 0;
  thc_sem_init(&flounder_test_done_sem, 0);

  // Send request
  timings[0] = total_timing = bench_tsc();
  errval_t err = b_c2s->tx_vtbl.fsb_empty_request(b_c2s,
                                                  NOP_CONT);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "Client send failed (sequential)\n");
    abort();
  }

  // Wait for callbacks to finish
  thc_sem_p(&flounder_test_done_sem);
}

static void client_tx_pipeline_handler(struct bench_binding *b) {
  assert(CONFIG_IS_FLOUNDER_CLIENT(config));
  assert(CONFIG_IS_PIPELINED(config));
  assert(b == b_c2s);
  while (tx_count < ITERATIONS) {
    errval_t err = b_c2s->tx_vtbl.fsb_empty_request(b_c2s, NOP_CONT);
    if (err == FLOUNDER_ERR_TX_BUSY) {
      err = b_c2s->register_send(b_c2s, 
                                 get_default_waitset(), 
                                 MKCONT((void(*)(void*))client_tx_pipeline_handler, 
                                        b));
      assert(err_is_ok(err));
      break;
    } else if (err_is_fail(err)) {
      DEBUG_ERR(err, "Client send failed (pipelined)\n");
      abort();
    }
    timings[tx_count] = bench_tsc();
    if (tx_count == SKIP) {
      total_timing = bench_tsc();
    }
    tx_count ++;
  }
}

static void client_rx_pipeline_handler(struct bench_binding *b) {
  assert(CONFIG_IS_FLOUNDER_CLIENT(config));
  assert(CONFIG_IS_PIPELINED(config));
  assert(b == b_s2c);
  timings[rx_count] = bench_tsc()-timings[rx_count];
  rx_count++;
  if (rx_count == ITERATIONS) {
    total_timing = bench_tsc() - total_timing;
    b_s2c->rx_vtbl.fsb_empty_reply = old_fn;
    thc_sem_v(&flounder_test_done_sem);
  }
}

static void client_flounder_pipeline(void) {
  assert(CONFIG_IS_FLOUNDER_CLIENT(config));
  assert(CONFIG_IS_PIPELINED(config));
  old_fn = b_s2c->rx_vtbl.fsb_empty_reply;
  b_s2c->rx_vtbl.fsb_empty_reply = client_rx_pipeline_handler;
  tx_count = rx_count = 0;
  thc_sem_init(&flounder_test_done_sem, 0);

  // Send as many requests as possible
  client_tx_pipeline_handler(b_c2s);

  // Wait for callbacks to finish
  thc_sem_p(&flounder_test_done_sem);
}

// Generic client harness

static int compar(const void *a, const void *b) {
  cycles_t ca = *(cycles_t*)a;
  cycles_t cb = *(cycles_t*)b;
  return ca-cb;
}

static void client_work(void) {
  errval_t err;

  debug_printf("connecting to service from core=%d\n", disp_get_core_id());
  
  err = bench_thc_connect_by_name(my_service_name,
                                  get_default_waitset(),
                                  IDC_BIND_FLAGS_DEFAULT,
                                  &priv_b_c2s);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "connect failed");
    abort();
  }

  debug_printf("              ... client done first connection\n");

  err = bench_thc_connect_by_name(my_service_name,
                                  get_default_waitset(),
                                  IDC_BIND_FLAGS_DEFAULT,
                                  &priv_b_s2c);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "connect failed");
    abort();
  }

  debug_printf("              ... client done second connection\n");

  struct bench_thc_client_binding_t cl;
  while (1) {
    for (config = 0; config < NUM_CONFIG; config ++) {
      for (int rep = 0; rep < NUM_REP; rep ++) {
        // Configure 1-channel v 2-channel as appropriate
        if (CONFIG_IS_1CHAN(config)) {
          err = bench_thc_init_client(&cl, priv_b_c2s, priv_b_c2s);
        } else {
          assert(CONFIG_IS_2CHAN(config));
          err = bench_thc_init_client(&cl, priv_b_c2s, priv_b_s2c);
        }
        assert(err_is_ok(err));
        b_c2s = cl._c2s_st;
        b_s2c = cl._s2c_st;
        
        // Run THC / flounder client
        if (CONFIG_IS_PIPELINED(config)) {
          if (CONFIG_IS_THC_CLIENT(config)) {
            client_thc_pipeline(&cl);
          } else {
            assert(CONFIG_IS_FLOUNDER_CLIENT(config));
            client_flounder_pipeline();
          }
        } else {
          if (CONFIG_IS_THC_CLIENT(config)) {
            client_thc(&cl);
          } else {
            assert(CONFIG_IS_FLOUNDER_CLIENT(config));
            client_flounder();
          }
        }
        
        // Calculate stats
        cycles_t total_of_timings = 0;
        for (int i = SKIP; i < ITERATIONS; i ++) {
          total_of_timings += timings[i];
        }
        qsort(&timings[SKIP], ITERATIONS-SKIP, sizeof(cycles_t), &compar);
        
        // Report stats
        cycles_t min = timings[SKIP];
        cycles_t pc5 = timings[SKIP+(5*(ITERATIONS-SKIP))/100];
        cycles_t pc50 = timings[SKIP+(ITERATIONS-SKIP) / 2];
        cycles_t pc95 = timings[SKIP+(95*(ITERATIONS-SKIP))/100];
        cycles_t max = timings[SKIP+(ITERATIONS-SKIP) - 1];
        cycles_t per = total_timing / (ITERATIONS-SKIP);
        
        if (max > 50 *pc50) {
          debug_printf("%2d %s ******\n", config, config_name[config]);
        } else {
          debug_printf("%2d %s min %5zd 5%%-ile %5zd median %5zd 95%%-ile %5zd max %5zd per %4zd\n",  
                       config, config_name[config], (size_t)min, (size_t)pc5, (size_t)pc50, (size_t)pc95, (size_t)max, (size_t)per);
        }
      }
    }
    THCDumpStats(1);
  }
}

/* ------------------------------ SERVER ------------------------------ */

// THC server code

static void server_thc(struct bench_thc_service_binding_t *sv) {
  assert(CONFIG_IS_THC_SERVER(config));
  for (int i = 0; i < ITERATIONS; i++) {
    errval_t err = sv->recv.fsb_empty_request(sv);
    assert(err_is_ok(err));
    err = sv->send.fsb_empty_reply(sv);
    assert(err_is_ok(err));
  }
}

// Direct-to-flounder server code
static int sends_due = 0;

static void server_tx_handler(struct bench_binding *b) {
  assert(CONFIG_IS_FLOUNDER_SERVER(config));
  assert(b == b_s2c);
  assert(sends_due > 0);

  while (sends_due > 0) {
    // Retry send to client
    errval_t err = b_s2c->tx_vtbl.fsb_empty_reply(b_s2c, 
                                                  NOP_CONT);
    if (err == FLOUNDER_ERR_TX_BUSY) {
      // Stalled again, register another callback
      err = b_s2c->register_send(b_s2c,
                                 get_default_waitset(),
                                 MKCONT((void(*)(void*))server_tx_handler, 
                                        b));
      assert(err_is_ok(err));
      return;
    }
    
    // Done send
    rx_count++;
    sends_due--;
  }

  // Wake the main server loop if we have sent the last reply
  assert(rx_count <= ITERATIONS);
  if (rx_count == ITERATIONS) {
    b_c2s->rx_vtbl.fsb_empty_request = old_fn;
    thc_sem_v(&flounder_test_done_sem);
  } 
}

static void server_handler(struct bench_binding *b) {
  assert(CONFIG_IS_FLOUNDER_SERVER(config));
  assert(b == b_c2s);

  if (sends_due > 0) {
    // Already registered a callback, increase the number of
    // messages we need to transmit
    sends_due ++;
  } else {
    // Try to send a reply directly
    errval_t err = b_s2c->tx_vtbl.fsb_empty_reply(b_s2c, 
                                                  NOP_CONT);
    if (err == FLOUNDER_ERR_TX_BUSY) {
      assert(sends_due == 0);
      sends_due = 1;
      err = b_s2c->register_send(b_s2c,
                                 get_default_waitset(),
                                 MKCONT((void(*)(void*))server_tx_handler, 
                                        b_s2c));
      assert(err_is_ok(err));
      return;
    }
    
    // Done send
    rx_count++;
    if (rx_count == ITERATIONS) {
      b_c2s->rx_vtbl.fsb_empty_request = old_fn;
      thc_sem_v(&flounder_test_done_sem);
    } 
  }
}

static void server_flounder(void) {
  assert(CONFIG_IS_FLOUNDER_SERVER(config));

  thc_sem_init(&flounder_test_done_sem, 0);
  old_fn = b_c2s->rx_vtbl.fsb_empty_request;

  // Initialize non-THC receive handler
  b_c2s->rx_vtbl.fsb_empty_request = server_handler;
  rx_count = 0;

  // Wait for callback-soup to finish
  thc_sem_p(&flounder_test_done_sem);
}

// Generic server harness

static void server_work(void) {
  struct bench_thc_export_info info;
  errval_t err;

  debug_printf("exporting service from core=%d\n", disp_get_core_id());
  err = bench_thc_export(&info,
                         my_service_name,
                         get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT,
                         NULL);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "export failed");
    abort();
  }

  debug_printf("exported service, waiting for client c2s connection\n");

  err = bench_thc_accept(&info, &priv_b_c2s);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "accept failed");
    abort();
  }

  debug_printf("              ... waiting for client s2c connection\n");

  err = bench_thc_accept(&info, &priv_b_s2c);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "accept failed");
    abort();
  }

  debug_printf("              ... done\n");

  struct bench_thc_service_binding_t sv;
  while (1) {
    for (config = 0; config < NUM_CONFIG; config ++) {
      for (int rep = 0; rep < NUM_REP; rep++) {
        // Configure 1-channel v 2-channel as appropriate
        if (CONFIG_IS_1CHAN(config)) {
          err = bench_thc_init_service(&sv, priv_b_c2s, priv_b_c2s);
        } else {
          assert(CONFIG_IS_2CHAN(config));
          err = bench_thc_init_service(&sv, priv_b_c2s, priv_b_s2c);
        }
        assert(err_is_ok(err));
        b_c2s = sv._c2s_st;
        b_s2c = sv._s2c_st;

        // Run THC / flounder server
        if (CONFIG_IS_THC_SERVER(config)) {
          server_thc(&sv);
        } else {
          assert(CONFIG_IS_FLOUNDER_SERVER(config));
          server_flounder();
        }
      }
    }
  }
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc, char *argv[])
{
  // Allow arbitrary early parameters (e.g., "boot" when invoked
  // directly from menu.lst by monitor)
  if (argc >= 2 && strcmp(argv[argc-1], "client") == 0) {
    kind = "client";
    client_work();
  } else if (argc >= 2 && strcmp(argv[argc-1], "server") == 0) {
    kind = "server";
    server_work();
  } else {
    debug_printf("Usage: %s ... client|server\n", argv[0]);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

