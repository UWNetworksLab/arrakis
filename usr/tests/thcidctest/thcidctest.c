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
#include <if/ping_pong_defs.h>
#include <if/ping_pong_thc.h>
#include <thc/thc.h>

#define ITERATIONS    4
#define CANCEL_ITERS 32

/* ------------------------------ COMMON ------------------------------ */

static const char *my_service_name = "idctest";
static const char *kind;

/* ------------------------------ CLIENT ------------------------------ */


static void test_ooo_rpc(struct ping_pong_thc_client_binding_t *cl,
                         uint64_t val) {
  uint64_t result_val;
  cl->call.outoforder(cl, val, &result_val);
  fprintf(stderr, "   (%d, %d)\n", (int)val, (int)result_val);
}

static void do_nx(struct ping_pong_thc_client_binding_t *cl,
                  int i) {
  DO_FINISH_NX({
      uint64_t response;
      if (cl->call_x.outoforder(cl, i, &response) != THC_CANCELED) {
        fprintf(stderr, "   %d -> %d\n", (int)i, (int)response);
        assert(response == i * 10);
      } else {
        fprintf(stderr, "   URK! CANCELED RPC for %d\n", (int)i);
      }
    });
};

static void client_work(void) {
    errval_t err;
    uint64_t val;
    uint64_t result_val;
    struct ping_pong_thc_client_binding_t cl;
    struct ping_pong_binding *b;
    err = ping_pong_thc_connect_by_name(my_service_name,
                                        get_default_waitset(),
                                        IDC_BIND_FLAGS_DEFAULT,
                                        &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect failed");
        abort();
    }

    err = ping_pong_thc_init_client(&cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
    }

    fprintf(stderr, "Starting string/array tests\n");
    for (int i = 0; i < 10; i ++) {
      const char *s = strdup("foo");
      uint64_t len = 0, total=0;
      fprintf(stderr, "  Sending string '%s'\n", s);
      cl.send.str0(&cl, 42, s);
      fprintf(stderr, "  Sent string\n");
      free((void*)s);
      cl.recv.pong(&cl, &len);
      fprintf(stderr, "  Reply was '%d'\n", (int)len);
      fprintf(stderr, "  Sending array ['a', 'b', 'c']\n");
      cl.send.arr0(&cl, 42, "abc", 3);
      fprintf(stderr, "  Sent array\n");
      cl.recv.pong(&cl, &total);
      fprintf(stderr, "  Reply was '%d'\n\n", (int)total);
    }

    fprintf(stderr, "Starting basic tests\n");

    DO_FINISH({
      // Asynchronously, invoke a slower operation
      ASYNC({
        uint64_t v = 0;
        fprintf(stderr, " . . . . . . . . . . . s l o w  . . . . ->\n");
        cl.send.slow_op(&cl, 100);
        cl.recv.slow_reply(&cl, &v);
        fprintf(stderr, " s l o w = %d\n", (int)v);
        });
      
      // Do a series of short ping-pong messages
      fprintf(stderr, "Doing non-RPC test:\n");
      val = 0;
      do {
        fprintf(stderr, "    --- %d -->\n", (int)val);
        cl.send.ping(&cl, val);
        cl.recv.pong(&cl, &val);
      } while (val < ITERATIONS);
      
      // Do a further series of ping-pong messages via RPC,
      // invoking the underlying send operation directly
      fprintf(stderr, "Doing RPC test using explicit messages:\n");
      val = 100;
      do {
        cl.send.testrpc(&cl, val);
        cl.recv.testrpc(&cl, &result_val);
        fprintf(stderr, "   (%d, %d)\n", (int)val, (int)result_val);
        val++;
      } while (val < 100 + ITERATIONS);
      
      // Do a series of ping-pong messages via RPC
      fprintf(stderr, "Doing RPC test:\n");
      val = 200;
      do {
        cl.call_seq.testrpc(&cl, val, &result_val);
        fprintf(stderr, "   (%d, %d)\n", (int)val, (int)result_val);
        val++;
      } while (val < 200 + ITERATIONS);
      
      // Do a series of ping-pong messages via FIFO RPC
      fprintf(stderr, "Doing FIFO-RPC test:\n");
      DO_FINISH({
        for (int v = 300; v < 300 + ITERATIONS; v ++) {
          ASYNC({
              int my_v = v;
              uint64_t r = 0;
              printf("before fifo: %d\n", my_v);
              cl.call_fifo.testrpc(&cl, my_v, &r);
              fprintf(stderr, "   (%d, %d)\n", (int)my_v, (int)r);
            });
        }
        });

      // Do a series of ping-pong messages via out-of-order RPC.
      // The computation time is proportional to the value, so
      // we should get the results returning in numerical order.
      fprintf(stderr, "Doing OOO RPC test:\n");
      val = 400;
      DO_FINISH({
          ASYNC({test_ooo_rpc(&cl, 10);});
          ASYNC({test_ooo_rpc(&cl, 50);});
          ASYNC({test_ooo_rpc(&cl, 20);});
          ASYNC({test_ooo_rpc(&cl, 40);});
          ASYNC({test_ooo_rpc(&cl, 30);});
          ASYNC({test_ooo_rpc(&cl, 50);});
          ASYNC({test_ooo_rpc(&cl, 10);});
        });
      
      fprintf(stderr, "Testing cancellation: cancelling never-received message\n");

      // Simple test, cancel a receive operation on a message we 
      // will never get
      DO_FINISH_(cb1, {
          ASYNC({
              fprintf(stderr,
                      "   Starting receive...\n");
              int r = cl.recv_x.pong(&cl, &val); 
              fprintf(stderr,
                      "   Return val %s\n", (r==THC_CANCELED) ? "CANCELLED" : "???");
            });
          fprintf(stderr, "   Cancelling receive...\n");
          CANCEL(cb1);
        });

      // Simple test, cancel a receive-any operation on a message we 
      // will never get
      DO_FINISH_(cb2,{
          ASYNC({
              fprintf(stderr,
                      "   Starting receive-any...\n");
              ping_pong_client_msg_t m;
              int r = cl.recv_any_x(&cl, &m,
                                    (struct ping_pong_client_selector) {
                                      .pong=1, .testrpc=1, .testrpc2=1});
              fprintf(stderr,
                      "   Return val %s\n", (r==THC_CANCELED) ? "CANCELLED" : "???");
            });
          fprintf(stderr, "   Cancelling receive-any...\n");
          CANCEL(cb2);
        });

      fprintf(stderr, "Testing cancellation: cancelling a send that blocks\n");

      DO_FINISH({
        // Send FIFO RPC requests as fast as possible, cancel as soon as one
        // blocks
        int num_sent = 0;
        DO_FINISH_(cb3, {
            ASYNC({
                while (cl.send_x.testrpc(&cl, 100+num_sent) == SYS_ERR_OK) {
                  num_sent++;
                }
              });
            fprintf(stderr, "   Cancelling after %d sent\n", num_sent);
            CANCEL(cb3);
          });
        fprintf(stderr, "   Cancel done, %d sent\n", num_sent);
        for (int i = 0; i < num_sent; i ++) {
          uint64_t response;
          cl.recv.testrpc(&cl, &response);
          fprintf(stderr, "   Got %d\n", (int)response);
        }
        fprintf(stderr, "   Cancel done, %d received\n", num_sent);
        });

      // Try to receive one more... we don't expect another response,
      // but this will detect bugs if a duplicate arrives
      DO_FINISH_(cb4, {
          ASYNC({
              uint64_t response;
              int r = cl.recv_x.testrpc(&cl, &response);
              assert(r == THC_CANCELED);
            });
          THCYield();
          THCYield();
          THCYield();
          THCYield();
          THCYield();
          THCYield();
          fprintf(stderr, "   Good: no stray responses\n");
          CANCEL(cb4);
        });

      // Send a stream of testrpc and testrpc2 requests.
      //
      // Try to receive responses, cancelling some of the receives.  
      // Check that we get back the correct number of responses, still 
      // in sequence.
      fprintf(stderr, "Testing cancellation: large numbers of cancellation attempts\n");
      DO_FINISH({
        ASYNC({ 
            for (int v = 1000; v < 1000 + CANCEL_ITERS; v++ ) {
              cl.send.testrpc(&cl, v);
            }
          });
        ASYNC({ 
            for (int v = 1000; v < 1000 + CANCEL_ITERS; v++ ) {
              cl.send.testrpc2(&cl, v);
            }
          });
        
        int v = 1000;
        while (v < 1000 + (CANCEL_ITERS*2)) {
          DO_FINISH_(cb5, {
              // Start two concurrent attempts to receive RPC responses
              ASYNC({
                  uint64_t response;
                  if (cl.recv_x.testrpc(&cl, &response) != THC_CANCELED) {
                    fprintf(stderr, "   recv %d\n", (int)response);
                    v++;
                  }
                  CANCEL(cb5);
                });
              ASYNC({
                  uint64_t response;
                  if (cl.recv_x.testrpc2(&cl, &response) != THC_CANCELED) {
                    fprintf(stderr, "                  recv %d\n", (int)response);
                  v++;
                  }
                  CANCEL(cb5);
                });
            });
        }
        });
      
      // Send a series of testrpc calls, cancelling some of them.  Check
      // that the calls and responses keep matching up
      fprintf(stderr, "Testing cancellation of sequential RPC\n");
      for (int i = 0; i < CANCEL_ITERS; i ++) {
        DO_FINISH_(cb6, {
            thc_sem_t sem;
            thc_sem_init(&sem, 0);
            ASYNC({
                uint64_t response;
                if (cl.call_seq_x.testrpc(&cl, i, &response) != THC_CANCELED) {
                  fprintf(stderr, "   %d -> %d\n", (int)i, (int)response);
                  assert(response == i * 10);
                } else {
                  fprintf(stderr, "   CANCELED RPC for %d\n", (int)i);
                }
                thc_sem_v(&sem);
              });
            if ((i % 5) == 0) {
              thc_sem_p(&sem);
            }
            CANCEL(cb6);
          });
      }   
      
      // Send a series of FIFO-RPC calls, cancelling some of them.  Check
      // that the calls and responses keep matching up
      fprintf(stderr, "Testing cancellation of FIFO RPC\n");
      DO_FINISH({
          for (int i = 0; i < CANCEL_ITERS; i ++) {
            ASYNC({
                int my_i = i;
                DO_FINISH_(cb7, {
                    thc_sem_t sem;
                    thc_sem_init(&sem, 0);
                    ASYNC({
                        uint64_t response;
                        if (cl.call_fifo_x.testrpc(&cl, my_i, &response) != THC_CANCELED) {
                          fprintf(stderr, "   %d -> %d\n", (int)my_i, (int)response);
                          assert(response == my_i * 10);
                        } else {
                          fprintf(stderr, "   CANCELED RPC for %d\n", (int)my_i);
                        }
                        thc_sem_v(&sem);
                      });
                    if ((my_i % 5) == 0) {
                      thc_sem_p(&sem);
                    }
                    CANCEL(cb7);
                  });
              });
          }     
        }); 

      // Send a series of OOO-RPC calls, cancelling some of them.  Check
      // that the calls and responses keep matching up
      fprintf(stderr, "Testing cancellation of OOO RPC\n");
      DO_FINISH({
        for (int i = 0; i < CANCEL_ITERS; i ++) {
          ASYNC({
              int my_i = i;
              DO_FINISH_(cb8, {
                  thc_sem_t sem;
                  thc_sem_init(&sem, 0);
                  ASYNC({
                      uint64_t response;
                      if (cl.call_x.outoforder(&cl, my_i, &response) != THC_CANCELED) {
                        fprintf(stderr, "   %d -> %d\n", (int)my_i, (int)response);
                        assert(response == my_i * 10);
                      } else {
                        fprintf(stderr, "   CANCELED RPC for %d\n", (int)i);
                      }
                      thc_sem_v(&sem);
                    });
                  if ((my_i % 5) == 0) {
                    thc_sem_p(&sem);
                  }
                  CANCEL(cb8);
                });
            });
        }     
        });


      // Send a series of OOO-RPC calls, trying to cancel them all, but using 
      // non-cancellable funtcions.
      fprintf(stderr, "Testing cancellation of non-cancellable OOO RPC\n");
      DO_FINISH({
        for (int i = 0; i < 5; i ++) {
          ASYNC({
              int my_i = i;
              DO_FINISH_(cb9, {
                  ASYNC({do_nx(&cl, my_i);});
                  CANCEL(cb9);
                });
            });
        }     
        });
      
      // Send shutdown request to server
      fprintf(stderr, "Finished tests: sending stop request\n");
      cl.send.stop(&cl);
    });
}

/* ------------------------------ SERVER ------------------------------ */

static volatile int x = 0;

static void test_service_ping(struct ping_pong_thc_service_binding_t *sv,
                              uint64_t val) {
  val++;
  printf("    <-- %d ---\n", (int)val);
  sv->send.pong(sv, val);
}

static void test_service_str0(struct ping_pong_thc_service_binding_t *sv,
			      uint64_t arg,
			      char *str) {
  int len = strlen(str);
  printf("    (Got arg %d string %s length %d)\n", (int)arg, str, len);
  free(str);
  sv->send.pong(sv, len);
}

static void test_service_arr0(struct ping_pong_thc_service_binding_t *sv,
			      const char *arr,
			      size_t len) {
  int total = 0;
  printf("    (Got array at %p, len %d)\n", arr, (int)len);
  for (size_t i = 0; i < len; i++) {
    printf("        %d\n", arr[i]);
    total += arr[i];
  }
  free((void *)arr);
  sv->send.pong(sv, total);
}

static void test_service_rpc(struct ping_pong_thc_service_binding_t *sv,
                             uint64_t val) {
  sv->send.testrpc(sv, val*10);
}

static void test_service_rpc2(struct ping_pong_thc_service_binding_t *sv,
                             uint64_t val) {
  sv->send.testrpc2(sv, val*10);
}

static void test_service_outoforder(struct ping_pong_thc_service_binding_t *sv,
                                    uint64_t seq,
                                    uint64_t val) {
  for (int j = 0; j < val; j ++) {
    for (int i = 0; i < 1000000; i ++) {
      x++;
    }
    THCYield();
  }
  sv->send.outoforder(sv, seq, val*10);
}

static void test_service_slow_reply(struct ping_pong_thc_service_binding_t *sv,
                                    uint64_t val) {
  val++;
  for (int j = 0; j < 5; j ++) {
    for (int i = 0; i < 1000000; i ++) {
      x++;
    }
    THCYield();
  }
  printf("    < - - %d  - - - . . . . . . . . . . . \n", (int)val);
  sv->send.slow_reply(sv, val);
}

static void service_client(struct ping_pong_thc_service_binding_t *sv) {
  DO_FINISH({
      bool stop = false;
      while (!stop) {
        ping_pong_service_msg_t m;
        sv->recv_any(sv, &m, (struct ping_pong_service_selector) {
            .ping=1, .stop=1, .testrpc=1, .testrpc2=1, .outoforder=1, .slow_op=1, .str0=1, .arr0=1});
        switch (m.msg) {
        case ping_pong_slow_op:
          ASYNC({test_service_slow_reply(sv, m.args.slow_op.val);});
          break;
          
        case ping_pong_ping: 
          ASYNC({test_service_ping(sv, m.args.ping.val);});
          break;
          
        case ping_pong_testrpc:
          test_service_rpc(sv, m.args.testrpc.in.testin); // Sync: ensure FIFO
          break;
          
        case ping_pong_testrpc2:
          test_service_rpc2(sv, m.args.testrpc.in.testin); // Sync: ensure FIFO
          break;
          
        case ping_pong_outoforder:
          ASYNC({test_service_outoforder(sv,
                                         m.args.outoforder.in.seq_in,
                                         m.args.outoforder.in.testin);});
          break;

	case ping_pong_str0:
	  test_service_str0(sv, 
			    m.args.str0.arg1,
			    m.args.str0.s);
	  break;
		    
	case ping_pong_arr0:
	  test_service_arr0(sv,
			    m.args.arr0.a,
			    m.args.arr0.l);
	  break;
		    
        
        case ping_pong_stop:
          fprintf(stderr, "Service: stopping\n");
          stop = 1;
        break;
        
        default:
          assert(0 && "Unexpected message");
          break;
        }
      }
    });
}

static void server_work(void) {
  struct ping_pong_thc_service_binding_t *sv;
  struct ping_pong_binding *b;
  struct ping_pong_thc_export_info info;
  errval_t err;
  iref_t iref;

  printf("Starting server_work\n");
  err = ping_pong_thc_export(&info,
                             my_service_name,
                             get_default_waitset(),
                             IDC_EXPORT_FLAGS_DEFAULT,
                             &iref);
  printf("Done export iref=%"PRIuIREF"\n", iref);
  if (err_is_fail(err)) {
    DEBUG_ERR(err, "export failed");
    abort();
  }

  DO_FINISH({
    while (1) {
      printf("server waiting for connection\n");
      err = ping_pong_thc_accept(&info, &b);
      if (err_is_fail(err)) {
        DEBUG_ERR(err, "accept failed");
        abort();
      }

      sv = malloc(sizeof(struct ping_pong_thc_service_binding_t));
      if (sv == NULL) {
        DEBUG_ERR(err, "malloc failed");
        abort();
      }

      err = ping_pong_thc_init_service(sv, b, b);
      if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
      }

      printf("Got service %p\n", sv);
      ASYNC({service_client(sv);});
    }
    });
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
      printf("Usage: %s ... client|server\n", argv[0]);
      return EXIT_FAILURE;
    }

    printf("%s %s DONE!\n", argv[0], kind);
    return EXIT_SUCCESS;
}

