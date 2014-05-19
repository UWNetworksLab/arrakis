/** \file
 *  \brief Example lwip socket application
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <lwip/init.h>
#include <lwip/tcpip.h>
#include <sys/socket.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/spawn_client.h>
#include <posixcompat.h>
#include <vfs/vfs.h>

#define NET_DRIVER "rtl8029"
#define SERVER_PORT 8080
#define LISTENQ 1024

//#define UDP_TEST 1

void network_polling_loop(void);

static int poll_loop(void *args)
{
    network_polling_loop();
    return 0;
}

static errval_t spawn_child(int rfd)

{
    errval_t err;
    char *argv[2] = { "net-test", NULL };
    domainid_t new_domain = -1;
    coreid_t core = 0;
    struct capref fdcap;

    err = spawn_setup_fds(&fdcap, rfd);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawn_setup_fds");
    }

    struct capref inheritcn_cap;
    err = alloc_inheritcn_with_fdcap(&inheritcn_cap, fdcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to setup inheritcn");
    }

    err = spawn_program_with_caps(core, argv[0], argv, NULL, inheritcn_cap,
                                  NULL_CAP, SPAWN_NEW_DOMAIN, &new_domain);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawn on core %d", core);
        return err;
    }

    return SYS_ERR_OK;

}

#ifndef UDP_TEST
static void debug_uipaddr_print(u32_t addr)
{

    debug_printf("%"U16_F".%"U16_F".%"U16_F".%"U16_F"\n",
                 (u16_t)((ntohl(addr) >> 24) & 0xff),
                 (u16_t)((ntohl(addr) >> 16) & 0xff),
                 (u16_t)((ntohl(addr) >> 8) & 0xff),
                 (u16_t)(ntohl(addr) & 0xff));
}


static void debug_sin_print(struct sockaddr_in *inaddr) {
    u32_t addr = inaddr->sin_addr.s_addr;
    debug_uipaddr_print(addr);
}
#endif // UDP_TEST



#define BUF_SIZE 1024
static int FORKING_SERVER = 1;

/* Following variables are needed for networking */
static struct thread_mutex my_mutex = THREAD_MUTEX_INITIALIZER;
static struct waitset lwip_waitset;
static struct thread *t;

static void provide_echo_service(int sock, int iterations)
{
    // and echo incoming data
    char buf[BUF_SIZE];
    int ret;

    debug_printf("[%"PRIuDOMAINID"]provide_echo_service(): Starting %d iterations on socket %d\n",
            disp_get_domain_id(), iterations, sock);


    for(int i = 0; i < iterations; ++i) {
        debug_printf("[%"PRIuDOMAINID"]provide_echo_service(%d): calling read on the socket %d\n",
                disp_get_domain_id(), i, sock);

        ret = read(sock, buf, BUF_SIZE);
        if(ret < 0) {
            printf("[%"PRIuDOMAINID"]provide_echo_service(%d): ERROR: read failed\n",
                    disp_get_domain_id(), i);
            return;
        }

        debug_printf("[%"PRIuDOMAINID"]provide_echo_service(%d): read %d bytes (%s)\n",
                disp_get_domain_id(), i, ret, buf);
        ret = write(sock, buf, ret);
        debug_printf("[%"PRIuDOMAINID"]provide_echo_service(%d): sent %d bytes\n",
                disp_get_domain_id(), i, ret);

        if(ret < 0) {
            printf("[%"PRIuDOMAINID"]provide_echo_service(%d): ERROR: write failed\n",
                    disp_get_domain_id(), i);
            return;
        }
    } // end for: for each iteration
    debug_printf("[%"PRIuDOMAINID"]provide_echo_service(): Done with %d iterations on socket %d\n",
            disp_get_domain_id(), iterations, sock);

} // end function: provide_echo_service


static void network_setup_helper(void)
{
    debug_printf("[%"PRIuDOMAINID"]network_setup_helper():###### init lwip\n",
            disp_get_domain_id());

    waitset_init(&lwip_waitset);
    thread_mutex_lock(&my_mutex);
    // FIXME: replaced lwip_init_auto_ex with lwip_init_auto without testing
//    lwip_init_auto_ex(&lwip_waitset, &my_mutex);

    // lwip_init_auto();
    tcpip_init(NULL, NULL); // FIXME: serious problem: why is it called after
                        // calling lwip_init_auto? henc lwip_init_auto is
                        // commented out (without testing!!!)
    lwip_socket_init();
    thread_mutex_unlock(&my_mutex);

    debug_printf("[%"PRIuDOMAINID"]network_setup_helper():######### lwip initialized\n",
            disp_get_domain_id());

    // start an event dispatch thread

    t = thread_create(poll_loop, NULL);
    assert(t != NULL);

    debug_printf("[%"PRIuDOMAINID"]network_setup_helper():######### polling thread created\n",
            disp_get_domain_id());

} // end function: network_setup_helper

static void do_server(void)
{
    errval_t err;

    int l_sock; // for listening
    int c_sock; // for a connection
    short int port = SERVER_PORT;
    struct sockaddr_in servaddr;
#ifndef UDP_TEST
    struct sockaddr_in conn_addr;
    socklen_t conn_addrlen = sizeof(conn_addr);
#endif // UDP_TEST

    debug_printf("[%"PRIuDOMAINID"]do_server(): Server started\n", disp_get_domain_id());
    network_setup_helper();

    debug_printf("[%"PRIuDOMAINID"]do_server(): server going to init lwip\n",
            disp_get_domain_id());

#ifndef UDP_TEST
    // setup a server socket (tcp)
    l_sock = socket(AF_INET, SOCK_STREAM, 0);
#else
    l_sock = socket(AF_INET, SOCK_DGRAM, 0);
#endif // UDP_TEST

    if (l_sock < 0) {
        debug_printf("[%"PRIuDOMAINID"]do_server(): socket failed: %d errno: %d\n",
                disp_get_domain_id(), l_sock, errno);
        exit(EXIT_FAILURE);
    }

    debug_printf("[%"PRIuDOMAINID"]do_server(): listen socket created %d\n",
            disp_get_domain_id(), l_sock);

    // bind
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port        = htons(port);

    if (bind(l_sock, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0 ) {
        debug_printf("[%"PRIuDOMAINID"]do_server(): Error calling bind() errno: %d\n",
                disp_get_domain_id(), errno);
        exit(EXIT_FAILURE);
    }

    debug_printf("[%"PRIuDOMAINID"]do_server(): ^^^^^^^^  listen socket bound to %d, now starting listen\n",
            disp_get_domain_id(), port);


#ifndef UDP_TEST
    // listen
    if (listen(l_sock, LISTENQ) < 0 ) {
        debug_printf("[%"PRIuDOMAINID"]do_server(): Error calling listen() errno: %d\n",
                disp_get_domain_id(), errno);
        exit(EXIT_FAILURE);
    }

    debug_printf("[%"PRIuDOMAINID"]do_server(): listen socket listening\n",
            disp_get_domain_id());

    debug_printf("[%"PRIuDOMAINID"]do_server(): ^^^^^^^^^^^^^^^^^^^ calling accept on listen socket\n",
            disp_get_domain_id());


    while(1) {
        // wait for connections
        //    c_sock = lwip_accept(l_sock, NULL, NULL);
        c_sock = accept(l_sock, (struct sockaddr*)&conn_addr, &conn_addrlen);
        if (c_sock < 0) {
            debug_printf("[%"PRIuDOMAINID"]do_server(): Error calling accept() errno: %d\n",
                    disp_get_domain_id(), errno);
            exit(EXIT_FAILURE);
        }

        debug_printf("[%"PRIuDOMAINID"]do_server(): ^^^^^^^^^^^^^^^^^^^ accepted a connection on socket %d\n",
                disp_get_domain_id(), c_sock);
        debug_printf("[%"PRIuDOMAINID"]do_server(): c_sock connected to: \n",
                disp_get_domain_id());
        debug_sin_print(&conn_addr);
        debug_printf("[%"PRIuDOMAINID"]do_server(): port: %u\n",
                disp_get_domain_id(), ntohs(conn_addr.sin_port));
    #else
        c_sock = l_sock;
    #endif // UDP_TEST

        // now echo incoming data
        //        provide_echo_service(c_sock, 1);

        if(FORKING_SERVER) {
            debug_printf("[%"PRIuDOMAINID"]do_server(): ^^^^^^^^^^^^^^^^ ##### server_1: Forking\n",
                    disp_get_domain_id());
            err = spawn_child(c_sock);
            if (err_is_fail(err)) {
                debug_printf("[%"PRIuDOMAINID"]do_server(): failed to spawn child\n",
                        disp_get_domain_id());
                return;
            }
            debug_printf("[%"PRIuDOMAINID"]do_server(): ^^^^^^^^^^ ################ child spawned\n",
                    disp_get_domain_id());
        } // end if: FORKING_SERVER


        debug_printf("[%"PRIuDOMAINID"]do_server(): closing connected socket %d\n",
                disp_get_domain_id(), c_sock);
        close(c_sock);


    } // end while : infinite

    // basically wait forever
    thread_join(t, NULL);
} // end function: do_server


static void do_child(void)
{
    /* errval_t err; */
    /* struct lwip_sockinfo si; */
    debug_printf("[%"PRIuDOMAINID"]do_child():############# child  ###############\n",
            disp_get_domain_id());

    network_setup_helper(); // perform the basic network setup
    posixcompat_unpack_fds(); // deserialize moved socket info into new sockets

    debug_printf("[%"PRIuDOMAINID"]do_child():################ starting echo service\n",
            disp_get_domain_id());

    int c_sock = 4; // Socket which is a copy of accepted socket
    provide_echo_service(c_sock, 10);

    // basically wait forever
    debug_printf("[%"PRIuDOMAINID"]do_child():####### waiting to join thread\n",
                disp_get_domain_id());
    thread_join(t, NULL);
} // end function : do_child



int main(int argc, char *argv[])
{
    coreid_t mycore = disp_get_core_id();

    vfs_init();

    debug_printf("[%"PRIuDOMAINID"]main(): This is %s on core %d with %d args\n",
            disp_get_domain_id(), argv[0], mycore, argc);

    if (argc > 1) {
        do_server();
    } else {
        do_child();
    }

    debug_printf("[%"PRIuDOMAINID"]main(): Finished with everything, Exiting...\n",
            disp_get_domain_id());

    return EXIT_SUCCESS;
} // end function : main
