/**
 * \file
 * \brief Simple Server to test Barrelfish's implementation of and conformance
 *        to the BSD/POSIX sockets API.
 *
 * The server waits until it receives BUFFER_SIZE characters from the client
 * and then echos these back to the client, waits again, ...
 *
 * The code it written so that it compiles on Barrelfish as well as on a
 * POSIX system.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>

#ifdef BARRELFISH
# include <barrelfish/barrelfish.h>
# include <lwip/tcpip.h>
#endif /* BARRELFISH */

#define DEFAULT_PORT        4242
#define BACKLOG             10
#define BUFFER_SIZE         32

#define	MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))

enum client_state {
    READING,
    WRITING,
    FINISHED
};

struct client {
    int socket;
    char buffer[BUFFER_SIZE + 1];
    int buffer_index;
    enum client_state state;
    struct client *next;
};

struct client *clients = NULL;

#ifdef BARRELFISH
extern void network_polling_loop(void);

static int poll_loop(void *args)
{
    network_polling_loop();

    // should never be reached
    return EXIT_FAILURE;
}
#endif /* BARRELFISH */

static void start_server(uint16_t port, int *serversocket)
{
    int ret = 0;
    int listenfd = 0;
    struct sockaddr_in listen_addr;

    listenfd = socket(AF_INET, SOCK_STREAM, 0);
    if (listenfd < 0) {
        perror("Failed to open socket");
        exit(EXIT_FAILURE);
    }

    memset(&listen_addr, 0, sizeof(struct sockaddr_in));
    listen_addr.sin_family      = AF_INET;
    listen_addr.sin_port        = htons(port);
    listen_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    ret = bind(listenfd, (struct sockaddr *)&listen_addr,
               sizeof(struct sockaddr_in));
    if (ret < 0) {
        perror("Failed to bind address to socket");
        exit(EXIT_FAILURE);
    }

    ret = listen(listenfd, BACKLOG);
    if (ret < 0) {
        perror("Failed to put socket into listening mode");
        exit(EXIT_FAILURE);
    }

    *serversocket = listenfd;
}

static void handle_new_client(int serversocket)
{
    struct client *client;
    int clientfd = 0;
    struct sockaddr_in client_addr;
    socklen_t client_addrlen = sizeof(struct sockaddr_in);

    clientfd = accept(serversocket, (struct sockaddr *) &client_addr,
                      &client_addrlen);
    if (clientfd < 0) {
        perror("Failed to accept new client.");
        exit(EXIT_FAILURE);
    }

    /* allocate client struct */
    client = calloc(1, sizeof(struct client));
    assert(client != NULL);

    client->socket       = clientfd;
    client->buffer_index = 0;
    client->state        = READING;

    /* insert new client at head of the list */
    if (clients == NULL) {
        client->next = NULL;
        clients = client;
    } else {
        client->next = clients;
        clients = client;
    }
}

static void handle_client_read(struct client *client)
{
    assert(client != NULL);
    assert(client->state == READING);

    ssize_t ret = 0;

    ret = read(client->socket, &client->buffer[client->buffer_index],
               BUFFER_SIZE - client->buffer_index);
    assert(read > 0);

    client->buffer_index += ret;

    if (client->buffer_index == BUFFER_SIZE) {
        client->buffer[BUFFER_SIZE] = '\0';
        client->state = WRITING;
        client->buffer_index = 0;
    }
}

static void handle_client_write(struct client *client)
{
    assert(client != NULL);
    assert(client->state == WRITING);

    ssize_t ret = 0;

    ret = write(client->socket, &client->buffer[client->buffer_index],
                (BUFFER_SIZE + 1) - client->buffer_index);
    assert(write > 0);

    client->buffer_index +=ret;

    if (client->buffer_index == BUFFER_SIZE + 1) {
        client->state = READING;
        client->buffer_index = 0;
        memset(client->buffer, '\0', BUFFER_SIZE + 1);
    }
}

static void server_accept_loop(int serversocket)
{
    int ret = 0;
    struct client *client;
    fd_set readfds;
    fd_set writefds;
    int maxfd = 0;

    while (true) {

        /* build select sets */
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);

        FD_SET(serversocket, &readfds);
        maxfd = MAX(maxfd, serversocket);

        for (client = clients; client != NULL; client = client->next) {
            if (client->state == READING) {
                FD_SET(client->socket, &readfds);
            } else if (client->state == WRITING) {
                FD_SET(client->socket, &writefds);
            }
            maxfd = MAX(maxfd, client->socket);
        }
        ret = select(maxfd + 1, &readfds, &writefds, NULL, NULL);
        assert(ret > 0);

        /* new client */
        if (FD_ISSET(serversocket, &readfds)) {
            handle_new_client(serversocket);
        }

        /* process clients */
        for (client = clients; client != NULL; client = client->next) {
            if (client->state == READING || client->state == WRITING) {
                if (FD_ISSET(client->socket, &readfds)) {
                    handle_client_read(client);
                }
                if (FD_ISSET(client->socket, &writefds)) {
                    handle_client_write(client);
                }
            } else {
                /* TODO remove finished clients from linked list. */
            }
        }
    }
}

int main(int argc, char *argv[])
{
    uint16_t port = 0;
    int serversocket = 0;
#ifdef BARRELFISH
    struct thread *t;
#endif /* BARRELFISH */

    // Parse command line arguments
    for (int i = 0; i < argc; i++) {
        if (strncmp(argv[i], "port=", strlen("port=")) == 0) {
            port = atoi(argv[i] + strlen("port="));
            printf("port=%" PRIu16 "\n", port);
        }
    }

    // Set default values
    if (port == 0) {
        port = DEFAULT_PORT;
    }

#ifdef BARRELFISH
    /*
     * Start the main lwIP thread, that has exclusive access to the lwip core
     * functions. Other threads communicate with this thread using message
     * boxes.
     *
     * Note that tcpip_init() calls lwip_init_auto().
     */
    tcpip_init(NULL, NULL);
    lwip_socket_init();

    t = thread_create(poll_loop, NULL);
#endif /* BARRELFISH */

    start_server(port, &serversocket);
    printf("Using server socket: %d\n", serversocket);
    server_accept_loop(serversocket);

    return EXIT_SUCCESS;
}
