/**
 * \file
 * \brief LWIP test/demo code
 */

/*
 * Copyright (c) 2013, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <netif/e1000.h>
/* #include <netif/etharp.h> */
#include <arranet_impl.h>

static ether_terminate_queue ether_terminate_queue_ptr = NULL;
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
static ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
static ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

uint64_t interrupt_counter = 0;
uint64_t total_rx_p_count = 0;
uint64_t total_rx_datasize = 0;
struct client_closure *g_cl = NULL;

#define MAX_PACKETS     256
/* #define MAX_PACKETS     2048 */
#define PACKET_SIZE     2048

//#define MEASURE_LATENCIES

struct packet {
    uint8_t     *payload;
    lpaddr_t    pa;
    size_t      len;
};

static struct packet rx_packets[MAX_PACKETS], tx_packets[MAX_PACKETS];

void ethernetif_backend_init(char *service_name, uint64_t queueid,
                             ether_get_mac_address_t get_mac_ptr,
                             ether_terminate_queue terminate_queue_ptr,
                             ether_transmit_pbuf_list_t transmit_ptr,
                             ether_get_tx_free_slots tx_free_slots_ptr,
                             ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                             size_t rx_bufsz,
                             ether_rx_register_buffer rx_register_buffer_ptr,
                             ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    ether_terminate_queue_ptr = terminate_queue_ptr;
    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;
    /* printf("PBUF_POOL_BUFSIZE = %u, rx buffer size = %zu\n", PBUF_POOL_BUFSIZE, */
    /*        rx_bufsz); */
}

static struct packet *get_tx_packet(void)
{
    static unsigned int idx = 0;
    struct packet *p = &tx_packets[idx];

    // Busy-wait until packet not in flight
    while(p->len != 0) {
        handle_free_tx_slot_fn_ptr();
    }

    idx = (idx + 1) % MAX_PACKETS;
    return p;
}

static void packet_output(struct packet *p)
{
    struct driver_buffer buf;

    buf.pa = p->pa;
    buf.va = p->payload;
    buf.len = p->len;
    buf.flags = 0;
    buf.opaque = p;

    errval_t err = ether_transmit_pbuf_list_ptr(&buf, 1);
    assert(err_is_ok(err));
}

#ifdef MEASURE_LATENCIES
#include <barrelfish/sys_debug.h>

// This is roughly Mon Apr 25 13:50 CEST 2011
#define TOD_OFFSET      1303732456ULL

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    uint64_t now = rdtsc();
    static uint64_t tscperms = 0;

    if(tscperms == 0) {
        errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
        assert(err_is_ok(err));
        assert(tscperms >= 1000);
    }

    uint64_t tod_us = (TOD_OFFSET * 1000000) + (now / (tscperms / 1000));

    if(tv != NULL) {
        tv->tv_sec = tod_us / 1000000;
        tv->tv_usec = tod_us % 1000000;
    }

    assert(tz == NULL);
    if(tz != NULL) {
    }

    return 0;
}

#define MAX_STAMPS      10000

static uint64_t tstamp[MAX_STAMPS];
static int stamps = 0;
#endif

void process_received_packet(struct driver_rx_buffer *buffer, size_t count,
                             uint64_t flags)
{
#ifdef MEASURE_LATENCIES
    uint64_t instamp = rdtsc();
#endif

    struct packet *p = buffer->opaque;
    assert(p != NULL);
    assert(count == 1);
    p->len = buffer->len;

    /* printf("Incoming packet\n"); */

    // Drop packets with invalid checksums
    if(flags & NETIF_RXFLAG_IPCHECKSUM) {
        if(!(flags & NETIF_RXFLAG_IPCHECKSUM_GOOD)) {
            goto out;
        }
    }

    if(flags & NETIF_RXFLAG_L4CHECKSUM) {
        if(!(flags & NETIF_RXFLAG_L4CHECKSUM_GOOD)) {
            goto out;
        }
    }

    /* printf("Checksum good\n"); */

    struct eth_hdr *ethhdr = (struct eth_hdr *)p->payload;
    switch (htons(ethhdr->type)) {
    case ETHTYPE_ARP:
        {
            /* printf("Is an ARP packet\n"); */
            struct etharp_hdr *arphdr = (struct etharp_hdr *)(p->payload + SIZEOF_ETH_HDR);
            struct eth_addr mymac;
            bool ourarp = false;

            if(htons(arphdr->opcode) == ARP_REQUEST) {
                // 10.0.2.15
                if(htons(arphdr->dipaddr.addrw[0]) == 0x0a00 &&
                   htons(arphdr->dipaddr.addrw[1]) == 0x020f) {
                    memcpy(&mymac.addr, "\x52\x54\x00\x12\x34\x56", ETHARP_HWADDR_LEN);
                    ourarp = true;
                } else if(htons(arphdr->dipaddr.addrw[0]) == 0x80d0 &&
                          htons(arphdr->dipaddr.addrw[1]) == 0x0643) {
                    // 128.208.6.67
                    memcpy(&mymac.addr, "\xa0\x36\x9f\x10\x00\xa6", ETHARP_HWADDR_LEN);
                    ourarp = true;
                } else if(htons(arphdr->dipaddr.addrw[0]) == 0x80d0 &&
                          htons(arphdr->dipaddr.addrw[1]) == 0x0682) {
                    // 128.208.6.130
                    memcpy(&mymac.addr, "\xa0\x36\x9f\x10\x00\xa2", ETHARP_HWADDR_LEN);
                    ourarp = true;
                } else if(htons(arphdr->dipaddr.addrw[0]) == 0xc0a8 &&
                          htons(arphdr->dipaddr.addrw[1]) == 0x0102) {
                    // 192.168.1.2
                    memcpy(&mymac.addr, "\xa0\x36\x9f\x10\x00\xa2", ETHARP_HWADDR_LEN);
                    ourarp = true;
                }
            }

            if(ourarp) {
                /* printf("ARP request for us\n"); */
                // Send reply
                struct packet *outp = get_tx_packet();
                struct eth_hdr *myeth = (struct eth_hdr *)outp->payload;
                struct etharp_hdr *myarp = (struct etharp_hdr *)(outp->payload + SIZEOF_ETH_HDR);

                // ETH header
                memcpy(&myeth->dest, &arphdr->shwaddr, ETHARP_HWADDR_LEN);
                memcpy(&myeth->src, &mymac, ETHARP_HWADDR_LEN);
                myeth->type = htons(ETHTYPE_ARP);

                // ARP header
                myarp->hwtype = htons(1);
                myarp->proto = htons(ETHTYPE_IP);
                myarp->hwlen = 6;
                myarp->protolen = 4;
                myarp->opcode = htons(ARP_REPLY);
                memcpy(&myarp->shwaddr, &mymac, ETHARP_HWADDR_LEN);
                memcpy(&myarp->sipaddr, &arphdr->dipaddr, sizeof(myarp->sipaddr));
                memcpy(&myarp->dhwaddr, &arphdr->shwaddr, ETHARP_HWADDR_LEN);
                memcpy(&myarp->dipaddr, &arphdr->sipaddr, sizeof(myarp->dipaddr));

                outp->len = p->len;
                packet_output(outp);
            }
        }
        break;

    case ETHTYPE_IP:
        {
            struct ip_hdr *iphdr = (struct ip_hdr *)(p->payload + SIZEOF_ETH_HDR);

            /* printf("Is an IP packet, type %x\n", IPH_PROTO(iphdr)); */

            if(IPH_PROTO(iphdr) == IP_PROTO_UDP) {
                struct udp_hdr *udphdr = (struct udp_hdr *)(p->payload + SIZEOF_ETH_HDR + (IPH_HL(iphdr) * 4));
                uint8_t *payload = p->payload + SIZEOF_ETH_HDR + (IPH_HL(iphdr) * 4) + sizeof(struct udp_hdr);

                /* printf("Got UDP packet, dest IP %x, dest port %u\n", */
                /*        iphdr->dest.addr, udphdr->dest); */

                if((htonl(iphdr->dest.addr) != 0x80d00643 &&
                    htonl(iphdr->dest.addr) != 0x80d00682 &&
                    htonl(iphdr->dest.addr) != 0xc0a80102 &&
                    htonl(iphdr->dest.addr) != 0x0a00020f) ||
                   htons(udphdr->dest) != 1234) {
                    goto out;
                }

                /* printf("payload '%s'\n", payload); */

                struct packet *outp = get_tx_packet();
                struct eth_hdr *myeth = (struct eth_hdr *)outp->payload;
                struct ip_hdr *myip = (struct ip_hdr *)(outp->payload + SIZEOF_ETH_HDR);
                struct udp_hdr *myudp = (struct udp_hdr *)(outp->payload + SIZEOF_ETH_HDR + (IPH_HL(iphdr) * 4));
                uint8_t *mypayload = outp->payload + SIZEOF_ETH_HDR + (IPH_HL(iphdr) * 4) + sizeof(struct udp_hdr);

                // ETH header
                memcpy(&myeth->dest, &ethhdr->src, ETHARP_HWADDR_LEN);
                memcpy(&myeth->src, &ethhdr->dest, ETHARP_HWADDR_LEN);
                myeth->type = htons(ETHTYPE_IP);

                // IP header
                memcpy(myip, iphdr, sizeof(struct ip_hdr));
                memcpy(&myip->src, &iphdr->dest, sizeof(ip_addr_p_t));
                memcpy(&myip->dest, &iphdr->src, sizeof(ip_addr_p_t));

                // UDP header
                memcpy(myudp, udphdr, sizeof(struct udp_hdr));
                myudp->src = udphdr->dest;
                myudp->dest = udphdr->src;

                // Payload
                memcpy(mypayload, payload, htons(udphdr->len) - 8);

                outp->len = p->len;
                packet_output(outp);

#ifdef MEASURE_LATENCIES
                uint64_t now = rdtsc();
                tstamp[stamps] = now - instamp;

                /* printf("got packet %d\n", stamps); */

                stamps++;
                if(stamps == MAX_STAMPS) {
                    printf("latencies:\n");
                    for(int i = 0; i < MAX_STAMPS; i++) {
                        printf("%" PRIu64 " cycles\n", tstamp[i]);
                    }
                    stamps = 0;
                }
#endif

            }
        }
        break;

    default:
        break;
    }

 out:
    {
        //now we have consumed the preregistered pbuf containing a received packet
        //which was processed in this function. Therefore we have to register a new
        //free buffer for receiving packets.
        errval_t err = rx_register_buffer_fn_ptr(p->pa, p->payload, p);
        assert(err_is_ok(err));
    }
}

bool handle_tx_done(void *opaque)
{
    struct packet *p = opaque;
    p->len = 0;
    return true;
}

/* allocate a single frame, mapping it into our vspace with given attributes */
static void *alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap)
{
    struct capref frame;
    errval_t r;

    r = frame_alloc(&frame, size, NULL);
    assert(err_is_ok(r));
    void *va;
    r = vspace_map_one_frame_attr(&va, size, frame, attr,
                                  NULL, NULL);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "vspace_map_one_frame failed");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}

int main(int argc, char *argv[])
{
    uint8_t mac[6];

    printf("Starting e10k test program...\n");

    e1000n_driver_init(argc, argv);

    ether_get_mac_address_ptr(mac);
    printf("e10ktest MAC address %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);

    struct capref frame;
    uint8_t *ram_base = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                                        MAX_PACKETS * PACKET_SIZE, &frame);
    assert(ram_base != NULL);

    struct frame_identity id;
    errval_t err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    // Add buffers to RX ring for packet reception
    for(int i = 0; i < MAX_PACKETS; i++) {
        struct packet *p = &rx_packets[i];

        p->payload = ram_base + (i * PACKET_SIZE);
        p->pa = id.base + (i * PACKET_SIZE);
        p->len = PACKET_SIZE;

        err = rx_register_buffer_fn_ptr(p->pa, p->payload, p);
        assert(err_is_ok(err));
    }

    // Setup TX packets
    ram_base = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
                               MAX_PACKETS * PACKET_SIZE, &frame);
    assert(ram_base != NULL);
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));
    for(int i = 0; i < MAX_PACKETS; i++) {
        struct packet *p = &tx_packets[i];
        p->payload = ram_base + (i * PACKET_SIZE);
        p->pa = id.base + (i * PACKET_SIZE);
        p->len = 0;
    }

    for(;;) {
        arranet_polling_loop();
        //        e1000n_polling_loop(get_default_waitset());
    }

    return 0;
}
