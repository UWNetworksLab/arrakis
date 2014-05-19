/**
 * \file
 * \brief Intel e1000 driver fragmentation support
 *
 * This file is a driver for the PCI Express e1000 card
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <string.h>
#include <net_queue_manager/net_queue_manager.h>
#include "queue_manager_debug.h"

/*****************************************************************
 * Data types:
 *****************************************************************/
//#define E_IPFRAG_SERVICE_DEBUG 1

#if defined(E_IPFRAG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define E_IPFRAG_DEBUG(x...) printf("e_IPFRAG: " x)
#else
#define E_IPFRAG_DEBUG(x...) ((void)0)
#endif


struct ip_packet {
    void *data;
    size_t len;
    uint64_t ip_id;
    uint64_t timeout;
    struct ip_packet *next;
    uint64_t flags;
};

struct fragment_filter {
    uint64_t ip_id;
    struct buffer_descriptor *buffer;
    struct fragment_filter *next;
};

/*****************************************************************
 * Local states:
 *****************************************************************/

struct ip_packet *frag_packets;
struct fragment_filter *frag_filters;

#define IP_OPTIONS_OFFSET 20
#define FRAGMENTED_FLAG 0x20
#define IP_MAGIC_OFFSET 12
#define IP_MAGIC_1 0x08
#define IP_MAGIC_2 0x00

static bool is_fragmented(void *packet, size_t len)
{
    if (len <= IP_OPTIONS_OFFSET) {
        return false;
    }
    if ((((uint8_t *) packet)[IP_MAGIC_OFFSET] != IP_MAGIC_1) ||
        (((uint8_t *) packet)[IP_MAGIC_OFFSET + 1] != IP_MAGIC_2)) {
        return false;
    }

    unsigned char fragmented = (unsigned char)
      ((uint8_t *) packet)[IP_OPTIONS_OFFSET];

    fragmented &= FRAGMENTED_FLAG;
    if (fragmented) {
        return true;
    } else {
        return false;
    }
}

#define IP_ID_OFFSET 18

static uint64_t get_ip_id(void *packet, size_t len)
{
    uint64_t id = 0;

    if (len <= IP_ID_OFFSET + 1) {
        return 0;
    }
    id = (uint64_t) (((uint8_t *) packet)[IP_ID_OFFSET]);
    id <<= 8;
    id |= (uint64_t) (((uint8_t *) packet)[IP_ID_OFFSET + 1]);
    return id;
}

#define FRAGMENT_OFFSET_OFFSET 20
#define CLEAN_OPTIONS_FLAGS 0x1F

// checks whether a udp or tcp header is present
// this assumes that the packet is fragmented
static bool has_headers(void *packet, size_t len)
{
    if (len <= FRAGMENT_OFFSET_OFFSET + 1) {
        return false;
    }
    unsigned char off1 = (unsigned char)
      ((uint8_t *) packet)[FRAGMENT_OFFSET_OFFSET];
    unsigned char off2 =
      (unsigned char) ((uint8_t *) packet)[FRAGMENT_OFFSET_OFFSET + 1];
    off1 &= 0x1F;
    return !(off1 | off2);
}


static void add_packet_to_fragment_list(void *packet, size_t len,
                                        uint64_t ip_id, uint64_t flags)
{
    struct ip_packet *new_packet = (struct ip_packet *) malloc
      (sizeof(struct ip_packet));
    if (new_packet == NULL) {
        return;
    }
    new_packet->data = malloc(len);
    if (new_packet->data == NULL) {
        free(new_packet);
        return;
    }
    memcpy(new_packet->data, packet, len);
    new_packet->ip_id = ip_id;
    new_packet->len = len;
    new_packet->flags = flags;
    // TODO: set timeout here
    new_packet->next = frag_packets;
    frag_packets = new_packet;
}

static void add_fragment_filter(uint64_t ip_id,
                                struct buffer_descriptor *buffer)
{
    struct fragment_filter *filter = (struct fragment_filter *) malloc
      (sizeof(struct fragment_filter));

    if (filter == NULL) {
        return;
    }
    filter->ip_id = ip_id;
    filter->buffer = buffer;
    filter->next = frag_filters;
    frag_filters = filter;
}

static void remove_fragment_filter(uint64_t ip_id)
{
    struct fragment_filter *filter = frag_filters, *prev = NULL;

    while (filter) {
        if (filter->ip_id == ip_id) {

            if (prev == NULL) {
                frag_filters = filter->next;
            } else {
                prev->next = frag_filters->next;
            }
            free(filter);
            return;
        }
        filter = filter->next;
    }
}

static struct buffer_descriptor *execute_all_fragment_filters(uint64_t ip_id)
{
    struct fragment_filter *filter = frag_filters;

    while (filter) {
        if (filter->ip_id == ip_id) {
            E_IPFRAG_DEBUG("IP_FRAG: Filter matched with id %lu for buf %lu\n",
                           filter->ip_id, filter->buffer->buffer_id);

            return filter->buffer;
        }
        filter = filter->next;
    }
    E_IPFRAG_DEBUG("IP_FRAG: no frag filters matched\n");
    return NULL;
}

static struct ip_packet *check_outstanding_fragmented_packets(uint64_t ip_id)
{
    struct ip_packet *packet = frag_packets, *prev_packet = NULL;

    while (packet) {
        if (packet->ip_id == ip_id) {
            if (prev_packet == NULL) {
                frag_packets = packet->next;
            } else {
                prev_packet->next = packet->next;
            }
            return packet;
        } else {
            // TODO: expire it in case of timeout
        }
        prev_packet = packet;
        packet = packet->next;
    }
    return NULL;
}

bool handle_fragmented_packet(void *packet, size_t len, uint64_t flags)
{
    struct buffer_descriptor *buffer = NULL;
    uint64_t ip_id = get_ip_id(packet, len);
    struct ip_packet *old_packet;

    if (!ip_id) {               // this is not an IP packet
        return false;
    }

    if (is_fragmented(packet, len)) {
        if (has_headers(packet, len)) {
            struct filter *ret_filter;
            E_IPFRAG_DEBUG("IP_FRAG: first fragment %lu\n", ip_id);
            ret_filter = execute_filters(packet, len);
            if (ret_filter == NULL || ret_filter->buffer == NULL) {
                E_IPFRAG_DEBUG("IP_FRAG: ERROR: issues with filter %lu\n",
                               ip_id);
                return true;    // we do not want to process this packet anymore
            }

            buffer = ret_filter->buffer;
            add_fragment_filter(ip_id, buffer);
            if (copy_packet_to_user(buffer, packet, len, flags)) {
//                              send_packet_received_notification(buffer);
                E_IPFRAG_DEBUG("IP_FRAG: user copy done %lu\n", ip_id);
            } else {
                E_IPFRAG_DEBUG("IP_FRAG: ERROR: usre copy failed %lu\n", ip_id);
            }

            // we should check for out of order packets here
            while ((old_packet =
                    check_outstanding_fragmented_packets(ip_id)) != NULL) {
                E_IPFRAG_DEBUG("out of order\n");
                if (copy_packet_to_user(buffer, old_packet->data, len,
                            old_packet->flags)) {
                    E_IPFRAG_DEBUG
                      ("IP_FRAG: user copy done for out of order\n");
//                                      send_packet_received_notification(buffer);
                    free(old_packet->data);
                    free(old_packet);
                } else {
                    E_IPFRAG_DEBUG("IP_FRAG: ERROR: usre copy failed %lu\n",
                                   ip_id);
                }
            }
            return true;

        } else {                // this is continuation of fragmented packets
            E_IPFRAG_DEBUG("IP_FRAG: continuation frags %lu\n", ip_id);
            buffer = execute_all_fragment_filters(ip_id);
            if (buffer) {
                E_IPFRAG_DEBUG
                  ("IP_FRAG: copying cont of ip_id %lu to buff %lu\n", ip_id,
                   buffer->buffer_id);
                if (copy_packet_to_user(buffer, packet, len, flags)) {
                    E_IPFRAG_DEBUG
                      ("IP_FRAG: copying ip_id %lu to buff %lu done\n", ip_id,
                       buffer->buffer_id);
//                                      send_packet_received_notification(buffer);
                    return true;
                } else {
                    E_IPFRAG_DEBUG("IP_FRAG: ERROR: usre copy failed %lu\n",
                                   ip_id);
                }
            } else {            // this is a very bad out of order packet
                E_IPFRAG_DEBUG("IP_FRAG: out of order frag %lu\n", ip_id);
                add_packet_to_fragment_list(packet, len, ip_id, flags);
                return true;
            }
        }
    } else {                    // in case this is the last one
        // FIXME: there is bug here. it might be that we delete the filter
        // but an out of order packet comes in later. this is unlikely but
        // possible. unfortunatly there is not a trivial way to fix this.
        // we can tag filters for removal and set a timeout and delete them
        // in a good time. right now, the packet goes into the frag_packets
        // list and gets removed when it gets a timeout. the packet would
        // not be constructed successfully at the lwip level
        E_IPFRAG_DEBUG("IP_FRAG: assuming last frag %lu\n", ip_id);

        buffer = execute_all_fragment_filters(ip_id);
        if (buffer) {
            E_IPFRAG_DEBUG("IP_FRAG: sending last frag %lu to buff %lu\n",
                           ip_id, buffer->buffer_id);

            // this is the last of this sequence if there is no out of order
            if (copy_packet_to_user(buffer, packet, len, flags)) {
                E_IPFRAG_DEBUG("IP_FRAG: send last frag %lu to buff %lu done\n",
                               ip_id, buffer->buffer_id);
//                              send_packet_received_notification(buffer);
            } else {
                E_IPFRAG_DEBUG("IP_FRAG: ERROR: usre copy failed %lu\n", ip_id);
            }
            // removing the filter, later we should not remove it and only
            // tag it for removal and set a timeout
            remove_fragment_filter(ip_id);
            return true;
        }
    }
    E_IPFRAG_DEBUG("IP_FRAG: non fragmented packet %lu\n", ip_id);
    return false;
}
