/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#include "elb.h"

#include <net_queue_manager/net_queue_manager.h>

#define BUFFER_SIZE 2048
#define BUF_COUNT ((128*1024*1024) / BUFFER_SIZE)

static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_terminate_queue terminate_queue_fn_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
static ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
static ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

static struct capref buffer_frame;
static uint8_t our_mac[8];
static bool initialized = false;

size_t buffer_size;
void *buffer_base;
size_t buffer_count = BUF_COUNT;
static uint64_t buffer_base_phys;

static inline uint64_t buffer_phys(size_t idx) {
    return buffer_base_phys + idx * buffer_size;
}


static void alloc_mem(uint64_t *phys, void **virt, size_t size,
                     struct capref *frame)
{
    errval_t r;
    vregion_flags_t flags;
    struct frame_identity frameid = { .base = 0, .bits = 0 };

    r = frame_alloc(frame, size, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Allocating memory region frame failed!");
    }

    r = invoke_frame_identify(*frame, &frameid);
    if (!err_is_ok(r)) {
        USER_PANIC("Identifying memory region frame failed!");
    }
    *phys = frameid.base;

    /*flags = (use_nocache ? VREGION_FLAGS_READ_WRITE_NOCACHE :
                           VREGION_FLAGS_READ_WRITE);*/
    flags = VREGION_FLAGS_READ_WRITE;
    r = vspace_map_one_frame_attr(virt, size, *frame, flags, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
}


void ethersrv_init(char *service_name, uint64_t queueid,
                   ether_get_mac_address_t get_mac_ptr,
                   ether_terminate_queue terminate_queue_ptr,
                   ether_transmit_pbuf_list_t transmit_ptr,
                   ether_get_tx_free_slots tx_free_slots_ptr,
                   ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                   size_t rx_bufsz,
                   ether_rx_register_buffer rx_register_buffer_ptr,
                   ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    uint64_t phys;
    void *virt;

    ether_get_mac_address_ptr = get_mac_ptr;
    terminate_queue_fn_ptr = terminate_queue_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;

    buffer_size = rx_bufsz;
    ether_get_mac_address_ptr(our_mac);

    // Allocate packet buffers
    alloc_mem(&phys, &virt, BUF_COUNT * rx_bufsz, &buffer_frame);
    buffer_base = virt;
    buffer_base_phys = phys;

    benchmark_init();
}

void ethersrv_argument(const char* arg)
{
    benchmark_argument((char*) arg);
}

void do_pending_work_for_all(void)
{
    if (!initialized) {
        return;
    }

    benchmark_do_pending_work();
}

void process_received_packet(void *opaque, size_t pkt_len, bool is_last)
{
    assert(is_last);
    benchmark_rx_done((size_t) opaque, pkt_len);
}

//errval_t buffer_tx_add(size_t idx, size_t offset, size_t len, size_t more_chunks)
errval_t buffer_tx_add(size_t idx, size_t offset, size_t len)
{
    struct driver_buffer buffer = {
        .va = buffer_address(idx) + offset,
        .pa = buffer_phys(idx) + offset,
        .len = len,
    };
    return ether_transmit_pbuf_list_ptr(&buffer, 1, (void*) idx);
}

bool handle_tx_done(void *opaque)
{
    benchmark_tx_done((size_t) opaque);
    return true;
}

errval_t buffer_rx_add(size_t idx)
{
    return rx_register_buffer_fn_ptr(buffer_phys(idx),
                              buffer_address(idx),
                              (void*) idx);
}

void net_if_init(const char* cardname, uint64_t qid)
{
}

void terminate_benchmark(void)
{
    // Free the buffers
    vspace_unmap(buffer_base);
    cap_delete(buffer_frame);

    terminate_queue_fn_ptr();
}

void benchmark_get_mac_address(uint8_t *mac)
{
    memcpy(mac, &our_mac, 6);
}

uint64_t get_rx_bufferid(void)
{
    return 0;
}

uint64_t get_tx_bufferid(void)
{
    return 0;
}


