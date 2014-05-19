/**
 * \file
 * \brief producer consumer library
 *
 * This file provides a producer consumer protocol
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef PROCON_H_
#define PROCON_H_

#ifdef __scc__
#define CACHESIZE   32
#else
#define CACHESIZE    64
#endif

#define SLOT_PADDING ((CACHESIZE) - ((sizeof(struct slot_data))%(CACHESIZE)))

#define SLOT_SIZE ((sizeof(struct slot_data)) + (SLOT_PADDING))

#define TMP_SLOTS 2


// information inside the slot
struct slot_data {
    volatile uint64_t buffer_id;
    volatile uint64_t pbuf_id;
    volatile uint64_t offset;
    volatile uint64_t len;
    volatile uint64_t no_pbufs;
    volatile uint64_t client_data;
    volatile uint64_t ts;
};


// slot container
union slot {
    struct slot_data d;
    volatile uint8_t raw[SLOT_SIZE];
};


// register to hold the indexes
union vreg {
    volatile uint64_t value;
    volatile uint8_t raw[CACHESIZE];
};


// Container for shared space
struct shared_pool {
    union vreg write_reg; // slot-index that producer will produce next
    union vreg read_reg;  // slot-index that Consumer will consume next
    union vreg size_reg;
    union slot slot_list[TMP_SLOTS];
};

struct shared_pool_private {
    struct shared_pool *sp;
    struct      capref cap;
//    struct ether_binding *con;
    lpaddr_t    pa;
    void        *va;
    uint64_t    mem_size;
    uint64_t    alloted_slots;
    bool        is_creator;
    uint8_t     role;       // Producer or consumer?
    uint64_t    ghost_read_id;
    uint64_t    ghost_write_id;
    uint64_t    pre_write_id;
    uint64_t    notify_other_side; // Something has happened here
    uint64_t    produce_counter;
    uint64_t    consume_counter;
    uint64_t    clear_counter;
    uint64_t    c_read_id;
    uint64_t    c_write_id;
    uint64_t    c_size;
};


// generic queue based code
bool sp_gen_queue_empty(uint64_t read, uint64_t write);
bool sp_gen_queue_full(uint64_t read, uint64_t write, uint64_t size);
bool sp_c_between(uint64_t start, uint64_t value, uint64_t end, uint64_t size);
uint64_t sp_c_range_size(uint64_t start, uint64_t end, uint64_t size);

// State checking function prototypes
uint64_t sp_get_read_index(struct shared_pool_private *spp);
uint64_t sp_get_write_index(struct shared_pool_private *spp);
uint64_t sp_get_queue_size(struct shared_pool_private *spp);

void sp_reload_regs(struct shared_pool_private *spp);
bool sp_queue_empty(struct shared_pool_private *spp);
bool sp_queue_full(struct shared_pool_private *spp);
bool sp_write_peekable_index(struct shared_pool_private *spp, uint64_t index);
bool sp_read_peekable_index(struct shared_pool_private *spp, uint64_t index);
bool sp_validate_read_index(struct shared_pool_private *spp, uint64_t index);
bool sp_validate_write_index(struct shared_pool_private *spp, uint64_t index);
uint64_t sp_queue_elements_count(struct shared_pool_private *spp);
uint64_t sp_queue_free_slots_count(struct shared_pool_private *spp);
uint64_t sp_is_slot_clear(struct shared_pool_private *spp, uint64_t id);


// initialization function prototypes
struct shared_pool_private *sp_create_shared_pool(uint64_t slot_no,
                                uint8_t role);
errval_t sp_map_shared_pool(struct shared_pool_private *spp, struct capref cap,
        uint64_t slot_no, uint8_t role);
//void sp_reset_pool(struct shared_pool_private *spp, uint64_t slot_count);


// State modifying function prototypes
bool sp_produce_slot(struct shared_pool_private *spp, struct slot_data *d);
bool validate_and_empty_produce_slot(struct shared_pool_private *spp,
        uint64_t produced_slot_id);
bool sp_replace_slot(struct shared_pool_private *spp,
            struct slot_data *new_slot);
bool sp_ghost_read_slot(struct shared_pool_private *spp, struct slot_data *dst);
bool sp_ghost_read_confirm(struct shared_pool_private *spp);
bool sp_ghost_produce_slot(struct shared_pool_private *spp,
        struct slot_data *d, uint64_t index);
bool sp_set_read_index(struct shared_pool_private *spp, uint64_t index);
bool sp_set_write_index(struct shared_pool_private *spp, uint64_t index);
bool sp_increment_write_index(struct shared_pool_private *spp);
bool sp_clear_slot(struct shared_pool_private *spp, struct slot_data *d,
        uint64_t id);


// Helper functions
void sp_copy_slot_data(struct slot_data *d, struct slot_data *s);
void copy_data_into_slot(struct shared_pool_private *spp, uint64_t buf_id,
        uint64_t id, uint64_t offset, uint64_t len, uint64_t no_pbufs,
        uint64_t client_data, uint64_t ts);
void sp_copy_slot_data_from_index(struct shared_pool_private *spp,
        uint64_t index, struct slot_data *d);

// Debugging functions
void sp_print_slot(struct slot_data *d);
void sp_print_metadata(struct shared_pool_private *spp);
void sp_print_pool(struct shared_pool_private *spp);

#endif // PROCON_H_
