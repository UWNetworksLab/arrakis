/**
 * \file
 * \brief Rockcreek (RCK) configuration registers driver.
 *
 * Message passing buffer layout
 * -----------------------------
 * We have two pages (8K) per core, in which we store a 2-level B-Tree of
 * bitmaps. We can fit 256 bitmaps of 256 bits (32 bytes) on the two pages.
 * We use the first 256 bits for the root node, which tells whether there are
 * bits set in the corresponding second level.
 *
 * 256 bits were chosen to equal the size of a cache line, which facilitates
 * I/O (only full lines can be read/written).
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <paging_kernel_helper.h>
#include <rck.h>
#include <paging_kernel_arch.h>
#include <arch/x86/apic.h>
#include <string.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <diteinfo.h>
#include <elf/elf.h>
#include <arch/x86/startup_x86.h>
#include "rck_dev.h"

/// Number of tiles on the RCK board
#define NUM_RCK_TILES           24

/// Physical base address for configuration registers in the default PTE map
#define RCK_CFG_REGS_BASE       0xe0000000

/// Physical base address of own config registers
#define RCK_CFG_REGS_OWN        0xf8000000

/// Physical base address for message passing buffers (MPBs) in the default PTE map
#define RCK_MPB_BASE             0xc0000000

/// Physical base address for own MPB
#define RCK_MPB_OWN             0xd8000000

/// Address range mapped by one LUT entry
#define LUT_SIZE                 0x1000000

// Max number of notification IDs that fit into message passing buffers
#define MAX_CHANIDS             65280

/// Offset for core 0 of a tile's MPB
#define CORE_0_OFFSET           0

/// Offset for core 1 of a tile's MPB
#define CORE_1_OFFSET           (2 * BASE_PAGE_SIZE)

/// Number of bits in a machine word
#define NBWO                    (NBBY * sizeof(uintptr_t))

/// Number of bits in a B-tree node
#define NODE_BITS               256

/// Root has one less bit valid, because we're out of memory (there are 255 children)
#define ROOT_BITS               255

/// Mask to get remainder of a non-cacheline-aligned address (32-byte cachelines)
#define CACHE_LINE_MASK         (CACHE_LINE_SIZE - 1)

/// B-Tree node size in bytes (#NODE_BITS / #NBBY)
#define NODE_SIZE               32

/// Size of cache line in words
#define CACHE_LINE_WORDS        (CACHE_LINE_SIZE / sizeof(uintptr_t))

/// Message passing buffer page table flags
#define MPB_PTABLE_FLAGS \
    (X86_32_PTABLE_PRESENT | X86_32_PTABLE_USER_SUPERVISOR | X86_32_PTABLE_READ_WRITE | SCC_PTABLE_MESSAGE_BUFFER | X86_32_PTABLE_WRITE_THROUGH)

/// Size of circular array in cachelines
#define RING_SIZE       255

/// Mackerel state for all the tiles
static rck_t rck[NUM_RCK_TILES];

// Mackerel state for my own tile
static rck_t rck_own;

/// Start addresses of MPBs for all tiles
static lvaddr_t mpb[NUM_RCK_TILES];

/// User-space endpoints awaiting notifications
static struct cte endpoints[MAX_CHANIDS];

/**
 * Acquire test&set lock.
 */
static void __attribute__ ((noinline)) acquire_lock(uint8_t dest)
{
    int tile = dest / 2, core = dest % 2;
    /* rck_tas_t tas; */

    /* // Acquire lock via test&set */
    /* do { */
    /*     tas = rck_tas_rd(&rck[tile], core); */
    /* } while(tas.val == 0); */

    uint32_t tas;

    // Acquire lock via test&set
    do {
        tas = rck_tas_rd_raw(&rck[tile], core);
    } while(tas == 0);
}

/**
 * Release test&set lock.
 */
static inline void release_lock(uint8_t dest)
{
    int tile = dest / 2, core = dest % 2;
    /* rck_tas_t tas; */

    // Release the lock via test&set
    /* tas.val = 0; */
    /* rck_tas_wr(&rck[tile], core, tas); */

    uint32_t tas = 0;
    rck_tas_wr_raw(&rck[tile], core, tas);
}

/**
 * Initialize RCK driver by mapping in config registers and MPBs.
 */
void rck_init(void)
{
    for(int i = 0; i < NUM_RCK_TILES; i++) {
        void *rck_base =
            (void *)paging_map_device(RCK_CFG_REGS_BASE + LUT_SIZE * i,
                                      2 * BASE_PAGE_SIZE);
        assert(rck_base != NULL);
        rck_initialize(&rck[i], rck_base);

        // XXX: Reset GLCFG config registers
        rck_glcfg_wr_raw(&rck[i], 0, 0x340df8);
        rck_glcfg_wr_raw(&rck[i], 1, 0x340df8);
    }

    // Map my own config registers
    void *rck_own_addr =
        (void *)paging_map_device(RCK_CFG_REGS_OWN, 2 * BASE_PAGE_SIZE);
    assert(rck_own_addr != NULL);
    rck_initialize(&rck_own, rck_own_addr);

    // Map message passing buffers
    for(int i = 0; i < NUM_RCK_TILES; i++) {
        // Map own MPB mappings for my tile
        if(rck_get_coreid() / 2 == i) {
            mpb[i] = paging_x86_32_map_special(RCK_MPB_OWN, 4 * BASE_PAGE_SIZE,
                                               MPB_PTABLE_FLAGS);
        } else {
            mpb[i] = paging_x86_32_map_special(RCK_MPB_BASE + LUT_SIZE * i,
                                               4 * BASE_PAGE_SIZE,
                                               MPB_PTABLE_FLAGS);
        }
        assert(mpb[i] != 0);

        // XXX: Bootstrap core clears all message buffers
        if(rck_get_coreid() == 0) {
            cl1flushmb();
            memset((void *)mpb[i], 0, 4 * BASE_PAGE_SIZE);
        }
    }

    // Map more shared RAM (960MB more)
    /* static int addr[20] = {0x1ec, 0x28, 0x51, 0x7a, 0xa3, 0xcc, 0xf5, 0x11e, 0x147, 0x170, 0x199, 0x1c2, 0x1eb, 0x1ed, 0x1ee, 0x1ef, 0x1f0, 0x1f1, 0x1f2, 0x1f3}; */
    static int addr[19] = {0x28, 0x51, 0x7a, 0xa3, 0xcc, 0xf5, 0x11e, 0x147, 0x170, 0x199, 0x1c2, 0x1eb, 0x1ed, 0x1ee, 0x1ef, 0x1f0, 0x1f1, 0x1f2, 0x1f3};
    for(int i = 0; i < 76; i++) {
        int current_lut;
        if(i < 60) {
            current_lut = 132 + i;
        } else {
            current_lut = 127 - (i - 60);
        }
        rck_lute_t lut = {
            .bypass = 0
        };

        switch(i / 19) {
        case 0:
            lut.route = 0;
            lut.subdest = rck_mc1_sd;
            break;

        case 1:
            lut.route = 5;
            lut.subdest = rck_mc2_sd;
            break;

        case 2:
            lut.route = 0x20;
            lut.subdest = rck_mc1_sd;
            break;

        case 3:
            lut.route = 0x25;
            lut.subdest = rck_mc2_sd;
            break;

        default:
            assert(!"shouldn't happen");
        };

        lut.addrbits = addr[i % 19];

        rck_lut0_wr(&rck_own, current_lut, lut);
        rck_lut1_wr(&rck_own, current_lut, lut);
    }
}

#define NUM_ROWS 4
#define NUM_COLS 6
#define NUM_CORES 2

/**
 * Return core ID of this core.
 */
uint8_t rck_get_coreid(void)
{
    /* rck_tileid_t tileid = rck_tileid_rd(&rck_own); */
    uint32_t core = rck_tileid_rd_raw(&rck_own);

    /* printf("rck_get_coreid: x = %d, y = %d, core = %d\n", */
    /*        tileid.x, tileid.y, tileid.coreid); */

    int x, y;

    x=   (core>>3) & 0x0f; // bits 06:03
    y=   (core>>7) & 0x0f; // bits 10:07
    core=(core   ) & 0x07; // bits 02:00

    /* printf("done right: x = %d, y = %d, core = %d\n", */
    /*        x, y, core); */

    /* return (NUM_CORES*NUM_COLS*tileid.y)+(NUM_CORES*tileid.x)+tileid.coreid; */
    return (NUM_CORES*NUM_COLS*y)+(NUM_CORES*x)+core;
}

extern struct dcb *run_next;

static void handle_channel(uintptr_t chanid)
{
    // Message pending, assert we have a waiting endpoint
    struct capability *ep = &endpoints[chanid].cap;

    if(ep->type == ObjType_Null) {
        printk(LOG_WARN, "unhandled RCK channel %"PRIuPTR"\n", chanid);
        return;
    } else {
      /* printf("%d: handle_channel(%d)\n", my_core_id, chanid); */
    }
    assert(ep->type == ObjType_EndPoint);

    errval_t err = lmp_deliver_notification(ep);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            /* dispatcher_handle_t handle = ep->u.endpoint.listener->disp; */
            /* struct dispatcher_shared_generic *disp = */
            /*     get_dispatcher_shared_generic(handle); */
            /* printk(LOG_DEBUG, "%.*s: RCK message buffer overflow\n", */
            /*        DISP_NAME_LEN, disp->name); */
        } else {
            printk(LOG_ERR, "Unexpected error delivering RCK notification\n");
        }
    }

    run_next = ep->u.endpoint.listener;
}

/**
 * Send a notification to a RCK core.
 *
 * \param dest          Destination RCK core ID
 * \param chanid        Destination notification ID
 */
void rck_send_notification(uint8_t dest, uintptr_t chanid)
{
    assert(chanid < MAX_CHANIDS);
    assert(dest != apic_id);

    int tile = dest / 2, core = dest % 2;
    lvaddr_t mb = mpb[tile] + core * CORE_1_OFFSET;
    volatile uintptr_t *cl;

    acquire_lock(dest);

    uintptr_t reader_pos = *(uintptr_t *)mb;
    uintptr_t writer_pos = *(uintptr_t *)(mb + 4);

    cl1flushmb();
    cl = (uintptr_t *)mb;
    cl[0] = reader_pos;
    cl[1] = (writer_pos + 1) % RING_SIZE;
    cl[2] = 0;
    cl[3] = 0;
    cl[4] = 0;
    cl[5] = 0;
    cl[6] = 0;
    cl[7] = 0;

    /* printf("%d: rck_send_notification(%u (%d, %d), %u)\n", my_core_id, dest, tile, core, chanid); */

    assert(reader_pos != (writer_pos + 1) % RING_SIZE);

    lvaddr_t pos = mb + CACHE_LINE_SIZE + writer_pos * CACHE_LINE_SIZE;
    assert(!(pos & CACHE_LINE_MASK));
    cl = (uintptr_t *)pos;

    cl[0] = chanid;
    cl[1] = 0;
    cl[2] = 0;
    cl[3] = 0;
    cl[4] = 0;
    cl[5] = 0;
    cl[6] = 0;
    cl[7] = 0;

#ifndef NO_INTERRUPT
    // Send the interrupt if not already pending
    /* rck_glcfg_t glcfg = rck_glcfg_rd(&rck[tile], core); */
    /* if(!glcfg.intr) { */
    /*     glcfg.intr = 1; */
    /*     rck_glcfg_wr(&rck[tile], core, glcfg); */
    /* } */
    uint32_t glcfg = rck_glcfg_rd_raw(&rck[tile], core);
    if(!(glcfg & (1 << 1))) {
        glcfg |= 1 << 1;
        rck_glcfg_wr_raw(&rck[tile], core, glcfg);
    }
#endif

    release_lock(dest);
}

/**
 * Determine and handle pending local notifications.
 */
void rck_handle_notification(void)
{
    uint8_t myself = rck_get_coreid();
    int tile = myself / 2, core = myself % 2;
    lvaddr_t mb = mpb[tile] + core * CORE_1_OFFSET;
    volatile uintptr_t *cl;

    /* printf("rck_handle_notification(0x%x)\n", mb); */

    acquire_lock(myself);

    // Get reader/writer pos
    cl1flushmb();
    uintptr_t reader_pos = *(uintptr_t *)mb;
    uintptr_t writer_pos = *(uintptr_t *)(mb + 4);

//#ifndef NO_INTERRUPT
//    assert(reader_pos != writer_pos);
//#else
    if(reader_pos == writer_pos) {
	   printf("reader_pos == writer_pos\n"); 
       goto out;
    }
//#endif

    while(reader_pos != writer_pos) {
        // Check channel ID
        uintptr_t *pos = (uintptr_t *)(mb + CACHE_LINE_SIZE + reader_pos * CACHE_LINE_SIZE);
        /* assert(!(pos & CACHE_LINE_MASK)); */
        handle_channel(*pos);
        reader_pos = (reader_pos + 1) % RING_SIZE;
    }

    // Update reader pos
    cl = (uintptr_t *)mb;
    cl[0] = reader_pos;
    cl[1] = writer_pos;
    cl[2] = 0;
    cl[3] = 0;
    cl[4] = 0;
    cl[5] = 0;
    cl[6] = 0;
    cl[7] = 0;

#ifndef NO_INTERRUPT
    // Reset interrupt line
    /* rck_glcfg_t glcfg = rck_glcfg_rd(&rck[tile], core); */
    /* glcfg.intr = 0; */
    /* rck_glcfg_wr(&rck[tile], core, glcfg); */
    uint32_t glcfg = rck_glcfg_rd_raw(&rck[tile], core);
    glcfg &= ~(1 << 1);
    rck_glcfg_wr_raw(&rck[tile], core, glcfg);
#endif

//#ifdef NO_INTERRUPT
 out:
//#endif
    release_lock(myself);
}

void rck_reset_lint1(void)
{
    uint8_t myself = rck_get_coreid();
    int tile = myself / 2, core = myself % 2;
    uint32_t glcfg = rck_glcfg_rd_raw(&rck[tile], core);
    glcfg &= ~1;
    rck_glcfg_wr_raw(&rck[tile], core, glcfg);
}

errval_t rck_get_route(genpaddr_t base, size_t size, uint8_t *route,
                       uint8_t *subdest, uint16_t *addrbits)
{
    uint8_t myself = rck_get_coreid();
    int tile = myself / 2, core = myself % 2;
    uint32_t lute;
    genpaddr_t index = base >> 24;
    assert(index < 256);
    bool first = true;
    assert(index + (size / LUT_SIZE) < 256);

    // This is probably overkill. A device is probably only able to
    // route to exactly one LUT mapping, and not multiple consecutive
    // ones.
    printf("#### base %"PRIxGENPADDR", %zu\n", base, size);
    for(genpaddr_t i = 0; i <= size / LUT_SIZE; i++) {
        if(core == 0) {
            lute = rck_lut0_rd_raw(&rck[tile], index + i);
        } else {
            lute = rck_lut1_rd_raw(&rck[tile], index + i);
        }

        uint8_t myroute = (lute >> 13) & 0xff;
        uint8_t mysubdest = (lute >> 10) & 0b111;
        uint16_t myaddrbits = lute & 0x3ff;
        printf("#### myroute = %x, %x %x\n", myroute, mysubdest, myaddrbits);
        if(!first) {
            if(myroute != *route || mysubdest != *subdest
               || myaddrbits != *addrbits + i) {
                return SYS_ERR_CROSS_MC;
            }
        } else {
            *route = myroute;
            *subdest = mysubdest;
            *addrbits = myaddrbits;
            first = false;
        }
    }

    return SYS_ERR_OK;
}

errval_t rck_register_notification(capaddr_t ep, int chanid)
{
    struct cte *recv;
    errval_t err;

    err = caps_lookup_slot(&dcb_current->cspace.cap, ep,
                           CPTR_BITS, &recv, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IRQ_LOOKUP);
    }

    assert(recv != NULL);

    // Return w/error if cap is not an endpoint
    if(recv->cap.type != ObjType_EndPoint) {
        return SYS_ERR_IRQ_NOT_ENDPOINT;
    }

    // Return w/error if no listener on endpoint
    if(recv->cap.u.endpoint.listener == NULL) {
        return SYS_ERR_IRQ_NO_LISTENER;
    }

    if(chanid < MAX_CHANIDS) {
        // check that we don't overwrite someone else's handler
        if (endpoints[chanid].cap.type != ObjType_Null) {
            printf("kernel: installing new handler for RCK notification %d\n", chanid);
        }
        return caps_copy_to_cte(&endpoints[chanid], recv, false, 0, 0);
    } else {
        return SYS_ERR_IRQ_INVALID;
    }
}

errval_t rck_delete_notification(int chanid)
{
    if(chanid < MAX_CHANIDS) {
        endpoints[chanid].cap.type = ObjType_Null;
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_IRQ_INVALID;
    }
}

#define CORES_PER_QUADRANT      12
/* #define LUTS_PER_CORE           20 */
#define XCORE_LUT_BASE          41
#define XCORE_PADDR_BASE        0x29000000

struct mcdest {
    int route;
    rck_mcsubdests_t subdest;
    int addrbits;
};

static struct mcdest dests[48] = {
    // Core 0
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x0
    },
    // Core 1
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x1
    },
    // Core 2
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x2
    },
    // Core 3
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x3
    },
    // Core 4
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x4
    },
    // Core 5
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x5
    },

    // ---------------

    // Core 6
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x0
    },
    // Core 7
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x1
    },
    // Core 8
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x2
    },
    // Core 9
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x3
    },
    // Core 10
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x4
    },
    // Core 11
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x5
    },

    // ---------------

    // Core 12
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x6
    },
    // Core 13
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x7
    },
    // Core 14
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x8
    },
    // Core 15
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0x9
    },
    // Core 16
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0xa
    },
    // Core 17
    {
        .route = 0,
        .subdest = rck_mc1_sd,
        .addrbits = 0xb
    },

    // ---------------

    // Core 18
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x6
    },
    // Core 19
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x7
    },
    // Core 20
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x8
    },
    // Core 21
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0x9
    },
    // Core 22
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0xa
    },
    // Core 23
    {
        .route = 0x5,
        .subdest = rck_mc2_sd,
        .addrbits = 0xb
    },

    // ---------------

    // Core 24
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x0
    },
    // Core 25
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x1
    },
    // Core 26
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x2
    },
    // Core 27
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x3
    },
    // Core 28
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x4
    },
    // Core 29
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x5
    },

    // ---------------

    // Core 30
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x0
    },
    // Core 31
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x1
    },
    // Core 32
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x2
    },
    // Core 33
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x3
    },
    // Core 34
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x4
    },
    // Core 35
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x5
    },

    // ---------------

    // Core 36
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x6
    },
    // Core 37
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x7
    },
    // Core 38
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x8
    },
    // Core 39
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0x9
    },
    // Core 40
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0xa
    },
    // Core 41
    {
        .route = 0x20,
        .subdest = rck_mc1_sd,
        .addrbits = 0xb
    },

    // ---------------

    // Core 42
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x6
    },
    // Core 43
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x7
    },
    // Core 44
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x8
    },
    // Core 45
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0x9
    },
    // Core 46
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0xa
    },
    // Core 47
    {
        .route = 0x25,
        .subdest = rck_mc2_sd,
        .addrbits = 0xb
    }
};

errval_t rck_map_core_lut(uint8_t entry, uint8_t coreid, uint8_t offset)
{
    int route = dests[coreid].route;
    rck_mcsubdests_t subdest = dests[coreid].subdest;
    int addrbits = dests[coreid].addrbits * 0x29 + offset;

    // XXX: Only works from core 0 for now
    assert(rck_get_coreid() == 0);

    printf("Kernel: Mapping entry %u to coreid %u, offset %u\n",
           entry, coreid, offset);

    uint32_t regval = ((route & 0xff) << 13) | ((subdest & 7) << 10) |
        (addrbits & 1023);
    rck_lut0_wr_raw(&rck_own, entry, regval);

    return SYS_ERR_OK;
}

struct allocate_state {
    void          *vbase;
    genvaddr_t     elfbase;
};

static errval_t elfload_allocate(void *state, genvaddr_t base,
                                 size_t size, uint32_t flags,
                                 void **retbase)
{
    struct allocate_state *s = state;

    *retbase = s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

#define SCC_L2_LINESIZE 32
#define SCC_L2_WAYS     4
#define SCC_L2_CAPACITY (256 * 1024)
#define SCC_L2_WBSTRIDE (SCC_L2_CAPACITY / SCC_L2_WAYS)

static void flush_l2_cache(void)
{
    for (lpaddr_t addr = 0; addr < SCC_L2_WBSTRIDE; addr += SCC_L2_LINESIZE) {
        volatile char tmp;
        lpaddr_t set = addr % SCC_L2_WBSTRIDE;
        volatile char *dummy = (volatile char*)local_phys_to_mem(set);

        /* Now read new data into all ways */
        for (unsigned int i = 0; i < SCC_L2_WAYS; i++) {
            tmp = *dummy;
            dummy += SCC_L2_WBSTRIDE;
        }
    }
}

int rck_start_core(uint8_t coreid, genpaddr_t urpcframe_base,
                   uint8_t urpcframe_bits, int chanid)
{
    int tile = coreid / 2, core = coreid % 2;
    int current_lut = XCORE_LUT_BASE;

    // XXX: Only works from core 0 for now
    assert(rck_get_coreid() == 0);

    int route = dests[coreid].route;
    rck_mcsubdests_t subdest = dests[coreid].subdest;
    int addrbits = dests[coreid].addrbits * 0x29;

    // Map core's memory at LUT entry 41
    // XXX: Something's utterly wrong here! The register isn't written correctly.
    // route is 11b, even though it should be 0.
#       if 0
    rck_lute_t lut = {
        .bypass = 0,
        .route = route,
        .subdest = subdest,
        .addrbits = (coreid % CORES_PER_QUADRANT) * LUTS_PER_CORE
    };
    rck_lut0_wr(&rck_own, current_lut, lut);
    printf("route = 0x%x, subdest = 0x%x, addrbits = 0x%x\n",
           lut.route, lut.subdest, lut.addrbits);
#       endif
    uint32_t regval = ((route & 0xff) << 13) | ((subdest & 7) << 10) |
        (addrbits & 1023);
    /* uint32_t regval = ((route & 0xff) << 13) | ((subdest & 7) << 10) | */
    /*     (((coreid % CORES_PER_QUADRANT) * LUTS_PER_CORE) & 1023); */
    rck_lut0_wr_raw(&rck_own, current_lut, regval);
    /* printf("wrote 0x%x\n", regval); */

    lvaddr_t mem = local_phys_to_mem(XCORE_PADDR_BASE);

    /* Look up modules */
    struct multiboot_modinfo *cpu_region =
        multiboot_find_module("scc/sbin/cpu");
    assert(cpu_region != NULL);
    lvaddr_t cpu_binary = local_phys_to_mem(cpu_region->mod_start);
    size_t cpu_binary_size = MULTIBOOT_MODULE_SIZE(*cpu_region);

    struct multiboot_modinfo *monitor_region =
        multiboot_find_module("scc/sbin/monitor");
    assert(monitor_region != NULL);
    lvaddr_t monitor_binary = local_phys_to_mem(monitor_region->mod_start);
    size_t monitor_binary_size = MULTIBOOT_MODULE_SIZE(*monitor_region);

    struct multiboot_modinfo *init_region =
        multiboot_find_module("scc/sbin/init");
    assert(init_region != NULL);
    lvaddr_t init_binary = local_phys_to_mem(init_region->mod_start);
    size_t init_binary_size = MULTIBOOT_MODULE_SIZE(*init_region);

    struct multiboot_modinfo *memserv_region =
        multiboot_find_module("scc/sbin/mem_serv");
    assert(memserv_region != NULL);
    lvaddr_t memserv_binary = local_phys_to_mem(memserv_region->mod_start);
    size_t memserv_binary_size = MULTIBOOT_MODULE_SIZE(*memserv_region);

    struct multiboot_modinfo *spawnd_region =
        multiboot_find_module("scc/sbin/spawnd");
    assert(spawnd_region != NULL);
    lvaddr_t spawnd_binary = local_phys_to_mem(spawnd_region->mod_start);
    size_t spawnd_binary_size = MULTIBOOT_MODULE_SIZE(*spawnd_region);

    // TODO: This needs to be updated -- it's incorrect
    /* assert(X86_CORE_DATA_PAGES * BASE_PAGE_SIZE + cpu_memory + */
    /*        monitor_binary_size + cpu_binary_size < (1 << 24)); */

    /* Load cpu */
    char *target_mem = (char *)mem;
    struct allocate_state state;
    state.elfbase = elf_virtual_base(cpu_binary);
    state.vbase = target_mem + state.elfbase;
    assert(sizeof(struct x86_core_data) <= BASE_PAGE_SIZE);
    genvaddr_t cpu_entry;
    errval_t err = elf_load(EM_386, elfload_allocate, &state, cpu_binary,
                   cpu_binary_size, &cpu_entry);
    if (err_is_fail(err)) {
        return err;
    }

    // Copy CPU driver binary to target memory
    target_mem += (state.elfbase + elf_virtual_size(cpu_binary)) & (~0xfff);
    genpaddr_t cpu_binary_phys = (genpaddr_t)(target_mem - (char *)mem);
    memcpy(target_mem, (void *)cpu_binary, cpu_binary_size);

    // Copy monitor binary to target memory
    target_mem += (cpu_binary_size & (~0xfff)) + 0x1000;
    genpaddr_t monitor_binary_phys = (genpaddr_t)(target_mem - (char *)mem);
    memcpy(target_mem, (void *)monitor_binary, monitor_binary_size);

    // Copy init binary to target memory
    target_mem += (monitor_binary_size & (~0xfff)) + 0x1000;
    genpaddr_t init_binary_phys = (genpaddr_t)(target_mem - (char *)mem);
    memcpy(target_mem, (void *)init_binary, init_binary_size);

    // Copy mem_serv binary to target memory
    target_mem += (init_binary_size & (~0xfff)) + 0x1000;
    genpaddr_t memserv_binary_phys = (genpaddr_t)(target_mem - (char *)mem);
    memcpy(target_mem, (void *)memserv_binary, memserv_binary_size);

    // Copy spawnd binary to target memory
    target_mem += (memserv_binary_size & (~0xfff)) + 0x1000;
    genpaddr_t spawnd_binary_phys = (genpaddr_t)(target_mem - (char *)mem);
    memcpy(target_mem, (void *)spawnd_binary, spawnd_binary_size);

    // Go to after mem_serv binary image and page-align
    target_mem += (spawnd_binary_size & (~0xfff)) + 0x1000;

    struct diteinfo *core_data = (struct diteinfo *)
        ((char *)mem + state.elfbase - BASE_PAGE_SIZE);
    uint32_t cdbase = state.elfbase - BASE_PAGE_SIZE;
    memset(core_data, 0, sizeof(struct diteinfo));

    core_data->elf.size = sizeof(struct Elf32_Shdr);

    struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_binary;
    core_data->elf.addr = cpu_binary_phys + (uintptr_t)head32->e_shoff;
    core_data->elf.num  = head32->e_shnum;

    // Copy MMAP verbatim
    core_data->mmap_addr = cdbase + __builtin_offsetof(struct diteinfo, mmap);
    core_data->mmap_length = glbl_core_data->mmap_length;
    memcpy(core_data->mmap, (void *)local_phys_to_mem(glbl_core_data->mmap_addr),
           core_data->mmap_length);

    // Copy cmdline verbatim
    char *strpos = core_data->strings;
    core_data->cmdline = cdbase + __builtin_offsetof(struct diteinfo, strings);
    strcpy(strpos, (void *)local_phys_to_mem(glbl_core_data->cmdline));
    strpos += strlen(strpos) + 1;

    // Generate modules
    core_data->mods_count = 5;
    core_data->mods_addr = cdbase + __builtin_offsetof(struct diteinfo, modinfo);

    // CPU driver
    core_data->modinfo[0].mod_start = cpu_binary_phys;
    core_data->modinfo[0].mod_end = cpu_binary_phys + cpu_binary_size;
    strcpy(strpos, "/scc/sbin/cpu");
    core_data->modinfo[0].string = cdbase + (uint32_t)(strpos - (char *)core_data);
    strpos += strlen(strpos) + 1;

    // Monitor
    core_data->modinfo[1].mod_start = monitor_binary_phys;
    core_data->modinfo[1].mod_end = monitor_binary_phys + monitor_binary_size;
    strcpy(strpos, "/scc/sbin/monitor");
    core_data->modinfo[1].string = cdbase + (uint32_t)(strpos - (char *)core_data);
    strpos += strlen(strpos) + 1;

    // init module
    core_data->modinfo[2].mod_start = init_binary_phys;
    core_data->modinfo[2].mod_end = init_binary_phys + init_binary_size;
    strcpy(strpos, "/scc/sbin/init");
    core_data->modinfo[2].string = cdbase + (uint32_t)(strpos - (char *)core_data);
    strpos += strlen(strpos) + 1;

    // mem_serv module
    core_data->modinfo[3].mod_start = memserv_binary_phys;
    core_data->modinfo[3].mod_end = memserv_binary_phys + memserv_binary_size;
    strcpy(strpos, "/scc/sbin/mem_serv");
    core_data->modinfo[3].string = cdbase + (uint32_t)(strpos - (char *)core_data);
    strpos += strlen(strpos) + 1;

    // spawnd module
    core_data->modinfo[4].mod_start = spawnd_binary_phys;
    core_data->modinfo[4].mod_end = spawnd_binary_phys + spawnd_binary_size;
    strcpy(strpos, "/scc/sbin/spawnd");
    core_data->modinfo[4].string = cdbase + (uint32_t)(strpos - (char *)core_data);
    strpos += strlen(strpos) + 1;

    core_data->start_free_ram = glbl_core_data->start_free_ram;
    core_data->urpc_frame_base = urpcframe_base;
    core_data->urpc_frame_bits = urpcframe_bits;
    core_data->src_core_id       = my_core_id;
    core_data->chan_id           = chanid;

    /* // Write back all caches */
    wbinvd();
    flush_l2_cache();

    // Start core
    rck_gcbcfg_t gcbcfg = rck_gcbcfg_rd(&rck[tile]);
    if(core) {
        gcbcfg.resc1 = 0;
        gcbcfg.resl21 = 0;
    } else {
        gcbcfg.resc0 = 0;
        gcbcfg.resl20 = 0;
    }
    rck_gcbcfg_wr(&rck[tile], gcbcfg);

    return 0;
}
