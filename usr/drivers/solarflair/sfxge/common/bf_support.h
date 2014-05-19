#ifndef    _BF_SUPPORT_SF__H
#define    _BF_SUPPORT_SF__H

#include <barrelfish/barrelfish.h>
#include <stdio.h>

#include <sys/endian.h>  // to find out endianess of machine
#include <sys/times.h>
#include <sys/param.h>
#include <string.h>  // for memcpy, memset

#include "error_list.h"

// FIXME: added for compilation purpose.  Replace by using appropriate
// Barrelfish constants for them.
#define FALSE (0)
#define TRUE (1)

// FIXME: may be I should replace all boolean_t with bool
typedef bool boolean_t;
typedef char *caddr_t;

// **********************************************************
// bus related data-types
// **********************************************************

typedef int bus_space_tag_t;
typedef unsigned long bus_space_handle_t;
typedef unsigned long bus_size_t;
typedef unsigned long bus_addr_t;

uint32_t bus_space_read_4(bus_space_tag_t bst, bus_space_handle_t bsh,
        bus_size_t ofs);

void bus_space_write_4(bus_space_tag_t bst, bus_space_handle_t bsh,
        bus_size_t ofs, uint8_t val);

// **********************************************************
// basic locking data-structure
// **********************************************************

struct mtx {
    int state; // 0: unlocked, 1: locked
};

void mtx_lock(struct mtx *mp);
void mtx_unlock(struct mtx *mp);

// *****************************************************************
// ASSERT
// *****************************************************************

#define panic(x...)    do {                                             \
                            printf("panic:sf: " x);                     \
                            abort();                                    \
                    } while(0)

//#define    myKASRT(_exp, msg) do {
#define    KASSERT(_exp, msg) do {                                      \
        if (!(_exp)) {                                                  \
            printf(msg);                                                \
            printf("\n");                                               \
            abort();                                                    \
        }                                                               \
    } while (0)


#define    EFSYS_ASSERT(_exp) do {                                      \
    if (!(_exp))                                                        \
        panic(#_exp);                                                   \
    } while (0)

#define EFSYS_ASSERT3(_x, _op, _y, _t) do {                             \
    const _t __x = (_t)(_x);                                            \
    const _t __y = (_t)(_y);                                            \
    if (!(__x _op __y))                                                 \
          panic("assertion failed at %s:%u", __FILE__, __LINE__);       \
    } while(0)

#define DELAY(_us)      do {                                            \
         (void) (_us);                                                  \
        assert(!"NYI");                                                 \
    } while(0)


#endif //   _BF_SUPPORT_SF__H
