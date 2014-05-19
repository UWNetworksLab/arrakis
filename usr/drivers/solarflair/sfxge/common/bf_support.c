

#include "bf_support.h"



// *******************************************************************
// Vary basic implementation of locking
// To be moved into separate locking implementation file
// *******************************************************************
void mtx_lock(struct mtx *mp)
{
    if(mp->state == 1) {
        printf("mutex already locked!\n");
        abort();
    }
    mp->state = 1; // lock it
} // end function: mtx_lock

void mtx_unlock(struct mtx *mp)
{
    if(mp->state == 0) {
        printf("mutex not locked!\n");
        abort();
    }
    mp->state = 0; // lock it
} // end function: mtx_lock


// **********************************************************
// bus related functions
// **********************************************************
static __inline uint32_t
ia64_ld4(uint32_t *p)
{
    assert(!"NYI");
    uint32_t v =0;
//    __asm volatile("ld4 %0=[%1];;": "=r"(v): "r"(p));
    return (v);
}

static __inline void
ia64_st4(uint32_t *p, uint32_t v)
{
    assert(!"NYI");
//    __asm volatile("st4 [%0]=%1;;": "r"(p): "r"(v));
}


__inline uint32_t
bus_space_read_4(bus_space_tag_t bst, bus_space_handle_t bsh, bus_size_t ofs)
{

    uint32_t val = 0;
    if (bst == 0) {
        assert(!"NYI");
        abort();
    }
    val = ia64_ld4((void *) (bsh + ofs));
    return val;
}

__inline void
bus_space_write_4(bus_space_tag_t bst, bus_space_handle_t bsh, bus_size_t ofs,
        uint8_t val)
{
    if (bst == 0) {
        assert(!"NYI");
        abort();
    }
    ia64_st4((void *) (bsh + ofs), val);
}

// FIXME: temperary hack to enable compilation of sfxge_err function call
// Ideally, it should be implemented by driver code using common library.
struct __efsys_identifier_s;
typedef struct __efsys_identifier_s    efsys_identifier_t;

void sfxge_err(efsys_identifier_t *arg, unsigned int code, uint32_t dword0,
    uint32_t dword1);

void
sfxge_err(efsys_identifier_t *arg, unsigned int code, uint32_t dword0,
    uint32_t dword1)
{
        assert(!"NYI");
}

