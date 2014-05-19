#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h> // sys_debug_flush_cache()
#include <barrelfish/threads.h>
#include <barrelfish/except.h>
#include <stdio.h>

static void *vbase = NULL;

#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];

static void handler(enum exception_type type, int subtype, void *addr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    debug_printf("got exception %d(%d) on %p\n", type, subtype, addr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    assert(addr == vbase);
    debug_printf("got expected write pagefault on %p\n", addr);
    // exit program
    exit(0);
}

int main(void)
{
    struct capref frame;
    errval_t err;
    size_t retsize;
    err = frame_alloc(&frame, BASE_PAGE_SIZE, &retsize);
    assert(err_is_ok(err));
    // map read-write
    struct memobj *memobj;
    struct vregion *vregion;
    err = vspace_map_anon_attr(&vbase, &memobj, &vregion, retsize, &retsize,
            VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
    err = memobj->f.fill(memobj, 0, frame, retsize);
    assert(err_is_ok(err));
    err = memobj->f.pagefault(memobj, vregion, 0, 0);
    assert(err_is_ok(err));
    assert(vbase);
    unsigned char *base = vbase;
    debug_printf("filling region %p\n", base);
    for (int i = 0; i < retsize; i++) {
        base[i] = i % 255;
    }
    sys_debug_flush_cache();
    debug_printf("checking region %p\n", base);
    // check
    for (int i = 0; i < retsize; i++) {
        if (base[i] != i % 255) {
            debug_printf("failed at %d\n", i);
        }
        assert(base[i] == i % 255);
    }
    // change region to read only
    debug_printf("changing region %p perms to readonly\n", base);
    err = memobj->f.protect(memobj, vregion, 0, retsize, VREGION_FLAGS_READ);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "protect");
        return 1;
    }
    // check
    debug_printf("checking region %p\n", base);
    for (int i = 0; i < retsize; i++) {
        assert(base[i] == i % 255);
    }
    // write should pagefault, so we register a handler
    err = thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));

    // this should fault
    debug_printf("provoke write pagefault on %p\n", base);
    base[0] = 0x42;

    assert(!"reached");

    return 0;
}
