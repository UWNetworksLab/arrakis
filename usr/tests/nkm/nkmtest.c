#include <barrelfish/barrelfish.h>
#include <barrelfish/cap_predicates.h>
#include <stdio.h>
#include "vspace_dump.h"

int main(int argc, char *argv[])
{
    errval_t err;
    struct capref mem;
    printf("ram_alloc\n");
    err = ram_alloc(&mem, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        printf("ram_alloc: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    for(int i = 0; i < 100; i++) {
        thread_yield();
    }

    struct capref frame;
    printf("retype\n");
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        printf("slot_alloc: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }
    err = cap_retype(frame, mem, ObjType_Frame, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        printf("cap_retype: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    printf("delete ram cap\n");
    err = cap_destroy(mem);
    if (err_is_fail(err)) {
        printf("cap_delete(mem): %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    struct frame_identity fi;
    err = invoke_frame_identify(frame, &fi);
    if (err_is_fail(err)) {
        printf("frame_identify: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }
    printf("frame: base = 0x%zx, bits = %d\n", fi.base, fi.bits);

    dump_page_tables();


    struct vregion *vr;
    struct memobj *memobj;
    void *vaddr;
    printf("map\n");
    vspace_map_one_frame(&vaddr, BASE_PAGE_SIZE, frame, &memobj, &vr);
    char *memory = vaddr;
    printf("vaddr = %p\n", vaddr);

    dump_page_tables();

    printf("write 1\n");
    int i;
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        memory[i] = i % INT8_MAX;
    }
    printf("verify 1\n");
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        assert(memory[i] == i % INT8_MAX);
    }

    printf("delete frame cap\n");
    cap_destroy(frame);
    if (err_is_fail(err)) {
        printf("cap_delete(frame): %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    dump_page_tables();
    err = debug_dump_hw_ptables();
    if (err_is_fail(err)) {
        printf("kernel dump ptables: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    printf("write 2\n");
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        memory[i] = i % INT8_MAX;
    }
    printf("verify 2\n");
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        assert(memory[i] == i % INT8_MAX);
    }

    printf("exit\n");
    return 0;
}
