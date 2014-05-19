#include <pager/pager.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    errval_t err;
    err = pager_install_handler(NULL, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pager_install_handler");
    }

    // region size
    size_t size = 16384u;
    // create dummy memobj
    struct memobj_one_frame *m = malloc(sizeof(struct memobj_one_frame));
    assert(m);
    err = memobj_create_one_frame(m, size, 0);
    // figure out address
    genvaddr_t address;
    struct pmap *pmap = get_current_pmap();
    err = pmap->f.determine_addr(pmap, &m->m, 4096u, &address);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_DETERMINE_ADDR);
    }

    printf("non-mapped region @0x%"PRIxGENVADDR"\n", address);

    char *buf = (char*)vspace_genvaddr_to_lvaddr(address);

    printf("writing to not-yet-mapped region\n");
    for (int i = 0; i < size; i++) {
        buf[i] = i % 0xff;
    }
    printf("checking region\n");
    for (int i = 0; i < size; i++) {
        assert(buf[i] == i % 0xff);
    }

    printf("pagertest completed successfully!\n");

    return 0;
}
