#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <string.h>
#include <ahci/ahci_dma_pool.h>

#define MEG 1048576

int main(int argc, char **argv)
{
    errval_t err;
    printf("ahci alloc test: starting\n");

    ahci_dma_pool_init(8*MEG);

    struct ahci_dma_region *ahciregion;

    for (int j = 0; j < 5; j++) {
        printf("==================\niteration %d\n==================\n", j);
        err = ahci_dma_region_alloc(MEG, &ahciregion);

        if (err_is_fail(err)) {
            printf("ahci alloc test: ahci_alloc_dma_region failed: %s\n", err_getstring(err));
            return 1;
        }

        printf("1\n");

        uint32_t *p = (uint32_t *)ahciregion->vaddr;

        printf("2\n");

        for (int32_t i = 0; i < MEG/4; i++) {
            p[i] = 0x00C0FFEE + i*1024;
        }

        printf("3\n");

        for (int32_t i = 0; i < MEG/4; i++) {
            assert(p[i] == 0x00C0FFEE + i*1024);
        }

        printf("4\n");

        err = ahci_dma_region_free(ahciregion);

        printf("5\n");

        if (err_is_fail(err)) {
            printf("ahci alloc test: ahci_free_region failed: %s\n", err_getstring(err));
            return 2;
        }
        printf("\n");
    }

    printf("ahci alloc test: successful!\n");

    return 0;
}
