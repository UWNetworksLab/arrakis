#include <inttypes.h>
#include <rcce/RCCE.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>

int RCCE_APP(int argc, char **argv)
{
    int ME;

    RCCE_init(&argc, &argv);
    //  RCCE_debug_set(RCCE_DEBUG_ALL);

    ME = RCCE_ue();
    printf("Core %d passed RCCE_init\n", ME);

    uint64_t oldtsc = rdtsc();
    for(;;) {
        uint64_t tsc = rdtsc();
        if(tsc - oldtsc > 500000000ULL) {
            printf("%" PRIu64 "\n", tsc);
        }
    }

    return 0;
}
