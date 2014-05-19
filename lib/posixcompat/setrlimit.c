#include <sys/resource.h>
#include <assert.h>
#include <string.h>
#include "posixcompat.h"

int setrlimit(int resource, const struct rlimit *rlim)
{
    POSIXCOMPAT_DEBUG("setrlimit(%d, %p) ignored.\n", resource, rlim);
    return 0;
}

int getrlimit(int resource, struct rlimit *rlim)
{
    static struct rlimit infty = {
        .rlim_cur = RLIM_INFINITY,
        .rlim_max = RLIM_INFINITY,
    };

    POSIXCOMPAT_DEBUG("getrlimit(%d, %p) always returns infinity.\n", resource, rlim);
    *rlim = infty;
    return 0;
}

int getrusage(int who, struct rusage *usage)
{
    // XXX: No fields are supported
    memset(usage, 0, sizeof(struct rusage));
    return 0;
}
