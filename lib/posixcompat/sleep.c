#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <assert.h>

int usleep(useconds_t usec)
{
    errval_t err = barrelfish_usleep(usec);
    assert(err_is_ok(err));
    return 0;
}

unsigned int sleep(unsigned int seconds)
{
    return usleep((useconds_t)seconds * 1000000);
}

int nanosleep(const struct timespec *req, struct timespec *rem)
{
    assert(!"NYI");
}
