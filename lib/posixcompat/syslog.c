#include <syslog.h>
#include <assert.h>

void syslog(int priority, const char *format, ...)
{
    assert(!"NYI");
}

void openlog(const char *ident, int option, int facility)
{
    assert(!"NYI");
}
