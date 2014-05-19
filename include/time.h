#ifndef __BF_TIME_H
#define	__BF_TIME_H

/* This just wraps and builds upon the default header in libc. */
#include_next <time.h>

#include <sys/cdefs.h>

__BEGIN_DECLS

/* This is a non-standard GNU extension implemented in libposixcompat. */
#if defined(_GNU_SOURCE) || defined(_BSD_SOURCE)
time_t timegm(struct tm *);
#endif

__END_DECLS

int nanosleep(const struct timespec *req, struct timespec *rem);

#endif /* __BF_TIME_H */
