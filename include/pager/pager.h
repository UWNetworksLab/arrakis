#ifndef __PAGER_H
#define __PAGER_H

#include <sys/cdefs.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>

__BEGIN_DECLS

errval_t pager_install_handler(char *ex_stack, size_t stack_size);

__END_DECLS

#endif // __PAGER_H
