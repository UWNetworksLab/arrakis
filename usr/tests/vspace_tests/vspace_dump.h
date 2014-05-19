#ifndef __VSPACE_DUMP_H
#define __VSPACE_DUMP_H

#include <barrelfish/barrelfish.h>

void dump_my_vregions(void);

void dump_pmap(struct pmap *pmap);

void dump_page_tables(void);

#endif // __VSPACE_DUMP_H
