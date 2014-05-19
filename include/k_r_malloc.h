#ifndef _LIBC_K_R_MALLOC_H_
#define _LIBC_K_R_MALLOC_H_

#include <sys/cdefs.h>

__BEGIN_DECLS

#define NALLOC  0x10000		/* minimum #units to request */

typedef long long Align;	/* for alignment to long long boundary */

union header {			/* block header */
	struct {
		union header   *ptr;	/* next block if on free list */
		unsigned        size;	/* size of this block */
	} s;
	Align           x;	/* force alignment of blocks */
};

typedef union header Header;

Header  *morecore(unsigned nu);
void lesscore(void);
void __free_locked(void *ap);
void __malloc_init(void*, void*);

__END_DECLS

#endif /* _LIBC_K_R_MALLOC_H_ */
