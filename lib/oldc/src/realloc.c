/*
 * K&R Malloc 
 *
 * System specifc code should implement `more_core'
 */

#include "k_r_malloc.h"
#include <stdlib.h>
#include <string.h>

typedef void *(*alt_realloc_t)(void *p, size_t bytes);
alt_realloc_t alt_realloc = NULL;

void *
realloc(void *ptr, size_t size)
{
    if (alt_realloc != NULL) {
        return alt_realloc(ptr, size);
    }

	Header *bp;
	void *new_ptr;
	size_t old_size;

	if (ptr == NULL)
		return malloc(size);
	bp = (Header *) ptr - 1; /* point to block header */
	old_size = sizeof(Header) * (bp->s.size - 1);
	new_ptr = malloc(size);
	if (new_ptr == NULL) {
		return NULL;
	}
	if (old_size <= size) {
		memcpy(new_ptr, ptr, old_size);
	} else {
		memcpy(new_ptr, ptr, size);
	}
	free(ptr);
	return new_ptr;
}
