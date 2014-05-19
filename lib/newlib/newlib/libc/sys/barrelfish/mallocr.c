
/* provide dummy reentrant versions for malloc functions */
#include <reent.h>
#include <stdlib.h>

void *_malloc_r(struct _reent *ptr, size_t size)
{
    return malloc(size);
}

void _free_r(struct _reent *r, void *p)
{
    free(p);
}

void *_realloc_r(struct _reent *r, void *ptr, size_t size)
{
    return realloc(ptr, size);
}

void *_calloc_r(struct _reent *r, size_t nmemb, size_t size)
{
    return calloc(nmemb, size);
}
