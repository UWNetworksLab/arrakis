#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static
void swap(void *base, size_t i, size_t j, size_t size)
{
	void *tmp = malloc(size);

	assert(tmp);

	memcpy(tmp, (char*) base + i * size, size);
	memmove((char*) base + i * size, (char*) base + j * size, size);
	memcpy((char*) base + j * size, tmp, size);
	free(tmp);
}

/* qsort: sort v[left]...v[right] into increasing order */
void
qsort(void *base, size_t nmemb, size_t size, 
      int(*compar)(const void *, const void *))
{
	int i, last;

	if (nmemb <= 1)
		return;

	swap(base, 0, nmemb / 2, size);

	last = 0;
	for (i = 1; i < nmemb; i++)
		if (compar((char*) base + (i * size), base) < 0)
			swap(base, i, ++last, size);

	swap(base, 0, last, size);

	qsort(base, last, size, compar);
	qsort((char*) base + (last + 1) * size, nmemb - last - 1, size, compar);
}
