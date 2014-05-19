/*
Author: Ben Leslie <benjl@cse.unsw.edu.au>
*/

#include <stdio.h>
#include "format.h"

#include <assert.h>

int
snprintf(char *s, size_t size, const char *format, ...)
{
	int ret;
	va_list ap;
	
	va_start(ap, format);
	ret = vsnprintf(s, size, format, ap);
	va_end(ap);
	return ret;
}
