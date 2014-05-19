/*
Copyright (c) 2002 Peter O'Gorman <ogorman@users.sourceforge.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/* Just to prove that it isn't that hard to add Mac calls to your code :)
   This works with pretty much everything, including kde3 xemacs and the gimp,
   I'd guess that it'd work in at least 95% of cases, use this as your starting
   point, rather than the mess that is dlfcn.c, assuming that your code does not
   require ref counting or symbol lookups in dependent libraries
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include "dlfcn_simple.h"

#define ERR_STR_LEN 256

static void *dlsymIntern(void *handle, const char *symbol);

static const char *error(int setget, const char *str, ...);



/* Set and get the error string for use by dlerror */
static const char *error(int setget, const char *str, ...)
{
	static char errstr[ERR_STR_LEN];
	static int err_filled = 0;
	const char *retval;
	va_list arg;
	if (setget == 0)
	{
		va_start(arg, str);
		strncpy(errstr, "dlcompat: ", ERR_STR_LEN);
		vsnprintf(errstr + 10, ERR_STR_LEN - 10, str, arg);
		va_end(arg);
		err_filled = 1;
		retval = NULL;
	}
	else
	{
		if (!err_filled)
			retval = NULL;
		else
			retval = errstr;
		err_filled = 0;
	}
	return retval;
}

/* dlopen */
void *dlopen(const char *path, int mode)
{
	void *module = 0;
	NSObjectFileImage ofi = 0;
	NSObjectFileImageReturnCode ofirc;

	/* If we got no path, the app wants the global namespace, use -1 as the marker
	   in this case */
	if (!path)
		return (void *)-1;

	/* Create the object file image, works for things linked with the -bundle arg to ld */
	ofirc = NSCreateObjectFileImageFromFile(path, &ofi);
	switch (ofirc)
	{
		case NSObjectFileImageSuccess:
			/* It was okay, so use NSLinkModule to link in the image */
			module = NSLinkModule(ofi, path,
								  NSLINKMODULE_OPTION_RETURN_ON_ERROR
								  | (mode & RTLD_GLOBAL) ? 0 : NSLINKMODULE_OPTION_PRIVATE
								  | (mode & RTLD_LAZY) ? 0 : NSLINKMODULE_OPTION_BINDNOW);
			NSDestroyObjectFileImage(ofi);
			break;
		case NSObjectFileImageInappropriateFile:
			/* It may have been a dynamic library rather than a bundle, try to load it */
			module = (void *)NSAddImage(path, NSADDIMAGE_OPTION_RETURN_ON_ERROR);
			break;
		default:
			/* God knows what we got */
			error(0, "Can not open \"%s\"", path);
			return 0;
	}
	if (!module)
		error(0, "Can not open \"%s\"", path);
	return module;

}

/* dlsymIntern is used by dlsym to find the symbol */
void *dlsymIntern(void *handle, const char *symbol)
{
	NSSymbol *nssym = 0;
	/* If the handle is -1, if is the app global context */
	if (handle == (void *)-1)
	{
		/* Global context, use NSLookupAndBindSymbol */
		if (NSIsSymbolNameDefined(symbol))
		{
			nssym = NSLookupAndBindSymbol(symbol);
		}

	}
	/* Now see if the handle is a struch mach_header* or not, use NSLookupSymbol in image
	   for libraries, and NSLookupSymbolInModule for bundles */
	else
	{
		/* Check for both possible magic numbers depending on x86/ppc byte order */
		if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
			(((struct mach_header *)handle)->magic == MH_CIGAM))
		{
			if (NSIsSymbolNameDefinedInImage((struct mach_header *)handle, symbol))
			{
				nssym = NSLookupSymbolInImage((struct mach_header *)handle,
											  symbol,
											  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
											  | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
			}

		}
		else
		{
			nssym = NSLookupSymbolInModule(handle, symbol);
		}
	}
	if (!nssym)
	{
		error(0, "Symbol \"%s\" Not found", symbol);
		return NULL;
	}
	return NSAddressOfSymbol(nssym);
}

const char *dlerror(void)
{
	return error(1, (char *)NULL);
}

int dlclose(void *handle)
{
	if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
		(((struct mach_header *)handle)->magic == MH_CIGAM))
	{
		error(0, "Can't remove dynamic libraries on darwin");
		return 0;
	}
	if (!NSUnLinkModule(handle, 0))
	{
		error(0, "unable to unlink module %s", NSNameOfModule(handle));
		return 1;
	}
	return 0;
}


/* dlsym, prepend the underscore and call dlsymIntern */
void *dlsym(void *handle, const char *symbol)
{
	static char undersym[257];	/* Saves calls to malloc(3) */
	int sym_len = strlen(symbol);
	void *value = NULL;
	char *malloc_sym = NULL;

	if (sym_len < 256)
	{
		snprintf(undersym, 256, "_%s", symbol);
		value = dlsymIntern(handle, undersym);
	}
	else
	{
		malloc_sym = malloc(sym_len + 2);
		if (malloc_sym)
		{
			sprintf(malloc_sym, "_%s", symbol);
			value = dlsymIntern(handle, malloc_sym);
			free(malloc_sym);
		}
		else
		{
			error(0, "Unable to allocate memory");
		}
	}
	return value;
}
