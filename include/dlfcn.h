/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

// Barrelfish does not support dynamic libraries, however you can provide
// an illusion to your program using these compatibility functions. You
// would statically link all the code and register each of the function
// pointers. This implementation only supports a single mapping.
//
// Example usage:
//   // Signatures of functions in dynamic library.
//   int foo(char* name);
//   void bar(int x, int y);
//   double baz(double z);
//
//   // Note: with C++ you will also need to explicitly cast the function
//   // pointers to void*.
//   static struct function_entry mylib[] = {
//     {"foo", foo},
//     {"bar", bar},
//     {"baz", baz}
//   };
//
//   int main() {
//     dlopen_set_params(mylib, sizeof(mylib) / sizeof(*mylib));
//     ...
//   }

#ifndef DLFCN_H_
#define DLFCN_H_

#include <sys/cdefs.h>

__BEGIN_DECLS

#define RTLD_NOW 1
#define RTLD_LAZY 2

struct function_entry {
    const char *name;
    void *f;
};

void dlopen_set_params(struct function_entry *fk, int nrk);
void *dlopen(const char *filename, int flags);
void *dlsym(void *handle, const char *symbol);
char *dlerror(void);
int dlclose(void *handle);

__END_DECLS

#endif // DLFCN_H_
