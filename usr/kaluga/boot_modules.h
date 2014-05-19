#ifndef BOOT_MODULES_H_
#define BOOT_MODULES_H_

#include <barrelfish/barrelfish.h>

struct module_info;
typedef errval_t(*module_start_fn)(coreid_t where, struct module_info* mi,
        char* record);

struct module_info {
    char* complete_line;
    char* path;
    char* binary;

    char* cmdargs; // Used for pointers in argv
    int argc;
    char* argv[MAX_CMDLINE_ARGS + 1];

    module_start_fn start_function;
    domainid_t did;
};


void init_environ(void);
errval_t init_boot_modules(void);
struct module_info* find_module(char*);
bool is_started(struct module_info*);
bool is_auto_driver(struct module_info*);
void set_start_function(char*, module_start_fn);

#endif /* BOOT_MODULES_H_ */
