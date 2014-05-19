/**
 * \file
 * \brief Argument processing
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARGS_H__
#define __ARGS_H__

struct args {
    char *path;
    coreid_t *cores;
    int cores_len;
    coreid_t *exclude;
    int exclude_len;
    int num_cores;
    bool all_cores;
    bool master;
    genpaddr_t ram;
};

struct args process_args(int argc, char *argv[]);
char *list_to_string(coreid_t *list, size_t l_len);

#endif

