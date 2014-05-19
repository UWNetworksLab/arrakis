/**
 * \file
 * \brief internal functions
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERNAL_H_
#define INTERNAL_H_

#define SERVICE_BASENAME    "spawn" // the core ID is appended to this
#define ALL_SPAWNDS_UP 	    "all_spawnds_up"

extern coreid_t my_core_id;
extern bool is_bsp_core;
extern const char *gbootmodules;

// const char *get_shortname(const char *start, const char *nameend, 
//                           size_t *namelen);
// bool spawn_domain(const char *name);
void bsp_bootup(const char *bootmodules, int argc, const char *argv[]);
//errval_t spawn(char *path, char *const argv[], char *const envp[]);
errval_t start_service(void);

#endif //INTERNAL_H_
