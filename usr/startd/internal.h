/**
 * \file
 * \brief startd internal functions
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERNAL_H_
#define INTERNAL_H_

extern const char *gbootmodules;

void spawn_dist_domains(void);
void spawn_app_domains(void);
void spawn_bootscript_domains(void);
void spawn_arrakis_domains(void);

#endif //INTERNAL_H_
