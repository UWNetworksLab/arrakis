/**
 * \file
 * \brief Prototypes of the functions to be called from main
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DATAGATHERER_H_
#define DATAGATHERER_H_

void gather_cpuid_data(coreid_t core_id);
void gather_rtt_data(struct monitor_binding *st);
void gather_nr_running_cores(struct monitor_binding *st);


#endif // DATAGATHERER_H_
