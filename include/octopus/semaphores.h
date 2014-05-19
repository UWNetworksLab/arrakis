/**
 * \file
 * \brief Header file for the octopus semaphores API.
 *
 * This API was written to replace the semaphore API
 * in chips in order to support posixcompat.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_SEMAPHORES_H_
#define OCTOPUS_SEMAPHORES_H_

errval_t oct_sem_new(uint32_t*, size_t);
errval_t oct_sem_post(uint32_t);
errval_t oct_sem_wait(uint32_t);
errval_t oct_sem_trywait(uint32_t);

#endif /* OCTOPUS_SEMAPHORES_H_ */
