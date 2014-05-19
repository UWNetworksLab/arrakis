/** \file
 * \brief Chips framework types
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef FRAMEWORK_H_
#define FRAMEWORK_H_

#include "dictionary.h"

struct service_registration;

/**
 * \brief a service reference is a handle to a service
 *      other than this, it can be used to examine the properties
 *      of a service.
 */
struct service_reference {
    char* iface;                        ///< the name of the service interface
    struct dictionary *dict;                 ///< the service properties
    iref_t service;                     ///< the iref of the service
};

/**
 * \brief a service registration is the handle that the
 *      process gets that registered the service. It's required
 *      to unregister a service and change the properties at runtime.
 */
struct service_registration {
    struct service_reference *ref;     ///< the service reference
};

#endif /*FRAMEWORK_H_*/
