/**
 * \file
 * \brief Contains definitions that need to be global.
 *
 * Currently only max query length define which we need on
 * server and client.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_DEFINITIONS_H_
#define OCTOPUS_DEFINITIONS_H_

#define MAX_QUERY_LENGTH (5*1024)


//
// Mode Bitmask (used to distinguish async events sent from server)
//

// Be aware: If you change those, adjust the trigger code in prolog as well!
#define OCT_ON_SET     (0x1)       /*!< Trigger checked for set events. */
#define OCT_ON_DEL     (0x1 << 1)  /*!< Trigger checked for del events. */
#define OCT_ON_PUBLISH (0x1 << 5)  /*!< Is Publish/Subscribe Event. */
#define OCT_PERSIST    (0x1 << 2)  /*!< Trigger installed until it is removed. */
#define OCT_ALWAYS_SET (0x1 << 3)  /*!< Provided error is ignored (trigger
                                         is always installed). */
#define OCT_REMOVED    (0x1 << 4)  /*!< Indicates that the trigger has been
                                         removed. Cleaning up any state for
                                         this trigger is safe in case this
                                         flag is set. */

#endif /* OCTOPUS_DEFINITIONS_H_ */
