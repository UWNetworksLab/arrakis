#ifndef KALUGA_H_
#define KALUGA_H_

#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>

#include "queue.h"
#include "debug.h"

#define TRIGGER_ALWAYS (OCT_PERSIST | OCT_ON_SET | OCT_ON_DEL | OCT_ALWAYS_SET)
#define BSP_CORE_ID 0

extern coreid_t my_core_id;
extern uint32_t my_arch_id;

errval_t trigger_existing_and_watch(const char*,
        trigger_handler_fn, void*,  octopus_trigger_id_t*);


#include "boot_modules.h"
#include "start_pci.h"
#include "start_cpu.h"
#include "driver_startup.h"
#include "device_caps.h"
#include "omap_startup.h"

#endif /* KALUGA_H_ */
