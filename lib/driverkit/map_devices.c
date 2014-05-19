/**
 * \brief Memory management helper functions for device drivers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>

#include <driverkit/driverkit.h>

#define UNBITS_GENPA(bits) (((genpaddr_t)1) << (bits))

/**
 * \brief Maps device register with the capabilities provided by the
 * argcn slot.
 *
 * The function is used mostly as a helper to map registers by programs
 * that were spawned by Kaluga.
 *
 * \param[in] address The address of the device region you want to map.
 * \param[in] size The size of the region.
 * \param[out] return_address The virtual memory address where the region
 * was mapped at.
 *
 * \retval SYS_ERR_OK Mapping was succesful.
 */
errval_t map_device_register(lpaddr_t address, size_t size, lvaddr_t *return_address)
{
    errval_t err;
    struct capref argcn = {
        .cnode = cnode_root,
        .slot = TASKCN_SLOT_ARGSPAGE
    };

    size_t bits = 8; // TODO(gz): How do I figure this value out on the fly?
    struct capref device_cap_iter = {
        .cnode = build_cnoderef(argcn, bits),
        .slot = 0
    };

    for (; device_cap_iter.slot < (((capaddr_t)1) << device_cap_iter.cnode.size_bits); 
         device_cap_iter.slot++) {
        // Get cap data
        struct capability cap;
        err = debug_cap_identify(device_cap_iter, &cap);
        // If cap type was Null, kernel returns error
        if (err_no(err) == SYS_ERR_IDENTIFY_LOOKUP ||
            err_no(err) == SYS_ERR_CAP_NOT_FOUND ||
            err_no(err) == SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP) {
            continue;
        }

        struct frame_identity fid;
        err = invoke_frame_identify(device_cap_iter, &fid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failure in invoke_frame_identify");
            return err;
        }
        assert(err_is_ok(err));
        
        lpaddr_t address_base = address & ~(BASE_PAGE_SIZE-1);
        lpaddr_t offset = address & (BASE_PAGE_SIZE-1);
        // XXX: should be address+size <= ...
        // Need to add proper register size
        if (address_base >= fid.base &&
                (address_base + size) <= (fid.base + UNBITS_GENPA(fid.bits))) {
            void* frame_base;
            err = vspace_map_one_frame_attr(&frame_base, size,
                                            device_cap_iter, VREGION_FLAGS_READ_WRITE_NOCACHE,
                                            NULL, NULL);
            *return_address = (lvaddr_t)frame_base + offset;
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Failure in vspace_map_one_frame_attr.\n");
            }
            return err;
        }
    }

    return DRIVERKIT_NO_CAP_FOUND;
}
