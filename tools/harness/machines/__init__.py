##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import socket, os

class Machine(object):
    name = None # should be overridden

    def __init__(self, options):
        pass

    def get_bootarch(self):
        """Return the architecture for booting and base system services."""
        return "x86_64" # old default!

    def get_buildarchs(self):
        """Return the architectures that must be enabled in hake for this machine."""
        return [self.get_bootarch()]

    def get_ncores(self):
        """Returns absolute number of cores."""
        raise NotImplementedError

    def get_coreids(self):
        """Returns the list of core IDs."""
        return range(0, self.get_ncores()) # default behaviour for x86

    # XXX: REMOVE ME. used only by lwip_loopback bench
    def get_cores_per_socket(self):
        """Returns number of cores per socket."""
        raise NotImplementedError

    def get_tickrate(self):
        """Returns clock rate in MHz."""
        raise NotImplementedError

    def get_perfcount_type(self):
        """Returns a string ('amd0f', 'amd10', or 'intel'), or None if unknown"""
        return None

    def get_kernel_args(self):
        """Returns list of machine-specific arguments to add to the kernel command-line"""
        return None

    def get_boot_timeout(self):
        """Returns a machine-specific timeout (in seconds), or None for the default"""
        return None

    def get_tftp_dir(self):
        """Return a unique TFTP boot directory for this machine."""
        raise NotImplementedError

    def set_bootmodules(self, modules):
        """Set the machine to boot from the given module data."""
        raise NotImplementedError

    def lock(self):
        """Lock the machine to prevent concurrent access."""
        raise NotImplementedError

    def unlock(self):
        """Unlock an already-locked machine."""
        raise NotImplementedError

    def setup(self):
        """Prepare a locked machine to be booted."""
        raise NotImplementedError

    def reboot(self):
        """Reboot (or boot) the machine."""
        raise NotImplementedError

    def shutdown(self):
        """Shut down/switch off the machine."""
        raise NotImplementedError

    def get_output(self):
        """Returns a file object to the output of a locked machine."""
        raise NotImplementedError

class MachineLockedError(Exception):
    """May be raised by lock() when the machine is locked by another user."""
    pass


all_machines = []

def add_machine(machine):
    all_machines.append(machine)
    return machine

# Assume that QEMU, pandaboard and Gem5 work everywhere if invoked
import qemu
import gem5
import pandaboard

# Other site-specific modules will be loaded by the siteconfig module
