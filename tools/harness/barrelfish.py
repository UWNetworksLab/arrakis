##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os.path
import builds

class BootModules(object):
    """Modules to boot (ie. the menu.lst file)"""

    def __init__(self, machine):
        self.hypervisor = None
        self.kernel = (None, [])
        self.modules = []
        self.machine = machine

    def set_kernel(self, kernel, args=None):
        self.kernel = (kernel, args if args else [])

    def add_kernel_arg(self, arg):
        self.kernel[1].append(arg)

    def set_hypervisor(self, h):
        self.hypervisor = h

    # does modulespec describe modulename?
    def _module_matches(self, modulename, modulespec):
        if '/' in modulespec: # if the spec contains a path, they must be the same
            return modulespec == modulename
        else: # otherwise, we look at the last part of the name only
            return modulespec == modulename.rsplit('/',1)[-1]

    def add_module(self, module, args=None):

        # Support for build targets with / in their name (e.g. examples/xmpl-spawn)
        module = module.replace('$BUILD', os.path.dirname(self.kernel[0]))

        # XXX: workaround for backwards compatibility: prepend default path
        if not '/' in module:
            assert self.kernel
            module = os.path.join(os.path.dirname(self.kernel[0]), module)
        elif module.startswith('/'):
            # XXX: workaround to allow working around the previous workaround
            module = module[1:]
        self.modules.append((module, args if args else []))

    def add_module_arg(self, modulename, arg):
        for (mod, args) in self.modules:
            if self._module_matches(mod, modulename):
                args.append(arg)

    def del_module(self, name):
        self.modules = [(mod, arg) for (mod, arg) in self.modules
                                   if not self._module_matches(mod, name)]

    def reset_module(self, name, args=[]):
        self.del_module(name)
        self.add_module(name, args)

    def get_menu_data(self, path):
        assert(self.kernel[0])
        r = "timeout 0\n"
        r += "title Harness image\n"
        r += "root (nd)\n"
        if self.hypervisor:
            r += "hypervisor %s\n" % os.path.join(path, self.hypervisor)
        r += "kernel %s %s\n" % (
                os.path.join(path, self.kernel[0]), " ".join(map(str, self.kernel[1])))
        for (module, args) in self.modules:
            r += "modulenounzip %s %s\n" % (os.path.join(path, module), " ".join(map(str, args)))
        return r

    # what targets do we need to build/install to run this test?
    def get_build_targets(self):
        def mktarget(modname):
            # workaround beehive's multi-core hack: discard everything after the |
            return modname.split('|',1)[0]

        ret = list(set([self.kernel[0]] + [mktarget(m) for (m, _) in self.modules]))

        if self.hypervisor:
            ret.append(self.hypervisor)
            
        if self.machine.get_bootarch() == "arm_gem5":
        	ret.append('arm_gem5_harness_kernel')
        elif self.machine.get_bootarch() == "armv7_gem5_2":
            ret.append('arm_gem5_image')

        return ret

def default_bootmodules(build, machine):
    """Returns the default boot module configuration for the given machine."""
    # FIXME: clean up / separate platform-specific logic

    a = machine.get_bootarch()
    m = BootModules(machine)

    # set the kernel: elver on x86_64
    if a == "x86_64":
        m.set_kernel("%s/sbin/elver" % a, machine.get_kernel_args())
    elif a == "armv5":
        m.set_kernel("%s/sbin/cpu.bin" % a, machine.get_kernel_args())
    elif a == "armv7":
        m.set_kernel("%s/sbin/cpu_arm_gem5" % a, machine.get_kernel_args())
    else:
        m.set_kernel("%s/sbin/cpu" % a, machine.get_kernel_args())
    # default for all barrelfish archs
    # hack: cpu driver is not called "cpu" for ARMv7 builds
    if a == "armv7":
        m.add_module("%s/sbin/cpu_arm_gem5" % a, machine.get_kernel_args())
        m.add_module("/arm_gem5_image")
    else:
        m.add_module("%s/sbin/cpu" % a, machine.get_kernel_args())
    m.add_module("%s/sbin/init" % a)
    m.add_module("%s/sbin/mem_serv" % a)
    m.add_module("%s/sbin/monitor" % a)
    m.add_module("%s/sbin/ramfsd" % a, ["boot"])
    m.add_module("%s/sbin/skb" % a, ["boot"])
    m.add_module("%s/sbin/spawnd" % a, ["boot"])
    m.add_module("%s/sbin/startd" % a, ["boot"])

    # SKB and PCI are x86-only for the moment
    if a == "x86_64" or a == "x86_32":
        m.add_module("%s/sbin/acpi" % a, ["boot"])
        m.add_module("/skb_ramfs.cpio.gz", ["nospawn"])
        m.add_module("%s/sbin/kaluga" % a, ["boot"])
	m.add_module("%s/sbin/routing_setup" %a, ["boot"])
        m.add_module("%s/sbin/pci" % a, ["auto"])

    # ARM-specific stuff
    elif a == "armv5":
        m.add_module_arg("spawnd", "bootarm")
    elif a == "arm_gem5":
    	if machine.get_ncores() == 1:
    		m.add_module_arg("spawnd", "bootarm=0")
    	elif machine.get_ncores() == 2:
    		m.add_module_arg("spawnd", "bootarm=1")
    	elif machine.get_ncores() == 4:
    		m.add_module_arg("spawnd", "bootarm=1-3")
    return m
