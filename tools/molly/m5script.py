# Simple M5 script for use with Barrelfish.  This creates a very 
# basic machine with a set of CPUs connected directly to the
# simulated memory.  
#
# (See configs/example/* in the M5 distribution for examples of
# fuller scripts -- caches, etc.).

import optparse
import os
import sys
import m5

from m5.defines import buildEnv
from m5.objects import *
from m5.util import fatal

#######################################################################
#
# Check that we are running on a full-system X86 simulator

if not buildEnv['FULL_SYSTEM']:
    fatal("Expected FULL_SYSTEM");

if not buildEnv['TARGET_ISA'] == "x86":
    fatal("Expected TARGET_ISA == x86");

#######################################################################
#
# Set up basic configuration options: kernel location and number of 
# CPUs. 

parser = optparse.OptionParser()
parser.add_option("--kernel", action="store", type="string")
parser.add_option("--num_cpus", action="store", type="int")
(options, args) = parser.parse_args()

#######################################################################
#
# Create simulated machine.  

system = X86System()

# Kernel to boot
system.kernel = options.kernel

# Physical memory
system.membus = Bus()
system.physmem = PhysicalMemory(range = AddrRange('512MB'))
system.physmem.port = system.membus.port

# CPUs
CPUClass = AtomicSimpleCPU
CPUClass.clock = '2GHz'
system.cpu = [CPUClass(cpu_id=i) for i in xrange(options.num_cpus)]
system.mem_mode = 'atomic'
for i in xrange(options.num_cpus):
    system.cpu[i].connectAllPorts(system.membus)
    system.intel_mp_table.add_entry(
         X86IntelMPProcessor(local_apic_id = i,
                             local_apic_version = 0x14,
                             enable = True,
                             bootstrap = (i == 0)))

# North Bridge
system.iobus = Bus(bus_id=0)
system.bridge = Bridge(delay='50ns', nack_delay='4ns')
system.bridge.side_a = system.iobus.port
system.bridge.side_b = system.membus.port

# Platform
system.pc = Pc()
system.pc.attachIO(system.iobus)
system.intrctrl = IntrControl()

# APIC and devices
io_apic = X86IntelMPIOAPIC(id = options.num_cpus,
                           version = 0x11,
                           enable = True,
                           address = 0xfec00000)
system.pc.south_bridge.io_apic.apic_id = io_apic.id
system.intel_mp_table.add_entry(io_apic)

connect_busses = X86IntelMPBusHierarchy(bus_id=0,
                                        subtractive_decode=True, 
                                        parent_bus=1)
system.intel_mp_table.add_entry(connect_busses)

pci_dev4_inta = X86IntelMPIOIntAssignment(
    interrupt_type = 'INT',
    polarity = 'ConformPolarity',
    trigger = 'ConformTrigger',
    source_bus_id = 1,
    source_bus_irq = 0 + (4 << 2),
    dest_io_apic_id = io_apic.id,
    dest_io_apic_intin = 16)

system.intel_mp_table.add_entry(pci_dev4_inta);

#######################################################################
#
# Start simulation

Root(system=system)
m5.instantiate()

print '..... STARTING SIMULATION'

exit_event = m5.simulate()
exit_cause = exit_event.getCause()

print 'Exiting @ tick %i because %s' % (m5.curTick(), exit_cause)





