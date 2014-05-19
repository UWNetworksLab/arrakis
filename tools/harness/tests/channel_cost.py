##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests
from common import TestCommon, TimeoutError
from results import RawResults, PassFailResult

class RpcTestCommon(TestCommon):

    def get_module_name(self):
        raise NotImplementedError

    def get_modules(self, build, machine):
        modules = super(RpcTestCommon, self).get_modules(build, machine)
        cores = machine.get_ncores()
        modules.add_module(self.get_module_name(), ["core=1", "server"])
        modules.add_module(self.get_module_name(),
                           ["core=%d" % (cores - 1), "client"])
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def process_data(self, testdir, rawiter):
        results = RawResults('connections')
        times = []
        connections = None
        for line in rawiter:
            m = re.match("running with (\d+) connections", line)
            if m:
                if times:
                    results.add_group(connections, times)
                connections = int(m.group(1))
                times = []
                continue

            m = re.match("\d+ (\d+)", line)
            if m:
                assert(connections is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(connections, times)
        return results

@tests.add_test
class ChannelCostTest(RpcTestCommon):
    ''' Cost of incrementally more channels '''
    name = "channel_cost"

    def get_module_name(self):
        return "channel_cost_bench"
