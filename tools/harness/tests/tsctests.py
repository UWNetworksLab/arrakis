##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests, barrelfish
from common import TestCommon
from results import RawResults

class TscTestCommon(TestCommon):

    def get_module_name(self):
        raise NotImplementedError

    def get_modules(self, build, machine):
        modules = super(TscTestCommon, self).get_modules(build, machine)
        modules.add_kernel_arg("ticks=false")
        modules.add_module(self.get_module_name())
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def process_data(self, testdir, raw_iter):
        results = RawResults('Index')

        diff = []
        for line in raw_iter:
            m = re.match("Iteration\s+(\d+): time0 (\d+) time1 (\d+) difference (\d+)",
                         line)
            if m:
                diff.append(int(m.group(3)) - int(m.group(2)))

        results.add_group('Index', diff)
        return results

@tests.add_test
class TscTest(TscTestCommon):
    ''' Test to measure the cost of reading from the timestamp counter '''
    name = "tsc_test"

    def get_module_name(self):
        return "tsc_bench"

@tests.add_test
class ShmcTest(TscTestCommon):
    ''' Measure cost of shared memory clock '''
    name = "shmc_test"

    def get_module_name(self):
        return "shared_mem_clock_bench"

    def process_data(self, testdir, rawiter):
        results = RawResults('core')
        times = []
        core = None
        for line in rawiter:
            m = re.match("Running on (\d+) cores", line)
            if m:
                if times:
                    results.add_group(core, times)
                core = int(m.group(1))
                times = []
                continue

            m = re.match("page \d+ took (\d+)", line)
            if m:
                assert(core is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(core, times)
        return results
