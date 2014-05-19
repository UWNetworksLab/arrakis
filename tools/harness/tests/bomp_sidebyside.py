##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests, barrelfish
from common import TestCommon, TimeoutError
from results import RowResults

@tests.add_test
class BOMPSideBySideTest(TestCommon):
    ''' BOMP side-by-side app execution benchmark '''
    name = "bomp_sidebyside"

    def get_modules(self, build, machine, cores=0):
        modules = super(BOMPSideBySideTest, self).get_modules(build, machine)
        modules.add_module("bomp_sync_progress", ["core=0", str(cores)])
        modules.add_module("bomp_cpu_bound_progress", ["core=8", "8"])
        return modules

    def run(self, build, machine, testdir):
        for cores in range(2, machine.get_ncores()):
            modules = self.get_modules(build, machine, cores)
            self.boot(machine, modules)
            for line in self.collect_data(machine):
                yield line

    def process_data(self, testdir, raw_iter):
        return PassFailResult(True)
        # results = RowResults(['threads', 'sync', 'cpu_bound'])
        # havesync = False
        # havecpu = False

        # for line in raw_iter:
        #     m = re.match("bomp_sync: threads (\d+), compute time (\d+) ticks", line)
        #     if m:
        #         threads = int(m.group(1))
        #         sync_ticks = int(m.group(2))
        #         havesync = True

        #     m = re.match("bomp_cpu_bound: threads (\d+), compute time (\d+) ticks", line)
        #     if m:
        #         cpu_bound_ticks = int(m.group(2))
        #         havecpu = True

        #     if havesync == True and havecpu == True:
        #         results.add_row([threads, sync_ticks, cpu_bound_ticks])
        #         havesync = False
        #         havecpu = False

        # return results
