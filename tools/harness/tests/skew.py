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
from common import TestCommon, TimeoutError
from results import RowResults

@tests.add_test
class ClockSkewTest(TestCommon):
    ''' Skew characterization test '''
    name = "skew_test"

    def get_modules(self, build, machine):
        modules = super(ClockSkewTest, self).get_modules(build, machine)
        modules.add_kernel_arg("ticks=false")
        maxcore = machine.get_coreids()[-1]
        modules.add_module("bcast_ping_pong", [maxcore])
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        self.set_timeout(None)
        return self.collect_data(machine)

    def process_data(self, testdir, raw_iter):
        results = RowResults(['time0', 'time1', 'time2',
                              'time1 - time0', 'time2 - time0'])
        passed = False

        for line in raw_iter:
            m = re.match("page\s+(\d+)\s+time0\s+(\d+)time1\s+(\d+)time2\s+(\d+)", line)
            if m:
                passed = True
                time0 = int(m.group(2))
                time1 = int(m.group(3))
                time2 = int(m.group(4))
                results.add_row([time0, time1, time2,
                                 time1 - time0, time2 - time0])

        if not passed:
            results.mark_failed()
        return results
