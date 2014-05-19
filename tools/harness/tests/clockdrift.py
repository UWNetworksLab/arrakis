##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests, datetime
from common import TestCommon, TimeoutError
from results import RowResults

# timeout for a complete run, including setup etc.
CLOCKDRIFT_TIMEOUT = datetime.timedelta(hours=13)

@tests.add_test
class ClockDriftTest(TestCommon):
    ''' APIC clock drift test '''
    name = "clockdrift_apic"

    def get_modules(self, build, machine):
        modules = super(ClockDriftTest, self).get_modules(build, machine)
        modules.add_module("apicdrift_bench", [machine.get_ncores()])
        return modules

    def get_finish_string(self):
        return "client done."

    def boot(self, *args):
        super(ClockDriftTest, self).boot(*args)
        self.set_timeout(CLOCKDRIFT_TIMEOUT)

    def process_data(self, testdir, rawiter):
        corestr = None
        lastdata = None

        for line in rawiter:
            m = re.match("Running on (\d+) cores.", line)
            if m:
                ncores = int(m.group(1))
                results = RowResults(["core %d to %d" % (n, (n + 1) % ncores) for n in range(ncores)])
                corestr = "\d+: "
                for n in range(ncores):
                    corestr += "(\d+) "
                continue

            if corestr != None:
                m = re.match(corestr, line)
                if m:
                    data = [int(m.group(n)) for n in range(1, ncores + 1)]
                    if lastdata != None:
                        diffs = [data[0] - lastdata]
                    else:
                        diffs = [0]
                    diffs += [(data[n] - data[n - 1]) for n in range(1, ncores)]
                    results.add_row(diffs)
                    lastdata = data[ncores - 1]

        return results
