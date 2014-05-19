##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
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
RCCE_TIMEOUT = datetime.timedelta(hours=1)

@tests.add_test
class RCCELUTest(TestCommon):
    '''RCCE LU Benchmark'''
    name = "rcce_lu"

    def get_finish_string(self):
        return " Please send the results of this run to:"

    def get_build_targets(self, build, machine):
        targets = super(RCCELUTest, self).get_build_targets(build, machine)
        for i in self.mkrange(machine):
            targets.append("%s/sbin/rcce_lu_A%d" % (machine.get_bootarch(), i))
        return targets

    def mkrange(self, machine):
        # XXX: Don't run on more than 16 cores -- it's awfully slow
        ncores = min(machine.get_ncores(), 16)
        return [2**x for x in range(0,ncores + 1) if 2 ** x <= ncores]

    def run(self, build, machine, testdir):
        ncores = machine.get_ncores()
        for i in self.mkrange(machine):
            debug.log('running %s power %d/%d' % (self.name, i, ncores))
            modules = self.get_modules(build, machine)
            modules.add_module("rcce_lu_A%d" % i, [i, 1] + range(0, i))
            self.boot(machine, modules)
            self.set_timeout(RCCE_TIMEOUT)
            for line in self.collect_data(machine):
                yield line

    def process_data(self, testdir, raw_iter):
        res = RowResults(["cores", "compute_time"])
        computetime = {}
        ct = 0

        for line in raw_iter:
            m = re.match(r" Time in seconds =\s+(\d+.\d+)", line)
            if m:
                ct = float(m.group(1));
                continue
            m = re.match(r" Total processes =\s+(\d+)", line)
            if m:
                computetime[int(m.group(1))] = ct

        allcores = computetime.keys()
        allcores.sort()
        nan = float('nan')
        for c in allcores:
            res.add_row([c, computetime.get(c, nan)])

        return res
