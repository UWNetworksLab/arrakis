##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests
from common import TestCommon, TimeoutError
from results import RowResults
from results import RawResults

@tests.add_test
class PhaseChangeTest(TestCommon):
    ''' Scheduler phase change latency benchmark '''
    name = "phasechange"

    def get_build_targets(self, build, machine):
        targets = super(PhaseChangeTest, self).get_build_targets(build, machine)
        targets.append("%s/sbin/phases_bench" % (machine.get_bootarch()))
        return targets

    def get_finish_string(self):
        return "client done."

    def run(self, build, machine, testdir):
        ncores = machine.get_ncores()
        for i in [2**x for x in range(0,ncores + 1) if 2 ** x <= ncores]:
            debug.log('running %s on %d/%d cores' % (self.name, i, ncores))
            modules = self.get_modules(build, machine)
            modules.add_module("phases_bench", [i])
            self.boot(machine, modules)
            for line in self.collect_data(machine):
                yield line

    def process_data(self, testdir, rawiter):
        results = RawResults('threads')
        data = []
        for line in rawiter:
            m = re.match("duration \d+: (\d+)", line)
            if m:
                data.append(int(m.group(1)))
                continue

            m = re.match("number of threads: (\d+)", line)
            if m:
                results.add_group(m.group(1), data)
                data = []

        return results

# Run every benchmark for 10 seconds (specified in ms)
PHASESCALE_TIMEOUT = 10000

@tests.add_test
class PhaseScaleTest(TestCommon):
    ''' Scheduler phase change scalability benchmark '''
    name = "phasechange_scale"

    def get_build_targets(self, build, machine):
        targets = super(PhaseScaleTest, self).get_build_targets(build, machine)
        targets.append("%s/sbin/phases_scale_bench" % (machine.get_bootarch()))
        return targets

    def get_finish_string(self):
        return "client done."

    def run(self, build, machine, testdir):
        ncores = machine.get_ncores()
        for delay in [0, 1, 5, 10, 25, 50, 100, 250, 500]:
            for i in range(2, ncores + 1):
                debug.log('running %s on %d/%d cores, delay %d' % (self.name, i, ncores, delay))
                modules = self.get_modules(build, machine)
                modules.add_module("phases_scale_bench", [i, delay, PHASESCALE_TIMEOUT])
                self.boot(machine, modules)
                for line in self.collect_data(machine):
                    yield line

    def process_data(self, testdir, rawiter):
        results = RowResults(['threads', 'delay', 'slowdown'])
        process_sum = 0
        sums = []
        baseline = []

        for line in rawiter:
            m = re.match("workcnt (\d+): (\d+)", line)
            if m:
                if int(m.group(1)) != 0:
                    process_sum += int(m.group(2))
                continue

            m = re.match("number of threads: (\d+), delay: (\d+)", line)
            if m:
                sums.append([m.group(1), m.group(2), process_sum])
                if int(m.group(2)) == 0:
                    baseline.append([m.group(1), process_sum])
                process_sum = 0

        for sum in sums:
            for [t,p] in baseline:
                if t == sum[0]:
                    basesum = p
                    break

            procsum = float(sum[2]) / float(basesum)
            results.add_row([sum[0], sum[1], procsum])

        return results
