##########################################################################
# Copyright (c) 2009, ETH Zurich.
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

class SplashMPCommon(TestCommon):
    def get_module_name(self):
        raise NotImplementedError

    def get_modules(self, build, machine):
        modules = super(SplashMPCommon, self).get_modules(build, machine)
        modules.add_module(self.get_module_name())
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        ncores = machine.get_ncores()
        timeouts = 0
        for i in range(1, ncores + 1):
            debug.log('running %s iteration %d/%d' % (self.name, i, ncores))
            yield '[harness: run on %d/%d cores]\n' % (i, ncores)
            modules.reset_module(self.get_module_name(), [str(i)])
            self.boot(machine, modules)
            try:
                for line in self.collect_data(machine):
                    yield line
            except TimeoutError:
                timeouts += 1
                if timeouts > 1:
                    # ignore first timeout, will be marked as failed later
                    continue
                else:
                    # something is really wrong
                    raise

    def process_data(self, testdir, raw_iter):
        res = RowResults(["cores", "compute_time", "create_time"])
        createtime = {}
        computetime = {}
        seencores = set()
        maxcores = None

        for line in raw_iter:
            m = re.match(r"\[harness: run on (\d+)/(\d+) cores\]", line)
            if m:
                runcores = int(m.group(1))
                thismaxcores = int(m.group(2))

                if maxcores is None:
                    maxcores = thismaxcores
                elif maxcores != thismaxcores:
                    res.mark_failed() # inconsistent max #cores in output

                if runcores in seencores or runcores > maxcores:
                    res.mark_failed() # inconsistent #cores for this run
                seencores.add(runcores)

                continue

            m = re.match(r"Createtime\s+(\d+)\s+(\d+)", line)
            if m:
                createtime[int(m.group(1))] = int(m.group(2))
                continue

            m = re.match(r"Computetime\s+(\d+)\s+(\d+)", line)
            if m:
                computetime[int(m.group(1))] = int(m.group(2))

        allcores = set(createtime.keys()).union(computetime.keys())
        if allcores == set() or allcores != seencores:
            res.mark_failed()

        nan = float('nan')
        allcores = list(allcores)
        allcores.sort()
        for c in allcores:
            a = computetime.get(c, nan)
            b = createtime.get(c, nan)
            res.add_row([c, a, b])
            if a == nan or b == nan:
                res.mark_failed()

        return res

@tests.add_test
class BarnesHutTest(SplashMPCommon):
    '''SPLASH2 Barnes Hut'''
    name = "BarnesHut"
    def get_module_name(self):
        return "splash_barnes"

@tests.add_test
class RadiosityTest(SplashMPCommon):
    '''SPLASH2 Radiosity'''
    name = "Radiosity"
    def get_module_name(self):
        return "splash_radiosity"

@tests.add_test
class BompCgTest(SplashMPCommon):
    '''NAS OpenMP Compute gradient'''
    name = "BompCG"
    def get_module_name(self):
        return "bomp_benchmark_cg"

@tests.add_test
class BompFtTest(SplashMPCommon):
    '''NAS OpenMP FFT'''
    name = "BompFT"
    def get_module_name(self):
        return "bomp_benchmark_ft"

@tests.add_test
class BompIsTest(SplashMPCommon):
    '''NAS OpenMP Integer sort'''
    name = "BompIS"
    def get_module_name(self):
        return "bomp_benchmark_is"
