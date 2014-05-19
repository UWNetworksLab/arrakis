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

class MultihopTestCommon(TestCommon):

    def get_module_name(self):
        raise NotImplementedError

    def get_modules(self, build, machine):
        modules = super(MultihopTestCommon, self).get_modules(build, machine)
        modules.add_module(self.get_module_name())
        return modules

@tests.add_test
class MultihopTest(MultihopTestCommon):
    ''' Test whether multi-hop messaging is working '''
    name = "multihop_test"

    def get_module_name(self):
        return "multihoptest"

    def get_finish_string(self):
        return "server all done"
 
    def get_modules(self, build, machine):
        modules = super(MultihopTestCommon, self).get_modules(build, machine)
        modules.add_module(self.get_module_name(),["core=0", "server"])
        modules.add_module(self.get_module_name(),["core=1", "client"])
        return modules

    def process_data(self, testdir, rawiter):
         # the test passed iff we see the finish string
	passed = False
        for line in rawiter:
            if line.startswith(self.get_finish_string()):
		passed = True
        return PassFailResult(passed)


@tests.add_test
class MultihopLatencyTest(MultihopTestCommon):
    ''' Multihop Transport Throughput microbenchmark '''
    name = "multihop_throughput_latency"

    def get_module_name(self):
        return "multihop_latency_bench"

    def process_data(self, testdir, rawiter):
        results = RawResults('message type')
        times = []
        iteration = None
        for line in rawiter:
            m = re.match("Running latency test for message (.*)....", line)
            if m:
                if times:
                    results.add_group(iteration, times)
                iteration = m.group(1)
                times = []
                continue

            m = re.match("page \d+ took (\d+)", line)
            if m:
                assert(iteration is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(iteration, times)
        return results

