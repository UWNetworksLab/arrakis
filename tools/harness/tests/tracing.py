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
from results import RawResults

@tests.add_test
class Tracing(TestCommon):
    ''' Tracing. ''' # XXX: What is this for? -AB
    name = "tracing"

    def get_module_name(self):
        return "barrier_bench"

    def get_modules(self, build, machine):
        modules = super(Tracing, self).get_modules(build, machine)
        modules.add_module(self.get_module_name(), ['2'])
        return modules

    def process_data(self, testdir, rawiter):
        results = RawResults('tracing')
        data = []
        for line in rawiter:
            m = re.match("trace: (\w+.*)", line)
            if m:
                data.append(m.group(1))


        if len(data) != 0:
            results.add_group(1, data)
        return results
