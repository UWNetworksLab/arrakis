##########################################################################
# Copyright (c) 2012, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult

@tests.add_test
class PerfMonTest(TestCommon):
    '''Sanity check for performance counter support'''
    name = "perfmontest"

    def get_modules(self, build, machine):
        modules = super(PerfMonTest, self).get_modules(build, machine)
        modules.add_module("perfmontest")
        return modules

    def get_finish_string(self):
        return "Perfmon Overflow data END"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)
