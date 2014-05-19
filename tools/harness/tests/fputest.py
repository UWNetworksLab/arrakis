##########################################################################
# Copyright (c) 2011, ETH Zurich.
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
class FpuTest(TestCommon):
    '''FPU context switching'''
    name = "fputest"

    def get_modules(self, build, machine):
        modules = super(FpuTest, self).get_modules(build, machine)
        modules.add_module("fputest", ["test1" , "2"])
        modules.add_module("fputest", ["test2" , "2"])
        return modules

    def get_finish_string(self):
        return "fputest passed successfully!"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)
