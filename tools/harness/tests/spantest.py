##########################################################################
# Copyright (c) 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult

MATCH = 'spantest.*Done.*cycles'
NUM_THREADS = 4

@tests.add_test
class SpanTest(TestCommon):
    '''Span a program on the twice each on the first and second core'''
    name = "spantest"

    def setup(self, build, machine, testdir):
        super(SpanTest, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(SpanTest, self).get_modules(build, machine)
        modules.add_module("spantest", [ NUM_THREADS ])
        return modules

    def is_finished(self, line):
        return re.match(MATCH, line)

    def process_data(self, testdir, rawiter):
        result = False
        for line in rawiter:
            if re.match(MATCH, line):
                result = True
        return PassFailResult(result)
