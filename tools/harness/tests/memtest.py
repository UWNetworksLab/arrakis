##########################################################################
# Copyright (c) 2009, ETH Zurich.
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
class MemTest(TestCommon):
    '''basic memory allocation functionality on a single core'''
    name = "memtest"

    def get_modules(self, build, machine):
        modules = super(MemTest, self).get_modules(build, machine)
        modules.add_module("memtest")
        return modules

    def get_finish_string(self):
        return "memtest passed successfully!"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)

@tests.add_test
class MemTestMulti(TestCommon):
    '''memory allocation functionality on all cores'''
    name = "memtest_multicore"

    def setup(self, build, machine, testdir):
        super(MemTestMulti, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(MemTestMulti, self).get_modules(build, machine)
        for core in machine.get_coreids():
            modules.add_module("memtest", ["core=%d" % core])
        return modules

    def is_finished(self, line):
        # XXX: count number of times we have seen the finish string
        if line.startswith("memtest passed successfully!"):
            self._nseen += 1
        return self._nseen == self._ncores

    def process_data(self, testdir, rawiter):
        nspawned = nseen = 0
        for line in rawiter:
            if re.match(r'.*pawning .*memtest on core', line):
                nspawned += 1
            if line.startswith("memtest passed successfully!"):
                nseen += 1
        return PassFailResult(nspawned == nseen)
