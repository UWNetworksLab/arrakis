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

NUM_SPAWNS = 4
NUM_CORES = 2
MATCH = '.*xmpl-spawn.*Finished'

@tests.add_test
class SpawnTest(TestCommon):
    '''Spawn a program on the twice each on the first and second core'''
    name = "spawntest"

    def setup(self, build, machine, testdir):
        super(SpawnTest, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(SpawnTest, self).get_modules(build, machine)
        modules.add_module("$BUILD/examples/xmpl-spawn", [ NUM_SPAWNS, NUM_CORES ])
        return modules

    def is_finished(self, line):
        if re.match(MATCH, line):
            self._nseen += 1
        return self._nseen == (NUM_SPAWNS + 1)

    def process_data(self, testdir, rawiter):
        nspawned = 0
        for line in rawiter:
            if re.match(MATCH, line):
                nspawned += 1
        return PassFailResult(nspawned == (NUM_SPAWNS + 1))
