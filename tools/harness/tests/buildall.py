##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from tests import Test
from results import PassFailResult

@tests.add_test
class BuildAllTest(Test):
    '''Simple test that everything in the default targets builds cleanly'''
    name = "buildall"

    def setup(self, build, machine, testdir):
        build.build(["all"])

    def run(self, build, machine, testdir):
        return []

    def cleanup(self, machine):
        pass

    def process_data(self, testdir, raw_iter):
        return PassFailResult(True)

@tests.add_test
class BuildPandaboardImage(Test):
    '''Simple test to ensure that the default pandaboard image builds cleanly'''
    name = "buildall_pandaboard"

    def setup(self, build, machine, testdir):
        build.build(['pandaboard_image'])

    def run(self, build, machine, testdir):
        return []

    def cleanup(self, machine):
        pass

    def process_data(self, testdir, raw_iter):
        return PassFailResult(True)
