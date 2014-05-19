##########################################################################
# Copyright (c) 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests, barrelfish
from common import TestCommon
from results import PassFailResult

@tests.add_test
class MonitorBootTest(TestCommon):
    '''Very simple test of ability to start the monitor'''
    name = "monitorboottest"

    def get_finish_string(self):
        return "monitor: invoked"

    def process_data(self, testdir, rawiter):
        passed = False
        for line in rawiter:
            if line.startswith(self.get_finish_string()):
               passed = True
               break
        return PassFailResult(passed)
