##########################################################################
# Copyright (c) 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################
import datetime
import re
import tests
from common import TestCommon
from results import PassFailResult

TIMERTEST_TIMEOUT = datetime.timedelta(minutes=5)

@tests.add_test
class TimerTest(TestCommon):
    '''Timer test'''
    name = "timer"
    
    def __init__(self, options):
        super(TimerTest, self).__init__(options)
        self.saw_line_A = False
        self.saw_line_B = False

    def get_modules(self, build, machine):
        modules = super(TimerTest, self).get_modules(build, machine)
        modules.add_module("lpc_timer")
        modules.add_module("timer_test",["core=%d" % machine.get_coreids()[1]])
        modules.add_module("timer_test",["core=%d" % machine.get_coreids()[2],
                                         "B"])
        return modules

    def get_test_A_finish_string(self):
        return "Done with test for client_A"

    def get_test_B_finish_string(self):
        return "Done with test for client_B"
    
    def is_finished(self, line):
        if line.startswith(self.get_test_A_finish_string()):
            self.saw_line_A = True
        if line.startswith(self.get_test_B_finish_string()):
            self.saw_line_B = True
        if self.saw_line_A and self.saw_line_B:
            return True
        return False
    

    
    def boot(self, *args):
        super(TimerTest, self).boot(*args)
        self.set_timeout(TIMERTEST_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff it has lines from both test_A and test_B
        test_A_passed = False
        test_B_passed = False

        for line in rawiter:
            if line.startswith(self.get_test_A_finish_string()):
                test_A_passed = True
            if line.startswith(self.get_test_B_finish_string()):
                test_B_passed = True
            if test_A_passed and test_B_passed:
                return PassFailResult(True)

        if test_A_passed and test_B_passed:
            return PassFailResult(True)
        else:
            return PassFailResult(False)
