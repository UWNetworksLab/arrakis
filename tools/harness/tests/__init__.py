##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

class Test(object):
    name = None # should be overridden

    def __init__(self, options):
        pass

    def setup(self, build, machine, testdir):
        """Prepare the machine to run the test."""
        raise NotImplementedError

    def run(self, build, machine, testdir):
        """(Start to) run the test, raise StopIteration when it finishes.
        Returns an iterator that will produce raw output lines from the machine.
        """
        raise NotImplementedError

    def cleanup(self, machine):
        """Cleanup after running a (possibly failed) test."""
        raise NotImplementedError

    def process_data(self, testdir, raw_iter):
        """Given test directory and iterator for the raw output from a run,
           return the results.
           MAY RUN ON A DIFFERENT INSTANCE FROM THAT USED TO RUN THE TEST!
           (therefore, this should be a pure function)
        """
        raise NotImplementedError


all_tests = []

def add_test(t):
    all_tests.append(t)
    return t

import memtest, webserver, basicNetwork, rpctests, splash_bomp, echoserver, \
    skew, tsctests, vmkit, nfscat, mdbbench, \
    rcce, bulktests, tracing, buildall, bomp_sidebyside, \
    monitortest, phases, clockdrift, channel_cost, fputest, TimerTest, \
    multihoptests, perfmontest, freemem, spawntest, spantest
