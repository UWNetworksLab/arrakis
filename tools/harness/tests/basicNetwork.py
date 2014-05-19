##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, socket, httplib, traceback, os, subprocess, select, datetime, glob, time
import tests, debug, siteconfig
from shutil import copyfile
from common import TestCommon, TimeoutError, select_timeout
from results import ResultsBase, PassFailResult, RowResults


WEBSERVER_TEST_FILES=['index.html', 'barrelfish.gif', 'barrelfish_sosp09.pdf']
WEBSERVER_TIMEOUT=5 # seconds
TEST_LOG_NAME = 'testlog.txt'
TRACE_LOG_NAME = 'tracelog.txt'

HTTPERF_BASE_ARGS='--hog --close-with-reset --timeout 2 '
HTTPERF_URI = '/index.html'

# desired duration of an httperf test run (seconds)
HTTPERF_DURATION = 20

# sleep time between runs (seconds)
HTTPERF_SLEEPTIME = 20

# timeout for a complete run, including setup etc.
HTTPERF_TIMEOUT = datetime.timedelta(seconds=(HTTPERF_DURATION + 30))

# connection rates across all client machines
HTTPERF_STARTRATE = 1000 # initial rate
HTTPERF_RATEINCREMENT = 1000 # amount to increment by for each new run


class NetCommon(TestCommon):

    def setup(self, build, machine, testdir):
        super(NetCommon, self).setup(build, machine, testdir)
        self.testdir = testdir
        self.finished = False
        self.ip = None
        self.traceLogsON = False
        self.traceLogs = []
        self.build_path = build.build_dir

    def get_modules(self, build, machine):
        cardName = "e1000"
        modules = super(NetCommon, self).get_modules(build, machine)
        modules.add_module("e1000n", ["core=%d" % machine.get_coreids()[1]])
        modules.add_module("NGD_mng", ["core=%d" % machine.get_coreids()[1],
                                    "cardname=%s"%cardName])
        modules.add_module("netd", ["core=%d" % machine.get_coreids()[1],
                                   "cardname=%s"%cardName])
        nfsip = socket.gethostbyname(siteconfig.get('WEBSERVER_NFS_HOST'))
        modules.add_module("webserver", ["core=%d" % machine.get_coreids()[1], #2
				cardName, nfsip,
                                siteconfig.get('WEBSERVER_NFS_TEST_PATH')])

        return modules

    def process_line(self, line):
        m = re.match(r'Interface up! IP address (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.ip = m.group(1)

        if line.startswith('dump trac buffers: Start') :
            self.traceLogsON = True
        elif line.startswith('dump trac buffers: Stop') :
            self.finished = True
            self.traceLogsON = False
        elif  self.traceLogsON :
            self.traceLogs.append(line);


    def is_finished(self, line):
        return self.finished


@tests.add_test
class NetdTraceTest(NetCommon):
    '''tests netd and generates tracing output'''
    name = "netdTrace"

    def setup(self, *args):
        super(NetdTraceTest, self).setup(*args)
        self.testlog = None


    def process_data(self, testdir, rawiter):

        tracelogFilePath = os.path.join(self.testdir, TRACE_LOG_NAME)
        tracelogFile = open(tracelogFilePath, 'w')

        passed = None
        traceLogsStarted = False
        traceLogsFinished = False
        #testlog = open(os.path.join(testdir, TEST_LOG_NAME), 'r')
        for line in rawiter:
            if line.startswith('dump trac buffers: Start') :
                traceLogsStarted = True
            elif line.startswith('dump trac buffers: Stop') :
                traceLogsStarted = False
            elif traceLogsStarted :
                tracelogFile.write(line);
                passed = True
        #testlog.close()
        tracelogFile.close()
        print "Tracefile: " +  tracelogFilePath
        JASONFILENAME = 'trace_defs.json'
        # copy trace_defs.json files
        jsonFilePath = os.path.join(self.build_path,
                "x86_64", "trace_definitions", JASONFILENAME)
        print "json file is at " + jsonFilePath
        destFile =  os.path.join(self.testdir, JASONFILENAME)
        copyfile(jsonFilePath, destFile)

        return PassFailResult(passed == True)



