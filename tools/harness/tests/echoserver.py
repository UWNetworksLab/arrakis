##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, os, subprocess, select, time, datetime
import tests, debug, siteconfig
from common import TestCommon, TimeoutError, select_timeout
from results import RowResults

# TODO: this test needs a control loop like the httperf test to
# ramp up the load until it saturates. currently it just runs N
# iterations of a pre-configured set of parameters.

IPBENCH_TEST = 'latency'
IPBENCH_TEST_ARGS = ['Mbps=400', 'size=1300'] # per daemon
IPBENCH_NDAEMONS = 3
IPBENCH_ITERATIONS = 4
IPBENCH_TIMEOUT = datetime.timedelta(seconds=60) # for a single ipbench run
IPBENCH_SLEEPTIME = 10 # seconds between iterations

LOGFILENAME = 'testlog.txt'

class EchoTestCommon(TestCommon):
    def setup(self, build, machine, testdir):
        super(EchoTestCommon, self).setup(build, machine, testdir)
        self.testdir = testdir
        self.finished = False

    def get_modules(self, build, machine):
        cardName = "e1000"
        modules = super(WebCommon, self).get_modules(build, machine)
        modules.add_module("e1000n", ["core=%d" % machine.get_coreids()[1]])
        modules.add_module("NGD_mng", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])
        modules.add_module("netd", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])
        modules.add_module("echoserver",["core=%d"%machine.get_coreids()[3],
                                         "cardname=%s"%cardName])
        return modules

    def process_line(self, line):
        m = re.match(r'Interface up! IP address (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.run_test(m.group(1))
            self.finished = True

    def is_finished(self, line):
        return self.finished

    def get_ipbench_test(self):
        return (IPBENCH_TEST, IPBENCH_TEST_ARGS)

    def _run_ipbenchd(self, user, host):
        ssh_dest = '%s@%s' % (user, host)
        remotecmd = siteconfig.get('IPBENCHD_PATH')
        cmd = ['ssh'] + siteconfig.get('SSH_ARGS').split() + [ssh_dest, remotecmd]
        debug.verbose('spawning ipbenchd on %s' % host)
        return subprocess.Popen(cmd)

    def _cleanup_ipbenchd(self, user, host):
        # run a remote killall to get rid of ipbenchd
        ssh_dest = '%s@%s' % (user, host)
        remotecmd = 'killall -q python'
        cmd = ['ssh'] + siteconfig.get('SSH_ARGS').split() + [ssh_dest, remotecmd]
        debug.verbose('killing ipbenchd on %s' % host)
        retcode = subprocess.call(cmd)
        if retcode != 0:
            debug.warning('failed to killall python on %s!' % host)

    def _run_ipbench(self, args, logfile):
        cmd = [siteconfig.get('IPBENCH_PATH')] + args
        firstrun = True
        for _ in range(IPBENCH_ITERATIONS):
            if firstrun:
                firstrun = False
            else:
                # sleep a moment to let things settle down between runs
                debug.verbose('sleeping between ipbench runs')
                time.sleep(IPBENCH_SLEEPTIME)

            debug.verbose('running ipbench: %s' % ' '.join(cmd))
            child = subprocess.Popen(cmd, stdout=subprocess.PIPE)
            timeout = datetime.datetime.now() + IPBENCH_TIMEOUT
            while True:
                # wait for some output
                (rlist, _, _) = select_timeout(timeout, [child.stdout])
                if not rlist:
                    debug.warning('ipbench run timed out')
                    child.terminate()
                    child.wait()
                    raise TimeoutError('waiting for ipbench')
                # read one char at a time to avoid blocking
                c = child.stdout.read(1)
                if c == '':
                    break # EOF
                logfile.write(c)
            child.wait()
            assert(child.returncode == 0) # check for successful exit

    def run_test(self, targetip):
        ipbenchds = []
        ipbenchd_hosts = []
        logfile = open(os.path.join(self.testdir, LOGFILENAME), 'w')
        try:
            # spawn ipbenchds
            for _ in range(IPBENCH_NDAEMONS):
                user, host = siteconfig.site.get_load_generator()
                # can't run multiple ipbenchds on the same host
                assert(host not in [h for (_,h) in ipbenchd_hosts])
                ipbenchd_hosts.append((user, host))
                ipbenchds.append(self._run_ipbenchd(user, host))

            # give them a moment to start
            time.sleep(1)

            # construct command-line args to ipbench, and run it
            test, testargs = self.get_ipbench_test()
            args = (['--test=%s' % test, '--test-args=%s' % ','.join(testargs),
                     '--test-target=%s' % targetip]
                    + ['--client=%s' % h for (_, h) in ipbenchd_hosts])
            self._run_ipbench(args, logfile)

        finally:
            logfile.close()

            # terminate ipbenchds
            for child in ipbenchds:
                if child.poll() is None:
                    child.terminate()
                    child.wait()
            for (user, host) in ipbenchd_hosts:
                self._cleanup_ipbenchd(user, host)

    def process_data(self, testdir, raw_iter):
        cols = ('Requested Throughput,Achieved Throughput,Sent Throughput,'
                'Packet Size,Min,Avg,Max,Standard Deviation,Median')
        results = RowResults(cols.split(','))
        with open(os.path.join(testdir, LOGFILENAME), 'r') as logfile:
            for line in logfile:
                m = re.match('(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),'
                             '(\d+\.\d+),(\d+)', line)
                assert(m) # line must match, otherwise we have junk output
                vals = [float(s) if '.' in s else int(s) for s in m.groups()]
                results.add_row(vals)
        return results

@tests.add_test
class UDPEchoTest(EchoTestCommon):
    '''UDP echo throughput'''
    name = "udp_echo"

    def get_ipbench_test(self):
        (test, args) = super(UDPEchoTest, self).get_ipbench_test()
        args.append('socktype=udp')
        return (test, args)

@tests.add_test
class TCPEchoTest(EchoTestCommon):
    '''TCP echo throughput'''
    name = "tcp_echo"

    def get_ipbench_test(self):
        (test, args) = super(TCPEchoTest, self).get_ipbench_test()
        args.append('socktype=tcp')
        return (test, args)
