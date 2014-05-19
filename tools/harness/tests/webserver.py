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
from common import TestCommon, TimeoutError, select_timeout
from results import ResultsBase, PassFailResult, RowResults


WEBSERVER_TEST_FILES=['index.html', 'barrelfish.gif', 'barrelfish_sosp09.pdf']
WEBSERVER_TEST_FILES=['index.html', 'bigfile.bz2', 'bigfile.2.bz2', 'nevill-master-capabilities.pdf']
WEBSERVER_TEST_FILES=['nevill-master-capabilities.pdf']
WEBSERVER_TEST_FILES=['index.html']

#'razavi-master-performanceisolation.pdf']

WEBSERVER_TIMEOUT=5 # seconds
TEST_LOG_NAME = 'testlog.txt'

HTTPERF_BASE_ARGS='--hog --close-with-reset --timeout 2 '
HTTPERF_URI = '/index.html'

# Webserver stress test, It will download index page repeatedly for following
#       number of times
WEBSERVER_STRESS_COUNTER = 3000

# desired duration of an httperf test run (seconds)
HTTPERF_DURATION = 20

# sleep time between runs (seconds)
HTTPERF_SLEEPTIME = 20

# timeout for a complete run, including setup etc.
HTTPERF_TIMEOUT = datetime.timedelta(seconds=(HTTPERF_DURATION + 30))

# connection rates across all client machines
HTTPERF_STARTRATE = 1000  # initial rate
HTTPERF_RATEINCREMENT = 1000  # amount to increment by for each new run


class WebCommon(TestCommon):

    def setup(self, build, machine, testdir):
        super(WebCommon, self).setup(build, machine, testdir)
        self.testdir = testdir
        self.finished = False
        self.ip = None

    def get_modules(self, build, machine):
        cardName = "e1000"
        modules = super(WebCommon, self).get_modules(build, machine)
        modules.add_module("e1000n", ["core=%d" % machine.get_coreids()[3]]) # 1
        modules.add_module("NGD_mng", ["core=%d" % machine.get_coreids()[1], #2
                                    "cardname=%s"%cardName])
        modules.add_module("netd", ["core=%d" % machine.get_coreids()[1], #2
                                   "cardname=%s"%cardName])
        nfsip = socket.gethostbyname(siteconfig.get('WEBSERVER_NFS_HOST'))
        modules.add_module("webserver", ["core=%d" % machine.get_coreids()[2], #2
				cardName, nfsip,
                                         siteconfig.get('WEBSERVER_NFS_PATH')])
#                                         siteconfig.get('WEBSERVER_NFS_TEST_PATH')])
        return modules

    def process_line(self, line):
        m = re.match(r'Interface up! IP address (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.ip = m.group(1)
        elif self.ip and line.startswith('Starting webserver'):
            self.runtests(self.ip)
            self.finished = True

    def is_finished(self, line):
        return self.finished


@tests.add_test
class WebserverTest(WebCommon):
    '''tests webserver functionality'''
    name = "webserver"

    def setup(self, *args):
        super(WebserverTest, self).setup(*args)
        self.testlog = None

    def getpage_stress(self, server, page, count):
        debug.verbose('requesting http://%s/%s' % (server, page))
        failure_count = 0;
        #c = httplib.HTTPConnection(server, timeout=WEBSERVER_TIMEOUT)
        for i in range(count):
            try:
                c = httplib.HTTPConnection(server, timeout=WEBSERVER_TIMEOUT)
                c.request('GET', '/' + page)
                r = c.getresponse()
                if (r.status / 100) != 2 :
                    print "HTTP request failed for %d" % (i)
                assert((r.status / 100) == 2) # check for success response

                # Reset failure count after sucessful retrival
                failure_count = 0
                c.close()
            except:
                print "HTTP request failed for %d, (failure count %d)" % (i,
                        failure_count)
                failure_count = failure_count + 1
                if failure_count >= 3:
                    print "HTTP request failed for 3 successive times."
                    print "Giving up for %d, (failure count %d)" % (i,
                        failure_count)
                    raise

            #c.close()
        debug.verbose('server replied %s %s for %d times' % (r.status, r.reason, count))


    def getpage(self, server, page):
        debug.verbose('requesting http://%s/%s' % (server, page))
        c = httplib.HTTPConnection(server, timeout=WEBSERVER_TIMEOUT)
        c.request('GET', '/' + page)
        r = c.getresponse()

        debug.verbose('server replied %s %s' % (r.status, r.reason))
        assert((r.status / 100) == 2) # check for success response

        try:
            local_path = siteconfig.get('WEBSERVER_LOCAL_PATH')
        except AttributeError:
            local_path = None
        local = os.path.join(local_path, page) if local_path else None
        if local and os.path.isfile(local) and os.access(local, os.R_OK):
            debug.verbose('comparing content to %s' % local)
            l = open(local, 'r')
            # read from both files and compare
            CHUNKSIZE = 4096
            while True:
                remote_data = r.read(CHUNKSIZE)
                local_data = l.read(CHUNKSIZE)
                if remote_data != local_data:
                    print "Remote and local data did not match:"
                    print "Remote data\n"
                    print remote_data
                    print "Local data\n"
                    print local_data
                assert(remote_data == local_data)
                if len(local_data) < CHUNKSIZE:
                    break

            debug.verbose('contents matched for %s' % local)
        c.close()

    def dotest(self, func, args):
        exception = None
        r = None
        try:
            r = func(*args)
        except Exception as e:
            exception = e

        s = 'Test: %s%s\t%s\n' % (func.__name__, str(args),
                                 'FAIL' if exception else 'PASS')
        if exception:
            debug.verbose('Exception while running test: %s\n'
                          % traceback.format_exc())
            s += 'Error was: %s\n' % traceback.format_exc()
        self.testlog.write(s)

        return r

    def runtests(self, server):
        stress_counter = WEBSERVER_STRESS_COUNTER
        self.testlog = open(os.path.join(self.testdir, TEST_LOG_NAME), 'w')
        for f in WEBSERVER_TEST_FILES:
            self.dotest(self.getpage, (server, f))
            debug.verbose("Running stresstest: (%d GET %s)" %
                    (stress_counter, str(f)))
            self.dotest(self.getpage_stress, (server, f, stress_counter))
        self.testlog.close()

    def process_data(self, testdir, rawiter):
        # the test passed iff we see at least one PASS and no FAILs in the log
        passed = None
        testlog = open(os.path.join(testdir, TEST_LOG_NAME), 'r')
        for line in testlog:
            if re.match('Test:.*FAIL$', line):
                passed = False
            elif passed != False and re.match('Test:.*PASS$', line):
                passed = True
        testlog.close()
        return PassFailResult(passed == True)


@tests.add_test
class HTTPerfTest(WebCommon):
    '''httperf webserver performance benchmark'''
    name = "httperf"

    def setup(self, *args):
        super(HTTPerfTest, self).setup(*args)
        self.nruns = 0

    def _runtest(self, target, nclients, nconns, rate):
        self.nruns += 1
        nrun = self.nruns
        httperfs = []
        try:
            for nclient in range(nclients):
                user, host = siteconfig.site.get_load_generator()
                assert(nrun < 100 and nclient < 100)
                filename = 'httperf_run%02d_%02d.txt' % (nrun, nclient)
                logfile = open(os.path.join(self.testdir, filename), 'w')
                debug.verbose('spawning httperf on %s' % host)
                hp = HTTPerfClient(logfile, user, host, target, nconns, rate)
                httperfs.append(hp)

            # loop collecting output from all of them
            busy_httperfs = list(httperfs) # copy list
            timeout = datetime.datetime.now() + HTTPERF_TIMEOUT
            while busy_httperfs:
                (ready, _, _) = select_timeout(timeout, busy_httperfs)
                if not ready:
                    raise TimeoutError('waiting for httperfs')
                for hp in ready:
                    try:
                        hp.read()
                    except EOFError:
                        busy_httperfs.remove(hp)
        finally:
            debug.log('cleaning up httperf test...')
            for hp in httperfs:
                hp.cleanup()

    def runtests(self, target):
        nclients = siteconfig.get('HTTPERF_MAXCLIENTS')
        firstrun = True
        totalrate = HTTPERF_STARTRATE
        while True:
            if firstrun:
                firstrun = False
            else:
                # sleep a moment to let things settle down between runs
                debug.verbose('sleeping between httperf runs')
                time.sleep(HTTPERF_SLEEPTIME)

            # compute rate and total number of connections for each client
            rate = totalrate / nclients
            nconns = HTTPERF_DURATION * rate

            debug.log('starting httperf: %d clients, %d conns, rate %d (%d per client)' %
                      (nclients, nconns, totalrate, rate))
            self._runtest(target, nclients, nconns, rate)

            # decide whether to keep going...
            results = self._process_run(self.nruns)
            if not results.passed():
                debug.log('previous test failed, stopping')
                break
            elif results.request_rate < (0.9 * results.connect_rate):
                debug.log('request rate below 90% of connect rate, stopping')
                break
            elif results.reply_rate < (0.9 * results.request_rate):
                debug.log('reply rate below 90% of request rate, stopping')
                break
            else:
                totalrate += HTTPERF_RATEINCREMENT
                continue

    def _process_one(self, logfile):
        ret = HTTPerfResults()
        matches = 0

        for line in logfile:
            # Connection rate
            m = re.match('Connection rate: (\d+\.\d+) conn/s', line)
            if m:
                matches += 1
                ret.connect_rate = float(m.group(1))

            # Request rate
            m = re.match('Request rate: (\d+\.\d+) req/s', line)
            if m:
                matches += 1
                ret.request_rate = float(m.group(1))

            # Reply rate
            m = re.search('Reply rate \[replies/s\]: min .* avg (\d+\.\d+)'
                          ' max .* stddev .*', line)
            if m:
                matches += 1
                ret.reply_rate = float(m.group(1))

            # Bandwidth
            m = re.match('Net I/O: .* KB/s \((\d+\.\d+)\*10\^6 bps\)', line)
            if m:
                matches += 1
                ret.bandwidth = float(m.group(1))

            # client-side errors
            m = re.match('Errors: fd-unavail (\d+) addrunavail (\d+)'
                         ' ftab-full (\d+) other (\d+)', line)
            if m:
                matches += 1
                ret.fd_unavail = int(m.group(1))
                ret.addrunavail = int(m.group(2))
                ret.ftab_full = int(m.group(3))
                ret.other_err = int(m.group(4))

            # server-side errors
            m = re.match('Errors: total \d+ client-timo (\d+) socket-timo (\d+)'
                         ' connrefused (\d+) connreset (\d+)', line)
            if m:
                matches += 1
                ret.client_timo = int(m.group(1))
                ret.socket_timo = int(m.group(2))
                ret.connrefused = int(m.group(3))
                ret.connreset = int(m.group(4))

        if matches != 6 : # otherwise we have an invalid log
            print "Instead of 6, only %d matches found\n" % (matches)

        return ret


    def _process_run(self, nrun):
        nameglob = 'httperf_run%02d_*.txt' % nrun
        results = []
        for filename in glob.iglob(os.path.join(self.testdir, nameglob)):
            with open(filename, 'r') as logfile:
                results.append(self._process_one(logfile))
        return sum(results, HTTPerfResults())

    def process_data(self, testdir, raw_iter):
        self.testdir = testdir
        totals = {}
        for filename in glob.iglob(os.path.join(testdir, 'httperf_run*.txt')):
            nrun = int(re.match('.*/httperf_run(\d+)_', filename).group(1))
            result = self._process_run(nrun)
            totals[nrun] = result

        fields = 'run connect_rate request_rate reply_rate bandwidth errors'.split()
        final = RowResults(fields)
        for run in sorted(totals.keys()):
            total = totals[run]
            errsum = sum([getattr(total, f) for f in total._err_fields])
            final.add_row([run, total.connect_rate, total.request_rate,
                           total.reply_rate, total.bandwidth, errsum])
            # XXX: often the last run will have errors in it, due to the control algorithm
            #if errsum:
            #    final.mark_failed()
        return final


class HTTPerfResults(ResultsBase):
    _err_fields = 'fd_unavail addrunavail ftab_full other_err'.split()
    _result_fields = ('client_timo socket_timo connrefused connreset'
                      ' connect_rate request_rate bandwidth reply_rate').split()
    _fields = _err_fields + _result_fields

    def __init__(self):
        super(HTTPerfResults, self).__init__()
        for f in self._fields:
            setattr(self, f, 0)

    def __add__(self, other):
        ret = HTTPerfResults()
        for f in self._fields:
            setattr(ret, f, getattr(self, f) + getattr(other, f))
        return ret

    def passed(self):
        return all([getattr(self, field) == 0 for field in self._err_fields])

    def to_file(self, fh):
        errs = [(f, getattr(self,f)) for f in self._err_fields if getattr(self,f)]
        if errs:
            fh.write('Failed run: ' + ' '.join(['%s %d' % e for e in errs]))

        fh.write('Request rate:\t%f\n' % self.request_rate)
        fh.write('Bandwidth:\t%f\n' % self.bandwidth)
        fh.write('Reply rate:\t%f\n' % self.reply_rate)


class HTTPerfClient(object):
    def __init__(self, logfile, user, host, target, nconns, rate):
        self.user = user
        self.host = host
        self.httperf_path = siteconfig.get('HTTPERF_PATH')
        cmd = '%s %s' % (self.httperf_path, HTTPERF_BASE_ARGS)
        cmd += ' --num-conns %d --rate %d --server %s --uri %s' % (
                        nconns, rate, target, HTTPERF_URI)
        self.proc = self._launchssh(cmd, stdout=subprocess.PIPE, bufsize=0)
        self.logfile = logfile

    def _launchssh(self, remotecmd, **kwargs):
        ssh_dest = '%s@%s' % (self.user, self.host)
        cmd = ['ssh'] + siteconfig.get('SSH_ARGS').split() + [ssh_dest, remotecmd]
        return subprocess.Popen(cmd, **kwargs)

    # mirror builtin file method so that we can pass this to select()
    def fileno(self):
        return self.proc.stdout.fileno()

    def read(self):
        # read only a single character to avoid blocking!
        s = self.proc.stdout.read(1)
        if s == '':
            raise EOFError
        self.logfile.write(s)

    def cleanup(self):
        """perform cleanup if necessary"""
        self.logfile.close()
        if self.proc is None or self.proc.poll() == 0:
            return # clean exit

        if self.proc.returncode:
            debug.warning('httperf: SSH to %s exited with error %d'
                          % (self.host, self.proc.returncode))
        else: # kill SSH if still up
            debug.warning('httperf: killing SSH child for %s' % self.host)
            self.proc.terminate()
            self.proc.wait()

        # run a remote killall to get rid of any errant httperfs
        debug.verbose('killing any errant httperfs on %s' % self.host)
        p = self._launchssh('killall -q %s' % self.httperf_path)
        retcode = p.wait()
        if retcode != 0:
            debug.warning('failed to killall httperf on %s!' % self.host)
