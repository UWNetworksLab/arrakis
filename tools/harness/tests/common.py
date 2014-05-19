##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, shutil, select, datetime
import barrelfish, debug, results
from tests import Test

DEFAULT_TEST_TIMEOUT = datetime.timedelta(seconds=360)
DEFAULT_BOOT_TIMEOUT = datetime.timedelta(seconds=240)
TEST_TIMEOUT_LINE = '[Error: test timed out]\n'
BOOT_TIMEOUT_LINE_RETRY = '[Error: boot timed out, retrying...]\n'
BOOT_TIMEOUT_LINE_FAIL = '[Error: boot timed out, retry limit reached]\n'
MAX_BOOT_ATTEMPTS = 3 # maximum number of times to attempt booting before giving up

class TimeoutError(Exception):
    def __init__(self, when=None):
        self.when = when
    def __str__(self):
        return 'timeout occurred%s' % (': ' + self.when if self.when else '')

class TestCommon(Test):
    name = None # should be overridden

    def __init__(self, options):
        super(TestCommon, self).__init__(options)
        self.timeout = None # current timeout (as absolute datetime)
        self.test_timeout_delta = DEFAULT_TEST_TIMEOUT # timeout delta for next test
        self.boot_phase = True # are we waiting for machine boot or test output?
        self.boot_attempts = 0 # how many times did we try to boot?

    def _setup_harness_dir(self, build, machine):
        dest_dir = machine.get_tftp_dir()
        debug.verbose('installing to %s' % dest_dir)
        if os.access(dest_dir, os.F_OK):
            debug.verbose('clearing out %s' % dest_dir)
            for e in os.listdir(dest_dir):
                p = os.path.join(dest_dir, e)
                if os.path.isdir(p):
                    shutil.rmtree(p, ignore_errors=True)
                elif not e.startswith('.nfs'):
                    os.unlink(p)
        else:
            debug.verbose('creating %s' % dest_dir)
            os.makedirs(dest_dir)
        return dest_dir

    def setup(self, build, machine, testdir):
        # build the default set of targets
        targets = self.get_build_targets(build, machine)
        # HACK: we write menu.lst here for arm_gem5 target, since it must
        # be known at compile time, also set test time out to 20 mins
        # this should really be set per test, since multihop_latency won't complete in 20 mins
        if machine.get_bootarch() == "arm_gem5":
        	path = os.path.join(build.build_dir, 'menu.lst')
        	machine._write_menu_lst(self.get_modules(build,machine).get_menu_data('/'), path)
        	self.test_timeout_delta = datetime.timedelta(seconds=1200)
        build.build(targets)

        # lock the machine
        machine.lock()
        machine.setup()

        # setup the harness dir and install there
        dest_dir = self._setup_harness_dir(build, machine)
        build.install(targets, dest_dir)

    def get_modules(self, build, machine):
        return barrelfish.default_bootmodules(build, machine)

    def get_build_targets(self, build, machine):
        return self.get_modules(build, machine).get_build_targets()

    def set_timeout(self, delta=DEFAULT_TEST_TIMEOUT):
        self.test_timeout_delta = delta
        if not self.boot_phase:
            if delta:
                debug.verbose('setting timeout for %s' % delta)
                self.timeout = datetime.datetime.now() + delta
            else:
                debug.verbose('cancelling timeout')
                self.timeout = None

    def boot(self, machine, modules):
        machine.set_bootmodules(modules)
        self.boot_attempts = 0
        self.reboot(machine)

    def reboot(self, machine):
        # retry a failed boot, without changing the modules
        machine.reboot()
        self.boot_phase = True
        self.boot_attempts += 1
        timeout_secs = machine.get_boot_timeout()
        if timeout_secs:
            self.timeout = (datetime.datetime.now()
                                 + datetime.timedelta(seconds=timeout_secs))
        else:
            self.timeout = datetime.datetime.now() + DEFAULT_BOOT_TIMEOUT

    def get_finish_string(self):
        # default output from a test program when it completes
        # should be overridden by subclasses
        return "client done"

    def is_finished(self, line):
        return line.startswith(self.get_finish_string())

    def is_booted(self, line):
        # early boot output from Barrelfish kernel
        return line.startswith("Barrelfish CPU driver starting")

    def process_line(self, rawline):
        """Can be used by subclasses to hook into the raw output stream."""
        pass

    def _readline(self, fh):
        # standard blocking readline if no timeout is set
        if not self.timeout:
            return fh.readline()

        line = ''
        while not line.endswith('\n'):
            # wait until there is something to read, with a timeout
            (readlist, _, _) = select_timeout(self.timeout, [fh])
            if not readlist:
                # if we have some partial data, return that first!
                # we'll be called again, and the next time can raise the error
                if line:
                    return line
                elif self.boot_phase:
                    raise TimeoutError('waiting for victim to boot')
                else:
                    raise TimeoutError('waiting for test output from victim')
            # read a single character, to avoid blocking
            # FIXME: there must be a better way to do nonblocking IO!
            c = fh.read(1)
            if c == '': # should never see EOF
                raise Exception('read from sub-process returned EOF')
            line += c
        return line

    def collect_data(self, machine):
        fh = machine.get_output()
        while True:
            try:
                line = self._readline(fh)
            except TimeoutError as e:
                if self.boot_phase:
                    if self.boot_attempts < MAX_BOOT_ATTEMPTS:
                        yield BOOT_TIMEOUT_LINE_RETRY
                        self.reboot(machine)
                        continue
                    else:
                        yield BOOT_TIMEOUT_LINE_FAIL
                else:
                    yield TEST_TIMEOUT_LINE
                debug.verbose("timeout encountered in collect_data");
                raise e

            yield line

            if not self.boot_phase:
                self.process_line(line)
                if self.is_finished(line):
                    debug.verbose("is_finished returned true for line %s" % line)
                    break
            elif self.is_booted(line):
                self.boot_phase = False
                self.set_timeout(self.test_timeout_delta)
                self.process_line(line)

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def cleanup(self, machine):
        tftp_dir = machine.get_tftp_dir()
        machine.shutdown()
        machine.unlock()
        debug.verbose('removing %s' % tftp_dir)
        shutil.rmtree(tftp_dir, ignore_errors=True)


# utility function used by other tests
def select_timeout(timeout, rlist=[], wlist=[], xlist=[]):
    """run select.select, with a timeout specified as a datetime object"""
    delta = timeout - datetime.datetime.now()
    if delta.days >= 0:
        assert(delta.days == 0) # unimplemented, and insane!
        secs = delta.seconds + delta.microseconds / 1000000.0
        assert(secs > 0)
        return select.select(rlist, wlist, xlist, secs)
    else: # already timed out
        return ([], [], [])
