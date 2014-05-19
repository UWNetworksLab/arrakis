#!/usr/bin/env python

#
# Copyright (c) 2009, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
#

import sys

# check interpreter version to avoid confusion over syntax/module errors
if sys.version_info < (2, 6):
    sys.stderr.write('Error: Python 2.6 or greater is required\n')
    sys.exit(1)

import os
import optparse
import traceback
import datetime
import getpass
import fnmatch
import harness
import debug
import checkout
import builds
import tests
import machines
from tests.common import TimeoutError
from socket import gethostname


def list_all():
    print 'Build types:\t', ', '.join([b.name for b in builds.all_builds])
    print 'Machines:\t', ', '.join([m.name for m in machines.all_machines])
    print 'Tests:'
    for t in tests.all_tests:
        print '  %-20s %s' % (t.name, (t.__doc__ or '').strip())


def parse_args():
    p = optparse.OptionParser(
        usage='Usage: %prog [options] SOURCEDIR RESULTDIR',
        description='Barrelfish regression/benchmark harness')

    g = optparse.OptionGroup(p, 'Basic options')
    g.add_option('-b', '--build', action='append', dest='buildspecs',
                 metavar='BUILD', help='build types to perform [default: release]')
    g.add_option('-B', '--buildbase', dest='buildbase', metavar='DIR',
                 help='places builds under DIR [default: SOURCEDIR/builds]')
    g.add_option('-e', '--existingbuild', dest='existingbuild', metavar='DIR',
                 help='existing build directory (may not be used with -b)')
    g.add_option('-m', '--machine', action='append', dest='machinespecs',
                 metavar='MACHINE', help='victim machines to use')
    g.add_option('-t', '--test', action='append', dest='testspecs',
                 metavar='TEST', help='tests/benchmarks to run')
    g.add_option('-c', '--comment', dest='comment',
                 help='comment to store with all collected data')
    p.add_option_group(g)

    g = optparse.OptionGroup(p, 'Debugging options')
    g.add_option('-L', '--listall', action='store_true', dest='listall',
                 help='list available builds, machines and tests')
    debug.addopts(g, 'debuglevel')
    g.add_option('-k', '--keepgoing', action='store_true', dest='keepgoing',
                 help='attempt to continue on errors')
    p.add_option_group(g)
    p.set_defaults(debuglevel=debug.NORMAL)

    options, args = p.parse_args()

    debug.current_level = options.debuglevel

    if options.listall:
        list_all()
        sys.exit(0)

    if len(args) != 2:
        p.error('source and results directories must be specified')
    options.sourcedir, options.resultsdir = args

    # determine default buildbase if needed
    if options.buildbase is None:
        options.buildbase = os.path.join(options.sourcedir, 'builds')

    # check validity of source and results dirs
    if not os.path.isdir(os.path.join(options.sourcedir, 'hake')):
        p.error('invalid source directory %s' % options.sourcedir)
    if not (os.path.isdir(options.resultsdir)
            and os.access(options.resultsdir, os.W_OK)):
        p.error('invalid results directory %s' % options.resultsdir)

    # resolve and instantiate all builds
    def _lookup(spec, classes):
        spec = spec.lower()
        return [c for c in classes if fnmatch.fnmatch(c.name.lower(), spec)]

    if options.existingbuild:
        if options.buildspecs:
            p.error('existing build directory cannot be used together'
                    ' with build types (-b)')
        options.builds = [builds.existingbuild(options, options.existingbuild)]
    else:
        options.builds = []
        if not options.buildspecs:
            options.buildspecs = ['release']
        for spec in options.buildspecs:
            matches = _lookup(spec, builds.all_builds)
            if matches == []:
                p.error('no builds match "%s" (try -L for a list)' % spec)
            options.builds.extend(
                [b for b in matches if b not in options.builds])
        options.builds = [b(options) for b in options.builds]

    # resolve and instantiate all machines
    if options.machinespecs is None:
        p.error('no machines specified')
    options.machines = []
    for spec in options.machinespecs:
        matches = _lookup(spec, machines.all_machines)
        if matches == []:
            p.error('no machines match "%s" (try -L for a list)' % spec)
        options.machines.extend(
            [m for m in matches if m not in options.machines])
    options.machines = [m(options) for m in options.machines]

    # resolve and instantiate all tests
    if options.testspecs:
        options.tests = []
        for spec in options.testspecs:
            matches = _lookup(spec, tests.all_tests)
            if matches == []:
                p.error('no tests match "%s" (try -L for a list)' % spec)
            options.tests.extend(
                [t for t in matches if t not in options.tests])
    else:
        p.error('no tests specified (try -t memtest if unsure)')
    options.tests = [t(options) for t in options.tests]

    debug.verbose('Host:     ' + gethostname())
    debug.verbose('Builds:   ' + ', '.join([b.name for b in options.builds]))
    debug.verbose('Machines: ' + ', '.join([m.name for m in options.machines]))
    debug.verbose('Tests:    ' + ', '.join([t.name for t in options.tests]))

    return options


def make_results_dir(options, build, machine, test):
    # Create a unique directory for the output from this test
    timestamp = datetime.datetime.now().strftime('%Y%m%d-%H%M%S')
    dirname = '-'.join([test.name, build.name, machine.name, timestamp])
    path = os.path.join(options.resultsdir, str(datetime.datetime.now().year), dirname)
    debug.verbose('create result directory %s' % path)
    os.makedirs(path)
    return path


def write_description(options, checkout, build, machine, test, path):
    debug.verbose('write description file')
    f = open(os.path.join(path, 'description.txt'), 'w')
    f.write('test: %s\n' % test.name)
    f.write('revision: %s\n' % checkout.describe())
    f.write('build: %s\n' % build.name)
    f.write('machine: %s\n' % machine.name)
    f.write('start time: %s\n' % datetime.datetime.now())
    f.write('user: %s\n' % getpass.getuser())
    if options.comment:
        f.write('\n' + options.comment + '\n')
    f.close()

    diff = checkout.changes()
    if diff:
        with open(os.path.join(path, 'changes.patch'), 'w') as f:
            f.write(diff)


def main(options):
    retval = True  # everything was OK
    co = checkout.Checkout(options.sourcedir)

    # determine build architectures
    buildarchs = set()
    for m in options.machines:
        buildarchs |= set(m.get_buildarchs())
    buildarchs = list(buildarchs)

    for build in options.builds:
        debug.log('starting build: %s' % build.name)
        build.configure(co, buildarchs)
        for machine in options.machines:
            for test in options.tests:
                debug.log('running test %s on %s, cwd is %s'
                          % (test.name, machine.name, os.getcwd()))
                path = make_results_dir(options, build, machine, test)
                write_description(options, co, build, machine, test, path)
                try:
                    harness.run_test(build, machine, test, path)
                except TimeoutError:
                    retval = False
                    msg = 'Timeout while running test'
                    if options.keepgoing:
                        msg += ' (attempting to continue)'
                    debug.error(msg)
                    if options.keepgoing:
                        continue
                    else:
                        return retval
                except Exception:
                    retval = False
                    msg = 'Exception while running test'
                    if options.keepgoing:
                        msg += ' (attempting to continue):'
                    debug.error(msg)
                    if options.keepgoing:
                        traceback.print_exc()
                        continue
                    else:
                        raise

                debug.log('test complete, processing results')
                try:
                    passed = harness.process_results(test, path)
                except Exception:
                    retval = False
                    msg = 'Exception while processing results'
                    if options.keepgoing:
                        msg += ' (attempting to continue):'
                    debug.error(msg)
                    if options.keepgoing:
                        traceback.print_exc()
                    else:
                        raise
                if not passed:
                    retval = False

    debug.log('all done!')
    return retval


if __name__ == "__main__":
    if not main(parse_args()):
        sys.exit(1)  # one or more tests failed
