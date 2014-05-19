##########################################################################
# Copyright (c) 2009-2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import subprocess, os, sys

QUIET   = 0
NORMAL  = 1
VERBOSE = 2
DEBUG   = 3

current_level = NORMAL

def message(level, message):
    if level <= current_level:
        sys.stdout.write(message + '\n')
        sys.stdout.flush()

def warning(s):
    message(QUIET, 'Warning: ' + s)

def error(s):
    message(QUIET, 'Error: ' + s)

def log(s):
    message(NORMAL, s)

def verbose(s):
    message(VERBOSE, s)

def debug(s):
    message(DEBUG, s)

def checkcmd(*args, **kwargs):
    """Run a command as with subprocess.check_call, but either discard or
    display the output, depending on the current debug level."""

    # display verbose message saying what we do
    verbose('executing ' + ' '.join(args[0]))

    # if non-debug mode, discard stdout
    # if non-verbose mode, discard stderr as well (hides noise from cmake etc.)
    devnull = None
    if current_level < VERBOSE:
        devnull = open(os.devnull, 'w')
        kwargs['stderr'] = devnull
        if current_level < DEBUG:
            kwargs['stdout'] = devnull

    subprocess.check_call(*args, **kwargs)

    if devnull:
        devnull.close()

def addopts(p, dest):
    p.add_option('-q', '--quiet', action='store_const', dest=dest, const=QUIET,
                 help='quiet output, only error messages and warnings')
    p.add_option('-v', '--verbose', action='store_const', dest=dest,
                 const=VERBOSE, help='more verbose output')
    p.add_option('--debug', action='store_const', dest=dest, const=DEBUG,
                 help='debug output, results of all commands')
