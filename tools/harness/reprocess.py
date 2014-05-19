#!/usr/bin/env python

##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, optparse, re
import harness, debug, tests


def parse_args():
    p = optparse.OptionParser(usage='Usage: %prog [options] RESULTDIR...',
            description='Reprocess raw results from scalebench/harness runs')
    debug.addopts(p, 'debuglevel')
    options, dirs = p.parse_args()

    if len(dirs) == 0:
        p.error('no result directories specified')

    # check validity of result dirs
    for d in dirs:
        if not (os.path.isdir(d) and os.access(d, os.W_OK)
                and os.access(os.path.join(d, 'description.txt'), os.R_OK)):
            p.error('invalid results directory %s' % d)

    debug.current_level = options.debuglevel
    return dirs
            

def main(dirs):
    for dirname in dirs:
        debug.log('reprocessing %s' % dirname)
        debug.verbose('parse %s/description.txt for test' % dirname)
        testname = test = None
        f = open(os.path.join(dirname, 'description.txt'), 'r')
        for line in f:
            m = re.match(r'test:\s+(.*)', line)
            if m:
                testname = m.group(1)
                break
        f.close()

        if not testname:
            debug.error('unable to parse description for %s, skipped' % dirname)
            continue

        debug.verbose('locate test "%s"' % testname)
        for t in tests.all_tests:
            if t.name.lower() == testname.lower():
                test = t(None) # XXX: dummy options
        if not test:
            debug.error('unknown test "%s" in %s, skipped' % (testname, dirname))
            continue

        debug.verbose('reprocess results')
        harness.process_results(test, dirname)


if __name__ == "__main__":
    main(parse_args())
