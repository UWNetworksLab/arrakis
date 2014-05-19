#
# Copyright (c) 2009-2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
#

import os
import types
import string
import datetime
import debug

RAW_FILE_NAME = 'raw.txt'
BOOT_FILE_NAME = 'bootlog.txt'


def run_test(build, machine, test, path):
    # Open files for raw output from the victim and log data from the test
    raw_file_name = os.path.join(path, RAW_FILE_NAME)
    debug.verbose('open %s for raw output' % raw_file_name)
    raw_file = open(raw_file_name, 'w')

    # run the test, dumping the output to the raw file as we go
    try:
        debug.verbose('harness: setup test')
        test.setup(build, machine, path)
        debug.verbose('harness: run test')
        starttime = datetime.datetime.now()
        for out in test.run(build, machine, path):
            # timedelta for the time this line was emitted from the start of the run
            timestamp = datetime.datetime.now() - starttime
            # format as string, discarding sub-second precision
            timestr = str(timestamp).split('.', 1)[0]
            # filter output line of control characters
            filtered_out = filter(lambda c: c in string.printable, out.rstrip())
            # debug filtered output along with timestamp
            debug.debug('[%s] %s' % (timestr, filtered_out))
            # log full raw line (without timestamp) to output file
            raw_file.write(out)
        debug.verbose('harness: output complete')
    except KeyboardInterrupt:
        # let the user know that we are on our way out
        debug.error('Interrupted! Performing cleanup...')
        raise
    finally:
        raw_file.close()
        debug.verbose('harness: cleanup test')
        test.cleanup(machine)


def process_results(test, path):
    # open raw file for input processing
    raw_file_name = os.path.join(path, RAW_FILE_NAME)
    debug.verbose('open %s for raw input' % raw_file_name)
    raw_file = open(raw_file_name, 'r')

    try:
        results = test.process_data(path, raw_file)
    finally:
        raw_file.close()
    if not results:
        debug.verbose('no results')
        return True  # no results, assume success

    retval = True  # everything OK

    # Process raw.txt and make a bootlog.txt that begins with grubs
    # output, avoids having encoding issues when viewing logfiles
    boot_file_name = os.path.join(path, BOOT_FILE_NAME)
    if os.path.exists(raw_file_name):
        idx = 0
        with open(raw_file_name, 'r') as rf:
            lines = rf.readlines()
            for idx, line in enumerate(lines):
                if line.strip() == "root (nd)":
                    break
        if idx > 0:
            with open(boot_file_name, 'w') as wf:
                wf.writelines(lines[idx:])
        else:
            debug.verbose('Magic string root (nd) not found, do not write bootlog.txt')
    else:
        debug.verbose('No file named %s exists. Do not create bootlog.txt.' % raw_file_name)

    # if a single result, turn it into a list
    if not isinstance(results, types.ListType):
        results = [results]
    for result in results:
        # see if it passed
        try:
            passed = result.passed()
        except NotImplementedError:
            passed = None
        if passed is False:
            debug.log('Test %s FAILED' % test.name)
            retval = False
        elif passed:
            debug.verbose('Test %s PASSED' % test.name)

        # write it to a file
        name = result.name if result.name else 'results'
        data_file_name = os.path.join(path, name + '.dat')
        debug.verbose('create %s for processed output' % data_file_name)
        data_file = open(data_file_name, 'w')
        try:
            result.to_file(data_file)
            data_file.close()
        except NotImplementedError:
            debug.verbose('no processed output, remove %s' % data_file_name)
            data_file.close()
            os.remove(data_file_name)

    return retval
