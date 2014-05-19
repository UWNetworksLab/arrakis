#!/usr/bin/env python

##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# this program munges readings to produce a gnuplot file, data in the form:
#   <index> [<reading> ...]
# is converted to:
#   <index> <n_readings> <median> <mean> <std dev.> <min> <max>
#
# blank lines, and lines starting with a '#', are ignored
#
# -- andrewb@cse.unsw.edu.au, 2003/10/31, updated 2009/09/04

import sys, string, math

class Stats:
    def __init__(self, data):
        self.nvalues = len(data)
        self.minimum = self.maximum = self.median = self.mean = self.stddev = float('nan')
        if self.nvalues == 0:
            return
        data.sort()
        self.minimum = data[0]
        self.maximum = data[-1]
        self.mean = float(sum(data)) / self.nvalues
        if (self.nvalues % 2 == 0):
            self.median = (data[self.nvalues / 2]
                           + data[self.nvalues / 2 - 1]) / 2.0
        else:
            self.median = data[self.nvalues / 2]

        diffs = 0.0
        for datum in data:
            diffs += (datum - self.mean) ** 2
        self.stddev = math.sqrt(diffs / self.nvalues)


if __name__ == "__main__":
    if len(sys.argv) > 1:
        try:
            infile = open(sys.argv[1])
        except:
            sys.stderr.write("Error opening %s\n" % sys.argv[1])
            sys.exit(1)
    else:
        infile = sys.stdin

    for line in infile:
        stripped = string.lstrip(line)
        if (stripped == "" or stripped[0] == '#'):
            sys.stdout.write(line)
            continue

        words = string.split(line)
        if (len(words) == 0):
            sys.stderr.write("Error parsing line: %s" % line)
            sys.exit(1)

        index = words[0]
        data = map(float, words[1:])
        if (len(data) == 0):
            sys.stderr.write("Error parsing line: %s" % line)
            sys.exit(1)

        s = Stats(data)
        sys.stdout.write("%s %s %s %s %s %s %s\n" % (index, s.nvalues,
                         s.median, s.mean, s.stddev, s.minimum, s.maximum))
