#!/usr/bin/env python

##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import sys,string,socket

if len(sys.argv) != 2:
    print "usage: bfscope.py <host>"
    sys.exit(1)

#create an INET, STREAMing socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.connect((sys.argv[1], 666))

s.send("trace\n")

header = s.recv(6)
print header
tracelen = int(header)

trace = ""
while len(trace) < tracelen:
    trace += s.recv(1000000)
    #print len(trace)

print "Done"
s.close()

of=open("TRACE", "w")
of.write(trace)

