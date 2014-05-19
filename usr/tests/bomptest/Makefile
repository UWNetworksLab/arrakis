##########################################################################
# Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

#CFLAGS = -fopenmp -Wall -O3 -std=c99
#LDFLAGS = -fopenmp

CFLAGS = -openmp -Wall -O3 -std=c99
LDFLAGS = -openmp -static

all: compute cpu_bound sync

compute: compute.o
	$(CC) $(LDFLAGS) -o $@ $<
compute.o: compute.c

cpu_bound: cpu_bound.o
	$(CC) $(LDFLAGS) -o $@ $<
cpu_bound.o: cpu_bound.c

sync: sync.o
	$(CC) $(LDFLAGS) -o $@ $<
sync.o: sync.c
