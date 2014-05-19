#!/bin/sh

##########################################################################
# Copyright (c) 2007, 2008, 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

rm -f results*.txt

for i in `seq 1 $MAXCORES`; do
    ./scalability-gomp $i >> results_scalability_gomp.txt
    ./scalability-bomp $i >> results_scalability_bomp.txt
    ./cg-gomp $i >> results_cg_gomp.txt
    ./cg-bomp $i >> results_cg_bomp.txt
    ./ft-gomp $i >> results_ft_gomp.txt
    ./ft-bomp $i >> results_ft_bomp.txt
    ./is-gomp $i >> results_is_gomp.txt
    ./is-bomp $i >> results_is_bomp.txt
done
