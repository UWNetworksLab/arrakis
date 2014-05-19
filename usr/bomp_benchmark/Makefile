##########################################################################
# Copyright (c) 2007, 2008, 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

all: cg-gomp ft-gomp is-gomp


clean:
	rm -f cg-gomp cg-bomp ft-gomp ft-bomp is-gomp is-bomp


cg-gomp:
	gcc -o cg-gomp cg.c c_print_results.c c_randdp.c c_timers.c wtime.c -DPOSIX -lm -fopenmp -O2

ep-gomp:
	gcc -o ep-gomp ep.c c_print_results.c c_randdp.c c_timers.c wtime.c -DPOSIX -lm -fopenmp -O2

ft-gomp:
	gcc -o ft-gomp ft.c c_print_results.c c_randdp.c c_timers.c wtime.c -DPOSIX -lm -fopenmp -O2

is-gomp:
	gcc -o is-gomp is.c c_print_results.c c_timers.c wtime.c -DPOSIX -lm -fopenmp -O2

cg-bomp:
	gcc -o wtime.o -c wtime.c -DPOSIX -g -O2
	gcc -o cg-bomp cg.c c_print_results.c c_randdp.c c_timers.c wtime.o -DBOMP -lm -fopenmp libbomp.a -lpthread -lnuma -g -O2

ep-bomp:
	gcc -o wtime.o -c wtime.c -DPOSIX -g -O2
	gcc -o ep-bomp ep.c c_print_results.c c_randdp.c c_timers.c wtime.o -DBOMP -lm -fopenmp libbomp.a -lpthread -lnuma -g -O2

ft-bomp:
	gcc -o wtime.o -c wtime.c -DPOSIX -g -O2
	gcc -o ft-bomp ft.c c_print_results.c c_randdp.c c_timers.c wtime.o -DBOMP -lm -fopenmp libbomp.a -lpthread -lnuma -g -O2

is-bomp:
	gcc -o wtime.o -c wtime.c -DPOSIX -g -O2
	gcc -o is-bomp is.c c_print_results.c c_timers.c wtime.o -DBOMP -lm -fopenmp libbomp.a -lpthread -lnuma -g -O2

scalability-gomp: clean
	gcc -o scalability-gomp scalability.c -DPOSIX -lm -fopenmp -O2

scalability-bomp: clean
	gcc -o scalability-bomp scalability.c -DPOSIX -fopenmp libbomp.a -lpthread -lnuma -g -O2
