##########################################################################
# Copyright (c) 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

all: rcce_lu.pdf rcce_bt.pdf rcce_lu_speedup.pdf rcce_bt_speedup.pdf

.INTERMEDIATE: rcce_lu.eps rcce_bt.eps rcce_lu_speedup.eps

rcce_lu.pdf: lu.dat ./plot-rcce.sh
	./plot-rcce.sh $@ $< "LU benchmark"

rcce_bt.pdf: bt.dat ./plot-rcce.sh
	./plot-rcce.sh $@ $< "BT benchmark"

rcce_lu_speedup.pdf: lu.dat ./plot-rcce-speedup.sh
	./plot-rcce-speedup.sh $@ $< "LU benchmark"

rcce_bt_speedup.pdf: bt.dat ./plot-rcce-speedup.sh
	./plot-rcce-speedup.sh $@ $< "BT benchmark"

%.pdf: %.eps
	GS_OPTIONS=-dPDFSETTINGS=/prepress epstopdf $<

clean::
	$(RM) *.pdf *.eps
