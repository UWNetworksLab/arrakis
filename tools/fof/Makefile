##########################################################################
# Copyright (c) 2007, 2008, 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################


LHS2TEX = lhs2TeX
PDFTEX = pdflatex
BIBTEX = bibtex

INDEX = FiletOFish.tex
MAIN = Main.lhs
SOURCES = $(wildcard *.lhs)\
	  $(wildcard ./Libc/*.lhs)\
	  $(wildcard ./Libbarrelfish/*.lhs)\
          $(wildcard ./IL/FoF/*.lhs)\
          $(wildcard ./IL/Paka/*.lhs)\
          $(wildcard ./Constructs/*.lhs)
TEXS = $(patsubst %.lhs,%.tex,$(SOURCES))



all: literate 



.SUFFIXES: .tex .lhs

.lhs.tex: 
	$(LHS2TEX) -o $*.tex $*.lhs


literate: $(TEXS)
	 $(PDFTEX) $(INDEX)
	 $(BIBTEX) $(basename $(INDEX))
	 $(PDFTEX) $(INDEX)
	 $(PDFTEX) $(INDEX)

clean:
	rm -f *.aux *.ptb *.toc *.log *.o *.hi $(patsubst %.lhs,%.tex,$(SOURCES))

cleanall: clean
	rm -f FiletOFish.pdf
