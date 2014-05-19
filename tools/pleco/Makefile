##########################################################################
# Copyright (c) 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################


LHS2TEX = lhs2TeX
PDFTEX = pdflatex
BIBTEX = bibtex

FOF_DIR = ../fof
MAIN = Fugu.lhs
SOURCES = $(wildcard *.lhs)
TEXS = $(patsubst %.lhs,%.tex,$(SOURCES))



all: literate



.SUFFIXES: .tex .lhs

.lhs.tex: 
	$(LHS2TEX) -o $*.tex $*.lhs

clean:
	rm -f *.aux *.ptb *.toc *.log *.o *.hi $(patsubst %.lhs,%.tex,$(SOURCES))

cleanall: clean
	rm -f ErrorDefinition.pdf
