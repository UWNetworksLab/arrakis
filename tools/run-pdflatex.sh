##########################################################################
# Copyright (c) 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################
#
# run-pdflatex.sh: helper script to build Barrelfish technical notes
# 
# This script wraps up pdflatex into something that can be entirely
# controlled by command-line options, making it considerably easier to
# invoke from a Makefile or Hakefile.
# 
# In some ways this is a poor relation to rubber, but it doesn't
# require any support packages (not even Python), and is specifically
# tailored to the kinds of filesystem naming games that Hake likes to
# play.  That said, it's completely independent of Hake, and usable
# standalone.

#
# Usage summary
#
usage () {
    cat >&1 <<EOF
Usage: $0 <options>
	--input-tex <filename>
	--working-dir <dir>
	--output-pdf <filename>
	(--texinput <dir>)*
	(--bininput <dir>)*
	[--has-bib]
EOF
    exit 1
}

# 
# Shorthand for running pdflatex.  This is nicer than using a single
# shell variable as it always gets the quoting right.
#
run_latex () {
#    echo "dirs: $WORKING_DIR, $JOB_NAME, $INPUT_TEX, $TEXINPUTS, $BIBINPUTS" \
    pdflatex \
	-interaction=nonstopmode \
	-file-line-error \
	-output-directory "$WORKING_DIR" \
	-jobname "$JOB_NAME" \
	"$INPUT_TEX" #|| exit
}

#
# Work out how to construct TEXINPUTS and BIBINPUTS paths, according
# to Cygwin or not.
#
case "`which pdflatex`" in
    /cygdrive/*) _ISCYGWIN=Yes ;;
esac

cons_inputs() {
    if [ -z "$_ISCYGWIN" ] ; then
	echo "${1}${2}//:" 
    else
	echo "${1}${2}//\\;"
    fi
}

# 
# Initial values for input arguments
#
if [ -z "$TEXINPUTS" ]; then
    TEXINPUTS=`cons_inputs "" "."`
else
    TEXINPUTS=`cons_inputs "$TEXINPUTS" "."`
fi

# echo "TEXinputs is $TEXINPUTS"
# exit

BIBINPUTS=`cons_inputs "" "."`
INPUT_TEX=
WORKING_DIR=
OUTPUT_PDF=
HAS_BIB=
HAS_GLO=

#
# Argument processing.
#
while [ -n "$*" ]; do 
    case $1 in
	--input-tex)
	    shift; INPUT_TEX=$1 ;;
	--output-pdf)
	    shift; OUTPUT_PDF="$1" ;;
	--working-dir)
	    shift; WORKING_DIR="$1" ;;
	--texinput)
	    shift; TEXINPUTS=`cons_inputs "$TEXINPUTS" "$1"` ;;
	--bibinput)
	    shift; BIBINPUTS=`cons_inputs "$BIBINPUTS" "$1"` ;;
	--has-bib)
	    HAS_BIB=Yes ;;
	--has-glo)
	    HAS_GLO=Yes ;;
	*)
	    usage ;;
    esac
    shift
done
if [ -z "$INPUT_TEX" ] ; then usage ; fi
if [ -z "$WORKING_DIR" ] ; then usage ; fi
if [ -z "$OUTPUT_PDF" ] ; then usage ; fi

#
# Calculate all the other stuff we need.
#
INPUT_DIR=`dirname "$INPUT_TEX"`
INPUT_BASE=`basename "$INPUT_TEX" .tex`
TEXINPUTS=`cons_inputs "$TEXINPUTS" "$INPUT_DIR"`
BIBINPUTS=`cons_inputs "$BIBINPUTS" "$INPUT_DIR"`
export TEXINPUTS
export BIBINPUTS

# 
# We use a different jobname due to parallel Make paranoia: it's a
# good idea if the output file that we really care out only appears as
# the last step of the build.  This also allows us to specify
# different directories for the final PDF file and the intermediate
# working files, something (apparently) not possible within pdflatex.
#
JOB_NAME="${INPUT_BASE}.tmp"
# the man page for bibtex specifically reads
# "the filename on the command line must be given without the .aux extension"
AUX_FILE_WITHOUT_AUX="$WORKING_DIR/${JOB_NAME}"
AUX_FILE="$WORKING_DIR/${JOB_NAME}.aux"
HST_FILE="$WORKING_DIR/${JOB_NAME}.hst"
LOG_FILE="$WORKING_DIR/${JOB_NAME}.log"
TOC_FILE="$WORKING_DIR/${JOB_NAME}.toc"
BBL_FILE="$WORKING_DIR/${JOB_NAME}.bbl"
BLG_FILE="$WORKING_DIR/${JOB_NAME}.blg"
VER_FILE="$WORKING_DIR/${JOB_NAME}.ver"
GLO_FILE="$WORKING_DIR/${JOB_NAME}.glo"
ACN_FILE="$WORKING_DIR/${JOB_NAME}.acn"
IST_FILE="$WORKING_DIR/${JOB_NAME}.ist"
PDF_FILE="$WORKING_DIR/${JOB_NAME}.pdf"

#
# And, finally, do what the old Makefile used to do. 
#
# bibtex on cygwin can be miktex which always 0 exit code
run_latex
if [ -n "$HAS_BIB" ]; then (bibtex $AUX_FILE_WITHOUT_AUX && test -r $BBL_FILE)  || exit; echo run_latex; fi
if [ -n "$HAS_GLO" ]; then (makeglossaries -s $IST_FILE $GLO_FILE && makeglossaries -s $IST_FILE $ACN_FILE && test -r $GLO_FILE)  || exit; echo run_latex; fi
if [ -e "$TOC_FILE" -o -e "$BBL_FILE" -o -e "$VER_FILE" -o -e "$GLO_FILE" -o -e "$ACN_FILE" ]; then run_latex; fi
while egrep -e 'LaTeX Warning.*Rerun' "$LOG_FILE"; do run_latex; done
rm -f "$AUX_FILE" "$HST_FILE" "$LOG_FILE" "$TOC_FILE" "$BBL_FILE" 
rm -f "$BLG_FILE" "$VER_FILE" 
rm -f "$GLO_FILE" "$ACN_FILE" "$IST_FILE"
mv "$PDF_FILE" "$OUTPUT_PDF"
echo "Output file is in $OUTPUT_PDF"
