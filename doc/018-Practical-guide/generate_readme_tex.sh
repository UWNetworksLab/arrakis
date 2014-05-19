#!/bin/bash
set -e
set -x
README="../../README"
CONVERTER="rst2latex"

# Create couple of temparaty files to store intermediate files
CLEANEDREADME=`mktemp --tmpdir="./" tmp.XXXXXX`
CONVERTEDTEX=`mktemp --tmpdir="./" tmp.XXXXXX`

# cleanup unwanted text from the readme file
./cleanREADME.awk ${README} > ${CLEANEDREADME}

# Generate html from restructured text (assuming docutils are installed)
${CONVERTER} ${CLEANEDREADME} ${CONVERTEDTEX}

# Remove all the unwanted sections still remaining in readme.tex file.
./cleanTex.awk ${CONVERTEDTEX} > readme.tex

# remove the temp files created to store intermediate contents
rm -f ${CONVERTEDTEX} ${CLEANEDREADME}

