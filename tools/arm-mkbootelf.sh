#!/bin/bash

##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

#
# This script inserts a branch-link instruction as the first word in the
# kernel ELF file so it can self-load itself. This trickery allows the
# same kernel to load from ROM or RAM, and makes the RAW blob executable.
#
# This script should make you whince. Ultimately it should be replaced by
# a small program in the blessed scripting language.
#
if [ $# -ne 3 ] ; then
   echo "Usage: <objdump_path> <kernel> <output_file>"
   exit 1
fi

OBJDUMP=$1
KERNEL=$2
OUTPUT=$3

if [ ! -f ${KERNEL} ]; then
   echo Failed to find kernel :- \"${KERNEL}\"
   exit 1
fi

# check for required tools
if ! which tr sed dc dd > /dev/null; then
    echo $0: missing one or more required tools > /dev/stderr
    exit 1
fi

#
# Extract virtual start address, .text virtual address, .text file offset
# Set as shell variables ENTRY, SIZE, VTEXT, LTEXT, OFFSET
#
# The sed expression maps the address info from objdump into shelll variables.
#
# The use of tr is primarily to uppercase the hex digits to be presented to dc.
#
eval `"${OBJDUMP}" -x "${KERNEL}" | tr "[a-z]" "[A-Z]" | sed -n -e "/^START ADDRESS/ { s@.*0X\([0-9A-F]*\).*@ENTRY=\1;@; p }" -e "/TEXT/ { s@.*TEXT[ \t]*\([0-9A-F]*\)[ \t]*\([0-9A-F]*\)[ \t]*\([0-9A-F]*\)[ \t]*\([0-9A-F]*\).*@SIZE=\1 ; VTEXT=\2; LTEXT=\3; OFFSET=\4@; p; q }"`

if [ "${OFFSET}" = "" ] ; then
    echo "Failed to extract addresses."
    exit 1
fi

if [ "${ENTRY}" != "${VTEXT}" ] ; then
    # Strictly we should just check entry is in text section.
    echo "Entry address is not start of text section."
    exit 1
fi

#
# Generate BL instruction (EB + pc_relative_offset / 4).
#
BL=`dc -e "16 o 16 i ${ENTRY} ${VTEXT} - ${OFFSET} + 8 - 4 / EB000000 + p"`


#
# 1. In case we compile or a big endian environment:
# Reverse order of string to get a big endian representation instead of a little endian one. This corresponds to inverting the stack for dc
#
# 2. Convert BL instruction from 8 digit hex representation into
# 32-bit binary word and append rest of kernel.

${OBJDUMP} -f ${KERNEL} | grep -q bigarm # Using return value for if statement

if [ $? -eq 0 ]; then
    # We compile for big endian - reverse byte order
    BL=${BL:6:2}${BL:4:2}${BL:2:2}${BL:0:2}
fi


dc -e "16o 16i ${BL} d 100 % P 100 / d 100 % P 100 / d 100 % P 100 / d 100 % P" > ${OUTPUT}

dd if=${KERNEL} of=${OUTPUT} skip=1 seek=1 bs=4 >/dev/null 2>&1 
exit $?
