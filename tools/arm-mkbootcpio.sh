#!/bin/sh

##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

#
# Create a list of modules and their arguments. We use this list
# as the bootscript. It's a cleaned up version of menu.lst. The
# files named here are used to create a cpio archive that are in
# the same order as the script. This allows:
#
#   1) Writing script lines with 1:1 correspondance with files
#   in archive.
#
#   2) Checking all the files specified actually exist rather than
#   discovering this at boot.
#
# At some point the kernel should probably go into the generated
# archive and the bootloader do just enough to open the archive
# and setup the kernel.
#

if [ $# -ne 2 ] ; then
   echo "Usage: $0 arm/menu.lst <cpio_file>"
   exit 1
fi

BOOTSCRIPT=./$1.modules

echo ${BOOTSCRIPT} > ${BOOTSCRIPT}

cat $1 | \
sed -n -e 's/#.*//' \
 -e 's/[ \t]\+/ /g' \
 -e 's/ $//;s/^ //' \
 -e '/^\(module\|modulenounzip\)/ { s@ *\(module\|modulenounzip\) /?*\(.*\)@./\2@ ; p }' >>${BOOTSCRIPT}

sed -e 's/ .*//' ${BOOTSCRIPT} | cpio -o -H crc > $2
if [ $? -ne 0 ] ; then
    cat <<EOF

Failed to create $2.

Check files specified in $1 are being built in symbolic_target.mk.

EOF

    exit 1
fi
