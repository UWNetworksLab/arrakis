#!/bin/sh

##########################################################################
# Copyright (c) 2007, 2008, 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# Generates a debug.gdb file that can be used to debug the Barrelfish kernel.

KERNELPATH=$2

OBJDUMP=${OBJDUMP:-no_objdump}

get_section_start ()
{
	${OBJDUMP} -h $KERNELPATH | awk "\$2 == \"$1\"{ print \$4 }"
}

TEXT_ADDR=`get_section_start .text`
RODATA_ADDR=`get_section_start .rodata`
DATA_ADDR=`get_section_start .data`
DATA_REL_ADDR=`get_section_start .data.rel`
DATA_REL_LOCAL_ADDR=`get_section_start .data.rel.local`
BSS_ADDR=`get_section_start .bss`

MULTIPLIER=0x400000

sed -e "s,%KERNEL,$KERNELPATH,
s/%text/0x$TEXT_ADDR/
s/%rodata/0x$RODATA_ADDR/
s/%data_rel_local/0x$DATA_REL_LOCAL_ADDR/
s/%data_rel/0x$DATA_REL_ADDR/
s/%data/0x$DATA_ADDR/
s/%bss/0x$BSS_ADDR/
s/%multiplier/$MULTIPLIER/" $1
