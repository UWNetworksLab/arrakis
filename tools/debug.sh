#!/bin/bash

##########################################################################
# Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# Sanely starts the debugger with the simulator.

QEMU_CMD=$1
GDB=$2
GDB_ARGS=$3
SERIAL_OUTPUT=$4
PORT=$((10000 + UID))

if [ "${SERIAL_OUTPUT}" = "" ] ; then
    # Assuming session is interactive. Use terminal for serial output because
    # stdout does not work for daemonized qemu and output is lost. This likely
    # only matters on ARM where there is no video driver at time of writing.
    SERIAL_OUTPUT=`tty`
fi

PIDFILE=/tmp/qemu_debugsim_${USER}_${PORT}.pid
if test -f $PIDFILE; then
    if ps `cat $PIDFILE` >/dev/null; then
	echo "Another QEMU already running (PID: `cat $PIDFILE` PIDFILE: $PIDFILE)"
	exit 1
    else
	echo "Deleting stale lockfile $PIDFILE"
	rm -f $PIDFILE
    fi
fi

echo args = $GDB_ARGS

cat > barrelfish_debug.gdb <<EOF
# Connect to QEMU instance
target remote localhost:$PORT
EOF

QEMU_INVOCATION="${QEMU_CMD} -serial $SERIAL_OUTPUT -gdb tcp::$PORT -S -daemonize -pidfile $PIDFILE"
eval ${QEMU_INVOCATION}
if [ $? -eq 0 ] ; then 
    stty sane
    trap '' SIGINT
    ${GDB} -x barrelfish_debug.gdb ${GDB_ARGS}
    PID=`cat ${PIDFILE}`
    kill ${PID} > /dev/null || true
    rm -f $PIDFILE
else
    echo Failed to launch qemu with:
    echo "   ${QEMU_INVOCATION}"
fi
