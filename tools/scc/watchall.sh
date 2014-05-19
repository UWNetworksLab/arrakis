#!/bin/bash -i
#
# Shows output of all Barrelfish SCC cores simultaneously, as it arrives.

trap "pkill -P $$" SIGINT

for i in `seq 0 47`; do
# XXX: We use grep here to enforce line-buffered output, so concurrent
# input from UARTs isn't garbled.
    cat /dev/crbif0rb0c${i}ttyS0 | grep ^ -a --line-buffered &
done

wait
