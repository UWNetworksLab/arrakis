#!/bin/sh

gnuplot <<EOF
set terminal pdf
set output '`basename $1 .dat`.pdf'
#set title "One-way MPB messaging latencies from core 0"
set xlabel "Message to core"
set ylabel "Latency (cycles)"
set yrange [:6500]
plot '$1' with linespoints title "Overall", '$2' with linespoints title "Send", '$3' with linespoints title "Receive"
EOF
