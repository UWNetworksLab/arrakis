#!/bin/sh

gnuplot <<EOF
load "../header.gnuplot"
set xlabel "Cores"
set ylabel "Time (s)"
set xrange [1:]
set yrange [0:]
set xtics (1,4,8,12,16,20,24,28,32,36,40)
set output '$1'
set title '$3'
plot \
 '$2' index 1 using 1:2 title "Barrelfish" with linespoints, \
 '$2' index 0 using 1:2 title "Linux" with linespoints
EOF
