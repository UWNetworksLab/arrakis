#!/bin/bash

SINGLE=$(grep '^1\b' $2 | cut -f2)
LINUXSINGLE=$(echo $SINGLE | cut -d' ' -f1)
BFSINGLE=$(echo $SINGLE | cut -d' ' -f2)

gnuplot <<EOF
load "../header.gnuplot"
set xlabel "Cores"
set ylabel "Speedup"
set xrange [1:]
set yrange [1:]
set key top left
set xtics (1,4,8,12,16,20,24,28,32,36,40)
set output '$1'
set title '$3'
plot \
 '$2' index 0 using 1:($LINUXSINGLE/\$2) title "Linux" with linespoints, \
 '$2' index 1 using 1:($BFSINGLE/\$2) title "Barrelfish" with linespoints
EOF
