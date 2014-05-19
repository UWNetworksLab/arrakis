#padding overhead
plot "./space_consumption.dat" index 0 using 14:5 with lines, "./space_consumption.dat" index 1 using 14:5 with lines, "./space_consumption.dat" index 2 using 14:5 with lines , "./space_consumption.dat" index 3 using 14:5 with lines, "./space_consumption.dat" index 4 using 14:5 with lines, "./space_consumption.dat" index 5 using 14:5 with lines


#conumption% - fill%
plot "./space_consumption.dat" index 0 using 14:($15 - $13) with lines, "./space_consumption.dat" index 1 using 14:($15-$13) with lines, "./space_consumption.dat" index 2 using 14:($15-$13) with lines,"./space_consumption.dat" index 3 using 14:($15-$13) with lines, "./space_consumption.dat" index 4 using 14:($15-$13) with lines, "./space_consumption.dat" index 5 using 14:($15-$13) with lines



# baumdarstellung mehrere plots und darum mehrere farben (eine farbe pro device und pro bridge)
plot [0:100] [-1:4] "./space_consumption.dat" index 6 every 1:1:0:0:1:0 using 4:3 with linespoints, "./space_consumption.dat" index 6 every 1:1:0:1:1:1 using 4:3 with linespoints
gnuplot> 


# baumdarstellung ein plot
plot  "./space_consumption.dat" index 7  using 4:($6==1?$3:1/0):yticlabels(7) title "Bridge" with linespoints, "./space_consumption.dat" index 7 using 4:($6==0?$3:1/0):yticlabels(7) title "Device" with linespoints

gnuplot> plot [0:100] [-1:4] "./space_consumption.dat" index 6  using 4:($6==1?$3:1/0) title "Bridges" with linespoints, "./space_consumption.dat" index 6  using 4:($6==0?$3:1/0) title "Devices" with linespoints,"./space_consumption.dat" index 6 every 1:1:0:0:0 using ($4+$5/2):($3+0.2):2 with labels
gnuplot> 

 plot  "./space_consumption.dat" index 12  using 4:($6==1?$3:1/0) with linespoints, "./space_consumption.dat" index 12 using 4:($6==0?$3:1/0) with linespoints



# space_consumption
clear
reset
set terminal postscript eps enhanced dashed color
set output "space_consumption.eps"
set boxwidth 0.1
set key bottom right
set xlabel "Fillrate (device sum / max root size) [%]"
set ylabel "Used bytes"
plot [8:20] "space_consumption.dat" index 0 using 14:8 title "Root size (max)" with lines,   "space_consumption.dat" index 0 using 14:4 title "DeviceSum" with lines, "space_consumption.dat" index 0 using 14:($17==1?$8:0) title "Added Bridge" with boxes, "space_consumption.dat" index 0 using 14:3 title 'CLP' with lines, "space_consumption.dat" index 1 using 14:3 title 'postorder' with lines, "space_consumption.dat" index 2 using 14:3 title 'postorder\_sorted' with lines, "space_consumption.dat" index 3 using 14:3 title 'postorder\_sum' with lines, "space_consumption.dat" index 4 using 14:3 title 'postorder\_asc' with lines, "space_consumption.dat" index 5 using 14:3 title 'postorder\_asc\_sum' with lines

set output "padding_overhead.eps"
set key top left
set xlabel "Fillrate (device sum / max root size) [%]"
set ylabel "Padding overhead"
set arrow from 25,graph 0 to 25,graph 1 nohead
plot [0:100] "./space_consumption.dat" index 0 using 14:5 title "CLP" with lines, "./space_consumption.dat" index 1 using 14:5 title "postorder" with lines, "./space_consumption.dat" index 2 using 14:5 with lines , "./space_consumption.dat" index 3 using 14:5 with lines, "./space_consumption.dat" index 4 using 14:5 with lines, "./space_consumption.dat" index 5 using 14:5 with lines

set output "tree.eps"
set xlabel "Address [byte]"
set ylabel "Bus Nr"
plot "
