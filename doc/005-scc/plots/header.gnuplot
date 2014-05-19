# ensure an embedded Helvetica font that matches the one Latex uses
# NB: if gnuplot complains about no pfbtops program, you probably need to install the groff package (!)
# set terminal postscript eps enhanced dashed \
#   fontfile "/usr/share/fonts/type1/gsfonts/n019003l.pfb" \
#   font "NimbusSanL-Regu" 20
set terminal pdf

#set xtics nomirror
#set ytics nomirror

# common line styles
#set style line 1 lc 3 lw 2
#set style line 2 lc 1 lw 2
