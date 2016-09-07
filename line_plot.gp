#!/usr/bin/gnuplot
set terminal png size 1200,742 enhanced font 'Verdana,8'
#set output 'sections.svg'
set output outfile
datafile="/home/mpah/lisp/site/ut/figures/group_ts_data.txt"
set key top rmargin

## Time series
set timefmt '%Y-%m'
set xdata time
set format x '%Y-%b'

## axis
set xtic nomirror font ",8" rotate by -45
set style line 50 lc rgb '#808080' lt 1
set border 3 front ls 50
set tics nomirror out scale 0.75


set linetype 1 lc rgb '#D53E4F' lw 1
set linetype 2 lc rgb '#F46D43' lw 1
set linetype 3 lc rgb '#FDAE61' lw 1
set linetype 4 lc rgb '#FEE08B' lw 1
set linetype 5 lc rgb '#E6F598' lw 1
set linetype 6 lc rgb '#ABDDA4' lw 1
set linetype 7 lc rgb '#66C2A5' lw 1
set linetype 8 lc rgb '#3288BD' lw 1
set linetype 9 lc rgb '#8F5E99' lw 1
set linetype 10 lc rgb '#aaaaaa' lw 1
set linetype 11 lc rgb '#D53E4F' lw 1
set linetype 12 lc rgb '#F46D43' lw 1
set linetype 13 lc rgb '#FDAE61' lw 1
set linetype 14 lc rgb '#FEE08B' lw 1
set linetype 15 lc rgb '#E6F598' lw 1
set linetype 16 lc rgb '#ABDDA4' lw 1
set linetype 17 lc rgb '#66C2A5' lw 1
set linetype 18 lc rgb '#3288BD' lw 1
set linetype 19 lc rgb '#8F5E99' lw 1
set linetype 20 lc rgb '#aaaaaa' lw 1
set linetype cycle  20

#set logscale y
#set ylabel "log(count)"
set ylabel "count"
set xlabel "month"

plot for [IDX=1:20] datafile index (IDX-1) u 2:3 w linespoints t columnheader(1)

### To run 
#gnuplot -e "outfile='test.svg'" line_plot.gp 

