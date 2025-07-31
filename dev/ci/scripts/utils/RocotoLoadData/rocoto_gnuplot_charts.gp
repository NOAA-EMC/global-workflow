#!/usr/bin/gnuplot

# Rocoto Performance Analysis - GNUPlot Visualization Script
# This script displays all charts in a 2x2 grid using X11 terminal

# Set CSV separator
set datafile separator ","

# Set terminal to X11 with proper size and enable multiplot
set terminal x11 size 1400,1000 font "Arial,12" persist
set multiplot layout 2,2 title "Rocoto Workflow Performance Analysis Dashboard" font "Arial,16"

# Chart 1: Average Call Time Comparison (Top Left)
set title "Call Time Performance Comparison" font "Arial,14"
set ylabel "Average Call Time (seconds)"
set xlabel "Workflow Configuration"
set grid ytics
set style fill solid 0.7
set boxwidth 0.8
set xtics rotate by -45
set key off

plot 'rocoto_performance_data.csv' every ::1 using 0:3:xtic(1) with boxes linecolor rgb "#4472C4"

# Chart 2: Session Count vs Performance (Top Right)
set title "Session Count vs Call Time" font "Arial,14"
set xlabel "Number of Sessions"
set ylabel "Average Call Time (seconds)"
set grid
set style fill solid 0.5
unset xtics
set xtics auto
set key off

plot 'rocoto_performance_data.csv' every ::1 using 2:3 with points pointtype 7 pointsize 2 linecolor rgb "#E47470"

# Chart 3: Thread Usage Analysis (Bottom Left)
set title "Thread Usage Patterns" font "Arial,14"
set ylabel "Thread Count"
set xlabel "Workflow Configuration"
set grid ytics
set style fill solid 0.6
set xtics rotate by -45
set key right top

plot 'rocoto_performance_data.csv' every ::1 using 0:4:xtic(1) with boxes title "Start Threads" linecolor rgb "#70AD47", \
     '' every ::1 using 0:5:xtic(1) with boxes title "End Threads" linecolor rgb "#FFC000"

# Chart 4: Success Rate Analysis (Bottom Right)
set title "Workflow Success Analysis" font "Arial,14"
set xlabel "Workflow Configuration"
set ylabel "Success Rate"
set grid ytics
set style fill solid 0.8
set xtics rotate by -45
set yrange [0.99:1.001]
set key off

plot 'rocoto_performance_data.csv' every ::1 using 0:6:xtic(1) with boxes linecolor rgb "#32CD32"

# End multiplot
unset multiplot

# Also create individual PNG files for documentation
set terminal png size 800,600 font "Arial,12"

# Individual Chart 1: Call Time Comparison
set output 'rocoto_calltime_comparison.png'
set title "Rocoto Call Time Performance Comparison" font "Arial,16"
set ylabel "Average Call Time (seconds)"
set xlabel "Workflow Configuration"
set grid ytics
set style fill solid 0.7
set boxwidth 0.8
set xtics rotate by -45
set key off
unset yrange

plot 'rocoto_performance_data.csv' every ::1 using 0:3:xtic(1) with boxes linecolor rgb "#4472C4"

# Individual Chart 2: Session vs Performance
set output 'rocoto_sessions_vs_performance.png'
set title "Session Count vs Average Call Time" font "Arial,16"
set xlabel "Number of Sessions"
set ylabel "Average Call Time (seconds)"
set grid
set key off
unset xtics
set xtics auto

plot 'rocoto_performance_data.csv' every ::1 using 2:3 with points pointtype 7 pointsize 2 linecolor rgb "#E47470"

# Individual Chart 3: Thread Analysis
set output 'rocoto_thread_analysis.png'
set title "Thread Usage Patterns" font "Arial,16"
set ylabel "Thread Count"
set xlabel "Workflow Configuration"
set grid ytics
set style fill solid 0.6
set xtics rotate by -45
set key right top

plot 'rocoto_performance_data.csv' every ::1 using 0:4:xtic(1) with boxes title "Start Threads" linecolor rgb "#70AD47", \
     '' every ::1 using 0:5:xtic(1) with boxes title "End Threads" linecolor rgb "#FFC000"

# Individual Chart 4: Success Rate
set output 'rocoto_success_analysis.png'
set title "Workflow Success Rate Analysis" font "Arial,16"
set xlabel "Workflow Configuration"
set ylabel "Success Rate"
set grid ytics
set style fill solid 0.8
set xtics rotate by -45
set yrange [0.99:1.001]
set key off

plot 'rocoto_performance_data.csv' every ::1 using 0:6:xtic(1) with boxes linecolor rgb "#32CD32"

print "GNUPlot analysis complete!"
print "X11 Dashboard displayed in 2x2 grid"
print "Individual PNG charts generated:"
print "- rocoto_calltime_comparison.png"
print "- rocoto_sessions_vs_performance.png" 
print "- rocoto_thread_analysis.png"
print "- rocoto_success_analysis.png"
