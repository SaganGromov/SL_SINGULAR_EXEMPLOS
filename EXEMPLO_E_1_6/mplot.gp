reset

# Set terminal and output
set terminal pngcairo size 1000,700 enhanced font 'Arial,14'
set output 'autovalores.png'

# General settings
set title 'Autofunções obtidas pelo programa'
set xlabel 't'
set ylabel 'f(t)'
set grid
set xrange [:]
set key outside right top box

# Plot from .dat files using different styles
plot \
    'solucao_autovalor_0.dat' using 1:2 w l lw 2 dt 1 lc rgb "red"    t 'n=0', \
    'solucao_autovalor_1.dat' using 1:2 w l lw 2 dt 2 lc rgb "blue"   t 'n=1', \
    'solucao_autovalor_2.dat' using 1:2 w l lw 2 dt 3 lc rgb "green"  t 'n=2', \
    'solucao_autovalor_3.dat' using 1:2 w l lw 2 dt 4 lc rgb "orange" t 'n=3', \
    'solucao_autovalor_4.dat' using 1:2 w l lw 2 dt 5 lc rgb "purple" t 'n=4'
    # 'solucao_autovalor_5.dat' using 1:2 w l lw 2 dt 6 lc rgb "brown"  t 'n=5'
    # 'solucao_autovalor_5.dat' using 1:2 w l lw 2 dt 6 lc rgb "brown"  t 'n=5', \
    # 'solucao_autovalor_6.dat' using 1:2 w l lw 2 dt 7 lc rgb "cyan"   t 'n=6'

# Close output
unset output
reset

