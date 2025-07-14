reset

# Set terminal and output
set terminal pngcairo size 1000,700 enhanced font 'Arial,14'
set output 'legendre_polynomials.png'

# General settings
set title 'Primeiros cinco polinômios de Legendre'
set xlabel 'x'
set ylabel 'P_n(x)'
set grid

# Set symmetric ranges about 0
set xrange [-1:1]
set yrange [-1:1]

# Draw x and y axes crossing at (0,0)
set xzeroaxis lt 1 lc rgb "black"
set yzeroaxis lt 1 lc rgb "black"
set key outside right top box

# Define the first five Legendre polynomials
P0(x) = 1
P1(x) = x
P2(x) = (3*x**2 - 1)/2
P3(x) = (5*x**3 - 3*x)/2
P4(x) = (35*x**4 - 30*x**2 + 3)/8

# Plot the polynomials using different styles
plot \
    P0(x) w l lw 2 dt 1 lc rgb "red"    t 'n=0', \
    P1(x) w l lw 2 dt 2 lc rgb "blue"   t 'n=1', \
    P2(x) w l lw 2 dt 3 lc rgb "green"  t 'n=2', \
    P3(x) w l lw 2 dt 4 lc rgb "orange" t 'n=3', \
    P4(x) w l lw 2 dt 5 lc rgb "purple" t 'n=4'

# Close output
unset output
reset
