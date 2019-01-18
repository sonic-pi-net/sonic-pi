f1(x) = a*x
a = 0.5
fit f1(x) 'before.dat' using 1:2 via a
 
f2(x) = b*x
b = 0.5
fit f2(x) 'after.dat' using 1:2 via b
 
set xlabel "Length of input"
set ylabel "CPU time to parse"

plot a*x        title 'a*x (Before)',\
     b*x        title 'b*x (After)',\
     "before.dat" using 1:2 title 'Before', \
     "after.dat" using 1:2 title 'After'