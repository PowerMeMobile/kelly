set terminal jpeg
set output "test_50000.jpeg"
set title "50,000 operations, secs"
set grid
set key left
set style data histogram
set style fill solid border -1
set xtics nomirror rotate by -45 scale 0 font ",8"
plot "test_50000.dat" u 2:xtic(1) t "Insert", "" u 3 t "Update", "" u 4 t "Read", "" u 5 t "Delete"
