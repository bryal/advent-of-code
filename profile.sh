carth c $1 -o a.out
perf record --call-graph dwarf ./a.out
perf script | stackcollapse-perf.pl | flamegraph.pl > perf.svg
xdg-open perf.svg
