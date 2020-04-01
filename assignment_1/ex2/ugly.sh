awk '$1 == "dataset"' plot.txt |  egrep -o '(\d+_|(\d|\.)*μ)' | rev | cut -c2- | rev > pyplot.txt
