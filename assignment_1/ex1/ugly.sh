awk '$1 == "dataset"' plot1.txt |  egrep -o '(\d+_|(\d|\.)*μ)' | rev | cut -c2- | rev > pyplot1.txt
awk '$1 == "dataset"' plot2.txt |  egrep -o '(\d+_|(\d|\.)*μ)' | rev | cut -c2- | rev > pyplot2.txt
