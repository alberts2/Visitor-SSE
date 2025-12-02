#!/bin/bash

for i in $(seq 1 370); do
    input_file="../data/sim9/sim.$i.dat.nex" 
    output_file="../data/sim9/sim.$i.dat.unambig.nex"

    if [ -f "$input_file" ]; then
        sed -E -e 's/([0-9]+)[[:space:]]+100/\1 0/g' \
               -e 's/([0-9]+)[[:space:]]+010/\1 1/g' \
               -e 's/([0-9]+)[[:space:]]+001/\1 2/g' \
               -e 's/NCHAR=3/NCHAR=1/g' \
               -e 's/SYMBOLS="01"/SYMBOLS="012"/g' \
               "$input_file" > "$output_file" 
        echo "Processed: $input_file"
    else
        echo "File not found: $input_file"
    fi
done