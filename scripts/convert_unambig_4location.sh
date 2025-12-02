#!/bin/bash

for i in $(seq 1 370); do
    input_file="../data/sim9_4loc/sim.$i.dat.nex" 
    output_file="../data/sim9_4loc/sim.$i.dat.unambig.nex"

    if [ -f "$input_file" ]; then
        sed -E -e 's/([0-9]+)[[:space:]]+1000/\1 0/g' \
               -e 's/([0-9]+)[[:space:]]+0100/\1 1/g' \
               -e 's/([0-9]+)[[:space:]]+0010/\1 2/g' \
               -e 's/([0-9]+)[[:space:]]+0001/\1 3/g' \
               -e 's/NCHAR=4/NCHAR=1/g' \
               -e 's/SYMBOLS="01"/SYMBOLS="0123"/g' \
               "$input_file" > "$output_file" 
        echo "Processed: $input_file"
    else
        echo "File not found: $input_file"
    fi
done