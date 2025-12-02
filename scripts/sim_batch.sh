#!/usr/bin/env sh

START_IDX=$1
END_IDX=$2

#for ((i=${START_IDX};i<${END_IDX};i++)); do
#for {i I
for IDX in $(seq ${START_IDX} ${END_IDX});do
    ./scripts/sim_one.sh ${IDX} &
done
