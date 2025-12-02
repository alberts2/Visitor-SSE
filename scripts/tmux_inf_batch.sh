#!/usr/bin/env sh

START_IDX=$1
END_IDX=$2

for IDX in $(seq ${START_IDX} ${END_IDX});do
    ./scripts/tmux_inf_one.sh ${IDX}
done
