#!/usr/bin/env sh

IDX=$1

# echo ${RB_STR} | ${RB_CMD}
tmux new -d -s "visitor_sse_sim_${IDX}" ./scripts/inf_one.sh ${IDX}

