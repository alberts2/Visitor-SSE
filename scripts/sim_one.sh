#!/usr/bin/env sh

SIM_DIR="./data/sim"
PREFIX="sim"
IDX=$1
CLEANUP="False"

MIN_FILESIZE=100
MAX_ATTEMPT=100
NUM_ATTEMPT=0
VALID=0

while [ ${VALID} ]; do
    
    # run masterpy script
    python3 ./scripts/sim_one_visit_approx.py ${SIM_DIR} ${PREFIX} ${IDX} ${CLEANUP}

    # is tree file empty?
    FILESIZE=$(wc -c data/sim/sim.${IDX}.tre | cut -d' ' -f1)
    if [ ${FILESIZE} -ge ${MIN_FILESIZE} ]; then
        VALID=1
        break
    fi

    # increment attempts
    # NUM_ATTEMPT=$((NUM_ATTEMPT + 1))
    # echo "NUM_ATTEMPT: ${NUM_ATTEMPT}; FILESIZE: ${FILESIZE} < 50"

done

# convert output
DAT_FN="${SIM_DIR}/sim.${IDX}.dat.nex"
python3 ./scripts/convert_ambiguous_home_location.py ${DAT_FN}
