#!/usr/bin/env sh

IDX=$1
JOB_DIR="sim"
RB_CMD="rb-tp"
RB_STR="idx=${IDX}; job_dir=\"sim\"; source(\"code/visitor_sse.Rev\")"
LOG_FILENAME="out.${IDX}.log"

echo ${RB_STR}
echo ${RB_STR} | ${RB_CMD} | tee ./log/${LOG_FILENAME}

