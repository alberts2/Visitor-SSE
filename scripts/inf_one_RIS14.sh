#!/usr/bin/env sh


IFS="_"
read -ra arr <<< $LSB_JOBNAME
IFS=" "

#echo ${arr[@]} 
#
IDX=${arr[1]}

#echo $LSB_JOBNAME 

#IDX=$(echo $LSB_JOBNAME |cut -d '_' -f 2)

#IDX=$1
JOB_DIR="sim9"
RB_CMD="rb"
RB_STR="idx=${IDX}; job_dir=\"sim9\"; source(\"/storage1/fs1/michael.landis/Active/visitor_sse/code/visitor_sse9.Rev\")"
LOG_FILENAME="out.${IDX}.log"

echo ${RB_STR}
echo ${RB_STR} | ${RB_CMD} 

#| tee ./output/logs/${LOG_FILENAME}



