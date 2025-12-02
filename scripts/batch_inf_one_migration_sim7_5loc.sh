
#!/bin/bash

PATH=$PATH:/revbayes/projects/installer:/revbayes/projects/installer/rb



sim_num=$(seq 0 341)

# Initialize a counter for job submissions
counter=0


label_LIST=($(seq 0 0))
label=("RIS")




# Initialize a counter for job submissions
counter=0

for t in ${label_LIST[@]}
do	


# Loop through each rep_num
for r in ${sim_num[@]}; do



# Construct the job name
    # Construct the job name
    NAME="${label[$t]}_$r"
    
    # Submit the job
    bsub -G compute-michael.landis \
    -cwd /storage1/fs1/michael.landis/Active/visitor_sse/ \
    -o /storage1/fs1/michael.landis/Active/visitor_sse/output/stdout/$NAME  \
    -J $NAME \
    -q general \
    -g /soewongsono/visitor_sse \
    -n 1 -M 3GB -R "rusage [mem=3GB] span[hosts=1]" \
    -a 'docker(sswiston/phylo_docker:slim_amd64)' /bin/bash /storage1/fs1/michael.landis/Active/visitor_sse/scripts/inf_one_migration_sim7_5loc.sh

    # Increment the counter
    ((counter++))

    # Check if counter has reached 20
    if [ $counter -eq 20 ]; then
        # Reset counter
        counter=0
        # Wait for 10 seconds
        sleep 10
    fi

done

done
