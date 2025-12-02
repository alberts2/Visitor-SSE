#!/bin/bash
# run N simulations using determinisitic setting in masterpy to numerically calculate the trajectories
# for a random set of parameters under the full and approximate models.
for i in {1..10};do
	./master_full_deterministic_SIRV_sim_one.py   ./sim_out full   $i False
	./master_approx_deterministic_SIRV_sim_one.py ./sim_out approx $i False
	./compare_full_and_approx_visit.py sim_out/full.${i}.json sim_out/approx.${i}.json
	echo Finished sim set $i , compuate APE and plotting
done
