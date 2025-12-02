# This script gives visual representation for posterior mean rates from mcmc analysis and posterior mean
# counts from stoch character map for full Nadeau's tree 

library(igraph)
library(ggraph)
library(patchwork)
library(grid)
# test = function(a){
####### File System #######
##########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
# in_fp = paste0(fp,"/output/logs/full/visitor")
# in_fp = paste0(fp,"/output/logs/full/visitor_equal_rates_new_priors")
# in_fp = paste0(fp,"/output/logs/full/visitor_unequal_rates_fixed_sampling")
in_fp = paste0(fp,"/output/logs/full/visitor_unequal_new_priors_burnin")
# in_fp = paste0(fp,"/output/logs/full/visitor_unequal_new_priors_rows")
# in_fp = paste0(fp,"/output/logs/full/visitor_unequal_rates_fixed_sampling")

plot_fp = paste0(fp, "/scripts/plot")

####### Initialize #######
##########################

# lookup table to translate NxN compound state (home-current) to N^2 states, N = number of locations
home_current_state_df = data.frame(home = rep(c("A","B","C","D","E"),each = 5), 
                                   current = rep(c("A","B","C","D","E"),5), 
                                   state = seq(1:25)-1)

# all possible branching events with parent end state and child1 and child2 start states with 5 locations
branching_event_df = read.table(paste0(in_fp,"/branch_events_5loc.csv"),header = T,sep =',')

# Store posterior mean rate and posterior mean count for each event 
compare_dat = cbind(branching_event_df,
                    posterior_mean_count = rep(NA,nrow(branching_event_df)),
                    posterior_mean_rate  = rep(NA,nrow(branching_event_df)))

count_dat = cbind(branching_event_df,
                  count = rep(0,nrow(branching_event_df)))
#read the stoch mapping for the full tree under visitor model
stoch_dat = read.table(paste0(in_fp,"/out._full.seed_5.stoch_summ.tsv"),sep = "\t",header = T)
#read the mcmc analysis 
log_dat = read.table(paste0(in_fp,"/out._full.seed_5.model.log"),header = TRUE, sep = "", stringsAsFactors = FALSE)

#sort by MCMC iteration 
stoch_dat_sort = stoch_dat[order(stoch_dat$iteration),]

#list of all unique mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

# Get vector of counts for a particular parent-child state combination from each iteration
median_vec_dat <- branching_event_df

median_vec_dat$posterior_median_count <- replicate(nrow(median_vec_dat), rep(0,length(iteration_vec)), simplify = FALSE)


for (j in 1:length(iteration_vec)){ # loop over each MCMC sample (each ancestral state history)
  # j = 195
  stoch_dat_j = stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[j],] # look for the whole history for sampled history j
  #
  parent_dat_j = stoch_dat_j[!is.na(stoch_dat_j$child1_index) & !is.na(stoch_dat_j$child2_index),] # find all parent nodes (the ones that have children lineages)
  all_parent_ind = unique(parent_dat_j$node_index) # find all parent node indices for history j
  for (k in 1:length(all_parent_ind)){ # loop over each parent index for history j
    node_parent_dat_j = parent_dat_j[parent_dat_j$node_index==all_parent_ind[k],] # find all events on the parent edge with node index k
    last_event_parent_j = node_parent_dat_j[nrow(node_parent_dat_j), ] # get the last event on that parent edge with node index k
    if (last_event_parent_j$transition_type == "anagenetic"){
      end_state_parent_j = last_event_parent_j$end_state # get the end state before branching for that parent node k
    } else {
      end_state_parent_j = last_event_parent_j$start_state # get the end state before branching for that parent node k
    }
    # get history for children nodes originated from parent node k
    child_1_stoch_j = stoch_dat_j[stoch_dat_j$node_index == last_event_parent_j$child1_index,]
    child_2_stoch_j = stoch_dat_j[stoch_dat_j$node_index == last_event_parent_j$child2_index,]
    # get the start state of both children (always the first row on child_1_stoch_j and child_2_stoch_j)
    start_state_child_1 = child_1_stoch_j[1,]$start_state
    start_state_child_2 = child_2_stoch_j[1,]$start_state
    # Record the parent_state and child_state combination for that particular parent index k for history j 
    count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count = 
      count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count + 1
    #
    median_vec_dat[median_vec_dat$end_parent == end_state_parent_j & median_vec_dat$start_child_1 == start_state_child_1 & median_vec_dat$start_child_2 == start_state_child_2,]$posterior_median_count[[1]][j]=
      median_vec_dat[median_vec_dat$end_parent == end_state_parent_j & median_vec_dat$start_child_1 == start_state_child_1 & median_vec_dat$start_child_2 == start_state_child_2,]$posterior_median_count[[1]][j] + 1  
  }
  print(paste0("done iteration ",iteration_vec[j]))
}

# get the posterior mean count and rate across mcmc samples
for (i in 1:nrow(compare_dat)){
  compare_dat$posterior_median_count[i] = median(median_vec_dat$posterior_median_count[[i]])
  compare_dat$posterior_mean_count[i] = count_dat$count[i]/length(iteration_vec)
  compare_dat$posterior_mean_rate[i] = mean(log_dat[[paste0("clado_rates.",i,".")]])
}

####### Plot #############
##########################
from_to_dat = cbind(compare_dat,from = rep(NA,nrow(compare_dat)), to = rep(NA,nrow(compare_dat)))

for (l in 1:nrow(from_to_dat)){
  from_to_dat$from[l] = paste(home_current_state_df[home_current_state_df$state==from_to_dat$end_parent[l],1:2], collapse = "")
  #
  from_to_dat$to[l] = paste(paste(home_current_state_df[home_current_state_df$state==from_to_dat$start_child_1[l],1:2], collapse = ""),paste(home_current_state_df[home_current_state_df$state==from_to_dat$start_child_2[l],1:2], collapse = ""),sep = ":")
}

# sort by end state of parent node
from_to_dat = from_to_dat[order(from_to_dat$end_parent),]

write.csv(from_to_dat,paste0(in_fp,"/compare_posterior_mean_count.csv"),row.names = F)
saveRDS(median_vec_dat, file = paste0(in_fp, "/compare_posterior_median_count.rds"))