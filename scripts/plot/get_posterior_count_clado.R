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
in_fp = paste0(fp,"/output/logs/full/clado_migration_unequal_burnin") #unequal clado migration with burn in
plot_fp = paste0(fp, "/scripts/plot")

####### Initialize #######
##########################


# Store posterior mean rate and posterior mean count for each event 
compare_dat = compare_dat <- data.frame(
  type = c("within-location", "between-location"),
  posterior_mean_count = rep(NA,2)
)

#
clado_migration_rev = c(0,0,0,0,0,1,0,1,0,0,0,2,0,2,0,0,0,3,0,3,0,0,0,4,0,4,0,1,1,0,1,0,1,1,1,1,1,1,2,1,2,1,1,1,3,1,3,1,1,1,4,1,4,1,2,2,0,2,0,2,2,2,1,2,1,2,2,2,2,2,2,3,2,3,2,2,2,4,2,4,2,3,3,0,3,0,3,3,3,1,3,1,3,3,3,2,3,2,3,3,3,3,3,3,4,3,4,3,4,4,0,4,0,4,4,4,1,4,1,4,4,4,2,4,2,4,4,4,3,4,3,4,4,4,4) 

branching_event_df = data.frame(end_parent = NA, start_child_1 = NA, start_child_2 = NA)

for (i in 1:45){
  parent_state = clado_migration_rev[(i-1)*3+1] #parent's state
  child1_state = clado_migration_rev[(i-1)*3+2] #child1's state
  child2_state = clado_migration_rev[i*3] #child2's state 
  #
  rows_added = c(parent_state,child1_state,child2_state)
  #
  branching_event_df[i,] = rows_added
}

count_dat = cbind(branching_event_df,
                  count = rep(0,nrow(branching_event_df)))

#read the stoch mapping for the full tree under visitor model
stoch_dat = read.table(paste0(in_fp,"/out._fullclado.seed_5.stoch_summ.tsv"),sep = "\t",header = T)

#sort by MCMC iteration 
stoch_dat_sort = stoch_dat[order(stoch_dat$iteration),]

#list of all unique mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

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
  }
  print(paste0("done iteration ",iteration_vec[j]))
}

count_within = sum(count_dat[count_dat$end_parent == count_dat$start_child_1 & count_dat$start_child_1==count_dat$start_child_2,]$count)
count_between = sum(count_dat$count)-count_within
compare_dat$posterior_mean_count[1] = count_within/length(iteration_vec)
compare_dat$posterior_mean_count[2] = count_between/length(iteration_vec)


# plot bar plot

dat_plot <- data.frame(
  infection_type = factor(c("within-location", "between-location"), 
                          levels = c("within-location", "between-location")),
  posterior_mean_count = c(compare_dat$posterior_mean_count[1], compare_dat$posterior_mean_count[2])
)


dat_plot$percentage <- dat_plot$posterior_mean_count / sum(dat_plot$posterior_mean_count)


# Create bar plot
p <- ggplot(dat_plot, aes(x = infection_type, y = posterior_mean_count, fill = infection_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80,100))+
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.001)), 
            vjust = -0.5, size = 7) +  # Add percent labels above bars
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  labs( x = "Infection type", 
        y = "Sum posterior mean of counts", 
        fill = "Infection type") +    # Rename legend title
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# save plot
ggsave(paste0(plot_fp, "/summary_posterior_count_cladomigrate",".pdf"),p, height = 10, width = 15)

