library(ggplot2)

########### VISITOR ################

####### File System #######
##########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
in_fp = paste0(fp,"/output/logs/full/visitor_unequal_new_priors_burnin")

plot_fp = paste0(fp, "/scripts/plot")

####### Initialize #######
##########################

# lookup table to translate NxN compound state (home-current) to N^2 states, N = number of locations
home_current_state_df = data.frame(home = rep(c("A","B","C","D","E"),each = 5), 
                                   current = rep(c("A","B","C","D","E"),5), 
                                   state = seq(1:25)-1)

# all possible branching events with parent end state and child1 and child2 start states with 5 locations
branching_event_df = read.table(paste0(in_fp,"/branch_events_5loc.csv"),header = T,sep =',')

#read the stoch mapping for the full tree under visitor model
stoch_dat = read.table(paste0(in_fp,"/out._full.seed_5.stoch_summ.tsv"),sep = "\t",header = T)
#read the mcmc analysis 
log_dat = read.table(paste0(in_fp,"/out._full.seed_5.model.log"),header = TRUE, sep = "", stringsAsFactors = FALSE)

#sort by MCMC iteration 
stoch_dat_sort = stoch_dat[order(stoch_dat$iteration),]

#list of all unique mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

#drop all failed stochastic mapping histories 
for (i in 1:length(iteration_vec)){
  check_empty = stoch_dat_sort[stoch_dat_sort$iteration==iteration_vec[i],]
  if ( all(is.na(check_empty$transition_time))==TRUE){
    stoch_dat_sort = stoch_dat_sort[stoch_dat_sort$iteration != iteration_vec[i],]
  }
}

#update all unique successful mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

# Get vector of counts for a particular parent-child state combination from each iteration
count_dat <- branching_event_df

count_dat$count <- replicate(nrow(count_dat), rep(0,length(iteration_vec)), simplify = FALSE)

for (j in 1:length(iteration_vec)){ # loop over each MCMC sample (each ancestral state history)
  stoch_dat_j = stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[j],] # look for the whole history for sampled history j
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
      count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count[[1]][j]=
        count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count[[1]][j] + 1  
    }
    print(paste0("done iteration ",iteration_vec[j])) 
  #
}

count_dat$home_parent    <- rep(NA,225)
count_dat$away_parent    <- rep(NA,225)
count_dat$home_parent    <- rep(NA,225)
count_dat$home_child1    <- rep(NA,225)
count_dat$away_child1    <- rep(NA,225)
count_dat$home_child2    <- rep(NA,225)
count_dat$away_child2    <- rep(NA,225)
count_dat$infection_type <- rep(NA,225)

locs_vec = c("Hubei","France", "Germany","Italy","OtherEU")

#joint state-home-away locations lookup table for visitor model
joint_home_away_dat = data.frame(joint_state=NA,home_loc = NA, away_loc = NA)
for (i in 1:5){
  for (j in 1:5)
    joint_home_away_dat = rbind(joint_home_away_dat,c((i-1)*5+j-1,locs_vec[i],locs_vec[j]))
}
joint_home_away_dat = joint_home_away_dat[-1,]

# clado events for visitor model from revbayes script (ordered)
clado_visitor_rev = c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,0,5,0,5,6,6,1,6,1,6,7,7,2,7,2,7,8,8,3,8,3,8,9,9,4,9,4,9,10,10,0,10,0,10,11,11,1,11,1,11,12,12,2,12,2,12,13,13,3,13,3,13,14,14,4,14,4,14,15,15,0,15,0,15,16,16,1,16,1,16,17,17,2,17,2,17,18,18,3,18,3,18,19,19,4,19,4,19,20,20,0,20,0,20,21,21,1,21,1,21,22,22,2,22,2,22,23,23,3,23,3,23,24,24,4,24,4,24,0,0,5,0,5,0,1,1,6,1,6,1,2,2,7,2,7,2,3,3,8,3,8,3,4,4,9,4,9,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,5,10,5,10,11,11,6,11,6,11,12,12,7,12,7,12,13,13,8,13,8,13,14,14,9,14,9,14,15,15,5,15,5,15,16,16,6,16,6,16,17,17,7,17,7,17,18,18,8,18,8,18,19,19,9,19,9,19,20,20,5,20,5,20,21,21,6,21,6,21,22,22,7,22,7,22,23,23,8,23,8,23,24,24,9,24,9,24,0,0,10,0,10,0,1,1,11,1,11,1,2,2,12,2,12,2,3,3,13,3,13,3,4,4,14,4,14,4,5,5,10,5,10,5,6,6,11,6,11,6,7,7,12,7,12,7,8,8,13,8,13,8,9,9,14,9,14,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,10,15,10,15,16,16,11,16,11,16,17,17,12,17,12,17,18,18,13,18,13,18,19,19,14,19,14,19,20,20,10,20,10,20,21,21,11,21,11,21,22,22,12,22,12,22,23,23,13,23,13,23,24,24,14,24,14,24,0,0,15,0,15,0,1,1,16,1,16,1,2,2,17,2,17,2,3,3,18,3,18,3,4,4,19,4,19,4,5,5,15,5,15,5,6,6,16,6,16,6,7,7,17,7,17,7,8,8,18,8,18,8,9,9,19,9,19,9,10,10,15,10,15,10,11,11,16,11,16,11,12,12,17,12,17,12,13,13,18,13,18,13,14,14,19,14,19,14,15,15,15,16,16,16,17,17,17,18,18,18,19,19,19,20,20,15,20,15,20,21,21,16,21,16,21,22,22,17,22,17,22,23,23,18,23,18,23,24,24,19,24,19,24,0,0,20,0,20,0,1,1,21,1,21,1,2,2,22,2,22,2,3,3,23,3,23,3,4,4,24,4,24,4,5,5,20,5,20,5,6,6,21,6,21,6,7,7,22,7,22,7,8,8,23,8,23,8,9,9,24,9,24,9,10,10,20,10,20,10,11,11,21,11,21,11,12,12,22,12,22,12,13,13,23,13,23,13,14,14,24,14,24,14,15,15,20,15,20,15,16,16,21,16,21,16,17,17,22,17,22,17,18,18,23,18,23,18,19,19,24,19,24,19,20,20,20,21,21,21,22,22,22,23,23,23,24,24,24)
#
for (i in 1:225){
  parent_joint_state = clado_visitor_rev[(i-1)*3+1] #parent's joint state
  child1_joint_state = clado_visitor_rev[(i-1)*3+2] #child1's joint state
  child2_joint_state = clado_visitor_rev[i*3] #child2's joint state 
  parent_home = joint_home_away_dat[joint_home_away_dat$joint_state==parent_joint_state,]$home_loc #parent's home loc
  parent_away = joint_home_away_dat[joint_home_away_dat$joint_state==parent_joint_state,]$away_loc #parent's away loc
  child1_home = joint_home_away_dat[joint_home_away_dat$joint_state==child1_joint_state,]$home_loc #child1's home loc
  child1_away = joint_home_away_dat[joint_home_away_dat$joint_state==child1_joint_state,]$away_loc #child1's away loc
  child2_home = joint_home_away_dat[joint_home_away_dat$joint_state==child2_joint_state,]$home_loc #child2's home loc
  child2_away = joint_home_away_dat[joint_home_away_dat$joint_state==child2_joint_state,]$away_loc #child2's away loc
  #
  count_dat[i,5]  = parent_home
  count_dat[i,6]  = parent_away
  count_dat[i,7]  = child1_home
  count_dat[i,8]  = child1_away
  count_dat[i,9]  = child2_home
  count_dat[i,10] = child2_away
  #
  if (parent_home == parent_away && child1_home == child1_away && child2_home == child2_away) { #res-res
    count_dat[i,11] = "res-res"
  } else if (parent_home == parent_away && child1_home != child1_away){ #res-vis
    count_dat[i,11] = "res-vis"
  } else if (parent_home == parent_away && child2_home != child2_away){ #res-vis
    count_dat[i,11] = "res-vis"
  } else if (parent_home != parent_away && child1_home == child1_away){ #vis-res
    count_dat[i,11] = "vis-res"
  } else if (parent_home != parent_away && child2_home == child2_away){ #vis-res
    count_dat[i,11] = "vis-res"
  } else { #vis-vis
    count_dat[i,11] = "vis-vis"
  }
}

# all res-res infection counts 
res_res_dat <- count_dat[count_dat$infection_type == "res-res",]
# all res-vis infection counts 
res_vis_dat <- count_dat[count_dat$infection_type == "res-vis",]
# all vis-res infection counts 
vis_res_dat <- count_dat[count_dat$infection_type == "vis-res",]
# all vis-vis infection counts 
vis_vis_dat <- count_dat[count_dat$infection_type == "vis-vis",]

# Get the percentage of events at each iteration
percent_df <- data.frame(res_res = rep(NA,length(iteration_vec)),
                         res_vis = rep(NA,length(iteration_vec)),
                         vis_res = rep(NA,length(iteration_vec)),
                         vis_vis = rep(NA,length(iteration_vec)),
                         vis_infect = rep(NA,length(iteration_vec)))

for (i in 1:length(iteration_vec)){ #loop over each iteration
  sum_res_res_i = sum(sapply(res_res_dat$count, function(x) x[i]))
  sum_res_vis_i = sum(sapply(res_vis_dat$count, function(x) x[i]))
  sum_vis_res_i = sum(sapply(vis_res_dat$count, function(x) x[i]))
  sum_vis_vis_i = sum(sapply(vis_vis_dat$count, function(x) x[i]))
  #
  percent_df$res_res[i]    = (sum_res_res_i/(sum_res_res_i+sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i))*100
  percent_df$res_vis[i]    = (sum_res_vis_i/(sum_res_res_i+sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i))*100
  percent_df$vis_res[i]    = (sum_vis_res_i/(sum_res_res_i+sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i))*100
  percent_df$vis_vis[i]    = (sum_vis_vis_i/(sum_res_res_i+sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i))*100
  percent_df$vis_infect[i] = ((sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i)/(sum_res_res_i+sum_res_vis_i+sum_vis_res_i+sum_vis_vis_i))*100 #infections that involve at least one visitor
}

# Plotting 
plot_dat = data.frame(value = rep(NA,length(iteration_vec)*5),
                      type = rep(c("res-res","res-vis","vis-res","vis-vis","all-vis"),each = length(iteration_vec)))

plot_dat[1:length(iteration_vec),1]     = percent_df$res_res
plot_dat[(length(iteration_vec)+1):(2*length(iteration_vec)),1]  = percent_df$res_vis
plot_dat[(2*length(iteration_vec)+1):(3*length(iteration_vec)),1] = percent_df$vis_res
plot_dat[(3*length(iteration_vec)+1):(4*length(iteration_vec)),1] = percent_df$vis_vis
plot_dat[(4*length(iteration_vec)+1):(5*length(iteration_vec)),1] = percent_df$vis_infect
  

########### CLADO MIGRATION ################

####### File System #######
##########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
in_fp = paste0(fp,"/output/logs/full/clado_migration_unequal_burnin") #unequal clado migration with burn in

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

#read the stoch mapping for the full tree under visitor model
stoch_dat = read.table(paste0(in_fp,"/out._fullclado.seed_5.stoch_summ.tsv"),sep = "\t",header = T)

#sort by MCMC iteration 
stoch_dat_sort = stoch_dat[order(stoch_dat$iteration),]

#list of all unique mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

#drop all failed stochastic mapping histories 
for (i in 1:length(iteration_vec)){
  check_empty = stoch_dat_sort[stoch_dat_sort$iteration==iteration_vec[i],]
  if ( all(is.na(check_empty$transition_time))==TRUE){
    stoch_dat_sort = stoch_dat_sort[stoch_dat_sort$iteration != iteration_vec[i],]
  }
}

#update all unique successful mcmc iterations 
iteration_vec = unique(stoch_dat_sort$iteration) 

#
count_dat = cbind(branching_event_df,
                  count = rep(0,nrow(branching_event_df)))

count_dat$count <- replicate(nrow(count_dat), rep(0,length(iteration_vec)), simplify = FALSE)

for (j in 1:length(iteration_vec)){ # loop over each MCMC sample (each ancestral state history)
  # j = 195
  stoch_dat_j = stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[j],] # look for the whole history for sampled history j
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
      count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count[[1]][j] = 
        count_dat[count_dat$end_parent == end_state_parent_j & count_dat$start_child_1 == start_state_child_1 & count_dat$start_child_2 == start_state_child_2,]$count[[1]][j] + 1
    }
    print(paste0("done iteration ",iteration_vec[j]))
  #
}

count_dat$infection_type <- rep(NA,45)

for (i in 1:45){
  if (count_dat[i,]$end_parent == count_dat[i,]$start_child_1 && count_dat[i,]$start_child_1 == count_dat[i,]$start_child_2){ #within-loc
    count_dat[i,"infection_type"] = "within_loc"
  } else {
    count_dat[i,"infection_type"] = "between_loc"
  }
}

# all within-loc infection counts 
within_dat  <- count_dat[count_dat$infection_type == "within_loc",]
# all between-loc infection counts 
between_dat <- count_dat[count_dat$infection_type == "between_loc",]

# Get the percentage of events at each iteration
percent_df <- data.frame(within_loc  = rep(NA,length(iteration_vec)),
                         between_loc = rep(NA,length(iteration_vec)))

for (i in 1:length(iteration_vec)){ #loop over each iteration
  sum_within_i  = sum(sapply(within_dat$count, function(x) x[i]))
  sum_between_i = sum(sapply(between_dat$count, function(x) x[i]))
  #
  percent_df$within_loc[i]  = (sum_within_i/(sum_within_i + sum_between_i))*100
  percent_df$between_loc[i] = (sum_between_i/(sum_within_i + sum_between_i))*100
}

# Plotting 
plot_dat_migration = data.frame(value = rep(NA,length(iteration_vec)*2),
                                type = rep(c("within_loc","between_loc"),each = length(iteration_vec)))

plot_dat_migration[1:length(iteration_vec),1]     = percent_df$within_loc
plot_dat_migration[(length(iteration_vec)+1):(2*length(iteration_vec)),1]  = percent_df$between_loc

########### PLOTTING ################
plot_combined_within = rbind(plot_dat[plot_dat$type=="res-res",],plot_dat_migration[plot_dat_migration$type=="within_loc",])

plot_combined_between = rbind(plot_dat[plot_dat$type=="res-vis",],
                              plot_dat[plot_dat$type=="vis-res",],
                              plot_dat[plot_dat$type=="vis-vis",],
                              plot_dat[plot_dat$type=="all-vis",],
                              plot_dat_migration[plot_dat_migration$type=="between_loc",])
#set factor for type 
plot_combined_within$type  <- factor(plot_combined_within$type, levels = c("within_loc","res-res"))
plot_combined_between$type <- factor(plot_combined_between$type, levels = c("between_loc","res-vis","vis-res","vis-vis","all-vis"))

# scaled by area 
p_within = ggplot(plot_combined_within, aes(x = type, y = value,fill = type)) +
            geom_violin(position = position_dodge(width = 0.9), trim = FALSE) +
            theme_minimal() +
            labs(x = "Infection type", y = "Percentage",fill = "Infection type") +
            scale_fill_manual(values = c("within_loc" = "#CDC9C9","res-res" = "#FFAEB9"))+
            theme(axis.text.x = element_text(size = 24),   # x-axis tick labels
                  axis.text.y = element_text(size = 24),   # y-axis tick labels
                  axis.title.x = element_text(size = 24),  # x-axis title
                  axis.title.y = element_text(size = 24),  # y-axis title
                  legend.text = element_text(size = 16),   # legend item labels
                  legend.title = element_text(size = 16)) 

p_between = ggplot(plot_combined_between, aes(x = type, y = value,fill = type)) +
            geom_violin(position = position_dodge(width = 0.9), trim = FALSE) +
            theme_minimal() +
            labs(x = "Infection type", y = "Percentage",fill = "Infection type") +
            scale_fill_manual(values = c("between_loc" = "#CDC9C9","res-vis" = "#FFAEB9", "vis-res" = "#00EEEE","vis-vis" = "#CD950C","all-vis"="#46934E"),
                              labels = c("between_loc" = "between\nloc",
                                         "res-vis" = "res-vis",
                                         "vis-res" = "vis-res",
                                         "vis-vis" = "vis-vis",
                                         "all-vis" = "all-vis"))+
            scale_x_discrete(
              labels = c("between_loc" = "between\nloc",
                         "res-vis" = "res-vis",
                         "vis-res" = "vis-res",
                         "vis-vis" = "vis-vis",
                         "all-vis" = "all-vis")
            )+
            theme(axis.text.x = element_text(size = 16),   # x-axis tick labels
                  axis.text.y = element_text(size = 24),   # y-axis tick labels
                  axis.title.x = element_text(size = 24),  # x-axis title
                  axis.title.y = element_text(size = 24),  # y-axis title
                  legend.text = element_text(size = 16),   # legend item labels
                  legend.title = element_text(size = 16)) 


# save plot
ggsave(paste0(plot_fp, "/summary_posterior_count_within",".pdf"),p_within, height = 5, width = 7)
ggsave(paste0(plot_fp, "/summary_posterior_count_between",".pdf"),p_between, height = 5, width = 7)
