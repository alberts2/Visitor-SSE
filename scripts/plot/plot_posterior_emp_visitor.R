# NOTES:
# -. for each mcmc sample, the number of internal nodes always the same
# -. this because each mcmc sample corresponds to a possible history of characters on the given tree
# -. column "parent_index" is the node label at the start of a branch
# -. column "node_index" is the node label at the end of a branch 
# -. The parent_index of a child branch will be the same as the node_index of the parent branch 
# -. "cladogenesis" always corresponds to a branching event with state change
# -. "no_change" and "anagenetic" does not always end with a branching event, so need to filter using child1_index and child2_index as well
# If it does not end with branching event, child1_index = child2_index = NA
# -. The only difference between "no_change" and "anagenetic" types that lead to a branching event (child_index != NA) is that
# the latter will have state transitions along parent branch, so can have multiple rows of that parent branch that ends with
# the same branching event (if there are > 1 anagenetic events on that parent branch) 
# -. total number of clado events at each mcmc iteration = number of resident infects visitor + number of visitor infects resident 

# state lookup
# home current state
# A     A         0
# A     B         1
# A     C         2
# A     D         3
# A     E         4
# B     A         5
# .     .         .

fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
in_fp = paste0(fp,"/output/logs/full/visitor")
plot_fp = paste0(fp, "/scripts/plot")
                 
####### Initialize #######
##########################
#read the stoch mapping for the full tree under visitor model
stoch_dat = read.table(paste0(in_fp,"/out._full.seed_5.stoch_summ.tsv"),sep = "\t",header = T)

#sort by MCMC iteration 
stoch_dat_sort = stoch_dat[order(stoch_dat$iteration),]

num_int_nodes = length(unique(stoch_dat$node_index)) #num of int nodes on the tree 

iteration_vec = unique(stoch_dat_sort$iteration) #list of all unique mcmc iterations 

#all possible resident infects visitor cladogenetic events for 5 locations
resident_infect_visit_dat = data.frame(end_parent = rep(c(0,6,12,18,24),each=8),
                                       start_child_1 = c(rep(0,4),5,10,15,20,rep(6,4),1,11,16,21,rep(12,4),2,7,17,22,rep(18,4),3,8,13,23,rep(24,4),4,9,14,19),
                                       start_child_2 = c(5,10,15,20,rep(0,4),1,11,16,21,rep(6,4),2,7,17,22,rep(12,4),3,8,13,23,rep(18,4),4,9,14,19,rep(24,4)))


prop_branch_clado = c() #empty vector to store proportion of branching events that are cladogenesis across MCMC iterations
prop_statechange_clado = c() #empty vector to store proportion of state changes associated with cladogenetic nodes versus state changes along branch only (can lead to a branching or not)

count_resident_infect_visitor = c() #this will store the number of resident infects visitor cladogenetic events from each mcmc iteration
count_cladogenesis = c() #this will store the total number of cladogenetic events (resident infects visitor + visitor infects resident) for each mcmc iteration

for (i in 1:length(iteration_vec)){ #loop over each unique mcmc iteration
  ########### for prop_branch_clado
  temp_clado_df = stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] & stoch_dat_sort$transition_type=="cladogenetic",]
  num_clado = nrow(temp_clado_df[!duplicated(temp_clado_df$node_index),]) #num of cladogenetic events for that mcmc iteration
  #
  num_branching_nochange = nrow(stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] &
                                                 stoch_dat_sort$transition_type=="no_change" & 
                                                 !is.na(stoch_dat_sort$child1_index) & 
                                                 !is.na(stoch_dat_sort$child2_index),]) #num of no change branching events for that mcmc iteration
  #
  temp_df=stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] &
                             stoch_dat_sort$transition_type=="anagenetic" & 
                             !is.na(stoch_dat_sort$child1_index) & 
                             !is.na(stoch_dat_sort$child2_index),]
  num_branching_ana = nrow(temp_df[!duplicated(temp_df$node_index), ])# num of anagenetic branching events for that mcmc iteration (note: need to filter by node_index because can have duplicate rows of the same branch due to being anagenetic events)
  #
  total_branching = num_clado + num_branching_nochange + num_branching_ana #total num of branching events for that iteration
  #
  prop_branch_clado = append(prop_branch_clado,num_clado/total_branching)
  ########### for prop_resident_visitor_infect
  count = 0 #initialize the count
  for (j in 1:nrow(resident_infect_visit_dat)){#loop over each possible resident infects visitor cladogenesis event
    parent_df_i_j =  stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] &
                     stoch_dat_sort$transition_type=="cladogenetic" & 
                     stoch_dat_sort$end_state == resident_infect_visit_dat$end_parent[j],] #get all branches where parent has that end state j
    
    if (nrow(parent_df_i_j)==0){ #if there is no event where the parent branch has that end state for a particular mcmc iteration
      next
    } else {
      all_child1_idx = parent_df_i_j$child1_index #get all child 1 node labels from those branches
      all_child2_idx = parent_df_i_j$child2_index #get all child 2 node labels from those branches
      for (k in 1:length(all_child1_idx)){
        #
        # left or right ordering of state inheritance from parents 
        if (stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] & stoch_dat_sort$node_index == all_child1_idx[k],]$start_state[1]==resident_infect_visit_dat$start_child_1[j] &&
            stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] & stoch_dat_sort$node_index == all_child2_idx[k],]$start_state[1]==resident_infect_visit_dat$start_child_2[j]){
          count = count + 1
          # print(paste0("finish ",k," for row ",j," mcmc iteration ",i))
        }
      }
    }
  }
  count_resident_infect_visitor = append(count_resident_infect_visitor,count)
  count_cladogenesis = append(count_cladogenesis,nrow(stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] & stoch_dat_sort$transition_type=="cladogenetic",]))
  ########### for prop_statechange_clado
  num_state_change_clado = nrow(stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i] & stoch_dat_sort$transition_type=="cladogenetic",])
  num_state_change_nonclado = nrow(stoch_dat_sort[stoch_dat_sort$iteration == iteration_vec[i]&stoch_dat_sort$transition_type=="anagenetic" ,])
  prop_statechange_clado = append(prop_statechange_clado,num_state_change_clado/(num_state_change_clado+num_state_change_nonclado))
  prop_statechange_clado = prop_statechange_clado[!is.na(prop_statechange_clado)]
}


# get the proportion of resident infects visitor 
prop_resident_visitor_infect = count_resident_infect_visitor/count_cladogenesis
prop_resident_visitor_infect = prop_resident_visitor_infect[!is.na(prop_resident_visitor_infect)] #need to remove NA because its possible for some mcmc iteration, no cladogenesis event at all
# get the proportion of visitor infects resident 
prop_visitor_resident_infect = 1-prop_resident_visitor_infect

####### PLOTTING #######
##########################

# Posterior proportion of branching events on reconstructed tree that involve a cladogenetic state change (i.e. involves a visitor)
pdf(paste0(plot_fp,"/posterior_prop_cladoevents_vs_allbranchingevents.pdf"), height=7, width=7)
plot(density(prop_branch_clado),xlab = "Proportion",main = "Proportion of cladogenetic branching events",
     cex.axis = 0.8,
     cex.lab = 1.2,
     cex.main = 1.5)
dev.off()

# Posterior proportion of visitor bringing the infection to new location vs acquiring infection in new location
pdf(paste0(plot_fp,"/posterior_prop_visitor_resident_infect_.pdf"), height=7, width=7)
plot(density(prop_resident_visitor_infect),xlab = "Proportion",main = "Proportion of visitor resident infection",
     cex.axis = 0.8,
     cex.lab = 1.2,
     cex.main = 1.5)
lines(density(prop_visitor_resident_infect),col='red')
legend("topright", legend = c("Proportion resident infects visitor", "Proportion visitor infects resident"), col = c("black", "red"), 
      lwd = 2, cex = 0.9)
dev.off()

# Posterior proportion of state changes associated with cladogenetic nodes versus along branches

pdf(paste0(plot_fp,"/posterior_prop_statechange_clado_nonclado_.pdf"), height=7, width=7)
plot(density(prop_statechange_clado),xlab = "Proportion",main = "Proportion of state change associated with cladogenetic \n vs along branches",
     cex.axis = 0.8,
     cex.lab = 1.2,
     cex.main = 1.5)
dev.off()
