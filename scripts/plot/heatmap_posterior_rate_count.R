# This script gives visual representation for posterior mean rates from mcmc analysis and posterior mean
# counts from stoch character map for full Nadeau's tree 

####### libraries #######
##########################
library(ggplot2)

####### File System #######
##########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
in_fp = paste0(fp,"/output/logs/full/visitor_unequal_new_priors_burnin") # unequal visitor with burn in and new priors
plot_fp = paste0(fp, "/scripts/plot")

# lookup table to translate NxN compound state (home-current) to N^2 states, N = number of locations
home_current_state_df = data.frame(home = rep(c("A","B","C","D","E"),each = 5), 
                                   current = rep(c("A","B","C","D","E"),5), 
                                   state = seq(1:25)-1)

# all possible branching events with parent end state and child1 and child2 start states with 5 locations
branching_event_df = read.table(paste0(in_fp,"/branch_events_5loc.csv"),header = T,sep =',')

#### INITIALIZE
# initialize dataframe to build the network
network_dat  = data.frame(end_parent = integer(), start_child_1 = integer(), start_child_2 = integer(),
                          posterior_mean_count = numeric(), posterior_mean_rate = numeric(), from_parent = character(),
                          to_child_1 = character(), to_child_2 = character())

# Load the dataframe from plot_posterior_mean_and_count.R
from_to_dat = read.table(paste0(in_fp,"/compare_posterior_mean_count.csv"),header = T,sep = ',')

# Delete some unused columns
from_to_dat$from = NULL
from_to_dat$to   = NULL
from_to_dat$weight_mean_count = NULL
from_to_dat$weight_mean_rate  = NULL
from_to_dat$weight_ratio      = NULL

from_to_dat$from_parent = rep(NA,nrow(from_to_dat))
from_to_dat$to_child_1  = rep(NA,nrow(from_to_dat))
from_to_dat$to_child_2  = rep(NA,nrow(from_to_dat))

for (i in 1:nrow(from_to_dat)){
  from_to_dat$from_parent[i] = paste(home_current_state_df[home_current_state_df$state==from_to_dat$end_parent[i],1:2], collapse = "")
  from_to_dat$to_child_1[i] = paste(home_current_state_df[home_current_state_df$state==from_to_dat$start_child_1[i],1:2], collapse = "")
  from_to_dat$to_child_2[i] = paste(home_current_state_df[home_current_state_df$state==from_to_dat$start_child_2[i],1:2], collapse = "")
}

# remove duplicated branching patterns due to left right orientation (e.g. AB -> AB + CB and AB -> CB + AB will be treated as one)
sort_unique_branching_event = branching_event_df[!duplicated(t(apply(branching_event_df, 1, sort))), ]

for (i in 1:nrow(sort_unique_branching_event)){
  if ((sort_unique_branching_event$end_parent[i] == sort_unique_branching_event$start_child_1[i]) && (sort_unique_branching_event$start_child_1[i] == sort_unique_branching_event$start_child_2[i]) ){ # for within-location branching
    added_row = from_to_dat[from_to_dat$end_parent == sort_unique_branching_event$end_parent[i] & 
                          from_to_dat$start_child_1 == sort_unique_branching_event$start_child_1[i] &
                          from_to_dat$start_child_2 == sort_unique_branching_event$start_child_2[i], ]
    network_dat = rbind(network_dat,added_row)
  } else { #for across-location branching
    added_row = from_to_dat[from_to_dat$end_parent == sort_unique_branching_event$end_parent[i] & 
                          from_to_dat$start_child_1 == sort_unique_branching_event$start_child_1[i] &
                          from_to_dat$start_child_2 == sort_unique_branching_event$start_child_2[i], ]
    #
    added_row_2 = from_to_dat[from_to_dat$end_parent == sort_unique_branching_event$end_parent[i] & 
                            from_to_dat$start_child_1 == sort_unique_branching_event$start_child_2[i] &
                            from_to_dat$start_child_2 == sort_unique_branching_event$start_child_1[i], ]
    #
    network_dat = rbind(network_dat,added_row)
    network_dat$posterior_mean_count[i] = added_row$posterior_mean_count + added_row_2$posterior_mean_count # add mean_count from ij -> ij + kj and ij -> kj + ij
    network_dat$posterior_mean_rate[i]  = added_row$posterior_mean_rate + added_row_2$posterior_mean_rate # add mean_rate from ij -> ij + kj and ij -> kj + ij
  }
}

network_dat = network_dat[order(network_dat$end_parent),] # sort according to parent's state

# Get posterior counts and rates for each combinatior of parent state and one child state
network_count = subset(network_dat,select = c(from_parent,to_child_2,posterior_mean_count)) # child_1 always follows parent state
network_rate  = subset(network_dat,select = c(from_parent,to_child_2,posterior_mean_rate)) # child_1 always follows parent state

# remove all resident-resident infection events 
network_count_nores = network_count[network_count$from_parent != network_count$to_child_2, ]
network_rate_nores  = network_rate[network_rate$from_parent != network_rate$to_child_2, ]

# Heatmap for count and rate, not including resident-resident infections
x_axis = unique(from_to_dat$from)
y_axis = x_axis
heatmap_dat = expand.grid(parent_state=x_axis,child_state=y_axis)
heatmap_dat$posterior_count = NA
heatmap_dat$posterior_rate = NA

for (i in 1:nrow(heatmap_dat)){
  which_row = which(network_count_nores[[1]] == as.character(heatmap_dat[i, 1]) & 
             network_count_nores[[2]] == as.character(heatmap_dat[i, 2]))
  if (length(which_row) != 0){
    heatmap_dat$posterior_count[i] = network_count_nores[which_row,]$posterior_mean_count
    heatmap_dat$posterior_rate[i] = network_rate_nores[which_row,]$posterior_mean_rate
  }
}

# Heatmap for count and rate for resident-resident infections
heatmap_dat_res = expand.grid(parent_state=c("AA","BB","CC","DD","EE"),child_state=c("AA","BB","CC","DD","EE"))
heatmap_dat_res$posterior_count = NA
heatmap_dat_res$posterior_rate = NA

for (i in 1:nrow(heatmap_dat_res)){
  which_row = which(network_count[[1]] == as.character(heatmap_dat_res[i, 1]) & 
                      network_count[[2]] == as.character(heatmap_dat_res[i, 2]))
  if (length(which_row) != 0){
    heatmap_dat_res$posterior_count[i] = network_count[which_row,]$posterior_mean_count
    heatmap_dat_res$posterior_rate[i] = network_rate[which_row,]$posterior_mean_rate
  }
}



plot_count = ggplot(heatmap_dat, aes(x = parent_state, y = child_state, fill = posterior_count)) +
             geom_tile(color = "white") +
             scale_fill_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red", "darkred"))  +
             theme_minimal() +
             theme(axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   legend.text = element_text(size = 16))
             labs(title = "Posterior mean count", x = "Parent state", y = "Child state",fill = "Posterior mean count")

plot_count_res = ggplot(heatmap_dat_res, aes(x = parent_state, y = child_state, fill = posterior_count)) +
                 geom_tile(color = "white") +
                 scale_fill_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red", "darkred"))  +
                 theme_minimal() +
                 theme(axis.text.x = element_text(size = 16),
                       axis.text.y = element_text(size = 16),
                       legend.text = element_text(size = 16))
                 labs(title = "Posterior mean count including resident-resident", x = "Parent state", y = "Child state",fill = "Posterior mean count")


plot_rate = ggplot(heatmap_dat, aes(x = parent_state, y = child_state, fill = posterior_rate)) +
            geom_tile(color = "white") +
            scale_fill_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red", "darkred"))  +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 16),
                  axis.text.y = element_text(size = 16),
                  legend.text = element_text(size = 16))+
            labs(title = "Posterior mean rate", x = "Parent state", y = "Child state",fill = "Posterior mean rate")

plot_rate_res = ggplot(heatmap_dat_res, aes(x = parent_state, y = child_state, fill = posterior_rate)) +
                geom_tile(color = "white") +
                scale_fill_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red", "darkred"))  +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 16),
                      axis.text.y = element_text(size = 16),
                      legend.text = element_text(size = 16))
                labs(title = "Posterior mean rate including resident-resident", x = "Parent state", y = "Child state",fill = "Posterior mean rate")

ggsave(paste0(plot_fp, "/heatmap_posterior_count",".pdf"), plot_count, height = 10, width = 15)
ggsave(paste0(plot_fp, "/heatmap_posterior_rate",".pdf"), plot_rate, height = 10, width = 15)

ggsave(paste0(plot_fp, "/heatmap_posterior_count_res_res",".pdf"), plot_count_res, height = 10, width = 15)
ggsave(paste0(plot_fp, "/heatmap_posterior_rate_res_res",".pdf"), plot_rate_res, height = 10, width = 15)
