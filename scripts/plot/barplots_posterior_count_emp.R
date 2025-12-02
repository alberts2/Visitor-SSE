####### libraries #######
##########################
library(ggplot2)
library(scales)

####### File System #######
##########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
# in_fp = paste0(fp,"/output/logs/full/visitor")
# in_fp = paste0(fp,"/output/logs/full/visitor_equal_rates_new_priors")
# in_fp = paste0(fp,"/output/logs/full/visitor_unequal_rates_fixed_sampling")
in_fp = paste0(fp,"/output/logs/full/visitor_unequal_new_priors_burnin")

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

# List all res-res, visit-res and res-visit infections

res_res_list = data.frame(parent = c("AA","BB","CC","DD","EE"),
                          child  = c("AA","BB","CC","DD","EE"))

vis_res_list = data.frame(parent = c("BA","CA","DA","EA",
                                     "AB","CB","DB","EB",
                                     "AC","BC","DC","EC",
                                     "AD","BD","CD","ED",
                                     "AE","BE","CE","DE"),
                          child  = c(rep("AA",4),rep("BB",4),
                                     rep("CC",4),rep("DD",4),
                                     rep("EE",4)))

res_vis_list = data.frame(parent = c(rep("AA",4),rep("BB",4),
                                     rep("CC",4),rep("DD",4),
                                     rep("EE",4)),
                          child  = c("BA","CA","DA","EA",
                                     "AB","CB","DB","EB",
                                     "AC","BC","DC","EC",
                                     "AD","BD","CD","ED",
                                     "AE","BE","CE","DE"))

sum_res_res = 0
sum_vis_res = 0
sum_res_vis = 0 

for (i in 1:5){
  # get res-res infection
  idx = which(network_count$from_parent==res_res_list$parent[i] & network_count$to_child_2==res_res_list$child[i])
  sum_res_res = sum_res_res + network_count$posterior_mean_count[idx]
}

for (i in 1:20){
  # get vis-res infection
  idx = which(network_count$from_parent==vis_res_list$parent[i] & network_count$to_child_2==vis_res_list$child[i])
  sum_vis_res = sum_vis_res + network_count$posterior_mean_count[idx]
  # get res-vis infection
  idx = which(network_count$from_parent==res_vis_list$parent[i] & network_count$to_child_2==res_vis_list$child[i])
  sum_res_vis = sum_res_vis + network_count$posterior_mean_count[idx]
}

# get vis-vis infection
sum_vis_vis = sum(network_count$posterior_mean_count) - sum_res_res - sum_vis_res - sum_res_vis

# plot bar plot

dat_plot <- data.frame(infection_type = c("resident-resident", "resident-visitor", "visitor-resident","visitor-visitor"),
                       posterior_mean_count = c(sum_res_res, sum_res_vis, sum_vis_res, sum_vis_vis)
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
ggsave(paste0(plot_fp, "/summary_posterior_count",".pdf"),p, height = 10, width = 15)
