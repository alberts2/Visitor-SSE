library(ggplot2)
library(HDInterval)

########
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"

# # using unequal rates models with empirical priors on sampling proportions and movements. (with burnin)
in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_unequal_new_priors_burnin/")
in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_unequal_burnin/")

# using unequal rates models with empirical priors on sampling proportions and movements. (with burnin)
# in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_old_priors/")
# in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_old_priors/")

# # using unequal rates models with empirical priors on sampling proportions and movements.
# # for this one, visitor does not have burn-in and has mixing problem 
# in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_unequal_rates_new_priors/")
# in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_unequal_rates_new_priors/")

# # Using equal rates models with empirical priors on sampling proportions and movements
# in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_equal_rates_new_priors/")
# in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_equal_rates_new_priors/")

# # Unequal models with emp priors on movements and fixed sampling
# in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_unequal_rates_fixed_sampling/")
# in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_unequal_fixed_sampling/")

# # Unequal models with where movement rates are independent of away locations with emp priors on move and sampling
# in_fp_visitor      = paste0(fp, "/output/logs/full/visitor_unequal_new_priors_rows/")
# in_fp_cladomigrate = paste0(fp, "/output/logs/full/clado_migration_unequal_rows/")


# 

plot_fp = paste0(fp, "/scripts/plot")

##############
# DATA #
##############
log_visitor = paste0(in_fp_visitor, "out._full.seed_5.model.log")
log_clado   = paste0(in_fp_cladomigrate, "out._fullclado.seed_5.model.log")

dat_visitor = read.table(log_visitor,header = TRUE, sep = "", stringsAsFactors = FALSE)
dat_clado   = read.table(log_clado,header = TRUE, sep = "", stringsAsFactors = FALSE)

dat_visitor = dat_visitor[dat_visitor$Iteration > 1875,] # remove 25% burn-in samples
dat_clado   = dat_clado[dat_clado$Iteration > 1875,] # remove 25% burn-in samples

#######
locs_vec = c("Hubei","France", "Germany","Italy","OtherEU")

# clado events for visitor model from revbayes script (ordered)
clado_visitor_rev = c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,0,5,0,5,6,6,1,6,1,6,7,7,2,7,2,7,8,8,3,8,3,8,9,9,4,9,4,9,10,10,0,10,0,10,11,11,1,11,1,11,12,12,2,12,2,12,13,13,3,13,3,13,14,14,4,14,4,14,15,15,0,15,0,15,16,16,1,16,1,16,17,17,2,17,2,17,18,18,3,18,3,18,19,19,4,19,4,19,20,20,0,20,0,20,21,21,1,21,1,21,22,22,2,22,2,22,23,23,3,23,3,23,24,24,4,24,4,24,0,0,5,0,5,0,1,1,6,1,6,1,2,2,7,2,7,2,3,3,8,3,8,3,4,4,9,4,9,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,5,10,5,10,11,11,6,11,6,11,12,12,7,12,7,12,13,13,8,13,8,13,14,14,9,14,9,14,15,15,5,15,5,15,16,16,6,16,6,16,17,17,7,17,7,17,18,18,8,18,8,18,19,19,9,19,9,19,20,20,5,20,5,20,21,21,6,21,6,21,22,22,7,22,7,22,23,23,8,23,8,23,24,24,9,24,9,24,0,0,10,0,10,0,1,1,11,1,11,1,2,2,12,2,12,2,3,3,13,3,13,3,4,4,14,4,14,4,5,5,10,5,10,5,6,6,11,6,11,6,7,7,12,7,12,7,8,8,13,8,13,8,9,9,14,9,14,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,10,15,10,15,16,16,11,16,11,16,17,17,12,17,12,17,18,18,13,18,13,18,19,19,14,19,14,19,20,20,10,20,10,20,21,21,11,21,11,21,22,22,12,22,12,22,23,23,13,23,13,23,24,24,14,24,14,24,0,0,15,0,15,0,1,1,16,1,16,1,2,2,17,2,17,2,3,3,18,3,18,3,4,4,19,4,19,4,5,5,15,5,15,5,6,6,16,6,16,6,7,7,17,7,17,7,8,8,18,8,18,8,9,9,19,9,19,9,10,10,15,10,15,10,11,11,16,11,16,11,12,12,17,12,17,12,13,13,18,13,18,13,14,14,19,14,19,14,15,15,15,16,16,16,17,17,17,18,18,18,19,19,19,20,20,15,20,15,20,21,21,16,21,16,21,22,22,17,22,17,22,23,23,18,23,18,23,24,24,19,24,19,24,0,0,20,0,20,0,1,1,21,1,21,1,2,2,22,2,22,2,3,3,23,3,23,3,4,4,24,4,24,4,5,5,20,5,20,5,6,6,21,6,21,6,7,7,22,7,22,7,8,8,23,8,23,8,9,9,24,9,24,9,10,10,20,10,20,10,11,11,21,11,21,11,12,12,22,12,22,12,13,13,23,13,23,13,14,14,24,14,24,14,15,15,20,15,20,15,16,16,21,16,21,16,17,17,22,17,22,17,18,18,23,18,23,18,19,19,24,19,24,19,20,20,20,21,21,21,22,22,22,23,23,23,24,24,24)
#
clado_migration_rev = c(0,0,0,0,0,1,0,1,0,0,0,2,0,2,0,0,0,3,0,3,0,0,0,4,0,4,0,1,1,0,1,0,1,1,1,1,1,1,2,1,2,1,1,1,3,1,3,1,1,1,4,1,4,1,2,2,0,2,0,2,2,2,1,2,1,2,2,2,2,2,2,3,2,3,2,2,2,4,2,4,2,3,3,0,3,0,3,3,3,1,3,1,3,3,3,2,3,2,3,3,3,3,3,3,4,3,4,3,4,4,0,4,0,4,4,4,1,4,1,4,4,4,2,4,2,4,4,4,3,4,3,4,4,4,4) 

#joint state-home-away locations lookup table for visitor model
joint_home_away_dat = data.frame(joint_state=NA,home_loc = NA, away_loc = NA)
for (i in 1:5){
  for (j in 1:5)
    joint_home_away_dat = rbind(joint_home_away_dat,c((i-1)*5+j-1,locs_vec[i],locs_vec[j]))
}
joint_home_away_dat = joint_home_away_dat[-1,]

# clado events for visitor model
clado_events_visitor = data.frame(clado_event = NA ,parent = NA, child_1 = NA, child_2 = NA,
                                  home_parent = NA, away_parent = NA,
                                  home_child1 = NA, away_child1 = NA,
                                  home_child2 = NA, away_child2 = NA)

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
  rows_added = c(i,clado_visitor_rev[((i-1)*3+1):(i*3)],parent_home,parent_away,child1_home,child1_away,child2_home,child2_away)
  #
  clado_events_visitor[i,] = rows_added
}

# clado events for clado migration model
clado_events_migration = data.frame(clado_event = NA ,parent = NA, child_1 = NA, child_2 = NA,
                                    away_parent = NA,
                                    away_child1 = NA,
                                    away_child2 = NA)

for (i in 1:45){
  parent_state = clado_migration_rev[(i-1)*3+1] #parent's state
  child1_state = clado_migration_rev[(i-1)*3+2] #child1's state
  child2_state = clado_migration_rev[i*3] #child2's state 
  parent_loc   = locs_vec[parent_state+1]
  child1_loc   = locs_vec[child1_state+1]
  child2_loc   = locs_vec[child2_state+1]
  #
  rows_added = c(i,parent_state,child1_state,child2_state,
                 parent_loc,child1_loc,child2_loc)
  #
  clado_events_migration[i,] = rows_added
}

#######
#resident-infects-resident class 
#######

# in clado migration model: j -> j + j pattern
# in visitor model        : jj -> jj + jj pattern 

# all resident-infects-resident clado events 
clado_events_res_res <- clado_events_visitor[
  clado_events_visitor$home_parent == clado_events_visitor$away_parent &
    clado_events_visitor$home_child1 == clado_events_visitor$away_child1 &
    clado_events_visitor$home_child2 == clado_events_visitor$away_child2 &
    clado_events_visitor$away_parent == clado_events_visitor$away_child1 &
    clado_events_visitor$away_parent == clado_events_visitor$away_child2,
]
names(clado_events_res_res)[1] <- "clado_event_visitor"
# add the clado event ordering from migration model for each event type in clado_events_res_res
clado_events_res_res <- cbind(clado_events_res_res[1], clado_event_migration = rep(NA,nrow(clado_events_res_res)), 
                              clado_events_res_res[2:10],visitor_mean = rep(NA,nrow(clado_events_res_res)),
                              migration_mean = rep(NA,nrow(clado_events_res_res)),
                              visitor_lower = rep(NA,nrow(clado_events_res_res)),
                              visitor_upper = rep(NA,nrow(clado_events_res_res)),
                              migration_lower = rep(NA,nrow(clado_events_res_res)),
                              migration_upper = rep(NA,nrow(clado_events_res_res)))
#
clado_events_res_res = clado_events_res_res[,c(-3,-4,-5)]

for (i in 1:nrow(clado_events_res_res)){
  clado_migration = clado_events_migration[clado_events_migration$away_parent == clado_events_res_res[i,]$away_parent &
                                             clado_events_migration$away_child1 == clado_events_res_res[i,]$away_child1 &
                                             clado_events_migration$away_child2 == clado_events_res_res[i,]$away_child2,]$clado_event
  #
  clado_events_res_res$clado_event_migration[i] = clado_migration
  
}

# get the posterior means and 95% HPD for res-res infections class from both models

for (i in 1:nrow(clado_events_res_res)){
  # get posterior mean and 95% HPD interval for that clado event from visitor model
  which_clado_visitor = paste0("clado_rates.",clado_events_res_res$clado_event_visitor[i],".")
  visitor_mean    = mean(dat_visitor[[which_clado_visitor]])
  visitor_lower   = as.numeric(hdi(dat_visitor[[which_clado_visitor]])[1])
  visitor_upper   = as.numeric(hdi(dat_visitor[[which_clado_visitor]])[2])
  # get posterior mean and 95% HPD interval for that clado event from migration model
  which_clado_migration = paste0("clado_rates.",clado_events_res_res$clado_event_migration[i],".")
  migration_mean  = mean(dat_clado[[which_clado_migration]])
  migration_lower = as.numeric(hdi(dat_clado[[which_clado_migration]])[1])
  migration_upper = as.numeric(hdi(dat_clado[[which_clado_migration]])[2])
  #
  clado_events_res_res$visitor_mean[i]    = visitor_mean
  clado_events_res_res$visitor_lower[i]   = visitor_lower
  clado_events_res_res$visitor_upper[i]   = visitor_upper
  clado_events_res_res$migration_mean[i]  = migration_mean
  clado_events_res_res$migration_lower[i] = migration_lower
  clado_events_res_res$migration_upper[i] = migration_upper
}

#######
#resident-infects-visitor class 
#######

# in clado migration model: j -> j + i pattern (i is visitor & j is resident here)
# in visitor model        : jj -> jj + ij pattern 

# all resident-infects-visitor clado events 
# this is for jj -> jj + ij pattern
right_pattern_visit = clado_events_visitor[clado_events_visitor$home_parent == clado_events_visitor$away_parent &
                                             clado_events_visitor$home_parent == clado_events_visitor$home_child1 &
                                             clado_events_visitor$away_parent == clado_events_visitor$away_child1 &
                                             clado_events_visitor$home_parent != clado_events_visitor$home_child2,]
# this is for jj -> ij + jj pattern
left_pattern_visit = clado_events_visitor[clado_events_visitor$home_parent == clado_events_visitor$away_parent &
                                            clado_events_visitor$home_parent == clado_events_visitor$home_child2 &
                                            clado_events_visitor$away_parent == clado_events_visitor$away_child2 &
                                            clado_events_visitor$home_parent != clado_events_visitor$home_child1,]

clado_events_res_vis <- rbind(right_pattern_visit,left_pattern_visit) 

names(clado_events_res_vis)[1] <- "clado_event_visitor"

# add the clado event ordering from migration model for each event type in clado_events_res_vis
# j -> j + i and j -> i + j patterns
clado_events_res_vis <- cbind(clado_events_res_vis[1], clado_event_migration = rep(NA,nrow(clado_events_res_vis)), 
                              clado_events_res_vis[2:10],visitor_mean = rep(NA,nrow(clado_events_res_vis)),
                              migration_mean = rep(NA,nrow(clado_events_res_vis)),
                              visitor_lower = rep(NA,nrow(clado_events_res_vis)),
                              visitor_upper = rep(NA,nrow(clado_events_res_vis)),
                              migration_lower = rep(NA,nrow(clado_events_res_vis)),
                              migration_upper = rep(NA,nrow(clado_events_res_vis)))
#
clado_events_res_vis = clado_events_res_vis[,c(-3,-4,-5)]

for (i in 1:nrow(clado_events_res_vis)){
  clado_migration = clado_events_migration[clado_events_migration$away_parent == clado_events_res_vis[i,]$home_parent &
                                             clado_events_migration$away_child1 == clado_events_res_vis[i,]$home_child1 &
                                             clado_events_migration$away_child2 == clado_events_res_vis[i,]$home_child2,]$clado_event
  #
  clado_events_res_vis$clado_event_migration[i] = clado_migration
  
}

# ignore left-right inheritance of children lineages from clado_events_res_vis
# e.g. only consider (j,j) -> (j,j) + (i,j) instead of with (j,j) -> (i,j) + (j,j)

clado_events_res_vis = clado_events_res_vis[1:20,]

# get the posterior means and 95% HPD for res-vis infections class from both models

for (i in 1:nrow(clado_events_res_vis)){
  # get posterior mean and 95% HPD interval for that clado event from visitor model
  which_clado_visitor = paste0("clado_rates.",clado_events_res_vis$clado_event_visitor[i],".")
  visitor_mean    = mean(2*dat_visitor[[which_clado_visitor]]) #multiply the rates by 2 for left-right
  visitor_lower   = as.numeric(hdi(2*dat_visitor[[which_clado_visitor]])[1]) #multiply the rates by 2 for left-right
  visitor_upper   = as.numeric(hdi(2*dat_visitor[[which_clado_visitor]])[2]) #multiply the rates by 2 for left-right
  # get posterior mean and 95% HPD interval for that clado event from migration model
  which_clado_migration = paste0("clado_rates.",clado_events_res_vis$clado_event_migration[i],".")
  migration_mean  = mean(2*dat_clado[[which_clado_migration]])
  migration_lower = as.numeric(hdi(2*dat_clado[[which_clado_migration]])[1])
  migration_upper = as.numeric(hdi(2*dat_clado[[which_clado_migration]])[2])
  #
  clado_events_res_vis$visitor_mean[i]    = visitor_mean
  clado_events_res_vis$visitor_lower[i]   = visitor_lower
  clado_events_res_vis$visitor_upper[i]   = visitor_upper
  clado_events_res_vis$migration_mean[i]  = migration_mean
  clado_events_res_vis$migration_lower[i] = migration_lower
  clado_events_res_vis$migration_upper[i] = migration_upper
}

#######
#visitor-infects-resident class 
#######

# in clado migration model: j -> j + i pattern (i is visitor & j is resident here)
# in visitor model        : ij -> jj + ij pattern 

# all visitor-infects-resident clado events 
# this is for ij -> ij + jj pattern

right_pattern_visit = clado_events_visitor[clado_events_visitor$home_parent != clado_events_visitor$away_parent &
                                             clado_events_visitor$home_parent == clado_events_visitor$home_child1 &
                                             clado_events_visitor$away_parent == clado_events_visitor$away_child1 &
                                             clado_events_visitor$home_parent != clado_events_visitor$home_child2 &
                                             clado_events_visitor$home_child2 == clado_events_visitor$away_child2,]

# this is for ij -> jj + ij pattern
left_pattern_visit = clado_events_visitor[clado_events_visitor$home_parent != clado_events_visitor$away_parent &
                                            clado_events_visitor$home_parent == clado_events_visitor$home_child2 &
                                            clado_events_visitor$away_parent == clado_events_visitor$away_child2 &
                                            clado_events_visitor$home_parent != clado_events_visitor$home_child1 &
                                            clado_events_visitor$home_child1 == clado_events_visitor$away_child1,]

clado_events_vis_res <- rbind(right_pattern_visit,left_pattern_visit) 

names(clado_events_vis_res)[1] <- "clado_event_visitor"

# add the clado event ordering from migration model for each event type in clado_events_vis_res
# j -> j + i and j -> i + j patterns
clado_events_vis_res <- cbind(clado_events_vis_res[1], clado_event_migration = rep(NA,nrow(clado_events_vis_res)), 
                              clado_events_vis_res[2:10],visitor_mean = rep(NA,nrow(clado_events_vis_res)),
                              migration_mean = rep(NA,nrow(clado_events_vis_res)),
                              visitor_lower = rep(NA,nrow(clado_events_vis_res)),
                              visitor_upper = rep(NA,nrow(clado_events_vis_res)),
                              migration_lower = rep(NA,nrow(clado_events_vis_res)),
                              migration_upper = rep(NA,nrow(clado_events_vis_res)))
#
clado_events_vis_res = clado_events_vis_res[,c(-3,-4,-5)]

for (i in 1:nrow(clado_events_vis_res)){
  clado_migration = clado_events_migration[clado_events_migration$away_parent == clado_events_vis_res[i,]$away_parent &
                                             clado_events_migration$away_child1 == clado_events_vis_res[i,]$home_child1 &
                                             clado_events_migration$away_child2 == clado_events_vis_res[i,]$home_child2,]$clado_event
  #
  clado_events_vis_res$clado_event_migration[i] = clado_migration
  
}

# ignore left-right inheritance of children lineages from clado_events_vis_res
# e.g. only consider (i,j) -> (i,j) + (j,j) instead of with (i,j) -> (j,j) + (i,j)

clado_events_vis_res = clado_events_vis_res[1:20,]

# get the posterior means and 95% HPD for vis-res infections class from both models

for (i in 1:nrow(clado_events_vis_res)){
  # get posterior mean and 95% HPD interval for that clado event from visitor model
  which_clado_visitor = paste0("clado_rates.",clado_events_vis_res$clado_event_visitor[i],".")
  visitor_mean    = mean(2*dat_visitor[[which_clado_visitor]]) #multiply the rates by 2 for left-right
  visitor_lower   = as.numeric(hdi(2*dat_visitor[[which_clado_visitor]])[1]) #multiply the rates by 2 for left-right
  visitor_upper   = as.numeric(hdi(2*dat_visitor[[which_clado_visitor]])[2]) #multiply the rates by 2 for left-right
  # get posterior mean and 95% HPD interval for that clado event from migration model
  which_clado_migration = paste0("clado_rates.",clado_events_vis_res$clado_event_migration[i],".")
  migration_mean  = mean(2*dat_clado[[which_clado_migration]])
  migration_lower = as.numeric(hdi(2*dat_clado[[which_clado_migration]])[1])
  migration_upper = as.numeric(hdi(2*dat_clado[[which_clado_migration]])[2])
  #
  clado_events_vis_res$visitor_mean[i]    = visitor_mean
  clado_events_vis_res$visitor_lower[i]   = visitor_lower
  clado_events_vis_res$visitor_upper[i]   = visitor_upper
  clado_events_vis_res$migration_mean[i]  = migration_mean
  clado_events_vis_res$migration_lower[i] = migration_lower
  clado_events_vis_res$migration_upper[i] = migration_upper
}

# set factor for away parent's locations 
clado_events_res_res$away_parent = factor(clado_events_res_res$away_parent, levels = c("Hubei","France","Germany","Italy","OtherEU"))
clado_events_res_vis$away_parent = factor(clado_events_res_vis$away_parent, levels = c("Hubei","France","Germany","Italy","OtherEU"))
clado_events_vis_res$away_parent = factor(clado_events_vis_res$away_parent, levels = c("Hubei","France","Germany","Italy","OtherEU"))
######
#Plotting

p_res_res = ggplot(clado_events_res_res, aes(x = visitor_mean, y = migration_mean, color = away_parent)) +
  geom_point(size = 3) 
p_res_res = p_res_res + geom_segment(aes(x = visitor_mean, xend = visitor_mean, 
                                         y = migration_lower, yend = migration_upper), 
                                     linewidth = 0.6) 
p_res_res = p_res_res + geom_segment(aes(x = visitor_lower, xend = visitor_upper,
                                         y = migration_mean, yend = migration_mean), 
                                     linewidth = 0.6)
p_res_res = p_res_res + geom_abline(intercept=0, slope=1, lty=2, color="black",size = 1.2) + scale_x_log10(limits = c(0.02,0.40))  + scale_y_log10(limits = c(0.02,0.40)) 
p_res_res = p_res_res + xlab("Visitor SIR Infection Rate") + ylab("Cladogenetic Migration SIR Infection Rate") + labs(title = "Resident-Infects-Resident Class", color = "Infection location")+
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 20),   # x-axis tick labels
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),       # x-axis title
        axis.title.y = element_text(size = 20),       # y-axis title
        legend.text = element_text(size = 18),   # legend item labels
        legend.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24)
  )    # y-axis tick labels

p_res_res = p_res_res + scale_color_manual(values = c("#3B88C0","#D04A3A","#3C9850","#9352A6","#A87732"))
####

# Some of the upper bounds from the visitor model are outside the limit of y axis
# I keep those points using oob_keep 
p_res_vis = ggplot(clado_events_res_vis, aes(x = visitor_mean, y = migration_mean, color = away_parent)) +
  geom_point(size = 3) 
p_res_vis = p_res_vis + geom_segment(aes(x = visitor_mean, xend = visitor_mean, 
                                         y = migration_lower, yend = migration_upper), 
                                     linewidth = 0.6) 
p_res_vis = p_res_vis + geom_segment(aes(x = visitor_lower, xend = visitor_upper,
                                         y = migration_mean, yend = migration_mean), 
                                     linewidth = 0.6)
p_res_vis = p_res_vis + geom_abline(intercept=0, slope=1, lty=2, color="black",size = 1.2) + scale_x_log10(limits = c(10^-9,1))  + scale_y_log10(limits = c(10^-9,1)) 
p_res_vis = p_res_vis +  xlab("Visitor SIR Infection Rate") + ylab("Cladogenetic Migration SIR Infection Rate") + labs(title = "Resident-Infects-Visitor Class", color = "Infection location")+
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 20),   # x-axis tick labels
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),       # x-axis title
        axis.title.y = element_text(size = 20),       # y-axis title
        legend.text = element_text(size = 18),   # legend item labels
        legend.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24)
  )    # y-axis tick labels

p_res_vis = p_res_vis + scale_color_manual(values = c("#3B88C0","#D04A3A","#3C9850","#9352A6","#A87732"))
####

p_vis_res = ggplot(clado_events_vis_res, aes(x = visitor_mean, y = migration_mean, color = away_parent)) +
  geom_point(size = 3) 
p_vis_res = p_vis_res + geom_segment(aes(x = visitor_mean, xend = visitor_mean, 
                                         y = migration_lower, yend = migration_upper), 
                                     linewidth = 0.6) 
p_vis_res = p_vis_res + geom_segment(aes(x = visitor_lower, xend = visitor_upper,
                                         y = migration_mean, yend = migration_mean), 
                                     linewidth = 0.6)
p_vis_res = p_vis_res + geom_abline(intercept=0, slope=1, lty=2, color="black",size = 1.2) + scale_x_log10(limits = c(10^-4,0.4))  + scale_y_log10(limits = c(10^-4,0.4)) 
p_vis_res = p_vis_res +  xlab("Visitor SIR Infection Rate") + ylab("Cladogenetic Migration SIR Infection Rate") + labs(title = "Visitor-Infects-Resident Class", color = "Infection location")+
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 20),   # x-axis tick labels
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),       # x-axis title
        axis.title.y = element_text(size = 20),       # y-axis title
        legend.text = element_text(size = 18),   # legend item labels
        legend.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24)
  )# y-axis tick labels

p_vis_res = p_vis_res + scale_color_manual(values = c("#3B88C0","#D04A3A","#3C9850","#9352A6","#A87732"))

plot_res_res = paste0(plot_fp, "/plot_res_res.pdf")
print(plot_res_res)
pdf(plot_res_res, height=7, width=14)
print(p_res_res)
dev.off()


plot_res_vis = paste0(plot_fp, "/plot_res_vis.pdf")
print(plot_res_vis)
pdf(plot_res_vis, height=7, width=14)
print(p_res_vis)
dev.off()

plot_vis_res = paste0(plot_fp, "/plot_vis_res.pdf")
print(plot_vis_res)
pdf(plot_vis_res, height=7, width=14)
print(p_vis_res)
dev.off()