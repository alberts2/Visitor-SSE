dat_fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse/output/logs/full/visitor_unequal_rates/"

convert_mat <- data.frame("joint_states" = c(0:24),
                          "home" = rep(c("HU","FR","DE","IT","OE"),each=5),
                          "away" = rep(c("HU","FR","DE","IT","OE"),5))

clado_events = read.table(paste0(dat_fp,"clado_events.csv"))
clado_events$coverage = rep(NA,225)
colnames(clado_events) = c("clado_events","parent","child_1","child_2","ESS")
clado_events$clado_events = c(1:225)


clado_events_trans = clado_events 

for (i in 1:nrow(clado_events)){
  #
  joint_parent  = clado_events_trans[i,]$parent 
  joint_child_1 = clado_events_trans[i,]$child_1
  joint_child_2 = clado_events_trans[i,]$child_2
  #
  idx_parent  = which(convert_mat$joint_states==joint_parent)
  idx_child_1 = which(convert_mat$joint_states==joint_child_1)
  idx_child_2 = which(convert_mat$joint_states==joint_child_2)
  #
  clado_events_trans[i,]$parent = paste0("[",convert_mat$home[idx_parent],",",convert_mat$away[idx_parent],"]")
  clado_events_trans[i,]$child_1 = paste0("[",convert_mat$home[idx_child_1],",",convert_mat$away[idx_child_1],"]")
  clado_events_trans[i,]$child_2 = paste0("[",convert_mat$home[idx_child_2],",",convert_mat$away[idx_child_2],"]")
}

bad_cov_list = c(1,4,5,6,7,12,13,14,15,16,17,22,23,24,25,26,27,32,33,34,35,36,
                 37,42,43,44,45,52,53,59,67,68,77,78,87,88,97,98,107,108,114,
                 122,123,132,133,138,139,140,141,142,143,144,145,148,149,150,
                 151,152,153,154,155,158,159,160,161,162,163,164,165,167,168,
                 169,170,173,174,175,176,177,178,179,180,187,188,189,190,197,
                 198,199,200,207,208,209,210,217,218,219,220,224,225)

for (i in 1:nrow(clado_events)){
  if (clado_events_trans$clado_events[i] %in% bad_cov_list){
    clado_events_trans$ESS[i] = "bad"
  } else {
    clado_events_trans$ESS[i] = "good"
  }
}

bad_clado_events = clado_events_trans[clado_events_trans$ESS=="bad",]
good_clado_events = clado_events_trans[clado_events_trans$ESS=="good",]

write.csv(bad_clado_events,file = paste0(dat_fp,"low_ESS_clado.csv"),row.names = F)
write.csv(good_clado_events,file = paste0(dat_fp,"high_ESS_clado.csv"),row.names = F)