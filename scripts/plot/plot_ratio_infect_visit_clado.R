#############
# LIBRARIES #
#############

library(ggplot2)
library(HDInterval)
library(rjson)
library(data.table)
library(ape)

num_loc = 3
sim_batch = 7

##############
# FILESYSTEM #``
##############

#filepaths for project directories
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
##############
# CLADOGENETIC MIGRATION & VISITOR #
##############

out_inf_clado_fp = paste0(fp, "/output/logs/",num_loc,"_locations/log_sim",sim_batch,"_",num_loc,"loc","/cladogenetic\ migration") # sim9 under cladogenetic migration
out_inf_visit_fp = paste0(fp, "/output/logs/",num_loc,"_locations/log_sim",sim_batch,"_",num_loc,"loc","/visitor/survival") # sim9 under visitor

# out_inf_clado_fp = paste0(fp, "/output/logs/",num_loc,"_locations/log_sim",sim_batch,"_",num_loc,"loc","/cladogenetic\ migration_onlyprior") # sim9 under cladogenetic migration
# out_inf_visit_fp = paste0(fp, "/output/logs/",num_loc,"_locations/log_sim",sim_batch,"_",num_loc,"loc","/visitor_onlyprior") # sim9 under visitor


plot_fp = paste0(fp, "/scripts/plot")
# Define replicate indicies
rep_idx = 1:370 # For replicate 0 to 200 in RIS

#Create full file path for both data files
log_visit_files = paste0(out_inf_visit_fp, "/out.idx_", rep_idx-1, ".seed_5.model.log")
log_clado_files = paste0(out_inf_clado_fp, "/out.idx_", rep_idx-1, ".seed_5.model.log")

log_visit_files = sort(log_visit_files)
log_clado_files = sort(log_clado_files)

###################
# DATA PROCESSING #
###################

#store data for each replicate 
rep_dat_list = list()
rep_dat_idx = 1
#

#loop through each replicate index
for (i in rep_idx) {
  # check that both inference file and simulation file for index i exist 
  if (file.exists(log_visit_files[i]) && file.exists(log_clado_files[i]) == FALSE){
    next # skip to the next sample 
  }
  if (file.exists(log_visit_files[i]) && file.exists(log_clado_files[i]) == TRUE){
    #Print out progress
    cat("Processing ", i,"\n", sep="")
    # prepare matrix for replicate i
    rep_dat_i = matrix(NA, nrow=3, ncol=4)
    colnames(rep_dat_i) = c("variable", "idx",  "visitor", "clado_migration")
    rep_dat_i[,1] = c("within-location infection rate","visitor infects resident rate","resident infects visitor rate")
    rep_dat_i[,2] = i
    #
    for (j in 1:3) {
      log_visit_dat  = read.table(log_visit_files[i],header = TRUE, sep = "", stringsAsFactors = FALSE)
      log_clado_dat  = read.table(log_clado_files[i],header = TRUE, sep = "", stringsAsFactors = FALSE)
      if (j == 1){
        p_visit_mean = mean(log_visit_dat$clado_rates.1.)
        p_clado_mean = mean(log_clado_dat$clado_rates.1.)
      }
      else if (j == 2){
        infect_event = paste0("clado_rates.",1+num_loc,".")
        p_visit_mean = mean(log_visit_dat[[infect_event]] * 2)
        p_clado_mean = mean(log_clado_dat$clado_rates.2. * 2)
      }
      else if (j == 3){
        infect_event = paste0("clado_rates.",3+num_loc,".")
        p_visit_mean = mean(log_visit_dat[[infect_event]] * 2)
        p_clado_mean = mean(log_clado_dat$clado_rates.2. * 2)
      }
      #store results in matrix
      rep_dat_i[ rep_dat_i[,1] == rep_dat_i[,1][j], 3:ncol(rep_dat_i) ] = c(
        p_visit_mean, p_clado_mean)
    }
    #add results from replicate into single data frame
    rep_dat_list[[rep_dat_idx]] = data.frame(rep_dat_i)
    rep_dat_idx = rep_dat_idx + 1
  }
}
# set types for columns in the dataframe
df_dat = rbindlist(rep_dat_list)
df_dat$idx = as.numeric(df_dat$idx)
df_dat$visitor = as.numeric(df_dat$visito)
df_dat$clado_migration = as.numeric(df_dat$clado_migration)

# Sort the data frame according to its index
df_dat = df_dat[order(idx)]

# plotting

plot_list = list()

#loop for each parameter
for (i in 1:3) {
  # get dataset for parameter
  param = rep_dat_i[,1][i]
  df_dat_i = df_dat[ df_dat$variable==param, ]
  
  # make plot
  p = ggplot( df_dat_i, aes(x=clado_migration, y=visitor) )
  
  # plot 1:1 line
  p = p + geom_abline(intercept=0, slope=1, lty=2, color="gray")
  
  # plot true vs. mean points
  p = p + geom_point(color='red')
  
  # plot labels & title
  p = p + xlab("clado migration") + ylab("visitor")
  p = p + ggtitle(param)
  
  # adjust font size
  p = p +   theme(
    plot.title = element_text(size = 20, face = "bold"),         # Title size
    axis.title = element_text(size = 18),                        # Axis title size
    axis.text = element_text(size = 16),                         # Axis text size
    legend.title = element_text(size = 18),                      # Legend title size
    legend.text = element_text(size = 16)                        # Legend text size
  )
  
  plot_list[[i]] = p
  
  # save figure
  plot_fn = paste0(plot_fp, "/plot_ratio_visit_clado_", param, ".pdf")
  # plot_fn = paste0(plot_fp, "/plot_ratio_visit_clado_", param, "_prioronly",".pdf")
  print(plot_fn)
  pdf(plot_fn, height=7, width=7)
  print(p)
  dev.off()
}
# 
# 
plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
# }
