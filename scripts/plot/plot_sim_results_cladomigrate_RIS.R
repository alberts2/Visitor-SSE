#############
# LIBRARIES #
#############

library(ggplot2)
library(HDInterval)
library(rjson)
library(data.table)
library(ape)

##############
# SETTINGS   #
##############
equal_rates = TRUE

#Set coverage level for credible intervals
coverage = 0.80

# "Infect" <-> "lambda"
#Define parameter names for simulation, inference and their dimensions
sim_names = c("R0", "Sample",
              "VisitApproxInfect_0","VisitApproxInfect_0")
inf_names = c("R_0_param_i", "delta",
              "expected time to within-location infection","expected time to between-location infection")

dim = c(1,1,1,1)

param_names = data.frame(sim=sim_names, inf=inf_names, dim=dim)

num_loc = 5
sim_batch = 9
# model = "cladogenetic migration"
model = "migration"

##############
# FILESYSTEM #``
##############

#filepaths for project directories
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"

# inference file
out_inf_fp = paste0(fp, "/output/logs/",num_loc,"_locations/log_sim",sim_batch,"_",num_loc,"loc/",model)

# simulation file
out_sim_fp = paste0(fp,"/data/sim",sim_batch,"_",num_loc,"loc")  # dataset sim_batch num_loc locations

plot_fp = paste0(fp, "/scripts/plot")

# Define replicate indicies
rep_idx = 1:370 # For replicate 0 to 200 in RIS

#Create full file path for  simulation and inference data files
sim_files = paste0(out_sim_fp, "/sim.", rep_idx-1, ".param_col.csv")
inf_files = paste0(out_inf_fp, "/out.idx_", rep_idx-1, ".seed_5.model.json")
log_files = paste0(out_inf_fp, "/out.idx_", rep_idx-1, ".seed_5.model.log")

sim_files = sort(sim_files)
inf_files = sort(inf_files)
log_files = sort(log_files)

###################
# DATA PROCESSING #
###################

# get file index for files
sim_idx = as.numeric( sapply(sim_files, function(x) { y=strsplit(x,split="\\.")[[1]][2] }) )
inf_idx = sapply(inf_files, function(x) { y=strsplit(x,split="\\.")[[1]][2] })
inf_idx = as.numeric(sapply(inf_idx, function(x) { y=strsplit(x, split="_")[[1]][2] }))

# store failed runs 
fail_runs = c()

# match files by index
#store data for each replicate 
#rep_dat = matrix(NA, nrow=0, ncol=6)
#colnames(rep_dat) = c("variable", "true", "mean", "lower95", "upper95", "covered")
rep_dat_list = list()
rep_dat_idx = 1

#loop through each replicate index
for (i in rep_idx) {
  # check that index match
  # (better: check that files exist and share index name)
  if (!((i-1) %in% sim_idx && (i-1) %in% inf_idx)) {
    warning("cannot find replicate", i-1)
    break 
  }
  
  # check that both inference file and simulation file for index i exist 
  if (file.exists(sim_files[i]) && file.exists(inf_files[i]) == FALSE){
    warning("inference file out.idx_", inf_idx[i]," not exist")
    fail_runs <- append(fail_runs,inf_idx[i])
    next # skip to the next sample 
  }
  if (file.exists(sim_files[i]) && file.exists(inf_files[i]) == TRUE){
    #Print out progress
    cat("Processing ", i, " of ", length(rep_idx), "\n", sep="")
    # prepare matrix for replicate i
    rep_dat_i = matrix(NA, nrow=nrow(param_names), ncol=7)
    colnames(rep_dat_i) = c("variable", "idx",  "true", "mean", "lower", "upper", "covered")
    # rep_dat_i[,1] = c(param_names$sim)
    rep_dat_i[,1] = c(param_names$sim[1:2],
                      "rate for within-location infection","rate for between-location infection")
    # Label the tree index according to the file index that passes the check 
    rep_dat_i[,2] = inf_idx[i]
    
    # read simulation files
    sim_dat = read.csv(sim_files[i], sep=",") 
    
    # read inference files
    inf_dat = readLines(inf_files[i])
    num_lines = length(inf_dat)
    inf_json_list = list()
    json_idx = 1
    for (j in 1:num_lines) {
      if (j == 1) {
        next
      }
      inf_json_list[[json_idx]] = rjson::fromJSON(json_str=inf_dat[j])
      json_idx = json_idx + 1
    }
    inf_json_list = inf_json_list[-(1:75)] # remove 10% of MCMC samples as burn in
    
    for (j in 1:nrow(param_names)) {
      #calculate mean and HPD intervals for inferred parameter
      # get param name
      p_sim_base = param_names$sim[j]
      if (j == 3){# get the rate for  within-location infection
        p_sim_idx = paste0( rep("_0", param_names$dim[j]), collapse="") 
        p_sim = paste0(p_sim_base, p_sim_idx)
        #
        log_dat = read.table(log_files[i],header = TRUE, sep = "", stringsAsFactors = FALSE)
        log_dat = log_dat[-(1:75),] # remove the 10% mcmc samples as burn in
        p_mean = mean(log_dat$clado_rates.1.)
        p_hpd  = HDInterval::hdi(log_dat$clado_rates.1., credMass=0.80)
        #
        p_true  = sim_dat$value[sim_dat$param==p_sim]*sim_dat$value[sim_dat$param=="S0_0_0_0"]
      }
      else if (j == 4){
        p_sim_idx = paste0( rep("_1", param_names$dim[j]), collapse="") 
        p_sim = paste0(p_sim_base, p_sim_idx)
        #
        log_dat = read.table(log_files[i],header = TRUE, sep = "", stringsAsFactors = FALSE)
        log_dat = log_dat[-(1:75),] # remove the 10% mcmc samples as burn in
        p_mean = mean(log_dat$clado_rates.2.*2)
        p_hpd  = HDInterval::hdi(log_dat$clado_rates.2. * 2, credMass=0.80)
        #
        p_true  = sim_dat$value[sim_dat$param==p_sim]*sim_dat$value[sim_dat$param=="S0_0_0_0"]
      }
      else {
        p_sim_idx = paste0( rep("_0", param_names$dim[j]), collapse="") 
        p_sim = paste0(p_sim_base, p_sim_idx)
        #
        p_inf = param_names$inf[j]
        #
        p_mean  = mean(unlist(lapply( inf_json_list, function(x) { x[[p_inf]][1] } )))
        p_hpd   = HDInterval::hdi(unlist(lapply( inf_json_list, function(x) { x[[p_inf]][1] } )), credMass=0.80) 
        #
        p_true  = sim_dat$value[sim_dat$param==p_sim]
      }
      p_lower = p_hpd[1]
      p_upper = p_hpd[2]
      p_cover = (p_true <= p_upper && p_true >= p_lower)
      #store results in matrix
      rep_dat_i[ rep_dat_i[,1] == rep_dat_i[,1][j], 3:ncol(rep_dat_i) ] = c(
        p_true, p_mean, p_lower, p_upper, p_cover)
    }
    #add results from replicate into single data frame
    rep_dat_list[[rep_dat_idx]] = data.frame(rep_dat_i)
    rep_dat_idx = rep_dat_idx + 1
  }
  
}
# set types for columns in the dataframe
df_dat = rbindlist(rep_dat_list)
df_dat$idx = as.numeric(df_dat$idx)
df_dat$true = as.numeric(df_dat$true)
df_dat$mean = as.numeric(df_dat$mean)
df_dat$lower = as.numeric(df_dat$lower)
df_dat$upper = as.numeric(df_dat$upper)
df_dat$covered = as.logical(as.numeric(df_dat$covered))

# Sort the data frame according to its index
df_dat = df_dat[order(idx)]

# Get the tree size from each tree
tree_size <- c()
idx_list <-  df_dat$idx
for (i in 1:length(idx_list)){
  tree_name <- paste0(out_sim_fp, "/sim.", idx_list[i], ".tre")
  tree_file <- read.tree(tree_name)
  tree_size <- append(tree_size,length(tree_file$tip.label))
}

# Append to the dataframe
df_dat$ntips <- tree_size

data_sample <- df_dat[which(df_dat$variable %in% "Sample")]


# Sort by false vs true covered from filtered dataset by tree size
df_dat_false <- df_dat[which(df_dat$variable %in% "R0" & df_dat$covered == FALSE)]
df_dat_true <- df_dat[which(df_dat$variable %in% "R0" & df_dat$covered == TRUE)]


# plotting

plot_list = list()

#loop for each parameter
for (i in 1:nrow(param_names)) {
  # get dataset for parameter
  param = rep_dat_i[,1][i]
  df_dat_i = df_dat[ df_dat$variable==param, ]
  
  # make plot
  p = ggplot( df_dat_i, aes(x=true, y=mean) )
  
  # plot 1:1 line
  p = p + geom_abline(intercept=0, slope=1, lty=2, color="black",size = 1.2)
  
  # plot true vs. mean points
  p = p + geom_point()
  
  # plot HPD segments
  p = p + geom_segment(data = df_dat_i,
                       mapping=aes(x=true, xend=true, y=lower, yend=upper, color=covered))
  
  # # add text for the number of tips for each HPD
  # p = p + geom_text(aes(label = ntips),
  #                   hjust = -0.5, size = 2)
  
  # add coverage stats
  f_coverage = round( sum(df_dat_i$covered) / nrow(df_dat_i), digits=3)
  f_label = paste0("HPD", round(coverage*100,digits=0), ": ", f_coverage)
  p = p + annotate("text", x=-Inf, y=Inf, label=f_label, vjust=2, hjust=0)
  
  # plot labels & title
  p = p + xlab("true") + ylab("estimate")
  p = p + ggtitle(param)
  
  # adjust font size
  p = p +   theme(
    plot.title = element_text(size = 20, face = "bold"),         # Title size
    axis.title = element_text(size = 18),                        # Axis title size
    axis.text = element_text(size = 20),                         # Axis text size
    legend.title = element_text(size = 18),                      # Legend title size
    legend.text = element_text(size = 16)                        # Legend text size
  )
  
  plot_list[[i]] = p
  
  # save figure
  plot_fn = paste0(plot_fp, "/plot_coverage_", param, ".pdf")
  print(plot_fn)
  pdf(plot_fn, height=7, width=10)
  print(p)
  dev.off()
}
# 
# 
plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
