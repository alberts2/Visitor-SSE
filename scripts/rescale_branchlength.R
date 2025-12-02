# Load packages
library(ape)

########################
##### FILE SYSTEM ######
########################
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
# out_sim_fp = paste0(fp,"/data/sim7_rescale")  # dataset 7 rescaled
out_sim_fp = paste0(fp,"/data/sim7_rescale100")  # dataset 7 rescaled by 100

rep_idx = 1:350

sim_files = paste0(out_sim_fp, "/sim.", rep_idx, ".tre")
sim_idx = c()

# Get indexes from all tree files that exist in the folder 
for (i in 1:length(sim_files)){
  if(file.exists(sim_files[i]) == TRUE){
    sim_idx <- append(sim_idx,i)
  } else {
    next
  }
}

# Rescale all branch lengths of each individual tree 
for (i in 1:length(sim_idx)){
  tree_scale <- read.tree(paste("sim.",sim_idx[i],".tre",sep = ""))
  # Rescale edge lengths
  # tree_scale$edge.length <- tree_scale$edge.length*10
  tree_scale$edge.length <- tree_scale$edge.length*100
  # Rescale the stem branch length
  # tree_scale$root.edge   <- tree_scale$root.edge*10
  tree_scale$root.edge   <- tree_scale$root.edge*100
  # Save the modified tree
  write.tree(tree_scale,paste0(out_sim_fp,"/sim.",sim_idx[i],".tre"))
}