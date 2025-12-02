library(plotrix)
library(phytools)
#source("/Users/albertsoewongsono/Documents/Code Testing/visitor_sse/code/plot/phytools.mjl.R")
source("/Users/mlandis/projects/vib_div/code/plot/phytools.mjl.R")

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  base_fn = "out._full.seed_5"
} else if (length(args)==1) {
  base_fn = args[1]
}

# IO
fp = "/Users/mlandis/projects/visitor_sse/"
#fp ="/Users/albertsoewongsono/Documents/Code Testing/visitor_sse/"
fn = paste("code/plot/", base_fn, sep="")
phy_fn = paste(fp, fn, ".stoch.txt",  sep="")
plot_fp = paste(fp, "code/plot/", sep="")
col_fn = paste(plot_fp, "state_colors.n25.txt",sep="")

# plotting settings
write_pdf = !FALSE
prompt = FALSE
if (write_pdf) {
  pdf( paste(plot_fp, base_fn,".stoch_map.pdf",sep="") )
}

# read data
dat_col = read.table(col_fn,sep=",",header=T)
dat_ch = read.table(phy_fn, sep="\t", header=T)

# get phylo n_it, n_burn
phy = as.vector(dat_ch[,ncol(dat_ch)])

# read/plot/append simmap trees
simphy = list()
iterations = dat_ch[,1]
n_it = length(iterations)
burn = 0.75
n_burn = max(1, floor(n_it * burn))


ladderize_stree = function(s){
  
  s_ladder = ladderize(s, right = F)
  edge_strings = paste(s_ladder$edge[,1], s_ladder$edge[,2], sep=",")
  old_edge_strings = rownames(s_ladder$mapped.edge)
  old_edge_idx_dict = 1:length(old_edge_strings)
  names(old_edge_idx_dict) = old_edge_strings
  old_edge_idx_dict[edge_strings]
  s_ladder$mapped.edge = s_ladder$mapped.edge[edge_strings,]
  s_ladder$maps = s_ladder$maps[old_edge_idx_dict[edge_strings]]
  return(s_ladder)

}

# resident states
resident_states = c("1", "7", "13", "19", "25")

for (j in n_burn:n_it) {
  
  if (j %% 10 != 0) {
    next
  }
  
  n = which(dat_ch[,1]==iterations[j])
  cat("tree",j,":",iterations[j],"->",n,"\n")
  sim2 = read.simmap(text=phy[n])
  #sim2 = ladderize.simmap(sim2, right=F)
  sim2 = ladderize_stree(sim2)
  
  # find relevant colors
  colors = vector()
  for (i in 1:length( sim2$maps ) ) {
    colors = c(colors, names(sim2$maps[[i]]) )
  }
  colors = sort(as.numeric(unique(colors)))
  
  # convert state numbers to state labels    
  offset = 1     # RevBayes states are base-0
  #colnames(sim2$mapped.edge) = as.vector(dat_col$name[as.numeric(colnames(sim2$mapped.edge))+offset])
  colnames(sim2$mapped.edge) = as.vector(as.numeric(colnames(sim2$mapped.edge))+offset)
  for (j in 1:length(sim2$maps)) {
    if (length(sim2$maps[[j]]) > 0) {
      #names(sim2$maps[[j]]) = as.vector(dat_col$name[as.numeric(names(sim2$maps[[j]]))+offset])
      names(sim2$maps[[j]]) = as.vector(as.numeric(names(sim2$maps[[j]]))+offset)
    }
    sim2$maps[[j]] = rev(sim2$maps[[j]])
  }
  simphy[[n]] = sim2
  
  # colors
  cols = as.vector(dat_col$color)
  names(cols) = as.character(1:25)
  
  
  #plotTree(sim2)
  #nodelabels()
  # plot simmap
  op <- par(oma = c(4,4,2,2) + 0.1, mar=c(2,4,2,2))  # bottom, left, top, right
  plotSimmap(sim2, cols, split.vertical=T, lwd=2.5, ftype="off")
  rng <- range(ape::node.depth.edgelength(sim2))
  print(rng)
  if (rng[2] - rng[1] > 0) {
      days = c(31, 31, 29, 8) # Dec 01-31, Jan 01-31, Feb 01-29, Mar 01-08
      total_days = sum(days)
      tree_height = max(nodeHeights(sim2))
      print(tree_height)
      pos = c()
      pos[4] = tree_height-7
      pos[3] = tree_height-(29+7)
      pos[2] = tree_height-(31+29+7)
      pos[1] = tree_height-(31+31+29+7)
      labs = c("Dec 01 2020", "Jan 01 2020", "Feb 01 2020", "Mar 01 2020")
      #pos = c(0, tree_height)
      #labs = c("A","B")
      axis(side=1, at=pos, labels=labs)
  }
  #par(op)
  #nodelabels(cex=0.5)
  # add labels for RV, VR, VV events
  # NOTE: not working because simmap.ladderize does
  #       something strange to node indexing. Sean might
  #       know how to fix?
  # Goal: label internal nodes as RR, VR, RV, VV
  clado_idx  = c()
  clado_type = c()
  int_node = unique(sim2$edge[,1])
  for (j in 1:length(int_node)) {
    
    # get index   
    pa_idx = which(sim2$edge[,2] == int_node[j])
    ch_idx = which(sim2$edge[,1] == int_node[j])
    
    # get clado state triplet
    if (length(pa_idx)==0) {
      pa_st = NA
    } else {
      pa_st = rev(names(sim2$maps[[pa_idx]]))[1] # first element is last in time
    }
    ch1_st = names(sim2$maps[[ch_idx[1]]])[1] # last element is first in time
    ch2_st = names(sim2$maps[[ch_idx[2]]])[1] # last element is first in time
    
    # determine visitor/resident status for each branch
    #pa_flag = ifelse( , "R", "V")
    
    pa_res = (pa_st %in% resident_states)
    ch1_res = (ch1_st %in% resident_states)
    ch2_res = (ch2_st %in% resident_states)
    
    type=NA #"--"
    if (is.na(pa_st)) {
      # both children visitors
      if (ch1_res && ch2_res) {
        type = NA # "RR"
      } else if (!ch1_res && !ch2_res) {
        type = "VV"
      } else {
        type = NA # "RV_or_VR"
      }
    } else {
      if (pa_res && ch1_res && ch2_res) {
        type = NA #"RR"
      } else if (!ch1_res && !ch2_res) {
        type = "VV"
      } else if ((ch1_res && !ch2_res && !pa_res) || (!ch1_res && ch2_res && !pa_res)) {
        type = "VR"
      } else if ((ch1_res && !ch2_res && pa_res) || (!ch1_res && ch2_res && pa_res)) {
        type = "RV"
      } else {
        print("Undetected case!")
        cat(pa_res, ch1_res, ch2_res,"\n")
      }
    }
    
    clado_idx = c(clado_idx, int_node[j])
    clado_type = c(clado_type, type)
  }
  clado_text = clado_type
  clado_text[clado_type=="VR"] = "V"
  clado_text[clado_type=="VV"] = "V"
  clado_text[clado_type=="RV"] = "R"
  clado_idx[which(is.na(clado_type))] = NA
  clado_type[clado_type=="VR"] = 21
  clado_type[clado_type=="RV"] = 21 #22
  clado_type[clado_type=="VV"] = 21 #23
  clado_type = as.numeric(clado_type)
  
  
  nodelabels(text=NA, pch=clado_type, node=clado_idx, cex=1.25, frame="none", col="black", bg="white", adj=c(0.5,0.5))
  #nodelabels(text=clado_text, pch=clado_type, node=clado_idx, cex=1.0) #col="black", bg="white")
  #nodelabels(text=clado_text, node=clado_idx, cex=1.0) #col="black", bg="white")
  nodelabels(text=clado_text, node=clado_idx, frame="none", cex=0.5, adj=c(0.5,0.5)) #col="black", bg="white")
  
  location_names = c("Hubei","France","Germany","Italy","Other EU")
  
  home_loc = c("1", "7", "13", "19", "25")
  resident_col = cols[home_loc]
  names(resident_col) = paste0("Resident in ", location_names)
  add.simmap.legend(colors=resident_col,
                    vertical=TRUE,
                    shape="circle",
                    prompt=F,
                    x=0, y=140, fsize=0.8)
  
  
  visitor_col = unique(cols[-match(home_loc,names(cols))])
  names(visitor_col) = paste0("Visitor from ", location_names)
  #add.simmap.legend(colors=unique(cols[-match(home_loc,names(cols)))],
  add.simmap.legend(colors=visitor_col,
                    vertical=TRUE,
                    shape="circle",
                    prompt=F,
                    x=30, y=140, fsize=0.8)
}
if (!write_pdf) {
  invisible(readline(prompt="continue?"))
} else {
  dev.off()
}
