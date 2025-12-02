#############
# LIBRARIES #
#############

library(ggplot2)
library(HDInterval)
library(rjson)
library(data.table)


##############
# SETTINGS   #
##############

equal_rates = TRUE
coverage = 0.80

# "Infect" <-> "lambda"
sim_names = c("R0", "Sample", "log_VisitDepart", "log_VisitReturn")
inf_names = c("R_0", "delta", "vd_rate", "vr_rate")
dim = c(1, 1, 2, 2)
param_names = data.frame(sim=sim_names, inf=inf_names, dim=dim)

##############
# FILESYSTEM #``
##############

fp = "/Users/mlandis/projects/visitor_sse"
out_fp = paste0(fp, "/output/sim")
plot_fp = paste0(fp, "/scripts/plot")

rep_idx = 1:20
sim_files = paste0(out_fp, "/sim.", rep_idx, ".param_col.csv")
inf_files = paste0(out_fp, "/out.idx_", rep_idx, ".seed_5.model.json")
sim_files = sort(sim_files)
inf_files = sort(inf_files)

###################
# DATA PROCESSING #
###################

# get file index for files
sim_idx = as.numeric( sapply(sim_files, function(x) { y=strsplit(x,split="\\.")[[1]][2] }) )
inf_idx = sapply(inf_files, function(x) { y=strsplit(x,split="\\.")[[1]][2] })
inf_idx = as.numeric(sapply(inf_idx, function(x) { y=strsplit(x, split="_")[[1]][2] }))

# match files by index
#rep_dat = matrix(NA, nrow=0, ncol=6)
#colnames(rep_dat) = c("variable", "true", "mean", "lower95", "upper95", "covered")
rep_dat_list = list()
rep_dat_idx = 1

for (i in rep_idx) {
    
    # check that index match
    # (better: check that files exist and share index name)
    if (!(i %in% sim_idx && i %in% inf_idx)) {
        warning("cannot find replicate", i)
        break
    }
    cat("Processing ", i, " of ", length(rep_idx), "\n", sep="")
    
    # prepare matrix for replicate i
    rep_dat_i = matrix(NA, nrow=nrow(param_names), ncol=7)
    colnames(rep_dat_i) = c("variable", "idx",  "true", "mean", "lower", "upper", "covered")
    rep_dat_i[,1] = param_names$sim
    rep_dat_i[,2] = i

    # read simulation files
    sim_dat = read.csv(sim_files[i], sep=",") 
    # sim_dat_i[,2] = c(
    #     sim_dat$value[sim_dat$param=="R0_0"],
    #     sim_dat$value[sim_dat$param=="Sample_0"],
    #     sim_dat$value[sim_dat$param=="VisitDepart_0_0"],
    #     sim_dat$value[sim_dat$param=="VisitReturn_0_0"]
    # )
    
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
    
    for (j in 1:nrow(param_names)) {
        # get param name
        p_sim_base = param_names$sim[j]
        p_sim_idx = paste0( rep("_0", param_names$dim[j]), collapse="") 
        p_sim = paste0(p_sim_base, p_sim_idx)
        p_inf = param_names$inf[j]
        
        p_mean  = mean(unlist(lapply( inf_json_list, function(x) { x[[p_inf]][1] } )))
        p_hpd   = HDInterval::hdi(unlist(lapply( inf_json_list, function(x) { x[[p_inf]][1] } )), credMass=0.90)
        p_lower = p_hpd[1]
        p_upper = p_hpd[2]
        p_true  = sim_dat$value[sim_dat$param==p_sim]
        p_cover = (p_true <= p_upper && p_true >= p_lower)
        
        rep_dat_i[ rep_dat_i[,1] == p_sim_base, 3:ncol(rep_dat_i) ] = c(
            p_true, p_mean, p_lower, p_upper, p_cover
        )
        
    }
    
    rep_dat_list[[rep_dat_idx]] = data.frame(rep_dat_i)
    rep_dat_idx = rep_dat_idx + 1
    
}

# set types for columns in the dataframe
df_dat = rbindlist(rep_dat_list)
df_dat$idx = as.numeric(df_dat$idx)
df_dat$true = as.numeric(df_dat$true)
df_dat$mean = as.numeric(df_dat$mean)
df_dat$lower = as.numeric(df_dat$lower)
df_dat$upper = as.numeric(df_dat$upper)
df_dat$covered = as.logical(as.numeric(df_dat$covered))


# plotting

plot_list = list()

for (i in 1:nrow(param_names)) {
    
    # get dataset for parameter
    param = param_names$sim[i]
    df_dat_i = df_dat[ df_dat$variable==param, ]
    
    # make plot
    p = ggplot( df_dat_i, aes(x=true, y=mean) )
    
    # plot 1:1 line
    p = p + geom_abline(intercept=0, slope=1, lty=2, color="gray")
    
    # plot true vs. mean points
    p = p + geom_point()
    
    # plot HPD segments
    p = p + geom_segment(data = df_dat_i,
                         mapping=aes(x=true, xend=true, y=lower, yend=upper, color=covered))
    
    # add coverage stats
    f_coverage = round( sum(df_dat_i$covered) / nrow(df_dat_i), digits=3)
    f_label = paste0("HPD", round(coverage*100,digits=0), ": ", f_coverage)
    p = p + annotate("text", x=-Inf, y=Inf, label=f_label, vjust=2, hjust=0)
    
    # plot labels & title
    p = p + xlab("true") + ylab("estimate")
    p = p + ggtitle(param)
    
    plot_list[[i]] = p
    
    # save figure
    plot_fn = paste0(plot_fp, "/plot_coverage_", param, ".pdf")
    print(plot_fn)
    pdf(plot_fn, height=7, width=7)
    print(p)
    dev.off()
}


plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
