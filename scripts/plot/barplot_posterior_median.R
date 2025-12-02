fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"
in_fp = paste0(fp,"/output/logs/full/visitor")
plot_fp = paste0(fp, "/scripts/plot")

median_vec_dat = readRDS(paste0(in_fp, "/compare_posterior_median_count.rds"))
  resident_states = c(0, 6, 12, 18, 24)
  
  median_vec_dat$infection_type = rep(NA,nrow(median_vec_dat))
  
  for (i in 1:nrow(median_vec_dat)){
    pa_res = (median_vec_dat$end_parent[i] %in% resident_states)
    ch1_res = (median_vec_dat$start_child_1[i]%in% resident_states)
    ch2_res = (median_vec_dat$start_child_2[i] %in% resident_states)
    if (pa_res && ch1_res && ch2_res) {
      median_vec_dat$infection_type[i] = "RR"
    } else if (!ch1_res && !ch2_res) {
      median_vec_dat$infection_type[i] = "VV"
    } else if ((ch1_res && !ch2_res && !pa_res) || (!ch1_res && ch2_res && !pa_res)) {
      median_vec_dat$infection_type[i] = "VR"
    } else if ((ch1_res && !ch2_res && pa_res) || (!ch1_res && ch2_res && pa_res)) {
      median_vec_dat$infection_type[i] = "RV"
    }
  }
  
  RV_vec = median_vec_dat[median_vec_dat$infection_type=="RV",]
  RV_vec = colSums(do.call(rbind, RV_vec$posterior_median_count))
  median(RV_vec)
  
  VR_vec = median_vec_dat[median_vec_dat$infection_type=="VR",]
  VR_vec = colSums(do.call(rbind, VR_vec$posterior_median_count))
  median(VR_vec)
  
  VV_vec = median_vec_dat[median_vec_dat$infection_type=="VV",]
  VV_vec = colSums(do.call(rbind, VV_vec$posterior_median_count))
  median(VV_vec)
  
  RR_vec = median_vec_dat[median_vec_dat$infection_type=="RR",]
  RR_vec = colSums(do.call(rbind, RR_vec$posterior_median_count))
  median(RR_vec)
  
  dat_plot = data.frame(infection_type = c("resident-resident","resident-visitor","visitor-resident","visitor-visitor"),
                        posterior_median_count = c(median(RR_vec),median(RV_vec),median(VR_vec),median(VV_vec)))
  
  dat_plot$percentage <- dat_plot$posterior_median_count / sum(dat_plot$posterior_median_count)
  
  # Create bar plot
  p <- ggplot(dat_plot, aes(x = infection_type, y = posterior_median_count, fill = infection_type)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80,100))+
    geom_text(aes(label = scales::percent(percentage, accuracy = 0.001)), 
              vjust = -0.5, size = 4) +  # Add percent labels above bars
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16)) +
    labs( x = "Infection type", 
          y = "Posterior median of counts", 
          fill = "Infection type") +    # Rename legend title
    theme(plot.title = element_text(hjust = 0.5))  # Center the title
  
  # save plot
  ggsave(paste0(plot_fp, "/summary_posterior_median",".pdf"),p, height = 10, width = 15)
  