#############
# LIBRARIES #
#############

library(ggplot2)


##############
# FILESYSTEM #``
##############

#filepaths for project directories
fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse"

# inference file
# out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor/")
# out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration/")


# # Using new priors on sampling proportions (equal rates models)
# out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor_equal_rates_new_priors/")
# out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration_equal_rates_new_priors/")


# # Using new priors on sampling proportions (unequal rates models) - with burnin
out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor_unequal_new_priors_burnin/")
out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration_unequal_burnin/")

# Using new priors on sampling proportions (unequal rates models - fixed sampling)
# out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor_unequal_rates_fixed_sampling/")
# out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration_unequal_rates_new_priors/")


plot_fp = paste0(fp, "/scripts/plot")

setwd(fp)
##############
# DATA #
##############
log_files_visitor            = paste0(out_inf_fp_visitor, "out._full.seed_5.model.log")
log_files_clado              = paste0(out_inf_fp_clado, "out._fullclado.seed_5.model.log")

dat_visitor            = read.table(log_files_visitor,header = TRUE, sep = "", stringsAsFactors = FALSE)
dat_clado              = read.table(log_files_clado,header = TRUE, sep = "", stringsAsFactors = FALSE)

dat_visitor            = dat_visitor[dat_visitor$Iteration > 1875,] # remove 25% burn-in samples
dat_clado              = dat_clado[dat_clado$Iteration > 1875,] # remove 25% burn-in samples

#############################
# R_0 #
#############################
# A = Hubei 
# B = France
# C = Germany
# D = Italy
# E = Other European nations 

dat <- data.frame(value  = c(dat_visitor$R_0.1.,dat_clado$R_0.1., #R0 Hubei visitor & migration
                             dat_visitor$R_0.7.,dat_clado$R_0.2., #R0 France visitor & migration
                             dat_visitor$R_0.13.,dat_clado$R_0.3., #R0 Germany visitor & migration
                             dat_visitor$R_0.19.,dat_clado$R_0.4., #R0 Italy visitor & migration
                             dat_visitor$R_0.25.,dat_clado$R_0.5.),#R0 other EU visitor & migration 
                  model  = rep(c("Visitor SIR", "Cladogenetic Migration SIR"), each = 563, times = 5),
                  region =  rep(c("Hubei","France","Germany","Italy","Other EU"), each = 1126))

# lognormal priors with logmean = 0.8 and log stdev = 0.5
priors_dat <- data.frame(value = rlnorm(10000,meanlog = 0.8,sdlog = 0.5),
                         model  = "Prior",
                         region = "Any\n(prior)")

combined_dat <- rbind(priors_dat,dat)

#set factor for regions 
combined_dat$region <- factor(combined_dat$region, levels = c("Any\n(prior)","Hubei", "France", "Germany", "Italy", "Other EU"))
#set factor for models
combined_dat$model <- factor(combined_dat$model, levels = c("Prior","Visitor SIR","Cladogenetic Migration SIR"))
#

# scale to "width" since we want violin width to be independent of observations within each variable.
p = ggplot(combined_dat, aes(x = region, y = value, fill = model)) +
  # Prior with smaller width
  geom_violin(
    data = subset(combined_dat, model == "Prior"),
    trim = FALSE, scale = "width",
    width = 0.4,
    position = position_dodge(width = 0.9)
  ) +
  # Other models with default width
  geom_violin(
    data = subset(combined_dat, model != "Prior"),
    trim = FALSE, scale = "width",
    width = 0.9,
    position = position_dodge(width = 0.9)
  ) +
  theme_minimal() +
  labs(x = "Region", y = "Value", fill = "Model") +
  scale_fill_manual(values = c(
    "Prior" = "#CDC9C9",
    "Visitor SIR" = "#FFAEB9",
    "Cladogenetic Migration SIR" = "#00EEEE"
  ))  +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))


#############################
# SAVE PLOTS#
#############################
plot_df = paste0(plot_fp, "/plot_R0.pdf")
print(plot_df)
pdf(plot_df, height=7, width=14)
print(p)
dev.off()
