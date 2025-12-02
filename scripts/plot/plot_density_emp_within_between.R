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
# out_inf_fp_clado_no_migrate   = paste0(fp, "/output/logs/full/clado_migration_no_migration_rate/")

# # Using new priors on sampling proportions (equal rates models)
# out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor_equal_rates_new_priors/")
# out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration_equal_rates_new_priors/")
# out_inf_fp_clado_no_migrate   = paste0(fp, "/output/logs/full/clado_migration_no_migration_rate/")

# Using new priors on sampling proportions (unequal rates models)
out_inf_fp_visitor            = paste0(fp, "/output/logs/full/visitor_unequal_rates_new_priors/")
out_inf_fp_clado              = paste0(fp, "/output/logs/full/clado_migration_unequal_rates_new_priors/")
out_inf_fp_clado_no_migrate   = paste0(fp, "/output/logs/full/clado_migration_no_migration_rate/")

plot_fp = paste0(fp, "/scripts/plot")

##############
# DATA #
##############
log_files_visitor            = paste0(out_inf_fp_visitor, "out._full.seed_5.model.log")
log_files_clado              = paste0(out_inf_fp_clado, "out._fullclado.seed_5.model.log")
log_files_clado_no_migrate   = paste0(out_inf_fp_clado_no_migrate, "out._fullclado.seed_5.model.log")

dat_visitor            = read.table(log_files_visitor,header = TRUE, sep = "", stringsAsFactors = FALSE)
dat_clado              = read.table(log_files_clado,header = TRUE, sep = "", stringsAsFactors = FALSE)
dat_clado_no_migrate   = read.table(log_files_clado_no_migrate,header = TRUE, sep = "", stringsAsFactors = FALSE)

dat_visitor            = dat_visitor[dat_visitor$Iteration > 1875,] # remove 25% burn-in samples
dat_clado              = dat_clado[dat_clado$Iteration > 1875,] # remove 25% burn-in samples
dat_clado_no_migrate   = dat_clado_no_migrate[dat_clado_no_migrate$Iteration > 1875,] # remove 25% burn-in samples

#############################
# WITHIN-LOCATION INFECTION #
#############################
# A = Hubei 
# B = France
# C = Germany
# D = Italy
# E = Other European nations 
# Note: we can just use clado event that corresponds to visitor from any location since 
# visit return rates are equal across locations, similar with clado migration model (migration rate is equal)
#
# In Hubei
df_within_A <- data.frame(value = c(dat_visitor$clado_rates.1.,dat_clado$clado_rates.1.,dat_clado_no_migrate$clado_rates.1.),
                          model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                      each=length(dat_visitor$Iteration)))

fig_within_A <- ggplot(df_within_A, aes(x = value, fill = model)) +
  ggtitle("Within location infection in Hubei density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0, 50))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In France
df_within_B <- data.frame(value = c(dat_visitor$clado_rates.57.,dat_clado$clado_rates.12.,dat_clado_no_migrate$clado_rates.12.),
                          model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                      each=length(dat_visitor$Iteration)))

fig_within_B <- ggplot(df_within_B, aes(x = value, fill = model)) +
  ggtitle("Within location infection in France density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0, 50))+
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Germany
df_within_C <- data.frame(value = c(dat_visitor$clado_rates.113.,dat_clado$clado_rates.23.,dat_clado_no_migrate$clado_rates.23.),
                          model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                      each=length(dat_visitor$Iteration)))

fig_within_C <- ggplot(df_within_C, aes(x = value, fill = model)) +
  ggtitle("Within location infection in Germany density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0, 50))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Italy
df_within_D <- data.frame(value = c(dat_visitor$clado_rates.169.,dat_clado$clado_rates.34.,dat_clado_no_migrate$clado_rates.34.),
                          model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                      each=length(dat_visitor$Iteration)))

fig_within_D <- ggplot(df_within_D, aes(x = value, fill = model)) +
  ggtitle("Within location infection in Italy density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0, 50))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Other European nations
df_within_E <- data.frame(value = c(dat_visitor$clado_rates.225.,dat_clado$clado_rates.45.,dat_clado_no_migrate$clado_rates.45.),
                          model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                      each=length(dat_visitor$Iteration)))

fig_within_E <- ggplot(df_within_E, aes(x = value, fill = model)) +
  ggtitle("Within location infection in other European nations")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0, 50))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

#############################
# BETWEEN-LOCATION INFECTION #
# Resident-infects-visitor
#############################

# In Hubei
df_within_between_A <- data.frame(value = c(2*dat_visitor$clado_rates.46.,2*dat_clado$clado_rates.2.,2*dat_clado_no_migrate$clado_rates.2.),
                                  model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                              each=length(dat_visitor$Iteration)))

fig_between_A <- ggplot(df_within_between_A, aes(x = value, fill = model)) +
  ggtitle("Between location infection in Hubei density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.03))+
  scale_y_continuous(limits = c(0, 3500))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In France
df_within_between_B <- data.frame(value = c(2*dat_visitor$clado_rates.8.,2*dat_clado$clado_rates.10.,2*dat_clado_no_migrate$clado_rates.10.),
                                  model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                              each=length(dat_visitor$Iteration)))

fig_between_B <- ggplot(df_within_between_B, aes(x = value, fill = model)) +
  ggtitle("Between location infection in France density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.03))+
  scale_y_continuous(limits = c(0, 3500))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Germany
df_within_between_C <- data.frame(value = c(2*dat_visitor$clado_rates.20.,2*dat_clado$clado_rates.19.,2*dat_clado_no_migrate$clado_rates.19.),
                                  model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                              each=length(dat_visitor$Iteration)))

fig_between_C <- ggplot(df_within_between_C, aes(x = value, fill = model)) +
  ggtitle("Between location infection in Germany density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.03))+
  scale_y_continuous(limits = c(0, 3500))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Italy
df_within_between_D <- data.frame(value = c(2*dat_visitor$clado_rates.32.,2*dat_clado$clado_rates.28.,2*dat_clado_no_migrate$clado_rates.28.),
                                  model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                              each=length(dat_visitor$Iteration)))

fig_between_D <- ggplot(df_within_between_D, aes(x = value, fill = model)) +
  ggtitle("Between location infection in Italy density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.03))+
  scale_y_continuous(limits = c(0, 3500))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )

# In Other European nations
df_within_between_E <- data.frame(value = c(2*dat_visitor$clado_rates.44.,2*dat_clado$clado_rates.37.,2*dat_clado_no_migrate$clado_rates.37.),
                                  model = rep(c("Visitor SIR", "Cladogenetic migration SIR","Pure cladogenetic migration SIR"),
                                              each=length(dat_visitor$Iteration)))

fig_between_E <- ggplot(df_within_between_E, aes(x = value, fill = model)) +
  ggtitle("Between location infection in other European nations density")+
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.03))+
  scale_y_continuous(limits = c(0, 3500))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 20),   # x-axis tick labels
    axis.text.y = element_text(size = 20)    # y-axis tick labels
  )


#############################
# SAVE PLOTS#
#############################
plot_list = list()
plot_list[[1]] = fig_within_A
plot_list[[2]] = fig_within_B
plot_list[[3]] = fig_within_C
plot_list[[4]] = fig_within_D
plot_list[[5]] = fig_within_E
plot_list[[6]] = fig_between_A
plot_list[[7]] = fig_between_B
plot_list[[8]] = fig_between_C
plot_list[[9]] = fig_between_D
plot_list[[10]] = fig_between_E

for (i in 1:10){
  p=plot_list[[i]]
  if (i < 6){
    plot_fn = paste0(plot_fp, "/plot_within_",i, ".pdf")
  }else{
    plot_fn = paste0(plot_fp, "/plot_between_",i, ".pdf")
  }
  print(plot_fn)
  pdf(plot_fn, height=7, width=10)
  print(p)
  dev.off()
}

plot_list[[1]] 
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
plot_list[[5]]
plot_list[[6]]
plot_list[[7]]
plot_list[[8]]
plot_list[[9]]
plot_list[[10]]
