
dat_fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse/data/"

prior_depart_return = data.frame(home = c(rep(c("Hubei","France","Germany","Italy","OtherEU"),each = 5)),
                          dest = c(rep(c("Hubei","France","Germany","Italy","OtherEU"),5)),
                          Q1_depart_years = rep(NA,25), Q3_depart_years = rep(NA,25), mean_depart_years = rep(NA,25),
                          Q1_return_days = rep(NA,25), Q3_return_days = rep(NA,25), mean_return_days = rep(NA,25) )

prior_depart_return = prior_depart_return[prior_depart_return$home != prior_depart_return$dest,]

lnorm_sd       = 0.1 #stricter variance for confident mean estimates 
lnorm_sd_loose = 0.8 #stricter variance for confident mean estimates 

focal_eu = c("France","Germany","Italy")
for (i in 1:nrow(prior_depart_return)){
  if(prior_depart_return[i,]$home %in% focal_eu & prior_depart_return[i,]$dest %in% focal_eu){
    home = prior_depart_return[i,]$home
    dest = prior_depart_return[i,]$dest 
    # mean log normal for depart from home
    lnorm_mean = log(m_d[home,dest]) - 0.5 * lnorm_sd^2
    # mean log normal for return to home
    lnorm_mean_return = log(m_r[home,dest]) - 0.5 * lnorm_sd^2
    # sample vd rate
    sample_depart =  rlnorm(10000, meanlog = lnorm_mean, sdlog = lnorm_sd)
    # sample vr rate
    sample_return =  rlnorm(10000, meanlog = lnorm_mean_return, sdlog = lnorm_sd)
    # 1st and 3rd quartile 1/vd rate 
    first_depart = (1/summary(sample_depart)[2])/365
    third_depart = (1/summary(sample_depart)[5])/365
    # 1st and 3rd quartile 1/vr rate 
    first_return = 1/summary(sample_return)[2]
    third_return = 1/summary(sample_return)[5]
    # mean depart and return in days
    mean_depart = (1/m_d[home,dest])/365
    mean_return = 1/m_r[home,dest]
    #
    prior_depart_return[i,]$Q1_depart_years = first_depart
    prior_depart_return[i,]$Q3_depart_years = third_depart
    prior_depart_return[i,]$mean_depart_years = mean_depart
    #
    prior_depart_return[i,]$Q1_return_days = first_return
    prior_depart_return[i,]$Q3_return_days = third_return
    prior_depart_return[i,]$mean_return_days = mean_return
  } else {
    home = prior_depart_return[i,]$home
    dest = prior_depart_return[i,]$dest 
    # mean log normal for depart from home
    lnorm_mean = log(m_d[home,dest]) - 0.5 * lnorm_sd_loose^2
    # mean log normal for return to home
    lnorm_mean_return = log(m_r[home,dest]) - 0.5 * lnorm_sd_loose^2
    # sample vd rate
    sample_depart =  rlnorm(10000, meanlog = lnorm_mean, sdlog = lnorm_sd_loose)
    # sample vr rate
    sample_return =  rlnorm(10000, meanlog = lnorm_mean_return, sdlog = lnorm_sd_loose)
    # 1st and 3rd quartile 1/vd rate 
    first_depart = (1/summary(sample_depart)[2])/365
    third_depart = (1/summary(sample_depart)[5])/365
    # 1st and 3rd quartile 1/vr rate 
    first_return = (1/summary(sample_return)[2])
    third_return = (1/summary(sample_return)[5])
    # mean depart and return in days
    mean_depart = (1/m_d[home,dest])/365
    mean_return = (1/m_r[home,dest])
    #
    prior_depart_return[i,]$Q1_depart_years = first_depart
    prior_depart_return[i,]$Q3_depart_years = third_depart
    prior_depart_return[i,]$mean_depart_years = mean_depart
    #
    prior_depart_return[i,]$Q1_return_days = first_return
    prior_depart_return[i,]$Q3_return_days = third_return
    prior_depart_return[i,]$mean_return_days = mean_return
  }
}

prior_depart_return[, 3:8] <- round(prior_depart_return[, 3:8], 2)

write.table(prior_depart_return, file = paste0(dat_fp,"/priors_test_visit_depart.csv"), sep = ",", row.names = FALSE, quote = FALSE)