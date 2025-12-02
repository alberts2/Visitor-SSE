library(readxl)
#df = read_xlsx("~/Desktop/Covid_19_Spring-update.xlsx")
# dat_fp = "/Users/mlandis/projects/visitor_sse/data/"
dat_fp = "/Users/albertsoewongsono/Documents/Code Testing/visitor_sse/data/"

# This file contains total number of nights spent in different countries
df_trips = read.csv(paste0(dat_fp, "estat_tour_dem_ttw.tsv"), sep="\t", header=T)
df_nights = read.csv(paste0(dat_fp, "estat_tour_dem_tnw.tsv"), sep="\t", header=T)
df_trips_info = read.csv(paste0(dat_fp, "estat_tour_dem_ttw_info.csv"), sep=",", header=T)
df_nights_info = read.csv(paste0(dat_fp, "estat_tour_dem_tnw_info.csv"), sep=",", header=T)
# This file contains nights spent and trips from other countries to EU
df_nights_other = read.csv(paste0(dat_fp, "estat_tour_occ_ninraw.tsv"), sep="\t", header=T)
df_trips_other = read.csv(paste0(dat_fp, "estat_tour_occ_arnraw.tsv"), sep="\t", header=T)


# DATASET NOTES
# ttw trips?
# tnw nights?
#  purpose TOTAL = PER + PROF
#  duration N_GE1 = N1-3 + N_GE4    ... where N1-3 is 1-3 nights and N_GE4 is 4+ nights


# reduce dataset to relevant columns
colnames(df_trips)[1] = "info"
df_trips = df_trips[,c("info","X2019")]
colnames(df_nights)[1] = "info"
df_nights = df_nights[,c("info","X2019")]
# 
colnames(df_trips_other)[1] = "info"
df_trips_other = df_trips_other[,c("info","X2019")]
colnames(df_nights_other)[1] = "info"
df_nights_other = df_nights_other[,c("info","X2019")]
#
df_dat = data.frame(info=df_trips$info,
                    trips=df_trips$X2019,
                    nights=df_nights$X2019[match(df_trips$info,df_nights$info)])
df_dat = cbind(df_trips_info, df_dat[,2:3])

eu_countries = sort(unique(df_dat$geo)) #Albert's note: Iceland (IS) is not on this list 
eu_cn_countries = c(eu_countries, "CN")
other_eu_countries = c("ES","UK","NL","BE", "CH", "DK", "PT", "CZ", "FI", "IS", "IE", "LU", "NO","PL", "SK", "SE")

# trips and nights data from China 
# I551: hotels and similar accommodation
# I552: holiday and other short-stay accommodation
# I553: camping grounds, recreational vehicle parks and trailer parks 
# I551-I553 : I551 + I552 + I553 

df_nights_china = df_nights_other[3497:3540,]
df_trips_china = df_trips_other[3565:3608,]
# clean up strings in info column 
dest_string = strsplit(df_trips_china$info, ",")
dest_info = unlist(lapply( dest_string, function(x) {tail(x,1) }))

df_nights_china$info = dest_info
df_trips_china$info = dest_info

# combine trips and nights data from china
df_dat_china = data.frame(info=df_trips_china$info,
                          trips=df_trips_china$X2019,
                          nights=df_nights_china$X2019[match(df_trips_china$info,df_nights_china$info)])

# filter to destinations that we want (focal EU and other EU countries ) from China
df_dat_china = df_dat_china[df_dat_china$info %in% c(other_eu_countries,c("FR","DE","IT")),]

# Remove @C, :u, e, and : from trips and nights data in df_dat_china
# Note: C = confidential, u = low reliability, e = estimated 

trips_str_CN = strsplit(df_dat_china$trips, " ")
nights_str_CN = strsplit(df_dat_china$nights, " ")
trips_dat_CN = unlist(lapply(trips_str_CN, function(x) { as.numeric(x[[1]][1]) }))
nights_dat_CN = unlist(lapply(nights_str_CN, function(x) { as.numeric(x[[1]][1]) }))

df_dat_china$trips = trips_dat_CN
df_dat_china$nights = nights_dat_CN

###########
# filtering

# keep total all purposes for travel (total=pers+prof)
# and keep all trip lengths (N_GE1=N1-3+N_GE4)
df_dat = df_dat[ df_dat$purpose=="TOTAL" & df_dat$duration=="N_GE1", ]

# keep only EU and CN as destinations
# df_dat = df_dat[ df_dat$c_dest %in% eu_cn_countries, ]
df_dat = df_dat[ df_dat$c_dest %in% c(eu_cn_countries,"IS"), ] #Albert's note: need to include IS in the list of destinations 

# ignore travel to own country (domestic)
df_dat = df_dat[ df_dat$c_dest != df_dat$geo, ]


# filter down df_dat to what we actually want
home_country = c(other_eu_countries,c("FR","DE","IT"))
away_country = c(other_eu_countries,c("FR","DE","IT"),"CN")
#filter by list of home countries (Other EU, DE, FR,IT) and away countries 
df_dat = df_dat[ df_dat$geo %in% home_country & df_dat$c_dest %in% away_country , ] 

# clean up strings in trips and nights columns
trip_str = strsplit(df_dat$trips, " ")
night_str = strsplit(df_dat$nights, " ")
trip_missing = unlist(lapply( trip_str, function(x) { (":" %in% x) }))
night_missing = unlist(lapply( night_str, function(x) { (":" %in% x) }))
trip_uncertain = unlist(lapply( trip_str, function(x) { ("u" %in% x) }))
night_uncertain = unlist(lapply( night_str, function(x) { ("u" %in% x) }))
trip_numbers = unlist(lapply(trip_str, function(x) { as.numeric(x[[1]][1]) }))
night_numbers = unlist(lapply(night_str, function(x) { as.numeric(x[[1]][1]) }))

# update dataframe columns
df_dat$trips = trip_numbers
df_dat$trips_uncertain = trip_uncertain
df_dat$nights = night_numbers
df_dat$nights_uncertain = night_uncertain

for (i in 1:length(trip_str)) {
  print(trip_str[[i]])
  cat( trip_numbers[i], trip_uncertain[i], trip_missing[i], "\n\n")
}

# RevBayes info

# Albert's: list population sizes from other EU countries in other_eu_countries 
# UK's pop size from here: 
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2021
other_eu_pop_sizes = c(47000000,67000000,17000000,11000000,7000000,5800000,10300000,
                       10500000,5500000,350000,5100000,640000,5300000,37000000,
                       5400000,10400000)
names(other_eu_pop_sizes) = other_eu_countries

#
locations = c("Hubei","France","Germany","Italy","OtherEU")
loc_idx = c(CN=1, FR=2, DE=3, IT=4, OtherEU=5)
pop_sizes = c(57000000,68000000,85000000,60000000)
# pop_sizes[5] = 745000000 - sum(pop_sizes[2:4])
pop_sizes[5] = sum(other_eu_pop_sizes) #Albert's: change other_eu_countries's pop size based on this data?
names(pop_sizes) = names(loc_idx)

prop_Hubei_to_China = pop_sizes[1] / 1400000000

# We need total number of trips and nights for all source countries.
# Let's call the set of source countries S.
# However, many countries in S have NA records. These must be
# be approximated otherwise we will undercount the true numbers for S.
#
# So, let's compute the mean number of trips/nights, mu_trips and mu_nights, 
# per person across all countries in S. Then, compute the estimates for
#    n_trips[i] = mu_trips * pop_size[i]
# and
#    n_nights[i] = mu_nights * pop_size[i]

# Use these estimates to populate the table, and then compute
# rates for m_d and m_r as we did for FR, DE, IT

# Albert's: add Iceland to to df_dat as a source country to these locations: DE, FR, IT, and CN
# and approximate its trips and nights to these locations using Iceland's population sizes 
df_dat = rbind(df_dat,c("A","CN","TOTAL","N_GE1","NR","IS",NA,NA,TRUE,TRUE)) # IS -> CN
df_dat = rbind(df_dat,c("A","DE","TOTAL","N_GE1","NR","IS",NA,NA,TRUE,TRUE)) # IS -> DE
df_dat = rbind(df_dat,c("A","FR","TOTAL","N_GE1","NR","IS",NA,NA,TRUE,TRUE)) # IS -> FR
df_dat = rbind(df_dat,c("A","IT","TOTAL","N_GE1","NR","IS",NA,NA,TRUE,TRUE)) # IS -> IT

#Adding these new rows with NA to df_dat makes nights and trips changed to string
df_dat$trips = as.numeric(df_dat$trips) 
df_dat$nights = as.numeric(df_dat$nights) 

df_dat$source_size <- NA # add source population size to df_dat
for (i in 1:length(other_eu_pop_sizes)){
  df_dat[df_dat$geo==other_eu_countries[i],]$source_size = other_eu_pop_sizes[i] # assign pop size from other_eu_countries
}
#
for (i in 2:(length(pop_sizes)-1)){
  df_dat[df_dat$geo==names(pop_sizes)[i],]$source_size = pop_sizes[i] # assign pop size from DE, IT, and FR
}

# Albert's: Approximate missing trips/nights from some source countries, using those countries' population sizes
# time the mean number of trips/nights per person across all the other source countries to a specific destination that have complete data 

df_dat_missing = df_dat[is.na(df_dat$trips),] #get all the incomplete rows with missing trips and nights 

for (i in 1:nrow(df_dat_missing)){
  dest_country  = df_dat_missing[i,]$c_dest #find the destination country for that source country
  tot_trip_dest = sum(as.numeric(na.omit(df_dat[df_dat$c_dest==dest_country,]$trips))) #compute total trips to that destination country from all the other countries (include other_eu_countries)
  tot_nights_dest = sum(as.numeric(na.omit(df_dat[df_dat$c_dest==dest_country,]$nights))) #compute total nights spent in that destination country from all the other countries (include other_eu_countries)
  #
  tot_pop_size = sum(df_dat[df_dat$c_dest==dest_country & !is.na(df_dat$trips),]$source_size) #compute population size from those source countries
  #
  per_capita_mean_trip = tot_trip_dest/tot_pop_size #get per capita mean number of trips across those countries to the destination
  per_capita_mean_nights = tot_nights_dest/tot_pop_size #get per capita mean nigths spent across those countries to the destination
  #
  df_dat_missing[i,]$trips = per_capita_mean_trip*df_dat_missing[i,]$source_size #approx num_trips for that source country to the destination
  df_dat_missing[i,]$nights = per_capita_mean_nights*df_dat_missing[i,]$source_size #approx num_nights spent in the destination from the source country
}
# Import trips and nights data from df_dat_missing to df_dat
for (i in 1:nrow(df_dat)){
  if (is.na(df_dat[i,]$trips)){
    source_loc = df_dat[i,]$geo
    dest_loc   = df_dat[i,]$c_dest
    df_dat[i,]$trips = df_dat_missing[df_dat_missing$geo==source_loc & df_dat_missing$c_dest==dest_loc,]$trips
    df_dat[i,]$nights = df_dat_missing[df_dat_missing$geo==source_loc & df_dat_missing$c_dest==dest_loc,]$nights
  }
}

#filter to exclude travel from other_eu_countries to another other_eu_countries 
df_dat = df_dat[!(df_dat$geo %in% other_eu_countries & df_dat$c_dest %in% other_eu_countries), ]


# get list of travel from other_eu_countries to DE,IT,FR,and CN
# Note: IS is not on the list 
df_from_other_eu = df_dat[df_dat$geo %in% other_eu_countries,] 

# Approximate missing trips and nights data from China to some countries on the list using relative population size from country with similar population size 
# namely, trips to IE, trips and nights to NO, and trips and nights to UK 
# For IE that only missing trips data: we use trips data from country with similar nights data as nights data in IE
# This is to avoid trips < nights 

df_dat_china$trips[df_dat_china$info == "IE"] = df_dat_china$trips[df_dat_china$info == "DK"] * (other_eu_pop_sizes["IE"]/other_eu_pop_sizes["DK"])
df_dat_china$trips[df_dat_china$info == "NO"] = df_dat_china$trips[df_dat_china$info == "FI"] * (other_eu_pop_sizes["NO"]/other_eu_pop_sizes["FI"])
df_dat_china$nights[df_dat_china$info == "NO"] = df_dat_china$nights[df_dat_china$info == "FI"] * (other_eu_pop_sizes["NO"]/other_eu_pop_sizes["FI"])
df_dat_china$trips[df_dat_china$info == "UK"] = df_dat_china$trips[df_dat_china$info == "FR"] * (other_eu_pop_sizes["UK"]/pop_sizes["FR"])
df_dat_china$nights[df_dat_china$info == "UK"] = df_dat_china$nights[df_dat_china$info == "FR"] * (other_eu_pop_sizes["UK"]/pop_sizes["FR"])

# scale to Hubei using relative proportion of pop size of Hubei to China
df_dat_Hubei = data.frame(c_dest =df_dat_china$info,
                          geo = rep("CN",19),
                          trips = df_dat_china$trips*prop_Hubei_to_China,
                          nights = df_dat_china$nights*prop_Hubei_to_China)

# # Albert's: Approximate missing trips/nights from Hubei as source, using those countries' population sizes
# # time the mean number of trips/nights per person across all the other source countries to a specific destination that have complete data 
# 
# # NOTE: geo here is already Hubei, because we scale the per capitar rate using Hubei's population size, not China's
# df_dat_Hubei = data.frame(c_dest = c(other_eu_countries,"FR","DE","IT"),
#                           geo = rep("CN",19),trips = rep(NA,19),
#                           nights = rep(NA,19)) 
# 
# for (i in 1:nrow(df_dat_Hubei)){
#   dest_country  = df_dat_Hubei[i,]$c_dest #find the destination country from Hubei
#   tot_trip_dest = sum(df_dat[df_dat$c_dest == dest_country,]$trips) #compute total trips to that destination country from all the other countries (include other_eu_countries)
#   tot_nights_dest = sum(df_dat[df_dat$c_dest == dest_country,]$nights) #compute total nights spent in that destination country from all the other countries (include other_eu_countries)
#   #
#   tot_pop_size = sum(df_dat[df_dat$c_dest==dest_country,]$source_size) #compute population size from those source countries
#   #
#   per_capita_mean_trip = tot_trip_dest/tot_pop_size #get per capita mean number of trips across those countries to the destination
#   per_capita_mean_nights = tot_nights_dest/tot_pop_size #get per capita mean nigths spent across those countries to the destination
#   #
#   df_dat_Hubei[i,]$trips = per_capita_mean_trip*pop_sizes["CN"] #approx num_trips from Hubei to the destination
#   df_dat_Hubei[i,]$nights = per_capita_mean_nights*pop_sizes["CN"] #approx num_nights spent in the destination from Hubei
# }

# Make Visit/Depart matrices
m_d = matrix(0, nrow=5, ncol=5)
rownames(m_d) = colnames(m_d) = locations
# return matrix, row = origin country, column = destination country
# e.g. row = FR, column = IT, then it is a return rate from Italy to France 
m_r = m_d


# Populate Depart matrix, m_d

# Expected number of trips per person per day is:
# n_trips_from_to / (n_from * 365) 
# The depart rate is the inverse of this. (edited)  --- maybe not?
get_depart_rate = function(df_dat, c_from, c_to, n_from) {
  n_trips_from_to = df_dat$trips[ df_dat$geo == c_from & df_dat$c_dest == c_to ]
  x = n_trips_from_to / (n_from * 365)
  return(x)
}

## Among focal European nations: FR, DE, IT

# FR -> DE
m_d[ loc_idx["FR"], loc_idx["DE"] ] = get_depart_rate(df_dat, "FR", "DE", pop_sizes["FR"])
# FR -> IT
m_d[ loc_idx["FR"], loc_idx["IT"] ] = get_depart_rate(df_dat, "FR", "IT", pop_sizes["FR"])
# DE -> FR
m_d[ loc_idx["DE"], loc_idx["FR"] ] = get_depart_rate(df_dat, "DE", "FR", pop_sizes["DE"])
# DE -> IT
m_d[ loc_idx["DE"], loc_idx["IT"] ] = get_depart_rate(df_dat, "DE", "IT", pop_sizes["DE"])
# IT -> FR
m_d[ loc_idx["IT"], loc_idx["FR"] ] = get_depart_rate(df_dat, "IT", "FR", pop_sizes["IT"])
# IT -> DE
m_d[ loc_idx["IT"], loc_idx["DE"] ] = get_depart_rate(df_dat, "IT", "DE", pop_sizes["IT"])

## Other EU countries to focal European countries 

# OtherEU -> DE
m_d[ loc_idx["OtherEU"], loc_idx["DE"] ] = sum(df_from_other_eu[df_from_other_eu$c_dest=="DE",]$trips)/(sum(df_from_other_eu[df_from_other_eu$c_dest=="DE",]$source_size)*365)
# OtherEU -> IT
m_d[ loc_idx["OtherEU"], loc_idx["IT"] ] = sum(df_from_other_eu[df_from_other_eu$c_dest=="IT",]$trips)/(sum(df_from_other_eu[df_from_other_eu$c_dest=="IT",]$source_size)*365)
# OtherEU -> FR
m_d[ loc_idx["OtherEU"], loc_idx["FR"] ] = sum(df_from_other_eu[df_from_other_eu$c_dest=="FR",]$trips)/(sum(df_from_other_eu[df_from_other_eu$c_dest=="FR",]$source_size)*365)

## Focal European countries (FR,DE,IT) to any OtherEU
# NOTE: It is a total rate of leaving from a focal country to ANY otherEU countries, so no need to divide by 16

# DE -> OtherEU
m_d[ loc_idx["DE"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$geo=="DE" & df_dat$c_dest %in% other_eu_countries,]$trips)/( pop_sizes["DE"]*365)
# IT -> OtherEU
m_d[ loc_idx["IT"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$geo=="IT" & df_dat$c_dest %in% other_eu_countries,]$trips)/( pop_sizes["IT"]*365)
# FR -> OtherEU
m_d[ loc_idx["FR"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$geo=="FR" & df_dat$c_dest %in% other_eu_countries,]$trips)/( pop_sizes["FR"]*365)


## Europe into Hubei

# FR -> Hubei
m_d[ loc_idx["FR"], loc_idx["CN"] ] = get_depart_rate(df_dat, "FR", "CN", pop_sizes["FR"])
# DE -> Hubei
m_d[ loc_idx["DE"], loc_idx["CN"] ] = get_depart_rate(df_dat, "DE", "CN", pop_sizes["DE"])
# IT -> Hubei (no information! use FR data and scale to IT pop size)
m_d[ loc_idx["IT"], loc_idx["CN"] ] = m_d[ loc_idx["FR"], loc_idx["CN"] ] * (pop_sizes["IT"]/pop_sizes["FR"])
# OtherEU -> Hubei
m_d[ loc_idx["OtherEU"], loc_idx["CN"] ] = sum(df_from_other_eu[df_from_other_eu$c_dest=="CN",]$trips)/(sum(df_from_other_eu[df_from_other_eu$c_dest=="CN",]$source_size)*365)

# thin all rates by proportion of Hubei versus total CN pop size
m_d[ , loc_idx["CN"] ] = m_d[ , loc_idx["CN"] ] * prop_Hubei_to_China

## Hubei into EU countries (focal + otherEU)

# Hubei -> FR
m_d[ loc_idx["CN"], loc_idx["FR"] ] = get_depart_rate(df_dat_Hubei,"CN","FR",pop_sizes["CN"])
# Hubei -> DE
m_d[ loc_idx["CN"], loc_idx["DE"] ] = get_depart_rate(df_dat_Hubei,"CN","DE",pop_sizes["CN"])
# Hubei -> IT
m_d[ loc_idx["CN"], loc_idx["IT"] ] = get_depart_rate(df_dat_Hubei,"CN","IT",pop_sizes["CN"])
# Hubei -> OtherEU
m_d[ loc_idx["CN"], loc_idx["OtherEU"] ] = sum(df_dat_Hubei[df_dat_Hubei$geo=="CN" & df_dat_Hubei$c_dest %in% other_eu_countries,]$trips)/( pop_sizes["CN"]*365)

# Populate Return matrix, m_r

# Expected duration per trip is:
# (n_nights_from_to / total_trips_abroad_year)
# then both numerator and denominator would be divided by pop_size and thus cancel. You then take the inverse of expected duration per trip for return rate. (edited) 
# This will be expect time of stay, take inverse to convert to rate

get_return_rate = function(df_dat, c_from, c_to, n_from) {
  n_nights_from_to = df_dat$nights[ df_dat$geo == c_from & df_dat$c_dest == c_to ]
  n_trips_from_to = df_dat$trips[ df_dat$geo == c_from & df_dat$c_dest == c_to ]
  x = 1 / (n_nights_from_to / n_trips_from_to)
  return(x)
}

# FR <- DE
m_r[ loc_idx["FR"], loc_idx["DE"] ] = get_return_rate(df_dat, "FR", "DE", pop_sizes["FR"] )
# FR <- IT
m_r[ loc_idx["FR"], loc_idx["IT"] ] = get_return_rate(df_dat, "FR", "IT", pop_sizes["FR"])
# DE <- FR
m_r[ loc_idx["DE"], loc_idx["FR"] ] = get_return_rate(df_dat, "DE", "FR", pop_sizes["DE"])
# DE <- IT
m_r[ loc_idx["DE"], loc_idx["IT"] ] = get_return_rate(df_dat, "DE", "IT", pop_sizes["DE"])
# IT <- FR
m_r[ loc_idx["IT"], loc_idx["FR"] ] = get_return_rate(df_dat, "IT", "FR", pop_sizes["IT"])
# IT <- DE
m_r[ loc_idx["IT"], loc_idx["DE"] ] = get_return_rate(df_dat, "IT", "DE", pop_sizes["IT"])

# OtherEU <- DE 
m_r[ loc_idx["OtherEU"], loc_idx["DE"] ] = sum(df_dat[df_dat$c_dest == "DE" & df_dat$geo %in% other_eu_countries,]$nights)/sum(df_dat[df_dat$c_dest == "DE" & df_dat$geo %in% other_eu_countries,]$trips)
m_r[ loc_idx["OtherEU"], loc_idx["DE"] ] = 1/m_r[ loc_idx["OtherEU"], loc_idx["DE"] ]
# OtherEU <- FR 
m_r[ loc_idx["OtherEU"], loc_idx["FR"] ] = sum(df_dat[df_dat$c_dest == "FR" & df_dat$geo %in% other_eu_countries,]$nights)/sum(df_dat[df_dat$c_dest == "FR" & df_dat$geo %in% other_eu_countries,]$trips)
m_r[ loc_idx["OtherEU"], loc_idx["FR"] ] = 1/m_r[ loc_idx["OtherEU"], loc_idx["FR"] ]
# OtherEU <- IT
m_r[ loc_idx["OtherEU"], loc_idx["IT"] ] = sum(df_dat[df_dat$c_dest == "IT" & df_dat$geo %in% other_eu_countries,]$nights)/sum(df_dat[df_dat$c_dest == "IT" & df_dat$geo %in% other_eu_countries,]$trips)
m_r[ loc_idx["OtherEU"], loc_idx["IT"] ] = 1/m_r[ loc_idx["OtherEU"], loc_idx["IT"] ]

# FR <- OtherEU 
m_r[ loc_idx["FR"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "FR",]$nights)/sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "FR",]$trips)
m_r[ loc_idx["FR"], loc_idx["OtherEU"] ] = 1/m_r[ loc_idx["FR"], loc_idx["OtherEU"] ]
# DE <- OtherEU 
m_r[ loc_idx["DE"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "DE",]$nights)/sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "DE",]$trips)
m_r[ loc_idx["DE"], loc_idx["OtherEU"] ] = 1/m_r[ loc_idx["DE"], loc_idx["OtherEU"] ]
# IT <- OtherEU 
m_r[ loc_idx["IT"], loc_idx["OtherEU"] ] = sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "IT",]$nights)/sum(df_dat[df_dat$c_dest %in% other_eu_countries & df_dat$geo == "IT",]$trips)
m_r[ loc_idx["IT"], loc_idx["OtherEU"] ] = 1/m_r[ loc_idx["IT"], loc_idx["OtherEU"] ]

# FR <- Hubei 
m_r[ loc_idx["FR"], loc_idx["CN"] ] = get_return_rate(df_dat, "FR", "CN", pop_sizes["FR"] )
# DE <- Hubei 
m_r[ loc_idx["DE"], loc_idx["CN"] ] = get_return_rate(df_dat, "DE", "CN", pop_sizes["DE"] )
# IT <- Hubei 
m_r[ loc_idx["IT"], loc_idx["CN"] ] = get_return_rate(df_dat, "IT", "CN", pop_sizes["IT"] )
# OtherEU <- Hubei 
m_r[ loc_idx["OtherEU"], loc_idx["CN"] ] = sum(df_dat[df_dat$c_dest == "CN" & df_dat$geo %in% other_eu_countries,]$nights)/sum(df_dat[df_dat$c_dest == "CN" & df_dat$geo %in% other_eu_countries,]$trips)
m_r[ loc_idx["OtherEU"], loc_idx["CN"] ] = 1/m_r[ loc_idx["OtherEU"], loc_idx["CN"] ] 

# # Note: for return rate to european countries from Hubei, no need to thin rates by prop_hubei_to_china,
# because it will cancel out when dividing number of nights spent in China with total trips to China.
# e.g. if people from DE spent 1000 nights in China for 10 trips to china, then the return time is every 10 days.
# For return trip form hubei, it would be 1000 * prop_hubei_to_china nights in hubei for 10 * prop_hubei_to_china trips to hubei.
# m_r[ , loc_idx["CN"] ] = m_r[ , loc_idx["CN"] ] * prop_Hubei_to_China


# Hubei <- FR
m_r[ loc_idx["CN"], loc_idx["FR"] ] = df_dat_Hubei$nights[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "FR" ]/df_dat_Hubei$trips[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "FR" ]
m_r[ loc_idx["CN"], loc_idx["FR"] ] = 1/m_r[ loc_idx["CN"], loc_idx["FR"] ]
# Hubei <- DE
m_r[ loc_idx["CN"], loc_idx["DE"] ] = df_dat_Hubei$nights[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "DE" ]/df_dat_Hubei$trips[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "DE" ]
m_r[ loc_idx["CN"], loc_idx["DE"] ] = 1/m_r[ loc_idx["CN"], loc_idx["DE"] ]
# Hubei <- IT
m_r[ loc_idx["CN"], loc_idx["IT"] ] = df_dat_Hubei$nights[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "IT" ]/df_dat_Hubei$trips[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest == "IT" ]
m_r[ loc_idx["CN"], loc_idx["IT"] ] = 1/m_r[ loc_idx["CN"], loc_idx["IT"] ]
# Hubei <- OtherEU
m_r[ loc_idx["CN"], loc_idx["OtherEU"] ] = sum(df_dat_Hubei$nights[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest %in% other_eu_countries ])/sum(df_dat_Hubei$trips[ df_dat_Hubei$geo == "CN" & df_dat_Hubei$c_dest %in% other_eu_countries ])
m_r[ loc_idx["CN"], loc_idx["OtherEU"] ] = 1/m_r[ loc_idx["CN"], loc_idx["OtherEU"] ]

# For return time to Hubei from European countries, since the nights data in these two dataset are very different 
# for the same pair, we will be estimating using the mean return time to european countries from Hubei instead.
# i.e. people from european spend the same amount of time travelling in Hubei as hubei people travelling to european countries 
m_r[1,2:5] = mean(m_r[2:5,1])

## Save dataset and rate matrices
write.table(df_dat, file = paste0(dat_fp,"/trips_nights_data_eu.csv"), sep = ",", row.names = FALSE, quote = FALSE)
write.table(df_dat_Hubei, file = paste0(dat_fp,"/trips_nights_data_hubei.csv"), sep = ",", row.names = FALSE, quote = FALSE)
write.table(m_d, file = paste0(dat_fp,"/depart_rates.csv"), sep = ",", row.names = TRUE, col.names=NA ,quote = FALSE)
write.table(m_r, file = paste0(dat_fp,"/return_rates.csv"), sep = ",", row.names = TRUE, col.names=NA ,quote = FALSE)

## need to discuss: assigning priors distributions for rates according to mean values in m_d and m_r
# I believe, we need a wider variance for the following case since it is either unreliable data or approximated 
# -.Depart rates from OtherEU countries to focal EU countries and Hubei
# -.Depart rates from focal EU countries to Hubei
# -.Return rates to OtherEU countries from focal EU countries and Hubei
# -.Return rates to focal EU countries from Hubei
# -.Depart rates from Hubei to all EU countries
# -.Return rates to Hubei from all EU countries

