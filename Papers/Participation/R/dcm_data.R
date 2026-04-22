#------------------------------------------------------------------------#
## Sampling choice set for port-species model based on Hicks REE paper ##
#------------------------------------------------------------------------#

gc()
rm(list=ls())

## Load packages ##
library(doParallel)
library(tidyr)
library(plm)
library(tidyverse)
library(lubridate)

### Sampling choice data ####
# including expected revenue, expected cost and past behavior dummies #
source("D:\\GitHub\\EconAnalysis\\Functions\\participation_model\\sampled_rums_participation.R")
participation_data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

samps1 <- sampled_rums(data_in = participation_data, cluster = 3,
                         min_year = 2013, max_year = 2017,
                         min_year_prob = 2013, max_year_prob = 2017,
                         min_year_est = 2012, max_year_est = 2019,
                         ndays = 30, nhauls_sampled = 5,
                         seed = 300, ncores = 4, rev_scale = 1000,
                         sample_choices = FALSE, k_chosen = 0.5)
                         ## max_est_year for catch is 2014 (pre-closure)

  samps <- samps1 %>%
    mutate(PORT_AREA_CODE = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA))
    rm(samps1)
    saveRDS(samps, file = "C:\\Data\\PacFIN data\\sample_choice_set_c3_full.rds")

### Run saved data ####
samps <- readRDS(file = "C:\\Data\\PacFIN data\\sample_choice_set_c3_full.rds")


# Check if there is no similar alternatives within a trip 
test <- 
    samps %>% ungroup() %>% 
    group_by(fished_haul,selection) %>%
    summarize(n_count = n())
    max(test$n_count)
    rm(test)
    
#--------------------------------------------------------------------------#
## See how many times we have NA

# 4,168 NAs for availability (2.8%) -- (5.8% with full data -- C5: 4.3%)
# 10,909 NAs using catches (with SDM) (7.4%) -- (16.8% with full data -- C5: 20.2%)
# 36,588 NAs using catches (no SDM) (24.8%) -- (51% with full data -- C5: 57.7%)
# 29,330 Nas for dist to catch areas (19.9%) -- (36.4% with full data -- C5: 57.9%)
# 10,320 NAs for mean_price2 (7%) -- (11.5% with full data -- C5: 12.7%)

# samps0 <- samps %>%
#   filter(selection != "No-Participation")
# sum(is.na(samps0$mean_price))
# sum(is.na(samps0$mean_price2))
# sum(is.na(samps0$mean_avail))
# sum(is.na(samps0$mean_catch))
# sum(is.na(samps0$mean_catch2))
# sum(is.na(samps0$diesel_price))
# sum(is.na(samps0$dist_port_to_catch_area))
# sum(is.na(samps0$mean_price))/nrow(samps0)
# sum(is.na(samps0$mean_price2))/nrow(samps0)
# sum(is.na(samps0$mean_avail))/nrow(samps0)
# sum(is.na(samps0$mean_catch))/nrow(samps0)
# sum(is.na(samps0$mean_catch2))/nrow(samps0)
# sum(is.na(samps0$diesel_price))/nrow(samps0)
# sum(is.na(samps0$dist_port_to_catch_area))/nrow(samps0)
# 
# ## NA in distance means that it was not recorded

# ### All cases when we do not have availability, it was calculated using CPUE 
# ### (C4: Yellow and Bluefin tunas; C5: Albacore and crab)
# psych::describe(samps0 %>% dplyr::filter(is.na(samps0$mean_avail)))
# samps0 %>% dplyr::filter(is.na(samps0$mean_avail)) %>%
#   group_by(PACFIN_SPECIES_CODE) %>% summarize(total_obs = n()) %>%
#   mutate(perc = total_obs*100/nrow(samps0 %>%
#                                      dplyr::filter(
#                                        is.na(samps0$mean_avail))))


#---------------------------------------------------------------------------#
### Create dummies for missings ####
#   availability and price and convert NA to zero
samps1 <- samps %>% 
  mutate(dDieselState   = ifelse((is.na(samps$diesel_price)), 0, dDieselState)) %>%
  mutate(dPrice30       = ifelse((is.na(samps$mean_price)), 0, dPrice30)) %>%
  mutate(dPrice30_s     = ifelse((is.na(samps$dPrice30_s)), 0, dPrice30_s)) %>%
  mutate(dPrice90_s     = ifelse((is.na(samps$dPrice90_s)), 0, dPrice90_s)) %>%
  mutate(dPrice30_s2    = ifelse((is.na(samps$dPrice30_s2)), 0, dPrice30_s2)) %>%
  mutate(dPrice90_s2    = ifelse((is.na(samps$dPrice90_s2)), 0, dPrice90_s2)) %>%
  mutate(d_missing_cpue = ifelse((is.na(samps$mean_avail)) & dCPUE90 == 1, 1, 0)) %>%
  mutate(dCPUE        = ifelse((is.na(samps$mean_avail)), 0, dCPUE)) %>%
  mutate(dCPUE90      = ifelse((is.na(samps$mean_avail)), 0, dCPUE90)) %>%
  mutate(d_missing_catch    = ifelse((is.na(samps$mean_catch)), 1, 0)) %>%
  mutate(d_missing_catch2   = ifelse((is.na(samps$mean_catch2)), 1, 0)) %>%
  mutate(d_missing    = ifelse((is.na(samps$mean_avail)), 1, 0)) %>%
  mutate(d_missing_p  = ifelse((is.na(samps$mean_price)), 1, 0)) %>%
  mutate(d_missing_p2 = ifelse((is.na(samps$mean_price2)), 1, 0)) %>%
  mutate(d_missing_d  = ifelse((is.na(samps$dist_port_to_catch_area)), 1, 0)) %>%
  mutate(mean_avail   = ifelse((is.na(samps$mean_avail)), 0, mean_avail)) %>% 
  mutate(mean_catch   = ifelse((is.na(samps$mean_catch)), 0, mean_catch)) %>% 
  mutate(mean_catch2  = ifelse((is.na(samps$mean_catch2)), 0, mean_catch2)) %>% 
  mutate(mean_price   = ifelse((is.na(samps$mean_price)), 0, mean_price)) %>%
  mutate(mean_price2  = ifelse((is.na(samps$mean_price2)), 0, mean_price2)) %>%
  mutate(dist_port_to_catch_area_zero  = ifelse((is.na(samps$dist_port_to_catch_area)), 0, 
                                                dist_port_to_catch_area)) 

  
# ### See how many times we need to use CPUE (30 and 90 days), average 30 days prices and State diesel prices
# # 21.2% State diesel prices --- (24% -- C5: 16.9%)
# # 0% Prices (30 days) 
# # 14% Prices 2 (30 days using species) -- (30.9% - C5: 32.7%)
# # 10.8% Prices 2 (90 days using species) -- (20% - C5: 25%)
# # 8.6% CPUE index (30 days) -- 8.4% -- C5: 17.9%
# # 0.4% CPUE index (90 days) -- 2.2% -- C5: 3.1%

samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(diesel_price) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dDieselState) %>%
  summarize(n_obs = n(), perc = n()/mean(total))
    
samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(mean_price) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dPrice30) %>%
  summarize(n_obs = n(), perc = n()/mean(total))
    
samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(mean_price2) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dPrice30_s2) %>%
  summarize(n_obs = n(), perc = n()/mean(total))

samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(mean_price2) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dPrice90_s2) %>%
  summarize(n_obs = n(), perc = n()/mean(total))

samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(mean_avail) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dCPUE) %>%
  summarize(n_obs = n(), perc = n()/mean(total))

samps1 %>%
  filter(selection != "No-Participation")  %>%
  filter(is.na(mean_avail) == FALSE) %>%
  mutate(total = sum(n())) %>%
  group_by(dCPUE90) %>%
  summarize(n_obs = n(), perc = n()/mean(total))


### Replace values for No-Participation to zero ####
samps <- samps1 %>% 
  mutate(dist_to_cog = ifelse(selection == "No-Participation", 0, dist_to_cog)) %>%
  mutate(lat_cg = ifelse(selection == "No-Participation", 0, lat_cg))
  rm(samps1)
  samps %>% filter(selection == "No-Participation") %>% psych::describe()

  
### Incorporate multiday trip data ####
multiday_data <- participation_data %>% 
    dplyr::select(c(trip_id, multiday_trip, n_obs_within_FTID)) %>% unique() %>%
    rename(fished_haul = trip_id)
  samps <- merge(samps, multiday_data, by = "fished_haul", all.x = TRUE, all.y = FALSE) 

### Check if n_obs_within_FTID == 1
psych::describe(samps %>% group_by(fished_haul) %>% summarise(obs = sum(n_obs_within_FTID)/19))


### Include lunar data ####

samps <- samps %>% mutate(lunar.ill = lunar::lunar.illumination(as.Date(set_date), shift = 0))


### Include fish meal price as instrument ####
Deflactor <- read.csv(file = "C:\\Data\\PacFIN data\\deflactor.csv")
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
samps <- samps %>% mutate(LANDING_MONTH = month(set_date), LANDING_YEAR = year(set_date)) %>%
  merge(fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE) %>%
  merge(Deflactor, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
samps$Price.Fishmeal.AFI <- samps$Price.Fishmeal*samps$defl
rm(fish.meal)



### Incorporate wind data ####

## Read wind data
wind_2020_2020 <- readRDS("G:/My Drive/Data/Wind&Current/wind_U_V_2000-2020.RDS") 
colnames(wind_2020_2020)

## Calculate wind speed (magnitude in meter/second)
# wind_2020_2020$wind_mean_30  <- sqrt((wind_2020_2020$"windV_mean_30")^2  + (wind_2020_2020$"windU_mean_30")^2)
# wind_2020_2020$wind_mean_90  <- sqrt((wind_2020_2020$"windV_mean_90")^2  + (wind_2020_2020$"windU_mean_90")^2)
# wind_2020_2020$wind_mean_220 <- sqrt((wind_2020_2020$"windV_mean_220")^2 + (wind_2020_2020$"windU_mean_220")^2) 
wind_2020_2020$wind_max_30   <- sqrt((wind_2020_2020$"windV_max_30")^2  + (wind_2020_2020$"windU_max_30")^2)
wind_2020_2020$wind_max_90   <- sqrt((wind_2020_2020$"windV_max_90")^2  + (wind_2020_2020$"windU_max_90")^2)
wind_2020_2020$wind_max_220  <- sqrt((wind_2020_2020$"windV_max_220")^2 + (wind_2020_2020$"windU_max_220")^2) 

## Transform data from meters/second to miles/hour (multiply by 2.23694)  
wind_2020_2020 <- wind_2020_2020 %>%
  mutate(wind_max_30_mh   = wind_max_30   * 2.23694,
         wind_max_90_mh   = wind_max_90   * 2.23694,
         wind_max_220_mh  = wind_max_220  * 2.23694) %>%
  mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
    "SDA", "LAA", "SBA", "MRA", "MNA", 'SFA', "BDA", "BGA",
    "ERA", "CCA", "BRA", "CBA", "NPA", "TLA", "CLO", "CLW",
    "CWA", "SPS", "NPS")) 

wind <- wind_2020_2020 %>%
  mutate(set_date = as.Date(paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-"),
    "%Y-%m-%d")) %>%
  dplyr::select(c("PORT_AREA_CODE", "set_date", "wind_max_220_mh")) %>% 
  unique()

samps <- merge(samps, wind, by = (c('set_date', 'PORT_AREA_CODE')), all.x = TRUE, all.y = FALSE) %>%
#   mutate(wind_mean_220_mh = ifelse(selection == "No-Participation", 0, wind_mean_220_mh)) %>%
  mutate(wind_max_220_mh = ifelse(selection == "No-Participation", 0, wind_max_220_mh))
rm(wind, wind_2020_2020)
                

## Do a histogram with the wind speed by port (from Lisa's paper) ##
# 
# # Small craft -> 25–38 miles per hour
# # Gale warnings -> 39–57 miles per hour.
# # Tropical storm -> 39-74 miles per hour
# # Hurricanes -> > 74 miles per hour.
# 
# wind_2020_2020 <- wind_2020_2020 %>%
# mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
# mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
#                                     "SDA", "LAA", "SBA", "MRA", "MNA", 'SFA', "BDA", "BGA",
#                                     "ERA", "CCA", "BRA", "CBA", "NPA", "TLA", "CLO", "CLW",
#                                     "CWA", "SPS", "NPS"))
# 
# ports <- as_labeller(c("SDA"	= "San Diego", "LAA"	= "Los Angeles",
#                        "SBA"	= "Santa Barbara", "MRA"	= "Morro Bay",
#                        "MNA"	= "Monterey", "SFA"	= "San Francisco",
#                        "BDA"	= "Bodega Bay", "BGA"	= "Fort Bragg", 
#                        "ERA"	= "Eureka", "CCA"	= "Crescent City",
#                        "BRA"	= "Brookings", "CBA"	= "Coos Bay",
#                        "NPA"	= "Newport", "TLA"	= "Tillamook",
#                        "CLO"	= "Columbia River", "CLW"	= "Columbia River",
#                        "CWA"	= "Coastal\nWashington Area", 
#                        "SPS"	= "South Puget Sound", "NPS" = "North Puget Sound")) 
# 
# library(hrbrthemes)
# ggplot(data=wind_2020_2020, aes(x=wind_mean_220_mh)) +
#    geom_vline(aes(xintercept = 25, color = "Small craft"), size = 0.5) +
#    geom_vline(aes(xintercept = 39, color = "Gale"), size = 0.5) +
#    geom_density(adjust=1.5, 
#                 alpha=.4, 
#                 aes(y = ..count..), 
#                 fill = "#8da0cb") +
#   facet_wrap(~PORT_AREA_CODE, scales ="free_y", labeller = ports, ncol = 3) +
#   ylab("Frequency") + xlab("Average daily wind (miles/hour) within 220km radious") + 
#   scale_color_manual(name = "Warnings:", values = c("Small craft" = "#ff9224", "Gale" = "#D22B2B"))
# 
# 
# ggplot(data=wind_2020_2020, aes(x=wind_max_220_mh)) +
#   geom_vline(aes(xintercept = 25, color = "Small craft"), size = 0.5) +
#   geom_vline(aes(xintercept = 39, color = "Gale"), size = 0.5) +
#   geom_density(adjust=1.5, 
#                alpha=.4, 
#                aes(y = ..count..), 
#                fill = "#8da0cb") +
#   facet_wrap(~PORT_AREA_CODE, scales ="free_y", labeller = ports, ncol = 3) +
#   ylab("Frequency") + xlab("Maximum daily wind (miles/hour) within 220km radious") + 
#   scale_color_manual(name = "Warnings:", values = c("Small craft" = "#ff9224", "Gale" = "#D22B2B"))


### Merge storm warning signals ####
### to check correlation between data
 
# warnings.signals <- read.csv(file = "G://My Drive//Data//Storm Warnings//WCports_mww_events09-16.csv")
# warnings.signals <- warnings.signals %>%
#   select("pcid", "d_issued", "d_expired", "hurricane", "gale", "smcraft", "mww_other") %>%
#   dplyr::rename(PACFIN_PORT_CODE = pcid)
# port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
# warnings.signals <- warnings.signals %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
# warnings.signals$d_issued  <- as.Date(warnings.signals$d_issued, "%d%b%Y %H:%M:%OS")
# warnings.signals$d_expired <- as.Date(warnings.signals$d_expired, "%d%b%Y %H:%M:%OS")
# warnings.signals <- warnings.signals %>% dplyr::select(-c(PACFIN_PORT_CODE)) %>% 
#   unique() 
# warnings.signals$ID <- seq(1, nrow(warnings.signals))
# 
# library(sqldf)
# df1 <- wind_2020_2020
# df2 <- warnings.signals
# df1$date<-as.Date(with(df1,paste(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,sep="-")),"%Y-%m-%d")
# warnings.signals <-  sqldf("select df1.*, df2.hurricane, df2.gale, df2.smcraft, df2.mww_other, df2.ID
#                                       from df1 left join df2 on
#                                       (df1.PORT_AREA_CODE = df2.PORT_AREA_CODE) AND
#                                       (df1.date between df2.d_issued and df2.d_expired)")
# warnings.signals <- warnings.signals %>% unique() %>%
#   select("date", "PORT_AREA_CODE", 
#          "hurricane", "gale", "smcraft", "mww_other", 
#          "wind_mean_220_mh", "wind_max_220_mh", "ID") %>% 
#   unique() %>% drop_na() %>% 
#   group_by(ID, hurricane, gale, smcraft, mww_other) %>%
#   summarize(wind_max_220_mh = max(wind_max_220_mh))
#   
# warnings.small_craft <- warnings.signals %>%
#   filter(smcraft == 1)
# warnings.gale <- warnings.signals %>%
#   filter(gale == 1)
# ggplot() +
#   geom_density(data = warnings.small_craft,
#                adjust=1.5,
#                alpha=.2,
#                aes(y = ..count..,
#                    x = wind_max_220_mh, fill = "During small craft warning")) +
#   geom_density(data = warnings.gale,
#                adjust=1.5,
#                alpha=.2,
#                aes(y = ..count..,
#                    x = wind_max_220_mh, fill = "During gale warning days")) + ylab("Frequency") + 
#   xlab("Maximum daily wind (miles/hour) during the warning within 220km radius") +
#   scale_fill_manual(name = "Period:", 
#                     values = c("During small craft warning" = "#8da0cb", 
#                                "During gale warning days" = "#66c2a5")) + 
#   geom_vline(aes(xintercept = 25, color = "Small craft"), size = 0.75, linetype = "dashed") +
#   geom_vline(aes(xintercept = 39, color = "Gale"), size = 0.75, linetype = "dashed") +
#   scale_color_manual(name = "Threshold:", 
#                      values = c("Small craft" = "#ff9224", "Gale" = "#D22B2B"))


### Dummy for species ####
samps <- samps %>% mutate(dPSDN = ifelse(grepl("PSDN", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dMSQD = ifelse(grepl("MSQD", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dBTNA = ifelse(grepl("BTNA", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dDCRB = ifelse(grepl("DCRB", selection) == TRUE, 1, 0)) 



### Incorporate closure dummy ####
#### for PSDN ####

samps <- samps %>%
  dplyr::mutate(PSDN.Closure = 
    ifelse(set_date >= "2008-05-29" & set_date < "2008-07-01", 1, 
    ifelse(set_date >= "2008-08-08" & set_date < "2008-09-01", 1, 
    ifelse(set_date >= "2008-09-23" & set_date < "2009-01-01", 1, 
    ifelse(set_date >= "2009-02-20" & set_date < "2009-07-01", 1, 
    ifelse(set_date >= "2009-07-18" & set_date < "2009-09-01", 1, 
    ifelse(set_date >= "2009-09-23" & set_date < "2010-01-01", 1,  
    ifelse(set_date >= "2010-06-12" & set_date < "2010-07-01", 1, 
    ifelse(set_date >= "2010-07-22" & set_date < "2010-09-01", 1, 
    ifelse(set_date >= "2010-09-24" & set_date < "2011-01-01", 1, 
    ifelse(set_date >= "2011-03-05" & set_date < "2011-07-01", 1, 
    ifelse(set_date >= "2011-07-12" & set_date < "2011-09-01", 1, 
    ifelse(set_date >= "2011-09-21" & set_date < "2012-01-01", 1, 
    ifelse(set_date >= "2012-08-23" & set_date < "2012-09-01", 1, 
    ifelse(set_date >= "2013-08-22" & set_date < "2013-09-01", 1, 
    ifelse(set_date >= "2015-04-28", 1, 0)))))))))))))))) %>% 
    dplyr::mutate(PSDN.Closure.d = PSDN.Closure * dPSDN)

samps <- samps %>%
  dplyr::mutate(PSDN.Total.Closure = 
    ifelse(set_date >= "2015-04-28", 1, 0)) %>% 
  dplyr::mutate(PSDN.Total.Closure.d = PSDN.Total.Closure * dPSDN)



#### for MSQD + Weekend ####
samps <- samps %>%
  dplyr::mutate(MSQD.Closure = 
    ifelse(set_date >= "2010-12-17" & set_date < "2011-03-31", 1, 
    ifelse(set_date >= "2011-11-18" & set_date < "2012-03-31", 1, 
    ifelse(set_date >= "2012-11-21" & set_date < "2013-03-31", 1, 0)))) %>% 
    dplyr::mutate(Weekend = ifelse(chron::is.weekend(set_date), 1, 0)) %>% 
  dplyr::mutate(MSQD.Closure.d = MSQD.Closure * dMSQD) %>% 
  dplyr::mutate(MSQD.Weekend = Weekend * dMSQD)


#### for BTNA ####
samps <- samps %>%
  dplyr::mutate(BTNA.Closure = 
                  ifelse(set_date >= "2014-09-05" & set_date < "2014-11-12", 1, 
                  ifelse(set_date >= "2017-08-28" & set_date < "2017-12-31", 1, 0))) %>% 
  dplyr::mutate(BTNA.Closure.d = BTNA.Closure * dBTNA)



#### for DCRB ####

samps <- samps %>%
  dplyr::mutate(DCRB.Closure.WA = 
    ifelse(set_date >= "2009-09-15" & set_date < "2009-12-01" & PORT_AREA_CODE == "CLW", 1, 
    ifelse(set_date >= "2010-09-15" & set_date < "2010-12-01" & PORT_AREA_CODE == "CLW", 1, 
    ifelse(set_date >= "2011-09-15" & set_date < "2011-12-15" & PORT_AREA_CODE == "CLW", 1, 
    ifelse(set_date >= "2012-09-15" & set_date < "2012-12-31" & PORT_AREA_CODE == "CLW", 1, 
    ifelse(set_date >= "2013-09-15" & set_date < "2013-12-16" & PORT_AREA_CODE == "CLW", 1, 
    ifelse(set_date >= "2014-09-15" & set_date < "2014-12-01" & PORT_AREA_CODE == "CLW", 1,
    ifelse(set_date >= "2015-09-15" & set_date < "2016-01-04" & PORT_AREA_CODE == "CLW", 1,
    ifelse(set_date >= "2016-09-15" & set_date < "2017-01-01" & PORT_AREA_CODE == "CLW", 1,
    ifelse(set_date >= "2017-09-15" & set_date < "2018-01-15" & PORT_AREA_CODE == "CLW", 1,
    ifelse(set_date >= "2018-09-15" & set_date < "2019-01-04" & PORT_AREA_CODE == "CLW", 1,
    #
    ifelse(set_date >= "2009-09-15" & set_date < "2010-01-02" & PORT_AREA_CODE == "CWA", 1, 
    ifelse(set_date >= "2010-09-15" & set_date < "2011-01-15" & PORT_AREA_CODE == "CWA", 1, 
    ifelse(set_date >= "2011-09-15" & set_date < "2012-01-24" & PORT_AREA_CODE == "CWA", 1, 
    ifelse(set_date >= "2012-09-15" & set_date < "2013-01-24" & PORT_AREA_CODE == "CWA", 1, 
    ifelse(set_date >= "2013-09-15" & set_date < "2014-01-15" & PORT_AREA_CODE == "CWA", 1, 
    ifelse(set_date >= "2014-09-15" & set_date < "2015-01-03" & PORT_AREA_CODE == "CWA", 1,
    ifelse(set_date >= "2015-09-15" & set_date < "2016-01-04" & PORT_AREA_CODE == "CWA", 1,
    ifelse(set_date >= "2016-09-15" & set_date < "2017-01-07" & PORT_AREA_CODE == "CWA", 1,
    ifelse(set_date >= "2017-09-15" & set_date < "2018-01-28" & PORT_AREA_CODE == "CWA", 1,
    ifelse(set_date >= "2018-09-15" & set_date < "2019-01-10" & PORT_AREA_CODE == "CWA", 1,
           0))))))))))))))))))))) %>% 
    dplyr::mutate(DCRB.Closure.WA.d = DCRB.Closure.WA * dDCRB) %>%
    mutate(DCRB.Closure.WA.d = ifelse(selection == "No-Participation", 0, DCRB.Closure.WA.d)) %>%
    mutate(DCRB.Closure.WA = ifelse(selection == "No-Participation", 0, DCRB.Closure.WA))

samps <- samps %>%
  dplyr::mutate(DCRB.Closure.OR = 
    ifelse(set_date >= "2007-08-14" & set_date < "2007-12-01" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2008-08-14" & set_date < "2008-12-01" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2009-08-14" & set_date < "2009-12-01" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2010-08-14" & set_date < "2010-12-12" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2011-08-14" & set_date < "2011-12-15" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2012-08-14" & set_date < "2012-12-31" & AGENCY_CODE == "O", 1, 
    ifelse(set_date >= "2013-08-14" & set_date < "2013-12-16" & AGENCY_CODE == "O", 1,
    ifelse(set_date >= "2014-08-14" & set_date < "2014-12-01" & AGENCY_CODE == "O", 1,
    ifelse(set_date >= "2015-08-14" & set_date < "2016-01-04" & AGENCY_CODE == "O", 1,
    ifelse(set_date >= "2016-08-14" & set_date < "2016-12-18" & AGENCY_CODE == "O", 1,
    ifelse(set_date >= "2017-08-14" & set_date < "2018-01-15" & AGENCY_CODE == "O", 1, 0)))))))))))) %>% 
    dplyr::mutate(DCRB.Closure.OR.d = DCRB.Closure.OR * dDCRB) %>%
    mutate(DCRB.Closure.OR.d = ifelse(selection == "No-Participation", 0, DCRB.Closure.OR.d))

colnames(samps)


#### WA dummy for sardine ####

# Include WA dummy for sardine!
samps <- samps %>% 
  dplyr::mutate(WA.Closure = ifelse(lubridate::month(set_date) >= 1 & lubridate::month(set_date) <= 3 &
            (PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS" |
             PORT_AREA_CODE == "SPS" | PORT_AREA_CODE == "WA5"), 1, 0)) %>%
  dplyr::mutate(WA.Closure = ifelse(selection == "No-Participation", 0, WA.Closure)) %>%
  dplyr::mutate(WA.Closure.d = WA.Closure * dPSDN)


### Participation dummy  ####

samps <- samps %>% 
  dplyr::mutate(dParticipate = ifelse(selection == "No-Participation", 0, 1)) 



### Include unemployment ####

unem_CA <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192133_ae51a3.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "C") %>%
  dplyr::select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

unem_OR <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192159_36a94e.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "O")  %>%
  dplyr::select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

unem_WA <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192208_d35a30.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "W")  %>%
  dplyr::select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

unem <- rbind(unem_CA, unem_OR, unem_WA) %>%
  rename(set_year = Year) %>%
  rename(set_month = Period) %>%
  rename(unem_rate = `unemployment rate`) %>%
  mutate(set_month = ifelse(set_month == "Jan", 1, ifelse(set_month == "Feb", 2, ifelse(set_month == "Mar", 3, 
                     ifelse(set_month == "Apr", 4, ifelse(set_month == "May", 5, ifelse(set_month == "Jun", 6, 
                     ifelse(set_month == "Jul", 7, ifelse(set_month == "Aug", 8, ifelse(set_month == "Sep", 9, 
                     ifelse(set_month == "Oct", 10, ifelse(set_month == "Nov", 11, ifelse(set_month == "Dec", 12, set_month))))))))))))) 
rm(unem_CA, unem_OR, unem_WA)
samps <- samps %>% 
  mutate(set_month = month(set_date), set_year = year(set_date))%>%
  merge(unem, by = c('set_month', 'set_year', 'AGENCY_CODE'), all.x = TRUE, all.y = FALSE) %>%
  mutate(unem_rate = ifelse(selection == "No-Participation", 0, unem_rate))
  rm(unem)

  
#-----------------------------#
# Include vessel lenght
  
lenght.vessel <- participation_data %>% 
  dplyr::select(c(Vessel.length, VESSEL_NUM)) %>% 
  rename(fished_VESSEL_NUM = VESSEL_NUM) %>%
  unique() %>% drop_na()

samps2 <- merge(samps, lenght.vessel, by = "fished_VESSEL_NUM", all.x = TRUE, all.y = FALSE)
  

#-------------------------#
## Format as mlogit.data ##
#-------------------------#

## Subset database
rdo <- samps2 %>% dplyr::select(fished, fished_haul, selection, fished_VESSEL_NUM, set_date, set_month, set_year,
                               mean_price, mean_price2, mean_catch, mean_catch2, mean_avail, diesel_price, dCPUE, dPrice30, 
                               dDieselState, dCPUE90, wind_max_220_mh, dummy_last_day, dummy_prev_days_port,
                               dummy_prev_days, dummy_prev_year_days, dummy_clust_prev_days, lat_cg, dist_to_cog, 
                               dist_port_to_catch_area, dist_port_to_catch_area_zero, PSDN.Closure, BTNA.Closure, PSDN.Total.Closure, 
                               WA.Closure, MSQD.Closure, Weekend, BTNA.Closure.d, PSDN.Closure.d, PSDN.Total.Closure.d, WA.Closure.d, 
                               MSQD.Closure.d, MSQD.Weekend, DCRB.Closure.OR.d, DCRB.Closure.WA.d, DCRB.Closure.WA, dParticipate, unem_rate, d_missing_catch, d_missing_catch2, 
                               d_missing, d_missing_p, d_missing_p2, d_missing_cpue, d_missing_d, Vessel.length, 
                               lunar.ill, Price.Fishmeal.AFI, dPrice30_s, dPrice90_s,dPrice30_s2, dPrice90_s2)
                              rm(samps, samps2)
### Clean data ####
                              
#### Drop choice occasions ####
### that received no choice (none of them...)

rdo2 <- rdo %>%
  group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>% 
  ungroup() %>%
  filter(full!=0) %>%
  dplyr::select(-c(full)) 
rm(rdo)


#### Exclude NA winds ####
### (previous problem with ports...) NONE!

rdo_vessels_out <- rdo2 %>% 
  dplyr::filter(is.na(wind_max_220_mh)) %>%
  dplyr::select(fished_haul) %>% unique()
`%ni%` <- Negate(`%in%`)
rdo3 <- data.table::setDT(rdo2)[fished_haul %ni% rdo_vessels_out$fished_haul]
rm(rdo2, rdo_vessels_out, `%ni%`)


#### Check if more than one hauls in same day ####
### (None!)

fished_haul_select <- rdo3 %>%
  group_by(fished_VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>%
  ungroup() %>% dplyr::filter(ncount == min(ncount)) %>%
  dplyr::select('fished_haul') %>%
  unique()
rdo_R <- data.table::setDT(rdo3)[fished_haul %chin% fished_haul_select$fished_haul]
rm(fished_haul_select, rdo3)


### Save data ####

rdo_Stata <- as.data.frame(rdo_R[order(rdo_R$fished_VESSEL_NUM, rdo_R$fished_haul, -rdo_R$fished),]) %>%
  group_by(fished_VESSEL_NUM) %>%
  dplyr::mutate(fished_VESSEL_anon = cur_group_id()) %>%
  ungroup() %>% 
  #dplyr::select(-c('fished_VESSEL_NUM')) %>%
  group_by(set_date) %>%
  dplyr::mutate(time = cur_group_id()) %>%
  ungroup() %>%
  #%>% dplyr::select(-c('set_date')) %>%
  group_by(fished_haul) %>%
  dplyr::mutate(fished_haul_anon = cur_group_id()) %>%
  ungroup()

## Save data to run with Stata
write.csv(rdo_Stata,"C:\\Data\\PacFIN data\\rdo_Stata_c3_full.csv", row.names = FALSE)
saveRDS(rdo_Stata, file = "C:\\Data\\PacFIN data\\rdo_Stata_c3_full.rds")

#### Save data with no identifier ####

rdo_Stata_noid <- rdo_Stata %>% 
  dplyr::select(-c('fished_VESSEL_NUM')) %>%
  dplyr::select(-c('fished_haul'))

## Save data to run with Stata
write.csv(rdo_Stata_noid,"G:\\My Drive\\Data\\Anonymised data\\rdo_Stata_c3_full_noid.csv", row.names = FALSE)


# ## Compare mean_catch v/s mean_catch2
#
# rdo_Stata <- readRDS(file = "C:\\Data\\PacFIN data\\rdo_Stata_c5_full.rds")
# compare_catch_data <- rdo_Stata %>% 
#   mutate(Port = substr(selection, 1, 3)) %>%
#   mutate(Species = substr(selection, 5, 8)) %>% 
#   filter(mean_catch2 > 0) %>%
#   group_by(set_date, Species, Port) %>%
#             avg_catch2 = mean(mean_catch2)) %>%
#   filter(Species == "MSQD" | Species == "NANC" | Species == "PSDN") %>%
#   dplyr::select(c('avg_catch', 'avg_catch2', 'Species', 'Port')) 
# 
# Model <- lm(avg_catch ~ avg_catch2 + factor(Port) + factor(Species), data = compare_catch_data)
# summary(Model)