########################################################################## 
## Sampling choice set for port-species model based on Hicks REE paper ##
##########################################################################

gc()
rm(list=ls())

## Load packages ##
library(doParallel)
library(tidyr)
library(plm)
library(tidyverse)
library(lubridate)


#-------------------------------------------------------------------------------
## Read participation database ##

participation_data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

#-------------------------------------------------------------------------------
## Filter participation database ##

# ## Keep trip with maximum revenue within a day
# ## (only 5% of the data have repeated trips per day)
# participation_data_filtered <- participation_data %>%
#   mutate(Vessel.length = as.numeric(Vessel.length),
#          Vessel.weight = as.numeric(Vessel.weight),
#          Vessel.horsepower = as.numeric(Vessel.horsepower)) %>%
#   mutate(max_days_sea = ifelse(is.na(max_days_sea), 1, max_days_sea)) %>%
#   mutate(Revenue = ifelse(selection == "No-Participation", 0, Revenue)) %>%
#   group_by(VESSEL_NUM, set_date) %>%
#   mutate(max_rev = ifelse(Revenue == max(Revenue, na.rm=TRUE), 1, 0)) %>% ungroup() %>%
#   dplyr::filter(max_rev == 1) %>%
#   distinct_at(vars(-trip_id)) %>%
#   group_by(VESSEL_NUM, set_date) %>%
#   mutate(ncount = n()) %>%
#   mutate(lat_mean = mean(lat, na.rm = TRUE),
#          lon_mean = mean(lon, na.rm = TRUE),
#          dist_mean = mean(dist, na.rm = TRUE)) %>%
#   mutate(lat = ifelse(ncount>1, lat_mean, lat)) %>%
#   mutate(lon = ifelse(ncount>1, lon_mean, lon)) %>%
#   mutate(dist = ifelse(ncount>1, dist_mean, dist)) %>%
#   dplyr::select(-c('lat_mean', 'lon_mean', 'dist_mean',
#                    'lat_logbook', 'lon_logbook',
#                    'lat_ca', 'lon_ca')) %>% ungroup() %>%
#   distinct() %>%
#   group_by(VESSEL_NUM, set_date) %>%
#   mutate(order = seq(1:n())) %>% ungroup() %>%
#   filter(order == 1) %>%
#   dplyr::select(-c('max_rev', 'order', 'ncount')) %>%
#   group_by(VESSEL_NUM, set_date)
# 
# participation_data_filtered$trip_id <-
#   udpipe::unique_identifier(participation_data_filtered, fields = c('VESSEL_NUM', 'set_date'))
#   saveRDS(participation_data_filtered, "C:\\Data\\PacFIN data\\participation_data_filtered.rds")

participation_data_filtered <- readRDS("C:\\Data\\PacFIN data\\participation_data_filtered.rds")
  
#-------------------------------------------------------------------------------
# ## Day at sea:
# hist(participation_data$max_days_sea)
### How many row have day at sea variable? Just 4.3%, so I should not use it as filter, just as information
# ticket_part <- participation_data %>% dplyr::filter(selection != "No-Participation")
# ticket_part%>% summarize(perc = (nrow(ticket_part)-sum(is.na(max_days_sea)))/nrow(ticket_part))

#-------------------------------------------------------------------------------
## Sampling choice data including expected revenue, expected cost and past behavior dummies ##
## Landing and price regression do not depend on cluster ##

# source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\sampled_rums_participation.R")
# samps1 <- sampled_rums(data_in = participation_data_filtered, cluster = 4,
#                          min_year = 2013, max_year = 2017,
#                          min_year_prob = 2013, max_year_prob = 2017,
#                          min_year_est = 2012, max_year_est = 2019,
#                          ndays = 30, nhauls_sampled = 5,
#                          seed = 300, ncores = 4, rev_scale = 1000)
#   samps <- samps1 %>%
#     mutate(PORT_AREA_CODE = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA))
#     rm(participation_data_filtered, samps1)
#     saveRDS(samps, file = "C:\\GitHub\\EconAnalysis\\Participation\\R\\sample_choice_set_c4_V3.rds")
#   test <- samps %>% ungroup() %>% group_by(fished_haul,selection) %>% summarize(n_count = n())
#   max(test$n_count)
#   rm(test)

#----------------------------------
## Run saved data
samps <- readRDS(file = "C:\\GitHub\\EconAnalysis\\Participation\\R\\sample_choice_set_c4.rds")


#--------------------------------------------------------------------------
# ### See how many times we have NA 

# 285 NAs for price (0.003%)
# 2,455 NAs for availability (0.025%)
# 20,361 Nas for dist to catch areas (20.5%)

samps0 <- samps %>%
  filter(selection != "No-Participation")

sum(is.na(samps0$mean_price))
sum(is.na(samps0$mean_avail))
sum(is.na(samps0$diesel_price))
sum(is.na(samps0$dist_port_to_catch_area))

sum(is.na(samps0$mean_price))/nrow(samps0)
sum(is.na(samps0$mean_avail))/nrow(samps0)
sum(is.na(samps0$diesel_price))/nrow(samps0)
sum(is.na(samps0$dist_port_to_catch_area))/nrow(samps0)

## Diesel price not a problem
## NA in distance means that it was not recorded

# ### All cases when we do not have price, we don't have availability either
# psych::describe(samps0 %>% dplyr::filter(is.na(samps0$mean_price)))


#---------------------------------------------------------------------------
### Create dummies for missing availability and price and convert NA to zero

samps1 <- samps %>% 
  mutate(dDieselState = ifelse((is.na(samps$diesel_price)), 0, dDieselState)) %>%
  mutate(dPrice30     = ifelse((is.na(samps$mean_price)), 0, dPrice30)) %>%
  mutate(dCPUE        = ifelse((is.na(samps$mean_avail)), 0, dCPUE)) %>%
  mutate(dCPUE90      = ifelse((is.na(samps$mean_avail)), 0, dCPUE90)) %>%
  mutate(d_missing    = ifelse((is.na(samps$mean_avail)), 1, 0)) %>%
  mutate(d_missing_p  = ifelse((is.na(samps$mean_price)), 1, 0)) %>%
  mutate(d_missing_d  = ifelse((is.na(samps$dist_port_to_catch_area)), 1, 0)) %>%
  mutate(mean_avail   = ifelse((is.na(samps$mean_avail)), 0, mean_avail)) %>% 
  mutate(mean_price   = ifelse((is.na(samps$mean_price)), 0, mean_price)) %>%
  mutate(dist_port_to_catch_area_zero  = ifelse((is.na(samps$dist_port_to_catch_area)), 0, dist_port_to_catch_area)) %>%
    dplyr::select(-c('dPrice30_s', 'dPrice90_s')) 

  
#----------------------------------------------------------------------
# ### See how many times we need to use CPUE (30 and 90 days), average 30 days prices and State diesel prices
# # 20.2% State diesel prices
# # 0.0001% Prices (30 days)
# # 6.865% CPUE index (30 days)
# # 0.005% CPUE index (90 days)

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


#--------------------------------------------------------------------------
### Replace values for No-Participation to zero 
samps <- samps1 %>% 
  mutate(dist_to_cog = ifelse(selection == "No-Participation", 0, dist_to_cog)) %>%
  mutate(lat_cg = ifelse(selection == "No-Participation", 0, lat_cg))
  rm(samps1, samps0)
  # samps %>% filter(selection == "No-Participation") %>% psych::describe()

  
#--------------------------------------------------------------------------
### Incorporate wind data 

## Read wind data
wind_2020_2020 <- readRDS("C:/Data/Wind&Current/wind_U_V_2000-2020.RDS") 
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
  select(c("PORT_AREA_CODE", "set_date", "wind_max_220_mh")) %>% 
  unique()

samps <- merge(samps, wind, by = (c('set_date', 'PORT_AREA_CODE')), all.x = TRUE, all.y = FALSE) %>%
#   mutate(wind_mean_220_mh = ifelse(selection == "No-Participation", 0, wind_mean_220_mh)) %>%
  mutate(wind_max_220_mh = ifelse(selection == "No-Participation", 0, wind_max_220_mh))
rm(wind, wind_2020_2020)
                
#-------------------------------------------------------------------
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

#-------------------------------------------------------------------
# Merge storm warning signals to check correlation between data
# 
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


#-------------------------------------------------------------------

## Dummy for squid and sardine
samps <- samps %>% mutate(dPSDN = ifelse(grepl("PSDN", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dMSQD = ifelse(grepl("MSQD", selection) == TRUE, 1, 0)) 


#------------------------------------------
## Incorporate closure dummy for Pacific sardine

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


#------------------------------------------------------------------
## Incorporate closure dummy for market squid and weekend indicator
samps <- samps %>%
  dplyr::mutate(MSQD.Closure = 
    ifelse(set_date >= "2010-12-17" & set_date < "2011-03-31", 1, 
    ifelse(set_date >= "2011-11-18" & set_date < "2012-03-31", 1, 
    ifelse(set_date >= "2012-11-21" & set_date < "2013-03-31", 1, 0)))) %>% 
    dplyr::mutate(Weekend = ifelse(chron::is.weekend(set_date), 1, 0)) %>% 
  dplyr::mutate(MSQD.Closure.d = MSQD.Closure * dMSQD) %>% 
  dplyr::mutate(MSQD.Weekend = Weekend * dMSQD)


#------------------------------------------
## WA dummy for sardine

# Include WA dummy for sardine!
samps <- samps %>% 
  dplyr::mutate(WA.Closure = ifelse(lubridate::month(set_date) >= 1 & lubridate::month(set_date) <= 3 &
            (PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS" |
             PORT_AREA_CODE == "SPS" | PORT_AREA_CODE == "WA5"), 1, 0)) %>%
  dplyr::mutate(WA.Closure = ifelse(selection == "No-Participation", 0, WA.Closure)) %>%
  dplyr::mutate(WA.Closure.d = WA.Closure * dPSDN)


#------------------------------------------
## Participation dummy 

samps <- samps %>% 
  dplyr::mutate(dParticipate = ifelse(selection == "No-Participation", 0, 1)) 



#-------------------------------------------
## Include unemployment.

unem_CA <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192133_ae51a3.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "C") %>%
  select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

unem_OR <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192159_36a94e.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "O")  %>%
  select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

unem_WA <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192208_d35a30.xlsx", range = "A11:I263") %>%
  mutate(AGENCY_CODE = "W")  %>%
  select(c('Year', 'Period', 'unemployment rate', 'AGENCY_CODE'))

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


#-------------------------#
## Format as mlogit.data ##
#-------------------------#

## Subset database
rdo <- samps %>% dplyr::select(fished, fished_haul, selection, fished_VESSEL_NUM, set_date, set_month, set_year,
                               mean_price, mean_avail, diesel_price, dCPUE, dPrice30, dDieselState, dCPUE90,     
                               wind_max_220_mh, dummy_last_day, dummy_prev_days, dummy_prev_year_days, dummy_clust_prev_days,
                               lat_cg, dist_to_cog, dist_port_to_catch_area, dist_port_to_catch_area_zero, 
                               PSDN.Closure, WA.Closure, MSQD.Closure, Weekend, PSDN.Closure.d, WA.Closure.d, MSQD.Closure.d, MSQD.Weekend, 
                               dParticipate, unem_rate, d_missing, d_missing_p, d_missing_d)
rm(samps)

#-----------------------------------------------------------------
## Drop choice occasions that received no choice (none of them...)

rdo2 <- rdo %>%
  group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>%
  filter(full!=0) %>%
  select(-c(full))
rm(rdo)

#--------------------------------------------------------
## Exclude NA winds (previous problem with ports...) NONE!

rdo_vessels_out <- rdo2 %>% filter(is.na(wind_max_220_mh)) %>% select(fished_haul) %>% unique()
`%ni%` <- Negate(`%in%`)
rdo3 <- data.table::setDT(rdo2)[fished_haul %ni% rdo_vessels_out$fished_haul]
rm(rdo2, rdo_vessels_out, `%ni%`)


#----------------------------------------------
# Check more than one hauls in same day (None!)

fished_haul_select <- rdo3 %>%
  group_by(fished_VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>%
  ungroup() %>% dplyr::filter(ncount == min(ncount)) %>%
  select('fished_haul') %>%
  unique()
rdo_R <- data.table::setDT(rdo3)[fished_haul %chin% fished_haul_select$fished_haul]
rm(fished_haul_select, rdo3)


#------------------------------------------------------
## Save data to use in R

saveRDS(rdo_R, file = "C:\\GitHub\\EconAnalysis\\Participation\\R\\rdo_R_c4.rds")


#----------------------------------------------------
## Stata data

# Organize data for Stata estimation (drop_na()???)
rdo_Stata <- as.data.frame(rdo_R[order(rdo_R$fished_VESSEL_NUM, rdo_R$fished_haul, -rdo_R$fished),]) %>%
  group_by(fished_VESSEL_NUM) %>%
  dplyr::mutate(fished_VESSEL_ID = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('fished_VESSEL_NUM')) %>%
  group_by(set_date) %>%
  dplyr::mutate(time = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('set_date'))  

## Save data to run with Stata
write.csv(rdo_Stata,"C:\\GitHub\\EconAnalysis\\Participation\\Stata\\rdo_Stata_c4.csv", row.names = FALSE)
saveRDS(rdo_Stata, file = "C:\\GitHub\\EconAnalysis\\Participation\\Stata\\rdo_Stata_c4.rds")



