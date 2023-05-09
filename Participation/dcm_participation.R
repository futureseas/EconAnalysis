######################################################################################## 
## Sampling choice set for port-species model based on Peter's code for Hicks's paper ##
########################################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it to be used with PacFIN data.

gc()
rm(list=ls())

## Load packages ##
library(doParallel)
library(tidyr)
library(plm)
library(tidyverse)
library(lubridate)


#-----------------------------------------------------------------------------
## Read participation database ##
participation_data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds") %>%
  mutate(Vessel.length = as.numeric(Vessel.length),
         Vessel.weight = as.numeric(Vessel.weight),
         Vessel.horsepower = as.numeric(Vessel.horsepower))

#-----------------------------------------------------------------------------
## Day at sea: Should we filter by number of days in the sea?

# day_in_sea <- participation_data %>%
#   dplyr::filter(max_days_sea == 1)


#-----------------------------------------------------------------------------
## Sampling choice data including expected revenue and past behavior dummies ##
### Note: Increase ndays to 60? Then reduce dummy_miss (30% right now) ##

# source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\sampled_rums_participation.R")
# samps1 <- sampled_rums(data_in = participation_data, cluster = 4,
#                          min_year = 2013, max_year = 2017,
#                          min_year_prob = 2013, max_year_prob = 2017,
#                          min_year_est = 2012, max_year_est = 2019,
#                          ndays = 60, nhauls_sampled = 4,
#                          seed = 42, ncores = 4, rev_scale = 1000)
# saveRDS(samps1, file = "C:\\GitHub\\EconAnalysis\\Participation\\sample_choice_set_c4.rds")


## Restore the object
samps1 <- readRDS(file = "C:\\GitHub\\EconAnalysis\\Participation\\sample_choice_set_c4.rds")
samps <- samps1 %>% 
  mutate(PORT_AREA_CODE = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA)) %>%
  mutate(dummy_miss_both = ifelse(dummy_miss == 1 & dummy_miss_SDM == 1, 1, 0))

compare <- samps %>% 
  filter(dummy_miss == 0 & dummy_miss_SDM == 0) %>% 
  filter(selection != "No-Participation") %>% 
  select(c(mean_rev_adj, mean_rev_SDM_adj))

long <- reshape2::melt(compare)

ggplot(long, aes(value, fill = variable)) +
  geom_density(alpha=.5) +
  xlab("Revenue")

samps %>%
  filter(selection != "No-Participation") %>%
  group_by(dummy_miss_both) %>%
  summarize(n_obs = n(), perc = n()/nrow(samps))
samps %>%
  filter(selection != "No-Participation") %>%
  group_by(dummy_miss) %>%
  summarize(n_obs = n(), perc = n()/nrow(samps))
samps %>%
  filter(selection != "No-Participation") %>%
  group_by(dummy_miss_SDM) %>%
  summarize(n_obs = n(), perc = n()/nrow(samps))
  

#-------------------------------#
## Create additional variables ##
#-------------------------------#

#---------------------------------------------------------------
# Incorporate wind data 

## Read wind data
wind_2020_2020 <- readRDS("C:/Data/Wind&Current/wind_U_V_2000-2020.RDS") 
colnames(wind_2020_2020)

## Calculate wind speed (magnitude in meter/second)
wind_2020_2020$wind_mean_30  <- sqrt((wind_2020_2020$"windV_mean_30")^2  + (wind_2020_2020$"windU_mean_30")^2)
wind_2020_2020$wind_mean_90  <- sqrt((wind_2020_2020$"windV_mean_90")^2  + (wind_2020_2020$"windU_mean_90")^2)
wind_2020_2020$wind_mean_220 <- sqrt((wind_2020_2020$"windV_mean_220")^2 + (wind_2020_2020$"windU_mean_220")^2) 
wind_2020_2020$wind_max_30   <- sqrt((wind_2020_2020$"windV_max_30")^2  + (wind_2020_2020$"windU_max_30")^2)
wind_2020_2020$wind_max_90   <- sqrt((wind_2020_2020$"windV_max_90")^2  + (wind_2020_2020$"windU_max_90")^2)
wind_2020_2020$wind_max_220  <- sqrt((wind_2020_2020$"windV_max_220")^2 + (wind_2020_2020$"windU_max_220")^2) 

## Transform data from meters/second to miles/hour (multiply by 2.23694)  
wind_2020_2020 <- wind_2020_2020 %>%
  mutate(wind_mean_30_mh  = wind_mean_30  * 2.23694,
         wind_mean_90_mh  = wind_mean_90  * 2.23694,
         wind_mean_220_mh = wind_mean_220 * 2.23694,
         wind_max_30_mh   = wind_max_30   * 2.23694,
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
  select(c("PORT_AREA_CODE", "set_date", "wind_mean_220_mh", "wind_max_220_mh")) %>% 
  unique()


samps <- merge(samps, wind, by = (c('set_date', 'PORT_AREA_CODE')), all.x = TRUE, all.y = FALSE) %>%
  mutate(wind_mean_220_mh = ifelse(selection == "No-Participation", 0, wind_mean_220_mh)) %>%
  mutate(wind_max_220_mh = ifelse(selection == "No-Participation", 0, wind_max_220_mh))
                

# ## Do a histogram with the wind speed by port (from Lisa's paper) ##
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

# #---------------------------------------------------------------
# # Merge storm warning signals to check correlation between data
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

#------------------------------------------
## Dummy for each species
samps <- samps %>% mutate(dPSDN = ifelse(grepl("PSDN", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dMSQD = ifelse(grepl("MSQD", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dNANC = ifelse(grepl("NANC", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dCMCK = ifelse(grepl("CMCK", selection) == TRUE, 1, 0)) 
samps <- samps %>% mutate(dJMCK = ifelse(grepl("JMCK", selection) == TRUE, 1, 0)) 

#------------------------------------------
## Incorporate closure dummy for Pacific sardine

samps <- samps %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2008-05-29" & set_date < "2008-07-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2008-08-08" & set_date < "2008-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2008-09-23" & set_date < "2009-01-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2009-02-20" & set_date < "2009-07-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2009-07-18" & set_date < "2009-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2009-09-23" & set_date < "2010-01-01", 1, 0)) %>% 
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2010-06-12" & set_date < "2010-07-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2010-07-22" & set_date < "2010-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2010-09-24" & set_date < "2011-01-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2011-03-05" & set_date < "2011-07-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2011-07-12" & set_date < "2011-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2011-09-21" & set_date < "2012-01-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2012-08-23" & set_date < "2012-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2013-08-22" & set_date < "2013-09-01", 1, 0)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(set_date >= "2015-04-28", 1, 0)) %>% 
  dplyr::mutate(PSDN.Closure = PSDN.Closure * dPSDN)


#-----------------------------------------------
## Incorporate closure dummy for market squid


#------------------------------------------
## Weekend dummy


#------------------------------------------
## WA dummy for sardine

# Include WA dummy for sardine!
samps <- samps %>% 
  dplyr::mutate(WA.Closure = ifelse(set_month >= 1 & set_month <= 3 &
            (PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS" |
             PORT_AREA_CODE == "SPS" | PORT_AREA_CODE == "WA5"), 1, 0)) 


#------------------------------------------
## Participation dummy 

samps <- samps %>% 
  dplyr::mutate(dParticipate = ifelse(selection == "No-Participation", 0, 1)) 


#-------------------------#
## Format as mlogit.data ##
#-------------------------#

## Subset database

rdo <- samps %>% dplyr::select(fished, fished_haul,dummy_miss, mean_rev, mean_rev_adj, 
                               selection, fished_VESSEL_NUM, set_date, wind_max_220_mh, 
                               dummy_prev_days, dummy_prev_year_days, PSDN.Closure, dParticipate)

#-------------------------------------------
## Drop choice cards that received no choice

rdo2 <- rdo %>%
  group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>%
  filter(full!=0) %>%
  select(-c(full))


#----------------------------------------------------
## Check hauls in same day (filter trips in same day)

fished_haul_select <- rdo2 %>%
  group_by(fished_VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>%
  ungroup() %>% dplyr::filter(ncount == min(ncount)) %>%
  select('fished_haul') %>%
  unique()
rdo2 <- data.table::setDT(rdo2)[fished_haul %chin% fished_haul_select$fished_haul]


#----------------------------------------------------
## Exclude NA winds (previous problem with ports...)

rdo_vessels_out <- rdo2 %>% filter(is.na(wind_max_220_mh)) %>% select(fished_haul) %>% unique()
`%ni%` <- Negate(`%in%`)
rdo3 <- data.table::setDT(rdo2)[fished_haul %ni% rdo_vessels_out$fished_haul]


#----------------------------------------------------------
## Organize data for estimation
rdo4 <- as.data.frame(rdo3[order(rdo3$fished_VESSEL_NUM, rdo3$fished_haul, -rdo3$fished),]) %>%
  drop_na() %>%
  group_by(fished_VESSEL_NUM) %>%
  dplyr::mutate(fished_VESSEL_ID = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('fished_VESSEL_NUM')) %>%
  group_by(set_date) %>%
  dplyr::mutate(time = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('set_date')) 


#----------------------------------------------------------
## Save data to run with Stata

write.csv(rdo4,"C:\\GitHub\\EconAnalysis\\Data\\sampled_mixed_logit_data.csv", row.names = FALSE)




# #-----------------------------#
# ## Fit discrete choice model ##
# #-----------------------------#
# 
# ## Create mlogit.data 
# library(mlogit)
# the_tows <- mlogit.data(rdo4, shape = 'long', choice = 'fished', alt.var = 'selection', 
#                           id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")
# 
# ## Fit model

## mFormula(fished ~ Generic coeff (alt variable) | Individual variable | Alternative specific coefficient )

# f <- mFormula(fished ~ wind_max_220_mh + dummy_prev_days + dummy_prev_year_days + dummy_miss + mean_rev_adj + PSDN.Closure + dParticipate | 0 | 0)
# head(model.matrix(f, the_tows))

# res <- mlogit(f, the_tows, reflevel = 'No-Participation', rpar=c(dParticipate = "n"), 
#   R = 100, halton = NA, panel = TRUE)
# 
# 
# summary(res)
# 
# 
#               
# #---------------------------------------#
# ## Generate and format the predictions ##
# #---------------------------------------#
# 
# fits <- fitted(res2, outcome = FALSE)
# mfits <- reshape2::melt(fits) %>%
#   dplyr::rename(fished_haul = Var1) %>%
#   dplyr::rename(selection_pred = Var2)
# 
# ## Compare to correct prediction using max probability value
# pred_tows <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
# pred_tows <- merge(rdo4, pred_tows, by = "fished_haul") %>% 
#   filter(fished == TRUE) %>%
#   mutate(correct = ifelse(selection_pred == selection, 1, 0))
#   sum(pred_tows$correct) / nrow(pred_tows) # 60% accuracy!
# 
# 
# pred_tows2 <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
# pred_tows2 <- merge(rdo4, pred_tows2, by = "fished_haul") %>% 
#   filter(fished == TRUE) %>%
#   mutate(correct = ifelse(selection_pred == selection, 1, 0)) %>%
#   filter(selection != "No-Participation")
#   sum(pred_tows2$correct) / nrow(pred_tows2) ## 52% accuracy!

