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
# library(mlogit)


#-----------------------------------------------------------------------------
## Read participation database ##
participation_data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")


#-----------------------------------------------------------------------------
# ## Days at sea
# ### Should we filter by number of days in the sea???? ###
#
# day_in_sea <- participation_data %>% 
#   select(trip_id, max_days_sea) %>% 
#   unique() %>%  
#   group_by(max_days_sea) %>%
#   summarize(n_obs = n()) %>% drop_na() 
# # %>%
# #   filter(max_days_sea <= 8)
# ggplot(day_in_sea, aes(x=as.factor(max_days_sea), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Number of tickets") + xlab("Number of days in the sea") + 
#   theme(plot.title = element_text(face = "bold", size = 12),
#         axis.ticks = element_line(colour = "grey70", size = 0.2),
#         panel.grid.minor = element_blank())

#-----------------------------------------------------------------------------
## Sampling choice data including expected revenue and past behavior dummies ##
### Note: Increase ndays to 60? Then reduce dummy_miss (30% right now) ##

# source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\sampled_rums_participation.R")
# samps <- sampled_rums(data_in = participation_data, cluster = 5,
#                          min_year = 2004, max_year = 2018,
#                          min_year_prob = 2006, max_year_prob = 2016,
#                          ndays = 30, nhauls_sampled = 5,
#                          seed = 42, ncores = 4, rev_scale = 1000)
# saveRDS(samps, file = "C:\\GitHub\\EconAnalysis\\Participation\\sample_choice_set_c5.rds")


## Restore the object
samps <- readRDS(file = "C:\\GitHub\\EconAnalysis\\Participation\\sample_choice_set_c4.rds")
samps <- samps %>% 
  mutate(PORT_AREA_CODE = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA))
samps %>% group_by(dummy_miss) %>% summarize(n_obs = n(), perc = n()/nrow(samps))
  

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


#------------------#
## Estimate model ##
#------------------#

#Format as mlogit.data
rdo <- samps %>% dplyr::select(fished, fished_haul,dummy_miss, mean_rev, mean_rev_adj, selection, 
                               fished_VESSEL_NUM, set_date, wind_max_220_mh, 
                               dummy_prev_days, dummy_prev_year_days)

# rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(fished_haul)) %>% as.data.frame

#-----------------------------------------------------------------------------
## Fit mlogit models returning the coefficients, the models, and the data going into the model

#Fit the model for everything at once
library(data.table)
library(mlogit)


## Drop choice cards that received no choice
# rdo2 <- rdo %>% 
#   group_by(fished_haul) %>% 
#   mutate(full = sum(fished)) %>% 
#   filter(full!=0) %>% 
#   select(-c(full))


# Check hauls in same day (only 30)
fished_haul_select <- rdo %>%
  group_by(fished_VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>% dplyr::filter(ncount == 5) %>% ungroup() %>%
  select('fished_haul') %>% unique()

rdo <- data.table::setDT(rdo)[fished_haul %chin% fished_haul_select$fished_haul]


rdo2 <- as.data.frame(rdo[order(rdo$fished_VESSEL_NUM, rdo$fished_haul, -rdo$fished),]) %>%
  group_by(fished_VESSEL_NUM) %>%
  dplyr::mutate(fished_VESSEL_ID = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('fished_VESSEL_NUM')) %>%
  group_by(set_date) %>%
  dplyr::mutate(time = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('set_date'))


write.csv(rdo2,"C:\\GitHub\\EconAnalysis\\Data\\sampled_mixed_logit_data.csv", row.names = FALSE)


str(rdo2)
## Maybe create new fished_haul number within VESSEL_NUM?

the_tows <- mlogit.data(rdo2, shape = 'long', choice = 'fished', alt.var = 'selection', 
                        id.var = "fished_VESSEL_ID", chid.var = "fished_haul")

# drop.index
# mf <- mFormula(fished ~ dummy_miss | 1 | mean_rev_adj)
# res <- mlogit(mf, the_tows)
# summary(res)

res <- mlogit(fished ~ dummy_miss + mean_rev_adj,
              the_tows, reflevel = 'No-Participation', panel = TRUE, 
              rpar = c(mean_rev_adj = "n"),
              correlation = FALSE, R = 100, halton = NA)
summary(res)


###
# Error in if (abs(x - oldx) < ftol) { : 
#     missing value where TRUE/FALSE needed --- Export to Stata?

#--------------------------------------------------------------------
#Generate and format the predictions
fits <- fitted(res, outcome = FALSE)
mfits <- reshape2::melt(fits) %>% 
  dplyr::rename(fished_haul = Var1) %>%
  dplyr::rename(selection_pred = Var2)

## -- Correct prediction using max probability value -- ##
pred_tows <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
pred_tows <- merge(rdo, pred_tows, by = "fished_haul") %>% filter(fished == TRUE) %>% 
  mutate(correct = ifelse(selection_pred == selection, 1, 0))
correct_prediction <- sum(pred_tows$correct) / nrow(pred_tows) #score 1
correct_prediction


pred_tows2 <- pred_tows %>% dplyr::filter(selection != "No-Participation")
correct_prediction <- sum(pred_tows2$correct) / nrow(pred_tows2) #score 1
correct_prediction










