#-----------------------------#
## Fit discrete choice model ##
#-----------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)
library(mlogit)

## Load data ##
rdo <- readRDS(file = "C:\\Data\\PacFIN data\\rdo_Stata_c4_full.rds") 

rdo2 <- rdo %>% mutate(mean_price = mean_price / 1000) %>%
  mutate(mean_price2 = mean_price2 / 1000) %>% 
  mutate(mean_catch = mean_catch / 1000) %>% 
  mutate(Price.Fishmeal.AFI = Price.Fishmeal.AFI / 1000) %>%
  mutate(exp_cost_cog = diesel_price * dist_to_cog) %>%
  mutate(exp_cost_catch_area = diesel_price * dist_port_to_catch_area_zero) %>%
  mutate(psdnclosured2 = PSDN.Closure.d - PSDN.Total.Closure.d) %>%
  mutate(psdnclosure2 = PSDN.Closure - PSDN.Total.Closure) %>%
  mutate(d_missing_p = ifelse(mean_price > 50, d_missing_p2, d_missing_p)) %>%
  mutate(mean_price = ifelse(mean_price > 50, mean_price2, mean_price)) %>%
  mutate(exp_revenue  = mean_price  * mean_avail) %>%
  mutate(d_c   = ifelse(d_missing_p == 0 & d_missing == 1 & d_missing_d == 0, 1, 0)) %>%
  mutate(d_d   = ifelse(d_missing_p == 0 & d_missing == 0 & d_missing_d == 1, 1, 0)) %>%
  mutate(d_p   = ifelse(d_missing_p == 1 & d_missing == 0 & d_missing_d == 0, 1, 0)) %>%
  mutate(d_cd  = ifelse(d_missing_p == 0 & d_missing == 1 & d_missing_d == 1, 1, 0)) %>%
  mutate(d_pc  = ifelse(d_missing_p == 1 & d_missing == 1 & d_missing_d == 0, 1, 0)) %>%
  mutate(d_pd  = ifelse(d_missing_p == 1 & d_missing == 0 & d_missing_d == 1, 1, 0)) %>%
  mutate(d_pcd = ifelse(d_missing_p == 1 & d_missing == 1 & d_missing_d == 1, 1, 0)) 


#--------------------------------------
rdo3 <- rdo2 %>% 
  group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>% ungroup() %>% 
  filter(full != 0) %>%
  mutate(fished = as.logical(fished)) %>%
  arrange(fished_VESSEL_NUM, fished_haul, selection)

table(rdo3$selection) 

# xx <- list(Part = list(LAA = c('LAA-BTNA', 'LAA-CMCK', 'LAA-MSQD', 'LAA-NANC', 'LAA-PSDN', 'LAA-YTNA'),
#                  MNA = c('MNA-MSQD', 'MNA-NANC', 'MNA-PSDN'),
#                  MRA = c('MRA-MSQD'), 
#                  SBA = c('SBA-MSQD', 'SBA-CMCK'),
#                  SFA = c('SFA-MSQD', 'SFA-NANC')), 
#      No_Part = list(NO_port = c('No-Participation')))

yy <- list(LAA = c('LAA-BTNA', 'LAA-CMCK', 'LAA-MSQD', 'LAA-NANC', 'LAA-PSDN', 'LAA-YTNA'),
           MNA = c('MNA-MSQD', 'MNA-NANC', 'MNA-PSDN'),
           MRA = c('MRA-MSQD'), 
           SBA = c('SBA-MSQD', 'SBA-CMCK'),
           SFA = c('SFA-MSQD', 'SFA-NANC'),
           NO_port = c('No-Participation'))

## Create mlogit data
the_tows <- mlogit.data(rdo3, shape = 'long', choice = 'fished',
             alt.var = "selection", id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")
vtable::st(the_tows)

## Estimate mlogit model
model <- mlogit(fished ~ mean_price + mean_avail + wind_max_220_mh + dist_to_cog + 
                  + d_d + d_cd + unem_rate | 1 + Weekend, 
                data = the_tows,
                reflevel = 'No-Participation')
summary(model)

model2 <- mlogit(fished ~ mean_price + mean_avail + wind_max_220_mh + dist_to_cog + 
                  + d_d + d_cd + unem_rate | 1 + Weekend, 
                data = the_tows,
                reflevel = 'No-Participation')
summary(model2)


