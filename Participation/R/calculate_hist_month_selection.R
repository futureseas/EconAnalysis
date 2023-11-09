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

dat <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

#--------------------------------
## Calculate historical month selection (2005-2020)
dist_hauls_catch_shares <- dat %>% 
  dplyr::filter(set_year >= 2005, 
                set_year <= 2020, 
                group_all %in% 4) %>% 
  distinct(trip_id, .keep_all = T)  %>% 
  dplyr::select(trip_id, VESSEL_NUM, set_year, set_month, set_day, Revenue, selection) %>% 
  as.data.frame

## Select hauls used to calculate probability for the choice set
dist_hauls_catch_shares <- dist_hauls_catch_shares[dist_hauls_catch_shares$selection != "No-Participation", ]


#----------------------------------------------------------------------------
## Create probabilities for sampling choice set
## For this, we compute the average vessel catch and port composition by month
## (The choice set varies depending on the month of the year)

dbp <- dist_hauls_catch_shares %>% 
  group_by(selection, VESSEL_NUM, set_year, set_month) %>% 
  summarize(sum_rev = sum(Revenue, na.rm = TRUE)) %>%
  group_by(selection, VESSEL_NUM, set_month) %>%
  summarize(mean_rev = mean(sum_rev, na.rm = TRUE)) %>%
  group_by(VESSEL_NUM, set_month) %>%
  mutate(tot_rev = sum(mean_rev, na.rm = TRUE)) %>% ungroup() %>%
  mutate(catch_composition = mean_rev/tot_rev)

full_choice_set <- dist_hauls_catch_shares %>%
  dplyr::select(selection) %>% unique() %>% mutate(merge=1)

all_vessels <- dist_hauls_catch_shares %>% 
  dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)

all_month <- dist_hauls_catch_shares %>% 
  dplyr::select(set_month) %>% unique() %>% mutate(merge=1)

expand <- merge(full_choice_set, all_vessels, 
                by = c('merge'), all.x = TRUE, all.y = TRUE)

expand <- merge(expand, all_month, 
                by = c('merge'), all.x = TRUE, all.y = TRUE)

dbp <- merge(expand, dbp, by = c('VESSEL_NUM', 'selection', 'set_month'), all.x = TRUE) %>%
  mutate(catch_composition = ifelse(is.na(catch_composition),0,catch_composition))

dbp <- dbp %>%
  group_by(selection, set_month) %>%
  summarize(prop = mean(catch_composition)) 


### Create factor by month and graph composition
cl <- makeCluster(4)
registerDoParallel(cl)

dbp3 <- foreach::foreach(ll = 1:12, .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% {
  dbp2 <- dbp %>% dplyr::filter(set_month == ll)  %>%  
    mutate(prop = ifelse(prop == 0,0.00001,prop))
  factor <- 1/sum(dbp2$prop)
  dbp2 <- dbp2 %>% 
    mutate(prop = prop * factor)
  sum(dbp2$prop)
  return(dbp2)
}

dbp_month <- as.data.frame(do.call(rbind.data.frame, dbp3)) %>%
  dplyr::filter(prop > 0.2) %>% 
  dplyr::mutate(hist_selection = 1) %>%
  dplyr::select(-c(prop))
  
write.csv(dbp_month,"C:\\GitHub\\EconAnalysis\\Participation\\Stata\\dbp_month_Stata_c4.csv", row.names = FALSE)

