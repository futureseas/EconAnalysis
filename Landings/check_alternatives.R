
################################### 
## Check alternatives by cluster ##
###################################

gc()
rm(list=ls())

## Load packages ##
library(doParallel)
library(tidyr)
library(plm)
library(tidyverse)
library(lubridate)

dat <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

vessel_part_year <- dat %>% 
  group_by(VESSEL_NUM, set_year) %>%
  summarize(landings = sum(Landings_mtons, na.rm = TRUE),
            n_trips = n()) %>%
  arrange(set_year, desc(n_trips)) %>% 
  mutate(perc = n_trips / 360 * 100)

vessel_part_year <- vessel_part_year %>% 
  dplyr::filter(n_trips >= 20) %>% 
  dplyr::select(c('VESSEL_NUM', 'set_year')) %>%
  unique() %>% ungroup() %>% mutate(active_year = 1)
  # saveRDS(vessel_part_year, file = "C:\\Data\\PacFIN data\\vessel_part_year.RDS")

dat <- dat %>% 
  merge(vessel_part_year, by = c('VESSEL_NUM', 'set_year'), 
        all.x = TRUE, all.y = FALSE)  %>% 
        dplyr::filter(active_year == 1) 


#-----------------------------------------------------------------------------
## Define hauls data used for estimation (in this case, are the trips)


hauls <- dat %>% 
  dplyr::filter(set_year >= 2013, 
                set_year <= 2017, 
                group_all %in% 5) %>% 
  distinct(trip_id, .keep_all = T) %>% 
  dplyr::select(trip_id, VESSEL_NUM, set_year, set_month, 
                set_day, Revenue, selection, n_obs_within_FTID) %>% as.data.frame


pre <- hauls %>% 
  dplyr::filter(set_year < 2015) %>%
  group_by(selection) %>%
  mutate(fished = 1) %>%
  dplyr::mutate(time_chosen = sum(fished)) %>% 
  ungroup() %>% 
  dplyr::mutate(NP_n_choice_occ = ifelse(selection == "No-Participation", time_chosen, 0)) %>%
  dplyr::mutate(n_choice_occ = sum(fished) - max(NP_n_choice_occ))  %>%
  dplyr::mutate(perc_chosen = round(time_chosen*100/n_choice_occ, digits = 1)) %>%
  dplyr::select(c('perc_chosen', 'selection')) %>% unique()


post <- hauls %>% 
  dplyr::filter(set_year >= 2015) %>%
  group_by(selection) %>%
  mutate(fished = 1) %>%
  dplyr::mutate(time_chosen = sum(fished)) %>% 
  ungroup() %>% 
  dplyr::mutate(NP_n_choice_occ = ifelse(selection == "No-Participation", time_chosen, 0)) %>%
  dplyr::mutate(n_choice_occ = sum(fished) - max(NP_n_choice_occ))  %>%
  dplyr::mutate(perc_chosen = round(time_chosen*100/n_choice_occ, digits = 1)) %>%
  dplyr::select(c('perc_chosen', 'selection')) %>% unique()


all <- hauls %>%
  group_by(selection) %>%
  mutate(fished = 1) %>%
  dplyr::mutate(time_chosen = sum(fished)) %>% 
  ungroup() %>% 
  dplyr::mutate(NP_n_choice_occ = ifelse(selection == "No-Participation", time_chosen, 0)) %>%
  dplyr::mutate(n_choice_occ = sum(fished) - max(NP_n_choice_occ))  %>%
  dplyr::mutate(perc_chosen = round(time_chosen*100/n_choice_occ, digits = 1)) %>%
  dplyr::select(c('perc_chosen', 'selection')) %>% unique()

