######################################################################################## 
## Sampling choice set for port-species model based on Peter's code for Hicks's paper ##
########################################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
rm(list=ls())
memory.limit(9999999999)
min.year = 2009
max.year = 2016

## Load packages ##
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)
library(tidyr)
library(tidyverse)
library(mlogit)
library(parallel)

#-----------------------------------------------------------------------------

## Read participation database ##
participation_data <- read.csv("participation_data.csv")


#-----------------------------------------------------------------------------
# Clean dataset for discrete choice model (no haul in this case)

participation_data$trip_id <- udpipe::unique_identifier(participation_data, fields = c("FTID"))
participation_data$set_date<-as.Date(with(
   participation_data,
   paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
   "%Y-%m-%d")

participation_data$set_date <- as.Date(participation_data$set_date, "%d%b%Y %H:%M:%OS")
  participation_data <- participation_data %>% drop_na(set_date)  


# --------------------------------------------------
## Save database! 
participation_data.save <- participation_data %>%
      dplyr::rename(set_day = LANDING_DAY) %>% 
      dplyr::rename(set_month = LANDING_MONTH) %>%
      dplyr::rename(set_year = LANDING_YEAR) %>%
      select("set_date", "set_day", "set_month", "set_year", "Landings", "Revenue", 
             "Species_Dominant", "selection", "VESSEL_NUM", "trip_id", "selection", 'group_all')

write.csv(participation_data.save,"discrete_choice_participation.csv", row.names = FALSE)
# save.image(file = "disc_choice.RData")


#-----------------------------------------------------------------------------
## Sampling choice data ##
source("C:\\GitHub\\EconAnalysis\\Functions\\sampled_rums_participation.R")

sampsSDM <- sampled_rums(data_in = participation_data.save, cluster = "all",
                         min_year = min.year, max_year = max.year, ndays = 30,
                         focus_year = max.year, nhauls_sampled = 10,
                         seed = 42, ncores = 4, rev_scale = 100, habit_distance = 5,
                         return_hauls =FALSE, exp_rev = "sdm")

sampsCatch <- sampled_rums(data_in = psdn.logbook, cluster = "all",
                           min_year = min.year, max_year = max.year, ndays = 30,
                           focus_year = max.year, nhauls_sampled = 10,
                           seed = 42, ncores = 4, rev_scale = 100, habit_distance = 5,
                           return_hauls =FALSE, exp_rev = "catch")

