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

## Functions ##
deg2rad <- function(deg) {
  m <- deg * (pi/180)
  return(m)
}

getDistanceFromLatLonInKm <- function(lat1,lon1,lat2,lon2) {
  R <- 6371
  dLat <- deg2rad(lat2-lat1)
  dLon <- deg2rad(lon2-lon1)
  a <- sin(dLat/2) * sin(dLat/2) +  cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}


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


# ------------------------------------------------------------------
## Merge port area with SDM outputs (month or daily??)


# -------------------------------------------------
## Merge storm warning signals
warnings.signals <- read.csv(file = "G://My Drive//Data//Storm Warnings//WCports_mww_events09-16.csv")
warnings.signals <- warnings.signals %>% 
  select("pcid", "d_issued", "d_expired", "hurricane", "gale", "smcraft", "mww_other") %>%
  dplyr::rename(PACFIN_PORT_CODE = pcid) 
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
warnings.signals <- warnings.signals %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)

warnings.signals$d_issued  <- as.Date(warnings.signals$d_issued, "%d%b%Y %H:%M:%OS")
warnings.signals$d_expired <- as.Date(warnings.signals$d_expired, "%d%b%Y %H:%M:%OS")
warnings.signals <- warnings.signals %>% unique() 

library(sqldf)
df1 <- participation_data
df2 <- warnings.signals

warnings.signals <-  sqldf("select df1.*, df2.hurricane, df2.gale, df2.smcraft, df2.mww_other
                                        from df1 left join df2 on
                                        (df1.Port_Dominant = df2.PORT_AREA_CODE) AND 
                                        (df1.set_date between df2.d_issued and df2.d_expired)") 

warnings.signals <- warnings.signals %>% unique() %>% 
  select("trip_id", "hurricane", "gale", "smcraft", "mww_other")

  warnings.signals <- warning.signals %>% group_by(trip_id) %>%
    summarise(hurricane = sum(hurricane), gale = sum(gale), 
              smcraft = sum(smcraft), mww_other = sum(mww_other))
    warnings.signals[is.na(warnings.signals)] <- 0

    participation_data <- merge(participation_data, warnings.signals, 
                                by=c("trip_id"), all.x = TRUE)

# --------------------------------------------------
## Save database! 
participation_data.save <- participation_data %>%
      dplyr::rename(set_day = LANDING_DAY) %>% 
      dplyr::rename(set_month = LANDING_MONTH) %>%
      dplyr::rename(set_year = LANDING_YEAR) %>%
      select("set_date", "set_day", "set_month", "set_year", "Landings", "Revenue", 
             "Species_Dominant", "selection", "VESSEL_NUM", "trip_id", "hurricane", 
             "gale", "smcraft", "mww_other", "selection")
         # "psdn.sdm", "nanc.sdm", "msqd.sdm", "wind.u", "wind.v"


write.csv(participation_data.save,"discrete_choice_participation.csv", row.names = FALSE)
# save.image(file = "disc_choice.RData")



# # ------------------------------------------------------------------
# ## Obtain (year) price by port from PacFIN landing data
# 
# PacFIN_dat <- read.csv(file = here::here("Data", "PacFin.csv"))
# price_PSDN <- PacFIN_dat %>%
#   dplyr::filter(Species_code == "PSDN") %>%
#   group_by(Landing_year) %>%
#   summarize(price.PSDN = mean(Price, na.rm = T)) %>%
#   dplyr::rename(set_year = Landing_year)
# 
# psdn.logbook <- merge(psdn.logbook,price_PSDN,by=c('set_year'),all.x = TRUE) 


# ------------------------------------------------------------------
## Create expected and actual revenue -- psdn_rev variable-- using SDMs (maybe moving average?) and catch
## (I need prices by port)

# psdn.logbook <- psdn.logbook %>%
#   mutate(psdn.rev.catch = catch * price.PSDN) %>%
#   mutate(psdn.rev.sdm = pSDM * price.PSDN)


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


# Think how to add a multispecies framework. 
# This would work with PacFIN landing data???



