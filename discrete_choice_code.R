################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
memory.limit(9999999999)

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
library(mlogit)
library(parallel)
#-----------------------------------------------------------------------------

## Read logbooks available for Pacific Sardine from ODFW ##

psdn.logbook <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(set_lat = Lat + LatMin/60) %>%
  mutate(set_long = Long + LongMin/60) %>%
  mutate(up_lat = set_lat) %>%
  mutate(up_long = set_long) %>%
  mutate(fleet_name="OR") %>%
  mutate(species="PSDN") %>%
  dplyr::rename(drvid = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  mutate(set_day = day(set_date)) %>%
  dplyr::rename(catch = Sard) %>%
  dplyr::rename(haul_num = Set) %>%
  dplyr::rename(trip_id = Ticket) %>%
  dplyr::rename(depth = Depth) %>%
  filter(set_year > 2000) 

psdn.logbook <- psdn.logbook[-which(is.na(psdn.logbook$set_lat)), ] 
psdn.logbook <- psdn.logbook[-which(psdn.logbook$haul_num == 0), ] 
psdn.logbook$set_long <- with(psdn.logbook, ifelse(set_long > 0, -set_long, set_long))
psdn.logbook$haul_id <- udpipe::unique_identifier(psdn.logbook, fields = c("trip_id", "haul_num"))
psdn.logbook$depth_bin <- cut(psdn.logbook$depth, 9, include.lowest=TRUE, 
                              labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

psdn.logbook <- psdn.logbook %>% 
  dplyr::select(c('set_lat', 'set_long', 'up_lat', 'up_long', 'depth_bin', 'drvid', 'fleet_name', 
                'set_year', 'set_month', 'set_day', 'set_date', 
                'catch', 'haul_num', 'haul_id', 'trip_id')) %>% drop_na()


### Include port coordinate associated to a specific vessel ###
psdn.port.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\2021 CPS Request Lengths.xlsx", sheet = "2021_CPS_Request_Lengths") %>%
  filter(Year > 2000) %>%
  dplyr::rename(set_year = Year) %>%
  dplyr::rename(drvid = BOATID) %>%
  dplyr::rename(port = PORT) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_month = month(set_date)) %>%
  mutate(set_day = day(set_date)) %>%
  filter(COMMON_NAME == 'PACIFIC SARDINE') %>%
  distinct(drvid, port, set_year)

psdn.logbook <- merge(psdn.logbook,psdn.port.OR,by=c('drvid', 'set_year'),all.x = TRUE)


# Load port georeferenced data
ports <- readr::read_csv("C://GitHub//EconAnalysis//Data//port_names.csv")

# change uppercase letters
psdn.logbook <- psdn.logbook %>%
  mutate(port = tolower(port)) 

ports <- ports %>%
  dplyr::rename(port = port_name) %>%
  dplyr::rename(d_port_long = lon) %>%
  dplyr::rename(d_port_lat = lat) %>%
  mutate(port = tolower(port))

psdn.logbook <- merge(psdn.logbook,ports,by=c('port'),all.x = TRUE) 

psdn.logbook.set <- psdn.logbook %>%
                      filter(set_year >= 2011, set_year <= 2012)


#-----------------------------------------------------------------------------

## Sampling choice data ##
source("C:\\GitHub\\EconAnalysis\\Functions\\sampled_rums.R")

samps <- sampled_rums(data_in = psdn.logbook, the_port = "OR",
                      min_year = 2010, max_year = 2012, ndays = 30, 
                      focus_year = 2012, nhauls_sampled = 10, 
                      seed = 42, ncores = 4, rev_scale = 100, 
                      model_type = 'no_bycatch', net_cost = "qcos",
                      habit_distance = 5, return_hauls = FALSE)

