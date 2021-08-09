################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################

## Load packages ##
library(ggmap)
library(tidyverse)
library(gganimate)
library(magick)
library(lubridate)
library(here)
library(tigris)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(plyr)
library(readxl)

## Read logbooks available for Pacific Sardine from CDFW ##

### Oregon
psdn.logbook.OR <- read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(Lat = Lat + LatMin/60) %>%
  mutate(Long = Long + LongMin/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = FedDoc) %>%
  mutate(date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = Sard) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

psdn.logbook.WA <- read_excel("C:\\Data\\WDFW CPS logbooks\\WA Sardine Logbook Flatfile Data Reques 20-15485.xlsx") %>%
  mutate(Lat = `Latitude Degrees` + `Latitude Minutes`/60) %>%
  mutate(Long = `Longitude Degrees` + `Longitude Minutes`/60) %>%
  mutate(vessel="WA") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Vessel`) %>%
  mutate(date = as.Date(`Fishing Date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Sardine Retained mt`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))



## Read logbooks available for Pacific Sardine from ODFW and CDFW ##

### Oregon

sqd.logbook.OR.2016 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = FedDoc) %>%
  mutate(date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Squid (Lbs)`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2018 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2018") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Doc #`) %>%
  tidyr::fill(VesselID) %>%
  mutate(date = as.Date(`Log Date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2019 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2019") %>%
  mutate(lat = Lat + lat/60) %>%
  mutate(lon = Long + long/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(VesselID = `doc number`) %>%
  mutate(date = as.Date(`Fishing date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Est Lbs Squid`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2020 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2020") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Doc #`) %>%
  mutate(date = as.Date(`log date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR <- rbind(sqd.logbook.OR.2016, sqd.logbook.OR.2018, sqd.logbook.OR.2019, sqd.logbook.OR.2020)


### California

sqd.logbook.vessel <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  mutate(vessel="CA Vessel") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "CatchEstimate") %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.light.brail <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  dplyr::rename(lat = Lat_DD) %>%
  dplyr::rename(lon = Long_DD) %>%
  mutate(vessel="CA Light Boat") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "ElapsedTime") %>% 
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))


# DATABASE
sqd.logbook <- rbind(sqd.logbook.OR, sqd.logbook.vessel,sqd.logbook.light.brail)
sqd.logbook <- sqd.logbook[-which(is.na(sqd.logbook$lat)), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lat == 0), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lon == 0), ] 
sqd.logbook$lon <- with(sqd.logbook, ifelse(lon > 0, -lon, lon))

psdn.logbook <- rbind(psdn.logbook.OR, psdn.logbook.WA)
psdn.logbook <- psdn.logbook[-which(is.na(psdn.logbook$lat)), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lat == 0), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lon == 0), ] 
psdn.logbook$lon <- with(psdn.logbook, ifelse(lon > 0, -lon, lon))

logbook_data <- psdn.logbook %>% 
  filter(year > 2001)




## Sampling choice data ##

  #Sample Hauls  
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls)) # changes hauls for trips?
  
  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls), 
                                    .export = c("sample_hauls"), 
                                    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% 
    sample_hauls(xx = ii, hauls1 = hauls, 
                 dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled,
                 depth_bin_proportions = dbp, the_seed = seedz[ii])
  
  print("Done sampling hauls")  
  sampled_hauls <- plyr::ldply(sampled_hauls)