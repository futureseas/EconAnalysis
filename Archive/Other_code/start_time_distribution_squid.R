##############################################################
#### Distribution of hours that squid vessels start trips ####
##############################################################

### Load MSQD logbooks ###
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
library(dplyr)
library(hrbrthemes)

# Load data

sqd.logbook.vessel <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  mutate(vessel="CA Vessel") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "CatchEstimate") %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort', 'StartTime')) %>%
  mutate(d_closure = ifelse(year < 2015, "Pre Closure", "Post Closure"))

sqd.logbook.vessel$StartTime <- as.numeric(as.character(sqd.logbook.vessel$StartTime)) 
sqd.logbook.vessel %>%  ggplot(aes(x=StartTime, fill= d_closure)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


sqd.logbook.light.brail <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  dplyr::rename(lat = Lat_DD) %>%
  dplyr::rename(lon = Long_DD) %>%
  mutate(vessel="CA Light Boat") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "ElapsedTime") %>% 
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort', 'StartTime')) %>%
  mutate(d_closure = ifelse(year < 2015, "Pre Closure", "Post Closure"))

sqd.logbook.light.brail$StartTime <- as.numeric(as.character(sqd.logbook.light.brail$StartTime)) 
sqd.logbook.light.brail %>%  ggplot(aes(x=StartTime, fill= d_closure)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


# DATABASE
sqd.logbook <- rbind(sqd.logbook.vessel,sqd.logbook.light.brail)