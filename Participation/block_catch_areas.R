######################################
### Read block maps for each state ###
######################################

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)  


#-----------------------------------------------------
### Load in the data
part.data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

CATCH_AREAS <- part.data %>% 
  group_by(CATCH_AREA_CODE, AREA_TYPE_CODE) %>% 
  summarize(n_obs = n()) %>% 
  filter(AREA_TYPE_CODE == 1) 

numbers_only <- function(x) !grepl("\\D", x)
part.data$numbers <- numbers_only(part.data$CATCH_AREA_CODE)

part.data <- part.data %>% 
  mutate(CATCH_AREA_CODE = ifelse(numbers == TRUE, as.numeric(CATCH_AREA_CODE), NA)) %>%
  mutate(CATCH_AREA_CODE = as.numeric(CATCH_AREA_CODE)) %>%
  mutate(CATCH_AREA_CODE = ifelse(AREA_TYPE_CODE != 1, NA, CATCH_AREA_CODE))

### Open layers
setwd("C:/GitHub/EconAnalysis/Participation/BlockAreas")

library(maptools)
library(rgeos)
landuse <- readShapePoly("NOAA_Block/NonWDFWFisheryManagementAreas - NOAA Coastal Trawl Logbook Block") 
data <- as.data.frame(landuse) %>% select(c(BlockNumbe, CentroidLo, CentroidLa))
part.data.merge <- merge(part.data, data, 
                         by.x = 'CATCH_AREA_CODE', 
                         by.y = 'BlockNumbe', all.x = TRUE, all.y = FALSE)

###-------------------------------------------
# Link to logbook now!

# Load data

## Oregon Squid
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv") 
Tickets_filtered <- Tickets2 %>% filter(FTID == 4589493)
colnames(sqd.logbook.OR.2016)

sqd.logbook.OR.2016 <- readxl::read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(AGENCY_CODE="O") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(LANDING_MONTH = month(set_date)) %>%
  dplyr::rename(FTID = `Ticket #`) %>%
  dplyr::mutate(FTID = as.integer(FTID)) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'LANDING_YEAR', 'LANDING_MONTH', 'set_date', 'FTID'))

sqd.logbook.OR.2018 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2018") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = `Doc #`) %>%
  tidyr::fill(VESSEL_NUM) %>%
  mutate(set_date = as.Date(`Log Date`,format="%Y-%m-%d")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(month = month(set_date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'LANDING_YEAR', 'month', 'set_date', 'effort'))

sqd.logbook.OR.2019 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2019") %>%
  mutate(lat = Lat + lat/60) %>%
  mutate(lon = Long + long/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(VESSEL_NUM = `doc number`) %>%
  mutate(set_date = as.Date(`Fishing date`,format="%Y-%m-%d")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(month = month(set_date)) %>%
  dplyr::rename(effort = `Est Lbs Squid`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'LANDING_YEAR', 'month', 'set_date', 'effort'))

sqd.logbook.OR.2020 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2020") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = `Doc #`) %>%
  mutate(set_date = as.Date(`log date`,format="%Y-%m-%d")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(month = month(set_date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'LANDING_YEAR', 'month', 'set_date', 'effort'))

sqd.logbook.OR <- rbind(sqd.logbook.OR.2016, sqd.logbook.OR.2018, sqd.logbook.OR.2019, sqd.logbook.OR.2020)


## California Squid

sqd.logbook.vessel <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  mutate(vessel="CA Vessel") %>%
  mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(month = month(set_date)) %>%
  dplyr::rename(effort = "CatchEstimate") %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'LANDING_YEAR', 'month', 'set_date', 'effort'))

sqd.logbook.light.brail <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  dplyr::rename(lat = Lat_DD) %>%
  dplyr::rename(lon = Long_DD) %>%
  mutate(vessel="CA Light Boat") %>%
  mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(month = month(set_date)) %>%
  dplyr::rename(effort = "ElapsedTime") %>% 
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'LANDING_YEAR', 'month', 'set_date', 'effort'))


# DATABASE
sqd.logbook <- rbind(sqd.logbook.OR, sqd.logbook.vessel,sqd.logbook.light.brail)
sqd.logbook <- sqd.logbook[-which(is.na(sqd.logbook$lat)), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lat == 0), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lon == 0), ] 
sqd.logbook$lon <- with(sqd.logbook, ifelse(lon > 0, -lon, lon))


# Load data

nanc.logbook.OR.2016 <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Anchovy logbooks.xlsx", sheet = "2016") 
nanc.logbook.OR.2016 <- nanc.logbook.OR.2016 %>% dplyr::select(c("Boat Name", "FedDoc", "Date", "Set", "Time", "Fathoms",
                                                                 "Temp", "Long (D)", "Long (Decimal Min)", "Lat (D)", 
                                                                 "Lat (DM)", "lbs anchovy", "Ticket"))
nanc.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Anchovy logbooks.xlsx", sheet = "2015") 
nanc.logbook.OR <- nanc.logbook.OR %>% mutate(Ticket = "No ticket") %>% dplyr::select(-c("lbs sardine", "lbs mackerel"))

nanc.logbook.OR <- rbind(nanc.logbook.OR, nanc.logbook.OR.2016)
nanc.logbook.OR <- nanc.logbook.OR %>%
  dplyr::rename("lat_D" = "Lat (D)") %>% dplyr::rename("lat_min" = "Lat (DM)") %>%
  mutate(set_lat = lat_D + lat_min/60) %>%
  dplyr::rename("lon_D" = "Long (D)") %>% dplyr::rename("lon_min" = "Long (Decimal Min)")  %>% 
  mutate(set_lon = lon_D + lon_min/60) %>%
  mutate(fleet_name="OR") %>%
  dplyr::rename(drvid = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(catch = `lbs anchovy`) %>%
  dplyr::select(c('set_lat', 'set_lon', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'catch'))

# DATABASE
nanc.logbook.OR <- nanc.logbook.OR[-which(is.na(nanc.logbook.OR$set_lat)), ] 
nanc.logbook.OR$set_lon <- with(nanc.logbook.OR, ifelse(set_lon > 0, -set_lon, set_lon))


# Load data

## Oregon
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

# DATABASE
psdn.logbook <- rbind(psdn.logbook.OR, psdn.logbook.WA)
psdn.logbook <- psdn.logbook[-which(is.na(psdn.logbook$lat)), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lat == 0), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lon == 0), ] 
psdn.logbook$lon <- with(psdn.logbook, ifelse(lon > 0, -lon, lon))
