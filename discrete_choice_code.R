################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
memory.limit(9999999999)
min.year = 2013
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

## Read logbooks available for Pacific Sardine from ODFW (2013-2015) ##
psdn.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(set_lat = Lat + LatMin/60) %>%
  mutate(set_long = Long + LongMin/60) %>%
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
  filter(set_year >= min.year) %>%
  filter(set_year <= max.year) %>%
  filter(trip_id != "No ticket") 

psdn.logbook.OR <- psdn.logbook.OR[-which(is.na(psdn.logbook.OR$set_lat)), ] 
psdn.logbook.OR <- psdn.logbook.OR[-which(psdn.logbook.OR$haul_num == 0), ] 
psdn.logbook.OR$set_long <- with(psdn.logbook.OR, ifelse(set_long > 0, -set_long, set_long))

logbooks <- psdn.logbook.OR %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'catch', 'BoatName', 'species'))


## Read logbooks available for market squid from ODFW (2016) ##
msqd.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(fleet_name="OR") %>%
  mutate(species="MSQD") %>%
  dplyr::rename(set_lat = Lat) %>%
  dplyr::rename(set_long = Long) %>%
  dplyr::rename(drvid = FedDoc) %>%
  dplyr::rename(BoatName = "Boat Name") %>%  
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(catch = `Squid (Lbs)`) %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'catch', 'BoatName', 'species'))
logbooks <- rbind(logbooks, msqd.logbook.OR)


## Read logbooks available for Northern anchovy from ODFW (2015-2016) ##
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
    mutate(set_long = lon_D + lon_min/60) %>%
    mutate(fleet_name="OR") %>%
  dplyr::rename(drvid = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(catch = `lbs anchovy`) %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'catch'))



## Read logbooks available for Market squid from CDFW (2013-2016) ##

  ## California
  
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
  
  
## Read logbooks available for Market squid from WDFW (2013-2014) ##
  
  
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
  
  
#-----------------------------------------------------------------------------
# Use Vessel ID to obtain MMSI

#' Scrapes call sign and mmsi from USCG NAIS website using list of US Official numbers
#' from the CPS permit data I collected earlier.
#'

library(rvest)
library(curl)

# Use IDs from PSDN logbook in Oregon. Later we can expand this for the whole west coast using individual vessel data # 

# id <- psdn.logbook %>%
#   select(drvid, BoatName) %>%
#   unique()
# 
# id$mmsi <- "NA"
# 
# for(i in 1:nrow(id)) {
#   url <- str_c("https://www.navcen.uscg.gov/aisSearch/dbo_aisVessels_list.php?q=(US%20Official%20No~contains~", id$drvid[i], ")&f=all#skipdata")
# 
#   html.table <- read_html(url) %>%
#     html_node("table") %>%
#     html_table(fill = T) %>%
#     slice(13:14)
# 
#   names(html.table) <- html.table[1,]
#   html.table <- html.table[-1,]
#   if(is.null(html.table$`MMSI:`) == T) {
#     id$mmsi[i] <- "NA"
#   }
#   else {
#     id$mmsi[i] <- html.table$'MMSI:'
#   }
# }
# 
# ### Fix it manually using vessel.names from GFW databse 
# ### (online: https://globalfishingwatch.org/map/)
# 
# write.csv(id,"id_mmsi_2013-2015.csv", row.names = FALSE)
id.updated <- readr::read_csv(here::here("id_mmsi_2013-2015.csv"))



#--------------------------------------------------------
# Load database from Global Fishing Watch... Add set variable (position within a day)

gfw.fishing.effort <- readr::read_csv(here::here("Data", "GFW_data", "GFW_westcoast_2013-2015.csv"))
  gfw.fishing.effort <- gfw.fishing.effort %>% filter(fishing_hours > 0) 
  gfw.fishing.effort$haul_num <- with(gfw.fishing.effort, ave(mmsi, mmsi, date, FUN = seq_along))

vessel.names <- readr::read_csv(here::here("Data", "GFW_data", "MMSI_vessel_name.csv"))

# gfw.fishing.effort <- gfw.fishing.effort %>%
#   merge(y = vessel.names, by = "mmsi", all.x=TRUE)


# --------------------------------------------------------
# Merge GFW to logbook data
id.mmsi <- id.updated %>% select(mmsi, drvid) %>% drop_na() %>% unique()

psdn.logbook.gfw <- psdn.logbook %>% left_join(id.mmsi, by = "drvid") %>% 
    dplyr::rename(date = Date) 
    psdn.logbook.gfw$mmsi[psdn.logbook.gfw$BoatName == "Pacific Pursuit"] <- 367153810
  psdn.logbook.gfw <- psdn.logbook.gfw %>% select(mmsi, date) %>% drop_na() %>% mutate(dPSDN = 1)
  
#---------------------------------------------------------
# Identify which vessels from GFW actually harvest PSDN or MSQD or ANCHOVY...
gfw.fishing.effort.CPS <- gfw.fishing.effort %>% 
  left_join(psdn.logbook.gfw, by = c("mmsi", "date")) %>% filter(dPSDN == 1) %>% unique()
  


#---------------------------------------------------------
# How many trips GFW capture from logbooks? (R: 121 trips of 495 for the Oregon's PSDN logbook -> 24%)
psdn.logbook.compare <- psdn.logbook %>% left_join(id.mmsi, by = "drvid") %>% 
  dplyr::rename(date = Date) # %>% filter(catch > 0)
  psdn.logbook.compare$mmsi[psdn.logbook.compare$BoatName == "Pacific Pursuit"] <- 367153810
  psdn.logbook.compare <- psdn.logbook.compare %>%  group_by(BoatName, drvid, mmsi, date) %>%
    summarise(across(c("set_lat", "set_long"), mean, na.rm = TRUE)) %>% drop_na()
  
gfw.fishing.effort.compare <- gfw.fishing.effort %>% 
  group_by(mmsi, date) %>%
  summarise(across(c("cell_ll_lat", "cell_ll_lon"), mean, na.rm = TRUE))

psdn.joint.compare <- psdn.logbook.compare %>% left_join(gfw.fishing.effort.compare, by = c("mmsi", "date")) 


# --------------------------------------------------------
# How good is GFW compared to logbooks? (R: 20.9546 km average deviation).
psdn.joint.compare <- psdn.joint.compare %>% drop_na()


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

psdn.joint.compare$dist <- getDistanceFromLatLonInKm(psdn.joint.compare$set_lat, psdn.joint.compare$set_long, 
                                               psdn.joint.compare$cell_ll_lat, psdn.joint.compare$cell_ll_lon)

summary(psdn.joint.compare$dist)
psdn.joint.hist <- psdn.joint.compare %>% filter(dist<400)
hist(psdn.joint.hist$dist)


##############################################################################
##############################################################################
#-----------------------------------------------------------------------------
# Clean dataset for discrete choice model
psdn.logbook.OR$haul_id <- udpipe::unique_identifier(psdn.logbook.OR, fields = c("trip_id", "haul_num"))
  psdn.logbook.OR$depth_bin <- cut(psdn.logbook.OR$depth, 9, include.lowest=TRUE, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
  psdn.logbook.OR <- psdn.logbook.OR %>% 
    mutate(up_lat = set_lat) %>%
    mutate(up_long = set_long) %>%
    mutate(set_lat_sdm = round(set_lat, digits = 1)) %>%
    mutate(set_long_sdm = round(set_long, digits = 1)) %>%
    dplyr::select(c('set_lat', 'set_long', 'up_lat', 'up_long', 'depth_bin', 'drvid', 'fleet_name', 
                'set_year', 'set_month', 'set_day', 'set_date', 
                'catch', 'haul_num', 'haul_id', 'trip_id', 'set_lat_sdm', 'set_long_sdm')) %>% drop_na()


# ------------------------------------------------------------------

## Include port coordinate associated to a specific vessel ###
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


# ------------------------------------------------------------------
## Merge logbook to SDM outputs

SDM_pred <- tibble(set_year = integer(),
                   set_month = integer(),
                   set_lat_sdm = numeric(),
                   set_long_sdm = numeric(),
                   pSDM = numeric())


for (y in min.year:max.year) {
  for (m in 1:12) {
    
    # Read netcdf
    dat <- ncdf4::nc_open(paste0("G:/My Drive/Project/Data/SDM/sardine/sard_", 
                          paste0(as.character(m), paste0("_", paste0(as.character(y),"_GAM.nc")))))
    lon <- ncdf4::ncvar_get(dat, "lon")
    lat <- ncdf4::ncvar_get(dat, "lat")
    tim <- ncdf4::ncvar_get(dat, "time")
    predSDM <- ncdf4::ncvar_get(dat, "predGAM")
    
    # Close the netcdf
    ncdf4::nc_close(dat)			
    
    # Reshape the 3D array so we can map it, change the time field to be date
    dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
    sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
    sdmMelt$dt <- as.Date("1900-01-01") + days(sdmMelt$tim)			
    
    sdmMelt <- sdmMelt %>%
      group_by(lat, lon) %>%
      summarize(exp_prob = mean(predSDM, na.rm = T))	%>%
      ungroup(.)  
    
    SDM_pred <- SDM_pred %>%
      add_row(set_year = y, set_month = m, set_long_sdm = sdmMelt$lon , set_lat_sdm = sdmMelt$lat, pSDM = sdmMelt$exp_prob)
    
    print(y)
    print(m)
  }
}

psdn.logbook <- merge(psdn.logbook,SDM_pred,by=c('set_year', 'set_month', 'set_lat_sdm', 'set_long_sdm'),all.x = TRUE) 


# ------------------------------------------------------------------
## Obtain (year) price variable from PacFIN landing data

PacFIN_dat <- read.csv(file = here::here("Data", "PacFin.csv"))
price_PSDN <- PacFIN_dat %>%
  dplyr::filter(Species_code == "PSDN") %>%
  group_by(Landing_year) %>%
  summarize(price.PSDN = mean(Price, na.rm = T)) %>%
  dplyr::rename(set_year = Landing_year)

psdn.logbook <- merge(psdn.logbook,price_PSDN,by=c('set_year'),all.x = TRUE) 



# ------------------------------------------------------------------
## Create psdn_rev variable (Using monthly SDM and catch)


psdn.logbook <- psdn.logbook %>%
  mutate(psdn.rev.catch = catch * price.PSDN) %>%
  mutate(psdn.rev.sdm = pSDM * price.PSDN)


#-----------------------------------------------------------------------------
## Sampling choice data ##
source("C:\\GitHub\\EconAnalysis\\Functions\\sampled_rums.R")

sampsSDM <- sampled_rums(data_in = psdn.logbook, the_port = "OR",
                      min_year = min.year, max_year = max.year, ndays = 30, 
                      focus_year = max.year, nhauls_sampled = 10, 
                      seed = 42, ncores = 4, rev_scale = 100, habit_distance = 5,
                      return_hauls =FALSE, exp_rev = "sdm")

sampsCatch <- sampled_rums(data_in = psdn.logbook, the_port = "OR",
                         min_year = min.year, max_year = max.year, ndays = 30, 
                         focus_year = max.year, nhauls_sampled = 10, 
                         seed = 42, ncores = 4, rev_scale = 100, habit_distance = 5,
                         return_hauls =FALSE, exp_rev = "catch")


# Think how to add a multispecies framework. 
# This would work with PacFIN landing data???



