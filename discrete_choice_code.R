################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
memory.limit(9999999999)
min.year = 2012
max.year = 2014

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

## Read logbooks available for Pacific Sardine from ODFW ##

psdn.logbook <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
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


#-----------------------------------------------------------------------------
# Use Vessel ID to obtain MMSI

#' Scrapes call sign and mmsi from USCG NAIS website using list of US Official numbers
#' from the CPS permit data I collected earlier.
#'

library(rvest)
library(curl)

# Use IDs from PSDN logbook in Oregon. Later we can expand this for the whole west coast using individual vessel data # 
id <- psdn.logbook %>%
  select(drvid, BoatName) %>%
  unique()

id$mmsi <- "NA"

for(i in 1:nrow(id)) {
  url <- str_c("https://www.navcen.uscg.gov/aisSearch/dbo_aisVessels_list.php?q=(US%20Official%20No~contains~", id$drvid[i], ")&f=all#skipdata")
  
  html.table <- read_html(url) %>%
    html_node("table") %>%
    html_table(fill = T) %>%
    slice(13:14)
  
  names(html.table) <- html.table[1,]
  html.table <- html.table[-1,]
  if(is.null(html.table$`MMSI:`) == T) {
    id$mmsi[i] <- "NA"
  }
  else {
    id$mmsi[i] <- html.table$'MMSI:'
  }
}

### Fix it manually using vessel.names from GFW databse 
### (online: https://globalfishingwatch.org/map/)

# write.csv(id,"id_mmsi.csv", row.names = FALSE)
id.updated <- readr::read_csv(here::here("id_mmsi.csv"))



#--------------------------------------------------------
# Load database from Global Fishing Watch... Add set variable (position within a day)

gfw.fishing.effort <- readr::read_csv(here::here("Data", "GFW_data", "GFW_westcoast_2012-2014.csv"))
vessel.names <- readr::read_csv(here::here("Data", "GFW_data", "MMSI_vessel_name.csv"))

gfw.fishing.effort <- gfw.fishing.effort %>%
  merge(y = vessel.names, by = "mmsi", all.x=TRUE)

# --------------------------------------------------------
# Merge GFW to logbook data
id_mmsi <- id %>% select(drvid, mmsi) %>% drop_na()

psdn.logbook.gfw <- psdn.logbook %>% 
  left_join(id_mmsi, by = "drvid")  %>%
  dplyr::rename(date = Date) %>% merge(y = gfw.fishing.effort, by = c("mmsi", "date"), all.x=TRUE) 
# I just want to do this to identify wich vessels from GFW actually harvest PSDN or MSQD or ANCHOVY...


#-----------------------------------------------------------------------------
# Clean dataset for discrete choice model

psdn.logbook <- psdn.logbook[-which(is.na(psdn.logbook$set_lat)), ] 
psdn.logbook <- psdn.logbook[-which(psdn.logbook$haul_num == 0), ] 
psdn.logbook$set_long <- with(psdn.logbook, ifelse(set_long > 0, -set_long, set_long))
psdn.logbook$haul_id <- udpipe::unique_identifier(psdn.logbook, fields = c("trip_id", "haul_num"))
psdn.logbook$depth_bin <- cut(psdn.logbook$depth, 9, include.lowest=TRUE, 
                              labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

psdn.logbook <- psdn.logbook %>% 
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



