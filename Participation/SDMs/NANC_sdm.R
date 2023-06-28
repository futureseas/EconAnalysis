#############################################
## Calculate SDM outputs to PORT_AREA_CODE ##
#############################################

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)

#------------------------------------------------------
# Get SDM coordinates
dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/Historical CPS GAMs BRTs ERDDAP ROMS Domain/6_2010_SDMs.nc"))

  set_long <- ncdf4::ncvar_get(dat, "lon")
  set_lat <- ncdf4::ncvar_get(dat, "lat")
  nanc.date.sdm <- ncdf4::ncvar_get(dat, "time")
  nanc.sdm <- ncdf4::ncvar_get(dat, "anchGAM")
  ncdf4::nc_close(dat)

  # Reshape the 3D array so we can map it, change the time field to be date
  dimnames(nanc.sdm) <- list(set_long = set_long, set_lat = set_lat, nanc.date.sdm = nanc.date.sdm)
  sdmMelt <- reshape2::melt(nanc.sdm, value.name = "nanc.sdm")
  coord_sdm <- sdmMelt %>% dplyr::select(c(set_lat, set_long)) %>% unique()
  saveRDS(coord_sdm, "Participation/SDMs/port_dist_nanc/coord_sdm_nanc.rds")


#------------------------------------------------------
# Get distance by coordinate to port j and save it

coord <- readRDS("Participation/SDMs/port_dist_nanc/coord_sdm_nanc.rds")
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (j in 1:nrow(port_area_coord)) {
  distPorts <- coord %>%
    mutate(dist = by(., 1:nrow(.), function(row) {
      distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
    })) %>%
    mutate(dist = dist / 1000) %>% 
    dplyr::filter(dist <= 220)
  
  saveRDS(distPorts, paste0("Participation/SDMs/port_dist_nanc/portDist_", paste0(as.character(j),".rds")))
}

#----------------------------
# Calculate daily SDM radii

sdm.nanc <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   NANC_SDM_20 = numeric(),
                   NANC_SDM_30 = numeric(),
                   NANC_SDM_60 = numeric())

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (y in 2000:2020) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      
      # Open the monthly file that contain SDM by location
      dat <- ncdf4::nc_open(
        paste0("G:/My Drive/Data/SDM/Historical CPS GAMs BRTs ERDDAP ROMS Domain/",
               paste0(as.character(m),
                      paste0("_",
                             paste0(as.character(y),"_SDMs.nc")))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      nanc.date.sdm <- ncdf4::ncvar_get(dat, "time")
      nanc.sdm <- ncdf4::ncvar_get(dat, "anchGAM")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(nanc.sdm) <- list(set_long = set_long, set_lat = set_lat, nanc.date.sdm = nanc.date.sdm)
      sdmMelt <- reshape2::melt(nanc.sdm, value.name = "nanc.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$nanc.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(
        paste0("Participation/SDMs/port_dist_nanc/portDist_",
               paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date))
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_20  <- sdmMelt %>% dplyr::filter(dist <= 20) %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_30  <- sdmMelt %>% dplyr::filter(dist <= 30) %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_60  <- sdmMelt %>% dplyr::filter(dist <= 60) %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_20  <- mean(dat_prob_20$nanc.sdm, na.rm = TRUE)
        SDM_mean_30  <- mean(dat_prob_30$nanc.sdm, na.rm = TRUE)
        SDM_mean_60  <- mean(dat_prob_60$nanc.sdm, na.rm = TRUE)
        sdm.nanc <- sdm.nanc %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  NANC_SDM_20  = SDM_mean_20, 
                  NANC_SDM_30  = SDM_mean_30, 
                  NANC_SDM_60  = SDM_mean_60)
        rm(dat_prob_20, dat_prob_30, dat_prob_60, 
           SDM_mean_20, SDM_mean_30, SDM_mean_60)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.nanc, file = "Participation/SDMs/sdm_nanc_update.rds")
    }
  }
}