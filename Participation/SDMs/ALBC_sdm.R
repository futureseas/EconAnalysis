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
dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/Albacore/alb_6_2010_mboost_roms.nc"))

set_long <- ncdf4::ncvar_get(dat, "lon")
set_lat <- ncdf4::ncvar_get(dat, "lat")
albc.date.sdm <- ncdf4::ncvar_get(dat, "time")
albc.sdm <- ncdf4::ncvar_get(dat, "predGAMBOOST")
ncdf4::nc_close(dat)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(albc.sdm) <- list(set_long = set_long, set_lat = set_lat, albc.date.sdm = albc.date.sdm)
sdmMelt <- reshape2::melt(albc.sdm, value.name = "albc.sdm")
coord <- sdmMelt %>% dplyr::select(c(set_lat, set_long)) %>% unique()
saveRDS(coord, "Participation/SDMs/port_dist_albc/coord_sdm_albc.rds")


#------------------------------------------------------
# Get distance by coordinate to port j and save it

coord <- readRDS("Participation/SDMs/port_dist_albc/coord_sdm_albc.rds")
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (j in 1:nrow(port_area_coord)) {
  distPorts <- coord %>%
    mutate(dist = by(., 1:nrow(.), function(row) {
      distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
    })) %>%
    mutate(dist = dist / 1000) %>% 
    dplyr::filter(dist <= 180)
  
  saveRDS(distPorts, paste0("Participation/SDMs/port_dist_albc/portDist_", paste0(as.character(j),".rds")))
}

#----------------
# Albacore

sdm.albc <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   albc_SDM_90 = numeric(),
                   albc_SDM_180 = numeric())

### https://wdfw.wa.gov/fishing/commercial/tuna 50 to 100 nautical miles

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (y in 2000:2020) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {

      # Open the monthly file that contain SDM by location
      dat <- 
        ncdf4::nc_open(
          paste0("G:/My Drive/Data/SDM/Albacore/alb_",
                 paste0(as.character(m),
                        paste0("_",
                               paste0(as.character(y),"_mboost_roms.nc")))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      albc.date.sdm <- ncdf4::ncvar_get(dat, "time")
      albc.sdm <- ncdf4::ncvar_get(dat, "predGAMBOOST")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(albc.sdm) <- list(set_long = set_long, set_lat = set_lat, albc.date.sdm = albc.date.sdm)
      sdmMelt <- reshape2::melt(albc.sdm, value.name = "albc.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$albc.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(
        paste0("Participation/SDMs/port_dist_albc/portDist_",
               paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date))
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_90  <- sdmMelt %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_180  <- sdmMelt %>% dplyr::filter(dist <= 180)  %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_90  <- mean(dat_prob_30$albc.sdm, na.rm = TRUE)
        SDM_mean_180  <- mean(dat_prob_60$albc.sdm, na.rm = TRUE)
        sdm.albc <- sdm.albc %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  albc_SDM_90  = SDM_mean_90, 
                  albc_SDM_180  = SDM_mean_180)
        rm(dat_prob_90, dat_prob_180, SDM_mean_90, SDM_mean_180)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.albc, file = "Participation/SDMs/sdm_albc.rds")
    }
  }
}
