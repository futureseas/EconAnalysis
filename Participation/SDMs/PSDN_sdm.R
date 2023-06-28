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
psdn.date.sdm <- ncdf4::ncvar_get(dat, "time")
psdn.sdm <- ncdf4::ncvar_get(dat, "sardGAM")
ncdf4::nc_close(dat)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(psdn.sdm) <- list(set_long = set_long, set_lat = set_lat, psdn.date.sdm = psdn.date.sdm)
sdmMelt <- reshape2::melt(psdn.sdm, value.name = "psdn.sdm")
coord <- sdmMelt %>% dplyr::select(c(set_lat, set_long)) %>% unique()
saveRDS(coord, "Participation/SDMs/port_dist_psdn/coord_sdm_psdn.rds")


#------------------------------------------------------
# Get distance by coordinate to port j and save it

coord <- readRDS("Participation/SDMs/port_dist_psdn/coord_sdm_psdn.rds")
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (j in 1:nrow(port_area_coord)) {
  distPorts <- coord %>%
    mutate(dist = by(., 1:nrow(.), function(row) {
      distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
    })) %>%
    mutate(dist = dist / 1000) %>% 
    dplyr::filter(dist <= 220)
  
  saveRDS(distPorts, paste0("Participation/SDMs/port_dist_psdn/portDist_", paste0(as.character(j),".rds")))
}

#----------------
# Pacific sardine

sdm.psdn <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   PSDN_SDM_30 = numeric(),
                   PSDN_SDM_60 = numeric())

for (y in 2000:2020) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      
      # Open the monthly file that contain SDM by location
      dat <- 
        ncdf4::nc_open(
          paste0("G:/My Drive/Data/SDM/Historical CPS GAMs BRTs ERDDAP ROMS Domain/",
                 paste0(as.character(m),
                        paste0("_",
                               paste0(as.character(y),"_SDMs.nc")))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      psdn.date.sdm <- ncdf4::ncvar_get(dat, "time")
      psdn.sdm <- ncdf4::ncvar_get(dat, "sardGAM")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(psdn.sdm) <- list(set_long = set_long, set_lat = set_lat, psdn.date.sdm = psdn.date.sdm)
      sdmMelt <- reshape2::melt(psdn.sdm, value.name = "psdn.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$psdn.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(
        paste0("Participation/SDMs/port_dist_psdn/portDist_",
               paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date))
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_30  <- sdmMelt %>% dplyr::filter(dist <= 30)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_60  <- sdmMelt %>% dplyr::filter(dist <= 60)  %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_30  <- mean(dat_prob_30$exp_prob, na.rm = TRUE)
        SDM_mean_60  <- mean(dat_prob_60$exp_prob, na.rm = TRUE)
        sdm.psdn <- sdm.psdn %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  PSDN_SDM_30  = SDM_mean_30, 
                  PSDN_SDM_60  = SDM_mean_60)
        rm(dat_prob_30, dat_prob_60, SDM_mean_30, SDM_mean_60)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.psdn, file = "Participation/SDMs/sdm_psdn.rds")
    }
  }
}
