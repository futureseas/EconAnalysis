#############################################
## Calculate SDM outputs to PORT_AREA_CODE ##
#############################################

### Try 60, 90, 200

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
dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/chub/chub_6_2010_GAM.nc"))

set_long <- ncdf4::ncvar_get(dat, "lon")
set_lat <- ncdf4::ncvar_get(dat, "lat")
cmck.date.sdm <- ncdf4::ncvar_get(dat, "time")
cmck.sdm <- ncdf4::ncvar_get(dat, "predGAM")
ncdf4::nc_close(dat)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(cmck.sdm) <- list(set_long = set_long, set_lat = set_lat, cmck.date.sdm = cmck.date.sdm)
sdmMelt <- reshape2::melt(cmck.sdm, value.name = "cmck.sdm")
coord <- sdmMelt %>% dplyr::select(c(set_lat, set_long)) %>% unique()
saveRDS(coord, "Participation/SDMs/port_dist_cmck/coord_sdm_cmck.rds")


#------------------------------------------------------
# Get distance by coordinate to port j and save it

coord <- readRDS("Participation/SDMs/port_dist_cmck/coord_sdm_cmck.rds")
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (j in 1:nrow(port_area_coord)) {
  distPorts <- coord %>%
    mutate(dist = by(., 1:nrow(.), function(row) {
      distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
    })) %>%
    mutate(dist = dist / 1000) %>% 
    dplyr::filter(dist <= 220)
  
  saveRDS(distPorts, paste0("Participation/SDMs/port_dist_cmck/portDist_", paste0(as.character(j),".rds")))
}

#------------------------------------------------------
# Calculate SDM per day

sdm.cmck <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   CMCK_SDM_60 = numeric(),
                   CMCK_SDM_90 = numeric(),
                   CMCK_SDM_20 = numeric())

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (y in 2012:2018) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      
      # Open the monthly file that contain SDM by location
      dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/chub/chub_",
                                   paste0(as.character(m),
                                          paste0("_", paste0(as.character(y),"_GAM.nc")))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      cmck.date.sdm <- ncdf4::ncvar_get(dat, "time")
      cmck.sdm <- ncdf4::ncvar_get(dat, "predGAM")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(cmck.sdm) <- list(set_long = set_long, set_lat = set_lat, cmck.date.sdm = cmck.date.sdm)
      sdmMelt <- reshape2::melt(cmck.sdm, value.name = "cmck.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$cmck.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(paste0("Participation/SDMs/port_dist_cmck/portDist_",
                                  paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date)) 
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_60  <- sdmMelt %>% dplyr::filter(dist <= 60)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_90  <- sdmMelt %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_20 <- sdmMelt %>% dplyr::filter(dist <= 20) %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_60  <- mean(dat_prob_60$cmck.sdm, na.rm = TRUE)
        SDM_mean_90  <- mean(dat_prob_90$cmck.sdm, na.rm = TRUE)
        SDM_mean_20 <- mean(dat_prob_20$cmck.sdm, na.rm = TRUE)
        sdm.cmck <- sdm.cmck %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  CMCK_SDM_60  = SDM_mean_60, 
                  CMCK_SDM_90  = SDM_mean_90, 
                  CMCK_SDM_20 = SDM_mean_20)
        rm(dat_prob_60, dat_prob_90, dat_prob_20, 
           SDM_mean_60, SDM_mean_90, SDM_mean_20)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.cmck, file = "Participation/SDMs/sdm_cmck.rds")
    }
  }
}


  for (m in 1:8) {
    for (j in 1:nrow(port_area_coord)) {
      
      y = 2019      
      # Open the monthly file that contain SDM by location
      dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/chub/chub_",
                                   paste0(as.character(m),
                                          paste0("_", paste0(as.character(y),"_GAM.nc")))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      cmck.date.sdm <- ncdf4::ncvar_get(dat, "time")
      cmck.sdm <- ncdf4::ncvar_get(dat, "predGAM")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(cmck.sdm) <- list(set_long = set_long, set_lat = set_lat, cmck.date.sdm = cmck.date.sdm)
      sdmMelt <- reshape2::melt(cmck.sdm, value.name = "cmck.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$cmck.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(paste0("Participation/SDMs/port_dist_cmck/portDist_",
                                  paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date)) 
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_60  <- sdmMelt %>% dplyr::filter(dist <= 60)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_90  <- sdmMelt %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_20 <- sdmMelt %>% dplyr::filter(dist <= 20) %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_60  <- mean(dat_prob_60$cmck.sdm, na.rm = TRUE)
        SDM_mean_90  <- mean(dat_prob_90$cmck.sdm, na.rm = TRUE)
        SDM_mean_20 <- mean(dat_prob_20$cmck.sdm, na.rm = TRUE)
        sdm.cmck <- sdm.cmck %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  CMCK_SDM_60  = SDM_mean_60, 
                  CMCK_SDM_90  = SDM_mean_90, 
                  CMCK_SDM_20 = SDM_mean_20)
        rm(dat_prob_60, dat_prob_90, dat_prob_20, 
           SDM_mean_60, SDM_mean_90, SDM_mean_20)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.cmck, file = "Participation/SDMs/sdm_cmck.rds")
    }
  }


# cmck <- readRDS(file = "Participation/SDMs/sdm_cmck.rds") %>%
#   dplyr::filter(LANDING_YEAR < 2012) 
# 
# saveRDS(cmck, file = "Participation/SDMs/sdm_cmck_pre_2012.rds")
