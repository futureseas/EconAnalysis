#############################################
## Calculate SDM outputs to PORT_AREA_CODE ##
#############################################

rm(list=ls())
gc()

### Try 30, 90, 200

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)


# #------------------------------------------------------
# # Get SDM coordinates
# dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/squid_spawn/squidSpawn_6_2010_envelope.nc"))
# 
#   set_long <- ncdf4::ncvar_get(dat, "lon")
#   set_lat <- ncdf4::ncvar_get(dat, "lat")
#   msqd.date.sdm <- ncdf4::ncvar_get(dat, "time")
#   msqd.sdm <- ncdf4::ncvar_get(dat, "pred")
#   ncdf4::nc_close(dat)
#   
#   # Reshape the 3D array so we can map it, change the time field to be date
#   dimnames(msqd.sdm) <- list(set_long = set_long, set_lat = set_lat, msqd.date.sdm = msqd.date.sdm)
#   sdmMelt <- reshape2::melt(msqd.sdm, value.name = "msqd.sdm")
#   coord <- sdmMelt %>% dplyr::select(c(set_lat, set_long)) %>% unique()
#   saveRDS(coord, "Participation/SDM_code/coord_sdm_msqd_spawn.rds")
# 
# 
# #------------------------------------------------------  
# # Get distance by coordinate to port j and save it
# 
# coord <- readRDS("Participation/SDM_code/coord_sdm_msqd_spawn.rds")
# port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()
# 
# for (j in 1:nrow(port_area_coord)) {
#   distPorts <- coord %>%
#     mutate(dist = by(., 1:nrow(.), function(row) {
#       distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
#     })) %>%
#     mutate(dist = dist / 1000)
# 
#   saveRDS(distPorts, paste0("Participation/SDM_code/port_dist_msqd_spawn/portDist_", paste0(as.character(j),".rds")))
# }


#----------------
# Market squid SDM

sdm.msqd <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   MSQD_SPAWN_SDM_30 = numeric(),
                   MSQD_SPAWN_SDM_90 = numeric(),
                   MSQD_SPAWN_SDM_220 = numeric())

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()


for (y in 2000:2019) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/squid_spawn/squidSpawn_", 
                                   paste0(as.character(m), 
                                          paste0("_", paste0(as.character(y),"_envelope.nc")))))
     
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      msqd.date.sdm <- ncdf4::ncvar_get(dat, "time")
      msqd.sdm <- ncdf4::ncvar_get(dat, "pred")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(msqd.sdm) <- list(set_long = set_long, set_lat = set_lat, msqd.date.sdm = msqd.date.sdm)
      sdmMelt <- reshape2::melt(msqd.sdm, value.name = "msqd.sdm")
      sdmMelt$msqd.date.sdm <- as.integer(sdmMelt$msqd.date.sdm)
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$msqd.date.sdm)	
      
      # Merge to distance from port area to a set of coordinates
      distPorts <- readRDS(paste0("Participation/SDM_code/port_dist_msqd/portDist_",
                                  paste0(as.character(j), ".rds")))
      sdmMelt <- merge(sdmMelt, distPorts, by = c('set_lat', 'set_long'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date)) 
      
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_30  <- sdmMelt %>% dplyr::filter(dist <= 30)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_90  <- sdmMelt %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_220 <- sdmMelt %>% dplyr::filter(dist <= 220) %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_30  <- mean(dat_prob_30$msqd.sdm, na.rm = TRUE)
        SDM_mean_90  <- mean(dat_prob_90$msqd.sdm, na.rm = TRUE)
        SDM_mean_220 <- mean(dat_prob_220$msqd.sdm, na.rm = TRUE)
        sdm.msqd <- sdm.msqd %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  MSQD_SPAWN_SDM_30  = SDM_mean_30, 
                  MSQD_SPAWN_SDM_90  = SDM_mean_90, 
                  MSQD_SPAWN_SDM_220 = SDM_mean_220)
        rm(dat_prob_30, dat_prob_90, dat_prob_220, 
           SDM_mean_30, SDM_mean_90, SDM_mean_220)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(sdm.msqd, file = "Participation/SDM_code/sdm_msqd_spawn.rds")
    }
  }
}