#############################################
## Calculate SDM outputs to PORT_AREA_CODE ##
#############################################

### Try 30, 90, 200

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)

# Get port area coordinates
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>%
  drop_na()


#----------------
# Pacific sardine

sdm.cmck <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   LANDING_DAY = integer(),
                   PORT_AREA_CODE = character(),
                   CMCK_SDM = integer())

for (y in 2000:2018) {
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
      
      sdmMelt <- sdmMelt %>%
        group_by(set_lat, set_long, set_date) %>%
        summarize(exp_prob = mean(cmck.sdm, na.rm = T))	%>%
        ungroup(.)  
      sdmMelt <- sdmMelt %>% group_by(set_lat, set_long) %>%
        dplyr::mutate(ID_location = cur_group_id()) %>%
        ungroup(.) 
      
      # Calculate distance from port area to a set of coordinates (using ID location)
      dist_to_ID <- sdmMelt %>% select(c(set_long, set_lat, ID_location)) %>% unique() %>%
        mutate(dist = by(., 1:nrow(.), function(row) {
          distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
        })) %>%
        mutate(dist = dist / 1000) %>% select(-c(set_long, set_lat))
      
      sdmMelt <- merge(sdmMelt, dist_to_ID, by = c('ID_location'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date)) %>% dplyr::filter(dist <= 200)
      
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob <- sdmMelt %>% dplyr::filter(LANDING_DAY == 28)  
        SDM_mean = mean(dat_prob$exp_prob, na.rm = TRUE)
        sdm.cmck <- sdm.cmck %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  CMCK_SDM = SDM_mean)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      readr::write_csv(sdm.cmck, file = "Participation/SDM_code/sdm.cmck.csv")
    }
  }
}

### Add 2019 data until august
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      
      # Open the monthly file that contain SDM by location
      dat <- ncdf4::nc_open(paste0("G:/My Drive/Data/SDM/chub/chub_",
                                   paste0(as.character(m),
                                          paste0("_2019_GAM.nc"))))
      
      set_long <- ncdf4::ncvar_get(dat, "lon")
      set_lat <- ncdf4::ncvar_get(dat, "lat")
      cmck.date.sdm <- ncdf4::ncvar_get(dat, "time")
      cmck.sdm <- ncdf4::ncvar_get(dat, "predGAM")
      ncdf4::nc_close(dat)			
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(cmck.sdm) <- list(set_long = set_long, set_lat = set_lat, cmck.date.sdm = cmck.date.sdm)
      sdmMelt <- reshape2::melt(cmck.sdm, value.name = "cmck.sdm")
      sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$cmck.date.sdm)	
      
      sdmMelt <- sdmMelt %>%
        group_by(set_lat, set_long, set_date) %>%
        summarize(exp_prob = mean(cmck.sdm, na.rm = T))	%>%
        ungroup(.)  
      sdmMelt <- sdmMelt %>% group_by(set_lat, set_long) %>%
        dplyr::mutate(ID_location = cur_group_id()) %>%
        ungroup(.) 
      
      # Calculate distance from port area to a set of coordinates (using ID location)
      dist_to_ID <- sdmMelt %>% select(c(set_long, set_lat, ID_location)) %>% unique() %>%
        mutate(dist = by(., 1:nrow(.), function(row) {
          distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
        })) %>%
        mutate(dist = dist / 1000) %>% select(-c(set_long, set_lat))
      
      sdmMelt <- merge(sdmMelt, dist_to_ID, by = c('ID_location'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(LANDING_DAY = day(set_date)) %>% dplyr::filter(dist <= 200)
      
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob <- sdmMelt %>% dplyr::filter(LANDING_DAY == 28)  
        SDM_mean = mean(dat_prob$exp_prob, na.rm = TRUE)
        sdm.cmck <- sdm.cmck %>%
          add_row(LANDING_YEAR = 2019, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  CMCK_SDM = SDM_mean)
      }
      print(paste("Year:", 2019, "; month:", m, "--", "Port area:",j))
      readr::write_csv(sdm.cmck, file = "Participation/SDM_code/sdm.cmck.csv")
    }
  }