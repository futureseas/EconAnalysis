######################################
## Calculate wind to PORT_AREA_CODE ##
######################################

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


wind <- tibble(LANDING_YEAR = integer(),
               LANDING_MONTH = integer(),
               LANDING_DAY = integer(),
               PORT_AREA_CODE = character(),
               wind_30 = numeric(),
               wind_90 = numeric(),
               wind_220 = numeric())

     


 datU <- ncdf4::nc_open(paste0("C:/Data/Desiree_Felipe_Wind_Current/Uwind_ERA5_1980-2020.nc"))
      names(datU$var)
      
      lon <- ncdf4::ncvar_get(datU, "lon")
      lat <- ncdf4::ncvar_get(datU, "lat")
      date <- ncdf4::ncvar_get(datU, "wind_time")
      Uwind <- ncdf4::ncvar_get(datU, "Uwind")
      ncdf4::nc_close(datU)		
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(Uwind) <- list(lon = lon[,1], lat = lat[1,], date = date)
      UwindMelt <- reshape2::melt(Uwind, value.name = "Uwind")
      rm(lat, lon, date, Uwind)
      UwindMelt$date <- as.integer(UwindMelt$date)
      gc()
      
      UwindMelt <- UwindMelt %>%
        group_by(lat, lon, date) %>%
        summarize(exp_Uwind = mean(Uwind, na.rm = T))	%>%
        ungroup(.)

#------------------------------------------------------              
gc()    
datV <- ncdf4::nc_open(paste0("C:/Data/Desiree_Felipe_Wind_Current/Vwind_ERA5_1980-2020.nc"))
      names(datV$var)
      
      lon <- ncdf4::ncvar_get(datV, "lon")
      lat <- ncdf4::ncvar_get(datV, "lat")
      date <- ncdf4::ncvar_get(datV, "wind_time")
      Vwind <- ncdf4::ncvar_get(datV, "Uwind")
      ncdf4::nc_close(datV)		
      
      # Reshape the 3D array so we can map it, change the time field to be date
      dimnames(Vwind) <- list(lon = lon[,1], lat = lat[1,], date = date)
      VwindMelt <- reshape2::melt(Vwind, value.name = "Vwind")
      rm(lat, lon, date, Vwind)
      VwindMelt$date <- as.integer(VwindMelt$date)
      gc()
      
      VwindMelt <- VwindMelt %>%
        group_by(lat, lon, date) %>%
        summarize(exp_Vwind = mean(Vwind, na.rm = T))	%>%
        ungroup(.)
      
#------------------------------------------------------      
## Merge data
      
wind <- merge(VwindMelt, UwindMelt, by = c("lat", "lon", "date"))      
      
      
#------------------------------------------------------      
      
      
      
      
      UwindMelt <- UwindMelt %>% group_by(lat, lon) %>%
        dplyr::mutate(ID_location = cur_group_id()) %>%
        ungroup(.) 
      
      
      
      
      
      
      
      
      # Calculate distance from port area to a set of coordinates (using ID location)
      for (j in 1:nrow(port_area_coord)) {
        
      dist_to_ID <- sdmMelt %>% select(c(set_long, set_lat, ID_location)) %>% unique() %>%
        mutate(dist = by(., 1:nrow(.), function(row) {
          distHaversine(c(row$set_long, row$set_lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
        })) %>%
        mutate(dist = dist / 1000) %>% select(-c(set_long, set_lat))
      
      
      sdmMelt <- merge(sdmMelt, dist_to_ID, by = c('ID_location'), all.x = TRUE, all.y = FALSE) %>% 
        mutate(date = as.Date("1900-01-01") + days(UwindMelt$date)) %>%
        mutate(LANDING_DAY = day(set_date)) %>%
        mutate(LANDING_MONTH = day(set_date)) %>%
        mutate(LANDING_YEAR = day(set_date)) 
      
      
      # Calculate daily SDM level within port radius
      for (z in 1:max(sdmMelt$LANDING_DAY)) {
        dat_prob_30  <- sdmMelt %>% dplyr::filter(dist <= 30)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_90  <- sdmMelt %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        dat_prob_220 <- sdmMelt %>% dplyr::filter(dist <= 220) %>% dplyr::filter(LANDING_DAY == z)
        SDM_mean_30  <- mean(dat_prob_30$exp_prob, na.rm = TRUE)
        SDM_mean_90  <- mean(dat_prob_90$exp_prob, na.rm = TRUE)
        SDM_mean_220 <- mean(dat_prob_220$exp_prob, na.rm = TRUE)
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
      readr::write_csv(sdm.msqd, file = "Participation/SDM_code/sdm_msqd_spawn.csv")
 }
