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


# #------------------------------------------------------         
# datU <- ncdf4::nc_open(paste0("C:/Data/Wind&Current/Uwind_ERA5_1980-2020.nc"))
#   names(datU$var)
#   lon <- ncdf4::ncvar_get(datU, "lon")
#   lat <- ncdf4::ncvar_get(datU, "lat")
#   date <- ncdf4::ncvar_get(datU, "wind_time")
#   Uwind <- ncdf4::ncvar_get(datU, "Uwind")
#   ncdf4::nc_close(datU)
#   
#   # Reshape the 3D array so we can map it, change the time field to be date
#   dimnames(Uwind) <- list(lon = lon[,1], lat = lat[1,], date = as.integer(date))
#   UwindMelt <- reshape2::melt(Uwind, value.name = "Uwind") %>% filter(date >= 36524)
#   rm(lat, lon, date, Uwind, datU)
#   gc()
#   UwindMelt$date <- as.Date("1900-01-01") + days(UwindMelt$date)
#   str(UwindMelt)
#   gc()
#   UwindMelt <- UwindMelt %>%
#     mutate(LANDING_DAY = day(date)) %>%
#     mutate(LANDING_MONTH = month(date)) %>%
#     mutate(LANDING_YEAR = year(date))
#   UwindMelt <- UwindMelt %>% select(-c("date"))
#   gc()
# 
# 
#------------------------------------------------------
# datV <- ncdf4::nc_open(paste0("C:/Data/Wind&Current/Vwind_ERA5_1980-2020.nc"))
#   names(datV$var)
# 
#   lon <- ncdf4::ncvar_get(datV, "lon")
#   lat <- ncdf4::ncvar_get(datV, "lat")
#   date <- ncdf4::ncvar_get(datV, "wind_time")
#   Vwind <- ncdf4::ncvar_get(datV, "Vwind")
#   ncdf4::nc_close(datV)
# 
#   # Reshape the 3D array so we can map it, change the time field to be date
#   dimnames(Vwind) <- list(lon = lon[,1], lat = lat[1,], date = as.integer(date))
#   VwindMelt <- reshape2::melt(Vwind, value.name = "Vwind") %>% filter(date >= 36524)
#   rm(lat, lon, date, Vwind, datV)
#   gc()
#   VwindMelt$date <- as.Date("1900-01-01") + days(VwindMelt$date)
#   str(VwindMelt)
#   gc()
#   VwindMelt <- VwindMelt %>%
#     mutate(LANDING_DAY = day(date)) %>%
#     mutate(LANDING_MONTH = month(date)) %>%
#     mutate(LANDING_YEAR = year(date))
#   VwindMelt <- VwindMelt %>% select(-c("date"))
#   gc()
# 
#   coord <- VwindMelt %>% filter(LANDING_YEAR == 2010) %>% filter(LANDING_MONTH == 6) 
#   coord <- coord %>% dplyr::select(c(lat, lon)) %>% unique()
#   saveRDS(coord, "C:/Data/Wind&Current/coord_wind.rds")
# 
# #------------------------------------------------------      
# ## Merge data and save it by month
#   
# for (y in 2002:2020) {
#   for (m in 1:12) {
#     gc()
#     Uwind <- UwindMelt %>% filter(LANDING_YEAR == y) %>% filter(LANDING_MONTH == m)    
#     Vwind <- VwindMelt %>% filter(LANDING_YEAR == y) %>% filter(LANDING_MONTH == m)
#     wind <- merge(Uwind, Vwind, by = c("lat", "lon", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"))      
#     write.csv(wind, paste0("C:/Data/Wind&Current/monthly_data/wind_",
#                            paste0(as.character(m),
#                                   paste0("_",
#                                          paste0(as.character(y),
#                                                 ".csv")))), row.names = FALSE)
#     rm(Uwind, Vwind, wind)
#   }
# }


#------------------------------------------------------  
# Get distance by coordinate to port j and save it

# coord <- readRDS("C:/Data/Wind&Current/coord_wind.rds")  
# port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()
# 
# for (j in 1:nrow(port_area_coord)) {
#   distPorts <- coord %>%
#     mutate(dist = by(., 1:nrow(.), function(row) {
#       distHaversine(c(row$lon, row$lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
#     })) %>%
#     mutate(dist = dist / 1000)
#   
#   saveRDS(distPorts, paste0("C:/Data/Wind&Current/port_dist/portDist_", paste0(as.character(j),".rds")))
# }


#-------------------------------------------------------
## Calculate wind at each port every day

rm(list=ls())
gc()

wind_means <- tibble(LANDING_YEAR = integer(),
                     LANDING_MONTH = integer(),
                     LANDING_DAY = integer(),
                     PORT_AREA_CODE = character(),
                     windV_30 = numeric(),
                     windV_90 = numeric(),
                     windV_220 = numeric(),
                     windU_30 = numeric(),
                     windU_90 = numeric(),
                     windU_220 = numeric())

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()


for (y in 2000:2020) {
  for (m in 1:12) {
    for (j in 1:nrow(port_area_coord)) {
      
      distPorts <- readRDS(paste0("C:/Data/Wind&Current/port_dist/portDist_",
                                paste0(as.character(j), ".rds")))
      
      wind <- read.csv(paste0("C:/Data/Wind&Current/monthly_data/wind_", 
                            paste0(as.character(m),
                                   paste0("_",   
                                          paste0(as.character(y),
                                                 ".csv")))))
    
      wind_port <- merge(wind, distPorts, by = c('lat', 'lon'), all.x = TRUE, all.y = FALSE) 
    
      # Calculate daily wind within port radius
      for (z in 1:max(wind_port$LANDING_DAY)) {
        wind_30  <- wind_port %>% dplyr::filter(dist <= 30)  %>% dplyr::filter(LANDING_DAY == z)
        wind_90  <- wind_port %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
        wind_220 <- wind_port %>% dplyr::filter(dist <= 220) %>% dplyr::filter(LANDING_DAY == z)
        windV_mean_30  <- mean(wind_30$Vwind, na.rm = TRUE)
        windV_mean_90  <- mean(wind_90$Vwind, na.rm = TRUE)
        windV_mean_220 <- mean(wind_220$Vwind, na.rm = TRUE)
        windU_mean_30  <- mean(wind_30$Uwind, na.rm = TRUE)
        windU_mean_90  <- mean(wind_90$Uwind, na.rm = TRUE)
        windU_mean_220 <- mean(wind_220$Uwind, na.rm = TRUE)
        
        wind_means <- wind_means %>%
          add_row(LANDING_YEAR = y, 
                  LANDING_MONTH = m, 
                  LANDING_DAY = z, 
                  PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
                  windV_30  = windV_mean_30, 
                  windV_90  = windV_mean_90, 
                  windV_220 = windV_mean_220, 
                  windU_30  = windU_mean_30, 
                  windU_90  = windU_mean_90, 
                  windU_220 = windU_mean_220)
        rm(wind_30, wind_90, wind_220, 
           windU_mean_30, windU_mean_90, windU_mean_220,
           windV_mean_30, windV_mean_90, windV_mean_220)
      }
      print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
      saveRDS(wind_means, file = "C:/Data/Wind&Current/wind_U_V_2000-2020.RDS")
    }
  }
}
 
