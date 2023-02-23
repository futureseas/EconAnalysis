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

##########################################################################################################################
##########################################################################################################################

datsU1 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_su_daily_20110102_20170419.nc")

lon <- ncdf4::ncvar_get(datsU1, "lon")
lat <- ncdf4::ncvar_get(datsU1, "lat")
year <- ncdf4::ncvar_get(datsU1, "year")
month <- ncdf4::ncvar_get(datsU1, "month")
day <- ncdf4::ncvar_get(datsU1, "day")
su <- ncdf4::ncvar_get(datsU1, "su")
ncdf4::nc_close(datsU1)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(su) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sUmelt1 <- reshape2::melt(su, value.name = "su") 
rm(lat, lon, su, year, month, day, datsU1)
gc()

sUmelt1$date <- as.Date("1970-01-01") + days(sUmelt1$date)
gc()

sUmelt1 <- sUmelt1 %>%
 mutate(LANDING_DAY = day(date)) %>%
 mutate(LANDING_MONTH = month(date)) %>%
 mutate(LANDING_YEAR = year(date))
sUmelt1 <- sUmelt1 %>% select(-c("date"))
gc()

#--------------------------------------
datsU2 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_su_daily_20170420_20180731.nc")

lon <- ncdf4::ncvar_get(datsU2, "lon")
lat <- ncdf4::ncvar_get(datsU2, "lat")
year <- ncdf4::ncvar_get(datsU2, "year")
month <- ncdf4::ncvar_get(datsU2, "month")
day <- ncdf4::ncvar_get(datsU2, "day")
su <- ncdf4::ncvar_get(datsU2, "su")
ncdf4::nc_close(datsU2)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(su) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sUmelt2 <- reshape2::melt(su, value.name = "su") 

rm(lat, lon, su, year, month, day, datsU2)
gc()

sUmelt2$date <- as.Date("1970-01-01") + days(sUmelt2$date)
gc()

sUmelt2 <- sUmelt2 %>%
  mutate(LANDING_DAY = day(date)) %>%
  mutate(LANDING_MONTH = month(date)) %>%
  mutate(LANDING_YEAR = year(date))
sUmelt2 <- sUmelt2 %>% select(-c("date"))
gc()

#-------------------------------------
datsU3 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_su_daily_20180801_20190815.nc")

lon <- ncdf4::ncvar_get(datsU3, "lon")
lat <- ncdf4::ncvar_get(datsU3, "lat")
year <- ncdf4::ncvar_get(datsU3, "year")
month <- ncdf4::ncvar_get(datsU3, "month")
day <- ncdf4::ncvar_get(datsU3, "day")
su <- ncdf4::ncvar_get(datsU3, "su")
ncdf4::nc_close(datsU3)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(su) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sUmelt3 <- reshape2::melt(su, value.name = "su") 

rm(lat, lon, su, year, month, day, datsU3)
gc()

sUmelt3$date <- as.Date("1970-01-01") + days(sUmelt3$date)
gc()

sUmelt3 <- sUmelt3 %>%
  mutate(LANDING_DAY = day(date)) %>%
  mutate(LANDING_MONTH = month(date)) %>%
  mutate(LANDING_YEAR = year(date))
sUmelt3 <- sUmelt3 %>% select(-c("date"))


sUmelt <- rbind(sUmelt1, sUmelt2, sUmelt3)
rm(sUmelt1, sUmelt2, sUmelt3)

##########################################################################################################################
##########################################################################################################################

datsV1 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_sv_daily_20110102_20170419.nc")

lon <- ncdf4::ncvar_get(datsV1, "lon")
lat <- ncdf4::ncvar_get(datsV1, "lat")
year <- ncdf4::ncvar_get(datsV1, "year")
month <- ncdf4::ncvar_get(datsV1, "month")
day <- ncdf4::ncvar_get(datsV1, "day")
sv <- ncdf4::ncvar_get(datsV1, "sv")
ncdf4::nc_close(datsV1)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(sv) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sVmelt1 <- reshape2::melt(sv, value.name = "sv") 
rm(lat, lon, sv, year, month, day, datsV1)
gc()

sVmelt1$date <- as.Date("1970-01-01") + days(sVmelt1$date)
gc()

sVmelt1 <- sVmelt1 %>%
  mutate(LANDING_DAY = day(date)) %>%
  mutate(LANDING_MONTH = month(date)) %>%
  mutate(LANDING_YEAR = year(date))
sVmelt1 <- sVmelt1 %>% select(-c("date"))
gc()


#--------------------------------------
datsV2 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_sv_daily_20170420_20180731.nc")

lon <- ncdf4::ncvar_get(datsV2, "lon")
lat <- ncdf4::ncvar_get(datsV2, "lat")
year <- ncdf4::ncvar_get(datsV2, "year")
month <- ncdf4::ncvar_get(datsV2, "month")
day <- ncdf4::ncvar_get(datsV2, "day")
sv <- ncdf4::ncvar_get(datsV2, "sv")
ncdf4::nc_close(datsV2)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(sv) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sVmelt2 <- reshape2::melt(sv, value.name = "sv") 

rm(lat, lon, sv, year, month, day, datsV2)
gc()

sVmelt2$date <- as.Date("1970-01-01") + days(sVmelt2$date)
gc()

sVmelt2 <- sVmelt2 %>%
  mutate(LANDING_DAY = day(date)) %>%
  mutate(LANDING_MONTH = month(date)) %>%
  mutate(LANDING_YEAR = year(date))
sVmelt2 <- sVmelt2 %>% select(-c("date"))
gc()

#-------------------------------------
datsV3 <- ncdf4::nc_open("C:/Data/Wind&Current/ROMS u-v/wcnrt_sv_daily_20180801_20190815.nc")

lon <- ncdf4::ncvar_get(datsV3, "lon")
lat <- ncdf4::ncvar_get(datsV3, "lat")
year <- ncdf4::ncvar_get(datsV3, "year")
month <- ncdf4::ncvar_get(datsV3, "month")
day <- ncdf4::ncvar_get(datsV3, "day")
sv <- ncdf4::ncvar_get(datsV3, "sv")
ncdf4::nc_close(datsV3)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(sv) <- list(lon = lon[,1], lat = lat[1,], date = as.Date(paste0(year,paste0("-",paste0(month,paste0("-"),day)))))
sVmelt3 <- reshape2::melt(sv, value.name = "sv") 

rm(lat, lon, sv, year, month, day, datsV3)
gc()

sVmelt3$date <- as.Date("1970-01-01") + days(sVmelt3$date)
gc()

sVmelt3 <- sVmelt3 %>%
  mutate(LANDING_DAY = day(date)) %>%
  mutate(LANDING_MONTH = month(date)) %>%
  mutate(LANDING_YEAR = year(date))
sVmelt3 <- sVmelt3 %>% select(-c("date"))


sVmelt <- rbind(sVmelt1, sVmelt2, sVmelt3)
rm(sVmelt1, sVmelt2, sVmelt3)

##########################################################################################################################
##########################################################################################################################


# Get distance by coordinate to port j and save it
gc()
coord <- sUmelt %>% dplyr::select(c(lat, lon)) %>% unique()
port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

for (j in 1:nrow(port_area_coord)) {
  distPorts <- coord %>%
    mutate(dist = by(., 1:nrow(.), function(row) {
      distHaversine(c(row$lon, row$lat), c(port_area_coord[j,]$lon, port_area_coord[j,]$lat))
    })) %>%
    mutate(dist = dist / 1000)
  
  write.csv(distPorts, paste0("C:/Data/Wind&Current/port_dist/portDist_currents_",
                              paste0(as.character(j), ".csv")), row.names = FALSE)
}



#------------------------------------------------------      
## Merge data and save it by month

for (y in 2011:2019) {
  for (m in 1:12) {
    gc()
    sU <- sUmelt %>% filter(LANDING_YEAR == y) %>% filter(LANDING_MONTH == m)    
    sV <- sVmelt %>% filter(LANDING_YEAR == y) %>% filter(LANDING_MONTH == m)
    currents <- merge(sU, sV, by = c("lat", "lon", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"))      
    write.csv(currents, paste0("C:/Data/Wind&Current/monthly_data/currents/currents_",
                           paste0(as.character(m),
                                  paste0("_",
                                         paste0(as.character(y),
                                                ".csv")))), row.names = FALSE)
    rm(sU, sV, currents)
  }
}


#-------------------------------------------------------
## Calculate wind at each port every day

rm(list=ls())
gc()

currents_means <- tibble(LANDING_YEAR = integer(),
                         LANDING_MONTH = integer(),
                         LANDING_DAY = integer(),
                         PORT_AREA_CODE = character(),
                         currentsV_30 = numeric(),
                         currentsV_90 = numeric(),
                         currentsV_220 = numeric(),
                         currentsU_30 = numeric(),
                         currentsU_90 = numeric(),
                         currentsU_220 = numeric())

port_area_coord <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_areas.csv") %>% drop_na()

# for (y in 2000:2020) {
#   for (m in 1:12) {
y=2000
m=1
for (j in 1:nrow(port_area_coord)) {
  distP <- read_csv(paste0("C:/Data/Wind&Current/port_dist/portDist_",
                               paste0(as.character(j), ".csv")))
  currents <- read_csv(paste0("C:/Data/Wind&Current/monthly_data/currents/currents_", 
                          paste0(as.character(m),
                                 paste0("_",   
                                        paste0(as.character(y),
                                               ".csv")))))
  
  currents_port <- merge(currents, distP, by = c('lat', 'lon'), all.x = TRUE, all.y = FALSE) 
  
  # Calculate daily SDM level within port radius
  for (z in 1:max(currents_port$LANDING_DAY)) {
    currents_30  <- currents_port %>% dplyr::filter(dist <= 30)  %>% dplyr::filter(LANDING_DAY == z)
    currents_90  <- currents_port %>% dplyr::filter(dist <= 90)  %>% dplyr::filter(LANDING_DAY == z)
    currents_220 <- currents_port %>% dplyr::filter(dist <= 220) %>% dplyr::filter(LANDING_DAY == z)
    currentsV_mean_30  <- mean(currents_30$Vwind, na.rm = TRUE)
    currentsV_mean_90  <- mean(currents_90$Vwind, na.rm = TRUE)
    currentsV_mean_220 <- mean(currents_220$Vwind, na.rm = TRUE)
    currentsU_mean_30  <- mean(currents_30$Uwind, na.rm = TRUE)
    currentsU_mean_90  <- mean(currents_90$Uwind, na.rm = TRUE)
    currentsU_mean_220 <- mean(currents_220$Uwind, na.rm = TRUE)
    
    currents_means <- currents_means %>%
      add_row(LANDING_YEAR = y, 
              LANDING_MONTH = m, 
              LANDING_DAY = z, 
              PORT_AREA_CODE = as.character(port_area_coord[j, 1]), 
              currentsV_30  = currentsV_mean_30, 
              currentsV_90  = currentsV_mean_90, 
              currentsV_220 = currentsV_mean_220, 
              currentsU_30  = currentsU_mean_30, 
              currentsU_90  = currentsU_mean_90, 
              currentsU_220 = currentsU_mean_220)
    rm(currents_30, currents_90, currents_220, 
       currentsU_mean_30, currentsU_mean_90, currentsU_mean_220,
       currentsV_mean_30, currentsV_mean_90, currentsV_mean_220)
  }
  print(paste("Year:", y, "; month:", m, "--", "Port area:",j))
  readr::write_csv(currents_means, file = "C:/Data/Wind&Current/currents_U_V_2011-2019.csv")
}
