################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
memory.limit(9999999999)
min.year = 2013
max.year = 2016

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

## Functions ##
deg2rad <- function(deg) {
  m <- deg * (pi/180)
  return(m)
}

getDistanceFromLatLonInKm <- function(lat1,lon1,lat2,lon2) {
  R <- 6371
  dLat <- deg2rad(lat2-lat1)
  dLon <- deg2rad(lon2-lon1)
  a <- sin(dLat/2) * sin(dLat/2) +  cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}


#-----------------------------------------------------------------------------

## Read logbooks available for Pacific Sardine from ODFW (2013-2015) ##
psdn.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(set_lat = Lat + LatMin/60) %>%
  mutate(set_long = Long + LongMin/60) %>%
  mutate(fleet_name="OR") %>%
  mutate(species="PSDN") %>%
  dplyr::rename(drvid = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  mutate(set_day = day(set_date)) %>%
  dplyr::rename(effort = Sard) %>%
  dplyr::rename(haul_num = Set) %>%
  dplyr::rename(trip_id = Ticket) %>%
  dplyr::rename(depth = Depth) %>%
  filter(set_year >= min.year) %>%
  filter(set_year <= max.year) %>%
  filter(trip_id != "No ticket") 

logbooks <- psdn.logbook.OR %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))


## Read logbooks available for market squid from ODFW (2016) ##
msqd.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(fleet_name="OR") %>%
  mutate(species="MSQD") %>%
  dplyr::rename(set_lat = Lat) %>%
  dplyr::rename(set_long = Long) %>%
  dplyr::rename(drvid = FedDoc) %>%
  dplyr::rename(BoatName = "Boat Name") %>%  
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(effort = `Squid (Lbs)`) %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))
logbooks <- rbind(logbooks, msqd.logbook.OR)


## Read logbooks available for Northern anchovy from ODFW (2015-2016) ##
nanc.logbook.OR.2016 <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Anchovy logbooks.xlsx", sheet = "2016") 
  nanc.logbook.OR.2016 <- nanc.logbook.OR.2016 %>% dplyr::select(c("Boat Name", "FedDoc", "Date", "Set", "Time", "Fathoms",
                                                                   "Temp", "Long (D)", "Long (Decimal Min)", "Lat (D)", 
                                                                   "Lat (DM)", "lbs anchovy", "Ticket"))
  nanc.logbook.OR <- readxl::read_excel("C:\\Data\\ODFW CPS logbooks\\Anchovy logbooks.xlsx", sheet = "2015") 
  nanc.logbook.OR <- nanc.logbook.OR %>% mutate(Ticket = "No ticket") %>% dplyr::select(-c("lbs sardine", "lbs mackerel"))
  
nanc.logbook.OR <- rbind(nanc.logbook.OR, nanc.logbook.OR.2016)
  nanc.logbook.OR <- nanc.logbook.OR %>%
    dplyr::rename("lat_D" = "Lat (D)") %>% dplyr::rename("lat_min" = "Lat (DM)") %>%
    mutate(set_lat = lat_D + lat_min/60) %>%
    dplyr::rename("lon_D" = "Long (D)") %>% dplyr::rename("lon_min" = "Long (Decimal Min)")  %>% 
    mutate(set_long = lon_D + lon_min/60) %>%
    mutate(fleet_name="OR") %>%
  dplyr::rename(drvid = FedDoc) %>%
  dplyr::rename('BoatName' = "Boat Name") %>%
  mutate(species="NANC") %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(effort = `lbs anchovy`) %>%
  dplyr::select(c('set_lat', 'set_long', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))

logbooks <- rbind(logbooks, nanc.logbook.OR)

## Read logbooks available for Market squid from CDFW (2013-2016) ##

  ## California
  
  sqd.logbook.vessel.CA <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
    dplyr::rename(set_lat = SetLatitude) %>%
    dplyr::rename(set_long = SetLongitude) %>%
    mutate(fleet_name="CA") %>%
    mutate(species="MSQD") %>%
    mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
    mutate(set_year = year(set_date)) %>%
    mutate(set_month = month(set_date)) %>%
    dplyr::rename(effort = "CatchEstimate") %>%
    dplyr::rename(drvid = "VesselID") %>%
    mutate(BoatName = NA) %>%
    filter(set_year >= min.year) %>%
    filter(set_year <= max.year) %>%
    dplyr::select(c('set_lat', 'set_long', 'drvid', 'fleet_name', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))
  logbooks <- rbind(logbooks, sqd.logbook.vessel.CA)
  
  # sqd.logbook.light.brail.CA <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  #   dplyr::rename(set_lat = Lat_DD) %>%
  #   dplyr::rename(set_long = Long_DD) %>%
  #   mutate(fleet_name="CA Light Boat") %>%
  #   mutate(species="MSQD") %>%
  #   mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  #   mutate(set_year = year(set_date)) %>%
  #   mutate(set_month = month(set_date)) %>%
  #   dplyr::rename(effort = "ElapsedTime") %>%
  #   dplyr::rename(drvid = "VesselID") %>%
  #   mutate(BoatName = NA) %>%
  #   filter(set_year >= min.year) %>%
  #   filter(set_year <= max.year) %>%
  #   dplyr::select(c('set_lat', 'set_long', 'drvid', 'fleet_name', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))
  # logbooks <- rbind(logbooks, sqd.logbook.light.brail.CA)
  
  
## Read logbooks available for Pacific sardine from WDFW (2013-2014) ##
  
  psdn.logbook.WA <- readxl::read_excel("C:\\Data\\WDFW CPS logbooks\\WA Sardine Logbook Flatfile Data Reques 20-15485.xlsx") %>%
    mutate(Lat = `Latitude Degrees` + `Latitude Minutes`/60) %>%
    mutate(Long = `Longitude Degrees` + `Longitude Minutes`/60) %>%
    mutate(fleet_name="WA") %>%
    mutate(species="PSDN") %>%
    dplyr::rename(set_lat = Lat) %>%
    dplyr::rename(set_long = Long) %>%
    dplyr::rename(BoatName = `Vessel`) %>%
    mutate(set_date = as.Date(`Fishing Date`,format="%Y-%m-%d")) %>%
    mutate(set_year = year(set_date)) %>%
    mutate(set_month = month(set_date)) %>%
    dplyr::rename(effort = `Sardine Retained mt`) %>%
    filter(set_year >= min.year) %>%
    mutate(drvid = 0000) %>%
    dplyr::select(c('set_lat', 'set_long', 'drvid', 'fleet_name', 'set_year', 'set_month', 'set_date', 'effort', 'BoatName', 'species'))
  logbooks <- rbind(logbooks, psdn.logbook.WA)
  
  # DATABASE
  logbooks <- logbooks[-which(is.na(logbooks$set_lat)), ]
    logbooks$set_long <- with(logbooks, ifelse(set_long > 0, -set_long, set_long))
  
#-----------------------------------------------------------------------------
# Use Vessel ID to obtain MMSI

#' Scrapes call sign and mmsi from USCG NAIS website using list of US Official numbers
#' from the CPS permit data I collected earlier.
#'
#
# library(rvest)
# library(curl)
#
# Use IDs from logbooks available # 
#
# ## California
# id.CA <- logbooks %>% dplyr::filter(fleet_name == "CA") %>% 
#   select(drvid)  %>%
#   unique()
# 
# id.CA$mmsi <- "NA"
# 
# for(i in 1:nrow(id.CA)) {
#   url <- str_c("https://www.navcen.uscg.gov/aisSearch/dbo_aisVessels_list.php?q=(US%20Official%20No~contains~", id.CA$drvid[i], ")&f=all#skipdata")
#   html.table <- read_html(url) %>%
#     html_node("table") %>%
#     html_table(fill = T) %>%
#     slice(13:14)
#   names(html.table) <- html.table[1,]
#   html.table <- html.table[-1,]
#   if(is.null(html.table$`MMSI:`) == T) {
#     id.CA$mmsi[i] <- "NA"
#   }
#   else {
#     id.CA$mmsi[i] <- html.table$'MMSI:'
#   }
# }
# write.csv(id.CA,"id_mmsi_CA.csv", row.names = FALSE)
# 
# 
# ## Oregon
# id.OR <- logbooks %>% dplyr::filter(fleet_name == "OR") %>% 
#   select(drvid, BoatName)  %>%
#   unique() %>% drop_na()
# id.OR$mmsi <- "NA"
# 
# for(i in 1:nrow(id.OR)) {
#  url <- str_c("https://www.navcen.uscg.gov/aisSearch/dbo_aisVessels_list.php?q=(US%20Official%20No~contains~", id.OR$drvid[i], ")&f=all#skipdata")
#   html.table <- read_html(url) %>%
#     html_node("table") %>%
#     html_table(fill = T) %>%
#     slice(13:14)
#    names(html.table) <- html.table[1,]
#   html.table <- html.table[-1,]
#  if(is.null(html.table$`MMSI:`) == T) {
#   id.OR$mmsi[i] <- "NA"
#  }
#  else {
#   id.OR$mmsi[i] <- html.table$'MMSI:'
#  }
# }
# write.csv(id.OR,"id_mmsi_OR.csv", row.names = FALSE)
# 
# 
# # Washington
# id.WA <- logbooks %>% dplyr::filter(fleet_name == "WA") %>% 
#   select(drvid, BoatName)  %>%
#   unique() %>% drop_na()
# 
# 
# # All
# id.all <- logbooks %>% select(drvid, BoatName, fleet_name) %>% unique()
#   id.all$mmsi <- "NA"
#   write.csv(id.all,"id_mmsi_all.csv", row.names = FALSE)
  

## Fix it manually using vessel.names from GFW databse
### (online: https://globalfishingwatch.org/map/)
  id.all.update <- readr::read_csv(here::here("id_mmsi_all_update.csv"))
  
#--------------------------------------------------------
# Load database from Global Fishing Watch... Add set variable (position within a day)

gfw.fishing.effort <- readr::read_csv(here::here("Data", "GFW_data", "GFW_westcoast_2013-2015.csv"))
  gfw.fishing.effort <- gfw.fishing.effort %>% filter(fishing_hours > 0) 
  gfw.fishing.effort$haul_num <- with(gfw.fishing.effort, ave(mmsi, mmsi, date, FUN = seq_along))

# vessel.names <- readr::read_csv(here::here("Data", "GFW_data", "MMSI_vessel_name.csv"))
# gfw.fishing.effort <- gfw.fishing.effort %>%
#   merge(y = vessel.names, by = "mmsi", all.x=TRUE)


# --------------------------------------------------------
# Merge GFW to logbook data
id.drvid <- id.all.update %>% select(mmsi, drvid) %>% drop_na() %>% unique() %>% dplyr::rename(mmsi_drvid = mmsi)
id.boatname <- id.all.update %>% select(mmsi, BoatName) %>% drop_na() %>% unique() 
  logbooks.mmsi <- logbooks %>% left_join(id.boatname, by = "BoatName")
  logbooks.mmsi <- logbooks.mmsi %>% left_join(id.drvid, by = "drvid") 
  logbooks.mmsi$mmsi <- ifelse(is.na(logbooks.mmsi$mmsi) & !is.na(logbooks.mmsi$mmsi_drvid), logbooks.mmsi$mmsi_drvid, logbooks.mmsi$mmsi)

logbooks.mmsi.day <- logbooks.mmsi %>% select(mmsi, set_date, species, fleet_name) %>% dplyr::rename(date = set_date) %>% mutate(dCPS = 1) %>% drop_na()
  
#---------------------------------------------------------
# Identify which vessels from GFW actually harvest PSDN or MSQD or ANCHOVY...
gfw.fishing.effort.CPS <- gfw.fishing.effort %>% 
  left_join(logbooks.mmsi.day, by = c("mmsi", "date")) %>% filter(dCPS == 1) %>% unique()
  

# #---------------------------------------------------------
# # How many trips GFW capture from logbooks? 
# 
# logbooks.compare <- logbooks.mmsi %>%  dplyr::rename(date = set_date) %>% group_by(BoatName, drvid, mmsi, date, fleet_name, species) %>%
#     summarise(across(c("set_lat", "set_long", "effort"), list(mean = mean, sum = sum), na.rm = TRUE)) %>% drop_na(mmsi)  # %>% filter(catch > 0)
#   
# gfw.compare <- gfw.fishing.effort %>% 
#   group_by(mmsi, date) %>%
#   summarise(across(c("cell_ll_lat", "cell_ll_lon", "fishing_hours"), list(mean = mean, sum = sum), na.rm = TRUE))
# 
# joint.compare <- logbooks.compare %>% left_join(gfw.compare, by = c("mmsi", "date")) 
# 
# ## 150 observations of 677 ~ 22% of logbooks... missing ~50% of MMSI
# 
# 
# # --------------------------------------------------------
# # How good is GFW compared to logbooks? (R: ~ 22 km average deviation).
# 
# joint.compare <- joint.compare %>% drop_na(cell_ll_lat_mean)
#   joint.compare$id_seq <- seq(1, nrow(joint.compare))
# 
# 
# joint.compare$dist <- getDistanceFromLatLonInKm(joint.compare$set_lat_mean, joint.compare$set_long_mean, 
#                                                joint.compare$cell_ll_lat_mean, joint.compare$cell_ll_lon_mean)
# 
# summary(joint.compare$dist)
# hist(joint.compare$dist)
# 
# joint.hist <- joint.compare %>% filter(dist<200)
#   hist(joint.hist$dist)
# 
# joint.hist <- joint.compare %>% filter(dist<80)
#   hist(joint.hist$dist)
# 
# joint.hist <- joint.compare %>% filter(dist<20)
#   hist(joint.hist$dist)
#   
#   
# 
# ## Create map of deviations
#   
#   library(maps)
#   library(geosphere)
#   library(magrittr)
#   library(plotly)
#   library(dplyr)
# 
#   joint.map <- joint.compare  %>% filter(dist > 11) %>% filter(dist < 220)
#   
#   
#   # map projection
#   
#   g <- list(
#     scope = 'usa',
#     projection = list(type = 'albers usa'),
#     showland = TRUE,
#     landcolor = toRGB("gray95"),
#     subunitcolor = toRGB("gray85"),
#     countrycolor = toRGB("gray85"),
#     countrywidth = 0.5,
#     subunitwidth = 0.5,
#     showocean = TRUE,
#     oceancolor = toRGB("blue")
#   )
#   
#   fig <- plot_geo() %>%
#     layout(geo = g, legend = list(x = 0.9, y = 0.1)) %>% 
#     add_markers(data = joint.map, x = ~set_long_mean, y = ~set_lat_mean, 
#                 size = ~dist, text = ~paste(paste(paste(joint.map$dist, " km; "),"MMSI: "), joint.map$mmsi), 
#                 hoverinfo = "text", name = "Loogbook location") %>% 
#     add_markers(data = joint.map, x = ~cell_ll_lon_mean, y = ~cell_ll_lat_mean,
#                 size = ~dist, text = ~paste(paste(paste(joint.map$dist, " km; "),"MMSI: "), joint.map$mmsi), 
#                 hoverinfo = "text", name = "GFW location") %>% 
#     add_segments(x = ~set_long_mean, xend = ~cell_ll_lon_mean,
#                  y = ~set_lat_mean, yend = ~cell_ll_lat_mean, 
#                  color = I("gray"), text = ~paste(paste(paste(joint.map$dist, " km; "),"MMSI: "), 
#                  joint.map$mmsi),  opacity = 0.3, hoverinfo = "text", name = "Error (km)")
#     fig
#   
# 
# 
# 
# # How effort is related between GFW and logbooks?
#   coeff <- 25000
#   gfwColor <- "#69b3a2"
#   logbooksColor <- rgb(0.2, 0.6, 0.9, 1)
#   
#   ggplot(joint.compare, aes(id_seq)) +
#     geom_line( aes(y= fishing_hours_sum), size=1, color=gfwColor) + 
#     geom_line( aes(y=  effort_sum / coeff), size=1, color=logbooksColor)  +
#     scale_y_continuous(name = "GFW Fishing hours",
#                        sec.axis = sec_axis(~.*coeff, name="Logbooks Catch")
#     ) + theme(
#       axis.title.y = element_text(color = gfwColor, size=13),
#       axis.title.y.right = element_text(color = logbooksColor, size=13)
#     ) + ggtitle("GFW vs Logbooks")
#   
#   
#   library(plm)
#   # I get a negative value for effort????
#   d_panel <- pdata.frame(joint.compare, index=c("mmsi", "date"))
#   model <- plm(effort_sum ~ fishing_hours_sum, data=d_panel, model="within")
#   summary(model)
#   
#
#-----------------------------------------------------------------------------
# Clean dataset for discrete choice model (Change to Global Fishing Watch)

gfw.fishing.effort.CPS$trip_id <- udpipe::unique_identifier(gfw.fishing.effort.CPS, fields = c("mmsi", "date"))
  gfw.fishing.effort.CPS$haul_id <- udpipe::unique_identifier(gfw.fishing.effort.CPS, fields = c("trip_id", "haul_num"))
  gfw.fishing.effort.CPS <- gfw.fishing.effort.CPS %>% 
    dplyr::rename(set_lat = cell_ll_lat) %>%  dplyr::rename(set_long = cell_ll_lon) %>%
    dplyr::rename(set_date = date) %>%
    mutate(up_lat = set_lat) %>%
    mutate(up_long = set_long) 
  
  
# Include depth variable
  library(raster)
  depths <- raster("G:\\My Drive\\Project\\Data\\Global Fishing Watch\\Bathymetric\\bathymetry.tif")
  gfw_spdf <- SpatialPointsDataFrame(
    gfw.fishing.effort.CPS[,4:3], proj4string=depths@crs, gfw.fishing.effort.CPS)
  depth_mean <- raster::extract(depths,    # raster layer
                              gfw_spdf,    # SPDF with centroids for buffer
                              buffer = 20, # buffer size, units depend on CRS
                              fun=mean,    # what to value to extract
                              df=TRUE)     # return a dataframe?
    gfw.fishing.effort.CPS$depth <- depth_mean$bathymetry
    gfw.fishing.effort.CPS$depth_bin <- cut(gfw.fishing.effort.CPS$depth, 9, include.lowest=TRUE, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

# Include distance to shore
  shore.dist <- raster("G:\\My Drive\\Project\\Data\\Global Fishing Watch\\distance-from-shore.tif")
  gfw_spdf <- SpatialPointsDataFrame(
    gfw.fishing.effort.CPS[,4:3], proj4string=shore.dist@crs, gfw.fishing.effort.CPS)
  shore.dist_mean <- raster::extract(shore.dist, gfw_spdf, buffer = 10, fun=mean, df=TRUE)
    gfw.fishing.effort.CPS$shore.dist <- shore.dist_mean$distance.from.shore

# Include distance to port 
  port.dist <- raster("G:\\My Drive\\Project\\Data\\Global Fishing Watch\\distance-from-port-v20201104.tiff")
  gfw_spdf <- SpatialPointsDataFrame(
    gfw.fishing.effort.CPS[,4:3], proj4string=port.dist@crs, gfw.fishing.effort.CPS)
  port.dist_mean <- raster::extract(port.dist, gfw_spdf, buffer = 10, fun=mean, df=TRUE)
    gfw.fishing.effort.CPS$port.dist <- port.dist_mean$distance.from.port.v20201104


# ------------------------------------------------------------------
## Merge location data to SDM outputs

# Pacific sardine

sdm.psdn.all <- tibble(set_date = integer(), set_lat = integer(), set_long = integer(), psdn.sdm = integer(), psdn.date.sdm = integer())
    
for (y in min.year:max.year) {
  for (m in 1:12) {
  
    dat <- ncdf4::nc_open(paste0("G:/My Drive/Project/Data/SDM/sardine/sard_", 
                                     paste0(as.character(m), 
                                            paste0("_", paste0(as.character(y),"_GAM.nc")))))    

    set_long <- ncdf4::ncvar_get(dat, "lon")
    set_lat <- ncdf4::ncvar_get(dat, "lat")
    psdn.date.sdm <- ncdf4::ncvar_get(dat, "time")
    psdn.sdm <- ncdf4::ncvar_get(dat, "predGAM")
    
    # Close the netcdf
    ncdf4::nc_close(dat)			
    
    # Reshape the 3D array so we can map it, change the time field to be date
    dimnames(psdn.sdm) <- list(set_long = set_long, set_lat = set_lat, psdn.date.sdm = psdn.date.sdm)
    sdmMelt <- reshape2::melt(psdn.sdm, value.name = "psdn.sdm")
    sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$psdn.date.sdm)			
    
    
    # mutate(set_lat_sdm = round(set_lat, digits = 1)) %>%
    # mutate(set_long_sdm = round(set_long, digits = 1)) 
    
    sdm.month <- gfw.fishing.effort.CPS %>%
      left_join(sdmMelt, by = c("set_date", "set_lat", "set_long")) %>% drop_na(psdn.sdm) %>% 
      select("set_date", "set_lat", "set_long", "psdn.sdm", "psdn.date.sdm")
    
    sdm.psdn.all <- rbind(sdm.psdn.all, sdm.month)
    
    print(y)
    print(m)
  }
}
  
sdm.psdn.all <-  sdm.psdn.all  %>% unique() 
gfw.fishing.effort.CPS <- gfw.fishing.effort.CPS %>%
  left_join(sdm.psdn.all, by = c("set_date", "set_lat", "set_long"))
  gfw.fishing.effort.CPS$psdn.date.sdm <- as.Date("1900-01-01") + days(gfw.fishing.effort.CPS$psdn.date.sdm)	


# Northern anchovy

sdm.nanc.all <- tibble(set_date = integer(), set_lat = integer(), set_long = integer(), nanc.sdm = integer(), nanc.date.sdm = integer())
    
for (y in min.year:max.year) {
  for (m in 1:12) {
  
    dat <- ncdf4::nc_open(paste0("G:/My Drive/Project/Data/SDM/anchovy/anch_", 
                                     paste0(as.character(m), 
                                            paste0("_", paste0(as.character(y),"_GAM.nc")))))    

    set_long <- ncdf4::ncvar_get(dat, "lon")
    set_lat <- ncdf4::ncvar_get(dat, "lat")
    nanc.date.sdm <- ncdf4::ncvar_get(dat, "time")
    nanc.sdm <- ncdf4::ncvar_get(dat, "predGAM")
    
    # Close the netcdf
    ncdf4::nc_close(dat)      
    
    # Reshape the 3D array so we can map it, change the time field to be date
    dimnames(nanc.sdm) <- list(set_long = set_long, set_lat = set_lat, nanc.date.sdm = nanc.date.sdm)
    sdmMelt <- reshape2::melt(nanc.sdm, value.name = "nanc.sdm")
    sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$nanc.date.sdm)     
    
    
    # mutate(set_lat_sdm = round(set_lat, digits = 1)) %>%
    # mutate(set_long_sdm = round(set_long, digits = 1)) 
    
    sdm.month <- gfw.fishing.effort.CPS %>%
      left_join(sdmMelt, by = c("set_date", "set_lat", "set_long")) %>% drop_na(nanc.sdm) %>% 
      select("set_date", "set_lat", "set_long", "nanc.sdm", "nanc.date.sdm")
    
    sdm.nanc.all <- rbind(sdm.nanc.all, sdm.month)
    
    print(y)
    print(m)
  }
}
  
sdm.nanc.all <-  sdm.nanc.all  %>% unique() 
gfw.fishing.effort.CPS <- gfw.fishing.effort.CPS %>%
  left_join(sdm.nanc.all, by = c("set_date", "set_lat", "set_long"))
  gfw.fishing.effort.CPS$nanc.date.sdm <- as.Date("1900-01-01") + days(gfw.fishing.effort.CPS$nanc.date.sdm)


# Market squid

sdm.msqd.all <- tibble(set_date = integer(), set_lat = integer(), set_long = integer(), msqd.sdm = integer(), msqd.date.sdm = integer())
    
for (y in min.year:max.year) {
  for (m in 1:12) {
  
    dat <- ncdf4::nc_open(paste0("G:/My Drive/Project/Data/SDM/squid/squid_", 
                                     paste0(as.character(m), 
                                            paste0("_", paste0(as.character(y),"_GAM.nc")))))    

    set_long <- ncdf4::ncvar_get(dat, "lon")
    set_lat <- ncdf4::ncvar_get(dat, "lat")
    msqd.date.sdm <- ncdf4::ncvar_get(dat, "time")
    msqd.sdm <- ncdf4::ncvar_get(dat, "predGAM")
    
    # Close the netcdf
    ncdf4::nc_close(dat)      
    
    # Reshape the 3D array so we can map it, change the time field to be date
    dimnames(msqd.sdm) <- list(set_long = set_long, set_lat = set_lat, msqd.date.sdm = msqd.date.sdm)
    sdmMelt <- reshape2::melt(msqd.sdm, value.name = "msqd.sdm")
    sdmMelt$set_date <- as.Date("1900-01-01") + days(sdmMelt$msqd.date.sdm)     
    
    
    # mutate(set_lat_sdm = round(set_lat, digits = 1)) %>%
    # mutate(set_long_sdm = round(set_long, digits = 1)) 
    
    sdm.month <- gfw.fishing.effort.CPS %>%
      left_join(sdmMelt, by = c("set_date", "set_lat", "set_long")) %>% drop_na(msqd.sdm) %>% 
      select("set_date", "set_lat", "set_long", "msqd.sdm", "msqd.date.sdm")
    
    sdm.msqd.all <- rbind(sdm.msqd.all, sdm.month)
    
    print(y)
    print(m)
  }
}
  
sdm.msqd.all <-  sdm.msqd.all  %>% unique() 
gfw.fishing.effort.CPS <- gfw.fishing.effort.CPS %>%
  left_join(sdm.msqd.all, by = c("set_date", "set_lat", "set_long"))
  gfw.fishing.effort.CPS$msqd.date.sdm <- as.Date("1900-01-01") + days(gfw.fishing.effort.CPS$msqd.date.sdm)


# ------------------------------------------------------------------
## Merge location data to wind
  
# wind u  
dat_u <- ncdf4::nc_open("C:/Data/ROMS u-v/wcnrt_su_daily_20110102_20170419.nc")  
  set_long <- ncdf4::ncvar_get(dat_u, "lon")
    set_long <- set_long[,1]
  set_lat <- ncdf4::ncvar_get(dat_u, "lat")
    set_lat <- set_lat[1,]
  wind.u.date <- dat_u$dim$time$vals
  wind.u <- ncdf4::ncvar_get(dat_u, "su")
  
# Close the netcdf
ncdf4::nc_close(dat_u)      
  
# Reshape the 3D array so we can map it, change the time field to be date
dimnames(wind.u) <- list(set_long = set_long, set_lat = set_lat, wind.u.date = wind.u.date)
  wind.u.Melt <- reshape2::melt(wind.u, value.name = "wind.u")
  wind.u.Melt$set_date <- as.Date("2011-01-01") + days(wind.u.Melt$wind.u.date)   
  wind.u.Melt <- wind.u.Melt %>% mutate(set_long = set_long - 0.05)

gfw.fishing.effort.CPS <- merge(gfw.fishing.effort.CPS, wind.u.Melt, 
                                   by=c("set_date", "set_lat", "set_long"), all.x = TRUE)
  gfw.fishing.effort.CPS$wind.u.date <- as.Date("2011-01-01") + days(gfw.fishing.effort.CPS$wind.u.date)  

  
# wind v  
dat_v <- ncdf4::nc_open("C:/Data/ROMS u-v/wcnrt_sv_daily_20110102_20170419.nc")  
  set_long <- ncdf4::ncvar_get(dat_v, "lon")
    set_long <- set_long[,1]
  set_lat <- ncdf4::ncvar_get(dat_v, "lat")
    set_lat <- set_lat[1,]
  wind.v.date <- dat_v$dim$time$vals
  wind.v <- ncdf4::ncvar_get(dat_v, "sv")
  
# Close the netcdf
ncdf4::nc_close(dat_v)      
  
# Reshape the 3D array so we can map it, change the time field to be date
dimnames(wind.v) <- list(set_long = set_long, set_lat = set_lat, wind.v.date = wind.v.date)
  wind.v.Melt <- reshape2::melt(wind.v, value.name = "wind.v")
  wind.v.Melt$set_date <- as.Date("2011-01-01") + days(wind.v.Melt$wind.v.date)   
  wind.v.Melt <- wind.v.Melt %>% mutate(set_lat = set_lat - 0.05)
   
gfw.fishing.effort.CPS <- merge(gfw.fishing.effort.CPS, wind.v.Melt, 
                                by=c("set_date", "set_lat", "set_long"), all.x = TRUE)
  gfw.fishing.effort.CPS$wind.v.date <- as.Date("2011-01-01") + days(gfw.fishing.effort.CPS$wind.v.date)  
  

# -------------------------------------------------------------
## Obtain port of landing... (closest port for now...)  
ports.coord <- read.csv(file = "G://My Drive//Project//Data//Storm Warnings//WCports_mww_events09-16.csv")
  ports.coord <- ports.coord %>% select("pcid", "pcid_lat", "pcid_long") %>% unique()
  port.min.dist <- tibble(closest_port = character())
  
for (i in 1:nrow(gfw.fishing.effort.CPS)) {
  ports.coord$dist <- getDistanceFromLatLonInKm(ports.coord$pcid_lat, 
                                                ports.coord$pcid_long,
                                                gfw.fishing.effort.CPS$set_lat[i], 
                                                gfw.fishing.effort.CPS$set_long[i])
    ports.coord.min.dist <- ports.coord %>% filter(dist == min(dist)) %>% do(head(.,1))
    port.min.dist <- port.min.dist %>% add_row(closest_port = as.character(ports.coord.min.dist[1, 1]))
}
  
gfw.fishing.effort.CPS <- cbind(gfw.fishing.effort.CPS, port.min.dist)



# -------------------------------------------------
## Merge storm warning signals
warnings.signals <- read.csv(file = "G://My Drive//Project//Data//Storm Warnings//WCports_mww_events09-16.csv")
  warnings.signals <- warnings.signals %>% 
    select("pcid", "d_issued", "d_expired", "hurricane", "gale", "smcraft", "mww_other") %>%
    dplyr::rename(closest_port = pcid) 
    warnings.signals$d_issued <- as.Date(warnings.signals$d_issued, "%d%b%Y %H:%M:%OS")
    warnings.signals$d_expired <- as.Date(warnings.signals$d_expired, "%d%b%Y %H:%M:%OS")
    warnings.signals <- warnings.signals %>% unique() 

library(sqldf)
df1 <- gfw.fishing.effort.CPS
df2 <- warnings.signals
    
warnings.signals <-  sqldf("select df1.*, df2.hurricane, df2.gale, df2.smcraft, df2.mww_other
                                        from df1 left join df2 on
                                        (df1.closest_port = df2.closest_port) AND 
                                        (df1.set_date between df2.d_issued and df2.d_expired)") 

warnings.signals <- warning.signals %>% unique() %>% 
  select("haul_id", "hurricane", "gale", "smcraft", "mww_other")
  warnings.signals <- warning.signals %>% group_by(haul_id) %>%
    summarise(hurricane = sum(hurricane), gale = sum(gale), 
              smcraft = sum(smcraft), mww_other = sum(mww_other))
  warnings.signals[is.na(warnings.signals)] <- 0

  gfw.fishing.effort.CPS <- merge(gfw.fishing.effort.CPS, warnings.signals, 
                                  by=c("haul_id"), all.x = TRUE)
  
# --------------------------------------------------
## Save database! 
gfw.fishing.effort.CPS.save <- gfw.fishing.effort.CPS %>% 
  select("set_date", "set_lat", "set_long", "mmsi", "fishing_hours", "haul_num", "species", 
         "fleet_name", "trip_id", "haul_id", "up_lat", "up_long", "depth", "depth_bin", "shore.dist", 
         "port.dist", "psdn.sdm", "nanc.sdm", "msqd.sdm", "wind.u", "wind.v", "closest_port",
         "hurricane", "gale", "smcraft", "mww_other")

write.csv(gfw.fishing.effort.CPS.save,"Data\\discrete_choice_data.csv", row.names = FALSE)
save.image (file = "disc_choice.RData")



# ------------------------------------------------------------------
## Obtain (year) price by port from PacFIN landing data

PacFIN_dat <- read.csv(file = here::here("Data", "PacFin.csv"))
price_PSDN <- PacFIN_dat %>%
  dplyr::filter(Species_code == "PSDN") %>%
  group_by(Landing_year) %>%
  summarize(price.PSDN = mean(Price, na.rm = T)) %>%
  dplyr::rename(set_year = Landing_year)

psdn.logbook <- merge(psdn.logbook,price_PSDN,by=c('set_year'),all.x = TRUE) 


# ------------------------------------------------------------------
## Create expected and actual revenue -- psdn_rev variable-- using SDMs (maybe moving average?) and catch
## (I need prices by port)

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



