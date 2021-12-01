# Map fishing activity for Pacific Sardine using available Logbooks from CDFW #

## Load packages ##
library(ggmap)
library(tidyverse)
library(gganimate)
library(magick)
library(lubridate)
library(here)
library(tigris)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(plyr)
library(readxl)

# Load data

## Oregon
psdn.logbook.OR <- read_excel("C:\\Data\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(Lat = Lat + LatMin/60) %>%
  mutate(Long = Long + LongMin/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = FedDoc) %>%
  mutate(date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = Sard) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

psdn.logbook.WA <- read_excel("C:\\Data\\WDFW CPS logbooks\\WA Sardine Logbook Flatfile Data Reques 20-15485.xlsx") %>%
  mutate(Lat = `Latitude Degrees` + `Latitude Minutes`/60) %>%
  mutate(Long = `Longitude Degrees` + `Longitude Minutes`/60) %>%
  mutate(vessel="WA") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Vessel`) %>%
  mutate(date = as.Date(`Fishing Date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Sardine Retained mt`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

# DATABASE
psdn.logbook <- rbind(psdn.logbook.OR, psdn.logbook.WA)
psdn.logbook <- psdn.logbook[-which(is.na(psdn.logbook$lat)), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lat == 0), ] 
# psdn.logbook <- psdn.logbook[-which(psdn.logbook$lon == 0), ] 
psdn.logbook$lon <- with(psdn.logbook, ifelse(lon > 0, -lon, lon))

psdn.logbook <- psdn.logbook %>% 
  filter(year > 2001)

# Count number of vessels in data by year
tally.psdn.logbook <- psdn.logbook %>%
  group_by(year) %>%
  dplyr::summarize(numvessels = length(unique(VesselID)))

vessel.psdn.logbook <- psdn.logbook %>%
  group_by(year, vessel) %>%
  dplyr::summarize(numvessels = length(unique(VesselID)), .groups = 'drop')

library(viridis)
  ggplot(vessel.psdn.logbook, aes(fill=as.factor(vessel), y=numvessels, x=year)) +
    geom_bar(position="stack", stat="identity") + 
    scale_fill_viridis(discrete = TRUE, option="viridis") + 
    labs(y = "Number of vessels", x = "Year", fill = "State")
# +
#   ggsave("figures/vessel_tally.pdf")

  
# Sum total catch estimate by lat/lon grid cell
tot.effort <- psdn.logbook %>%
  group_by(lat, lon) %>%
  dplyr::summarize(tot_effort = sum(effort)) %>% 
  filter(tot_effort > 0)

# Sum effort by year and lat/lon grid cell
library(summarytools)
library(doBy)
sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}
ann.effort <- doBy::summaryBy(effort ~ year + lat + lon + vessel, FUN=sum, data=psdn.logbook)

### Exclude and identify effort made inland using Ocean shape file to obtain intersection ###
sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
ocean.shp <- sf::read_sf(here::here("Data", "ne_10m_ocean", "ne_10m_ocean.shp"), as_tibble = TRUE)
ann.effort.sf <- sf::st_as_sf(ann.effort, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
ann.effort.sf.ocean <- ann.effort.sf[ocean.shp, ]
ann.effort.sf.ocean <- cbind(ann.effort.sf.ocean, st_coordinates(ann.effort.sf.ocean))


## Calculate distance and min/max/mean ##

library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)

# Load port georeferenced data
ports <- read_csv("Data/Ports/port_names.csv")

# Compare port distances
ports_dist <- ports %>%
  mutate(k = 1)

ports_dist <- ports_dist %>%
  full_join(ports_dist, by = "k") %>%
  mutate(dist = by(., 1:nrow(.), function(row) { 
    distHaversine(c(row$lon.x, row$lat.x), c(row$lon.y, row$lat.y)) / 1000}))

# Calculate SDM values and build output data frame.
library(lwgeom)
ports.sf <- sf::st_as_sf(ports, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
dist_port <- as.data.frame(st_distance(ann.effort.sf.ocean, ports.sf))
ann.effort.sf.ocean$min_dist <- apply(dist_port, 1, FUN=min) / 1000
ann.effort.sp.ocean <- as_Spatial(ann.effort.sf.ocean)



# -------------------------------------------------------
# Create Kernel Density Map
library(sp)
library(spatstat)
library(rspatial)
library(maptools)

## Define working region 
city <- sp_data('city')
OceanOwin <- as.owin(city)

## Create SpatialPointDataFrame
str(dat)
dat <- as.data.frame(ann.effort.sf.ocean)
coordinates(dat) <- ~X+Y

## Create....
p <- ppp(dat[,1], dat[,2], window=OceanOwin)

# Unweighted KDE (spatial locations only)				
pt.kde <- sp.kde(x = dat, bw = 1000, standardize = TRUE, 
                 nr=104, nc=78, scale.factor = 10000 )

# Plot results
plot(pt.kde, main="Unweighted kde")
points(meuse, pch=20, col="red") 

#---------------------------------------------------------

# Assumes 20 km/hr and 22 hours for traveling there and back and 2 hours for fishing (Rose et al)
# ann.effort.sf.ocean <- ann.effort.sf.ocean %>%
#   filter(min_dist <= 220)

descr(ann.effort.sf.ocean$min_dist, stats = c("mean", "sd", "min", "max"), transpose = TRUE, headings = TRUE)

# Generate map of aggregate CPS boats activity (this won't work anymore)
# register_google(key = "ENTER KEY HERE")

map <- get_stamenmap(bbox = c(left = -128,
                              bottom = 42,
                              right = -121,
                              top = 50),
                     zoom = 6,
                     maptype = "toner-lite")

psdn_map <- ggmap(map) + 
  geom_point(aes(x = X, y = Y, colour = as.factor(vessel)),
             data = ann.effort.sf.ocean) +
  transition_time(year) +
  labs(title = 'Year: {frame_time} - Pacific Sardine'
       , subtitle =  'Distance to closest port: Mean = 27.95; SD = 11.24; Min = 0.40; Max = 182.29',
       colour = 'State')

gganimate::animate(psdn_map, fps = 1, duration = 14)
anim_save("Figures/psdn_logbook.gif")


## Identify effort made in-land (errors?) ##

psdn.logbook.sf <- sf::st_as_sf(psdn.logbook, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
logbook.intersects.ocean <- st_intersects(psdn.logbook.sf, ocean.shp, sparse = FALSE)

psdn.logbook.inland.sf <- psdn.logbook.sf %>%
  mutate(intersects_ocean  = logbook.intersects.ocean) %>%
  filter(intersects_ocean == "FALSE")

n_inland_data = nrow(psdn.logbook.inland.sf) / nrow(psdn.logbook.sf)

#  0.7% of the logbooks


