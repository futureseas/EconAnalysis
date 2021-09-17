# Map fishing activity for Market Squid using available Logbooks from CDFW #

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
sqd.logbook.OR.2016 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = FedDoc) %>%
  mutate(date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Squid (Lbs)`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2018 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2018") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Doc #`) %>%
  tidyr::fill(VesselID) %>%
  mutate(date = as.Date(`Log Date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2019 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2019") %>%
  mutate(lat = Lat + lat/60) %>%
  mutate(lon = Long + long/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(VesselID = `doc number`) %>%
  mutate(date = as.Date(`Fishing date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Est Lbs Squid`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR.2020 <- read_excel("C:\\Data\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2020") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(vessel="OR") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VesselID = `Doc #`) %>%
  mutate(date = as.Date(`log date`,format="%Y-%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = `Lbs`) %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.OR <- rbind(sqd.logbook.OR.2016, sqd.logbook.OR.2018, sqd.logbook.OR.2019, sqd.logbook.OR.2020)


## California

sqd.logbook.vessel <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  mutate(vessel="CA Vessel") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "CatchEstimate") %>%
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))

sqd.logbook.light.brail <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  dplyr::rename(lat = Lat_DD) %>%
  dplyr::rename(lon = Long_DD) %>%
  mutate(vessel="CA Light Boat") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "ElapsedTime") %>% 
  dplyr::select(c('lat', 'lon', 'VesselID', 'vessel', 'year', 'month', 'date', 'effort'))


# DATABASE
sqd.logbook <- rbind(sqd.logbook.OR, sqd.logbook.vessel,sqd.logbook.light.brail)
sqd.logbook <- sqd.logbook[-which(is.na(sqd.logbook$lat)), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lat == 0), ] 
sqd.logbook <- sqd.logbook[-which(sqd.logbook$lon == 0), ] 
sqd.logbook$lon <- with(sqd.logbook, ifelse(lon > 0, -lon, lon))

sqd.logbook <- sqd.logbook %>% 
  filter(year > 2001)

# Count number of vessels in data by year
tally.sqd.logbook <- sqd.logbook %>%
  group_by(year) %>%
  dplyr::summarize(numvessels = length(unique(VesselID)))

vessel.sqd.logbook <- sqd.logbook %>%
  group_by(year, vessel) %>%
  dplyr::summarize(numvessels = length(unique(VesselID)), .groups = 'drop')

library(viridis)
  ggplot(vessel.sqd.logbook, aes(fill=as.factor(vessel), y=numvessels, x=year)) +
    geom_bar(position="stack", stat="identity") + 
    scale_fill_viridis(discrete = TRUE, option="viridis") + 
    labs(y = "Number of vessels", x = "Year", fill = "Vessel permit?")
# +
#   ggsave("figures/vessel_tally.pdf")

  
# Sum total catch estimate by lat/lon grid cell
tot.effort <- sqd.logbook %>%
  group_by(lat, lon) %>%
  dplyr::summarize(tot_effort = sum(effort)) %>% 
  filter(tot_effort > 0)

# Sum effort by year and lat/lon grid cell
library(summarytools)
library(doBy)
sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}
ann.effort <- doBy::summaryBy(effort ~ year + lat + lon + vessel, FUN=sum, data=sqd.logbook)

### Exclude and idendify effort made inland using Ocean shape file to obtain intersection ###
sf::sf_use_s2(FALSE)
ocean.shp <- sf::read_sf(here::here("Data", "ne_10m_ocean", "ne_10m_ocean.shp"))
ann.effort.sf <- sf::st_as_sf(ann.effort, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
ann.effort.sf.ocean <- ann.effort.sf[ocean.shp, ]
ann.effort.sf.ocean <- cbind(ann.effort.sf.ocean, st_coordinates(ann.effort.sf.ocean))


## Calculate distance and min/max/mean ##

library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)

# Load port georeferenced data
ports <- read_csv("data/port_names.csv")

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


ann.effort.sf.ocean <- ann.effort.sf.ocean %>%
   filter(min_dist < 220)


descr(ann.effort.sf.ocean$min_dist, stats = c("mean", "sd", "min", "max"), transpose = TRUE, headings = TRUE)



# Generate map of aggregate CPS boats activity (this won't work anymore)
# register_google(key = "AIzaSyBypi8T09hkTVlclaBDxFf-qjIpt39UhwQ")

map <- get_stamenmap(bbox = c(left = -128,
                              bottom = 30,
                              right = -116,
                              top = 47),
                     zoom = 6,
                     maptype = "toner-lite")

msqd_map <- ggmap(map) + 
  geom_point(aes(x = X, y = Y, colour = as.factor(vessel)),
             data = ann.effort.sf.ocean) +
  transition_time(year) +
  labs(title = 'Year: {frame_time} - Market Squid'
       , subtitle =  'Distance to closest port: Mean = 26.76; SD = 21.82; Min = 0.10; Max = 219.88',
       colour = 'Category')

gganimate::animate(msqd_map, fps = 1, duration = 19)
anim_save("Figures/msqd_logbook.gif")


## Identify effort made in-land (errors?) ##

ann.effort <- doBy::summaryBy(effort ~ year + lat + lon, FUN=sum, data=sqd.logbook)

sqd.logbook.sf <- sf::st_as_sf(sqd.logbook, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
logbook.intersects.ocean <- st_intersects(sqd.logbook.sf, ocean.shp, sparse = FALSE)

sqd.logbook.inland.sf <- sqd.logbook.sf %>%
  mutate(intersects_ocean  = logbook.intersects.ocean) %>%
  filter(intersects_ocean == "FALSE")

n_inland_data = nrow(sqd.logbook.inland.sf) / nrow(sqd.logbook.sf)

#  6.4% of the logbooks, mostly around 2004-2008



