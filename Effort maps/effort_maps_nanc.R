# Map fishing activity for Market Squid using available Logbooks from CDFW #

## Load packages ##
library(ggmap)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(gganimate)
library(magick)
library(lubridate)
library(here)
library(tigris)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(readxl)

# Load data

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
    mutate(set_lon = lon_D + lon_min/60) %>%
    mutate(fleet_name="OR") %>%
  dplyr::rename(drvid = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  mutate(set_year = year(set_date)) %>%
  mutate(set_month = month(set_date)) %>%
  dplyr::rename(catch = `lbs anchovy`) %>%
  dplyr::select(c('set_lat', 'set_lon', 'fleet_name', 'drvid', 'set_year', 'set_month', 'set_date', 'catch'))

# DATABASE
nanc.logbook.OR <- nanc.logbook.OR[-which(is.na(nanc.logbook.OR$set_lat)), ] 
nanc.logbook.OR$set_lon <- with(nanc.logbook.OR, ifelse(set_lon > 0, -set_lon, set_lon))


# Sum effort by year and lat/lon grid cell
library(summarytools)
library(doBy)
sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}
ann.effort <- doBy::summaryBy(catch ~ set_year + set_lat + set_lon + fleet_name, FUN=sum, data=nanc.logbook.OR)

### Exclude and idendify effort made inland using Ocean shape file to obtain intersection ###
sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
ocean.shp <- sf::read_sf(here::here("Data", "ne_10m_ocean", "ne_10m_ocean.shp"))
ann.effort.sf <- sf::st_as_sf(ann.effort, coords = c("set_lon", "set_lat"), crs = 4326, na.fail = FALSE)
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

map <- get_stamenmap(bbox = c(left = -124.5,
                              bottom = 46,
                              right = -123.5,
                              top = 46.5),
                     zoom = 9,
                     maptype = "toner-lite")

ggmap(map) + 
  geom_point(aes(x = X, y = Y),
             data = ann.effort.sf.ocean) +
  labs(title = 'Northern Anchovy (2015-2016) in Oregon'
       , subtitle =  'Distance to closest port: Mean = 9.45; SD = 3.52; Min = 1.15; Max = 21.57')




## Identify effort made in-land (errors?) ##

ann.effort <- doBy::summaryBy(effort ~ year + lat + lon, FUN=sum, data=sqd.logbook)

sqd.logbook.sf <- sf::st_as_sf(sqd.logbook, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
logbook.intersects.ocean <- st_intersects(sqd.logbook.sf, ocean.shp, sparse = FALSE)

sqd.logbook.inland.sf <- sqd.logbook.sf %>%
  mutate(intersects_ocean  = logbook.intersects.ocean) %>%
  filter(intersects_ocean == "FALSE")

n_inland_data = nrow(sqd.logbook.inland.sf) / nrow(sqd.logbook.sf)

#  6.4% of the logbooks, mostly around 2004-2008



