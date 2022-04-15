###################################################
### Climate effects on species-location choices ###
###################################################

### Convex hull and kernel density ###

#----------------------------------
# Set up #

## Read data ##
disc.choice.dat <- read.csv(file = here("Data", "discrete_choice_data.csv"))
logbooks.dat <- read.csv(file = "C:/Data/logbooks_merged.csv")

## Create functions ##
cntrd <- function(x) {
  data.frame(geosphere::centroid(as.matrix(x[,c("set_long", "set_lat")])))

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

#-------------------------------------------------------------
# Graphs #

## Calculate centroids ##
centroids.fleet <- logbooks.dat %>% group_by(fleet_name, species) %>% do(cntrd(.)) %>% 
  dplyr::rename(long_fleet = lon) %>% dplyr::rename(lat_fleet = lat) 
  logbooks.dat <- logbooks.dat %>% 
    merge(centroids.fleet, by=c('fleet_name', 'species'),all.x = TRUE) %>% 
    mutate(drvid = ifelse(drvid == 0, BoatName, ifelse(is.na(drvid), BoatName, drvid)))
  
centroids.drvid <- logbooks.dat %>% group_by(drvid) %>% mutate(count = n()) %>% dplyr::filter(count>2) %>%
  group_by(drvid) %>% do(cntrd(.)) %>% 
  dplyr::rename(long_drvid = lon) %>% dplyr::rename(lat_drvid = lat)
  logbooks.dat <- merge(logbooks.dat, centroids.drvid, by=c('drvid'),all.x = TRUE) 
  
# Caclulate standard errors
  std_coordinates <- logbooks.dat %>% 
    mutate(std_lat = (set_lat - lat_drvid)^2) %>% 
    mutate(std_lon = (set_long - long_drvid)^2)  %>% group_by(drvid) %>% 
    mutate(sum_std_lat=sum(std_lat), n=n(), sum_std_lon=sum(std_lon))  %>%
    mutate(std_lat = sqrt(sum_std_lat / (n-1))) %>% mutate(std_lon = sqrt(sum_std_lon / (n-1))) %>%
    tidyr::unite('fleet', fleet_name:species, remove = FALSE) %>% 
    dplyr::select("std_lon", "std_lat", "drvid", "fleet") %>% unique()



### How many standard error deviate from the average (by mmsi and fleet) ###
LonCol <- rgb(1,0,0,0.2)
LatCol <- rgb(0,0,1,0.2)
hist_data <- gather(std_coordinates, condition, measurement, std_lon, std_lat, factor_key=TRUE)
ggplot(hist_data, aes(measurement, fill = condition)) +  geom_density(alpha = 0.2) +
  labs(fill = "Variable", x = "Standard deviation (in degrees) from centroids", y = "Density") +
  scale_fill_manual(labels = c("Longitude (SD)", "Latitute (SD)"), values = c(LonCol, LatCol))

### How many standard error deviate from the average (by mmsi and fleet) ###
hist_data <- gather(std_coordinates, condition, measurement, std_lon, std_lat, factor_key=TRUE)
ggplot(hist_data, aes(measurement, fill = condition)) +  geom_density(alpha = 0.2) +
  labs(fill = "Variable", x = "Standard deviation (in degrees) from centroids", y = "Density") +
  scale_fill_manual(labels = c("Longitude (SD)", "Latitute (SD)"), values = c(LonCol, LatCol)) + 
  facet_wrap(~fleet, scales = "free")


## Convex hull estimation

### Minimum convex polygon (or convex hull) It is the smallest polygon in which no internal angle exceeds 180 degrees and which contains all sites. 

logbooks.dat.sp <- logbooks.dat %>% tidyr::unite('fleet', c(fleet_name, species), remove = FALSE) %>% 
  group_by(drvid) %>% mutate(count = n()) %>% filter(count > 5)
  logbooks.sp <- logbooks.dat.sp[, c("drvid", "set_long", "set_lat")] 

# %>% dplyr::filter(count>=5) 
# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(logbooks.sp) <- c("set_long", "set_lat")
# proj4string(logbooks.sp) <- CRS( "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs" )

# Calculate MCPs for each turtle
library(adehabitatHR) # Load library to estimate "home range estimation" (used in ecology I guess)
logbooks.mcp <- mcp(logbooks.sp, percent = 100, unin = c("km"), unout = c("ha"))
logbooks.convex.hull <- as.data.frame(logbooks.mcp)
# # Plot
library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(logbooks.sp, col = as.factor(logbooks.sp@data$drvid), pch = 16)
plot(logbooks.mcp, col = alpha(1:5, 0.5), add = TRUE)
# 

# Plot histrogram for convex hull by fleet
convex.hull <- logbooks.dat %>% tidyr::unite('fleet', fleet_name:species, remove = FALSE) %>% 
  dplyr::select("drvid", "fleet") %>% unique() %>% dplyr::rename(id = drvid) 
  convex.hull <- merge(convex.hull, logbooks.convex.hull, by=c('id'), all.x = TRUE)
  # ggplot(convex.hull, aes(area)) +  geom_density() + facet_wrap(~fleet, scales = "free") +
  #   labs(x = "Convex hull area (hectares)", y = "Density")



## Convex hull estimation

# Create dataset 

library(sp)
logbooks.fleet <- logbooks.dat.sp[, c("fleet", "set_long", "set_lat")] 
sf::sf_use_s2(FALSE)

ocean.shp <- sf::read_sf(here::here("Data", "ne_10m_ocean", "ne_10m_ocean.shp"), as_tibble = TRUE)
logbooks.sf.fleet <- sf::st_as_sf(logbooks.fleet, coords = c("set_long", "set_lat"), crs = CRS("+proj=longlat"), na.fail = FALSE)
logbooks.sf.fleet <- logbooks.sf.fleet[ocean.shp, ]
logbooks.sp.fleet <- as(logbooks.sf.fleet, Class = "Spatial")

# ## Create a SpatialPointsDataFrame by defining the coordinates
# coordinates(logbooks.sp.fleet) <- c("set_long", "set_lat")

## Set the coordinate reference system (CRS)
proj4string(logbooks.sp.fleet) <- CRS("+proj=longlat")


# Calculate MCPs for each vessel

library(adehabitatHR) # Load library to estimate "home range estimation" (used in ecology I guess)
logbooks.mcp <- mcp(logbooks.sp.fleet, percent = 80, unin = c("km"), unout = c("ha"))

# Plot
## Convert SpatialPointsDataFrames and SpatialPolygonsDataFrames into lat/long
logbooks.spgeo <- sp::spTransform(logbooks.sp.fleet, CRS("+proj=longlat"))
logbooks.mcpgeo <- sp::spTransform(logbooks.mcp, CRS("+proj=longlat"))

# Download tiles using ggmap
library(ggmap)
mybasemap <- get_stamenmap(bbox = c(left = -126, bottom = 32, right = -117, top = 48), zoom = 5,  maptype = "terrain")

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
logbooks.geo <- data.frame(logbooks.spgeo@coords, 
                          id = logbooks.spgeo@data$fleet)

ggmap(mybasemap) + 
  geom_polygon(data = fortify(logbooks.mcpgeo),  
               # Polygon layer needs to be "fortified" to add geometry to the dataframe
              aes(long, lat, colour = id, fill = id),
              alpha = 0.5) +
  theme(legend.position = "right") +
  labs(x = "Longitude", y = "Latitude") +
  scale_colour_manual(name = "Fleet", 
                      values = c("red", "blue", "purple", "green", "orange"),
                      breaks = c("T001", "T002", "T003", "T004", "T005"))   


# -------------------------------------------------------
# Create Kernel Density Map

library(sp,sf,maptools,rgdal,rgeos,raster,adehabitatHR, tmap)

## Load the CCS shapfile
Output.Areas <- sf::st_read("C:/Users/fequezad/Downloads/lme/lme.shp")
library(spatstat)
  Output.Areas <- sf::st_transform(Output.Areas, crs = CRS("+init=EPSG:32610"))
  C <- as.owin(Output.Areas$geometry)
  W<- as.mask(C, eps = 100)
  # Output.Areas <- sf:::as_Spatial(Output.Areas$geom) # This works


## Create SpatialPointDataFrame for the year 2002
Effort.Points <- as.data.frame(ann.effort.sf.ocean) %>% 
  filter(year == 2003) %>%
  dplyr::select(c("X", "Y"))

point <- ppp(Effort.Points$X,Effort.Points$Y, window = W)
marks(point) <- NULL
plot(point)
  # coordinates(Effort.Points) <- ~X+Y
  
  
  # sigma <- bw.diggle(point)
  
  
## Runs the kernel density estimation
  memory.size(260000)
  d <- density.ppp(point, sigma=1000)
  plot(d) 
  # kde.output <- kernelUD(Effort.Points, h="href", grid = 2000)
  # plot(kde.output)

# ### Converts to raster
# kde <- raster(kde.output)
  
  Window(point) <- W
  plot(point, main=NULL, cols=rgb(0,0,0,.2), pch=20)

