###################################################
### Climate effects on species-location choices ###
###################################################

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







High variability on forage fisheries could encourage fisher to have complex fishers portfolio, using diversification as a tool for reducing risk [@kasperski2013]. This diversification can allow for species-specific environmental shocks affect other non-related fisheries through "harvest" arbitrage [@richerson2017]. Hetoregeneity must be considerate as fleet that differ by specific characteristics may have different responses to changes in policy and environmental conditions [@zhang2011]. We capture vessel heterogeneity estimating a Mixed Logit model. 

Because we have access to vessel characteristics, we can estimate a sorting model with observable heterogeneity following @zhang2011.


Previous studies on fishers behavior focus moslty on a single-species framework. TO our knowledge, only [@richerson2017] address the complexity of fleet dynamics on multispecies fishery in the U.S. West Coast, where the desicion which species to harvest is interdependent between species. They study how a closure of the salmon fishery on fisher behavior. Our study expand on their work studying the interrelation and the effect of closures and weather events on the desion to participate in fishery for the Coastal Pelagic Species fishery in the California current.  

Moreover, fisher decision to participate in a particular fishery could vary depending on the season. Some vessels change within a year their target species to follow species distribution over the year. Regulations (e.g. whether or not the fishery have limited entry, and the vessel holds a permit) and profitability can also condition the decision to participate in a fishery during a year [@richerson2017]. Complementary v/s substitute fisheries [@richerson2017]

Additional, we study fishers response to weather events (e.g. storms) and to a fishery closure, such as the Pacific sardine closure in 2015.


+ Research question:
  + **Big question:** What is the effect of climate change on fishers dexision. 
  + **Narrow question:** How does changes in the presence of CPS affect species and location choices by vessels registered in the US west coast?
  + Check if state limits affect the behavior of fish decisions.

+ Contribution:
  + SDM projections to forecast fisher behavior
  + Better understanding of fishers species portfolio
  + Policy analysis

+ Method
  + Mixed logir model:
    + Mixed logit? (A mixed logit (or random-coefficient logit) avoids assuming IIA allowing for marginal utility varies between individuals)
    + Relax the independence of irrelevant alternative assumption: Coeficient vary randomnly across vessels, and "Variance in the unobserved vessel-specific parameters induces correlation over alternatives in the stochastic portion
of utility." @revelt1998
    + Account for correlation in unobserved utility between repeated choices @revelt1998. The estimations is efficient. 

"Technological, economic, and management factors can limit both
the willingness and capacity for fishers to respond to shifting the
availability of target species, thus affecting the coupling between
landings and availability." Selden 2020


"Rogers in Nature: fishers are often limited in where they can fish based on local ecological
knowledge, vessel size or gear type, geographic distance,
spatial management or conservation measures and, in some cases,
customary territories9. Peer groups of vessels from the same port
and using the same gear type"

+ Characterize the fishery
  + Heterogeneity in location, do a map and calculate variation on latitude and longitude.
  + How far from ports
  + How far from the coast. 
  + Average distance traveled. 
  + Fishing effort
  
  
"Many discrete choice models include expected revenue across locations/time and travel distance [Eales and Wilen 1986; Smith 2002; Haynie, Hicks, and Schnier 2008; Haynie and Layton 2010; Zhang and Smith 2011]"

"Some incorporate past behavior (state dependence) and/or unobserved heterogeneity (random coefficients) [Holland and Sutinen 1999; Mistiaen and Strand 2000; Smith 2005]"

How to translate this to landings location?
In this paper we analyze location choice mode, how fisher grounds will evolve and how this will tranlaste to the decision to wich port to land. Using the choice of location/species as explanatory variable, we can then estimate a second stage for port landing (see Anna slides for NAAFE 2015, where she found that 75\% of the vessel never sqitch ports)


# Species/location choice model for the CPS fishery

+ Choices:
  + Set of location/specie
  + Number of choices can vary between vessels, as well as the number of periods or choice situtations @revelt1998.
  + What is the outside option, non-CPS fishery or non-fishing? [@zhang2011]
  + Use @hicks2020 methodology to select the choice set:
    + Crucial for the model the selection of the choice set. If is erroneous, our estimates would be biased.
    + We don't observe fishers complete set of alternatives. "A limitation of all of the models discussed thus far is the assumption that the choice set is finite and tractable. In the caseof spatial choice models in a number of environmental settings, including fisheries, the choice set is virtually infinite as spacecan be continually divided to form different “alternatives.”"
    + They propose a method to construct alternative choices when alternative are effectively infinite *we just observe a point where vessel fish over a very large open ocean area).
    + What is wrong with aggregating fishing areas? -> "areas may encompass highly heterogeneous fishing locations in terms of species composition and density or feasibility of fishing (e.g.unfishable rocky areas)". THIS COULD BIAS OUR RESULTS.
    + How this work: "Sample points from a fine scale grid of specific locations." It is a point-based approach for choosing the choice set. 
    + Should improve modeling in setting with fine-scale spatial heteogeneity.
    + The method is called: **Grid point-based sampling models.**
      + The region study is restricted, but within the region there are infinite points.
      + The choice set considerate **depth on water**. Does this is an important variable for CPS fisheries?
    
    
    
+ Explanatory variables:
  + Species value? This assumes fisher that has rational preferences and maximizes utility. It might be that they harvest a species as they have been doing it for a long time. 
Landings;
  + Vessel characteristics (Note: Not all vessels go to the exact location, so that decisions might depend on their characteristics).
  + Expected catch / species abundance?
  + Catch per unit of effort" (use monthly data to calculate this, maybe correlated with SDM outputs [@zhang2011]) Maybe is endogenus (comment from Dale Squires)... better to use SDM to get expected biomass at this area. 
  + DIstance? 
  
  + Girardin may have a way to compute congestion in location


+ Data 
  + CDWF logbooks and landings (Caitlin)
  + PacFIN landings by vessel & logbooks (try to connect to Global Fishing Watch). 
  + SDM's from Barb

+ Consideration:
  + Choice set important (locations and species).
  + Outside option?


# Method

+ Dale comments
  + Results are sensible when your have high resolution data (stochastic within day)
  + We have to find the optimal degree of disagregation (multicollinearity between explanatory variables)
  + Check for endogeneity, heterokedasticity and serial correlation (in panel data).
    + Distance might be endogeneous (location or distance is chosen? do a Hausman test)
  + Try including splines in the regressors 

It is of high relevance to define rigorously the choice set. For instance, @stafford2018 show that aggregating relevant alternative in the generic outside option lead to biased results and distorted substitution patterns. Thus, we should carefully assess the set of alternative in this case. In our case, this require to carefully select the set of species a fisher has as alternatives, as well as the location included in the choice set.
<!-- Model: Nested logit model -> Stages: Participation, species, and location. / How closure predictions were made? -->

We develop to different discrete choice model:
  + species participation model
  + Location and species decision model
  + species and gear decision model
  
Our second model is a location and species decision model. We can estimate a conditional logit for species-location choice set using charactristics of the choice and incorporating heterogeneity through interaction variables. 


A locations/species model could be implemented to see how fine scale behavior emerge to the aggregate one that we observe. Another think to consider is whether is better to use mixed logir or conditional logit with interactive terms (see Phaneuf and Requate (2016): "A Course in Environmental Economics: Theory, Policy, and Practice". 




## Graphs

```{r std_coordinates, include=FALSE}
# Calculate centroids 
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

```

```{r hist_all, echo=FALSE, fig.cap="Histogram for standard deviation from centroid", message=FALSE, warning=FALSE}
### How many standard error deviate from the average (by mmsi and fleet) ###
LonCol <- rgb(1,0,0,0.2)
LatCol <- rgb(0,0,1,0.2)
hist_data <- gather(std_coordinates, condition, measurement, std_lon, std_lat, factor_key=TRUE)
ggplot(hist_data, aes(measurement, fill = condition)) +  geom_density(alpha = 0.2) +
  labs(fill = "Variable", x = "Standard deviation (in degrees) from centroids", y = "Density") +
  scale_fill_manual(labels = c("Longitude (SD)", "Latitute (SD)"), values = c(LonCol, LatCol))

```

```{r hist_fleet, echo=FALSE, fig.cap="Histogram for standard deviation from centroid by fleet", message=FALSE, warning=FALSE}
### How many standard error deviate from the average (by mmsi and fleet) ###
hist_data <- gather(std_coordinates, condition, measurement, std_lon, std_lat, factor_key=TRUE)
ggplot(hist_data, aes(measurement, fill = condition)) +  geom_density(alpha = 0.2) +
  labs(fill = "Variable", x = "Standard deviation (in degrees) from centroids", y = "Density") +
  scale_fill_manual(labels = c("Longitude (SD)", "Latitute (SD)"), values = c(LonCol, LatCol)) + 
  facet_wrap(~fleet, scales = "free")
```


## Convex hull estimation

```{r convex_hull, echo=FALSE, fig.cap="Histogram for standard deviation of latitude", message=FALSE, warning=FALSE}
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


```


## Convex hull estimation

```{r convex_hull, echo=FALSE, fig.cap="Histogram for standard deviation of latitude", message=FALSE, warning=FALSE}

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

```


```{r kernel_density, eval=FALSE, include=FALSE}
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

```