######################################
### Read block maps for each state ###
######################################

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)  


#-----------------------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
logbooks_FTID <- readRDS("C:\\Data\\Logbooks\\logbooks_FTID.rds")


Tickets <- Tickets_raw %>% dplyr::select(c(CATCH_AREA_CODE, AREA_TYPE_CODE, FTID, PACFIN_SPECIES_CODE))

# CATCH_AREAS <- Tickets %>% 
#   group_by(CATCH_AREA_CODE, AREA_TYPE_CODE) %>% 
#   summarize(n_obs = n()) %>% 
#   filter(AREA_TYPE_CODE == 1) 

# numbers_only <- function(x) !grepl("\\D", x)
# Tickets$numbers <- numbers_only(Tickets$CATCH_AREA_CODE)

table <- as.data.frame(table(Tickets_catch$CATCH_AREA_CODE, Tickets_catch$AREA_TYPE_CODE)) 
Tickets_catch <- Tickets %>% 
  filter(CATCH_AREA_CODE != 'UNKN') %>%
  filter(CATCH_AREA_CODE != '0000') %>%
  filter(AREA_TYPE_CODE == 1 | AREA_TYPE_CODE == 5) %>% 
  mutate(CATCH_AREA_CODE = as.numeric(CATCH_AREA_CODE)) %>%
  mutate(CATCH_AREA_CODE = ifelse(CATCH_AREA_CODE == 0, NA, CATCH_AREA_CODE)) %>%
  dplyr::select(CATCH_AREA_CODE, AREA_TYPE_CODE, FTID, PACFIN_SPECIES_CODE) %>% unique() %>%
  drop_na() %>%
  subset(!(AREA_TYPE_CODE == 1 & CATCH_AREA_CODE < 101)) %>%
  subset(!(AREA_TYPE_CODE == 1 & CATCH_AREA_CODE > 1444)) %>%
  subset(!(AREA_TYPE_CODE == 5 & CATCH_AREA_CODE > 13))


# Tickets_catch_n <- Tickets_catch %>% 
#   group_by(FTID, PACFIN_SPECIES_CODE) %>% 
#   mutate(n_rep = n()) %>% ungroup() %>% filter(n_rep == 2) %>%
#   ifelse()

### Open layers
setwd("C:/GitHub/EconAnalysis/Participation/BlockAreas")

library(maptools)
library(rgeos)
catch_area_1 <- readShapePoly("NOAA_Block/NonWDFWFisheryManagementAreas - NOAA Coastal Trawl Logbook Block") 
data_1 <- as.data.frame(catch_area_1) %>% select(c(BlockNumbe, CentroidLo, CentroidLa)) %>%
  rename(CATCH_AREA_CODE = BlockNumbe) %>%
  rename(lon_ca = CentroidLo) %>%
  rename(lat_ca = CentroidLa)

ticket.merge <- merge(Tickets_catch, data_1, 
                         by = c('CATCH_AREA_CODE'), all.x = TRUE, all.y = FALSE)

catch_area_2 <- readShapePoly("MajorFishingArea/MajorFishingArea - Recreational Marine Area Code.shp")
centr <- gCentroid(catch_area_2, byid = TRUE)
centr <- SpatialPointsDataFrame(centr, data= catch_area_2@data) 
data_2 <- as.data.frame(centr) %>% mutate(AreaName = as.character(AreaName)) %>%
  mutate(AreaName = ifelse(AreaName == '8-1', 8, ifelse(AreaName == "8-2", "8", AreaName))) %>%
  mutate(AreaName = ifelse(AreaName == '2-1', 2, ifelse(AreaName == "2-2", "2", AreaName))) %>%
  mutate(AreaName = as.numeric(AreaName)) %>% group_by(AreaName) %>%
  summarize(lat_ca2 = mean(y), lon_ca2 = mean(x)) %>% drop_na() %>%
  rename(CATCH_AREA_CODE = AreaName) 

ticket.catch.area <- merge(ticket.merge, data_2, 
                      by = c('CATCH_AREA_CODE'), all.x = TRUE, all.y = FALSE) %>%
  mutate(lon_ca = ifelse(is.na(lon_ca), lon_ca2, lon_ca)) %>%
  mutate(lat_ca = ifelse(is.na(lat_ca), lat_ca2, lat_ca)) %>%
  dplyr::select(c(lat_ca, lon_ca, FTID, PACFIN_SPECIES_CODE)) %>%
  drop_na()


saveRDS(ticket.catch.area, "catchareas_FTID.rds")



