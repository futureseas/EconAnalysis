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
part.data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

CATCH_AREAS <- part.data %>% 
  group_by(CATCH_AREA_CODE, AREA_TYPE_CODE) %>% 
  summarize(n_obs = n()) %>% 
  filter(AREA_TYPE_CODE == 1) 

numbers_only <- function(x) !grepl("\\D", x)
part.data$numbers <- numbers_only(part.data$CATCH_AREA_CODE)

part.data <- part.data %>% 
  mutate(CATCH_AREA_CODE = ifelse(numbers == TRUE, as.numeric(CATCH_AREA_CODE), NA)) %>%
  mutate(CATCH_AREA_CODE = as.numeric(CATCH_AREA_CODE)) %>%
  mutate(CATCH_AREA_CODE = ifelse(AREA_TYPE_CODE != 1, NA, CATCH_AREA_CODE))

### Open layers
setwd("C:/GitHub/EconAnalysis/Participation/BlockAreas")

library(maptools)
library(rgeos)
landuse <- readShapePoly("NOAA_Block/NonWDFWFisheryManagementAreas - NOAA Coastal Trawl Logbook Block") 
data <- as.data.frame(landuse) %>% select(c(BlockNumbe, CentroidLo, CentroidLa))
part.data.merge <- merge(part.data, data, 
                         by.x = 'CATCH_AREA_CODE', 
                         by.y = 'BlockNumbe', all.x = TRUE, all.y = FALSE)

# Link to logbook now!

