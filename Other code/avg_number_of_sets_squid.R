#####################################################
#### Average number of sets over time for squid  ####
#####################################################

### Load MSQD logbooks ###
## Load packages ##
library(ggmap)
library(tidyverse)
library(lubridate)
library(here)
library(ggplot2)
library(dplyr)
detach(package:plyr)



# Load data #

sqd.logbook.vessel <- read_csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  mutate(vessel="CA Vessel") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  dplyr::rename(effort = "CatchEstimate") 
  
max_n_set.by.trip <- sqd.logbook.vessel %>% 
  filter(SetNumber>0 & SetNumber<99) %>%
  group_by(LogSerialNumber) %>%
  filter(SetNumber == max(SetNumber, na.rm=TRUE)) %>% 
  dplyr::select(c('LogSerialNumber', 'SetNumber', 'year')) %>% unique() %>%
  group_by(year) %>% summarise(avg_set = mean(SetNumber))


max_n_set.by.trip %>% 
  ggplot(aes(x=year, y=avg_set)) +
  geom_line() +
  geom_point()


# Save data to use it in estimations #



