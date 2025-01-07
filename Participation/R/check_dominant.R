########################################
### Participation model -- database ###
########################################

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)
library(tidyverse)  

setwd("C:/GitHub/EconAnalysis")

#-----------------------------------------------------
### Load in the data
# Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets_raw <- fread("C:/FutureSeasIII_2010_2020.csv")
# Tickets_raw<-rbind(Tickets1, Tickets2)
# rm(Tickets1, Tickets2)


#-----------------------------------------------------
### Add port area
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
Tickets_raw <- Tickets_raw %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
rm(port_area)


#-----------------------------------------------------
### Filter by type of catch
Tickets_raw <- Tickets_raw %>% 
  filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") %>% 
  drop_na(PORT_AREA_CODE)


#-----------------------------------------------------
### Subset the data to remove columns not relevant to this analysis. This will speed things up.
### Create also unique FTID
Tickets <- dplyr::select(Tickets_raw, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PORT_AREA_CODE,
                                        VESSEL_NUM, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, VESSEL_HORSEPOWER, 
                                        LANDED_WEIGHT_MTONS, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE,
                                        PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME,
                                        FISHER_LICENSE_NUM, AFI_PRICE_PER_POUND, NUM_OF_DAYS_FISHED)) %>%
  mutate(AFI_PRICE_PER_MTONS = AFI_PRICE_PER_POUND/0.000453592)
Tickets$FTID_unique <- udpipe::unique_identifier(Tickets, fields = c("FTID", "VESSEL_NUM", "LANDING_YEAR"))

Boats <- Tickets %>% group_by(FTID_unique, PACFIN_SPECIES_CODE) %>% 
  summarize(revenue = sum(AFI_EXVESSEL_REVENUE)) %>% 
  group_by(FTID_unique) %>% mutate(total = sum(revenue)) %>% ungroup() %>% mutate(porc = (100*revenue/total))

result <- Boats %>%
  group_by(FTID_unique) %>%
  arrange(desc(porc)) %>%
  slice(1) %>%
  ungroup()
mean(result$porc, na.rm = TRUE)
sd(result$porc, na.rm = TRUE)

