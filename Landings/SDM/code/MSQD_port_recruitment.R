### Processing of Justin Suca squid outputs ###

library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)
library(here)
library(readxl)
library(dplyr)

rm(list=ls())
gc()

SDM_port <- tibble(LANDING_YEAR = integer(),
                   PORT_NAME = character(),
                   SDM_90_JS_CPUE = numeric())

# Obtain port names used to land species of interest
port_codes <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv")

# Load port coordinates
ports_coord <- read.csv(here::here("Data", "Ports", "port_names.csv"))

# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by=c("PORT_NAME", "AGENCY_CODE"),all.x=TRUE, all.y=FALSE) %>%
  drop_na()


dat <- read.csv("G:\\My Drive\\Project\\Data\\SDM\\squid\\JS outputs\\Annual_Recruitment_MSq_Recruiment_Index.csv")
bounds <- read.csv("G:\\My Drive\\Project\\Data\\SDM\\squid\\JS outputs\\RREAS_Strata_Bounds.csv") %>%
  mutate(Stratum = ifelse(Stratum == 'South', "S", 
         ifelse(Stratum == 'South Central', "SC",
         ifelse(Stratum == 'Core', "C", 
         ifelse(Stratum == 'North Central', "NC",
         ifelse(Stratum == 'North', "N", Stratum)))))) %>% rename(strata = Stratum)

dat <- merge(dat, bounds, by = c('strata'), all.x = TRUE)



library(fuzzyjoin)
recruitmen_index <- fuzzy_left_join(ports, dat, 
                by=c("Latitude"="Lat_S", "Latitude"="Lat_N"),
                match_fun=list(`>=`, `<=`))

recruitmen_index <- recruitmen_index %>% dplyr::select('PORT_NAME', 'AGENCY_CODE', 'year', 'Model_Predictions') %>%
  dplyr::rename(LANDING_YEAR = year) %>% unique() %>% drop_na()


write_csv(recruitmen_index, file = "data/SDM/MSQD_recruitmen_index.csv")

