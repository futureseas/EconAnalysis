# Initial setup ####

## Clear working space ####
rm(list=ls())
gc()

## Load packages ####

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)
library(tidyverse)  

setwd("C:/GitHub/EconAnalysis")

# Landings by clusters ####

## Obtain PacFIN data ####
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

## Obtain cluster by specific vessel ####
PAM_Vessel_Groups <- read.csv("C:\\Data\\Cluster data\\PAM_Vessel_Groups.csv")
Tickets_raw <- merge(Tickets_raw, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)
Tickets_raw <- Tickets_raw %>% mutate(group_all = ifelse(is.na(group_all), 9, group_all))
colnames(Tickets_raw)

## Aggregate data ####
Tickets_clusters_agg <- Tickets_raw %>% 
  group_by(LANDING_YEAR, LANDING_MONTH, group_all, PACFIN_SPECIES_CODE, PACFIN_PORT_CODE, VESSEL_NUM) %>%
  summarize(total_vessel_revenue = sum(AFI_EXVESSEL_REVENUE, na.rm = TRUE), 
            total_vessel_landings = sum(LANDED_WEIGHT_MTONS, na.rm = TRUE)) %>%
  group_by(LANDING_YEAR, LANDING_MONTH, group_all, PACFIN_SPECIES_CODE, PACFIN_PORT_CODE) %>%
  summarize(total_revenue = sum(total_vessel_revenue, na.rm  = TRUE), 
            total_landings = sum(total_vessel_landings, na.rm = TRUE),
            mean_revenue = mean(total_vessel_revenue, na.rm = TRUE),
            mean_landings = mean(total_vessel_landings, na.rm = TRUE))

# Save data ####
write.csv(Tickets_clusters_agg,"G:\\My Drive\\Data\\Cluster\\cluster_aggregates.csv", row.names = FALSE)


