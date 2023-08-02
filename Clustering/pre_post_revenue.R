
###Load packages and set working directory
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)

###I run these lines as well as they are packages I frequently use that can interfere with some of the processes below
# detach(package:raster, unload=TRUE)
# detach(package:igraph, unload=TRUE)

rm(list=ls())
gc()
setwd("C:/GitHub/EconAnalysis/Clustering")


# ----------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 
Tickets <- dplyr::select(Tickets, c(LANDING_YEAR, VESSEL_NUM, AFI_EXVESSEL_REVENUE))



#--------------------------
## Load cluster groups

new_entrants_ID <- readRDS(here::here("Clustering", "new_entrants_ID.rds"))
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")

Tickets.c <- merge(Tickets, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
Tickets.cluster <- merge(Tickets.c, new_entrants_ID, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)


#--------------------------
## Create pre and after closure database
Ticket.cluster.pre <- 
  Tickets.cluster %>% filter(LANDING_YEAR < 2015) %>% filter(LANDING_YEAR >= 2005) %>% dplyr::select(-c(group_all_new)) %>% drop_na()
Ticket.cluster.post <- 
  Tickets.cluster %>% filter(LANDING_YEAR >= 2015) %>% dplyr::select(-c(group_all_new)) %>% drop_na()
Ticket.cluster.post.new_entrants <- 
  Tickets.cluster %>% filter(LANDING_YEAR >= 2015) %>% mutate(group_all = ifelse(is.na(group_all), group_all_new, group_all)) %>% 
  dplyr::select(-c(group_all_new)) %>% drop_na()

rm(new_entrants_ID, PAM_Vessel_Groups, Tickets, Tickets.c, Tickets.cluster)




#--------------------------
## Create tables
Table.pre <- Ticket.cluster.pre %>% 
  group_by(group_all, LANDING_YEAR, VESSEL_NUM) %>% 
  summarize(Revenue = sum(AFI_EXVESSEL_REVENUE)) %>% 
  dplyr::filter(Revenue > 0) %>%
  group_by(VESSEL_NUM) %>% 
  mutate(rev_per_vessel = sum(Revenue, na.rm = TRUE)/n()) %>% ungroup() %>%
  group_by(group_all) %>% 
  mutate(rev_per_vessel1 = mean(rev_per_vessel, na.rm = TRUE)) %>% ungroup() %>%
  group_by(group_all, LANDING_YEAR) %>% 
  dplyr::summarise(Revenue_sum = sum(Revenue), n_vessel = n(), rev_per_vessel2 = mean(rev_per_vessel, na.rm = TRUE)) %>% 
  group_by(group_all) %>% 
  summarize(Revenue = mean(Revenue_sum), mean_n_vessel = mean(n_vessel), rev_vessel = mean(rev_per_vessel2, na.rm = TRUE))

Table.post <- Ticket.cluster.post  %>% 
  group_by(group_all, LANDING_YEAR, VESSEL_NUM) %>% 
  summarize(Revenue = sum(AFI_EXVESSEL_REVENUE)) %>% 
  dplyr::filter(Revenue > 0) %>%
  group_by(VESSEL_NUM) %>% 
  mutate(rev_per_vessel = sum(Revenue, na.rm = TRUE)/n()) %>% ungroup() %>%
  group_by(group_all) %>% 
  mutate(rev_per_vessel1 = mean(rev_per_vessel, na.rm = TRUE)) %>% ungroup() %>%
  group_by(group_all, LANDING_YEAR) %>% 
  dplyr::summarise(Revenue_sum = sum(Revenue), n_vessel = n(), rev_per_vessel2 = mean(rev_per_vessel, na.rm = TRUE)) %>% 
  group_by(group_all) %>% 
  summarize(Revenue = mean(Revenue_sum), mean_n_vessel = mean(n_vessel), rev_vessel = mean(rev_per_vessel2, na.rm = TRUE))

Table.post.new_entrants <- Ticket.cluster.post.new_entrants  %>% 
  group_by(group_all, LANDING_YEAR, VESSEL_NUM) %>% 
  summarize(Revenue = sum(AFI_EXVESSEL_REVENUE)) %>% 
  dplyr::filter(Revenue > 0) %>%
  group_by(VESSEL_NUM) %>% 
  mutate(rev_per_vessel = sum(Revenue, na.rm = TRUE)/n()) %>% ungroup() %>%
  group_by(group_all) %>% 
  mutate(rev_per_vessel1 = mean(rev_per_vessel, na.rm = TRUE)) %>% ungroup() %>%
  group_by(group_all, LANDING_YEAR) %>% 
  dplyr::summarise(Revenue_sum = sum(Revenue), n_vessel = n(), rev_per_vessel2 = mean(rev_per_vessel, na.rm = TRUE)) %>% 
  group_by(group_all) %>% 
  summarize(Revenue = mean(Revenue_sum), mean_n_vessel = mean(n_vessel), rev_vessel = mean(rev_per_vessel2, na.rm = TRUE))









  
 


