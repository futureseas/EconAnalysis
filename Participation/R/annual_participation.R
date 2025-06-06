# ################################
# ## Annual participation model ##
# ################################
# 
# library(tidyverse)
# library(data.table)
# library(raster)
# library(doParallel)
# library(brms)
# rm(list = ls())
# gc()
# 
# 
# ## Read data (vessel has more than 20 trips in a year to be considered active)
# annual.part <- readRDS(file = "C:/Data/PacFIN data/vessel_part_year.RDS")
# Tickets <- readRDS("C:/Data/PacFIN data/Tickets.rds")
# 
# 
# # ## Check how many years each vessel participate
# # n_years_active <- annual.part %>% group_by(VESSEL_NUM) %>%
# #   summarize(n_years = sum(active_year))
# # n_vessels_per_year <- annual.part %>% group_by(set_year) %>%
# #   summarize(n_years = sum(active_year))
# 
# # # Plor participation
# # ggplot(annual.part.exp, aes(x=set_year, y=VESSEL_NUM, color=active_year)) +
# #   geom_point(size=1)
# 
# 
# 
# #################################
# ### Add explanatory variables ###
# #################################
# 
# 
# #-------------------------------------------
# ## Add Vessel Center of Gravity and Inertia.
# Coords<-read.csv("C:/GitHub/EconAnalysis/Clustering/Port_Zips_1.10.22.csv")
# Coords<-Coords[c(-1,-4)]
# Ticket_coordinates<-merge(Tickets, Coords, by=c("PORT_NAME", "AGENCY_CODE"))
# rm(Tickets)
# Ticket_Coords<-aggregate(AFI_EXVESSEL_REVENUE ~ VESSEL_NUM+PORT_NAME+AGENCY_CODE+Latitude+Longitude+LANDING_YEAR, data=Ticket_coordinates, FUN=sum)
# registerDoParallel(cl = 4)
# getDoParWorkers()
# COG_allyears <- foreach::foreach(ii = 2000:2020, .packages = c('tidyverse', 'raster')) %dopar% {
#   source("C:/GitHub/EconAnalysis/Clustering/CGI_Function.R")
#   Ticket_Coords2 <- Ticket_Coords %>% dplyr::filter(LANDING_YEAR == ii)
#   Permit_ID <- as.data.frame(unique(Ticket_Coords2$VESSEL_NUM))
#   names(Permit_ID)<-"VESSEL_NUM"
#   List<-as.list(as.character(Permit_ID$VESSEL_NUM))
#   Permit_COG<-NULL
#   for (i in 1:length(List)) {
#     Permit = List[i]
#     Single_Permit<- Ticket_Coords2[which(Ticket_Coords2$VESSEL_NUM==Permit),]
#     Single_COG<-cgi(x=Single_Permit$Longitude, y=Single_Permit$Latitude, z=Single_Permit$AFI_EXVESSEL_REVENUE, plot=F)
#     Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2],
#                                      Single_COG$xcg),
#                              lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2],
#                                      Single_COG$ycg),group = c("A", "A", "B", "B","C"))
#     Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
#     Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
#     Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]
#     Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
#     Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
#     Value<-as.data.frame(c(Permit, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B))
#     names(Value)<-c("uniqueid", "LON", "LAT", "DISTANCE_A", "DISTANCE_B")
#     Permit_COG<-rbind(Permit_COG, Value)
#   }
#   Permit_COG$DISTANCE_A <- sub(NaN, 0, Permit_COG$DISTANCE_A)
#   Permit_COG$DISTANCE_B <- sub(NaN, 0, Permit_COG$DISTANCE_B)
#   Permit_COG$DISTANCE_A<-as.numeric(Permit_COG$DISTANCE_A)
#   Permit_COG$DISTANCE_B<-as.numeric(Permit_COG$DISTANCE_B)
#   Permit_COG<-Permit_COG[c(1,3,4)]
#   names(Permit_COG)[1]<-"VESSEL_NUM"
#   Vessel_Geography<-Permit_COG
#   Vessel_Geography$set_year <- ii
#   return(Vessel_Geography)
# }
# COG_data <- plyr::ldply(COG_allyears)
# rm(COG_allyears, Ticket_Coords)
# 
# 
# #-------------------------------------------
# ## Diversity index
# HHI_allyears <- foreach::foreach(ii = 2000:2020,  .packages = c('tidyverse')) %dopar% {
#     Tickets_filt <- Ticket_coordinates %>% filter(LANDING_YEAR == ii)
#     HHI<-aggregate(AFI_EXVESSEL_REVENUE~ PACFIN_SPECIES_COMMON_NAME + VESSEL_NUM, data=Tickets_filt, FUN=sum)
#     HHI<-reshape2::dcast(HHI, VESSEL_NUM ~ PACFIN_SPECIES_COMMON_NAME, value.var="AFI_EXVESSEL_REVENUE", fill=0)
#     rownames(HHI) <- HHI[,1]
#     HHI <- HHI[,-1]
#     HHI<-as.data.frame(vegan::diversity(HHI, index="invsimpson"))
#     HHI$VESSEL_NUM <- rownames(HHI)
#     names(HHI)<-c("diversity_all", "VESSEL_NUM")
#     HHI$diversity_all[which(!is.finite(HHI$diversity_all))] <- 0
#     HHI$set_year <- ii
#     return(HHI)
# }
# HHI_data <- plyr::ldply(HHI_allyears)
# rm(HHI_allyears)
# registerDoSEQ()
# 
# 
# #-------------------------------------------
# ## Get cluster inertia
# PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
# Ticket_clust <- merge(Ticket_coordinates, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# rm(Ticket_coordinates, PAM_Vessel_Groups)
# gc()
# Ticket_clust <- Ticket_clust[!is.na(Ticket_clust$group_all), ]
# source("C:\\GitHub\\EconAnalysis\\Clustering/CGI_Function.R")
# Permit_ID <- as.data.frame(unique(Ticket_clust$group_all))
# names(Permit_ID) <- "group_all"
# List<-as.list(as.character(Permit_ID$group_all))
# Permit_COG<-NULL
# for (i in 1:length(List)) {
#     Permit = List[i]
#     Single_Permit<- Ticket_clust[which(Ticket_clust$group_all==Permit),]
#     Single_COG <- cgi(x=Single_Permit$Longitude, y=Single_Permit$Latitude, z=Single_Permit$AFI_EXVESSEL_REVENUE, plot=F)
#     Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2],
#                                      Single_COG$xcg),
#                              lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2],
#                                      Single_COG$ycg),group = c("A", "A", "B", "B","C"))
#     Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
#     Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
#     Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]
#     Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
#     Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
#     Value<-as.data.frame(c(Permit, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B))
#     names(Value)<-c("uniqueid", "LON", "LAT", "DISTANCE_A", "DISTANCE_B")
#     Permit_COG<-rbind(Permit_COG, Value)
# }
# Permit_COG$DISTANCE_A <- sub(NaN, 0, Permit_COG$DISTANCE_A)
# Permit_COG$DISTANCE_B <- sub(NaN, 0, Permit_COG$DISTANCE_B)
# Permit_COG$DISTANCE_A<-as.numeric(Permit_COG$DISTANCE_A)/1000
# Permit_COG$DISTANCE_B<-as.numeric(Permit_COG$DISTANCE_B)/1000
# Permit_COG<-Permit_COG[c(1,2,3,4)]
# names(Permit_COG)[1]<-"VESSEL_NUM"
# Cluster_Geography <- Permit_COG
# rm(Line_Coord_A, Line_Coord_B, List, Permit, Permit_COG, Permit_ID, Point_Coord,
#    Single_COG, Single_Permit, Value, i, Distance_A, Distance_B)
# gc()
# 
# 
# #-------------------------------------------
# ## Add MSQD Spawn SDM
# SDM_MSQD_Spawn <- read.csv(file = here::here("Landings", "SDM", "MSQD_Spawn_SDM_port_month.csv")) %>%
#   merge(Coords, by=c("PORT_NAME", "AGENCY_CODE"))
# SDM_MSQD <- read.csv(file = here::here("Landings", "SDM", "MSQD_SDM_port_month.csv")) %>%
#   merge(Coords, by=c("PORT_NAME", "AGENCY_CODE"))
# SDM_PSDN <- read.csv(file = here::here("Landings", "SDM", "PSDN_SDM_port_month.csv")) %>%
#   merge(Coords, by=c("PORT_NAME", "AGENCY_CODE"))
# SDM_NANC <- read.csv(file = here::here("Landings", "SDM", "NANC_SDM_port_month.csv")) %>%
#   merge(Coords, by=c("PORT_NAME", "AGENCY_CODE"))
# ports <- SDM_MSQD_Spawn %>%
#     dplyr::select(c("Latitude", "Longitude", "PORT_NAME")) %>%
#     unique()
# registerDoParallel(cl = 4)
# getDoParWorkers()
# SDM_PRICE_cluster <- foreach::foreach(ii = 1:8,
#   .packages = c('tidyverse', 'raster', 'geosphere', 'data.table')) %dopar% {
#   distPorts <- ports %>%
#     mutate(dist = by(., 1:nrow(.), function(row) {
#       distHaversine(c(row$Longitude, row$Latitude), c(row$Longitude, Cluster_Geography[ii,]$LAT))
#       })) %>%
#     mutate(dist = dist / 1000)  %>%
#     dplyr::filter(dist <= Cluster_Geography[ii,]$DISTANCE_A) %>%
#     dplyr::select(c(PORT_NAME)) %>%
#     unique()
#   SDM_MSQD_Spawn_cluster <- setDT(SDM_MSQD_Spawn)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#     group_by(LANDING_YEAR) %>%
#     summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90, na.rm = TRUE)) %>%
#     rename(set_year = LANDING_YEAR) %>%
#     mutate(group_all = ii)
#   SDM_MSQD_cluster <- setDT(SDM_MSQD)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#     group_by(LANDING_YEAR) %>%
#     summarize(MSQD_SDM_90 = mean(SDM_90, na.rm = TRUE)) %>%
#     rename(set_year = LANDING_YEAR) %>%
#     mutate(group_all = ii)
#   SDM_PSDN_cluster <- setDT(SDM_PSDN)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#     group_by(LANDING_YEAR) %>%
#     summarize(PSDN_SDM_60 = mean(SDM_60, na.rm = TRUE)) %>%
#     rename(set_year = LANDING_YEAR) %>%
#     mutate(group_all = ii)
#   SDM_NANC_cluster <- setDT(SDM_NANC)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#     group_by(LANDING_YEAR) %>%
#     summarize(NANC_SDM_20 = mean(SDM_20, na.rm = TRUE)) %>%
#     rename(set_year = LANDING_YEAR) %>%
#     mutate(group_all = ii)
#   Price_MSQD <- Ticket_clust %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, PORT_NAME) %>%
#     dplyr::filter(PACFIN_SPECIES_CODE == "MSQD")
#     Price_MSQD <- setDT(Price_MSQD)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#       group_by(LANDING_YEAR) %>%
#       summarize(MSQD_Price = mean(AFI_PRICE_PER_POUND, na.rm = TRUE)) %>%
#       rename(set_year = LANDING_YEAR) %>%
#       mutate(group_all = ii)
#   Price_PSDN <- Ticket_clust %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, PORT_NAME) %>%
#     dplyr::filter(PACFIN_SPECIES_CODE == "PSDN")
#     Price_PSDN <- setDT(Price_PSDN)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#       group_by(LANDING_YEAR) %>%
#       summarize(PSDN_Price = mean(AFI_PRICE_PER_POUND, na.rm = TRUE)) %>%
#       rename(set_year = LANDING_YEAR) %>%
#       mutate(group_all = ii)
#   Price_NANC <- Ticket_clust %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, PORT_NAME) %>%
#     dplyr::filter(PACFIN_SPECIES_CODE == "NANC")
#     Price_NANC <- setDT(Price_NANC)[PORT_NAME %chin% distPorts$PORT_NAME] %>%
#       group_by(LANDING_YEAR) %>%
#       summarize(NANC_Price = mean(AFI_PRICE_PER_POUND, na.rm = TRUE)) %>%
#       rename(set_year = LANDING_YEAR) %>%
#       mutate(group_all = ii)
#   SDM_PRICE <- merge(SDM_MSQD_Spawn_cluster, SDM_MSQD_cluster, by = (c("group_all", "set_year")))
#   SDM_PRICE <- merge(SDM_PRICE, SDM_PSDN_cluster, by = (c("group_all", "set_year")))
#   SDM_PRICE <- merge(SDM_PRICE, SDM_NANC_cluster, by = (c("group_all", "set_year")))
#   SDM_PRICE <- merge(SDM_PRICE, Price_MSQD, by = (c("group_all", "set_year")))
#   SDM_PRICE <- merge(SDM_PRICE, Price_NANC, by = (c("group_all", "set_year")))
#   SDM_PRICE <- merge(SDM_PRICE, Price_PSDN, by = (c("group_all", "set_year")))
#   return(SDM_PRICE)
# }
# registerDoSEQ()
# SDM_PRICE_data <- plyr::ldply(SDM_PRICE_cluster)
# rm(SDM_MSQD_Spawn, SDM_MSQD, SDM_PSDN, SDM_NANC, SDM_PRICE_cluster, ports, Coords, Cluster_Geography, cgi, Ticket_clust)
# 
# 
# #-------------------------------------------
# ## Include unemployment.
# unem_CA <-
#   readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192133_ae51a3.xlsx", range = "A11:I263") %>%
#   dplyr::select(c('Year', 'Period', 'unemployment rate')) %>%
#   rename(LANDING_YEAR = 'Year') %>%
#   rename(unem_rate_CA = 'unemployment rate') %>%
#   group_by(LANDING_YEAR) %>%
#   summarize(mean.unemployment.CA = mean(unem_rate_CA))
# unem_OR <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192159_36a94e.xlsx", range = "A11:I263") %>%
#   dplyr::select(c('Year', 'Period', 'unemployment rate')) %>%
#   rename(LANDING_YEAR = 'Year') %>%
#   rename(unem_rate_OR = 'unemployment rate') %>%
#   group_by(LANDING_YEAR) %>%
#   summarize(mean.unemployment.OR = mean(unem_rate_OR)) %>%
#   arrange(LANDING_YEAR) %>%
#   dplyr::select(-c(LANDING_YEAR))
# unem_WA <- readxl::read_excel("Participation/Unemployment/SeriesReport-20230609192208_d35a30.xlsx", range = "A11:I263") %>%
#   dplyr::select(c('Year', 'Period', 'unemployment rate')) %>%
#   rename(LANDING_YEAR = 'Year') %>%
#   rename(unem_rate_WA = 'unemployment rate') %>%
#   group_by(LANDING_YEAR) %>%
#   summarize(mean.unemployment.WA = mean(unem_rate_WA)) %>%
#   arrange(LANDING_YEAR) %>%
#   dplyr::select(-c(LANDING_YEAR))
# unem <- cbind(unem_CA, unem_OR, unem_WA) %>%
#   rename(set_year = LANDING_YEAR)
# rm(unem_CA, unem_OR, unem_WA)
# 
# 
# #-------------------------------------------
# ### Include fuel price
# fuel.prices.state <-
#   readxl::read_excel(here::here("Data", "Fuel_prices", "state_averages.xls"), sheet = "state_averages") %>%
#   dplyr::rename(LANDING_YEAR = YEAR) %>%
#   dplyr::rename(LANDING_MONTH = MONTH) %>%
#   dplyr::rename(AGENCY_CODE = STATE) %>%
#   dplyr::rename(diesel.price = avgpricegal) %>%
#   dplyr::select(-c('avgpricettl')) %>%
#   mutate(AGENCY_CODE = ifelse(AGENCY_CODE == 'WA', 'W', ifelse(AGENCY_CODE == 'CA', 'C', ifelse(AGENCY_CODE == 'OR', 'O', 'A')))) %>%
#   group_by(AGENCY_CODE, LANDING_YEAR) %>%
#   summarize(diesel.price.year = mean(diesel.price)) %>%
#   pivot_wider(names_from = AGENCY_CODE, values_from = diesel.price.year) %>%
#   rename(diesel.price.WA = W) %>%
#   rename(diesel.price.CA = C) %>%
#   rename(diesel.price.AK = A) %>%
#   rename(diesel.price.OR = O) %>%
#   rename(set_year = LANDING_YEAR)
# 
# ###
# gc()
# save.image("C:/Users/fequezad/work_save.RData")


##############################################################################################
##############################################################################################


# load("C:/Users/fequezad/work_save.RData")
# 
# 
# ##-----------------------------------
# # Expand data
# annual.part <- complete(annual.part, VESSEL_NUM, set_year) %>%
#   mutate(active_year = ifelse(is.na(active_year), 0, active_year)) %>%
#   mutate(set_year_actual = set_year) %>%
#   mutate(set_year = set_year - 1)
# 
# ##-----------------------------------
# # Merge data
# PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
# annual.part <- annual.part %>%
#   merge(HHI_data, by = (c("VESSEL_NUM", "set_year")), all.x = TRUE, all.y = FALSE) %>%
#   mutate(diversity_all = ifelse(is.na(diversity_all), 0, diversity_all)) %>%
#   arrange(VESSEL_NUM, set_year_actual) %>%
#   merge(COG_data, by = (c("VESSEL_NUM", "set_year")), all.x = TRUE, all.y = FALSE) %>%
#   merge(PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
#   merge(SDM_PRICE_data, by = (c("group_all", "set_year")), all.x = TRUE, all.y = FALSE) %>%
#   merge(unem, by = "set_year", all.x = TRUE, all.y = FALSE) %>%
#   merge(fuel.prices.state, by = "set_year", all.x = TRUE, all.y = FALSE)
#   rm(PAM_Vessel_Groups, HHI_data, COG_data, SDM_PRICE_data, unem, fuel.prices.state)
# 
# 
# ##-----------------------------------
# ### Computing moving averages
# annual.part <- annual.part %>%
#   arrange(VESSEL_NUM, set_year_actual) %>%
#   group_by(VESSEL_NUM) %>%
#   mutate(years_active        = RcppRoll::roll_sum(active_year,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_HHI            = RcppRoll::roll_mean(diversity_all,        5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_COG            = RcppRoll::roll_mean(LAT,                  5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_LI             = RcppRoll::roll_mean(DISTANCE_A,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_NANC_PRICE     = RcppRoll::roll_mean(NANC_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_NANC_SDM       = RcppRoll::roll_mean(NANC_SDM_20,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_PSDN_PRICE     = RcppRoll::roll_mean(PSDN_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_PSDN_SDM       = RcppRoll::roll_mean(PSDN_SDM_60,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_PRICE     = RcppRoll::roll_mean(MSQD_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_SDM       = RcppRoll::roll_mean(MSQD_SDM_90,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_SPAWN_SDM = RcppRoll::roll_mean(MSQD_SPAWN_SDM_90,    5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.CA        = RcppRoll::roll_mean(mean.unemployment.CA, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.CA      = RcppRoll::roll_mean(diesel.price.CA,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.OR        = RcppRoll::roll_mean(mean.unemployment.OR, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.OR      = RcppRoll::roll_mean(diesel.price.OR,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.WA        = RcppRoll::roll_mean(mean.unemployment.WA, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.WA      = RcppRoll::roll_mean(diesel.price.WA,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(set_year_actual>=2005) %>%
#   dplyr::select(c('years_active', 'mean_HHI', 'mean_COG' , 'mean_LI' , 'mean_MSQD_PRICE', 'mean_MSQD_SDM',
#                   'mean_NANC_PRICE', 'mean_NANC_SDM', 'mean_MSQD_SPAWN_SDM', 'mean_PSDN_PRICE', 'mean_PSDN_SDM',
#                   'VESSEL_NUM', 'set_year', 'set_year_actual', 'active_year', 'group_all', 
#                   'mean_unem.CA', 'mean_diesel.CA', 'mean_unem.OR', 'mean_diesel.OR', 'mean_unem.WA', 'mean_diesel.WA'))
# 
# ##-----------------------------------
# ## Filter data
# annual.part <- annual.part %>%
#   group_by(VESSEL_NUM) %>%
#   mutate(n_years = sum(active_year)) %>%
#   ungroup() %>%
#   filter(n_years >= 5)
# 
# ###
# saveRDS(annual.part, "C:/Data/PacFIN data/annual_part.RDS")


######################
### Estimate model ###
######################

library(tidyverse)
library(tidybayes)
library(ROCR)
library(data.table)
library(brms)
rm(list = ls())
gc()

# ## Load data
# annual.part <- readRDS("C:/Data/PacFIN data/annual_part.RDS") %>% 
#     mutate(mean_COG = ifelse(is.na(mean_COG), mean(mean_COG, na.rm=TRUE), mean_COG)) %>%
#     mutate(mean_HHI = ifelse(is.na(mean_HHI), 0, mean_HHI)) %>%
#     mutate(mean_LI  = ifelse(is.na(mean_LI), 0, mean_LI)) %>%
#     mutate(PSDN.Closure = as.factor(ifelse(set_year_actual >= 2015, 1, 0))) %>%
#     mutate(years_active = years_active - active_year) 
# 
# ## Calculate weight averages for prices and SDM
# annual.part <- annual.part %>%
#   mutate(mean_SDM = ifelse(group_all == 4, mean_PSDN_SDM * 0.0608 + mean_MSQD_SDM * 0.9005 + mean_NANC_SDM * 0.0101, 
#                     ifelse(group_all == 5, mean_PSDN_SDM * 0.3132 + mean_MSQD_SDM * 0.5222 + mean_NANC_SDM * 0.0014, 
#                     ifelse(group_all == 6, mean_PSDN_SDM * 0.7960 + mean_MSQD_SDM * 0.0028 + mean_NANC_SDM * 0.1288,
#                     ifelse(group_all == 7, mean_PSDN_SDM * 0.2005 + mean_MSQD_SDM * 0.3092 + mean_NANC_SDM * 0.1927, 
#                            NA))))) %>%
#   mutate(mean_PRICE = ifelse(group_all == 4, mean_PSDN_PRICE * 0.0608 + mean_MSQD_PRICE * 0.9005 + mean_NANC_PRICE * 0.0101,		
#                       ifelse(group_all == 5, mean_PSDN_PRICE * 0.3132 + mean_MSQD_PRICE * 0.5222 + mean_NANC_PRICE * 0.0014,
#                       ifelse(group_all == 6, mean_PSDN_PRICE * 0.7960 + mean_MSQD_PRICE * 0.0028 + mean_NANC_PRICE * 0.1288,
#                       ifelse(group_all == 7, mean_PSDN_PRICE * 0.2005 + mean_MSQD_PRICE * 0.3092 + mean_NANC_PRICE * 0.1927,
#                            NA))))) %>%
#   mutate(mean_unem = ifelse(group_all == 4, 0.93 * mean_unem.CA + 0.01 * mean_unem.OR, 
#                      ifelse(group_all == 5, 0.54 * mean_unem.CA + 0.21 * mean_unem.OR + 0.21 * mean_unem.WA,
#                      ifelse(group_all == 6, 0.44 * mean_unem.OR + 0.55 * mean_unem.WA,
#                      ifelse(group_all == 7, 0.95 * mean_unem.CA,
#                             NA))))) %>%
#   dplyr::filter(group_all == 4 | group_all == 5 | group_all == 6 | group_all == 7) %>% drop_na()
           
           
# ##  Estimate Bayesian model
# 
# logit_all <- brm(active_year ~ mean_SDM + mean_PRICE + mean_unem + PSDN.Closure + mean_HHI + 
#                           (1 | group_all) + (1 | VESSEL_NUM) + (1 | set_year_actual), 
#               data = annual.part, seed = 123, family = bernoulli(link = "logit"), warmup = 1000, 
#               iter = 3000, chain = 4, cores = 4,
#               prior = c(set_prior("lognormal(0,1)", class = "b", coef = "mean_PRICE")),
#               #           set_prior("lognormal(0,1)", class = "b", coef = "mean_PRICE")),
#               control = list(adapt_delta = 0.999))
#             summary(logit_all)
#             saveRDS(logit_all, "logit_all.RDS")
#             
# logit_all_2 <- brm(active_year ~ mean_SDM + mean_unem + PSDN.Closure + mean_HHI + 
#                                (1 | group_all) + (1 | VESSEL_NUM) + (1 | set_year_actual), 
#                              data = annual.part, seed = 123, family = bernoulli(link = "logit"), warmup = 1000, 
#                              iter = 3000, chain = 4, cores = 4,
#                              control = list(adapt_delta = 0.999))
#             summary(logit_all_2)
#             saveRDS(logit_all_2, "logit_all_2.RDS")
# 
# LOO(logit_all, logit_all_2)
            
logit_all_2 <- readRDS("logit_all_2.RDS")


## Check model
pp_check(logit_all_2)

## Check convergence
launch_shinystan(logit_all_2)

### Population parameters ###
mcmc_plot(logit_all_2, regex = TRUE, variable = 
            c("b_mean_SDM",
              "b_mean_unem",
              "b_PSDN.Closure",
              "b_mean_HHI",
              "b_Intercept")) +
  theme(axis.text.y = element_text(hjust = 0)) + scale_y_discrete(
    labels = c(
      "b_mean_SDM" = "mean(AVailability)",
      "b_mean_unem" = "mean(Unemployment)",
      "b_PSDN.Closure1" = "PSDN Closure",
      "b_mean_HHI" = "mean(Diversity)",
      "b_Intercept" = "Intercept"))

### Obtain AUC # 89%
Prob <- predict(logit_all_2, type="response")
Prob <- Prob[,1]
Pred <- ROCR::prediction(Prob, as.vector(pull(annual.part, active_year)))
AUC <- ROCR::performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC


## Plot marginal effects     
conditional_effects(logit_all_2)

