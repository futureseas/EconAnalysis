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
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

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


#-----------------------------------------------------
### Find the dominant species by value of each fishing trip (i.e., target species). 

Boats <- dcast(Tickets, FTID_unique ~ PACFIN_SPECIES_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID_unique
FTID_unique<-Boats$FTID_unique
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)]) # Indicate the name of the column (PACFIN_SPECIES_CODE) with the highest "AFI_EXVESSEL_REVENUE" 
colnames(X)<-"Species_Dominant"
Trip_Species_Dominant<-as.data.frame(cbind(FTID_unique,X))
Tickets<-merge(Tickets, Trip_Species_Dominant, by='FTID_unique')
rm(Trip_Species_Dominant, X, Boats,FTID_unique)


#-----------------------------------------------------
### Compute vessel characteristics

# nrow(Tickets %>% 
#   dplyr::select(c(VESSEL_NUM)) %>% 
#   drop_na %>% 
#   unique())

Vessel.chr.lenght <- Tickets %>% 
  dplyr::select(c(VESSEL_NUM, VESSEL_LENGTH, LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>% 
  drop_na %>% 
  unique() %>%
  group_by(VESSEL_NUM) %>% 
  summarize(Vessel.length = names(which.max(table(VESSEL_LENGTH)))) %>% 
  unique() %>% ungroup() 

Vessel.chr.weight <- Tickets %>% 
  dplyr::select(c(VESSEL_NUM, VESSEL_WEIGHT, LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>% 
  drop_na %>% 
  unique() %>%
  group_by(VESSEL_NUM) %>% 
  summarize(Vessel.weight = names(which.max(table(VESSEL_WEIGHT)))) %>% 
  unique() %>% ungroup()

Vessel.chr.horsepower <- Tickets %>% 
  dplyr::select(c(VESSEL_NUM, VESSEL_HORSEPOWER, LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>% 
  drop_na %>% 
  unique() %>%
  group_by(VESSEL_NUM) %>% 
  summarize(Vessel.horsepower = names(which.max(table(VESSEL_HORSEPOWER)))) %>% 
  unique() %>% ungroup()


#-----------------------------------------------------
### Aggregate species in a FTID_unique
Tickets <- Tickets %>% group_by(AGENCY_CODE, FTID_unique, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_AREA_CODE,
                                VESSEL_NUM, PACFIN_SPECIES_CODE, Species_Dominant) %>% 
  summarize(Landings_mtons = sum(LANDED_WEIGHT_MTONS),
            Landings_lbs = sum(LANDED_WEIGHT_LBS),
            Revenue  = sum(AFI_EXVESSEL_REVENUE),
            Price_lbs = mean(AFI_PRICE_PER_POUND),
            Price_mtons = mean(AFI_PRICE_PER_MTONS),
            max_days_sea = max(NUM_OF_DAYS_FISHED)) %>%
  mutate(dDelete = ifelse(FTID == "142301E" & LANDING_YEAR == 2020, 1, 0)) %>%
  filter(dDelete == 0) %>% dplyr::select(-c(dDelete)) %>% ungroup()

#-------------------------------------------------------------------------------------
### Subset to select only records where one of the CPS was the target species
### (squid, sardine, mackerel, anchovy, or albacore using anchovy as bait)

FF_Tickets<-Tickets[which((Tickets$Species_Dominant == "PSDN" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "MSQD" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "NANC" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "CMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "JMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "UMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "ALBC" &
                           Tickets$PACFIN_SPECIES_CODE == "NANC")),]

## Aggregate mackerels in one category
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "CMCK"] <- "OMCK")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "JMCK"] <- "OMCK")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "UMCK"] <- "OMCK")

###Creating a filter here to only retain vessels with more than 10 forage fish landings (tickets where FF is the dominant species)
FTID_Value<-aggregate(Revenue ~ FTID_unique + VESSEL_NUM, FUN=sum, data=FF_Tickets)
FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 10)), ]
FF_Tickets<-setDT(FF_Tickets)[VESSEL_NUM %chin% FTID_Value$VESSEL_NUM]
FF_Tickets<-as.data.frame(FF_Tickets)

# FF_Tickets indicate tickets where FF are dominant in the trip, but still have landings for other species.
FF_Vessels<-as.data.frame(unique(FTID_Value$VESSEL_NUM))
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"


#-----------------------------------------------------
### Filter rows with non-zero revenues and only keep the species dominant (no by-catch)

Tickets<-Tickets[which(Tickets$Revenue > 0),] # 137,971 row deleted
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant) %>% # 44,035 row deleted 
  mutate(selection = paste(PORT_AREA_CODE, Species_Dominant, sep = "-", collapse = NULL)) 


#-----------------------------------------------------
### Merge vessel characteristics
Tickets_chr <- merge(Tickets, Vessel.chr.lenght, by = c("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
Tickets_chr <- merge(Tickets_chr, Vessel.chr.weight, by = c("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
Tickets_chr <- merge(Tickets_chr, Vessel.chr.horsepower, by = c("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)

saveRDS(Tickets_chr, "C:/Data/PacFIN data/Tickets_filtered.rds")


#---------------------------------------------------------------------
## Calculate CPUE, standardize within species and transform to tanh()
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
Catch.per.trip <- Tickets_chr %>% 
  dplyr::filter(selection != "No-Participation") %>% 
  dplyr::select(c('VESSEL_NUM', 'FTID_unique', 'Species_Dominant', 
                  'LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 
                  'Landings_mtons', 'Vessel.weight', 'PORT_AREA_CODE', 
                  'max_days_sea')) %>% 
  mutate(max_days_sea = ifelse(is.na(max_days_sea), 1, max_days_sea)) %>% 
  mutate(size = as.numeric(Vessel.weight)) %>% drop_na() %>% 
  group_by(Species_Dominant, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_AREA_CODE) %>% 
  summarize(CPUE = sum(Landings_mtons, na.rm = TRUE)/(sum(size*max_days_sea, na.rm = TRUE))) %>%
  ungroup() %>% group_by(Species_Dominant) %>%
  mutate(mean.CPUE = mean(CPUE, na.rm = TRUE),
         sd.CPUE = sd(CPUE, na.rm = TRUE)) %>% ungroup() %>%
  mutate(CPUE.z = (CPUE - mean.CPUE)/sd.CPUE)%>%
  mutate(CPUE_index = sigmoid(CPUE.z)) %>%
  dplyr::select(-c('mean.CPUE', 'sd.CPUE', 'CPUE', 'CPUE.z'))

saveRDS(Catch.per.trip, "C:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds")


#----------------------------------------------------------------------------------------
###Subset from the complete data set to only retain records associated with these Vessels
###Remove records associated with landings of zero value; this is likely bycatch

Tickets_FF<-setDT(Tickets_chr)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM] # 1,927,235 row deleted
Tickets_FF<-as.data.frame(Tickets_FF)
rm(FF_Vessels, FTID_Value)

#---------------------------------------------------------------------------------------
### Expand database to include outside option (when vessel do not have fish ticket.)
Tickets_exp <- complete(Tickets_FF, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
  mutate(selection = ifelse(is.na(selection), 'No-Participation', selection)) %>%
  mutate(FTID_unique = ifelse(is.na(FTID_unique), paste('NP-',1:n()), FTID_unique))


#-----------------------------------------------------
### Merge with cluster data
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets_exp, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)
Tickets_clust <- Tickets_clust[!is.na(Tickets_clust$group_all), ]

#-----------------------------------------------
### Merge data to fishing locations

loogbook_coord <- readRDS("C:/Data/Logbooks/logbooks_FTID.rds")
catch_area_coord <- readRDS("C:/GitHub/EconAnalysis/Participation/BlockAreas/catchareas_FTID.rds")

Tickets_coord <- merge(Tickets_clust, loogbook_coord, 
                     by = (c('FTID', 'PACFIN_SPECIES_CODE')),
                     all.x = TRUE, all.y = FALSE)

Tickets_coord <- merge(Tickets_coord, catch_area_coord, 
                       by = (c('FTID', 'PACFIN_SPECIES_CODE')),
                       all.x = TRUE, all.y = FALSE)

Tickets_coord <-  Tickets_coord %>%
  mutate(lon = ifelse(is.na(lon_logbook), lon_ca, lon_logbook)) %>%
  mutate(lat = ifelse(is.na(lat_logbook), lat_ca, lat_logbook))

#---------------------------------------------------------------------------------------------------------------------------------------------------------
## Clean dataset for discrete choice model  
## Add unique trip_ID, set_date, set_year, set_day and set_month 
## (exclude weird period from expanding data)

Tickets_coord$trip_id <- udpipe::unique_identifier(Tickets_coord, fields = c("FTID_unique"))


### Create date variable
Tickets_coord$set_date <- 
  as.Date(with(Tickets_coord, paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, sep="-")), 
          "%Y-%m-%d")
Tickets_coord$prev_days_date <- Tickets_coord$set_date - days(1)

Tickets_coord <- Tickets_coord %>% drop_na(set_date) %>% 
  dplyr::rename(set_day = LANDING_DAY) %>%
  dplyr::rename(set_month = LANDING_MONTH) %>%
  dplyr::rename(set_year = LANDING_YEAR) %>%
  dplyr::select(
    "VESSEL_NUM", "AGENCY_CODE", "trip_id", 
    "set_date", "set_year", "set_month", "set_day", "prev_days_date",
    "selection", "PORT_AREA_CODE", "Species_Dominant", 
    "Landings_mtons", "Revenue", "Price_mtons", "max_days_sea",
    "group_all", "Vessel.length", "Vessel.weight", "Vessel.horsepower", 
    "lat", "lon", "lon_logbook", "lon_ca", "lat_logbook", "lat_ca") 



#---------------------------------------------------------------------------------------
## Create data to filter non-participation

library(fpp2)           # working with time series data
library(zoo)            # working with time series data

n_days_participation = 365

participation_data_all <- Tickets_coord %>% 
  mutate(CPS_revenue = 
    ifelse(Species_Dominant == "PSDN", Revenue,
    ifelse(Species_Dominant == "NANC", Revenue,
    ifelse(Species_Dominant == "MSQD", Revenue,
    ifelse(Species_Dominant == "PSDN", Revenue,
    ifelse(Species_Dominant == "CMCK", Revenue,
    ifelse(Species_Dominant == "JMCK", Revenue,
    ifelse(Species_Dominant == "UMCK", Revenue, 0)))))))) %>%
  mutate(CPS_revenue = ifelse(selection == "No-Participation", 0, CPS_revenue)) %>% 
  mutate(partDummy = ifelse(selection == "No-Participation", 0, 1)) %>%
  dplyr::select(VESSEL_NUM, set_date, set_year, partDummy, CPS_revenue, Revenue, group_all) %>% unique() %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>%
  group_by(VESSEL_NUM) %>%
  mutate(participation_ndays = rollsum(partDummy, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  mutate(CPS_revenue_MA = rollsum(CPS_revenue, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  mutate(Revenue_MA = rollsum(Revenue, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  ungroup() %>% 
  filter(is.na(participation_ndays) == 0) %>%
  mutate(perc_CPS = CPS_revenue_MA / Revenue_MA) %>%
  mutate(partDummy = as.factor(partDummy))


# ----------
## Plots with filtered data 

# cluster <- 5
# ndays_filter <- 90
# ndays_filter_t <- 30
# 
# participation_filtered <- participation_data_all %>% 
#   #filter(group_all == cluster) %>%
#   mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
#   mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
#   filter(Dfilter == 1) %>%
#   dplyr::arrange(VESSEL_NUM, set_date)
# 
# perc <- participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
# x1 <- trunc(perc$perc[1]*100*10^2)/10^2
# x2 <- trunc(perc$perc[2]*100*10^2)/10^2
# 
# library(hrbrthemes)
# ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   ylab("Number of observations") +
#   xlab("Days participating within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle(paste0("At least ",
#     paste0(ndays_filter,
#     paste0(" participating within a year (",
#     paste0(formatC(nrow(participation_filtered), format="d", big.mark=","), " obs)")))),
#     subtitle = paste0("...and ", paste0(ndays_filter_t, " days participating for rows with tickets")))
#
# ggplot(data=participation_filtered, aes(x=Revenue_MA, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   xlab("Revenue within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle("Revenue within year?")
# 
# ggplot(data=participation_filtered, aes(x=perc_CPS, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   xlab("Days participating within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle("Revenue share from CPS within year?")


#-----------------------------------------------
## filter non-participation

ndays_filter <- 90 ## For rows with no-participation
ndays_filter_t <- 30 ## For row with tickets

participation_filtered <- participation_data_all %>%
  mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
  mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
  filter(Dfilter == 1) %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>%
  dplyr::select(VESSEL_NUM, set_date) %>% 
  unique() %>% mutate(filter = 1)

Tickets_part <- 
  merge(Tickets_coord, participation_filtered,
        by = c("VESSEL_NUM", "set_date"),
        all.x = TRUE, all.y = FALSE) %>% 
  filter(filter == 1) %>% 
  dplyr::select(-c("filter"))


#------------------------------------------------
### Calculate distances from Ports to catch areas

## Get PORT_AREA_CODE coordinates.
# Load port georeferenced data
ports <- read_csv("Data/Ports/port_areas.csv") %>% 
  drop_na() %>%
  rename(PORT_AREA_CODE = port_group_code) %>%
  rename(lat_port = lat) %>%
  rename(lon_port = lon) %>%
  dplyr::select(-c(port_group_name))

# Compare port distances
Tickets_dist <- merge(Tickets_part, ports, by = c("PORT_AREA_CODE"), all.x = TRUE, all.y = FALSE) 
gc()
Tickets_dist <- Tickets_dist %>% rowwise() %>%
  mutate(dist = ifelse(is.na(lat), NA, geosphere::distm(c(lon_port, lat_port), c(lon, lat), fun = distHaversine))) %>%
  mutate(dist = dist/1000) %>% ungroup()

### How many row have coordinates? 91%!
# ticket_part_2 <- Tickets_dist %>% dplyr::filter(selection != "No-Participation")
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(dist)))/nrow(ticket_part_2))
# ticket_part_2 %>% mutate(dist = ifelse(dist > 220, NA, dist)) %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(dist)))/nrow(ticket_part_2))
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(lat_logbook)))/nrow(ticket_part_2))
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(lat_ca)))/nrow(ticket_part_2))

Tickets_final <- Tickets_dist %>% mutate(dist = ifelse(dist > 220, NA, dist))
hist(Tickets_final$dist)

#------------------------------------------------------
### Save participation data

saveRDS(Tickets_final, "C:\\Data\\PacFIN data\\participation_data.rds")
# 
# ########################################
# unique_tickets <- Tickets_final %>%
#   dplyr::select(trip_id) %>%
#   group_by(trip_id) %>%
#   summarize(n_count = n()) %>% ungroup()
# 
# Tickets_check <- Tickets_final %>% group_by(trip_id) %>% summarize(n_obs = n()) %>%
#   ungroup() %>% filter(n_obs>1)
# #######################################
