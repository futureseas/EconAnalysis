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
  
  saveRDS(Tickets, "C:/Data/PacFIN data/Tickets_filtered.rds")


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

## How many vessels?
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

#----------------------------------------------------------------------------------------
## Create time variable
Tickets_FF <- Tickets_FF %>%
  mutate(max_days_sea = ifelse(is.na(max_days_sea), 1, max_days_sea)) 
Tickets_FF$set_date_busy <- 
  as.Date(with(Tickets_FF, paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, sep="-")), "%Y-%m-%d")
Tickets_FF <- Tickets_FF %>% mutate(set_date = set_date_busy - days(max_days_sea-1))

#-------------------------------------------------------------------------------
## Keep trip with maximum revenue within a day (only 5% of the data have repeated trips per day)
Tickets_FF_filtered <- Tickets_FF %>%
  group_by(VESSEL_NUM, set_date) %>%
  mutate(max_rev = ifelse(Revenue == max(Revenue, na.rm=TRUE), 1, 0)) %>% ungroup() %>%
  dplyr::filter(max_rev == 1) %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>%
  mutate(LANDING_YEAR = year(set_date)) %>%
  mutate(LANDING_MONTH = month(set_date)) %>%
  mutate(LANDING_DAY = day(set_date))


#---------------------------------------------------------------------------------------
### Expand database to include outside option (when vessel do not have fish ticket.)
Tickets_exp <- complete(Tickets_FF_filtered, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY)
Tickets_exp$set_date <- as.Date(with(Tickets_exp, paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, sep="-")), "%Y-%m-%d") 
Tickets_exp <- Tickets_exp %>% drop_na(set_date)


#---------------------------------------------------------------------------------------
## Identify days that vessel is active but no fish ticket (multidays trips)
Tickets_exp_md <- Tickets_exp %>%
  # Create participation dummy
  group_by(VESSEL_NUM) %>% 
  mutate(set_date_busy =  zoo::na.locf(set_date_busy, na.rm = F)) %>% ungroup() %>%
  mutate(participating = ifelse(set_date <= set_date_busy, 1, 0)) %>%
  mutate(participating = ifelse(is.na(participating), 0, participating)) %>%
  mutate(selection2 = selection) %>% 
  # Carry forward information in multicast trips
  group_by(VESSEL_NUM) %>%
  mutate(selection           = ifelse(participating == 1, zoo::na.locf(selection, na.rm = F), selection)) %>%
  mutate(Landings_mtons      = ifelse(participating == 1, zoo::na.locf(Landings_mtons, na.rm = F), Landings_mtons)) %>%
  mutate(Landings_lbs        = ifelse(participating == 1, zoo::na.locf(Landings_lbs, na.rm = F), Landings_lbs)) %>%
  mutate(Revenue             = ifelse(participating == 1, zoo::na.locf(Revenue, na.rm = F), Revenue)) %>%
  mutate(Price_lbs           = ifelse(participating == 1, zoo::na.locf(Price_lbs, na.rm = F), Price_lbs)) %>%
  mutate(Price_mtons         = ifelse(participating == 1, zoo::na.locf(Price_mtons, na.rm = F), Price_mtons)) %>%
  mutate(max_days_sea        = ifelse(participating == 1, zoo::na.locf(max_days_sea, na.rm = F), max_days_sea)) %>%
  mutate(Vessel.length       = ifelse(participating == 1, zoo::na.locf(Vessel.length, na.rm = F), Vessel.length)) %>%
  mutate(Vessel.weight       = ifelse(participating == 1, zoo::na.locf(Vessel.weight, na.rm = F), Vessel.weight)) %>%
  mutate(Vessel.horsepower   = ifelse(participating == 1, zoo::na.locf(Vessel.horsepower, na.rm = F), Vessel.horsepower)) %>%
  mutate(FTID                = ifelse(participating == 1, zoo::na.locf(FTID, na.rm = F), FTID)) %>%
  mutate(FTID_unique         = ifelse(participating == 1, zoo::na.locf(FTID_unique, na.rm = F), FTID_unique)) %>%
  mutate(AGENCY_CODE         = ifelse(participating == 1, zoo::na.locf(AGENCY_CODE, na.rm = F), AGENCY_CODE)) %>%
  mutate(PORT_AREA_CODE      = ifelse(participating == 1, zoo::na.locf(PORT_AREA_CODE, na.rm = F), PORT_AREA_CODE)) %>%
  mutate(PACFIN_SPECIES_CODE = ifelse(participating == 1, zoo::na.locf(PACFIN_SPECIES_CODE, na.rm = F), PACFIN_SPECIES_CODE)) %>%
  mutate(Species_Dominant    = ifelse(participating == 1, zoo::na.locf(Species_Dominant, na.rm = F), Species_Dominant)) %>%
  ungroup() %>%
  # Create No-Participation choice
  mutate(selection = ifelse(is.na(selection), 'No-Participation', selection)) %>%
  mutate(FTID_unique = ifelse(is.na(FTID_unique), paste('Exp-',1:n()), FTID_unique)) %>% 
  # Create dummy multiday trip
  group_by(VESSEL_NUM, FTID_unique) %>% 
  mutate(n_days_sea = sum(participating)) %>% mutate(n_obs_within_FTID = 1:n()) %>% ungroup() %>% 
  mutate(multiday_trip = ifelse(n_days_sea>1, 1, 0))


#-----------------------------------------------------
### Merge with cluster data
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets_exp_md, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
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
## Add set_year, set_day and set_month 

Tickets_coord <- Tickets_coord %>% 
  dplyr::rename(set_day = LANDING_DAY) %>%
  dplyr::rename(set_month = LANDING_MONTH) %>%
  dplyr::rename(set_year = LANDING_YEAR) %>%
  dplyr::select(
    "VESSEL_NUM", "AGENCY_CODE", 
    "set_date", "set_date_busy", "set_year", "set_month", "set_day", 
    "selection", "PORT_AREA_CODE", "Species_Dominant", 
    "Landings_mtons", "Revenue", "Price_mtons", "max_days_sea",
    "group_all", "Vessel.length", "Vessel.weight", "Vessel.horsepower", 
    "lat", "lon", "lon_logbook", "lon_ca", "lat_logbook", "lat_ca", "multiday_trip", "n_days_sea", "n_obs_within_FTID") 


#---------------------------------------------------------------------------------------
## Create data to filter non-participation

library(fpp2)           # working with time series data
library(zoo)            # working with time series data

n_days_participation = 30
n_days_prev_participation = n_days_participation / 2

participation_data_all <- Tickets_coord %>% 
  mutate(partDummy = ifelse(selection == "No-Participation", 0, 1)) %>%
  dplyr::select(VESSEL_NUM, set_date, set_year, partDummy, group_all) %>% unique() %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>% group_by(VESSEL_NUM) %>%
  mutate(participation_ndays = RcppRoll::roll_sum(partDummy, n_days_participation, fill = NA, align = "center", na.rm = TRUE)) %>%
  mutate(participation_prev_ndays = RcppRoll::roll_sum(partDummy, n_days_prev_participation, fill = NA, align = "right", na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(is.na(participation_ndays) == 0) %>%
  filter(is.na(participation_prev_ndays) == 0) %>%
  mutate(partDummy = as.factor(partDummy))


# # ----------
# # Plots with filtered data
# 
# cluster <- 4
# ndays_filter <- 5
# ndays_filter_t <- ndays_filter
# 
# participation_filtered <- participation_data_all %>%
#   filter(group_all == cluster) %>%
#   mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
#   mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
#   mutate(Dfilter = ifelse(partDummy == 0 & participation_prev_ndays == 0, 0, Dfilter)) %>%
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
#   xlab("Days participating within a month") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle(paste0("At least ",
#     paste0(ndays_filter,
#     paste0(" days participating within a month (",
#     paste0(formatC(nrow(participation_filtered), format="d", big.mark=","), " obs)")))),
#     #subtitle = paste0("...and ", paste0(ndays_filter_t, " days participating for rows with tickets"))
#     )

#-----------------------------------------------
## filter non-participation

ndays_filter <- 5 ## For rows with no-participation
ndays_filter_t <- ndays_filter ## For row with tickets

participation_filtered <- participation_data_all %>%
  mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
  mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
  mutate(Dfilter = ifelse(partDummy == 0 & participation_prev_ndays == 0, 0, Dfilter)) %>%
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

### How many row have coordinates? 
# ticket_part_2 <- Tickets_dist %>% dplyr::filter(selection != "No-Participation")
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(dist)))/nrow(ticket_part_2))
# ticket_part_2 %>% mutate(dist = ifelse(dist > 220, NA, dist)) %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(dist)))/nrow(ticket_part_2))
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(lat_logbook)))/nrow(ticket_part_2))
# ticket_part_2 %>% summarize(perc = (nrow(ticket_part_2)-sum(is.na(lat_ca)))/nrow(ticket_part_2))

Tickets_dist <- Tickets_dist %>% mutate(dist = ifelse(dist > 220*max_days_sea, NA, dist))

#------------------------------------------------------
### Details before savings (e.g. create previous day, trip_id. Make vessel characteristics numeric)

Tickets_final <- Tickets_dist %>%
  mutate(Vessel.length = as.numeric(Vessel.length),
         Vessel.weight = as.numeric(Vessel.weight),
         Vessel.horsepower = as.numeric(Vessel.horsepower)) %>%
  mutate(Revenue = ifelse(selection == "No-Participation", 0, Revenue)) %>%
  mutate(dist = ifelse(selection == "No-Participation", 0, dist)) %>%
  mutate(max_days_sea = ifelse(selection == "No-Participation", 0, max_days_sea)) %>%
  mutate(Landings_mtons = ifelse(selection == "No-Participation", 0, Landings_mtons))

Tickets_final$prev_days_date <- Tickets_final$set_date - days(1)
Tickets_final$trip_id <- udpipe::unique_identifier(Tickets_final, fields = c('VESSEL_NUM', 'set_date'))


#-------------------------------------------------------------------------------
## Filter participation database ##

## Keep one trip per day!

Tickets_final <- Tickets_final  %>%
  distinct_at(vars(-trip_id)) %>%
  group_by(VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>%
  mutate(lat_mean = mean(lat, na.rm = TRUE),
         lon_mean = mean(lon, na.rm = TRUE),
         dist_mean = mean(dist, na.rm = TRUE)) %>%
  mutate(lat = ifelse(ncount>1, lat_mean, lat)) %>%
  mutate(lon = ifelse(ncount>1, lon_mean, lon)) %>%
  mutate(dist = ifelse(ncount>1, dist_mean, dist)) %>%
  dplyr::select(-c('lat_mean', 'lon_mean', 'dist_mean',
                   'lat_logbook', 'lon_logbook',
                   'lat_ca', 'lon_ca')) %>% 
  ungroup() %>%
  distinct() %>%
  group_by(VESSEL_NUM, set_date) %>%
  mutate(order = seq(1:n())) %>% 
  ungroup() %>%
  filter(order == 1) %>%
  dplyr::select(-c('order', 'ncount')) 

Tickets_final$trip_id <- udpipe::unique_identifier(Tickets_final, fields = c('VESSEL_NUM', 'set_date'))



#------------------------------------------------------
### Save participation data

saveRDS(Tickets_final, "C:\\Data\\PacFIN data\\participation_data.rds")
#Tickets_final <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")


# #################################
# ### Daily participation graph ###
# #################################
# 
# df.subs <- Tickets_final %>% 
#   dplyr::select(c(VESSEL_NUM, Species_Dominant, selection, set_month,
#                   set_date, set_year, group_all)) %>%
#   dplyr::filter(set_year >= 2013, set_year <= 2016, 
#                 #set_month ==10, 
#                 group_all == 4) %>%
#  dplyr::filter(VESSEL_NUM == "648720" | VESSEL_NUM == "984694" |
#                VESSEL_NUM == "942575" | VESSEL_NUM == "602455") %>% drop_na() 
# 
# ggplot(df.subs, aes(x=set_date, y=VESSEL_NUM, color=Species_Dominant)) + 
#   geom_point(size=1) 


# #####################################################
# ## Distance to catch areas for CMCK, JMCK and PHRG ##
# #####################################################

# avg.dist <- Tickets_final %>%
#   dplyr::filter(n_obs_within_FTID == 1) %>%
#   dplyr::select(Species_Dominant, trip_id, dist) %>%
#   dplyr::filter(Species_Dominant == "CMCK") %>%
#   unique() %>% drop_na(dist)
#   psych::describe(avg.dist$dist)
# 
# avg.dist <- Tickets_final %>%
#   dplyr::filter(n_obs_within_FTID == 1) %>%
#   dplyr::select(Species_Dominant, trip_id, dist) %>%
#   dplyr::filter(Species_Dominant == "JMCK") %>%
#   unique() %>% drop_na(dist)
#   psych::describe(avg.dist$dist)
# 
# avg.dist <- Tickets_final %>%
#   dplyr::filter(n_obs_within_FTID == 1) %>%
#   dplyr::select(Species_Dominant, trip_id, dist) %>%
#   dplyr::filter(Species_Dominant == "PHRG") %>%
#   unique() %>% drop_na(dist)
#   psych::describe(avg.dist$dist)


# #################################
# ## Species with multiday trips ##
################################### 
#
# days_sea <- Tickets_final %>%
#   dplyr::filter(n_obs_within_FTID == 1)   %>%
#   dplyr::select(Species_Dominant, max_days_sea, trip_id) %>%
#   dplyr::filter(max_days_sea > 1) %>%
#   unique() %>% 
#   mutate(Total_obs = n(), obs = 1)  %>% 
#   group_by(Species_Dominant) %>%
#   summarize(Total_obs = mean(Total_obs), obs = sum(obs),
#    avg.days.sea = mean(max_days_sea)) %>%
#   mutate(perc = scales::percent(obs/Total_obs)) 
# 
# library("googlesheets4")
# gs4_auth(
#   email = "fequezad@ucsc.edu",
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL)
# 
# gs4_create("days_sea", sheets = days_sea)


# #######################
# ## Repeated trip_id? ##
# #######################
# unique_tickets <- Tickets_final %>%
#   dplyr::select(trip_id) %>%
#   group_by(trip_id) %>%
#   summarize(n_count = n()) %>% ungroup()
# 
# Tickets_check <- Tickets_final %>% 
# group_by(trip_id) %>% 
# summarize(n_obs = n()) %>%
#   ungroup() %>% filter(n_obs>1)


# ################
# ## Day at sea ##
# ################
# 
# ticket_part <- participation_data %>% dplyr::filter(selection != "No-Participation")
# hist(ticket_part$max_days_sea, breaks = max(ticket_part$max_days_sea))

