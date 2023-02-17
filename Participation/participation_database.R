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
###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets_raw, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PORT_AREA_CODE,
                                 VESSEL_NUM, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE,
                                 PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME,
                                 VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, FISHER_LICENSE_NUM, AFI_PRICE_PER_POUND, 
                                 VESSEL_OWNER_ADDRESS_CITY, REMOVAL_TYPE_CODE, NUM_OF_DAYS_FISHED, CATCH_AREA_CODE)) %>% 
  filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 



#-----------------------------------------------------
####Find the dominant species by value of each fishing trip ( = target species). 

Boats <- dcast(Tickets, FTID ~ PACFIN_SPECIES_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)]) # Indicate the name of the column (PACFIN_SPECIES_CODE) with the highest "AFI_EXVESSEL_REVENUE" 
colnames(X)<-"Species_Dominant"
Trip_Species_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Species_Dominant, by='FTID')
rm(Trip_Species_Dominant, X, Boats)

# Filter relevant row (targeted species in dominant port)
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant) 


#-----------------------------------------------------
### Aggregate species in a FTID 
Tickets <- Tickets %>% group_by(FTID, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PACFIN_SPECIES_COMMON_NAME,
                                 Species_Dominant, PORT_AREA_CODE) %>% 
  summarize(Landings_lbs = sum(LANDED_WEIGHT_LBS), 
            Revenue  = sum(AFI_EXVESSEL_REVENUE),
            Price_lbs = mean(AFI_PRICE_PER_POUND))

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
                           Tickets$PACFIN_SPECIES_COMMON_NAME == "NORTHERN ANCHOVY")),]

## Aggregate mackerels in one category
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "CMCK"] <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "JMCK"] <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "UMCK"] <- "MACKEREL")


###Creating a filter here to only retain vessels with more than 10 forage fish landings (tickets where FF is the dominant species)
FTID_Value<-aggregate(Revenue ~ FTID + VESSEL_NUM, FUN=sum, data=FF_Tickets)
FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 10)), ]
FF_Tickets<-setDT(FF_Tickets)[VESSEL_NUM %chin% FTID_Value$VESSEL_NUM]
FF_Tickets<-as.data.frame(FF_Tickets)

# FF_Tickets indicate tickets where FF are dominant in the trip, but still have landings for other species.

###Find the list of unique vessels in the subset, these are the vessels we will cluster
FF_Vessels<-as.data.frame(unique(FTID_Value$VESSEL_NUM))
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"


#----------------------------------------------------------------------------------------
###Subset from the complete data set to only retain records associated with these Vessels
###Remove records associated with landings of zero value; this is likely bycatch

Tickets<-Tickets[which(Tickets$Revenue > 0),]
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]
Tickets<-as.data.frame(Tickets)
rm(FF_Vessels, FTID_Value)

#-----------------------------------------------------
### Create port-species choice
Tickets <- Tickets %>% 
  mutate(selection = paste(PORT_AREA_CODE, Species_Dominant, sep = "-", collapse = NULL)) %>%
  mutate(dDelete = ifelse(FTID == "142301E" & LANDING_YEAR == 2020, 1, 0)) %>%
           filter(dDelete == 0) %>% select(-c(dDelete))

# Tickets_FTID <- Tickets %>% filter(FTID == "142301E")
# Tickets_check <- Tickets %>% group_by(FTID) %>% summarize(n_obs = n())


### How many vessels?
Tickets %>% select('VESSEL_NUM') %>% unique() %>% summarize(n_vessels = n())


#-----------------------------------------------
### Merge data to SDM 

#... (STILL COMPUTING SDMs Daily)




#-----------------------------------------------------
### Expand database 
### Include outside option? Then, expand data when variables are not observed.

# #### Se first how many trips per day
# n_trips_per_day <- Tickets_clust_2 %>% 
#   dplyr::select('VESSEL_NUM', 'FTID', 'LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY') %>% 
#   unique() %>% 
#   group_by(VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   summarize(n_trips = n()) 
# 
# hist(n_trips_per_day$n_trips, 
#      main = '', 
#      xlab	= 'Number of trips per day')


#---------------------------------------------------------------------------
# Include outside option only when vessel participate in the fishery during 
#---------------------------------------------------------------------------
library(tidyr)
Tickets_exp <- complete(Tickets, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
  mutate(selection = ifelse(is.na(selection), 'No-Participation', selection)) %>%
  mutate(FTID = ifelse(is.na(FTID), paste('NP-',1:n()), FTID))

  Tickets_3 <- Tickets_3 %>% group_by(FTID) %>% mutate(n_obs = n()) %>% 
    ungroup() %>% filter(n_obs==1)


# #-----------------------------------------------------
# ### Create each vessel's choice set 
# 
# ### --- Start with all the port areas in the database. 
# ### --- Later, based on the inertia? 
# 
# # freq_selection <- count(Tickets_clust_2, 'selection')
# # gs4_create("freq_selection", sheets = freq_selection)
# 
# choice_set <- Tickets_3 %>% dplyr::select('selection') %>% unique()
# choice_set_by_vessel    <- expand.grid(VESSEL_NUM = unique(Tickets_3$VESSEL_NUM), choice_set = unique(Tickets_3$selection))
# choice_set_by_vessel_v2 <- Tickets_3 %>% dplyr::select('selection', 'VESSEL_NUM') %>% unique()
# 
# gc()
# memory.limit(9999999999)
# Tickets_4 <- merge(Tickets_3, choice_set_by_vessel_v2, by = ('VESSEL_NUM'), all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE)
# gc()
# participation_df <- Tickets_4 %>% mutate(choice = ifelse(selection.x == selection.y, 1, 0))
# head(participation_df, 25)



#-----------------------------------------------------
### Merge with cluster data...
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets_3, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)

Tickets_clust <- Tickets_clust[!is.na(Tickets_clust$group_all), ]


### Save logbooks ###
write.csv(Tickets_clust, "C:\\Data\\PacFIN data\\participation_data.csv", row.names = FALSE)


# #####################################################
# #### OTHER ANALYSIS ####
# 
# # ### How many tickets per species?
# 
# # freq_dominant_species <- count(Tickets, 'Species_Dominant')
# # gs4_create("freq_dominant_species_participation", sheets = freq_dominant_species)
# 
