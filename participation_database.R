########################################
### Participation model -- database ###
########################################


library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


###Load packages and set working directory
library(plyr)
library(data.table)
library(dplyr)
library(distances)
library(forcats)
library(cluster)
library(ggplot2)
library(vegan)
library(NbClust)
library(factoextra)


###I run these lines as well as they are packages I frequently use that can interfere with some of the processes below
detach(package:raster, unload=TRUE)
detach(package:igraph, unload=TRUE)

rm(list=ls())
gc()
setwd("C:/GitHub/EconAnalysis/Clustering")


#-----------------------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)


#-----------------------------------------------------
###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PACFIN_PORT_CODE, VESSEL_NUM, 
                             VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, REMOVAL_TYPE_CODE))


#-----------------------------------------------------
#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


#-----------------------------------------------------
####Find the dominant species by value of each fishing trip ( = target species). 
Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Species_Dominant"
Trip_Species_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Species_Dominant, by='FTID')
rm(Trip_Species_Dominant, X, Boats)


#-----------------------------------------------------
####Find the dominant port by value of each fishing trip ( = target species). 
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
Tickets <- Tickets %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
Boats<-dcast(Tickets, FTID ~ PORT_AREA_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Port_Dominant"
Trip_Port_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Port_Dominant, by='FTID')
rm(Trip_Port_Dominant, X, Boats)


#-----------------------------------------------------
# ### Subset to select only records where one of the forage fish species of interest was the target species
# ### (species in the CPS FMP; squid, sardine, mackerrels and anchovy) 
# FF_Tickets<-Tickets[which((Tickets$Species_Dominant == "PACIFIC SARDINE"  & Tickets$AFI_EXVESSEL_REVENUE>0) | 
#                           (Tickets$Species_Dominant == "MARKET SQUID"     & Tickets$AFI_EXVESSEL_REVENUE>0) | 
#                           (Tickets$Species_Dominant == "NORTHERN ANCHOVY" & Tickets$AFI_EXVESSEL_REVENUE>0) | 
#                           (Tickets$Species_Dominant == "CHUB MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) | 
#                           (Tickets$Species_Dominant == "JACK MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) |
#                           (Tickets$Species_Dominant == "UNSP. MACKEREL"   & Tickets$AFI_EXVESSEL_REVENUE>0) | 
#                           (Tickets$Species_Dominant == "ALBACORE" & 
#                            Tickets$PACFIN_SPECIES_COMMON_NAME == "NORTHERN ANCHOVY")),]
# 
# ## Agreggate mackerrels in one category
# FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "CHUB MACKEREL"]  <- "MACKEREL")
# FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "JACK MACKEREL"]  <- "MACKEREL")
# FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "UNSP. MACKEREL"] <- "MACKEREL")
# 
# 
# ###Creating a filter here to only retain vessels with more than 3 forage fish landings (tickets where FF is the dominant species) 
# ###over the time period. I think 3 is appropriate if you are clustering
# ###several years together, but if you are just clustering a single year than maybe you should drop it down to 1. 
# FTID_Value<-aggregate(AFI_EXVESSEL_REVENUE~FTID+VESSEL_NUM, FUN=sum, data=FF_Tickets)
# FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 3)), ]
# FF_Tickets<-setDT(FF_Tickets)[VESSEL_NUM %chin% FTID_Value$VESSEL_NUM]    
# FF_Tickets<-as.data.frame(FF_Tickets)
# 
# # FF_Tickets indicate tickets where FF are dominant in the trip, but still have landings for other species. 
# 
# ###Find the list of unique vessels in the subset, these are the vessels we will cluster                           
# FF_Vessels<-as.data.frame(unique(FTID_Value$VESSEL_NUM)) 
# names(FF_Vessels)[1]<-"VESSEL_NUM"
# FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
# names(FF_Vessels)[1]<-"VESSEL_NUM"
# FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
# names(FF_Vessels)[1]<-"VESSEL_NUM"
# FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
# names(FF_Vessels)[1]<-"VESSEL_NUM"
# 
# write.csv(FF_Vessels, "FF_Vessels_participation.csv", row.names = FALSE)

# ###Subset from the complete data set to only retain records associated with these Vessels       
# ###Remove records associated with landings of zero value; this is likely bycatch
# # Tickets<-Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),]
# Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]   
# Tickets<-as.data.frame(Tickets)
# rm(FF_Vessels, FTID_Value)


#-----------------------------------------------------
### Merge with cluster data...
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)
Tickets_clust <- Tickets_clust[!is.na(Tickets_clust$group_all), ]


#-----------------------------------------------------
### Rename species dominant: MSQD, PSDN, NANC, OMCK, NON-CPS.

Tickets_clust <- within(Tickets_clust, Species_Dominant[Species_Dominant == "CMCK"] <- "OMCK")
Tickets_clust <- within(Tickets_clust, Species_Dominant[Species_Dominant == "JMCK"] <- "OMCK")
Tickets_clust <- within(Tickets_clust, Species_Dominant[Species_Dominant == "UMCK"] <- "OMCK")
Tickets_clust <- Tickets_clust %>% mutate(
  Species_Dominant = ifelse(Species_Dominant == "OMCK", Species_Dominant, 
                     ifelse(Species_Dominant == "PSDN", Species_Dominant, 
                     ifelse(Species_Dominant == "MSQD", Species_Dominant, 
                     ifelse(Species_Dominant == "NANC", Species_Dominant, "OTHER")))))

#-----------------------------------------------------
### Create port-species choice
Tickets_clust_2 <- Tickets_clust %>% mutate(selection = paste(Port_Dominant, Species_Dominant, sep = "-", collapse = NULL))


#-----------------------------------------------------
### Create each vessel's choice set 

### --- Start with all the port areas in the database. 
### --- Later, based on the inertia? 

# freq_selection <- count(Tickets_clust_2, 'selection')
# gs4_create("freq_selection", sheets = freq_selection)

choice_set <- Tickets_clust_2 %>% dplyr::select('selection') %>% unique()
choice_set_by_vessel <- expand.grid(VESSEL_NUM = unique(Tickets_clust_2$VESSEL_NUM), choice_set = unique(Tickets_clust_2$selection))
Tickets_clust_3 <- merge(Tickets_clust_2, choice_set_by_vessel, by = ('VESSEL_NUM'), all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE)
participation_df <- Tickets_clust_3 %>% mutate(choice = ifelse(selection == choice_set, 1, 0)) 
head(participation_df, 5)


### Include outside option? Then, expand data when variables are not observed.





#-----------------------------------------------------
### Run a base model...
















######################################################
#### OTHER ANALYSIS ####

# ### How many tickets per species?

# freq_dominant_species <- count(Tickets, 'Species_Dominant')
# gs4_create("freq_dominant_species_participation", sheets = freq_dominant_species)


