#######################################################
#### Number of CPS vessels before and after closure ###
#######################################################

###Load packages and set working directory
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


############# Pre closure ##############

period = "2005-2014"
min.year = 2005 
max.year = 2014

# ----------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, PORT_NAME, PACFIN_PORT_CODE, VESSEL_NUM, 
                             VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, REMOVAL_TYPE_CODE))

####Select the time period you want to cluster over
Tickets<-Tickets[which(Tickets$LANDING_YEAR>=min.year & Tickets$LANDING_YEAR<=max.year),]

#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


####Find the dominant species by value of each fishing trip, presumably this is the target species. Using the logic of metiers,
###all landings of each fishing trip should be tagged with a single identifier.
Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_COMMON_NAME, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Dominant"
Trip_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Dominant, by='FTID')

rm(Trip_Dominant, X, Boats)


### Subset to select only records where one of the forage fish species of interest was the target species
### (species in the CPS FMP; squid, sardine, mackerrels and anchovy) 
FF_Tickets<-Tickets[which((Tickets$Dominant == "PACIFIC SARDINE"  & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "MARKET SQUID"     & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "NORTHERN ANCHOVY" & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "CHUB MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "JACK MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) |
                            (Tickets$Dominant == "UNSP. MACKEREL"   & Tickets$AFI_EXVESSEL_REVENUE>0)),]

## Agreggate mackerrels in one category
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "CHUB MACKEREL"]  <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "JACK MACKEREL"]  <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "UNSP. MACKEREL"] <- "MACKEREL")


###Creating a filter here to only retain vessels with more than 3 forage fish landings (tickets where FF is the dominant species) 
###over the time period. I think 3 is appropriate if you are clustering
###several years together, but if you are just clustering a single year than maybe you should drop it down to 1. 
FTID_Value<-aggregate(AFI_EXVESSEL_REVENUE~FTID+VESSEL_NUM, FUN=sum, data=FF_Tickets)
FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 3)), ]
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

FF_Vessels <- FF_Vessels %>% mutate(Pre_activity = 1)

rm(FF_Tickets, FTID_Value, Tickets, FTID)


########### Post Closure ###########

period = "2015-2020"
min.year = 2015 
max.year = 2020

### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, PORT_NAME, PACFIN_PORT_CODE, VESSEL_NUM, 
                             VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, REMOVAL_TYPE_CODE))

####Select the time period you want to cluster over
Tickets<-Tickets[which(Tickets$LANDING_YEAR>=min.year & Tickets$LANDING_YEAR<=max.year),]

#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


####Find the dominant species by value of each fishing trip, presumably this is the target species. Using the logic of metiers,
###all landings of each fishing trip should be tagged with a single identifier.
Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_COMMON_NAME, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Dominant"
Trip_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Dominant, by='FTID')

rm(Trip_Dominant, X, Boats)


### Subset to select only records where one of the forage fish species of interest was the target species
### (species in the CPS FMP; squid, sardine, mackerrels and anchovy) 
FF_Tickets<-Tickets[which((Tickets$Dominant == "PACIFIC SARDINE"  & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "MARKET SQUID"     & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "NORTHERN ANCHOVY" & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "CHUB MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                            (Tickets$Dominant == "JACK MACKEREL"    & Tickets$AFI_EXVESSEL_REVENUE>0) |
                            (Tickets$Dominant == "UNSP. MACKEREL"   & Tickets$AFI_EXVESSEL_REVENUE>0)),]

## Agreggate mackerrels in one category
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "CHUB MACKEREL"]  <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "JACK MACKEREL"]  <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, Dominant[Dominant == "UNSP. MACKEREL"] <- "MACKEREL")


###Creating a filter here to only retain vessels with more than 3 forage fish landings (tickets where FF is the dominant species) 
###over the time period. I think 3 is appropriate if you are clustering
###several years together, but if you are just clustering a single year than maybe you should drop it down to 1. 
FTID_Value<-aggregate(AFI_EXVESSEL_REVENUE~FTID+VESSEL_NUM, FUN=sum, data=FF_Tickets)
FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 3)), ]
FF_Tickets<-setDT(FF_Tickets)[VESSEL_NUM %chin% FTID_Value$VESSEL_NUM]    
FF_Tickets<-as.data.frame(FF_Tickets)

# FF_Tickets indicate tickets where FF are dominant in the trip, but still have landings for other species. 

###Find the list of unique vessels in the subset, these are the vessels we will cluster                           
FF_Vessels_POST <- as.data.frame(unique(FTID_Value$VESSEL_NUM)) 
names(FF_Vessels_POST)[1]<-"VESSEL_NUM"
FF_Vessels_POST <- as.data.frame(FF_Vessels_POST[which(!FF_Vessels_POST$VESSEL_NUM==""),])
names(FF_Vessels_POST)[1]<-"VESSEL_NUM"
FF_Vessels_POST <- as.data.frame(FF_Vessels_POST[which(!FF_Vessels_POST$VESSEL_NUM=="UNKNOWN"),])
names(FF_Vessels_POST)[1]<-"VESSEL_NUM"
FF_Vessels_POST <- as.data.frame(FF_Vessels_POST[which(!FF_Vessels_POST$VESSEL_NUM=="MISSING"),])
names(FF_Vessels_POST)[1]<-"VESSEL_NUM"

FF_Vessels_POST <- FF_Vessels_POST %>% mutate(Post_activity = 1)


rm(FF_Tickets, FTID_Value, Tickets, FTID)


############# Merge both databases #################

new.CPS.vessels <- merge(FF_Vessels, FF_Vessels_POST, by = ('VESSEL_NUM'), all.x = FALSE, all.y = TRUE) %>%
  dplyr::filter(is.na(Pre_activity))

 ## FQ: From 251 vessels, only 162 harvest CPS after closure, and only 102 are vessels that harvest CPS previously.
  ## From 60 new vessels harvesting CPS, 30 have harvest previosuly, and 30 are NEW vessels. 
  ## Therefore, there is a reduction of 89 vessels from pre to post. Also, 60 are news, so actually 89 + 60 = 149 vessels 
  ## might have left, but there are 30 news that could be from the same owner. Thus, the number is between 119 and 149 vessels.
  ## Enough to support our hypothesis. 


## Do this new vessels have landings other species before???
period = "2005-2014"
min.year = 2005 
max.year = 2014
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)
Tickets<-Tickets[which(Tickets$LANDING_YEAR>=min.year & Tickets$LANDING_YEAR<=max.year),]
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E")


New.CPS.Vessel_Tickets <- setDT(Tickets)[VESSEL_NUM %chin% new.CPS.vessels$VESSEL_NUM] %>% 
  dplyr::filter(AFI_EXVESSEL_REVENUE>0) %>% dplyr::select('VESSEL_NUM') %>% unique()



