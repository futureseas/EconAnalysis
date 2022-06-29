###################################
### Participation model -- MSQD ###
###################################




### Create metrics by month for each vessel 
#----------------------------------------------

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

#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


###Vessel Revenue and Poundage 
###Find the average annual poundage and revenue (active years) reported by each vessel each year it appears in the data subset
Total_Weight<-aggregate(LANDED_WEIGHT_LBS ~ VESSEL_NUM + LANDING_MONTH + LANDING_YEAR, FUN=sum, data=Tickets)
Total_Revenue<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, FUN=sum, data=Tickets) 

##Finding the number of years the vessel is active in the subset
Total_Years<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM + LANDING_YEAR, FUN=sum, data=Tickets)  
Total_Years<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, FUN=length, data=Total_Years)  

##Dividing total landings and revenue reported by each vessel by the number of years the vessel was active.
Landings_Volume_Value<-merge(Total_Weight, Total_Revenue, by="VESSEL_NUM") 
Landings_Volume_Value<-merge(Landings_Volume_Value, Total_Years, by="VESSEL_NUM")
Landings_Volume_Value$LANDED_WEIGHT_LBS<-Landings_Volume_Value$LANDED_WEIGHT_LBS/Landings_Volume_Value$AFI_EXVESSEL_REVENUE.y
Landings_Volume_Value$AFI_EXVESSEL_REVENUE.x<-Landings_Volume_Value$AFI_EXVESSEL_REVENUE.x/Landings_Volume_Value$AFI_EXVESSEL_REVENUE.y
Landings_Volume_Value<-Landings_Volume_Value[c(1,2,3)] 
names(Landings_Volume_Value)<-c("VESSEL_NUM", "AVG_LBS", "AVG_REVENUE")

rm(Total_Weight, Total_Revenue, Total_Years)

#----------------------------------------------
###Step 3: Vessel Center of Gravity and Inertia. Note you will have to load in the "cgi" function described in a separate file before proceeding
###Loading in this package here because if you load it in previously it masks the "select" function used to subset the data.
library(raster)    

###A file that has the Lat Lon Coordinates of all ports were there has been landings of a fishing trip whose value was dominated by forage fish
Coords<-read.csv("Port_Zips_1.10.22.csv")
Coords<-Coords[c(-1,-4)]

###Append these lat lon coords to the Fish Ticket Database
Ticket_Coords<-merge(Tickets, Coords, by=c("PORT_NAME", "AGENCY_CODE"))
Ticket_Coords<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM+PORT_NAME+AGENCY_CODE+Latitude+Longitude, data=Ticket_Coords, FUN=sum)

### A Loop that finds the LAT, LON of Center of Gravity of each fishing vessel (as weighted by the value landed at each port) and the length of their primary,
### and secondary axes of dispersion (Distance A & Distance B), often times referred to as range or inertia.

Permit_ID<-as.data.frame(unique(Ticket_Coords$VESSEL_NUM))
names(Permit_ID)<-"VESSEL_NUM"
List<-as.list(as.character(Permit_ID$VESSEL_NUM))
Permit_COG<-NULL

### Run function 
source("./CGI_Function.R")

for (i in 1:length(List)) {
  Permit = List[i]
  Single_Permit<- Ticket_Coords[which(Ticket_Coords$VESSEL_NUM==Permit),]
  Single_COG<-cgi(x=Single_Permit$Longitude, y=Single_Permit$Latitude, z=Single_Permit$AFI_EXVESSEL_REVENUE, plot=F)
  Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2],  
                                   Single_COG$xcg),
                           lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2],
                                   Single_COG$ycg),group = c("A", "A", "B", "B","C"))
  Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
  Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
  Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]	
  Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
  Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
  Value<-as.data.frame(c(Permit, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B))
  names(Value)<-c("uniqueid", "LON", "LAT", "DISTANCE_A", "DISTANCE_B")
  Permit_COG<-rbind(Permit_COG, Value)
}

###Any vessel with NaN Values only landed at 1 port, so we change those values to 0
Permit_COG$DISTANCE_A <- sub(NaN, 0, Permit_COG$DISTANCE_A)
Permit_COG$DISTANCE_B <- sub(NaN, 0, Permit_COG$DISTANCE_B)
Permit_COG$DISTANCE_A<-as.numeric(Permit_COG$DISTANCE_A)
Permit_COG$DISTANCE_B<-as.numeric(Permit_COG$DISTANCE_B)

###Produce dissimilarity matrix and pairwise comparisons following methods above
Permit_COG<-Permit_COG[c(1,3,4)]
names(Permit_COG)[1]<-"VESSEL_NUM"
Vessel_Geography<-Permit_COG

rm(Coords, Ticket_Coords, List, Permit_ID, Permit_COG, Distance_A, Distance_B, 
   Point_Coord, Single_Permit, Single_COG, i, Permit, Value, Line_Coord_A, Line_Coord_B)

#----------------------------------------------
### Step 4: Calculate Forage Fish Diversity and % of Revenue derived from Forage Fish,
### and average number of months per year landing forage fish for each vessel

###Prepare your forage fish matrix
FF_Tickets <- Tickets[which(Tickets$PACFIN_SPECIES_COMMON_NAME=="PACIFIC SARDINE"  | 
                              Tickets$PACFIN_SPECIES_COMMON_NAME=="MARKET SQUID"     | 
                              Tickets$PACFIN_SPECIES_COMMON_NAME=="NORTHERN ANCHOVY" |
                              Tickets$PACFIN_SPECIES_COMMON_NAME=="CHUB MACKEREL"    | 
                              Tickets$PACFIN_SPECIES_COMMON_NAME=="JACK MACKEREL"    | 
                              Tickets$PACFIN_SPECIES_COMMON_NAME=="UNSP. MACKEREL"),]

FF_Tickets<- within(FF_Tickets, PACFIN_SPECIES_COMMON_NAME[PACFIN_SPECIES_COMMON_NAME == "CHUB MACKEREL"] <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, PACFIN_SPECIES_COMMON_NAME[PACFIN_SPECIES_COMMON_NAME == "JACK MACKEREL"] <- "MACKEREL")
FF_Tickets<- within(FF_Tickets, PACFIN_SPECIES_COMMON_NAME[PACFIN_SPECIES_COMMON_NAME == "UNSP. MACKEREL"] <- "MACKEREL")


Boats<-aggregate(AFI_EXVESSEL_REVENUE~ PACFIN_SPECIES_COMMON_NAME + 
                   VESSEL_NUM, data=FF_Tickets, FUN=sum) 

Boats<-dcast(Boats, VESSEL_NUM ~ PACFIN_SPECIES_COMMON_NAME, 
             value.var="AFI_EXVESSEL_REVENUE", fill=0)

rownames(Boats) <- Boats[,1]
Boats <- Boats[,-1]


###Calculate the diversity value
Boats<-as.data.frame(diversity(Boats, index="invsimpson"))
Boats$VESSEL_NUM <- rownames(Boats)
names(Boats)<-c("diversity", "VESSEL_NUM")
Boats$diversity[which(!is.finite(Boats$diversity))] <- 0


###Calculate the percentage of revenue derived from CPS
FF_Total<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, data=FF_Tickets, FUN=sum)
Total<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, data=Tickets, FUN=sum)
FFP<-merge(Total, FF_Total, by="VESSEL_NUM")
FFP$Percentage<-FFP$AFI_EXVESSEL_REVENUE.y/FFP$AFI_EXVESSEL_REVENUE.x
Boats<-merge(FFP, Boats, by="VESSEL_NUM")
Boats<-Boats[c(-2,-3)]
FF_Landings_and_Diversity<-Boats

rm(Boats, FFP, FF_Total, Total)




















#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


## Read packages 
library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")

## Read PacFIN database 
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv") %>% 
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum > 0)

## Create database of port area code and port names
ports_area_codes <- PacFIN.month %>% dplyr::select('PORT_NAME', 'AGENCY_CODE', 'PORT_AREA_CODE') %>% unique()

#---------------------------------------------------
## Construct monthly database
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) }

library(doBy)
PacFIN.month.aggregate <- doBy::summaryBy(
  LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean + AFI_EXVESSEL_REVENUE.sum + Length.mean 
    ~ LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + PACFIN_SPECIES_CODE + AGENCY_CODE + group_all,
  FUN=sum_mean_fun, data=PacFIN.month) %>%
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum.sum > 0) %>%
  dplyr::select(-c('LANDED_WEIGHT_MTONS.sum.mean', 'AFI_PRICE_PER_MTON.mean.sum',
                   'AFI_EXVESSEL_REVENUE.sum.mean', 'Length.mean.sum'))
rm(PacFIN.month)


########################################################
# Create full database

### If a vessel land more than 5,000 USD in value on a port during 2000-2020, then he have to decide during that port and month 
### to participate or not in a fishery.
vessel.participation <- PacFIN.month.aggregate %>%
  group_by(VESSEL_NUM, PORT_AREA_CODE, AGENCY_CODE, group_all) %>%
  summarize(total_rev = sum(AFI_EXVESSEL_REVENUE.sum.sum)) %>% filter(total_rev >= 5000)

### Create grid with all the years and month by port where a vessel participate
year = expand.grid(PORT_AREA_CODE = unique(vessel.participation$PORT_AREA_CODE), LANDING_YEAR = 2000:2020)
vessel.participation <- left_join(vessel.participation, year, by = "PORT_AREA_CODE")

months = expand.grid(LANDING_YEAR = unique(vessel.participation$LANDING_YEAR), LANDING_MONTH = 1:12)
vessel.participation <- left_join(vessel.participation, months, by = "LANDING_YEAR")

PacFIN.month.left.join <- left_join(vessel.participation, PacFIN.month.aggregate,
                                    by = c("LANDING_YEAR" ,"VESSEL_NUM", "PORT_AREA_CODE", 
                                           "AGENCY_CODE", "group_all", "LANDING_MONTH"))

# PacFIN.month.merge <- merge(vessel.participation, PacFIN.month.aggregate,
#   by = c("LANDING_YEAR" ,"VESSEL_NUM", "PORT_AREA_CODE",
#   "AGENCY_CODE", "group_all", "LANDING_MONTH"), all.x = TRUE, all.y = TRUE) %>%
#   dplyr::filter(is.na(total_rev))


########################################################


PacFIN.month.dataset <- PacFIN.month.left.join %>% 
  dplyr::rename(AFI_PRICE_PER_MTON.mean = AFI_PRICE_PER_MTON.mean.mean) %>%
  dplyr::rename(LANDED_WEIGHT_MTONS.sum = LANDED_WEIGHT_MTONS.sum.sum) %>%
  dplyr::rename(AFI_EXVESSEL_REVENUE.sum = AFI_EXVESSEL_REVENUE.sum.sum) %>%
  dplyr::rename(Length = Length.mean.mean) %>%
  mutate(AFI_PRICE_PER_MTON.mean = na_if(AFI_PRICE_PER_MTON.mean, 0)) %>% 
  filter(group_all != is.na(group_all)) %>% 
  select(-c('total_rev')) %>%
  reshape2::melt(id.vars=c(
    "LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM", 'PORT_AREA_CODE',
    "PACFIN_SPECIES_CODE", "AGENCY_CODE", "Length", "group_all")) %>% 
  reshape2::dcast(
    LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + AGENCY_CODE + group_all + Length ~
      PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 'PORT_AREA_CODE', 'AGENCY_CODE', 'group_all',
                'Length', 'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 'NANC_LANDED_WEIGHT_MTONS.sum',  
                'PSDN_AFI_PRICE_PER_MTON.mean', 'MSQD_AFI_PRICE_PER_MTON.mean', 'NANC_AFI_PRICE_PER_MTON.mean')

rm(PacFIN.month.aggregate, PacFIN.month.left.join)

                
  
### Selecting port using results from cluster analysis (10% of the revenue for at least one cluster)
PacFIN.month.dataset <- PacFIN.month.dataset %>%
  dplyr::filter(PORT_AREA_CODE == "SDA" | PORT_AREA_CODE == "LAA" |
                  PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" | 
                  PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | 
                  PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS")


### Create ID data and change NaN to NA ###
PacFIN.month.dataset <- PacFIN.month.dataset %>%
  mutate(PORT_AREA_ID = as.numeric(as.factor(PORT_AREA_CODE))) 
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA



### Add variables for participation model
















### Merge SDM by year/month/port ###

#### Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(PSDN_SDM_60 = mean(SDM_60))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_PSDN,
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(NANC_SDM_20 = mean(SDM_20))
dataset <- merge(PacFIN.month.dataset, SDM_port_NANC, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

rm(PacFIN.month.dataset, 
   SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD_Spawn, ports_area_codes, vessel.monthly.dataset)


#-----------------------------------------------------------------
## Calculate SDM for N/A's using PacFIN port code and port area code.

### Pacific sardine

#### Using port area code
SDM.port.area.PSDN <- aggregate(x=dataset$PSDN_SDM_60,
                                by = list(dataset$LANDING_YEAR,
                                          dataset$LANDING_MONTH,
                                          dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.PSDN <- SDM.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.area = x)
SDM.port.area.PSDN[SDM.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.area, PSDN_SDM_60))
rm(SDM.port.area.PSDN)

dataset = subset(dataset, select = -c(PSDN.SDM.port.area))


### Northern anchovy

#### Using port area code
SDM.port.area.NANC <- aggregate(x=dataset$NANC_SDM_20,
                                by = list(dataset$LANDING_YEAR,
                                          dataset$LANDING_MONTH,
                                          dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.NANC <- SDM.port.area.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(NANC.SDM.port.area = x)
SDM.port.area.NANC[SDM.port.area.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(NANC_SDM_20 = ifelse(is.na(NANC_SDM_20), NANC.SDM.port.area, NANC_SDM_20))
rm(SDM.port.area.NANC)

dataset = subset(dataset, select = -c(NANC.SDM.port.area))


### Market squid (SPAWN)

#### Using port area code
SDM.port.area.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, 
                                      by = list(dataset$LANDING_YEAR,
                                                dataset$LANDING_MONTH, 
                                                dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD_SPAWN <- SDM.port.area.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.area = x)
SDM.port.area.MSQD_SPAWN[SDM.port.area.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD_SPAWN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.area, MSQD_SPAWN_SDM_90))
rm(SDM.port.area.MSQD_SPAWN)
dataset = subset(dataset, select = -c(MSQD_SPAWN.SDM.port.area))

#----------------------------------------------------------------------------------------------
## Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

### Pacific sardine

#### (a) Using port area code
price.port.area.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, 
                                  by = list(dataset$LANDING_YEAR, dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.PSDN <- price.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.area = x)
price.port.area.PSDN[price.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.port.area, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.PSDN)

#### (b) Using state
price.state.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.PSDN <- price.state.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.state = x)
price.state.PSDN[price.state.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.state, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.state.PSDN)

#### (c) Using year/month
price.year.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.PSDN <- price.year.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PSDN.price.year.month = x)
price.year.PSDN[price.year.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.year.month, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.year.PSDN)

dataset = subset(dataset, select = 
                   -c(PSDN.price.port.area, PSDN.price.state, PSDN.price.year.month))

### Market squid

#### (a) Using port area code
price.port.area.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.MSQD <- price.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.area = x)
price.port.area.MSQD[price.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.port.area, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.MSQD)

#### (b) Using state
price.state.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.MSQD <- price.state.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.state = x)
price.state.MSQD[price.state.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.state, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.state.MSQD)

#### (c) Using year/month
price.year.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.MSQD <- price.year.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(MSQD.price.year.month = x)
price.year.MSQD[price.year.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.year.month, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.year.MSQD)

dataset = subset(dataset, select = 
                   -c(MSQD.price.port.area, MSQD.price.state, MSQD.price.year.month))


### Northern anchovy

#### (a) Using port area code
price.port.area.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.NANC <- price.port.area.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(NANC.price.port.area = x)
price.port.area.NANC[price.port.area.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.port.area, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.NANC)

#### (b) Using state
price.state.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.NANC <- price.state.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(NANC.price.state = x)
price.state.NANC[price.state.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.state, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.state.NANC)

#### (c) Using year/month
price.year.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.NANC <- price.year.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(NANC.price.year.month = x)
price.year.NANC[price.year.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.NANC, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.year.month, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.year.NANC)

dataset = subset(dataset, select = 
                   -c(NANC.price.port.area, NANC.price.state, NANC.price.year.month))


# --------------------------------------------------------------------------------------
### Include closure data
PSDN_closure <- read.csv("C:\\Data\\Closures\\PSDN_closures.csv")
MSQD_closure <- read.csv("C:\\Data\\Closures\\MSQD_closures.csv") 
dataset <- merge(dataset, PSDN_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
dataset <- merge(dataset, MSQD_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
rm(PSDN_closure, MSQD_closure)


#---------------------------------------------------------------------------------------
### Include Consumer Price Index to use as a deflactor
CPI <- read.csv(here::here("Data", "CPI", "CPIAUCSL.csv"))
CPI$DATE <- as.Date(CPI$DATE, format = "%Y-%m-%d") 
CPI$LANDING_YEAR  <- lubridate::year(CPI$DATE)
CPI$LANDING_MONTH <- lubridate::month(CPI$DATE)
CPI <- CPI %>% dplyr::select(-c('DATE')) %>% dplyr::rename(CPI = CPIAUCSL)
dataset <- merge(dataset, CPI, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
dataset$deflactor <- dataset$CPI/100 
rm(CPI)

#---------------------------------------------------------------------------------------
### Include world's fish meal price as instrument
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
dataset <- merge(dataset, fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
dataset$Price.Fishmeal.AFI <- dataset$Price.Fishmeal/dataset$deflactor
rm(fish.meal)

#---------------------------------------------------------------------------------------
### Include fuel prices
fuel.prices.CA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelca.xls"), sheet = "fuelca")
fuel.prices.WA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelwa.xls"), sheet = "fuelwa")
fuel.prices.OR <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelor.xls"), sheet = "fuelor")

fuel.prices <- rbind(fuel.prices.CA,fuel.prices.OR,fuel.prices.WA) %>% 
  dplyr::select(-c('portname', 'DOCKCODE', 'notes', 'pricettl', 'pxquoted')) %>%
  dplyr::rename(PACFIN_PORT_CODE = port) %>%
  dplyr::rename(LANDING_YEAR = YEAR) %>%
  dplyr::rename(LANDING_MONTH = MONTH) 

## Replace zero to NA
fuel.prices[fuel.prices == 0] <- NA

## Merge port area code
port_area_codes <- read.csv(here::here('Data', 'Ports', 'ports_area_and_name_codes.csv'), 
                            header = TRUE, stringsAsFactors = FALSE)

fuel.prices <- merge(fuel.prices, port_area_codes, 
                     by = ("PACFIN_PORT_CODE"), all.x = TRUE, all.y = FALSE)

fuel.prices <- fuel.prices %>% group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarize(diesel.price = mean(pricegal, na.rm = TRUE))

## Merge price to dataset
dataset <- merge(dataset, fuel.prices, by = c('LANDING_YEAR', 'LANDING_MONTH', 'PORT_AREA_CODE'), all.x = TRUE, all.y = FALSE)

#### replace NA using state price
fuel.prices.state <- readxl::read_excel(here::here("Data", "Fuel_prices", "state_averages.xls"), sheet = "state_averages") %>%
  dplyr::rename(LANDING_YEAR = YEAR) %>%
  dplyr::rename(LANDING_MONTH = MONTH) %>%
  dplyr::rename(AGENCY_CODE = STATE) %>%
  dplyr::rename(diesel.price.state = avgpricegal) %>%
  dplyr::select(-c('avgpricettl')) %>%
  mutate(AGENCY_CODE = ifelse(AGENCY_CODE == 'WA', 'W', 
                              ifelse(AGENCY_CODE == 'CA', 'C', 
                                     ifelse(AGENCY_CODE == 'OR', 'O', 'A'))))

dataset <- dataset %>%
  merge(fuel.prices.state, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(diesel.price = ifelse(is.na(diesel.price), diesel.price.state, diesel.price))

## Deflact prices
dataset$diesel.price.AFI <- dataset$diesel.price/dataset$deflactor
rm(fuel.prices, fuel.prices.CA, fuel.prices.OR, fuel.prices.WA, fuel.prices.state)


#---------------------------------------------------------------------------------------
## Include trend and average number of set for MSQD
avg.number.set <- read.csv(here::here("Data", "Logbook_output", "max_n_set.by.trip.csv"), header = TRUE, stringsAsFactors = FALSE)
dataset <- merge(dataset, avg.number.set, by = ('LANDING_YEAR'), all.x = TRUE, all.y = FALSE)


#---------------------------------------
# Create centerized variables and z-values 

dataset <- dataset %>%
  dplyr::mutate(MSQD_Price_z = ((MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) / sd(MSQD_Price, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_Price_z = ((PSDN_Price - mean(PSDN_Price, na.rm = TRUE)) / sd(PSDN_Price, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SPAWN_SDM_90_z = ((MSQD_SPAWN_SDM_90 - mean(MSQD_SPAWN_SDM_90, na.rm = TRUE))/sd(MSQD_SPAWN_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SDM_90_z = ((MSQD_SDM_90 - mean(MSQD_SDM_90, na.rm = TRUE))/sd(MSQD_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_SDM_60_z = ((PSDN_SDM_60 - mean(PSDN_SDM_60, na.rm = TRUE))/sd(PSDN_SDM_60, na.rm = TRUE))) %>%
  dplyr::mutate(NANC_SDM_20_z = ((NANC_SDM_20 - mean(NANC_SDM_20, na.rm = TRUE))/sd(NANC_SDM_20, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal_z = ((Price.Fishmeal - mean(Price.Fishmeal, na.rm = TRUE))/sd(Price.Fishmeal, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price_z = ((diesel.price - mean(diesel.price, na.rm = TRUE))/sd(diesel.price, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal.AFI_z = ((Price.Fishmeal.AFI - mean(Price.Fishmeal.AFI, na.rm = TRUE))/sd(Price.Fishmeal.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price.AFI_z = ((diesel.price.AFI - mean(diesel.price.AFI, na.rm = TRUE))/sd(diesel.price.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(Length_z = ((Length - mean(Length, na.rm = TRUE))/sd(Length, na.rm = TRUE))) %>%
  dplyr::mutate(avg_set_MSQD_z = ((avg_set_MSQD - mean(avg_set_MSQD, na.rm = TRUE))/sd(avg_set_MSQD, na.rm = TRUE)))



#-----------------------------------------------
## Create dataset for estimation and run landing models 

### Market squid ###

#### Select data for estimation, replace N/A landings to zero ####
dataset_msqd <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_Landings, NANC_Landings, PSDN_Price, NANC_Price, 
                PSDN_SDM_60, NANC_SDM_20,
                MSQD_Price_z, PSDN_Price_z, MSQD_SPAWN_SDM_90_z, MSQD_SDM_90_z, 
                PSDN_SDM_60_z, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open, Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price, diesel.price.AFI, diesel.price_z, diesel.price.AFI_z,
                Length, Length_z, avg_set_MSQD, avg_set_MSQD_z) %>% 
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.0001, 0, MSQD_Landings)) %>%
  dplyr::mutate(PSDN.Participation = ifelse(PSDN_Landings > 0, 1, 0)) %>% 
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_MSQD_Landings = log(MSQD_Landings)) %>%
    filter(group_all == 1 | group_all == 2 | group_all == 4 | group_all == 5 | group_all == 7) %>%
    filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA") %>% drop_na()


## install.packages(c("fastDummies", "recipes"))
# library('fastDummies')
# dataset_msqd <- dummy_cols(dataset_msqd, select_columns = 'cluster')

#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_msqd$port_ID      <- factor(dataset_msqd$PORT_AREA_CODE)
dataset_msqd$cluster      <- factor(dataset_msqd$group_all)
class(dataset_msqd$port_ID)
class(dataset_msqd$cluster)

dataset_msqd_landing <- dataset_msqd %>%
  dplyr::filter(MSQD_Landings > 0) %>%
  dplyr::filter(MSQD.Open == 1) 

### Descriptive statistics 
desc_data <- dataset_msqd_landing %>%
  subset(select = c(Length, MSQD_Landings, MSQD_Price, 
                    MSQD_SDM_90, MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,  
                    PSDN.Open, Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fraction of month open: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Fishmeal price", vars)) 

# gs4_create("SummaryMonthly_Q_MSQD", sheets = table)
rm(desc_data, table)


#-------------------------------------------------------------------
#### Estimate models ####
library(brms)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(insight)
library(httr)




