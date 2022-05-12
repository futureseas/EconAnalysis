#####################################################  
### Create average plots for pre and post closure ###
#####################################################

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
library(tidyr)

###I run these lines as well as they are packages I frequently use that can interfere with some of the processes below
detach(package:raster, unload=TRUE)
detach(package:igraph, unload=TRUE)

rm(list=ls())
gc()
setwd("C:/GitHub/EconAnalysis/Clustering")


#######################
### Data processing ###
#######################

period = "2015-2020"
min.year = 2015
max.year = 2020

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

Tickets <- Tickets %>% 
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1)


#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


#### Load vessel used in the clustering analysis
FF_Vessels <- fread("FF_Vessels.csv")


###Subset from the complete data set to only retain records associated with these Vessels       
###Remove records associated with landings of zero value; this is likely bycatch
# Tickets<-Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),]
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]   
Tickets<-as.data.frame(Tickets)

rm(FF_Vessels, FTID_Value)


######################
### Create metrics ###
######################

#----------------------------------------------
###Vessel Revenue and Poundage 
###Find the average annual poundage and revenue (active years) reported by each vessel each year it appears in the data subset

Total_Weight<-aggregate(LANDED_WEIGHT_LBS~VESSEL_NUM, FUN=sum, data=Tickets)
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
###Vessel Center of Gravity and Inertia. Note you will have to load in the "cgi" function described in a separate file before proceeding
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
### Calculate Forage Fish Diversity and % of Revenue derived from Forage Fish,
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
FFP<-merge(Total, FF_Total, by="VESSEL_NUM", all.x = TRUE)
FFP$Percentage<-FFP$AFI_EXVESSEL_REVENUE.y/FFP$AFI_EXVESSEL_REVENUE.x
Boats<-merge(FFP, Boats, by="VESSEL_NUM", all.x = TRUE )
Boats<-Boats[c(-2,-3)]


FF_Landings_and_Diversity<-Boats

rm(Boats, FFP, FF_Total, Total)


#----------------------------------------------
### Combine metrics
RAW<-merge(Landings_Volume_Value, Vessel_Geography, by="VESSEL_NUM", all.x = TRUE, all.y = TRUE)
RAW<-merge(RAW, FF_Landings_and_Diversity, by="VESSEL_NUM", all.x = TRUE, all.y = TRUE)
Vessel_IDs <- RAW[,1]
Vessels<-as.data.frame(Vessel_IDs)
RAW<-RAW[c(-1)]
rownames(RAW)<-Vessel_IDs
rm(Vessel_Characteristics, Landings_Volume_Value, Vessel_Geography, FF_Landings_and_Diversity)
### I wound up removing weight because I felt as if that and length provided redundant information;
RAW<-RAW[c(-1)] # Delete AVG_LBS
RAW$VESSEL_NUM<-Vessel_IDs

rm(FF_Tickets, Tickets, Vessels, cgi)

#-----------------------------------------
## Merge pre and post RAW

RAW_POST <- RAW %>%
  dplyr::rename(AVG_REVENUE_POST = AVG_REVENUE) %>%
  dplyr::rename(LAT_POST         = LAT) %>%
  dplyr::rename(DISTANCE_A_POST  = DISTANCE_A) %>%
  dplyr::rename(Percentage_POST  = Percentage) %>%
  dplyr::rename(diversity_POST   = diversity) %>% mutate(Post = 1)
  rm(RAW)

RAW_PRE  <- fread("RAW_cluster_inputs.csv") %>%
  dplyr::rename(AVG_REVENUE_PRE = AVG_REVENUE) %>%
  dplyr::rename(LAT_PRE         = LAT) %>%
  dplyr::rename(DISTANCE_A_PRE  = DISTANCE_A) %>%
  dplyr::rename(Percentage_PRE  = Percentage) %>%
  dplyr::rename(diversity_PRE   = diversity) 

RAW <- merge(RAW_PRE, RAW_POST, by = c("VESSEL_NUM") , all.x = TRUE)

#-----------------------------------------
## Number of vessels that still active but do not have CPS landing tickets

non_CPS_tickets <- RAW %>% filter(Post == 1) %>%
  group_by(group_all) %>% 
  summarize(n_vessels_by_cluster = n(), percentage_non_CPS_tickets = (n()/ sum(is.na(diversity_POST)))) 

n_vessel_exit <- RAW %>% group_by(group_all) %>% 
  summarize(n_vessels = n(), n_vessel_exit = sum(is.na(AVG_REVENUE_POST))) %>%
  mutate(percentage_stay = ((n_vessels - n_vessel_exit) / n_vessels))


#---------------------------------------
## Compare inputs pre and after closure by cluster

