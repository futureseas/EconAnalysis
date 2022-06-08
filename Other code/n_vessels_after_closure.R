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
FF_Tickets_POST<-as.data.frame(FF_Tickets)

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
Tickets_POST <- Tickets
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

rm(Tickets, FF_Vessels)

#################################################
######## Inputs average for new vessels #########
#################################################

Tickets <- setDT(Tickets_POST)[VESSEL_NUM %chin% new.CPS.vessels$VESSEL_NUM]
FF_Tickets <- setDT(FF_Tickets_POST)[VESSEL_NUM %chin% new.CPS.vessels$VESSEL_NUM]

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


#----------------------------------------------
####Step 5: Combine metrics, scale metrics, determine optimal cluster number, assign vessels to clusters
RAW<-merge(Landings_Volume_Value, Vessel_Geography, by="VESSEL_NUM")
RAW<-merge(RAW, FF_Landings_and_Diversity, by="VESSEL_NUM")
Vessel_IDs <- RAW[,1]
Vessels<-as.data.frame(Vessel_IDs)
RAW<-RAW[c(-1)]
rownames(RAW)<-Vessel_IDs
rm(Landings_Volume_Value, Vessel_Geography, FF_Landings_and_Diversity)
### I wound up removing weight because I felt as if that and length provided redundant information;
RAW<-RAW[c(-1)] # Delete AVG_LBS


##############################
### Descriptive statistics ###
##############################

### Create database to plot inputs average by cluster ###
Group_Stats <- RAW %>% summarise_each(funs(mean, se=sd(.)/sqrt(n())))

Group_Avg_Revenue<-Group_Stats[c(1,6)]
Group_Avg_Revenue$Var<-"Average annual revenue"
names(Group_Avg_Revenue)<- c("mean", "sd", "Variable")
Group_LAT<-Group_Stats[c(2,7)]
Group_LAT$Var<-"LCG"
names(Group_LAT)<- c("mean", "sd", "Variable")
Group_Inertia<-Group_Stats[c(3,8)]
Group_Inertia$Var<-"Inertia"
names(Group_Inertia)<- c("mean", "sd", "Variable")
Group_Percentage_FF<-Group_Stats[c(4,9)]
Group_Percentage_FF$Var<-"Percent of revenue from CPS"
names(Group_Percentage_FF)<- c("mean", "sd", "Variable")
Group_FF_Diversity<-Group_Stats[c(5,10)]
Group_FF_Diversity$Var<-"CPS diversity index"
names(Group_FF_Diversity)<- c("mean", "sd", "Variable")


Group_Stats_Wide_POST <- rbind(Group_Avg_Revenue, Group_LAT, Group_Inertia, Group_Percentage_FF, Group_FF_Diversity)
rm(Group_Avg_Revenue, Group_LAT, Group_Inertia, Group_Percentage_FF, Group_FF_Diversity)

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
gs4_create("Inputs_new_vessels", sheets = Group_Stats_Wide_POST)

