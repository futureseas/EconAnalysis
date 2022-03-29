#######################################################################
### Calculate Latitudinal center of gravity for squid in California ###
#######################################################################

squid.logbook <- read.csv(file ="C:\\Data\\CDFW CPS Logbooks\\MarketSquidVesselDataExtract.csv")

###Step 3: Vessel Center of Gravity and Inertia. Note you will have to load in the "cgi" function described in a separate file before proceeding
###Loading in this package here because if you load it in previously it masks the "select" function used to subset the data.
library(raster)    

logbook_coords<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM+Latitude+Longitude, data=squid.logbook, FUN=sum)

### A Loop that finds the LAT, LON of Center of Gravity of each fishing vessel (as weighted by the value landed at each port) and the length of their primary,
### and secondary axes of dispersion (Distance A & Distance B), often times referred to as range or inertia.

Permit_ID<-as.data.frame(unique(logbook_coords$VESSEL_NUM))
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
