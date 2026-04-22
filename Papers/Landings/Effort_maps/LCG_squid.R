#######################################################################
### Calculate Latitudinal center of gravity for squid in California ###
#######################################################################


## Load packages ##
library(ggmap)
library(tidyverse)
library(lubridate)
library(here)
library(ggplot2)
library(raster)
library(dplyr)
library(readxl)

squid.logbook <- read.csv(file ="C:\\Data\\CDFW CPS Logbooks\\MarketSquidVesselDataExtract.csv") %>%
  mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>% dplyr::filter(year > 2004)
  

###Step 3: Vessel Center of Gravity and Inertia. Note you will have to load in the "cgi" function described in a separate file before proceeding
###Loading in this package here because if you load it in previously it masks the "select" function used to subset the data.
 logbook_coords_all <-aggregate(CatchEstimate ~
                             VesselID + SetLatitude + SetLongitude + year, 
                           data=squid.logbook, 
                           FUN=sum)

### A Loop that finds the LAT, LON of Center of Gravity of each fishing vessel (as weighted by the value landed at each port) and the length of their primary,
### and secondary axes of dispersion (Distance A & Distance B), often times referred to as range or inertia.
### Run function 
source(here::here("Effort maps", "CGI_Function.R"))
 
Permit_COG<-NULL
for(y in 2005:2020) {
  logbook_coords <- logbook_coords_all %>% dplyr::filter(year == y) %>% drop_na()
  Permit_ID<-as.data.frame(unique(logbook_coords$VesselID))
  names(Permit_ID)<-"VesselID"
  List<-as.list(as.character(Permit_ID$VesselID))
 
  for (i in 1:length(List)) {
    Year = y
    Permit = List[i]
    Single_Permit<- logbook_coords[which(logbook_coords$VesselID==Permit),]
    Single_COG<-cgi(x=Single_Permit$SetLongitude, y=Single_Permit$SetLatitude, z=Single_Permit$CatchEstimate, plot=F)
    Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2],  
                                     Single_COG$xcg),
                             lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2],
                                     Single_COG$ycg),group = c("A", "A", "B", "B","C"))
    Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
    Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
    Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]	
    Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
    Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
    Value<-as.data.frame(c(Permit, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B, Year))
    names(Value)<-c("uniqueid", "LON", "LAT", "DISTANCE_A", "DISTANCE_B", "Year")
    Permit_COG<-rbind(Permit_COG, Value)
  }
}
 
###Any vessel with NaN Values only landed at 1 port, so we change those values to 0
Permit_COG$DISTANCE_A <- sub(NaN, 0, Permit_COG$DISTANCE_A)
Permit_COG$DISTANCE_B <- sub(NaN, 0, Permit_COG$DISTANCE_B)
Permit_COG$DISTANCE_A<-as.numeric(Permit_COG$DISTANCE_A)
Permit_COG$DISTANCE_B<-as.numeric(Permit_COG$DISTANCE_B)

###Produce dissimilarity matrix and pairwise comparisons following methods above
Permit_COG<-Permit_COG[c(1,3,4,6)] %>% filter(LAT != 0)
names(Permit_COG)[1]<-"VesselID"
names(Permit_COG)[2]<-"LCG"
names(Permit_COG)[3]<-"Inertia"
names(Permit_COG)[4]<-"Year"

coeff <- 0.15
Permit_COG_mean  <- Permit_COG %>% mutate(Inertia = Inertia/1000) %>%
  group_by(Year) %>%
  summarise_at(vars(LCG, Inertia), list(mean = mean))


LcgColor <- "#69b3a2"
InertiaColor <- rgb(0.2, 0.6, 0.9, 1)

library(patchwork) 
library(hrbrthemes)

ggplot(data = Permit_COG_mean, aes(x = Year)) + 
  geom_line(aes(y = LCG_mean/coeff), size=2, color=LcgColor) +
  geom_line(aes(y = Inertia_mean), size=2, color=InertiaColor)+
  xlab('Year') +
  scale_y_continuous(name = "Inertia (km)", 
    sec.axis = sec_axis(~.*coeff, name="LCG (Latitude)")) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = InertiaColor, size=13),
    axis.title.y.right = element_text(color = LcgColor, size=13)
  ) 

write.csv(Permit_COG_mean, 
          here::here("Effort maps", "LCG_squid.csv"), 
          row.names = FALSE)
  


