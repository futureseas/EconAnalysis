###Load packages and set working directory

library(data.table)
library(dplyr)
library(distances)
library(forcats)
library(cluster)
library(ggplot2)

setwd("C:/Data/clusteringcode")
rm(list=ls())

###Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

###Subset the data to get remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, PORT_NAME, VESSEL_NUM, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET))

###Remove records associated with landings of zero value; this is likely bycatch
Tickets<-Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),]


### Find the dominant species by value of each fishing trip, presumably this is the target species. 
### Using the logic of metiers, all landings of each fishing trip should be tagged with a single identifier.
Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_COMMON_NAME, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Dominant"
Trip_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Dominant, by='FTID')

###Subset to select only records where one of the forage fish species of interest was the target species
FF_Tickets<-Tickets[which(Tickets$Dominant=="PACIFIC SARDINE" | Tickets$Dominant=="MARKET SQUID"| 
                            Tickets$Dominant=="NORTHERN ANCHOVY"|
                            Tickets$Dominant=="CHUB MACKEREL"),]
FF_Tickets<-as.data.frame(FF_Tickets)


###Find the list of unique vessels in the subset, these are the vessels we will cluster                           
FF_Vessels<-as.data.frame(unique(FF_Tickets$VESSEL_NUM)) 
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"

###Subset from the complete data set to only retain records associated with these Vessels       
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]            
Tickets<-as.data.frame(Tickets)

# --------------------------------------------------------
#####################
### Create layers ###
#####################
# --------------------------------------------------------
###LAYER 1: Vessel Length & Weight
Tickets$VESSEL_LENGTH<-as.character(Tickets$VESSEL_LENGTH)
Tickets$VESSEL_LENGTH[Tickets$VESSEL_LENGTH==""] <- NA
Tickets$VESSEL_LENGTH[Tickets$VESSEL_LENGTH==0] <- NA
Tickets$VESSEL_LENGTH<-as.factor(Tickets$VESSEL_LENGTH)

##Find the most frequently reported length for each vessel
Dominant_Length<-Tickets %>% group_by(VESSEL_NUM) %>%
  summarize(Length=names(which.max(table(VESSEL_LENGTH))))

Tickets$VESSEL_WEIGHT<-as.character(Tickets$VESSEL_WEIGHT)
Tickets$VESSEL_WEIGHT[Tickets$VESSEL_WEIGHT==""] <- NA
Tickets$VESSEL_WEIGHT[Tickets$VESSEL_WEIGHT==0] <- NA
Tickets$VESSEL_WEIGHT<-as.factor(Tickets$VESSEL_WEIGHT)

##Find the most frequently reported weight for each vessel
Dominant_Weight<-Tickets %>% group_by(VESSEL_NUM) %>%
  summarize(Weight=names(which.max(table(VESSEL_WEIGHT))))

Vessel_Characteristics<-merge(Dominant_Length, Dominant_Weight, by="VESSEL_NUM")
Vessel_Characteristics$Length<-as.numeric(as.character(Vessel_Characteristics$Length))
Vessel_Characteristics$Weight<-as.numeric(as.character(Vessel_Characteristics$Weight))

###Saving a vessel length and weight df as an intermediate output to use later for plotting
Vessel_CharacteristicsRAW<-Vessel_Characteristics
###Because length and weight are in different units, they should be saled FQ: Standarize????
Vessel_Characteristics<-Vessel_Characteristics %>% mutate_at(c("Length", "Weight"), ~(scale(.) %>% as.vector))
##Finds a dissimilarity matrix based on vessel length and weight. IGNORE WARNING MESSAGE
Vessel_CharacteristicsM <-as.matrix(distances(Vessel_Characteristics, id_variable="VESSEL_NUM", normalize="studentize"))
###Transforms the dissimilarity matrix to a list of pairwise comparisons, which I find easier to work with
Vessel_Characteristics_df<-setNames(melt(Vessel_CharacteristicsM), c('V1', 'V2', 'Attributes'))
###Final form of this layer which will be used after all other layers have been reproduced
Vessel_Characteristics_df<-as.data.table(Vessel_Characteristics_df)
###Remove the intermediate matrix as it takes up a lot of memory
rm(Vessel_CharacteristicsM)


# --------------------------------------------------------
###LAYER 2: Vessel Revenue and Poundage 

###Find total poundage and revenue reported by each vessel, calculate dissimilarity matrix, and produce pairwise comparisons following
###the same methods used above
Total_Weight<-aggregate(LANDED_WEIGHT_LBS~VESSEL_NUM, FUN=sum, data=Tickets)
Total_Revenue<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, FUN=sum, data=Tickets)                          
Landings_Volume_Value<-merge(Total_Weight, Total_Revenue, by="VESSEL_NUM") 
Landings_Volume_ValueRAW<-Landings_Volume_Value
Landings_Volume_Value<-Landings_Volume_Value%>% mutate_at(c("LANDED_WEIGHT_LBS", "AFI_EXVESSEL_REVENUE"), ~(scale(.) %>% as.vector))                          
Landings_Volume_ValueM <-as.matrix(distances(Landings_Volume_Value, id_variable="VESSEL_NUM", normalize="studentize"))   
Landings_Volume_Value_df<-setNames(melt(Landings_Volume_ValueM), c('V1', 'V2', 'Attributes'))
Landings_Volume_Value_df<-as.data.table(Landings_Volume_Value_df)
rm(Landings_Volume_ValueM)


# --------------------------------------------------------
###LAYER 3: Vessel Center of Gravity and Inertia. Note you will have to load in the "cgi" function described in a separate file before proceeding
###Loading in this package here because if you load it in previously it masks the "select" function used to subset the data.
library(raster)    

###A file that has the Lat Lon Coordinates of all ports were there has been landings of a fishing trip whose value was dominated by forage fish
Coords<-read.csv("Port_Zips_1.10.22.csv")
Coords<-Coords[c(-1,-4)]

###Append these lat lon coords to the Fish Ticket Database
Ticket_Coords<-merge(Tickets, Coords, by=c("PORT_NAME", "AGENCY_CODE"))
Ticket_Coords<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM+PORT_NAME+AGENCY_CODE+Latitude+Longitude, data=Ticket_Coords, FUN=sum)

###A Loop that finds the LAT, LON of Center of Gravity of each fishing vessel (as weighted by the value landed at each port) and the length of their primary,
### and secondary axes of dispersion (Distance A & Distance B), often times refered to as range or inertia.

Permit_ID<-as.data.frame(unique(Ticket_Coords$VESSEL_NUM))
names(Permit_ID)<-"VESSEL_NUM"
List<-as.list(as.character(Permit_ID$VESSEL_NUM))
Permit_COG<-NULL

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
Vessel_Geography_Raw<-Permit_COG
###I choose to only incorporate Latitude and Distance A, figuring their would not be much variation along the secondary (i.e. longitudinal axis). You could add
###Distance B and LON back in if you wanted to, even make COG and Inertia seperate layers (as I did in the study where this method was developed)
Vessel_Geography<-Permit_COG %>% mutate_at(c("LAT", "DISTANCE_A"), ~(scale(.) %>% as.vector))
Vessel_GeographyM <-as.matrix(distances(Vessel_Geography, id_variable="VESSEL_NUM", normalize="studentize"))
Vessel_Geography_df<-setNames(melt(Vessel_GeographyM), c('V1', 'V2', 'Attributes'))
Vessel_Geography_df<-as.data.table(Vessel_Geography_df)
rm(Vessel_GeographyM)



# --------------------------------------------------------
###LAYER 4: Vessel Catch Composition

###Collapsing all the different species into the primary categories of interest
Tickets<-within(Tickets, Dominant[Dominant == "JACK MACKEREL"] <- "MACKERELS")   
Tickets<-within(Tickets, Dominant[Dominant == "UNSP. MACKEREL"] <- "MACKERELS") 
Tickets<-within(Tickets, Dominant[Dominant == "CHUB MACKEREL"] <- "MACKERELS") 

Top<-c("MARKET SQUID", "PACIFIC SARDINE", "NORTHERN ANCHOVY", "MACKERELS")
Tickets <- mutate(Tickets, Dominant = fct_other(Dominant, keep = Top, other_level = 'Other Species')) 
Tickets$LANDING_MONTH<-as.character(Tickets$LANDING_MONTH)

##Creating a "Season" Variable based on Landing Month
Tickets$Season<- fct_collapse(Tickets$LANDING_MONTH,
                              Q1 = c("1", "2", "3"),
                              Q2 = c("4", "5", "6"),
                              Q3 = c("7", "8", "9"),
                              Q4 = c("10", "11", "12"))

###Creating our dissimilarity comparison based on the percentage of revenue generated by each vessel in each species category and season
Quarterly_Value<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM+Dominant+Season, FUN=sum, data=Tickets)           
Total_Value<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, FUN=sum, data=Tickets)      
Quarterly_Value<-merge(Quarterly_Value, Total_Value, by="VESSEL_NUM")
Quarterly_Value$Percentage<-Quarterly_Value$AFI_EXVESSEL_REVENUE.x/Quarterly_Value$AFI_EXVESSEL_REVENUE.y
Quarterly_Value<- Quarterly_Value %>%
  mutate(Species_Season = paste0(Dominant,"_",Season))
Catch_Composition<-dcast(Quarterly_Value, VESSEL_NUM ~ Species_Season, fun.aggregate=sum, value.var="Percentage", fill=0)
Catch_CompositionM <-as.matrix(distances(Catch_Composition, id_variable="VESSEL_NUM", normalize="studentize"))
Catch_Composition_df<-setNames(melt(Catch_CompositionM), c('V1', 'V2', 'Attributes'))
Catch_Composition_df<-as.data.table(Catch_Composition_df)
rm(Catch_CompositionM)


#---------------------------------------------------------------------------------
###COMBINE EACH INDIVIDUAL DISSIMILARITY LAYER INTO AN AGGREGATE DF
Combined_ALL_df<-left_join(Vessel_Characteristics_df, Landings_Volume_Value_df,  by=c("V1", "V2"), all.x=TRUE, all.y=TRUE)
names(Combined_ALL_df)<-c("V1","V2", "Characteristics", "Volume_Value")
Combined_ALL_df<-left_join(Combined_ALL_df, Vessel_Geography_df,  by=c("V1", "V2"), all.x=TRUE, all.y=TRUE)
names(Combined_ALL_df)[5]<-"Geography" 
Combined_ALL_df<-left_join(Combined_ALL_df, Catch_Composition_df,  by=c("V1", "V2"), all.x=TRUE, all.y=TRUE)
names(Combined_ALL_df)[6]<-"Catch_Composition" 
Combined_ALL_df<-Combined_ALL_df[!(Combined_ALL_df$V1==Combined_ALL_df$V2),]

###Find the average dissimilarity for each pair of vessels. If you add or remove a dissimilarity layer you will want to change the denominator
Combined_ALL_df$Average<-(Combined_ALL_df$Characteristics + Combined_ALL_df$Volume_Value +
                            Combined_ALL_df$Geography + Combined_ALL_df$Catch_Composition)/4
Combined_ALL_df<-as.data.frame(Combined_ALL_df)
Average_df<-Combined_ALL_df[c(1,2,7)]

###Save List of Vessels and convert back into matrices required for clustering inputs
Vessels<-as.data.frame(unique(Average_df$V1))
names(Vessels)<-c("VESSEL_NUM")

Average_matrix<-dcast(Average_df, V1~V2, value.var="Average")
rownames(Average_matrix) <- Average_matrix[,1]
Average_matrix <- Average_matrix[,-1]
Average_matrix<-as.matrix(Average_matrix)
Average_matrix[is.na(Average_matrix)] <- 0
Distance_matrix<-as.dist(Average_matrix)


# --------------------------------------------------------
##########################
### Clustering methods ###
##########################
# --------------------------------------------------------

###CLUSTERING METHOD 1: Hierarchical Clustering
Cluster_obj<-hclust(Distance_matrix, method='ward.D2')
plot(2:35, sapply(2:35, function(i) { 
  mean(silhouette(cutree(Cluster_obj, i), dmatrix=Average_matrix)[,"sil_width"]) }),
  xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20)
####Choose the number of clusters you want
sub_grp <- cutree(Cluster_obj, 5)
###See how many vessels fall ino each cluster
table(sub_grp)
###Create a DF with VESSEL_NUM and Cluster ID
Hierarchical_Vessel_Groups <- Vessels %>% mutate(cluster=sub_grp)
names(Hierarchical_Vessel_Groups)<-c("VESSEL_NUM", "group")

write.csv(Hierarchical_Vessel_Groups,
          "C:\\GitHub\\EconAnalysis\\Data\\Hierarchical_Vessel_Groups.csv", 
          row.names = FALSE)


# --------------------------------------------------------
###CLUSTERING METHOD 2: Partitioning Around Meoids
###Noe that in my previous work I have found this to produce the most intuitive clusters.

###Produces a 'elbow" plot that lets you see how appropriately different numbers of clusters characterize the data. Uses "Distance_Matrix" as input.
Ks=sapply(2:25,
          function(i) 
            summary(silhouette(pam((Distance_matrix), k=i)))$avg.width)

plot(2:25,Ks, xlab="k",ylab="av.silhouette",type="b", pch=19)

##Choose the number of Clusters you want
Clusters<-pam(Distance_matrix, 5)
table(Clusters$clustering)
PAM_Vessel_Groups<-Vessels
###Create a DF with VESSEL_NUM and Cluster ID
PAM_Vessel_Groups$group<-Clusters$clustering

write.csv(PAM_Vessel_Groups,
          "C:\\GitHub\\EconAnalysis\\Data\\PAM_Vessel_Groups.csv", 
          row.names = FALSE)

# --------------------------------------------------------
#############
### Plots ###
#############
# --------------------------------------------------------
###CODE USED TO MAKE BARPLOTS COMPARING RELATIVE LOADINGS OF DIFFERENT CLUSTER INPUTS
###Merge the RAW DFs that we saved earlier
RAW<-merge(Vessel_CharacteristicsRAW, Landings_Volume_ValueRAW, by="VESSEL_NUM")
RAW<-merge(RAW, Vessel_Geography_Raw, by="VESSEL_NUM")

###There was no RAW output for catch composition given that there were so many columns. I am creating one here using the total proportions of each species (omitting the seasonal info),
###for ease of plotting. If two clusters have similar values for these inputs, it is likely that there was some variation in the seasonal version respomnsible for partioning
###them into seperate groups
Catch_Groups<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM+Dominant, data=Tickets, FUN=sum)
Catch_Total<-aggregate(AFI_EXVESSEL_REVENUE~VESSEL_NUM, data=Tickets, FUN=sum)
Catch_Groups<-merge(Catch_Groups, Catch_Total, by="VESSEL_NUM")
Catch_Groups$Percentage<-Catch_Groups$AFI_EXVESSEL_REVENUE.x/Catch_Groups$AFI_EXVESSEL_REVENUE.y
Catch_Groups<-dcast(Catch_Groups, VESSEL_NUM ~ Dominant, fun.aggregate=sum, value.var="Percentage", fill=0)

RAW<-merge(RAW, Catch_Groups, by="VESSEL_NUM")

Vessel_IDs <- RAW[,1]
RAW<-RAW[c(-1)]
###Scaling all inputs togeteher to faciliatate cross variable comparison
RAW_Scaled<-as.data.frame(RAW %>% scale())
RAW_Scaled$VESSEL_NUM<-Vessel_IDs
###Looking at clusters as defined by the selected method 
# (Change between methods here: Hierarchical_ or PAM_)
RAW_Scaled<-merge(RAW_Scaled, Hierarchical_Vessel_Groups, by="VESSEL_NUM")
RAW_Scaled<-RAW_Scaled[c(-1)]

###Finding the mean and S.E. for each variable that we want to visualize for each cluster group
Group_Stats<-RAW_Scaled%>% group_by(group) %>% summarise_each(funs(mean, se=sd(.)/sqrt(n())))


###I like to look at Catch Composition and other characteristics in separate graphs just to streamline things visually. First we will do catch composition.
Group_Mackerels<-Group_Stats[c(1,8,19)]
Group_Mackerels$Var<-"Mackerels"
names(Group_Mackerels)<- c("memb", "mean", "sd", "Variable")
Group_Squid<-Group_Stats[c(1,9,20)]
Group_Squid$Var<-"Squid"
names(Group_Squid)<- c("memb", "mean", "sd", "Variable")
Group_Anchovy<-Group_Stats[c(1,10,21)]
Group_Anchovy$Var<-"Anchovy"
names(Group_Anchovy)<- c("memb", "mean", "sd", "Variable")
Group_Sardine<-Group_Stats[c(1,11,22)]
Group_Sardine$Var<-"Sardine"
names(Group_Sardine)<- c("memb", "mean", "sd", "Variable")
Group_OTH<-Group_Stats[c(1,12,23)]
Group_OTH$Var<-"Other_Not_Forage"
names(Group_OTH)<- c("memb", "mean", "sd", "Variable")

Group_Stats_Wide<-rbind(Group_Squid, Group_Sardine, Group_Anchovy, Group_Mackerels , Group_OTH)
Group_Stats_Wide$memb<-as.factor(Group_Stats_Wide$memb)

ggplot(Group_Stats_Wide, aes(memb, y=mean, fill=Variable)) + geom_bar(stat='identity', position=position_dodge(.9), color="black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, group=Variable), width = 0.4, position=position_dodge(.9)) + 
  theme_classic() + scale_fill_manual(values=c("#F8766D", "#BB9D00","#FF61C9", "#00A5FF", "#00B81F")) + theme(axis.text.x = element_text(angle = 90))


##Now the other attributes.
Group_Length<-Group_Stats[c(1,2,13)]
Group_Length$Var<-"Length"
names(Group_Length)<- c("memb", "mean", "sd", "Variable")
Group_Lton<-Group_Stats[c(1,4,15)]
Group_Lton$Var<-"Landed_Tonnage"
names(Group_Lton)<- c("memb", "mean", "sd", "Variable")
Group_Lrevenue<-Group_Stats[c(1,5,16)]
Group_Lrevenue$Var<-"Landed_Revenue"
names(Group_Lrevenue)<- c("memb", "mean", "sd", "Variable")
Group_LAT<-Group_Stats[c(1,6,17)]
Group_LAT$Var<-"Latitude"
names(Group_LAT)<- c("memb", "mean", "sd", "Variable")
Group_Inertia<-Group_Stats[c(1,7,18)]
Group_Inertia$Var<-"Inertia"
names(Group_Inertia)<- c("memb", "mean", "sd", "Variable")


Group_Stats_Wide<-rbind(Group_Length, Group_Lton, Group_Lrevenue, Group_LAT, Group_Inertia)
Group_Stats_Wide$memb<-as.factor(Group_Stats_Wide$memb)


ggplot(Group_Stats_Wide, aes(memb, y=mean, fill=Variable)) + geom_bar(stat='identity', position=position_dodge(.9), color="black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, group=Variable), width = 0.4, position=position_dodge(.9)) + 
  theme_classic() + scale_fill_manual(values=c("#F8766D", "#BB9D00","#FF61C9", "#00A5FF", "#00B81F")) + theme(axis.text.x = element_text(angle = 90))






