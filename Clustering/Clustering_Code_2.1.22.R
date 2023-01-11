########################
### Clustering code ###
########################

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


#######################
### Data processing ###
#######################

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
                          (Tickets$Dominant == "UNSP. MACKEREL"   & Tickets$AFI_EXVESSEL_REVENUE>0) | 
                          (Tickets$Dominant == "ALBACORE" & 
                            Tickets$PACFIN_SPECIES_COMMON_NAME == "NORTHERN ANCHOVY")),]

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

write.csv(FF_Vessels, "FF_Vessels.csv", row.names = FALSE)

###Subset from the complete data set to only retain records associated with these Vessels       
###Remove records associated with landings of zero value; this is likely bycatch
# Tickets<-Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),]
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]   
Tickets<-as.data.frame(Tickets)

rm(FF_Vessels, FTID_Value)

nrow(as.data.frame(unique(Tickets$FTID))) 
nrow(as.data.frame(unique(Tickets$VESSEL_NUM))) 


######################
### Create metrics ###
######################

#----------------------------------------------
### Step 1: Vessel Length & Weight Metrics ###

Tickets$VESSEL_LENGTH<-as.character(Tickets$VESSEL_LENGTH)
Tickets$VESSEL_LENGTH[Tickets$VESSEL_LENGTH==""] <- NA
Tickets$VESSEL_LENGTH[Tickets$VESSEL_LENGTH==0] <- NA
Tickets$VESSEL_LENGTH<-as.factor(Tickets$VESSEL_LENGTH)

##Find the most frequently reported length for each vessel
Dominant_Length<-Tickets %>% group_by(VESSEL_NUM) %>%
  summarize(Length=names(which.max(table(VESSEL_LENGTH)))) %>%
  mutate(Length = ifelse(VESSEL_NUM == 1240646, 58, Length))

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

rm(Dominant_Length, Dominant_Weight)

#----------------------------------------------
###Step 2: Vessel Revenue and Poundage 
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



##################
### Clustering ###
##################

#----------------------------------------------
####Step 5: Combine metrics, scale metrics, determine optimal cluster number, assign vessels to clusters
RAW<-merge(Vessel_Characteristics, Landings_Volume_Value, by="VESSEL_NUM")
RAW<-merge(RAW, Vessel_Geography, by="VESSEL_NUM")
RAW<-merge(RAW, FF_Landings_and_Diversity, by="VESSEL_NUM")
Vessel_IDs <- RAW[,1]
Vessels<-as.data.frame(Vessel_IDs)
RAW<-RAW[c(-1)]
rownames(RAW)<-Vessel_IDs
rm(Vessel_Characteristics, Landings_Volume_Value, Vessel_Geography, FF_Landings_and_Diversity)
### I wound up removing weight because I felt as if that and length provided redundant information;
RAW<-RAW[c(-1)] # Delete Lenght
RAW<-RAW[c(-1)] # Delete Weight
RAW<-RAW[c(-1)] # Delete AVG_LBS


# Check the degree to which other cluster inputs are collinear using VIF (O'Farrel et al. 2019)
cor(RAW)
M <- lm(AVG_REVENUE ~.,data=RAW)
regclass::VIF(M)
M <- lm(LAT~.,data=RAW)   
regclass::VIF(M)
M <- lm(DISTANCE_A~.,data=RAW)    
regclass::VIF(M)
M <- lm(Percentage~.,data=RAW)     
regclass::VIF(M)
M <- lm(diversity~.,data=RAW) 
regclass::VIF(M)

###Create a scaled data frame of cluster inputs 
## and the accompanying distance matrix
RAW_Scaled<-as.data.frame(RAW %>% scale())
Distance_matrix<-dist(RAW_Scaled, method='euclidean')

#--------------------------------------------------------
## Figure 5. Determine the Optimal Number of Clusters 

## (A) Using NbClust (the Method O'Farrell et al. 2019 use);
## If you don't like the value it spits out, another defensible means
## of choosing would be the peak of the Second differences Dindex Values
## (which in this case is 7)
# dbclust <- NbClust(RAW_Scaled, distance = "euclidean", min.nc=2, max.nc=7,
#              method = "ward.D", index = "all")
#            fviz_nbclust(dbclust)
#            # length(unique(dbclust$Best.partition))

## (B) Using PAM and the "Average Silhouette Method"

Ks=sapply(2:25,
          function(i)
            summary(silhouette(pam((Distance_matrix), k=i)))$avg.width)
          plot(2:25,Ks, xlab="k",ylab="av.silhouette",type="b", pch=19)

          n.clust = 8

#--------------------------------------------------------
# ### Heriarchical Clustering
# 
## Visualize what the dendrogram looks like with this number of clusters
hc <- hclust(Distance_matrix, method="ward.D")
fviz_dend(hc, cex = 0.5, k = n.clust, color_labels_by_k = TRUE)
sub_grp <- cutree(hc, n.clust)
# 
## See how many vessels are in each group
# table(sub_grp)
# Hierarchical_Vessel_Groups <- Vessels %>% mutate(cluster=sub_grp)
# names(Hierarchical_Vessel_Groups)<-c("VESSEL_NUM", "group")
# 
# names(Hierarchical_Vessel_Groups)[names(Hierarchical_Vessel_Groups) == "group"] <- "group_all"
# write.csv(Hierarchical_Vessel_Groups, "Hierarchical_Vessel_Groups.csv", row.names = FALSE)
# 
# rm(hc, sub_grp)


#--------------------------------------------------------
### PAM Clustering
Clusters<-pam(Distance_matrix, n.clust, keep.diss = TRUE)
    Clusters$data <- RAW
    
    ### Figure 8. Principal component analysis
    p <- fviz_cluster(Clusters, geom = "point",
                      main = "") + 
      scale_fill_brewer('Cluster', palette='Set2')  + 
      scale_color_brewer('Cluster', palette='Set2') + 
      scale_shape_manual('Cluster', values=c(1,2,3,4,5,6,7,8)) 
    p        
                 
## Obtain cluster outputs 
PAM_Vessel_Groups<-Vessels
PAM_Vessel_Groups$group<-Clusters$clustering
names(PAM_Vessel_Groups)[1]<-"VESSEL_NUM"
aggregate(VESSEL_NUM~group, FUN=length, data=PAM_Vessel_Groups )

names(PAM_Vessel_Groups)[names(PAM_Vessel_Groups) == "group"] <- "group_all"
    # write.csv(PAM_Vessel_Groups, "PAM_Vessel_Groups.csv", row.names = FALSE)

# ## Check cluster validity

    # library("googlesheets4")
    # gs4_auth(
    #   email = gs4_auth(),
    #   path = NULL,
    #   scopes = "https://www.googleapis.com/auth/spreadsheets",
    #   cache = gargle::gargle_oauth_cache(),
    #   use_oob = gargle::gargle_oob_default(),
    #   token = NULL)

    # compute intercluster distances and intracluster diameters
    library(clv)
    cls.scatt <- cls.scatt.data(RAW, as.integer(Clusters$clustering), dist="euclidean")
  # Intercluster diameters
    intradist <- rbind.data.frame(cls.scatt$intracls.complete, cls.scatt$intracls.average)
    colnames(intradist) = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
    rownames(intradist) = c("Complete distance", "Average distance")
    # gs4_create("Intradist", sheets = intradist)
    
  # Intercluster distances
    interdist <- as.data.frame(cls.scatt$intercls.average)
    colnames(interdist) = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
    rownames(interdist) = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
    # gs4_create("Interdist", sheets = interdist)
    
## Save RAW for analysis with cluster ID
RAW$VESSEL_NUM<-Vessel_IDs
RAW <- merge(RAW, PAM_Vessel_Groups, by="VESSEL_NUM")
write.csv(RAW, "RAW_cluster_inputs.csv", row.names = FALSE)

rm(Ks, Clusters, Vessels, Distance_matrix)

##############################
### Descriptive statistics ###
##############################

#----------------------------------------------
###Step 6: Visualize relative contribution of different cluster inputs

### Figure 6. Random forrest to see importance of each data
library(randomForest)
set.seed(17)
RAW_rf <- RAW[c(-1)]
RAW_rf <- within(RAW_rf, group_all[group_all == 1]  <- "Cluster1")
RAW_rf <- within(RAW_rf, group_all[group_all == 2]  <- "Cluster2")
RAW_rf <- within(RAW_rf, group_all[group_all == 3]  <- "Cluster3")
RAW_rf <- within(RAW_rf, group_all[group_all == 4]  <- "Cluster4")
RAW_rf <- within(RAW_rf, group_all[group_all == 5]  <- "Cluster5")
RAW_rf <- within(RAW_rf, group_all[group_all == 6]  <- "Cluster6")
RAW_rf <- within(RAW_rf, group_all[group_all == 7]  <- "Cluster7")
RAW_rf <- within(RAW_rf, group_all[group_all == 8]  <- "Cluster8")
RAW_rf$group_all <- as.factor(RAW_rf$group_all)  
str(RAW_rf)
RAW.rf <- randomForest(group_all ~ ., data = RAW_rf, importance = TRUE, mtry=2, do.trace=100, ntree = 1000)
print(RAW.rf)
rownames(RAW.rf$importance) <- c("Average annual revenue", "LCG", "Inertia", 
                                 "Percent of revenue from CPS", "CPS diversity index") 

varImpPlot(RAW.rf, main = "", type = 1) 
rm(RAW_rf, RAW.rf)


### Create database to plot inputs average by cluster ###
RAW_Scaled$VESSEL_NUM<-Vessel_IDs
RAW_Scaled<-merge(RAW_Scaled, PAM_Vessel_Groups, by="VESSEL_NUM")
RAW_Scaled<-RAW_Scaled[c(-1)]
Group_Stats<-RAW_Scaled%>% group_by(group_all) %>% summarise_each(funs(mean, se=sd(.)/sqrt(n())))

Group_Avg_Revenue<-Group_Stats[c(1,2,7)]
Group_Avg_Revenue$Var<-"Average annual revenue"
names(Group_Avg_Revenue)<- c("memb", "mean", "sd", "Variable")
Group_LAT<-Group_Stats[c(1,3,8)]
Group_LAT$Var<-"LCG"
names(Group_LAT)<- c("memb", "mean", "sd", "Variable")
Group_Inertia<-Group_Stats[c(1,4,9)]
Group_Inertia$Var<-"Inertia"
names(Group_Inertia)<- c("memb", "mean", "sd", "Variable")
Group_Percentage_FF<-Group_Stats[c(1,5,10)]
Group_Percentage_FF$Var<-"Percent of revenue from CPS"
names(Group_Percentage_FF)<- c("memb", "mean", "sd", "Variable")
Group_FF_Diversity<-Group_Stats[c(1,6,11)]
Group_FF_Diversity$Var<-"CPS diversity index"
names(Group_FF_Diversity)<- c("memb", "mean", "sd", "Variable")
# Group_FF_Months<-Group_Stats[c(1,8,15)]
# Group_FF_Months$Var<-"FF_Months"
# names(Group_FF_Months)<- c("memb", "mean", "sd", "Variable")

Group_Stats_Wide<-rbind(Group_Avg_Revenue, Group_LAT, Group_Inertia, Group_Percentage_FF, Group_FF_Diversity)
Group_Stats_Wide$memb<-as.factor(Group_Stats_Wide$memb)
rm(Group_Avg_Revenue, Group_LAT, Group_Inertia, Group_Percentage_FF, Group_FF_Diversity)

Group_Stats_Wide <- Group_Stats_Wide %>%
  mutate(time.period = period) 
saveRDS(Group_Stats_Wide, file = "stats_input.RDS")  


## Archetype by each cluster ##
RAW_ns<-RAW[c(-1)]
Group_Stats_ns <- RAW_ns %>% group_by(group_all) %>% summarise_each(funs(mean, se=sd(.)/sqrt(n())))

RAW_archetype <- merge(RAW, Group_Stats_ns, by = c('group_all'), all.x = TRUE) %>%
  mutate(AVG_REVENUE_z = abs(AVG_REVENUE - AVG_REVENUE_mean) / AVG_REVENUE_se) %>%      
  mutate(LAT_z = abs(LAT - LAT_mean) / LAT_se) %>%     
  mutate(DISTANCE_A_z = abs(DISTANCE_A - DISTANCE_A_mean) / DISTANCE_A_se) %>%     
  mutate(Percentage_z = abs(Percentage - Percentage_mean) / Percentage_se) %>%     
  mutate(diversity_z = abs(diversity - diversity_mean) / diversity_se) 
  
RAW_archetype$Percentage_z[is.na(RAW_archetype$Percentage_z)] <- 0
RAW_archetype$diversity_z[is.na(RAW_archetype$diversity_z)] <- 0

RAW_archetype <- RAW_archetype %>% 
  mutate(sum_z = AVG_REVENUE_z + LAT_z + DISTANCE_A_z + Percentage_z + diversity_z) %>% 
  group_by(group_all) %>% 
  slice_min(sum_z, n = 2) %>% 
  dplyr::select('VESSEL_NUM', 'AVG_REVENUE', 'LAT', 'DISTANCE_A', 'Percentage', 'diversity') 

# library("googlesheets4")
# gs4_auth(
#   email = "fequezad@ucsc.edu",
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL
# )
# gs4_create("vessel_archetype_by_cluster", sheets = RAW_archetype)
rm(Group_Stats_Wide, Group_Stats, FTID)

### Figure 7. Inputs contribution to each cluster
library(viridis)
Group_Stats_Wide <- readRDS(here::here("Clustering", "stats_input.RDS"))
  ggplot(Group_Stats_Wide, aes(memb, y=mean, fill=Variable)) + 
    geom_bar(stat='identity', position=position_dodge(.9), color="black") + 
    geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, group=Variable), 
                  width = 0.4, position=position_dodge(.9)) + 
    theme_classic()  + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Cluster", y = "Mean (z)") + scale_fill_viridis(discrete = TRUE)
    # scale_fill_brewer(palette="YlGnBu")
  
# rm(Group_Stats_Wide)
  

# -------------------------------------------
##Step 8: Look at descriptive cluster metrics
  
###Look at summary values for revenue and range
RAW$VESSEL_NUM<-Vessel_IDs
RAW<-merge(RAW, PAM_Vessel_Groups, by="VESSEL_NUM")
RAW<-RAW[c(-1)]
Group_Stats<-RAW %>% group_by(group_all.x) %>% summarise_each(funs(mean, se=sd(.)/sqrt(n())))
Group_Stats$DISTANCE_A_mean/10000
Group_Stats$AVG_REVENUE_mean
