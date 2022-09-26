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
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PACFIN_PORT_CODE, 
                             VESSEL_NUM, 
                             VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, 
                             VESSEL_OWNER_ADDRESS_CITY, REMOVAL_TYPE_CODE))


#-----------------------------------------------------
#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") %>%
  filter(LANDING_YEAR >= 2009) %>% filter(LANDING_YEAR <= 2016)


#-----------------------------------------------------
####Find the dominant species by value of each fishing trip ( = target species). 
Boats <- dcast(Tickets, FTID ~ PACFIN_SPECIES_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
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
Tickets<-merge(Tickets, Trip_Port_Dominant, by = 'FTID')
rm(Trip_Port_Dominant, X, Boats)


#------------------------------------------------------
### Filter relevant row (targeted species in dominant port)

Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant) 
Tickets <- Tickets %>% filter(PORT_AREA_CODE == Port_Dominant) 


#---------------------------------------
## Case study Squid ##
# Which vessels are landing in Oregon
# Sardine vessels that switches
# Or CA squid that moves

library(scales)

vessel_squid_OR <- Tickets %>% 
  dplyr::filter(Species_Dominant == "MSQD") %>%
  dplyr::filter(AGENCY_CODE == "O") %>% 
  dplyr::filter(LANDING_YEAR >= 2016) %>% 
  select('VESSEL_NUM') %>% unique()

MSQD_OR_Tickets_hist <- setDT(Tickets)[VESSEL_NUM %chin% vessel_squid_OR$VESSEL_NUM] %>%
  group_by(LANDING_YEAR, Species_Dominant, AGENCY_CODE) %>% 
  summarize(Revenue = sum(AFI_EXVESSEL_REVENUE)) %>% group_by(LANDING_YEAR) %>%
  mutate(percentage = Revenue / sum(Revenue))

MSQD_OR_Tickets_hist$species_state <- paste(MSQD_OR_Tickets_hist$Species_Dominant, '-', 
                                              MSQD_OR_Tickets_hist$AGENCY_CODE)

species_included <- MSQD_OR_Tickets_hist %>% 
  group_by(Species_Dominant, LANDING_YEAR) %>% 
  summarise(comp = sum(percentage)) %>% 
  group_by(Species_Dominant) %>% 
  summarise(mean.comp = mean(comp)) %>% 
  filter(mean.comp > 0.05) %>% 
  dplyr::select('Species_Dominant') %>%
  unique()


df <- setDT(MSQD_OR_Tickets_hist)[Species_Dominant %chin% species_included$Species_Dominant]
df <- df %>% dplyr::select('LANDING_YEAR', 'species_state', 'percentage')
  

library(ggplot2)
library(hrbrthemes)
library(viridis)

ggplot(df, aes(fill = species_state, y = percentage, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_ipsum() 


#----------------------------------------------------
### Check catch composition for Purse seine first (cluster specialist)

# install.packages("scales") 

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)

library("scales")

Seine_tickets <- Tickets %>% dplyr::filter(PACFIN_GEAR_CODE == "SEN") %>%
  group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
  summarize(revenue_species = sum(AFI_EXVESSEL_REVENUE))  %>%
  group_by(VESSEL_NUM, PACFIN_SPECIES_CODE) %>% 
  summarize(vessel_yearly_mean_revenue = mean(revenue_species)) %>%
  group_by(PACFIN_SPECIES_CODE) %>% 
  summarize(catch_composition = mean(vessel_yearly_mean_revenue)) %>%
              mutate(percentage = scales::percent(catch_composition/sum(catch_composition))) 

# gs4_create("catch_comp_seine", sheets = Seine_tickets)

  
### How about cluster 
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)

tickets_select_cluster <- Tickets_clust %>% dplyr::filter(PACFIN_GEAR_CODE == "SEN") %>% 
  dplyr::filter(group_all == 4 | group_all == 5) %>%
  group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
  summarize(revenue_species = sum(AFI_EXVESSEL_REVENUE))  %>%
  group_by(VESSEL_NUM, PACFIN_SPECIES_CODE) %>% 
  summarize(vessel_yearly_mean_revenue = mean(revenue_species)) %>%
  group_by(PACFIN_SPECIES_CODE) %>% 
  summarize(catch_composition = mean(vessel_yearly_mean_revenue)) %>%
  mutate(percentage = scales::percent(catch_composition/sum(catch_composition))) 
  
# gs4_create("catch_comp_industrial_cluster", sheets = tickets_select_cluster)


#-----------------------------------------------------
## Check homeowner address
  
port_owner_city <- Tickets_clust %>% 
  filter(VESSEL_OWNER_ADDRESS_CITY != "") %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE, VESSEL_OWNER_ADDRESS_CITY, group_all) %>% 
  summarize(revenue = sum(AFI_EXVESSEL_REVENUE)) %>%
  group_by(VESSEL_OWNER_ADDRESS_CITY, PORT_AREA_CODE, group_all) %>% 
  summarize(revenue_port = mean(revenue)) %>% tidyr::drop_na() %>%
  group_by(VESSEL_OWNER_ADDRESS_CITY, group_all) %>%
  mutate(pecentage = revenue_port/sum(revenue_port)) 

# gs4_create("port_owner_city", sheets = port_owner_city)


#### Calculate diversity index
port_owner_city <- port_owner_city %>% select(VESSEL_OWNER_ADDRESS_CITY, group_all, PORT_AREA_CODE, revenue_port)
port_owner_city <- dcast(port_owner_city, group_all + VESSEL_OWNER_ADDRESS_CITY ~ PORT_AREA_CODE, 
             value.var="revenue_port", fill=0)

cluster <- port_owner_city[,1]
owner_city <- port_owner_city[,2]
port_owner_city <- port_owner_city[,-1]
port_owner_city <- port_owner_city[,-1]


###Calculate the diversity value
port_owner_city <- as.data.frame(diversity(port_owner_city, index = "invsimpson"))
port_owner_city$cluster <- cluster
port_owner_city$VESSEL_OWNER_ADDRESS_CITY <- owner_city

names(port_owner_city) <- c("diversity", "cluster", "VESSEL_OWNER_ADDRESS_CITY")
port_owner_city$diversity[which(!is.finite(port_owner_city$diversity))] <- 0
port_owner_city <- port_owner_city %>% group_by(cluster) %>% summarize(diversity_cluster = mean(diversity))



#-----------------------------------------------------
### Aggregate species in a FTID 
Tickets <- Tickets %>% group_by(FTID, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, 
  PACFIN_SPECIES_COMMON_NAME, Species_Dominant, Port_Dominant) %>% 
  summarize(Landings = sum(LANDED_WEIGHT_LBS), 
            Revenue  = sum(AFI_EXVESSEL_REVENUE))


#-----------------------------------------------------
### Subset to select only records where one of the forage fish species of interest was the target species
### (species in the CPS FMP; squid, sardine, mackerrels and anchovy)

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


###Creating a filter here to only retain vessels with more than 3 forage fish landings (tickets where FF is the dominant species)
###over the time period. I think 3 is appropriate if you are clustering
###several years together, but if you are just clustering a single year than maybe you should drop it down to 1.
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

###Subset from the complete data set to only retain records associated with these Vessels
###Remove records associated with landings of zero value; this is likely bycatch
Tickets<-Tickets[which(Tickets$Revenue > 0),]
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]
Tickets<-as.data.frame(Tickets)
rm(FF_Vessels, FTID_Value)


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
Tickets_clust_2 <- Tickets_clust %>% 
  mutate(selection = paste(Port_Dominant, Species_Dominant, sep = "-", collapse = NULL))


#-----------------------------------------------------
### Expand database 
### Include outside option? Then, expand data when variables are not observed.

#### Se first how many trips per day
n_trips_per_day <- Tickets_clust_2 %>% 
  dplyr::select('VESSEL_NUM', 'FTID', 'LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY') %>% 
  unique() %>% 
  group_by(VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
  summarize(n_trips = n()) 

hist(n_trips_per_day$n_trips, 
     main = '', 
     xlab	= 'Number of trips per day')

library(tidyr)
Tickets_clust_3 <- complete(Tickets_clust_2, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
  mutate(selection = ifelse(is.na(selection), 'No-Participation', selection)) %>%
  mutate(FTID = ifelse(is.na(FTID), paste('NP-',1:n()), FTID))

  Tickets_clust_3 <- Tickets_clust_3 %>% group_by(FTID) %>% mutate(n_obs = n()) %>% 
    ungroup() %>% filter(n_obs==1)


#-----------------------------------------------------
### Create each vessel's choice set 

### --- Start with all the port areas in the database. 
### --- Later, based on the inertia? 

# freq_selection <- count(Tickets_clust_2, 'selection')
# gs4_create("freq_selection", sheets = freq_selection)

choice_set <- Tickets_clust_3 %>% dplyr::select('selection') %>% unique()
choice_set_by_vessel    <- expand.grid(VESSEL_NUM = unique(Tickets_clust_3$VESSEL_NUM), choice_set = unique(Tickets_clust_3$selection))
choice_set_by_vessel_v2 <- Tickets_clust_3 %>% dplyr::select('selection', 'VESSEL_NUM') %>% unique()

gc()
memory.limit(9999999999)
Tickets_clust_4 <- merge(Tickets_clust_3, choice_set_by_vessel_v2, by = ('VESSEL_NUM'), all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE)
gc()
participation_df <- Tickets_clust_4 %>% mutate(choice = ifelse(selection.x == selection.y, 1, 0))
head(participation_df, 25)

### Save logbooks ###
write.csv(Tickets_clust_3, "participation_data.csv", row.names = FALSE)



# #-----------------------------------------------------
# ### Create variables
# 
# #### Expected revenue by port and species (from Peter's paper)
# ## Calculate revenues (obtained by all the fleet within 30 days, excluding the actual haul)
# ##                    (or... the revenues within a finer radius (habit_dist) and from the whole fleet, rather than individual vessel)
# 
# ### First, calculate previous day, year and day/year date
# 
# library(lubridate)
# 
# participation_df$set_date<-as.Date(with(
#   participation_df,
#   paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
#   "%Y-%m-%d")
# 
# ndays = 60
# 
# participation_df$prev_days_date      <- participation_df$set_date       - days(ndays)
# participation_df$prev_year_set_date  <- participation_df$set_date       - days(365)
# participation_df$prev_year_days_date <- participation_df$prev_days_date - days(365)
# 
# 
# ### Calculate interval 
# ##  Create loop
# 
# temp_dat <- participation_df[1, ]
# dum_rev  <- participation_df %>% ungroup %>% dplyr::filter(FTID != temp_dat$haul_id,
#                                               set_date %within% temp_dat$days_inter,
#                                               fleet_name %in% cluster)
# dum_rev <- dum_rev %>% distinct(haul_id, .keep_all = T)
# 
# #Calculate revenue
# mean_rev <- mean(dum_rev$psdn.rev.catch)
# mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
# 
# # Include new information in temp_dat
# temp_dat$mean_rev <- mean_rev
# temp_dat$dummy_prev_days <- dum30_val
# temp_dat$dummy_prev_year_days <- dum30y_val 
# 
# 
# #-----------------------------------------------------
# ### Run a base model...
# library("mlogit")
# 
# # data <- Tickets_clust_3 %>% dplyr::select('selection', 'VESSEL_NUM', 'FTID') %>%
# #   group_by(FTID) %>% mutate(n_obs = n()) %>% ungroup() %>% filter(n_obs==1) %>% 
# #   dplyr::select(-c('n_obs'))
# #   choice_data <- dfidx(data, choice = "selection", 
# #                        idx = c("VESSEL_NUM", "FTID"),
# #                        idnames = c(NA, "alt"))
#   
#   choice_data <- mlogit.data(participation_df, shape = 'long', 
#                              choice = 'choice', alt.var = 'selection.y', chid.var = 'FTID')
# 
#   
#   mf <- mFormula(choice ~ LANDING_YEAR | 0)
#   res <- mlogit(mf, choice_data, reflevel = 'No-Participation')
#     
#   
#     # #List coefficients and rename to align with jeem paper
#     coefs <- coef(res)
#     coefs <- plyr::rename(coefs, c('dummy_prev_days' = 'dum30',
#                                    "dummy_prev_year_days" = "dum30y", 
#                                    "distance:dummy_first" = 'dist1',
#                                    "distance:dummy_not_first" = 'distN',
#                                    'haul_net_revenue.sdm' = 'rev'))
#     
#     
#     # coefs <- data.frame(coefs = round(coefs[c('distN', 'dist1', 'dum30', 'dum30y', 'rev')],
#     #                                   digits = 5))
#     # 
#     ps <- summary(res)$CoefTable[, 4]
#     # 
#     # ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30',
#     #                          "dummy_prev_year_days" = "dum30y", "distance:dummy_first" = 'dist1',
#     #                          "distance:dummy_not_first" = 'distN', 
#     #                          'haul_net_revenue.sdm' = 'rev'))
#     # 
#     # ps <- ps[c('dist1', 'distN', 'dum30', 'dum30y', 'rev')]
# 
#     
#     summary(res)
#     
# 
#    
# 
#     
#   
# 
# # data("Electricity", package = "mlogit")
# # Electricity$chid <- 1:nrow(Electricity)
# # Electr <- dfidx(Electricity, idx = list(c("chid", "id")),
# #                 choice = "choice", varying = 3:26, sep = "")
# # 
# # Elec.mxl <- mlogit(choice ~ pf + cl + loc + wk + tod + seas | 0, Electr, 
# #                    rpar=c(pf = 'n', cl = 'n', loc = 'n', wk = 'n', 
# #                           tod = 'n', seas = 'n'), 
# #                    R = 100, halton = NA, panel = TRUE)
# 
# 
# #####################################################
# #### OTHER ANALYSIS ####
# 
# # ### How many tickets per species?
# 
# # freq_dominant_species <- count(Tickets, 'Species_Dominant')
# # gs4_create("freq_dominant_species_participation", sheets = freq_dominant_species)
# 
