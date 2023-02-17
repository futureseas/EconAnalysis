
#--------------------
# DELETED CODE
#--------------------

# #-----------------------------------------------------
# ## Case study Squid ##
# # Which vessels are landing in Oregon
# # Sardine vessels that switches
# # Or CA squid that moves
# 
# library(scales)
# 
# vessel_squid_OR <- Tickets %>% 
#   dplyr::filter(Species_Dominant == "MSQD") %>%
#   dplyr::filter(AGENCY_CODE == "O") %>% 
#   dplyr::filter(LANDING_YEAR >= 2016) %>% 
#   select('VESSEL_NUM') %>% unique()
# 
# MSQD_OR_Tickets_hist <- setDT(Tickets)[VESSEL_NUM %chin% vessel_squid_OR$VESSEL_NUM] %>%
#   group_by(LANDING_YEAR, Species_Dominant, AGENCY_CODE) %>% 
#   summarize(Revenue = sum(AFI_EXVESSEL_REVENUE)) %>% group_by(LANDING_YEAR) %>%
#   mutate(percentage = Revenue / sum(Revenue))
# 
# MSQD_OR_Tickets_hist$species_state <- paste(MSQD_OR_Tickets_hist$Species_Dominant, '-', 
#                                               MSQD_OR_Tickets_hist$AGENCY_CODE)
# 
# species_included <- MSQD_OR_Tickets_hist %>% 
#   group_by(Species_Dominant, LANDING_YEAR) %>% 
#   summarise(comp = sum(percentage)) %>% 
#   group_by(Species_Dominant) %>% 
#   summarise(mean.comp = mean(comp)) %>% 
#   filter(mean.comp > 0.05) %>% 
#   dplyr::select('Species_Dominant') %>%
#   unique()
# 
# 
# df <- setDT(MSQD_OR_Tickets_hist)[Species_Dominant %chin% species_included$Species_Dominant]
# df <- df %>% dplyr::select('LANDING_YEAR', 'species_state', 'percentage')
#   
# 
# library(ggplot2)
# library(hrbrthemes)
# library(viridis)
# 
# ggplot(df, aes(fill = species_state, y = percentage, x = LANDING_YEAR)) + 
#   geom_bar(position="stack", stat="identity") + 
#   scale_fill_brewer(palette = "Paired") + 
#   theme_ipsum() 
# 
# 



# #-----------------------------------------------------
# ### Check catch composition for Purse seine first (cluster specialist)
# 
# # install.packages("scales") 
# 
# library("googlesheets4")
# gs4_auth(
#   email = "fequezad@ucsc.edu",
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL)
# 
# library("scales")
# 
# Seine_tickets <- Tickets %>% dplyr::filter(PACFIN_GEAR_CODE == "SEN") %>%
#   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
#   summarize(revenue_species = sum(AFI_EXVESSEL_REVENUE))  %>%
#   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE) %>% 
#   summarize(vessel_yearly_mean_revenue = mean(revenue_species)) %>%
#   group_by(PACFIN_SPECIES_CODE) %>% 
#   summarize(catch_composition = mean(vessel_yearly_mean_revenue)) %>%
#               mutate(percentage = scales::percent(catch_composition/sum(catch_composition))) 
# 
# # gs4_create("catch_comp_seine", sheets = Seine_tickets)
# 
#   
# ### How about cluster 
# PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
# Tickets_clust <- merge(Tickets, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# rm(PAM_Vessel_Groups)
# 
# tickets_select_cluster <- Tickets_clust %>% dplyr::filter(PACFIN_GEAR_CODE == "SEN") %>% 
#   dplyr::filter(group_all == 4 | group_all == 5) %>%
#   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
#   summarize(revenue_species = sum(AFI_EXVESSEL_REVENUE))  %>%
#   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE) %>% 
#   summarize(vessel_yearly_mean_revenue = mean(revenue_species)) %>%
#   group_by(PACFIN_SPECIES_CODE) %>% 
#   summarize(catch_composition = mean(vessel_yearly_mean_revenue)) %>%
#   mutate(percentage = scales::percent(catch_composition/sum(catch_composition))) 
#   
# # gs4_create("catch_comp_industrial_cluster", sheets = tickets_select_cluster)
# 
# 
# #-----------------------------------------------------
# ## Check homeowner address
#   
# port_owner_city <- Tickets_clust %>% 
#   filter(VESSEL_OWNER_ADDRESS_CITY != "") %>%
#   group_by(LANDING_YEAR, PORT_AREA_CODE, VESSEL_OWNER_ADDRESS_CITY, group_all) %>% 
#   summarize(revenue = sum(AFI_EXVESSEL_REVENUE)) %>%
#   group_by(VESSEL_OWNER_ADDRESS_CITY, PORT_AREA_CODE, group_all) %>% 
#   summarize(revenue_port = mean(revenue)) %>% tidyr::drop_na() %>%
#   group_by(VESSEL_OWNER_ADDRESS_CITY, group_all) %>%
#   mutate(pecentage = revenue_port/sum(revenue_port)) 
# 
# # gs4_create("port_owner_city", sheets = port_owner_city)
# 
# 
# #### Calculate diversity index
# port_owner_city <- port_owner_city %>% select(VESSEL_OWNER_ADDRESS_CITY, group_all, PORT_AREA_CODE, revenue_port)
# port_owner_city <- dcast(port_owner_city, group_all + VESSEL_OWNER_ADDRESS_CITY ~ PORT_AREA_CODE, 
#              value.var="revenue_port", fill=0)
# 
# cluster <- port_owner_city[,1]
# owner_city <- port_owner_city[,2]
# port_owner_city <- port_owner_city[,-1]
# port_owner_city <- port_owner_city[,-1]
# 
# 
# ###Calculate the diversity value
# port_owner_city <- as.data.frame(diversity(port_owner_city, index = "invsimpson"))
# port_owner_city$cluster <- cluster
# port_owner_city$VESSEL_OWNER_ADDRESS_CITY <- owner_city
# 
# names(port_owner_city) <- c("diversity", "cluster", "VESSEL_OWNER_ADDRESS_CITY")
# port_owner_city$diversity[which(!is.finite(port_owner_city$diversity))] <- 0
# port_owner_city <- port_owner_city %>% group_by(cluster) %>% summarize(diversity_cluster = mean(diversity))

# #-----------------------------------------------------
# ####Find the dominant port by value of each fishing trip ( = target species). 
# Boats<-dcast(Tickets, FTID ~ PORT_AREA_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
# row.names(Boats) <- Boats$FTID
# FTID<-Boats$FTID
# Boats<-Boats[,-(1)]
# X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
# colnames(X)<-"Port_Dominant"
# Trip_Port_Dominant<-as.data.frame(cbind(FTID,X))
# Tickets<-merge(Tickets, Trip_Port_Dominant, by = 'FTID')
# rm(Trip_Port_Dominant, X, Boats)



