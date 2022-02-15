### Clustering ###

## Merge PacFIN data with results from clustering (~\Clustering\Clustering_Code_2.1.22)

Hierarchical_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\Hierarchical_Vessel_Groups.csv")
# Hierarchical_Vessel_Groups1 <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\Hierarchical_Vessel_Groups1.csv")
# Hierarchical_Vessel_Groups2 <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\Hierarchical_Vessel_Groups2.csv")
# Hierarchical_Vessel_Groups3 <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\Hierarchical_Vessel_Groups3.csv")
# Hierarchical_Vessel_Groups4 <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\Hierarchical_Vessel_Groups4.csv")

PacFIN.month.cluster <- merge(PacFIN.month, Hierarchical_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# PacFIN.month.cluster <- merge(PacFIN.month.cluster, Hierarchical_Vessel_Groups1, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# PacFIN.month.cluster <- merge(PacFIN.month.cluster, Hierarchical_Vessel_Groups2, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# PacFIN.month.cluster <- merge(PacFIN.month.cluster, Hierarchical_Vessel_Groups3, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# PacFIN.month.cluster <- merge(PacFIN.month.cluster, Hierarchical_Vessel_Groups4, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)

# rm(Hierarchical_Vessel_Groups, Hierarchical_Vessel_Groups1, Hierarchical_Vessel_Groups2, 
#    Hierarchical_Vessel_Groups3, Hierarchical_Vessel_Groups4)



## Descriptive statistics

# Gear

library("googlesheets4")
gs4_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


### How gear are used by clusters??? ###
options(scipen=999)
cluster.gear <- PacFIN.month.cluster %>% filter(LANDING_YEAR >= 2000) %>% 
  group_by(group_all, PACFIN_GEAR_CODE) %>% summarise(landings = sum(LANDED_WEIGHT_MTONS.sum)) %>% 
  group_by(group_all) %>% mutate(Percentage = landings / sum(landings)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

cluster.gear <- cluster.gear[order(cluster.gear$group_all, -cluster.gear$Percentage),]
cluster.gear.highest <- cluster.gear %>%
  group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>% 
  dplyr::select('PACFIN_GEAR_CODE') %>% unique()
cluster.gear<-setDT(cluster.gear)[PACFIN_GEAR_CODE %chin% cluster.gear.highest$PACFIN_GEAR_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PACFIN_GEAR_CODE + group_all, cluster.gear ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Gear", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
# gs4_create("Table1", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, PacFIN.cluster)



### In which port each cluster land? ###
options(scipen=999)
cluster.port <- PacFIN.month.cluster %>% filter(LANDING_YEAR >= 2000) %>% 
  group_by(group_all, PACFIN_PORT_CODE) %>% summarise(landings = sum(LANDED_WEIGHT_MTONS.sum)) %>% 
  group_by(group_all) %>% mutate(Percentage = landings / sum(landings)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

cluster.port <- cluster.port[order(cluster.port$group_all, -cluster.port$Percentage),]
cluster.port.highest <- cluster.port %>%
  group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>% 
  dplyr::select('PACFIN_PORT_CODE') %>% unique()

cluster.port<-setDT(cluster.port)[PACFIN_PORT_CODE %chin% cluster.port.highest$PACFIN_PORT_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PACFIN_PORT_CODE + group_all, cluster.port ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
gs4_create("Table2", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.port)


### In which port each cluster land? ###
options(scipen=999)
cluster.port.area <- PacFIN.month.cluster %>% filter(LANDING_YEAR >= 2000) %>% 
  group_by(group_all, PORT_AREA_CODE) %>% summarise(landings = sum(LANDED_WEIGHT_MTONS.sum)) %>% 
  group_by(group_all) %>% mutate(Percentage = landings / sum(landings)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

cluster.port.area <- cluster.port.area[order(cluster.port.area$group_all, -cluster.port.area$Percentage),]
cluster.port.area.highest <- cluster.port.area %>%
  group_by(group_all) %>% filter(row_number()==1:4) %>% ungroup() %>% 
  dplyr::select('PORT_AREA_CODE') %>% unique()

cluster.port.area<-setDT(cluster.port.area)[PORT_AREA_CODE %chin% cluster.port.area.highest$PORT_AREA_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port.area ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
gs4_create("Table3", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.port.area)

```

```{r cluster_descriptive_species}

### In whoch port each cluster land? ###
options(scipen=999)
cluster.port.species <- PacFIN.month.cluster %>% filter(LANDING_YEAR >= 2000) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(landings = sum(LANDED_WEIGHT_MTONS.sum)) %>% 
  group_by(group_all) %>% mutate(Percentage = landings / sum(landings)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.port.species ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
gs4_create("Table3", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.port.area)

```

## Gear used

```{r n_gears_used_vessels, eval=FALSE, include=FALSE}
# How many gears a vessel use in the dataset
PacFIN.CPS.unique.gear <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% 
  select(VESSEL_NUM, PACFIN_GEAR_CODE) %>% unique()

PacFIN.CPS.n_gears <- PacFIN.CPS.unique.gear %>% mutate(n_gears = 1) %>% group_by(VESSEL_NUM) %>% 
  summarise(n_gears= sum(n_gears)) 
summary(PacFIN.CPS.n_gears$n_gears)
sd(PacFIN.CPS.n_gears$n_gears)

rm(PacFIN.CPS.unique.gear, PacFIN.CPS.n_gears)

```

```{r n_species_species_gear, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'}

PacFIN.CPS <- PacFIN.month.forage %>% filter(LANDING_YEAR >= 2000) %>% 
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, PACFIN_GEAR_CODE) %>% 
  unique() %>%  
  mutate(CPS_species = 1) %>%
  group_by(VESSEL_NUM, PACFIN_GEAR_CODE) %>% 
  summarise(n_CPS_landed = sum(CPS_species)) 

table <- xtabs(n_CPS_landed ~ PACFIN_GEAR_CODE, aggregate(n_CPS_landed ~ PACFIN_GEAR_CODE, PacFIN.CPS, mean))
table

rm(PacFIN.CPS, table)

```

## Port landed

```{r n_ports_vessels_CPS, eval=FALSE, include=FALSE}
# How many gears a vessel use in the dataset
PacFIN.CPS.unique.port <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% 
  select(VESSEL_NUM, PACFIN_PORT_CODE) %>% unique() 
PacFIN.CPS.n_ports <- PacFIN.CPS.unique.port %>% mutate(n_ports = 1) %>% 
  group_by(VESSEL_NUM) %>% summarise(n_ports= sum(n_ports)) 
summary(PacFIN.CPS.n_ports$n_ports)
sd(PacFIN.CPS.n_ports$n_ports)

rm(PacFIN.CPS.unique.port, PacFIN.CPS.n_ports)


PacFIN.CPS.unique.area <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>%
  select(VESSEL_NUM, PORT_AREA_CODE) %>% unique() 
PacFIN.CPS.n_ports <- PacFIN.CPS.unique.area %>% mutate(n_ports = 1) %>% 
  group_by(VESSEL_NUM) %>% summarise(n_ports= sum(n_ports)) 
summary(PacFIN.CPS.n_ports$n_ports)
sd(PacFIN.CPS.n_ports$n_ports)

rm(PacFIN.CPS.unique.area, PacFIN.CPS.n_ports)

```

# -----------------------------------------------------------------
### Participation (excluded from paper, enougth to use cluster) ###

# total.revenue <- PacFIN.month %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(year_revenue = sum(AFI_EXVESSEL_REVENUE.sum))
# 
# ### Vessel that participate in Pacific sardine fishery ###
# psdn.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_psdn = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_psdn > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_psdn = mean(revenue_psdn), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_psdn_rev = anual_revenue_psdn/anual_revenue_all) %>%
#   filter(anual_revenue_psdn >= 1000) %>%
#   filter(perc_psdn_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_psdn_fleet = 1)
# 
# ### Vessel that participate in Market squid fishery ###
# msqd.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_msqd = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_msqd > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_msqd = mean(revenue_msqd), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_msqd_rev = anual_revenue_msqd/anual_revenue_all) %>%
#   filter(anual_revenue_msqd >= 1000) %>%
#   filter(perc_msqd_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_msqd_fleet = 1)
# 
# ### Vessel that participate in Northen anchovy fishery ###
# nanc.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("NANC")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_nanc = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_nanc > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_nanc = mean(revenue_nanc), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_nanc_rev = anual_revenue_nanc/anual_revenue_all) %>%
#   filter(anual_revenue_nanc >= 1000) %>%
#   filter(perc_nanc_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_nanc_fleet = 1)
# 
# ### Vessel that participate in Pacific mackerel fishery ###
# cmck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("CMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_cmck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_cmck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_cmck = mean(revenue_cmck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_cmck_rev = anual_revenue_cmck/anual_revenue_all) %>%
#   filter(anual_revenue_cmck >= 1000) %>%
#   filter(perc_cmck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_cmck_fleet = 1)
# 
# ### Vessel that participate in Jack mackerel fishery ###
# jmck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("JMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_jmck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_jmck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_jmck = mean(revenue_jmck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_jmck_rev = anual_revenue_jmck/anual_revenue_all) %>%
#   filter(anual_revenue_jmck >= 1000) %>%
#   filter(perc_jmck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_jmck_fleet = 1)
# 
# ### Vessel that participate in Unknown mackerel fishery ###
# umck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("UMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_umck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_umck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_umck = mean(revenue_umck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_umck_rev = anual_revenue_umck/anual_revenue_all) %>%
#   filter(anual_revenue_umck >= 1000) %>%
#   filter(perc_umck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_umck_fleet = 1)
# 
# 
# # Select vessel that participate in the CPS fishery. 
# PacFIN.month <- PacFIN.month %>% 
#   merge(psdn.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(msqd.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(nanc.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(cmck.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(jmck.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>% 
#   filter(d_psdn_fleet == 1 | d_msqd_fleet == 1 | d_nanc_fleet == 1 | d_cmck_fleet == 1 | d_jmck_fleet == 1) %>%
#   rowwise() %>% 
#   mutate(d_fleet_CPS = sum(d_psdn_fleet, d_msqd_fleet, d_nanc_fleet, d_cmck_fleet, d_jmck_fleet, na.rm = TRUE))
# 
# rm(total.revenue, psdn.fleet, msqd.fleet, nanc.fleet, cmck.fleet, jmck.fleet, umck.fleet)
# 
# # CREATE NEW PACFIN.MONTH.CPS DATASET!
# PacFIN.month.CPS <- PacFIN.month %>% 
#   dplyr::filter(PACFIN_SPECIES_CODE %in% 
#                   c("CMCK", "JMCK", "MSQD", "NANC", "PSDN", "UMCK"))





#-----------------
