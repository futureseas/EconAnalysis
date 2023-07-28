#################################
### Fishery operation results ###
#################################

rm(list = ls(all.names = TRUE)) 
gc()

##Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)
library(forcats)
library(ggplot2)
library(viridis)
library(patchwork)

#-----------------------------------------------------
### Read tickets
Tickets <- readRDS("C:/Data/PacFIN data/Tickets_filtered.rds")
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant)
FF_Tickets <- Tickets %>% select(FTID) %>% unique()
FF_Vessels <- Tickets %>% select(VESSEL_NUM) %>% unique()


#----------------------------------------------------------------
### Change Species names for easy representation in figures
Tickets <- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
Tickets <- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
Tickets <- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
Tickets <- Tickets %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))
Tickets$date<-as.Date(with(
  Tickets,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")

Tickets.CPS <- Tickets %>% 
  dplyr::filter(PACFIN_SPECIES_CODE %in% c("OMCK", "MSQD", "NANC", "PSDN"))


###########################
### Descriptive figures ###
###########################

# #-------------------------------------------
# ### How many tickets per day?
# colnames(Tickets)
# n_tickets_by_day <- Tickets %>% 
#   select(FTID_unique, date, VESSEL_NUM) %>%
#   unique() %>% group_by(VESSEL_NUM, date) %>%
#   summarize(n_tickets = n()) %>% group_by(n_tickets) %>%
#   summarize(n_obs = n())
# 
# 
# barplot(height=n_tickets_by_day$n_obs, names=as.factor(n_tickets_by_day$n_tickets), 
#         col=rgb(0.8,0.1,0.1,0.6),
#         xlab="Number of tickets in a day", 
#         ylab="Frequency"
# )
# 
# ggplot(n_tickets_by_day, aes(x=as.factor(n_tickets), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Frequency") + xlab("Number of tickets in a day")
# 
# 
# #---------------------------------------------------------------------------
# ### How long trips last??? Filter with the data I actually use...
# 
# ## From PacFIN data
# day_in_sea <- Tickets %>% 
#   select(FTID_unique, max_days_sea) %>% 
#   unique() %>%  
#   group_by(max_days_sea) %>%
#   summarize(n_obs = n()) %>% drop_na() %>%
#   filter(max_days_sea <= 20)
# gg0 <- ggplot(day_in_sea, aes(x=as.factor(max_days_sea), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Number of tickets") + xlab("Number of days in the sea") + 
#   ggtitle("(a) PacFIN data") + 
#   theme(
#     plot.title = element_text(face = "bold", size = 12),
#      axis.ticks = element_line(colour = "grey70", size = 0.2),
#      panel.grid.minor = element_blank()
#  )
# 
# ## From logbooks data
# # Logbooks CDFW
# logbooks.cdfw.msqd <- read.csv(file = "C:/Data/Logbooks/CDFW CPS Logbooks/MarketSquidVesselDataExtract.csv")
# logbooks.cdfw.msqd <- setDT(logbooks.cdfw.msqd)[LandingReceipts %in% FF_Tickets$FTID] # 1,927,235 row deleted
# day_in_sea <- logbooks.cdfw.msqd %>% 
#   select(LandingReceipts, LogDateString) %>% 
#   unique() %>% 
#   group_by(LandingReceipts) %>%
#   summarize(n_days = n()) %>% group_by(n_days) %>%
#   summarize(n_obs = n())
# gg1 <- ggplot(day_in_sea, aes(x=as.factor(n_days), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Number of tickets") + xlab("Number of days in the sea") + 
#   ggtitle("(b) Market squid logbooks from CDFW") + 
#   theme(plot.title = element_text(face = "bold", size = 12),
#         axis.ticks = element_line(colour = "grey70", size = 0.2),
#         panel.grid.minor = element_blank()) +
#   scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"), 
#                    labels = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"), 
#                    limits = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"))
#   
# # Logbooks WDFW
# logbooks.wdfw.psdn <- readxl::read_xlsx("C:\\Data\\Logbooks\\WDFW CPS logbooks\\WA Sardine Logbook Flatfile Data Reques 20-15485.xlsx")
# colnames(logbooks.wdfw.psdn)[1] <- "logbook_number"
# colnames(logbooks.wdfw.psdn)[8] <- "date"
# day_in_sea <- logbooks.wdfw.psdn %>% 
#   select(logbook_number, date) %>% 
#   unique() %>% 
#   group_by(logbook_number) %>%
#   summarize(n_days = n()) %>% group_by(n_days) %>%
#   summarize(n_obs = n())
# gg2 <- ggplot(day_in_sea, aes(x=as.factor(n_days), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Number of tickets") + xlab("Number of days in the sea") + 
#   ggtitle("(c) Pacific sardine logbooks from WDFW") + 
#   theme(plot.title = element_text(face = "bold", size = 12),
#         axis.ticks = element_line(colour = "grey70", size = 0.2),
#         panel.grid.minor = element_blank()) + 
#   scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"), 
#                    labels = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"), 
#                    limits = c("1", "2", "3", "4", "5", "6", "7", "8","9" ,"10"))
# 
# gg0 / (gg1 + gg2)


#-----------------------------------------------------------------------------
# ## Days at sea
# ### Should we filter by number of days in the sea???? ###
#
# participation_data <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")
# day_in_sea <- participation_data %>% 
#   select(trip_id, max_days_sea) %>% 
#   unique() %>%  
#   group_by(max_days_sea) %>%
#   summarize(n_obs = n()) %>% drop_na() 
# # %>%
# #   filter(max_days_sea <= 8)
# ggplot(day_in_sea, aes(x=as.factor(max_days_sea), y=n_obs)) + 
#   geom_bar(stat = "identity", width=0.4) + ylab("Number of tickets") + xlab("Number of days in the sea") + 
#   theme(plot.title = element_text(face = "bold", size = 12),
#         axis.ticks = element_line(colour = "grey70", size = 0.2),
#         panel.grid.minor = element_blank())


#----------------------------------
# ## Figure 1. Average annual landings, prices and revenues by species ##
# 
# # Landings
# landings.year <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
#   summarize(Landings_mtons = sum(Landings_mtons))
# landings.year.2000_2014 <- landings.year %>% filter(LANDING_YEAR <2015) %>% mutate(period="2000-2014")
# landings.year.2015_2020 <- landings.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
# landings.avg.year.2000_2014 <- landings.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
#   summarize(Landings_mtons = mean(Landings_mtons))
# landings.avg.year.2015_2020 <- landings.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
#   summarize(Landings_mtons = mean(Landings_mtons))
# landings.avg.year <- rbind.data.frame(landings.avg.year.2015_2020,landings.avg.year.2000_2014)
# g1 <- ggplot(landings.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=Landings_mtons)) +
#   geom_bar(position="dodge", stat="identity") + ggtitle("(a) Annual landings") + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
#         axis.title = element_text(size = 8)) +
#   theme(legend.position = "none")  +
#   xlab("") + ylab("Landings (tons)") +
#   scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
#                             "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
#                             "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")
# 
# # Price
# price.year <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
#   summarize(Price_mtons = mean(Price_mtons))
# price.year.2000_2014 <- price.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
# price.year.2015_2020 <- price.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
# price.avg.year.2000_2014 <- price.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
#   summarize(Price_mtons = mean(Price_mtons))
# price.avg.year.2015_2020 <- price.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
#   summarize(Price_mtons = mean(Price_mtons))
# price.avg.year <- rbind.data.frame(price.avg.year.2015_2020,price.avg.year.2000_2014)
# g2 <- ggplot(price.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=Price_mtons)) +
#   geom_bar(position="dodge", stat="identity") + ggtitle("(b) Average price") + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
#         axis.title = element_text(size = 8)) + theme(legend.position = "right") +
#   guides(fill=guide_legend(title="Period: ")) +
#   xlab("") + ylab("Price (USD/ton)") + 
#   scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
#                             "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
#                             "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")
# 
# # Availability average
# psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
# psdn.sdm[is.na(psdn.sdm)] <- 0
# Tickets <- merge(Tickets, psdn.sdm, 
#                  by = (c('LANDING_YEAR', 
#                          'LANDING_MONTH', 
#                          'LANDING_DAY', 
#                          'PORT_AREA_CODE')),
#                  all.x = TRUE, 
#                  all.y = FALSE)
# psdn.sdm.mean <- psdn.sdm %>% group_by(LANDING_YEAR) %>% summarize(SDM = mean(PSDN_SDM_90))
# psdn.sdm.mean.2000_2014 <- psdn.sdm.mean %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
# psdn.sdm.mean.2015_2020 <- psdn.sdm.mean %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
# psdn.sdm.avg.year.2000_2014 <- psdn.sdm.mean.2000_2014 %>% group_by(period) %>% summarize(SDM = mean(SDM)) %>% mutate(PACFIN_SPECIES_CODE="PSDN")
# psdn.sdm.avg.year.2015_2020 <- psdn.sdm.mean.2015_2020 %>% group_by(period) %>% summarize(SDM = mean(SDM)) %>% mutate(PACFIN_SPECIES_CODE="PSDN")
# psdn.sdm.avg.year <- rbind.data.frame(psdn.sdm.avg.year.2015_2020, psdn.sdm.avg.year.2000_2014)
# g3 <- ggplot(psdn.sdm.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=SDM)) +
#   geom_bar(position="dodge", stat="identity") + ggtitle("(c) Average probability of presence") + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
#         axis.title = element_text(size = 8)) + theme(legend.position = "none") +
#   xlab("") + ylab("Average probability of presence") +
#   scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
#                             "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
#                             "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")
# 
# 
# # Number of vessels
# n_vessels <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% summarize(Revenue = sum(Revenue))
# n_vessels <- n_vessels %>% mutate(n_vessels = 1)
# n_vessels.year <- n_vessels %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>% summarize(n_vessels = sum(n_vessels))
# n_vessels.year.2000_2014 <- n_vessels.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
# n_vessels.year.2015_2020 <- n_vessels.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
# n_vessels.avg.year.2000_2014 <- n_vessels.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>% summarize(n_vessels = mean(n_vessels))
# n_vessels.avg.year.2015_2020 <- n_vessels.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>% summarize(n_vessels = mean(n_vessels))
# n_vessels.avg.year <- rbind.data.frame(n_vessels.avg.year.2015_2020,n_vessels.avg.year.2000_2014)
# g4 <- ggplot(n_vessels.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=n_vessels)) +
#   geom_bar(position="dodge", stat="identity") + ggtitle("(d) Average number of vessels per year") + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
#         axis.title = element_text(size = 8)) + theme(legend.position = "none") +
#   xlab("") + ylab("Average number of vessels") +
#   scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
#                             "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
#                             "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")
# 
# (g1 + g2) / (g3 + g4)
# 
# rm(g1, g3, g2, g4, landings.year, 
#    landings.avg.year, landings.avg.year.2000_2014, landings.avg.year.2015_2020, 
#    landings.year.2000_2014, landings.year.2015_2020, 
#    price.avg.year, price.avg.year.2000_2014, price.avg.year.2015_2020, 
#    price.year.2000_2014, price.year.2015_2020,
#    n_vessels.avg.year, n_vessels.avg.year.2000_2014, n_vessels.avg.year.2015_2020, 
#    n_vessels.year.2000_2014, n_vessels.year.2015_2020, 
#    price.year, n_vessels, n_vessels.year,
#    psdn.sdm, psdn.sdm.mean.2000_2014, psdn.sdm.mean.2015_2020,
#    psdn.sdm.avg.year.2000_2014, psdn.sdm.avg.year.2015_2020, psdn.sdm.avg.year)


#----------------------------------
## Figure 2. Evolution of landings, number of vessels and availability

# Create dataframes

# nvessel.year <- Tickets.CPS %>% 
#   dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% 
#   unique() %>% mutate(n_vessel = 1) %>% 
#   group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
#   summarise(n_vessel = sum(n_vessel))
# landing.year <- Tickets.CPS %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
#   summarize(Landings_mtons = sum(Landings_mtons)) %>% ## ADD HERE SDMs
#   inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
#   dplyr::select("Landings_mtons", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")
# psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
#   psdn.sdm[is.na(psdn.sdm)] <- 0
#   psdn.sdm.mean <- psdn.sdm %>% group_by(LANDING_YEAR) %>% 
#     summarize(SDM = mean(PSDN_SDM_90))%>% 
#     mutate(PACFIN_SPECIES_CODE = "PSDN") %>%
#     merge(landing.year, by = c("PACFIN_SPECIES_CODE", 
#                                "LANDING_YEAR"), all.y = TRUE)
# 
# coeff = 0.01
# df <- psdn.sdm.mean %>% 
#   mutate (Landings_mtons = Landings_mtons / 100000) %>%
#   mutate(n_vessel = n_vessel * coeff)
# 
# ggplot(df) + 
#   geom_line(aes(x=LANDING_YEAR, y = Landings_mtons, color = "Landings")) +
#   geom_line(aes(x=LANDING_YEAR, y = n_vessel, color = "Number of vessels"), size = 1) +
#   geom_line(aes(x=LANDING_YEAR, y = SDM, color = "Probability of Presence")) +
#   facet_wrap(~ PACFIN_SPECIES_CODE, ncol = 2, labeller = as_labeller(c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
#                                                        "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels"))) + 
#   scale_y_continuous(name = "Landings (10,000 Tons) / Pr(Presence)", 
#                      sec.axis = sec_axis(~./coeff, 
#                                          name = "Number of vessels")) +
#   geom_point(aes(x=LANDING_YEAR, y = Landings_mtons), color ="#66c2a5") +
#   geom_point(aes(x=LANDING_YEAR, y = SDM), color = "#D22B2B", shape = 17) +
#   scale_color_manual(name = "Variables:", 
#                      values = c("Landings" = "#66c2a5",
#                                 "Number of vessels" = "#8da0cb",
#                                 "Probability of Presence" = "#D22B2B")) + 
#   theme(legend.position="bottom") + xlab("Landing Year")


#----------------------------------
## Percentage squid coming from California
colnames(Tickets.CPS)
Tickets.CPS.MSQD <- Tickets.CPS %>% 
  filter(Species_Dominant == "MSQD") %>%
  filter(LANDING_YEAR >= 2000, LANDING_YEAR <= 2020) %>%
  group_by(AGENCY_CODE) %>%
  summarize(Landings = sum(Landings_mtons)) %>% ungroup() %>%
  mutate(total = sum(Landings)) %>% mutate(perc = Landings/total*100)
  



#----------------------------------
## Figure 3. Evolution of landings, number of vessels and availability by port area (one graph per species)

# Create dataframes
nvessel.year <- Tickets.CPS %>% 
  dplyr::select(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, VESSEL_NUM) %>% 
  unique() %>% mutate(n_vessel = 1) %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(n_vessel = sum(n_vessel))
landing.year <- Tickets.CPS %>% group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR) %>% 
  summarize(Landings_mtons = sum(Landings_mtons)) %>% 
  inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR", "PORT_AREA_CODE")) %>%
  dplyr::select("Landings_mtons", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR", "PORT_AREA_CODE")
psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
psdn.sdm[is.na(psdn.sdm)] <- 0
sdm.mean <- psdn.sdm %>% group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM = mean(PSDN_SDM_90))%>% 
  mutate(PACFIN_SPECIES_CODE = "PSDN") %>%
  merge(landing.year, by = c("PACFIN_SPECIES_CODE", "PORT_AREA_CODE", 
                             "LANDING_YEAR"), all.y = TRUE)

## Pacific sardine
psdn.sdm.mean <- sdm.mean %>% filter(PACFIN_SPECIES_CODE == "PSDN") %>%
  filter(PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" |
         PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "SBA") %>% 
  mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "SBA", "MNA", "CLO", "CLW", "CWA"))
coeff = 0.015
df <- psdn.sdm.mean %>% 
  mutate (Landings_mtons = Landings_mtons / 100000) %>%
  mutate(n_vessel = n_vessel * coeff)
ggplot(df) + 
  geom_line(aes(x=LANDING_YEAR, y = Landings_mtons, color = "Landings")) +
  geom_line(aes(x=LANDING_YEAR, y = n_vessel, color = "Number of vessels"), size = 1) +
  geom_line(aes(x=LANDING_YEAR, y = SDM, color = "Probability of Presence")) +
  facet_wrap(~ PORT_AREA_CODE, ncol = 2, labeller = as_labeller(c("LAA" = "Los Angeles",
                                                                  "CLO" = "Columbia River (Oregon)",
                                                                  "CLW" = "Columbia River (Washington)",
                                                                  "CWA" = "Coastal Washinton Ports",
                                                                  "MNA" = "Monterey",
                                                                  "SBA" = "Santa Barbara"))) + 
  scale_y_continuous(name = "Landings (10,000 Tons) / Pr(Presence)", 
                     sec.axis = sec_axis(~./coeff, 
                                         name = "Number of vessels")) +
  geom_point(aes(x=LANDING_YEAR, y = Landings_mtons), color ="#66c2a5") +
  geom_point(aes(x=LANDING_YEAR, y = SDM), color = "#D22B2B", shape = 17) +
  scale_color_manual(name = "Variables:", 
                     values = c("Landings" = "#66c2a5",
                                "Number of vessels" = "#8da0cb",
                                "Probability of Presence" = "#D22B2B")) + 
  theme(legend.position="bottom") + xlab("Landing Year")







# #----------------------------------
# ## Annual average landings by port area ##
# 
# # Calculate landings by port and year
# landings.port.year.PRE <- Tickets %>% 
#   group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE) %>%
#   summarize(Landings_mtons = sum(Landings_mtons)) %>% 
#   filter(LANDING_YEAR < 2015) %>% 
#   group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
#   summarize(Landings_mtons = mean(Landings_mtons)) %>%
#   filter(PORT_AREA_CODE == "LAA" | 
#            PORT_AREA_CODE == "SBA" | 
#            PORT_AREA_CODE == "MNA" | 
#            PORT_AREA_CODE == "CLO" | 
#            PORT_AREA_CODE == "CLW" | 
#            PORT_AREA_CODE == "CWA" | 
#            PORT_AREA_CODE == "SFA" | 
#            PORT_AREA_CODE == "ERA" | 
#            PORT_AREA_CODE == "CBA") %>%
#   mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
#   mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
#                                       "LAA", "SBA", "MNA", "SFA", "ERA", "CBA", "CLO", "CLW", "CWA"))
# 
# states_names <- as_labeller(c(`C` = "California", `O` = "Oregon",`W` = "Washington"))
# 
# gg1 <- ggplot(landings.port.year.PRE, aes(fill=PACFIN_SPECIES_CODE, y=Landings_mtons, x=PORT_AREA_CODE)) +
#   geom_bar(position="dodge", stat="identity") + 
#   facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
#   theme(strip.text.x = element_text(size = 7)) +
#   theme(legend.position = "none") + 
#   theme(plot.title = element_text(size=9, face="bold.italic")) +
#   scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
#                                             "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
#                                             "OTHER" = "Non-CPS")) +
#   xlab("") + theme(axis.title = element_text(size = 9)) +
#   ylab("Landings (tons)") +
#   guides(fill=guide_legend(title="Species: "))  + 
#   scale_color_brewer(palette="Set2") +    
#   scale_x_discrete(labels=NULL)  + ggtitle("(a) 2000-2014")
# 
# 
# landings.port.year.POST <- Tickets %>% 
#   group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE) %>%
#   summarize(Landings_mtons = sum(Landings_mtons)) %>% 
#   filter(LANDING_YEAR >= 2015) %>% 
#   group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
#   summarize(Landings_mtons = mean(Landings_mtons)) %>%
#   filter(PORT_AREA_CODE == "LAA" | 
#            PORT_AREA_CODE == "SBA" | 
#            PORT_AREA_CODE == "MNA" | 
#            PORT_AREA_CODE == "CLO" | 
#            PORT_AREA_CODE == "CLW" | 
#            PORT_AREA_CODE == "CWA" | 
#            PORT_AREA_CODE == "SFA" | 
#            PORT_AREA_CODE == "ERA" | 
#            PORT_AREA_CODE == "CBA") %>%
#   mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
#   mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
#                                       "LAA", "SBA", "MNA", "SFA", "ERA", "CBA", "CLO", "CLW", "CWA"))
# 
# gg2 <- ggplot(landings.port.year.POST, aes(fill=PACFIN_SPECIES_CODE, y=Landings_mtons, x=PORT_AREA_CODE)) +
#   geom_bar(position="dodge", stat="identity") + 
#   facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
#   theme(strip.text.x = element_text(size = 7)) +
#   theme(legend.position="bottom") + 
#   theme(axis.title = element_text(size = 9)) +
#   theme(plot.title = element_text(size=9, face="bold.italic")) +
#   scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
#                                             "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
#                                             "OTHER" = "Non-CPS")) +
#   xlab("") + ylab("Landings (tons)") + guides(fill=guide_legend(title="Species: ")) +
#   scale_color_brewer(palette="Set2") + 
#   scale_x_discrete(labels=c(
#     "LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey", 
#     "SFA" = "San Francisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
#     "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", "SFA" = "San Francisco",
#     "CBA" = "Coos Bay")) +
#   scale_y_continuous(limits = c(0, 36000)) + ggtitle("(b) 2015-2020")
# 
# gg1 / gg2