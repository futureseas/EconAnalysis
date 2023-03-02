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


### How many tickets per day???

#----------------------------------------------------------------

### Read tickets
Tickets <- readRDS("C:/Data/PacFIN data/Tickets_filtered.rds")
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant)

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

Tickets.CPS <- Tickets %>% 
  dplyr::filter(PACFIN_SPECIES_CODE %in% c("OMCK", "MSQD", "NANC", "PSDN"))



###########################
### Descriptive figures ###
###########################


#----------------------------------
## Figure 1. Average annual landings, prices and revenues by species ##

# Landings
landings.year <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarize(Landings_mtons = sum(Landings_mtons))
landings.year.2000_2014 <- landings.year %>% filter(LANDING_YEAR <2015) %>% mutate(period="2000-2014")
landings.year.2015_2020 <- landings.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
landings.avg.year.2000_2014 <- landings.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(Landings_mtons = mean(Landings_mtons))
landings.avg.year.2015_2020 <- landings.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(Landings_mtons = mean(Landings_mtons))

landings.avg.year <- rbind.data.frame(landings.avg.year.2015_2020,landings.avg.year.2000_2014)

g1 <- ggplot(landings.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=Landings_mtons)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(a) Annual landings") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  theme(legend.position = "right") + guides(fill=guide_legend(title="Period: ")) +
  xlab("") + ylab("Landings (tons)") +
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Price
price.year <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarize(Price_mtons = mean(Price_mtons))
price.year.2000_2014 <- price.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
price.year.2015_2020 <- price.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
price.avg.year.2000_2014 <- price.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(Price_mtons = mean(Price_mtons))
price.avg.year.2015_2020 <- price.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(Price_mtons = mean(Price_mtons))
price.avg.year <- rbind.data.frame(price.avg.year.2015_2020,price.avg.year.2000_2014)

g2 <- ggplot(price.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=Price_mtons)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(b) Average price") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Price (USD/ton)") + 
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Number of vessels
n_vessels <- Tickets %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% summarize(Revenue = sum(Revenue))
n_vessels <- n_vessels %>% mutate(n_vessels = 1)
n_vessels.year <- n_vessels %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>% summarize(n_vessels = sum(n_vessels))
n_vessels.year.2000_2014 <- n_vessels.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
n_vessels.year.2015_2020 <- n_vessels.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
n_vessels.avg.year.2000_2014 <- n_vessels.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, period) %>% summarize(n_vessels = mean(n_vessels))
n_vessels.avg.year.2015_2020 <- n_vessels.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, period) %>% summarize(n_vessels = mean(n_vessels))
n_vessels.avg.year <- rbind.data.frame(n_vessels.avg.year.2015_2020,n_vessels.avg.year.2000_2014)

g4 <- ggplot(n_vessels.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=n_vessels)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(c) Average number of vessels per year") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Average number of vessels") +
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

(g1) / (g2 + g4)

rm(g1, g2, g4, landings.year, 
   landings.avg.year, landings.avg.year.2000_2014, landings.avg.year.2015_2020, 
   landings.year.2000_2014, landings.year.2015_2020, 
   price.avg.year, price.avg.year.2000_2014, price.avg.year.2015_2020, 
   price.year.2000_2014, price.year.2015_2020,
   n_vessels.avg.year, n_vessels.avg.year.2000_2014, n_vessels.avg.year.2015_2020, 
   n_vessels.year.2000_2014, n_vessels.year.2015_2020, 
   price.year, n_vessels, n_vessels.year)


#----------------------------------
## Figure 2. Evolution of landings, number of vessels and availability

# Create dataframes
nvessel.year <- Tickets.CPS %>% 
  dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% 
  unique() %>% mutate(n_vessel = 1) %>% 
  group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarise(n_vessel = sum(n_vessel))

landing.year <- Tickets.CPS %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>% 
  summarize(Landings_mtons = sum(Landings_mtons)) %>% ## ADD HERE SDMs
  inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
  dplyr::select("Landings_mtons", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")


coeff = 1000
df <- landing.year %>% mutate(n_vessel = n_vessel * coeff)

ggplot(df) + 
  geom_line(aes(x=LANDING_YEAR, y = Landings_mtons), color = "#66c2a5") +
  geom_line(aes(x=LANDING_YEAR, y = n_vessel), color = "#8da0cb") +
  facet_wrap(~ PACFIN_SPECIES_CODE, ncol = 2) + 
  scale_y_continuous(name = "Landings (M Tons)", 
                     sec.axis = sec_axis(~./coeff, 
                                         name = "Number of vessels")) +
  geom_point(aes(x=LANDING_YEAR, y = Landings_mtons), color = "#66c2a5") + 
  scale_color_manual(values = c("#66c2a5", "#8da0cb"), labels=c(
    "Landings" = "Landings_mtons", "Number of vessels" = "n_vessel")) + 
  theme(legend.position="bottom")




 
  



#----------------------------------
## Figure 3. Annual average landings by port area ##

# Calculate landings by port and year
landings.port.year.PRE <- Tickets %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE) %>%
  summarize(Landings_mtons = sum(Landings_mtons)) %>% 
  filter(LANDING_YEAR < 2015) %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
  summarize(Landings_mtons = mean(Landings_mtons)) %>%
  filter(PORT_AREA_CODE == "LAA" | 
           PORT_AREA_CODE == "SBA" | 
           PORT_AREA_CODE == "MNA" | 
           PORT_AREA_CODE == "CLO" | 
           PORT_AREA_CODE == "CLW" | 
           PORT_AREA_CODE == "CWA" | 
           PORT_AREA_CODE == "SFA" | 
           PORT_AREA_CODE == "ERA" | 
           PORT_AREA_CODE == "CBA") %>%
  mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                      "LAA", "SBA", "MNA", "SFA", "ERA", "CBA", "CLO", "CLW", "CWA"))

states_names <- as_labeller(c(`C` = "California", `O` = "Oregon",`W` = "Washington"))

gg1 <- ggplot(landings.port.year.PRE, aes(fill=PACFIN_SPECIES_CODE, y=Landings_mtons, x=PORT_AREA_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +
  xlab("") + theme(axis.title = element_text(size = 9)) +
  ylab("Landings (tons)") +
  guides(fill=guide_legend(title="Species: "))  + 
  scale_color_brewer(palette="Set2") +    
  scale_x_discrete(labels=NULL)  + ggtitle("(a) 2000-2014")


landings.port.year.POST <- Tickets %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE) %>%
  summarize(Landings_mtons = sum(Landings_mtons)) %>% 
  filter(LANDING_YEAR >= 2015) %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
  summarize(Landings_mtons = mean(Landings_mtons)) %>%
  filter(PORT_AREA_CODE == "LAA" | 
           PORT_AREA_CODE == "SBA" | 
           PORT_AREA_CODE == "MNA" | 
           PORT_AREA_CODE == "CLO" | 
           PORT_AREA_CODE == "CLW" | 
           PORT_AREA_CODE == "CWA" | 
           PORT_AREA_CODE == "SFA" | 
           PORT_AREA_CODE == "ERA" | 
           PORT_AREA_CODE == "CBA") %>%
  mutate(PORT_AREA_CODE = as.factor(PORT_AREA_CODE)) %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                      "LAA", "SBA", "MNA", "SFA", "ERA", "CBA", "CLO", "CLW", "CWA"))

gg2 <- ggplot(landings.port.year.POST, aes(fill=PACFIN_SPECIES_CODE, y=Landings_mtons, x=PORT_AREA_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position="bottom") + 
  theme(axis.title = element_text(size = 9)) +
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +
  xlab("") + ylab("Landings (tons)") + guides(fill=guide_legend(title="Species: ")) +
  scale_color_brewer(palette="Set2") + 
  scale_x_discrete(labels=c(
    "LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey", 
    "SFA" = "San Francisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
    "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", "SFA" = "San Francisco",
    "CBA" = "Coos Bay")) +
  scale_y_continuous(limits = c(0, 36000)) + ggtitle("(b) 2015-2020")

gg1 / gg2

# rm(landings.port.year.PRE, landings.port.year.POST)



#----------------------------------
## Figure 4. Landings v/s probability of presence by port area ##

PacFIN.month.CPS.nvessels <- left_join(PacFIN.month.CPS, nvessel.year, 
                                 by = c("LANDING_YEAR", "PACFIN_SPECIES_CODE"))

sdm.by.species <- PacFIN.month.CPS.nvessels %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, PSDN_SDM_60, MSQD_SDM_90_JS_CPUE,
                MSQD_SDM_90, MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_v2, 
                NANC_SDM_20, MSQD_recruitment, LANDED_WEIGHT_MTONS.sum, 
                PACFIN_SPECIES_CODE, PORT_AREA_CODE, n_vessel) %>% 
  filter(LANDING_YEAR >= 1998 & LANDING_YEAR <= 2019) %>%
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE, PACFIN_SPECIES_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_CPUE, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm=TRUE),
            MSQD_SPAWN_SDM_90_v2 = mean(MSQD_SPAWN_SDM_90_v2, na.rm=TRUE),
            MSQD_recruitment = mean(MSQD_recruitment, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
            n_vessel = mean(n_vessel, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE",
  "PSDN_SDM_60", "NANC_SDM_20", "MSQD_SDM_90_JS_cpue", "MSQD_SPAWN_SDM_90",
  "MSQD_SDM_90", "MSQD_SPAWN_SDM_90_v2", "MSQD_recruitment"), 
              names_from = PACFIN_SPECIES_CODE,
              values_from = c("LANDED_WEIGHT_MTONS", "n_vessel")) %>% 
  dplyr::rename(Landings_PSDN = LANDED_WEIGHT_MTONS_PSDN) %>% 
  dplyr::rename(Landings_MSQD = LANDED_WEIGHT_MTONS_MSQD) %>% 
  dplyr::rename(Landings_NANC = LANDED_WEIGHT_MTONS_NANC)

area_names <- as_labeller(c(`LAA` = "Los Angeles", `SBA` = "Santa Barbara",
                            `MNA` = "Monterey", `CLO` = "Columbia River (OR)", 
                            `CWA` = "Washington Coastal Ports"))

sdm.by.species$Date <- zoo::as.yearmon(paste(sdm.by.species$LANDING_YEAR, sdm.by.species$LANDING_MONTH, sep='-'))


# Create sardine plot
sdm.by.species.PSDN <- sdm.by.species %>% 
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "CLO") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "MNA", "CLO")) %>%
  group_by(Date, PORT_AREA_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            Landings_PSDN = sum(Landings_PSDN, na.rm = TRUE),
            n_vessel_PSDN = mean(n_vessel_PSDN, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_PSDN / n_vessel_PSDN))


# Create anchovy plot
sdm.by.species.NANC <- sdm.by.species %>% 
  filter(PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "SBA", "MNA", "CWA")) %>%
  group_by(Date, PORT_AREA_CODE) %>% 
  summarize(NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            Landings_NANC = sum(Landings_NANC, na.rm = TRUE),
            n_vessel_NANC = mean(n_vessel_NANC, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_NANC / n_vessel_NANC))


# Plot all squid SDMs outputs

sdm.by.species.MSQD <- sdm.by.species %>% 
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "SBA", "MNA")) %>%
  group_by(Date, PORT_AREA_CODE) %>% 
  summarize(MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_cpue, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm = TRUE),
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm = TRUE),
            MSQD_SPAWN_SDM_90_v2 = mean(MSQD_SPAWN_SDM_90_v2, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm = TRUE),
            MSQD_recruitment = mean(MSQD_recruitment, na.rm=TRUE),
            Landings_MSQD = sum(Landings_MSQD, na.rm = TRUE),
            n_vessel_MSQD = mean(n_vessel_PSDN, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_MSQD / n_vessel_MSQD))



# coeff1 <- 60000
# g1 <- ggplot(sdm.by.species.PSDN) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_PSDN), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = PSDN_SDM_60*coeff1), 
#             size = 0.5, color = "blue", linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")), labeller = area_names,  ncol = 4) + 
#   scale_x_continuous(name = element_blank(), labels = NULL) +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff1, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(b) Pacific sardine (60 km radius)")

g1_2 <- ggplot(sdm.by.species.PSDN) + 
  geom_line(mapping = aes(x = Date, y = RATIO), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = Date, y = PSDN_SDM_60*coeff1_2), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")), labeller = area_names,  ncol = 4) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings by active vessel", sec.axis = sec_axis(~./coeff1_2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Pacific sardine (60 km radius)")






