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

#-----------------------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

#-----------------------------------------------------
### Add port area
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
Tickets_raw <- Tickets_raw %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
rm(port_area)


#-----------------------------------------------------
###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets_raw, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PORT_AREA_CODE,
                                 VESSEL_NUM, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE,
                                 PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME,
                                 VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, FISHER_LICENSE_NUM, AFI_PRICE_PER_POUND, 
                                 VESSEL_OWNER_ADDRESS_CITY, REMOVAL_TYPE_CODE, NUM_OF_DAYS_FISHED, CATCH_AREA_CODE)) %>% 
  filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


#-----------------------------------------------------
### Create unique trip ID
Tickets$FTID_unique <- udpipe::unique_identifier(Tickets, fields = c("FTID", "VESSEL_NUM", "LANDING_YEAR"))


#-----------------------------------------------------
###Subset the data to remove columns not relevant to this analysis. This will speed things up.

PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))

PacFIN.month.CPS <- PacFIN.month %>% 
  dplyr::filter(PACFIN_SPECIES_CODE %in% 
                  c("OMCK", "MSQD", "NANC", "PSDN"))

###########################
### Descriptive figures ###
###########################

#----------------------------------
## Figure 1. Annual average landings by port area ##

# Calculate landings by port and year
landings.by.port.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ 
                                     PACFIN_SPECIES_CODE + PORT_AREA_CODE + LANDING_YEAR + AGENCY_CODE, FUN = sumfun, data=PacFIN.month)
landings.by.port.year.PRE <- landings.by.port.year %>% filter(LANDING_YEAR <2015)
landings.by.port.avg.year <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + PORT_AREA_CODE 
                                       + AGENCY_CODE, FUN=meanfun, data=landings.by.port.year.PRE) %>%
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" |
           PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS") %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                     "LAA", "SBA", "MNA", "CLO", "CLW", "CWA", "NPS"))

# filter(PACFIN_PORT_CODE == "AST" | PACFIN_PORT_CODE == "HNM" | PACFIN_PORT_CODE == "SP" |
#          PACFIN_PORT_CODE == "VEN" | PACFIN_PORT_CODE == "TRM" | PACFIN_PORT_CODE == "MOS" |
#          PACFIN_PORT_CODE == "WPT" | PACFIN_PORT_CODE == "MNT" | PACFIN_PORT_CODE == "LWC" | 
#          PACFIN_PORT_CODE == "PRN" | PACFIN_PORT_CODE == "ERK" ) %>%
#   mutate(PACFIN_PORT_CODE = fct_relevel(PACFIN_PORT_CODE, 
#                                         "SP", "WLM", "TRM", "HNM", "VEN", "MNT", "MOS", "PRN", 
#                                         "SF", "SLT", "ERK", "WIN", "NEW", "AST", "LWC", "WPT"))
# PACFIN_PORT_CODE == "PRN" | PACFIN_PORT_CODE == "WLM" | PACFIN_PORT_CODE == "WIN"

states_names <- as_labeller(c(`C` = "California", `O` = "Oregon",`W` = "Washington"))


gg1 <- ggplot(landings.by.port.avg.year, aes(fill=PACFIN_SPECIES_CODE, 
                                      y=LANDED_WEIGHT_MTONS.sum.sum.mean, x=PORT_AREA_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +
  theme(axis.text.x = element_text(angle=90, hjust=0.95, vjust=0.45)) +
  xlab("") + 
  theme(axis.title = element_text(size = 9)) +
  ylab("Landings (tons)") + theme(legend.position="none") +
  guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=NULL) +
  scale_color_brewer(palette="Set2") + ggtitle("(a) 2000-2014")

# "SP" = "San\nPedro", "HNM" = "Hueneme", 
# "MNT" = "Monterrey", "MOS" = "Moss\nLanding", 
# "LWC" = "Ilwaco /\nChinook", "AST" = "Astoria", 
# "PRN" = "Princeton/\nHalf Moon\nBay", "TRM" = "Terminal\nIsland",
# "VEN" = "Ventura", "WIN" = "Winchester\nBay",
# "WLM" = "Willmington", "WPT" = "Westport", "NEW" = "Newport", 
# "ERK" = "Eureka"
# Calculate landings by port and year


landings.by.port.year.POST <- landings.by.port.year %>% filter(LANDING_YEAR >=2015)
landings.by.port.avg.year <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + PORT_AREA_CODE 
                                       + AGENCY_CODE, FUN=meanfun, data=landings.by.port.year.POST) %>%
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" |
           PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS") %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                      "LAA", "SBA", "MNA", "CLO", "CLW", "CWA", "NPS"))

gg2 <- ggplot(landings.by.port.avg.year, aes(fill=PACFIN_SPECIES_CODE, 
                                             y=LANDED_WEIGHT_MTONS.sum.sum.mean, x=PORT_AREA_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position="bottom") + 
  theme(axis.title = element_text(size = 9)) +
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +
  xlab("") + 
  ylab("Landings (tons)") + guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=c(
    "LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey", 
    "SFA" = "San Francisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
    "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", "NPS" = "North Puget\nSound")) +
  scale_color_brewer(palette="Set2") + ggtitle("(b) 2015-2020") +   
  scale_y_continuous(limits = c(0, 33750), breaks = seq(0, 33750, 10000))

gg1 / gg2



rm(landings.by.port.avg.year, landings.by.port.year, states_names)


#----------------------------------
## Figure 2. Fishing seasons ##

port_area_names <- as_labeller(c("LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey", 
                                 "CLO" = "Columbia\nRiver (OR)", "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports"))

# port_names <- as_labeller(c("SP" = "San Pedro", "HNM" = "Hueneme", "MNT" = "Monterrey", "MOS" = "Moss Landing", 
#                             "LWC" = "Ilwaco / Chinook", "AST" = "Astoria", 
#                             "PRN" = "Princeton/ Half Moon\nBay", "SF" = "San Francisco", 
#                             "SLT" = "Sausalito", "TRM" = "Terminal Island",
#                             "VEN" = "Ventura", "WPT" = "Westport"))

q.psdn.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%  
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_PSDN = sum(LANDED_WEIGHT_MTONS.sum)) %>%  
  group_by(LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_PSDN = mean(Landings_PSDN)) %>%
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA" |
           PORT_AREA_CODE == "CLO") 

g1 <- ggplot(data=q.psdn.by.month, aes(x=LANDING_MONTH, y=Landings_PSDN)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) + 
  scale_x_continuous(name = "", breaks=1:12) +
  scale_y_continuous(name = "Landings (M tons)" ) + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")), 
             labeller = port_area_names, ncol = 1) + ggtitle("(a) Pacific sardine") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))

q.msqd.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%  
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_MSQD = sum(LANDED_WEIGHT_MTONS.sum)) %>%  
  group_by(LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_MSQD = mean(Landings_MSQD)) %>%
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" |
           PORT_AREA_CODE == "MNA") 

g2 <- ggplot(data=q.msqd.by.month, aes(x=LANDING_MONTH, y=Landings_MSQD)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) + 
  scale_x_continuous(name = "Month", breaks=1:12) +
  scale_y_continuous(name = "" ) + 
  facet_wrap(~factor(PORT_AREA_CODE, levels = c("LAA", "SBA", "MNA")), 
             labeller = port_area_names, ncol = 1) + ggtitle("(b) Market squid") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))

q.nanc.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  filter(PACFIN_SPECIES_CODE  %in% c("NANC")) %>%  
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_NANC = sum(LANDED_WEIGHT_MTONS.sum)) %>%  
  group_by(LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarise(Landings_NANC = mean(Landings_NANC)) %>%
  filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" |
           PORT_AREA_CODE == "CWA") 

g3 <- ggplot(data=q.nanc.by.month, aes(x=LANDING_MONTH, y=Landings_NANC)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) + 
  scale_x_continuous(name = "", breaks=1:12) +
  scale_y_continuous(name = "" ) + 
  facet_wrap(~factor(PORT_AREA_CODE, levels = c("SBA", "MNA", "CWA")), 
             labeller = port_area_names, ncol = 1) + ggtitle("(c) Northern anchovy") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))


g1 + g2 + g3

rm(g1, g2, g3, q.psdn.by.month, q.msqd.by.month, q.nanc.by.month, port_area_names)



#----------------------------------
## Figure 3. Average annual landings, prices and revenues by species ##

# Landings
landings.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sumfun, data=PacFIN.month)
landings.year.2000_2014 <- landings.year %>% filter(LANDING_YEAR <2015) %>% mutate(period="2000-2014")
landings.year.2015_2020 <- landings.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
landings.avg.year.2000_2014 <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=landings.year.2000_2014)
landings.avg.year.2015_2020 <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=landings.year.2015_2020)
landings.avg.year <- rbind.data.frame(landings.avg.year.2015_2020,landings.avg.year.2000_2014)

g1 <- ggplot(landings.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=LANDED_WEIGHT_MTONS.sum.sum.mean)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(a) Annual landings") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Landings (tons)") +
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Price
price.year <- summaryBy(AFI_PRICE_PER_MTON.mean ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=meanfun, da=PacFIN.month)
price.year.2000_2014 <- price.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
price.year.2015_2020 <- price.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
price.avg.year.2000_2014 <- summaryBy(AFI_PRICE_PER_MTON.mean.mean ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=price.year.2000_2014)
price.avg.year.2015_2020 <- summaryBy(AFI_PRICE_PER_MTON.mean.mean ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=price.year.2015_2020)
price.avg.year <- rbind.data.frame(price.avg.year.2015_2020,price.avg.year.2000_2014)

g2 <- ggplot(price.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=AFI_PRICE_PER_MTON.mean.mean.mean)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(c) Average annual price") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + guides(fill=guide_legend(title="Period: ")) +
  xlab("") + ylab("Price (USD/ton)") + 
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Revenue
revenue.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sumfun, da=PacFIN.month)
revenue.year.2000_2014 <- revenue.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
revenue.year.2015_2020 <- revenue.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
revenue.avg.year.2000_2014 <- summaryBy(AFI_EXVESSEL_REVENUE.sum.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=revenue.year.2000_2014)
revenue.avg.year.2015_2020 <- summaryBy(AFI_EXVESSEL_REVENUE.sum.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=revenue.year.2015_2020)
revenue.avg.year <- rbind.data.frame(revenue.avg.year.2015_2020,revenue.avg.year.2000_2014)

g3 <- ggplot(revenue.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=AFI_EXVESSEL_REVENUE.sum.sum.mean)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(b) Annual revenue") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Annual revenues (Millions of USD)") +  scale_y_continuous(labels = label_number(scale = 1e-6)) +
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Number of vessels
n_vessels <- summaryBy(AFI_EXVESSEL_REVENUE.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR + VESSEL_NUM, FUN=sumfun, da=PacFIN.month)
  n_vessels <- n_vessels %>% mutate(n_vessels = 1)
n_vessels.year <- summaryBy(n_vessels ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sumfun, da=n_vessels)
n_vessels.year.2000_2014 <- n_vessels.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
n_vessels.year.2015_2020 <- n_vessels.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")
n_vessels.avg.year.2000_2014 <- summaryBy(n_vessels.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=n_vessels.year.2000_2014)
n_vessels.avg.year.2015_2020 <- summaryBy(n_vessels.sum ~ PACFIN_SPECIES_CODE + period, FUN=meanfun, da=n_vessels.year.2015_2020)
n_vessels.avg.year <- rbind.data.frame(n_vessels.avg.year.2015_2020,n_vessels.avg.year.2000_2014)

g4 <- ggplot(n_vessels.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=n_vessels.sum.mean)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(d) Average number of vessels per year") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Average number of vessels") +
  scale_x_discrete(labels=c("OMCK" = "Mackerels", "OTHER" = "Non-CPS",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

(g1 + g3) / (g2 + g4)

rm(g1, g2, g3, g4, landings.year, 
   landings.avg.year, landings.avg.year.2000_2014, landings.avg.year.2015_2020, 
   landings.year.2000_2014, landings.year.2015_2020, 
   price.avg.year, price.avg.year.2000_2014, price.avg.year.2015_2020, 
   price.year.2000_2014, price.year.2015_2020,
   n_vessels.avg.year, n_vessels.avg.year.2000_2014, n_vessels.avg.year.2015_2020, 
   n_vessels.year.2000_2014, n_vessels.year.2015_2020, 
   revenue.avg.year, revenue.avg.year.2000_2014, revenue.avg.year.2015_2020, 
   revenue.year.2000_2014, revenue.year.2015_2020, 
   revenue.year, price.year, n_vessels, n_vessels.year)



#----------------------------------
## Figure 4. Evolution of annual total landings and average annual prices ##

## Calculate average price and total landings by species per month
landing.price.year <- summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~
                        PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month)

  landing.price.year.sel <- landing.price.year %>% 
    dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean",
                  "PACFIN_SPECIES_CODE", "LANDING_YEAR")

# Graph landings v/s number of vessels
coeff1 <- 100
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff1 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g1 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  ggtitle("(a) Market squid") +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff1, name = "Price (USD/Ton)")) +
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + # ggtitle("(b) Market Squid")  +  
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))

coeff2 <- 300
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "PSDN")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff2 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))

g2 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff2, name = "Price (USD/Ton)")) + 
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Pacific sardine")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

coeff3 <- 25
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "NANC")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff3
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g3 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff3, name = "Price (USD/Ton)")) + 
  theme(legend.position="bottom") + scale_x_continuous(name = "Year") +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(c) Northern anchovy")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

(g1 / g2 / g3)



rm(df, g1, g2, landing.price.year, landing.price.year.sel, coeff1, coeff2)



#----------------------------------
## Figure 5. Evolution of pacific sardine and squid landings and the number of vessels landing squid

# Create dataframes
nvessel.year <- PacFIN.month %>% dplyr::filter(LANDED_WEIGHT_MTONS.sum > 0) %>%
  dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% unique() %>% 
  mutate(n_vessel = 1) %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarise(n_vessel = sum(n_vessel))

landing.price.year.sel <- summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~
  PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month) %>%
  inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
  dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")


# Graph landing v/s number of  vessels
coeff <- 1500
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD" | PACFIN_SPECIES_CODE == "PSDN") %>%
  mutate(n_vessel = ifelse(PACFIN_SPECIES_CODE == "MSQD", n_vessel, 0)) %>%
  mutate(LANDED_WEIGHT_MTONS.sum.sum = ifelse(PACFIN_SPECIES_CODE == "PSDN",
                                              LANDED_WEIGHT_MTONS.sum.sum, 
                                              ifelse(PACFIN_SPECIES_CODE == "MSQD",
                                                     LANDED_WEIGHT_MTONS.sum.sum,
                                                     0))) %>%
  group_by(LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
  summarise(n_vessel = sum(n_vessel), LANDED_WEIGHT_MTONS.sum.sum = sum(LANDED_WEIGHT_MTONS.sum.sum)) %>%
  reshape2::melt(id.vars=c("LANDING_YEAR", "PACFIN_SPECIES_CODE")) %>%
  reshape2::dcast(LANDING_YEAR ~ PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select(-c("PSDN_n_vessel"))


df$MSQD_n_vessel <- df$MSQD_n_vessel * coeff
df <- gather(df, key = Variable, value = value,
             c("MSQD_n_vessel", "MSQD_LANDED_WEIGHT_MTONS.sum.sum", "PSDN_LANDED_WEIGHT_MTONS.sum.sum"))
ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + 
  geom_line(aes(linetype=Variable), size=1) +
  geom_point() + 
  scale_linetype_manual(values=c("solid", "dashed", "solid"), labels=c(
                              "PSDN_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of Pacific sardine",
                              "MSQD_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of market squid",                     
                              "MSQD_n_vessel" = "Number of squid vessels")) +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff, name = "Number of vessels landing market squid")) +
  theme(legend.position="bottom") + scale_x_continuous(name = element_blank()) +
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels=c(
                           "PSDN_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of Pacific sardine",
                           "MSQD_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of market squid",                     
                           "MSQD_n_vessel" = "Number of squid vessels")) 




#----------------------------------
## Figure 6. Evolution of squid landings relative to sardine landings and number of vessels landing squid

df2 <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD" | PACFIN_SPECIES_CODE == "PSDN") %>%
  mutate(n_vessel = ifelse(PACFIN_SPECIES_CODE == "MSQD", n_vessel, 0)) %>%
  mutate(LANDED_WEIGHT_MTONS.sum.sum = ifelse(PACFIN_SPECIES_CODE == "PSDN",
                                              LANDED_WEIGHT_MTONS.sum.sum, 
                                              ifelse(PACFIN_SPECIES_CODE == "MSQD",
                                                     LANDED_WEIGHT_MTONS.sum.sum,
                                                     0))) %>%
  group_by(LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
  summarise(n_vessel = sum(n_vessel), LANDED_WEIGHT_MTONS.sum.sum = sum(LANDED_WEIGHT_MTONS.sum.sum)) %>%
  reshape2::melt(id.vars=c("LANDING_YEAR", "PACFIN_SPECIES_CODE")) %>%
  reshape2::dcast(LANDING_YEAR ~ PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select(-c("PSDN_n_vessel")) %>% mutate(r_landing_vessels = MSQD_LANDED_WEIGHT_MTONS.sum.sum / MSQD_n_vessel)


gg1 <- ggplot(df2, aes(x=LANDING_YEAR, y = r_landing_vessels)) + 
  geom_line() +
  ggtitle("(a) Average landing by active vessels") +
  geom_point() + 
  scale_linetype_manual(values=c("dashed")) +
  scale_y_continuous(name = "Ratio (Average landing by active vessels)") + xlab("Landing year")
  rm(df, df2, coeff)
  
  
  
  # Load data #
  sqd.logbook.vessel <- read.csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
    dplyr::rename(lat = SetLatitude) %>%
    dplyr::rename(lon = SetLongitude) %>%
    dplyr::rename(LogSerialNumber = ï..LogSerialNumber) %>%
    mutate(vessel="CA Vessel") %>%
    mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
    mutate(LANDING_YEAR = year(date)) %>%
    mutate(month = month(date)) %>%
    dplyr::rename(effort = "CatchEstimate") 
  
  max_n_set.by.trip <- sqd.logbook.vessel %>% 
    filter(SetNumber>0 & SetNumber<99) %>%
    group_by(LogSerialNumber) %>%
    filter(SetNumber == max(SetNumber, na.rm=TRUE)) %>% 
    dplyr::select(c('LogSerialNumber', 'SetNumber', 'LANDING_YEAR')) %>% unique() %>%
    group_by(LANDING_YEAR) %>% summarise(avg_set = mean(SetNumber))
  
  
gg2 <- max_n_set.by.trip %>%
  ggplot(aes(x=LANDING_YEAR, y=avg_set)) +
  ggtitle("(b) Average number of sets") + 
    geom_line() +
    geom_point() + 
  scale_y_continuous(name = "Average number of sets") + xlab("Landing year")


  
gg1 + gg2
  


#----------------------------------
## Figure 7. Landings v/s probability of presence by port area ##

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


# coeff2 <- 12000
# g2 <-  ggplot(sdm.by.species.NANC) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_NANC, color = "Landings"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = NANC_SDM_20*coeff2, color = "Probability of presence"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("SBA", "MNA", "CWA")), labeller = area_names,  ncol = 4) + 
#   scale_x_continuous(name = "Year")  +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8),
#         legend.position="bottom") + 
#   ggtitle("(b) Northern anchovy (20 km radius)") +  
#   scale_color_manual(name = "Variable: ", 
#                      values = c("Landings" = "grey", "Probability of presence" = "blue"))

g2_2 <-  ggplot(sdm.by.species.NANC) + 
  geom_line(mapping = aes(x = Date, y = RATIO, color = "Landings by active vessel"), size = 0.5) +
  geom_line(mapping = aes(x = Date, y = NANC_SDM_20*coeff2_2, color = "Probability of presence"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("SBA", "MNA", "CWA")), labeller = area_names,  ncol = 4) + 
  scale_x_continuous(name = "Month")  +
  scale_y_continuous(name = "Landings by active vessel", sec.axis = sec_axis(~./coeff2_2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8),
        legend.position="bottom") + 
  ggtitle("(c) Northern anchovy (20 km radius)") +  
  scale_color_manual(name = "Variable: ", 
                     values = c("Landings by active vessel" = "grey", "Probability of presence" = "blue"))


# coeff3 <- 500000
# g3 <-  ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90_v2*coeff3), 
#             size = 0.5, linetype = "dashed", color = "blue") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = element_blank(), labels = NULL)  +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff3, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="bottom") + 
#   ggtitle("(a) Market squid (spawning aggregation model; 90 km radius; August - October)") 
# 
# coeff4 <- 100000
# g4 <- ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD, color = "Landings"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90*coeff4, color = "SDM output"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 1) + 
#   scale_x_continuous(name = "Year")  +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff4, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") + 
#   ggtitle("(b) Market squid (spawning aggregation model; 90 km radius)") +  
#   scale_color_manual(name = "Variable: ", 
#                      values = c("Landings" = "grey", "SDM output" = "blue"))
# 
# coeff5 <- 30000
# g5 <- ggplot(sdm.by.species.MSQD) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD, color = "Landings"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90*coeff5, color = "SDM output"),
#             size = 0.5, linetype = "dashed") +
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 1) +
#   scale_x_continuous(name = "Year")  +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff4, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"),
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") +
#   ggtitle("(b) Market squid (90 km radius)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Landings" = "grey", "SDM output" = "blue"))
# 
# coeff5 <- 8000
# g6 <-  ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90_JS_cpue*coeff5), 
#             size = 0.5, color = "blue", linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = element_blank(), labels = NULL) +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff5, name = "Abundance (CPUE)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(c) Market squid (Justin Suca's abundance; August - October; 90 km radius)")
# 
# 
# coeff6 <- 10000
# g7 <- ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD, color = "Landings"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_recruitment*coeff6, color = "SDM output"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff6, name = "Index")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(d) Market squid (Recrutiment index)") +  
#   scale_color_manual(name = "Variable: ", 
#                      values = c("Landings" = "grey", "SDM output" = "blue"))
# 
# 
# 

# ###################################################################################################
# 
# coeff3_2 <- 10000
# g3_2 <-  ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90_v2*coeff3_2), 
#             size = 0.5, linetype = "dashed", color = "blue") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = element_blank(), labels = NULL)  +
#   scale_y_continuous(name = "Landings by vessel", sec.axis = sec_axis(~./coeff3_2, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="bottom") + 
#   ggtitle("(a) Market squid (spawning aggregation model; 90 km radius; August - October)") 
# 

g4_2 <- ggplot(sdm.by.species.MSQD) +
  geom_line(mapping = aes(x = Date, y = RATIO, color = "Landings by active vessel"), size = 0.5) +
  geom_line(mapping = aes(x = Date, y = MSQD_SPAWN_SDM_90*coeff4_2, color = "SDM output"),
            size = 0.5, linetype = "dashed") +
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "Landings by vessel", sec.axis = sec_axis(~./coeff4_2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="none") +
  ggtitle("(a) Market squid (spawning aggregation model; 90 km radius)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Landings by active vessel" = "grey", "SDM output" = "blue"))


coeff4_2 <- 3000
coeff1_2 <- 150
coeff2_2 <- 150

g4_2 / g1_2 / g2_2


# g3_2 / g4_2
# 
# coeff5_2 <- 3000
# g5_2 <- ggplot(sdm.by.species.MSQD) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO, color = "Landings by vessel"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90*coeff5_2, color = "SDM output"),
#             size = 0.5, linetype = "dashed") +
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) +
#   scale_x_continuous(name = "Year")  +
#   scale_y_continuous(name = "Landings by vessel", sec.axis = sec_axis(~./coeff5_2, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"),
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") +
#   ggtitle("(b) Market squid (90 km radius)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Landings by vessel" = "grey", "SDM output" = "blue"))
# 
# coeff6_2 <- 80
# g6_2 <-  ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90_JS_cpue*coeff6_2), 
#             size = 0.5, color = "blue", linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = element_blank(), labels = NULL) +
#   scale_y_continuous(name = "Landings by vessel", sec.axis = sec_axis(~./coeff6_2, name = "Abundance (CPUE)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(c) Market squid (Justin Suca's abundance; August - October; 90 km radius)")
# 
# 
# coeff7_2 <- 100
# g7_2 <- ggplot(sdm.by.species.MSQD) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO, color = "Landings by vessel"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_recruitment*coeff7_2, color = "SDM output"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Landings by vessel", sec.axis = sec_axis(~./coeff7_2, name = "Index")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(d) Market squid (Recrutiment index)") +  
#   scale_color_manual(name = "Variable: ", 
#                      values = c("Landings by vessel" = "grey", "SDM output" = "blue"))
# g6_2 / g7_2




# rm(g1, g2, g3, g4, g5, g6, sdm.by.species, coeff1, coeff2, coeff3, coeff4, coeff5, coeff6 , area_names, landing.price.year.sel)


#### Se first how many trips per day
# n_trips_per_day <- Tickets_clust_2 %>% 
#   dplyr::select('VESSEL_NUM', 'FTID', 'LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY') %>% 
#   unique() %>% 
#   group_by(VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   summarize(n_trips = n()) 
# 
# hist(n_trips_per_day$n_trips, 
#      main = '', 
#      xlab	= 'Number of trips per day')

### How many tickets per species?
# freq_dominant_species <- count(Tickets, 'Species_Dominant')
# gs4_create("freq_dominant_species_participation", sheets = freq_dominant_species)




