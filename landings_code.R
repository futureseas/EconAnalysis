
###########################
### Landings paper code ###
###########################

rm(list = ls(all.names = TRUE)) 
gc()
# load("stan_fit.RData")

### Load packages
library(bookdown)
library(brms)
library(cluster)
library(cmdstanr)
library(data.table)
library(distances)
library(doBy)
library(dplyr)
library(fastDummies)
library(forcats)
library(ggplot2)
library(here)
library(hrbrthemes)
library(kableExtra)
library(lmtest)
library(lubridate)
library(magrittr)
library(Rcpp)
library(patchwork)
library(plm)
library(papaja)
library(reshape)
library(reshape2)
library(rstan)
library(scales)
library(sjlabelled)
library(texreg)
library(tidyr)
library(tidyverse)
library(tinytex)
library(viridis)
library(xtable)
library(zoo)

### Load functions
meanfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}
sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}
sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) #, l=length(x))
}

#---------------------------
### Read monthly data created in "~\data_processing_vessels.R" ###
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")
PacFIN.month.CPS.expand <- PacFIN.month %>% 
  dplyr::filter(PACFIN_SPECIES_CODE %in% 
                  c("CMCK", "JMCK", "UMCK", "PBNT", "RHRG", "MSQD", "NANC", "PSDN"))

PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OCPS")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OCPS")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OCPS")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "PBNT"] <- "OCPS")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "RHRG"] <- "OCPS")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OCPS",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))

### Include port area code (using PacFIN Port Code)
PacFIN.month <- PacFIN.month %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
PacFIN.month.CPS <- PacFIN.month %>% 
  dplyr::filter(PACFIN_SPECIES_CODE %in% 
                  c("OCPS", "MSQD", "NANC", "PSDN"))

rm(port_area)


#----------------------------------
### Descriptive figures ###

## Figure 1. Fishing seasons ##

port_names <- as_labeller(c("SP" = "San Pedro", "HNM" = "Hueneme", "MNT" = "Monterrey", "MOS" = "Moss Landing", 
                            "LWC" = "Ilwaco / Chinook", "AST" = "Astoria", 
                            "PRN" = "Princeton/ Half Moon\nBay", "SF" = "San Francisco", 
                            "SLT" = "Sausalito", "TRM" = "Terminal Island",
                            "VEN" = "Ventura", "WPT" = "Westport"))

q.psdn.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%  
  group_by(LANDING_YEAR, LANDING_MONTH, PACFIN_PORT_CODE) %>% 
  summarise(Landings_PSDN = sum(LANDED_WEIGHT_MTONS.sum)) %>%  
  group_by(LANDING_MONTH, PACFIN_PORT_CODE) %>% 
  summarise(Landings_PSDN = mean(Landings_PSDN)) %>%
  filter(PACFIN_PORT_CODE == "AST" | PACFIN_PORT_CODE == "SP" |
           PACFIN_PORT_CODE == "TRM" | PACFIN_PORT_CODE == "MOS" | 
           PACFIN_PORT_CODE == "WPT") 

g1 <- ggplot(data=q.psdn.by.month, aes(x=LANDING_MONTH, y=Landings_PSDN)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) + 
  scale_x_continuous(name = "Month", breaks=1:12) +
  scale_y_continuous(name = "Landings (M tons)" ) + 
  facet_grid(~ factor(PACFIN_PORT_CODE, levels=c("SP", "TRM", "MOS", "AST", "WPT")), 
             labeller = port_names) + ggtitle("(a) Pacific sardine") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))

q.msqd.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%  
  group_by(LANDING_YEAR, LANDING_MONTH, PACFIN_PORT_CODE) %>% 
  summarise(Landings_PSDN = sum(LANDED_WEIGHT_MTONS.sum)) %>%  
  group_by(LANDING_MONTH, PACFIN_PORT_CODE) %>% 
  summarise(Landings_PSDN = mean(Landings_PSDN)) %>%
  filter(PACFIN_PORT_CODE == "SP" | PACFIN_PORT_CODE == "VEN" | PACFIN_PORT_CODE == "TRM" | 
           PACFIN_PORT_CODE == "MOS" | PACFIN_PORT_CODE == "MNT") 

g2 <- ggplot(data=q.msqd.by.month, aes(x=LANDING_MONTH, y=Landings_PSDN)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) + 
  scale_x_continuous(name = "Month", breaks=1:12) +
  scale_y_continuous(name = "Landings (M tons)" ) + 
  facet_grid(~factor(PACFIN_PORT_CODE, levels = c("SP", "TRM", "VEN", "MNT", "MOS")), 
             labeller = port_names) + ggtitle("(b) Market squid") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))

g1 / g2

rm(g1, g2, q.psdn.by.month, q.msqd.by.month, port_names)



## Figure 2. Average annual landings, prices and revenues by species ##

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
        axis.title = element_text(size = 8)) + guides(fill=guide_legend(title="Period: ")) +
  xlab("") + ylab("Landings (tons)") +
  scale_x_discrete(labels=c("OCPS" = "Other\nCPEL\nSpecies", "OTHER" = "Other\nnon-CPEL\nSpecies",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine")) +  scale_fill_brewer(palette="Paired")

# Price
price.year <- summaryBy(AFI_PRICE_PER_MTON.mean ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=meanfun, da=PacFIN.month)
price.year <- price.year %>% filter(LANDING_YEAR <2015)
price.avg.year <- summaryBy(AFI_PRICE_PER_MTON.mean.mean ~ PACFIN_SPECIES_CODE, FUN=meanfun, da=price.year)

g2 <- ggplot(price.avg.year, aes(PACFIN_SPECIES_CODE, AFI_PRICE_PER_MTON.mean.mean.mean)) +
  geom_col(width=0.2) + ggtitle("(c) Average annual price") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  xlab("") + ylab("Price (USD/ton)") + 
  scale_x_discrete(labels=c("OCPS" = "Other\nCPEL\nSpecies", "OTHER" = "Other\nnon-CPEL\nSpecies",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine"))

# Revenue
revenue.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sumfun, data=PacFIN.month)
revenue.year <- revenue.year %>% filter(LANDING_YEAR <2015)
revenue.avg.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum.sum ~ PACFIN_SPECIES_CODE, FUN=meanfun, data=revenue.year)

g3 <- ggplot(revenue.avg.year, aes(PACFIN_SPECIES_CODE, AFI_EXVESSEL_REVENUE.sum.sum.mean)) +
  geom_col(width=0.4) + ggtitle("(b) Annual revenue") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  xlab("") + ylab("Annual revenues (Millions of USD)") +  scale_y_continuous(labels = label_number(scale = 1e-6)) +
  scale_x_discrete(labels=c("OCPS" = "Other\nCPEL\nSpecies", "OTHER" = "Other\nnon-CPEL\nSpecies",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine"))

# Number of vessels
revenue.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sumfun, data=PacFIN.month)
revenue.year <- revenue.year %>% filter(LANDING_YEAR <2015)
revenue.avg.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum.sum ~ PACFIN_SPECIES_CODE, FUN=meanfun, data=revenue.year)

g4 <- ggplot(revenue.avg.year, aes(PACFIN_SPECIES_CODE, AFI_EXVESSEL_REVENUE.sum.sum.mean)) +
  geom_col(width=0.4) + ggtitle("(b) Annual number of vessels") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  xlab("") + ylab("Annual revenues (Millions of USD)") +  scale_y_continuous(labels = label_number(scale = 1e-6)) +
  scale_x_discrete(labels=c("OCPS" = "Other\nCPEL\nSpecies", "OTHER" = "Other\nnon-CPEL\nSpecies",
                            "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy",
                            "PSDN" = "Pacific\nSardine"))

(g1 + g3) / (g2 + g4)

rm(g1, g2, g3, landings.year, landings.avg.year, revenue.year, revenue.avg.year, price.year, price.avg.year)



## Figure 3. Annual average landings by port area ##

# # Calculate number of vessels by species per month 
# nvessel.year <- PacFIN.month %>% 
#   dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% unique() %>% 
#   mutate(n_vessel = 1) %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
#   summarise(n_vessel = sum(n_vessel))
# 
# # Calculate average price and total landings by species per month
# landing.price.year <- summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~ 
#                                   PACFIN_SPECIES_CODE + LANDING_YEAR, 
#                                 FUN=sum_mean_fun, data=PacFIN.month) 

# Calculate landings by port and year
landings.by.port.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ 
                                     PACFIN_SPECIES_CODE + PACFIN_PORT_CODE + LANDING_YEAR + AGENCY_CODE, FUN=sumfun, data=PacFIN.month)
landings.by.port.year <- landings.by.port.year %>% filter(LANDING_YEAR <2015)
landings.by.port.avg.year <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + PACFIN_PORT_CODE 
                                       + AGENCY_CODE, FUN=meanfun, data=landings.by.port.year) %>%
  filter(PACFIN_PORT_CODE == "AST" | PACFIN_PORT_CODE == "HNM" | PACFIN_PORT_CODE == "SP" |
           PACFIN_PORT_CODE == "VEN" | PACFIN_PORT_CODE == "TRM" | PACFIN_PORT_CODE == "MOS" |
           PACFIN_PORT_CODE == "WPT" | PACFIN_PORT_CODE == "MNT" | PACFIN_PORT_CODE == "LWC" | 
           PACFIN_PORT_CODE == "PRN" | PACFIN_PORT_CODE == "ERK" ) %>%
  mutate(PACFIN_PORT_CODE = fct_relevel(PACFIN_PORT_CODE, 
                                        "SP", "WLM", "TRM", "HNM", "VEN", "MNT", "MOS", "PRN", 
                                        "SF", "SLT", "ERK", "WIN", "NEW", "AST", "LWC", "WPT"))

# PACFIN_PORT_CODE == "PRN" | PACFIN_PORT_CODE == "WLM" | PACFIN_PORT_CODE == "WIN"

states_names <- as_labeller(c(`C` = "California", `O` = "Oregon",`W` = "Washington"))


ggplot(landings.by.port.avg.year, aes(fill=PACFIN_SPECIES_CODE, 
                                      y=LANDED_WEIGHT_MTONS.sum.sum.mean, x=PACFIN_PORT_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position="bottom") + 
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OCPS" = "Other CPEL",
                                            "OTHER" = "Other Non-CPEL")) +
  theme(axis.text.x = element_text(angle=90, hjust=0.95, vjust=0.45)) + xlab("Ports") + 
  ylab("Landings (tons)") + guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=c("SP" = "San\nPedro", "HNM" = "Hueneme", 
                            "MNT" = "Monterrey", "MOS" = "Moss\nLanding", 
                            "LWC" = "Ilwaco /\nChinook", "AST" = "Astoria", 
                            "PRN" = "Princeton/\nHalf Moon\nBay", "TRM" = "Terminal\nIsland",
                            "VEN" = "Ventura", "WIN" = "Winchester\nBay",
                            "WLM" = "Willmington", "WPT" = "Westport", "NEW" = "Newport", 
                            "ERK" = "Eureka")) +
  scale_color_brewer(palette="Set2")



## Figure 4. Evolution of annual total landings and average annual prices ##

landing.price.year.sel <- landing.price.year %>% 
  dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean",
                "PACFIN_SPECIES_CODE", "LANDING_YEAR")




# Graph landing v/s number of  vessels
coeff1 <- 200
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff1 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g1 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff1, name = "Price (USD/Ton)")) +
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Market Squid")  +  
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))

coeff2 <- 100
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "PSDN")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff2 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g2 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff2, name = "Price (USD/Ton)")) + 
  theme(legend.position="right") + scale_x_continuous(name = "Year") +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(a) Pacific Sardine")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

(g2 / g1)
rm(df, g1, g2, landing.price.year.sel)


# ## Figure XX. Yearly total landing, average price and total number of vessel by CPS species (1981 - 2020).
# 
# g1 <- ggplot(landing.price.year,
#              aes(x = LANDING_YEAR, y = LANDED_WEIGHT_MTONS.sum.sum, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Landings (MTons)") +
#   theme(legend.position = "right", plot.title = element_text(size=9, face="bold.italic"),
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(a) Landings") +   guides(colour=guide_legend(title="Species: ")) +
#   scale_color_brewer(palette="Set2", labels=c("MSQD" = "Market Squid",
#                                               "NANC" = "Northern Anchovy",
#                                               "PSDN" = "Pacific Herring",
#                                               "PSDN" = "Pacific Sardine"))
# 
# g2 <- ggplot(landing.price.year,
#              aes(x = LANDING_YEAR, y = AFI_PRICE_PER_KG.mean.mean, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Price ($/Kg)")  +
#   scale_color_brewer(palette="Set2") +  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic"),
#                                               axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(b) Prices")
#  
# g3 <- ggplot(nvessel.year, aes(x = LANDING_YEAR, y = n_vessel, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "# of vessels") +   theme(legend.position="none")  +
#   scale_color_brewer(palette="Set2") +  theme(plot.title = element_text(size=9, face="bold.italic"),
#                                               axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(c) Number of vessels")
# 
# (g2 + g1) / g3
# 
# rm(g1, g2, g3)


# ## Figure XX. Relationship between the probability of presence (from SDMs) and landings by CPS species in three main ports
# 
# sdm.by.species <- PacFIN.month.CPS %>%
#   dplyr::select(LANDING_YEAR, PSDN_SDM_60, MSQD_SDM_90, NANC_SDM_20, LANDED_WEIGHT_MTONS.sum, PACFIN_SPECIES_CODE) %>% 
#   filter(LANDING_YEAR >= 1998 & LANDING_YEAR <= 2019) %>%
#   group_by(LANDING_YEAR, PACFIN_SPECIES_CODE) %>% 
#   summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
#             NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
#             MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm = TRUE),
#             LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
#   spread(PACFIN_SPECIES_CODE, LANDED_WEIGHT_MTONS) %>% 
#   dplyr::rename(Landings_PSDN = PSDN) %>% dplyr::rename(Landings_MSQD = MSQD) %>% 
#   dplyr::rename(Landings_NANC = NANC) %>%
#   group_by(LANDING_YEAR) %>% 
#   summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
#             NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
#             MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm = TRUE),
#             Landings_PSDN = sum(Landings_PSDN, na.rm = TRUE), Landings_MSQD = sum(Landings_MSQD, na.rm = TRUE),
#             Landings_NANC = sum(Landings_NANC, na.rm = TRUE)) 
# # sdm.by.species$Date <- zoo::as.yearmon(paste(sdm.by.species$LANDING_YEAR, sdm.by.species$LANDING_MONTH, sep='-'))
# 
# # Plot
# coeff <- 200000
# g1 <-  ggplot(sdm.by.species) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_PSDN, color = "Landings"), size = 0.5) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = PSDN_SDM_60*coeff, color = "Probability of presence"), 
#             size = 0.5, linetype = "dashed") + 
#   scale_x_continuous(name = element_blank(), labels = NULL) +
#   scale_y_continuous(name = element_blank(), sec.axis = sec_axis(~./coeff, name = "")) +
#   theme(legend.position="right", plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(a) Pacific sardine") +  scale_color_manual(name = "Variable: ", 
#                                                        values = c("Landings" = "grey", "Probability of presence" = "blue"))
# 
# coeff2 <- 300000
# g2 <- ggplot(sdm.by.species) + 
#   geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD), size = 0.5, color = "grey") +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90*coeff2), 
#             size = 0.5, color = "blue", linetype = "dashed") + 
#   scale_x_continuous(name = element_blank(), labels = NULL) +
#   scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("(b) Market squid")
# 
# g1 / g2
# 
# rm(g1, g2, sdm.by.species, coeff, coeff2, coeff3)



## Figure 5. Landings v/s probability of presence by port area ##

sdm.by.species <- PacFIN.month.CPS %>%
  dplyr::select(LANDING_YEAR, PORT_NAME, PSDN_SDM_60, MSQD_SDM_90, MSQD_SPAWN_SDM_90, NANC_SDM_20, 
                LANDED_WEIGHT_MTONS.sum, PACFIN_SPECIES_CODE, PACFIN_PORT_CODE) %>% 
  filter(LANDING_YEAR >= 1998 & LANDING_YEAR <= 2019) %>%
  group_by(LANDING_YEAR, PORT_NAME, PACFIN_PORT_CODE, PACFIN_SPECIES_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
  spread(PACFIN_SPECIES_CODE, LANDED_WEIGHT_MTONS) %>% 
  dplyr::rename(Landings_PSDN = PSDN) %>% dplyr::rename(Landings_MSQD = MSQD) %>% 
  dplyr::rename(Landings_NANC = NANC) %>%
  filter(PACFIN_PORT_CODE == "SP" | PACFIN_PORT_CODE == "TRM" | PACFIN_PORT_CODE == "MOS") %>% 
  mutate(PACFIN_PORT_CODE = fct_relevel(PACFIN_PORT_CODE, "SP", "TRM", "MOS")) %>%
  group_by(LANDING_YEAR, PORT_NAME, PACFIN_PORT_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm = TRUE),
            Landings_PSDN = sum(Landings_PSDN, na.rm = TRUE), 
            Landings_MSQD = sum(Landings_MSQD, na.rm = TRUE),
            Landings_NANC = sum(Landings_NANC, na.rm = TRUE))

# Plot
coeff <- 60000
g1 <-  ggplot(sdm.by.species) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_PSDN), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = PSDN_SDM_60*coeff), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ PORT_NAME) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(a) Pacific sardine (60 km radius)")

coeff2 <- 300000
g2 <- ggplot(sdm.by.species) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD, color = "Landings"), size = 0.5) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90*coeff2, color = "Probability of presence"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ PORT_NAME) + scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8),
        legend.position="bottom") + 
  ggtitle("(b) Market squid (Spawning aggregation model; 90 km radius)") +  scale_color_manual(name = "Variable: ", 
                                                                                                      values = c("Landings" = "grey", "Probability of presence" = "blue"))

g1 / g2

rm(g1, g2, g3, sdm.by.species, coeff, coeff2, coeff3)

# ## Figure XX. SDM v/s LAnding in Monterrey ##
# 
# collapse <- PacFIN_yearly %>%
#   filter(Species_code == "NANC") %>%
#   dplyr::select(Landing_year, Port, NANC_SDM_20_sep_dec, Landings, Species_code) %>% 
#   filter(Port == "MNA") %>% mutate(Port = fct_relevel(Port, "MNA")) %>% 
#   spread(Species_code, Landings) %>% dplyr::rename(Landings_NANC = NANC) %>%
#   filter(Landing_year >= 2000 & Landing_year <= 2018)
# 
# # Plot
# coeff <- 50000
# g3 <- ggplot(collapse) + 
#   geom_line(mapping = aes(x = Landing_year, y = Landings_NANC, color = "Landings"),
#             size = 0.5) +
#   geom_line(mapping = aes(x = Landing_year, y = NANC_SDM_20_sep_dec*coeff, color = "Probability of presence"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ Port) + 
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = element_blank(), sec.axis = sec_axis(~./coeff, name = element_blank())) +
#   theme(legend.position="bottom", plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("Northern anchovy") +  scale_color_manual(name = "Variable: ", 
#                                                     values = c("Landings" = "grey", "Probability of presence" = "blue"))
# 
# g3
# 
# 
# collapse2 <- as.data.frame(diff(collapse$NANC_SDM_20_sep_dec))
# names(collapse2)[1] <- 'NANC_SDM_20_sep_dec'
# collapse2 <- collapse2 %>% mutate(Landings_NANC = diff(collapse$Landings_NANC)) %>% mutate(Port = "MNA") %>%
#   mutate(Landing_year = seq(2001,2018))
# 
# dg3 <- ggplot(collapse2) + 
#   geom_line(mapping = aes(x = Landing_year, y = Landings_NANC, color = "diff(Landings)"),
#             size = 0.5) +
#   geom_line(mapping = aes(x = Landing_year, y = NANC_SDM_20_sep_dec*coeff, color = "diff(Probability of presence)"), 
#             size = 0.5, linetype = "dashed") + 
#   facet_wrap(~ Port) + 
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = element_blank(), sec.axis = sec_axis(~./coeff, name = element_blank())) +
#   theme(legend.position="bottom", plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   ggtitle("Northern anchovy (first differences)") +  scale_color_manual(name = "Variable: ", 
#                                                                         values = c("diff(Landings)" = "grey", "diff(Probability of presence)" = "blue"))
# 
# dg3
# 
# 
# library("writexl")
# write_xlsx(collapse,"Data\\monterrey_NANC_sdm_landings.xlsx")



## Figure 6. Evolution of Pacific sardine landings and the number of vessels landing market squid

landing.nvessels.year.sel <- landing.price.year %>%
  inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
  dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")

# Graph landing v/s number of  vessels
coeff <- 1500
df <- landing.nvessels.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD" | PACFIN_SPECIES_CODE == "PSDN") %>%
  mutate(n_vessel = ifelse(PACFIN_SPECIES_CODE == "MSQD", n_vessel, 0)) %>%
  mutate(LANDED_WEIGHT_MTONS.sum.sum = ifelse(PACFIN_SPECIES_CODE == "PSDN",
                                              LANDED_WEIGHT_MTONS.sum.sum, 0)) %>%
  group_by(LANDING_YEAR) %>%
  summarise(n_vessel = sum(n_vessel), LANDED_WEIGHT_MTONS.sum.sum = sum(LANDED_WEIGHT_MTONS.sum.sum))

df$n_vessel <- df$n_vessel * coeff
df <- gather(df, key = Variable, value = value,
             c("n_vessel", "LANDED_WEIGHT_MTONS.sum.sum"))
ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  scale_y_continuous(name = "Landings of Pacific sardine (M Tons)", sec.axis = sec_axis(~./coeff, name = "Number of vessels landing market squid")) +
  theme(legend.position="bottom") + scale_x_continuous(name = element_blank()) +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  guides(colour=guide_legend(title="Variables: ")) + scale_color_brewer(palette="Set2",
                                                                        limits = c("LANDED_WEIGHT_MTONS.sum.sum", "n_vessel"),
                                                                        labels=c("LANDED_WEIGHT_MTONS.sum.sum" = "Landings of Pacific sardine", 
                                                                                 "n_vessel" = "Number of vessels landing market squid")) 

rm(df, landing.price.year, nvessel.year, coeff, coeff1)


# ## Figure XXX. Landings of squid and sardine by ports 
# 
# landings.by.sel.ports <- landings.by.port.year %>% 
#   filter(PACFIN_PORT_CODE == "SP" | PACFIN_PORT_CODE == "TRM" | 
#            PACFIN_PORT_CODE == "MOS") %>%
#   filter(PACFIN_SPECIES_CODE == "PSDN" |  PACFIN_SPECIES_CODE == "MSQD") %>%
#   mutate(PACFIN_PORT_CODE = fct_relevel(PACFIN_PORT_CODE, "SP", "TRM", "HNM", "MNT", "MOS")) %>%
#   filter(LANDING_YEAR >= 2000)
# 
# port_names <- as_labeller(c(`SP` = "San Pedro", `TRM` = "Terminal Island", 
#                             `MOS` = "Moss Landing", `MNT` = "Monterrey", `HNM` = "Hueneme"))
# 
# 
# ggplot(landings.by.sel.ports, aes(y=LANDED_WEIGHT_MTONS.sum.sum, 
#                                   x=LANDING_YEAR, color=PACFIN_SPECIES_CODE, group=PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + xlab("Year") + ylab("Landings (tons)") + 
#   facet_wrap(~ PACFIN_PORT_CODE, labeller = port_names, nrow = 3, ncol = 1) +
#   theme(legend.position="right") + 
#   theme(axis.text.x = element_text(angle=90)) +
#   guides(color=guide_legend(title="Species: ")) +
#   scale_color_brewer(palette="Set2", labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
#                                               "PHRG" = "Pacific Herring", "PSDN" = "Pacific Sardine"))
# 
# rm(port_names, landings.by.sel.ports, landings.by.port.year)





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


## Switching observed

```{r switching_day_landings, eval=FALSE, include=FALSE}
# PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
#   PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
#
#   # Merge different decades of data #
# PacFIN <- rbind.data.frame(PacFIN_2000_2009, PacFIN_2010_2020)
#   rm(PacFIN_2000_2009, PacFIN_2010_2020)
#   gc()
#
# psdn.landings.by.day.vessel <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, LANDED_WEIGHT_MTONS) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   summarise(Landed_PSDN = sum(LANDED_WEIGHT_MTONS)) %>% mutate(harv.psdn = 1)
#
# # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# squid.cond.psdn <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, LANDED_WEIGHT_MTONS) %>%
#   merge(psdn.landings.by.day.vessel, by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
#   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>%
#   mutate(perc_land_squid = LANDED_WEIGHT_MTONS / (LANDED_WEIGHT_MTONS + Landed_PSDN))
#
# write.csv(squid.cond.psdn,"C:\\Data\\day_switch_land.csv", row.names = FALSE)
df <- read.csv(file = "C:\\Data\\day_switch_land.csv")
hist(df$perc_land_squid, col = 'skyblue3', breaks = 10)
plot.ecdf(df$perc_land_squid)

```

```{r switching_day, eval=FALSE, include=FALSE}
# PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
#   PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
#
#   # Merge different decades of data #
# PacFIN <- rbind.data.frame(PacFIN_2000_2009, PacFIN_2010_2020)
#   rm(PacFIN_2000_2009, PacFIN_2010_2020)
#   gc()
#
# psdn.by.day <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   unique() %>% mutate(harv.psdn = 1)
#
# # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# squid.cond.psdn <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   merge(psdn.by.day, by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
#   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()
#
# # Calculate number of vessels harvesting sardine and squid and percentage per year
# n_vessel_psdn <- psdn.by.day %>% group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   summarize(n_vessel_psdn = sum(harv.psdn, na.rm = TRUE))
# # %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
# #   summarize(n_vessel_psdn = mean(n_vessel_psdn, na.rm = TRUE))
#
# n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
#   summarize(n_vessel_msqd = sum(harv.psdn, na.rm = TRUE))
# # %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
# #   summarize(n_vessel_msqd = mean(n_vessel_msqd, na.rm = TRUE))
#
# df <- merge(n_vessel_psdn, n_vessel_msqd_c_psdn, by=c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
#     mutate(n_vessel_msqd = if_else(is.na(n_vessel_msqd), 0, n_vessel_msqd)) %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
#     summarize(n_vessel_msqd = sum(n_vessel_msqd, na.rm = TRUE), n_vessel_psdn = sum(n_vessel_psdn, na.rm = TRUE)) %>%
#     mutate(perc = (n_vessel_msqd / n_vessel_psdn)) %>% select(LANDING_MONTH, LANDING_DAY, perc)
#
# df$MonthDay <- paste( month.abb[df$LANDING_MONTH], df$LANDING_DAY, sep="-" )
#
# write.csv(df,"C:\\Data\\day_switch.csv", row.names = FALSE)
df <- read.csv(file = "C:\\Data\\day_switch.csv")
ggplot(data=df, aes(x=MonthDay, y=perc)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
  scale_x_discrete(name = "Day", breaks=1:366) +
  scale_y_percent(name = "Percentage of vessels harvesting squid" )
rm(df)
```

```{r switching_graph, eval=FALSE, include=FALSE}
psdn.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
  unique() %>% mutate(harv.psdn = 1)
msqd.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
  unique() %>% mutate(harv.msqd = 1)

# If a vessel harvest Pacific sardine in a year, how many also harvest squid.
squid.cond.psdn <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% merge(psdn.by.year,
                                                                  by = c("VESSEL_NUM", "LANDING_YEAR"), all.y = TRUE) %>%
  filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()

# Calculate number of vessels harvesting sardine and squid and percentage per year
n_vessel_psdn <- psdn.by.year %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_psdn = sum(harv.psdn))
n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_msqd = sum(harv.psdn))
df <- n_vessel_msqd_c_psdn %>% mutate(perc = n_vessel_psdn$n_vessel_psdn) %>%
  mutate(n_vessel_only_psdn = perc) %>%  # - n_vessel_msqd
  mutate(perc = (n_vessel_msqd / perc))

coeff = 115
g1 <- ggplot(df) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_only_psdn,
                          color = "# of vessels landing sardine"), size = 1) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_msqd,
                          color = "# of vessels landing squid (c/ landing sardine)"), size = 1) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = perc*coeff,
                          color = "% of vessels landing squid (c/ landings sardine)"),
            size = 1, linetype = "dashed") + scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Number of vessels",
                     sec.axis = sec_axis(~./coeff, name = "% vessels harvesting squid" )) +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +  scale_color_manual(name = "Variables: ",
                                                                                                       values = c("# of vessels landing sardine" = "green",
                                                                                                                  "# of vessels landing squid (c/ landing sardine)" = "blue",
                                                                                                                  "% of vessels landing squid (c/ landings sardine)" = "grey")) +
  theme(legend.position="right")

rm(psdn.by.year, squid.cond.psdn, n_vessel_psdn, n_vessel_msqd_c_psdn, df, coeff)

msqd.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2020) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
  unique() %>% mutate(harv.msqd = 1)

# If a vessel harvest Pacific sardine in a year, how many also harvest squid.
psdn.cond.msqd <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2020) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% merge(msqd.by.year,
                                                                  by = c("VESSEL_NUM", "LANDING_YEAR"), all.y = TRUE) %>%
  filter(harv.msqd == 1) %>% filter(PACFIN_SPECIES_CODE.x == "PSDN") %>% unique()

# Calculate number of vessels harvesting sardine and squid and percentage per year
n_vessel_msqd <- msqd.by.year %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_msqd = sum(harv.msqd))
n_vessel_psdn_c_msqd <- psdn.cond.msqd %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_psdn = sum(harv.msqd))
df <- n_vessel_psdn_c_msqd %>%  mutate(n_vessel_msqd = n_vessel_msqd$n_vessel_msqd) %>%
  mutate(perc = n_vessel_psdn / n_vessel_msqd)

coeff = 200
g2 <- ggplot(df) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_msqd,
                          color = "# of vessels landing squid"), size = 1) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_psdn,
                          color = "# of vessels landing sardine (c/ landing squid)"), size = 1) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = perc*coeff,
                          color = "% of vessels landing sardine (c/ landings squid)"),
            size = 1, linetype = "dashed") + scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Number of vessels",
                     sec.axis = sec_axis(~./coeff, name = "% vessels harvesting squid" )) +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +  scale_color_manual(name = "Variables: ",
                                                                                                       values = c("# of vessels landing squid" = "green",
                                                                                                                  "# of vessels landing sardine (c/ landing squid)" = "blue",
                                                                                                                  "% of vessels landing sardine (c/ landings squid)" = "grey")) +
  theme(legend.position="right")

g1 / g2

rm(msqd.by.year, psdn.cond.msqd, n_vessel_msqd, n_vessel_psdn_c_msqd, df, coeff, g1, g2)

```

```{r switching_month, eval=FALSE, include=FALSE}
psdn.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH) %>% filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
  unique() %>% mutate(harv.psdn = 1)

# If a vessel harvest Pacific sardine in a year, how many also harvest squid.
squid.cond.psdn <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
  select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH) %>% merge(psdn.by.month,
                                                                                 by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH"), all.y = TRUE) %>%
  filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()

# Calculate number of vessels harvesting sardine and squid and percentage per year
n_vessel_psdn <- psdn.by.month %>% group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarize(n_vessel_psdn = sum(harv.psdn, na.rm = TRUE))

n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarize(n_vessel_msqd = sum(harv.psdn, na.rm = TRUE))

df <- merge(n_vessel_psdn, n_vessel_msqd_c_psdn, by=c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>%
  mutate(n_vessel_msqd = if_else(is.na(n_vessel_msqd), 0, n_vessel_msqd)) %>% group_by(LANDING_MONTH) %>%
  summarize(n_vessel_msqd = sum(n_vessel_msqd, na.rm = TRUE), n_vessel_psdn = sum(n_vessel_psdn, na.rm = TRUE)) %>%
  mutate(perc = (n_vessel_msqd / n_vessel_psdn)) %>% select(LANDING_MONTH, perc)

ggplot(data=df, aes(x=LANDING_MONTH, y=perc)) +
  geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
  scale_x_continuous(name = "Month", breaks=1:12) +
  scale_y_percent(name = "Percentage of vessels harvesting squid" )

rm(psdn.by.month, squid.cond.psdn, n_vessel_psdn, n_vessel_msqd_c_psdn, df)


```



# Methods

```{r dataset, include=FALSE}
# Organize dataset
PacFIN.month.dataset = PacFIN.month.cluster[,!grepl("*_LE_",names(PacFIN.month.cluster))]
PacFIN.month.dataset <- PacFIN.month.dataset %>% filter(LANDING_YEAR >= 2000) %>% # filter(PACFIN_SPECIES_CODE == "PSDN" | PACFIN_SPECIES_CODE == "MSQD") %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, PORT_NAME, VESSEL_NUM, PSDN_SDM_60, MSQD_SDM_90,
                MSQD_SPAWN_SDM_90, LANDED_WEIGHT_MTONS.sum, AFI_PRICE_PER_KG.mean, 
                PACFIN_SPECIES_CODE, PACFIN_PORT_CODE, AGENCY_CODE, group) %>% 
  mutate(AFI_PRICE_PER_KG.mean = na_if(AFI_PRICE_PER_KG.mean, 0)) %>% 
  filter(group != is.na(group)) %>% 
  melt(id.vars=c("LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM",  "PORT_NAME", 
                 "PACFIN_PORT_CODE", "PACFIN_SPECIES_CODE", "AGENCY_CODE", "group")) %>% 
  dcast(LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_NAME + PACFIN_PORT_CODE + AGENCY_CODE + group ~
          PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 'PORT_NAME', 'PACFIN_PORT_CODE', 
                'AGENCY_CODE', 'group', 'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 
                'PSDN_AFI_PRICE_PER_KG.mean', 'MSQD_AFI_PRICE_PER_KG.mean')

# Add port area code #  
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
PacFIN.month.dataset <- PacFIN.month.dataset %>% 
  merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)

# Exclude port areas without landings of MSQD or PSDN #
selected.ports <- PacFIN.month.dataset %>% 
  group_by(PORT_AREA_CODE) %>% 
  summarise(landings_PSDN = sum(PSDN_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
            landings_MSQD = sum(MSQD_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
  rowwise() %>% mutate(total = min(landings_PSDN, landings_MSQD)) %>%
  filter(total > 0) %>% select(PORT_AREA_CODE) %>% mutate(port_included = 1)
PacFIN.month.dataset <- PacFIN.month.dataset %>% 
  merge(selected.ports, by = c("PORT_AREA_CODE"), all.x = TRUE) %>%
  filter(port_included == 1) 

# Create ID data and change NaN to NA #

PacFIN.month.dataset <- PacFIN.month.dataset %>%
  mutate(PORT_ID = as.numeric(as.factor(PORT_NAME))) %>%
  mutate(PORT_CODE_ID = as.numeric(as.factor(PACFIN_PORT_CODE))) %>% 
  mutate(PORT_AREA_ID = as.numeric(as.factor(PORT_AREA_CODE))) 
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA

# Merge SDM and ACL by year/month/port

## Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_PSDN, 
                              by = c("PORT_NAME", "AGENCY_CODE","LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

## Merge data with SDM Market Squid
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD, 
                              by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

## Merge data with SDM Market Squid 
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                              by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)  

dataset <- PacFIN.month.dataset %>%
  dplyr::rename(PSDN_SDM_60 = SDM_60) %>% dplyr::rename(MSQD_SDM_90 = SDM_90) %>%
  dplyr::rename(MSQD_SPAWN_SDM_90 = SDM_SPAWN_90)


rm(PacFIN.month.dataset, port_area, selected.ports, SDM_port_PSDN, SDM_port_MSQD, SDM_port_MSQD_Spawn)

```

```{r calculate-sdm, include=FALSE}
# Calculate year SDM for N/A's using PacFIN port code and port area code.

## Pacific sardine

### (a) Using PacFIN port code
SDM.port.code.PSDN <- aggregate(x=dataset$PSDN_SDM_60, by = list(dataset$LANDING_YEAR, 
                                                                 dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.PSDN <- SDM.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.code = x)
SDM.port.code.PSDN[SDM.port.code.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.PSDN, 
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.code, PSDN_SDM_60))
rm(SDM.port.code.PSDN)

### (b) Using port area code
SDM.port.area.PSDN <- aggregate(x=dataset$PSDN_SDM_60, by = list(dataset$LANDING_YEAR,
                                                                 dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.PSDN <- SDM.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.area = x)
SDM.port.area.PSDN[SDM.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.area, PSDN_SDM_60))
rm(SDM.port.area.PSDN)

dataset = subset(dataset, select = -c(PSDN.SDM.port.code, PSDN.SDM.port.area))

## Market squid 

### (a) Using PacFIN port code
SDM.port.code.MSQD <- aggregate(x=dataset$MSQD_SDM_90, by = list(dataset$LANDING_YEAR, 
                                                                 dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.MSQD <- SDM.port.code.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD.SDM.port.code = x)
SDM.port.code.MSQD[SDM.port.code.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.MSQD, 
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_SDM_90 = ifelse(is.na(MSQD_SDM_90), MSQD.SDM.port.code, MSQD_SDM_90))
rm(SDM.port.code.MSQD)

### (b) Using port area code
SDM.port.area.MSQD <- aggregate(x=dataset$MSQD_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                 dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD <- SDM.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.SDM.port.area = x)
SDM.port.area.MSQD[SDM.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SDM_90 = ifelse(is.na(MSQD_SDM_90), MSQD.SDM.port.area, MSQD_SDM_90))
rm(SDM.port.area.MSQD)

dataset = subset(dataset, select = -c(MSQD.SDM.port.code, MSQD.SDM.port.area))


# Market squid (SPAWN)
## (a) Using PacFIN port code
SDM.port.code.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                             dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.MSQD_SPAWN <- SDM.port.code.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.code = x)
SDM.port.code.MSQD_SPAWN[SDM.port.code.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.MSQD_SPAWN,
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.code, MSQD_SPAWN_SDM_90))
rm(SDM.port.code.MSQD_SPAWN)
### (b) Using port area code
SDM.port.area.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                             dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD_SPAWN <- SDM.port.area.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.area = x)
SDM.port.area.MSQD_SPAWN[SDM.port.area.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD_SPAWN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.area, MSQD_SPAWN_SDM_90))
rm(SDM.port.area.MSQD_SPAWN)
dataset = subset(dataset, select = -c(MSQD_SPAWN.SDM.port.code, MSQD_SPAWN.SDM.port.area))

```

```{r calculate-price, include=FALSE}

# Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

## Pacific sardine

### (a) Using port name
price.port.name.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
price.port.name.PSDN <- price.port.name.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
  dplyr::rename(PSDN.price.port.name = x)
price.port.name.PSDN[price.port.name.PSDN == "NaN"] <- NA
dataset <- dataset %>% 
  merge(price.port.name.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.name, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.name.PSDN)

### (b) Using PacFIN port code
price.port.code.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
price.port.code.PSDN <- price.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.code = x)
price.port.code.PSDN[price.port.code.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.code.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.code, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.code.PSDN)

### (c) Using port area code
price.port.area.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.PSDN <- price.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.area = x)
price.port.area.PSDN[price.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.area, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.area.PSDN)

### (d) Using state
price.state.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.PSDN <- price.state.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.state = x)
price.state.PSDN[price.state.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.state, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.state.PSDN)

### (e) Using year/month
price.year.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.PSDN <- price.year.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PSDN.price.year.month = x)
price.year.PSDN[price.year.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.year.month, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.year.PSDN)

dataset = subset(dataset, select = 
                   -c(PSDN.price.port.name, PSDN.price.port.code, 
                      PSDN.price.port.area, PSDN.price.state, PSDN.price.year.month))


## Market squid

### (a) Using port name
price.port.name.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
price.port.name.MSQD <- price.port.name.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
  dplyr::rename(MSQD.price.port.name = x)
price.port.name.MSQD[price.port.name.MSQD == "NaN"] <- NA
dataset <- dataset %>% 
  merge(price.port.name.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.name, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.name.MSQD)

### (b) Using PacFIN port code
price.port.code.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
price.port.code.MSQD <- price.port.code.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.code = x)
price.port.code.MSQD[price.port.code.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.code.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.code, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.code.MSQD)

### (c) Using port area code
price.port.area.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.MSQD <- price.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.area = x)
price.port.area.MSQD[price.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.area, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.area.MSQD)

### (d) Using state
price.state.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.MSQD <- price.state.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.state = x)
price.state.MSQD[price.state.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.state, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.state.MSQD)

### (e) Using year/month
price.year.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.MSQD <- price.year.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(MSQD.price.year.month = x)
price.year.MSQD[price.year.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.year.month, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.year.MSQD)

dataset = subset(dataset, select = 
                   -c(MSQD.price.port.name, MSQD.price.port.code, 
                      MSQD.price.port.area, MSQD.price.state, MSQD.price.year.month))

```

```{r desc, echo=FALSE, message=FALSE, warning=FALSE, results='asis'}
# Rename Price and landing variables #
dataset <- dataset %>% 
  dplyr::rename(PSDN_Price = PSDN_AFI_PRICE_PER_KG.mean) %>%
  dplyr::rename(MSQD_Price = MSQD_AFI_PRICE_PER_KG.mean) %>%
  dplyr::rename(PSDN_Landings = PSDN_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(MSQD_Landings = MSQD_LANDED_WEIGHT_MTONS.sum)

# Label dataset
sjlabelled::set_label(dataset$MSQD_SDM_90)         <- "Prob(presence): MSQD"
sjlabelled::set_label(dataset$MSQD_SPAWN_SDM_90)   <- "Prob(presence): MSQD (Spawn)"
sjlabelled::set_label(dataset$PSDN_SDM_60)         <- "Prob(presence): PSDN"
sjlabelled::set_label(dataset$LANDING_YEAR)     <- "Year"
sjlabelled::set_label(dataset$LANDING_MONTH)    <- "Month"
sjlabelled::set_label(dataset$PACFIN_PORT_CODE) <- "Port code"
sjlabelled::set_label(dataset$PORT_AREA_CODE)   <- "Port area code"
sjlabelled::set_label(dataset$PORT_NAME)        <- "Port name"
sjlabelled::set_label(dataset$AGENCY_CODE)      <- "State"
sjlabelled::set_label(dataset$VESSEL_NUM)       <- "Vessel ID"
sjlabelled::set_label(dataset$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(dataset$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(dataset$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(dataset$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(dataset$PORT_ID)       <- "Port ID"


### Convert to quarterly??? ###
library("zoo")
dataset$MonthYear <- as.yearmon(paste(dataset$LANDING_YEAR, dataset$LANDING_MONTH), "%Y %m")
dataset$QuarterYear <- as.yearqtr(dataset$MonthYear, format = "%Y-%m")

dataset <- dataset %>% 
  group_by(LANDING_YEAR, VESSEL_NUM, group, PORT_AREA_ID) %>%
  summarise(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm=T), MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm=T),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=T), PSDN_Landings = sum(PSDN_Landings, na.rm=T), 
            MSQD_Landings = sum(MSQD_Landings, na.rm=T), PSDN_Price = mean(PSDN_Price, na.rm=T), 
            MSQD_Price = mean(MSQD_Price, na.rm=T))
dataset[dataset == "NaN"] <- NA

desc_data <- dataset %>%
  subset(select = -c(PORT_AREA_ID, VESSEL_NUM, group))

sjlabelled::set_label(desc_data$MSQD_SDM_90)       <- "Prob(presence): MSQD"
sjlabelled::set_label(desc_data$MSQD_SPAWN_SDM_90) <- "Prob(presence): MSQD (Spawn)"
sjlabelled::set_label(desc_data$PSDN_SDM_60)   <- "Prob(presence): PSDN"
sjlabelled::set_label(desc_data$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(desc_data$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(desc_data$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(desc_data$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(desc_data$ACL_PSDN)      <- "Annual Catch Limit: PSDN"


# Compute descriptive statistics #

desc_data <- desc_data[c("PSDN_SDM_60", "MSQD_SDM_90", "MSQD_SPAWN_SDM_90", "PSDN_Landings", "MSQD_Landings",
                         "PSDN_Price", "MSQD_Price")]

vtable::st(desc_data, labels = TRUE, title='Summary Statistics \\label{tab:sum_stats}', out='latex')
rm(desc_data, Tot.landings.ports)

```

<!-- ############################################### -->
  <!-- ### CREATE DATASET BY SPECIES AND RUN MODEL ### -->
  <!-- ############################################### -->
  
  <!-- ### Pacific Sardine ### -->
  
  ```{r dataset_psdn, message=FALSE, warning=FALSE, include=FALSE, results='asis'}

## Select data for estimation, replace N/A landings to zero #

dataset_psdn <- dataset %>% 
  dplyr::select(VESSEL_NUM, PSDN_SDM_60, PSDN_Landings, PSDN_Price, MSQD_Price,
                PORT_AREA_ID, LANDING_YEAR, MSQD_SDM_90, MSQD_SPAWN_SDM_90, group) %>%
  filter(LANDING_YEAR >= 2000) %>% 
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% #filter(PSDN_Landings > 0) %>% 
  mutate(PSDN_Landings = ifelse(PSDN_Landings<=0, 0 ,PSDN_Landings)) %>% 
  mutate(Closure = ifelse(LANDING_YEAR >= 2015,1,0)) %>%
  mutate(RelPrice = PSDN_Price / MSQD_Price) %>% drop_na() 
dataset_psdn$port_ID <- as.factor(
  udpipe::unique_identifier(dataset_psdn, fields = "PORT_AREA_ID", start_from = 1))
dataset_psdn$cluster <- as.factor(
  udpipe::unique_identifier(dataset_psdn, fields = "group", start_from = 1))
# dataset_psdn_port_names <- dataset_psdn %>% dplyr::select(PORT_AREA_CODE, port_ID) %>% unique()

```

```{r psdn_estimation, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
fit_qPSDN <- brm(bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SPAWN_SDM_90 + (1 | cluster) + (1 | port_ID), 
                    hu ~ (1 | cluster) + (1 | port_ID)),
                 data = dataset_psdn, 
                 prior = c(set_prior("cauchy(0,2)", class = "sd")),
                 family = hurdle_gamma(), 
                 chains = 4, cores = 4, warmup = "1000", iter = "2000",
                 control = list(adapt_delta = 0.95))

plot(fit_qPSDN)
plot(fit_qPSDN, pars = c("PSDN_SDM_60"))
coef(fit_qPSDN)

save.image (file = "stan_fit.RData")

# # work better with SDM separated. 
# loo(fit_qPSDN, fit_qPSDNv2)

```

```{r pp_check_psdn, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 <- pp_check(fit_qPSDN) + ggtitle('(a) Pacific sardine') + 
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) +  
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic")) +  
  xlim(0.1, 4000) + ylab("Density")

g1
```

<!-- # print summary -->
  <!-- texreg(list(f2, f3, r2, r3), caption = 'Panel data models for Pacific Sardine landings.\\label{table:sardine_est}', caption.above = TRUE, float.pos = "h", custom.model.names = c("FE: Model 1", "FE: Model 2", "RE: Model 1", "RE: Model 2")) -->
  
  
  <!-- # ##  Extract Grouo-Level estimates ## -->
  <!-- # ranef(fit_qPSDN_gamma) -->
  <!-- #  -->
  <!-- # ## Check divergence and other model check ## -->
  <!-- # shinystan::launch_shinystan(fit_qPSDN_gamma) -->
  
  <!-- # Investigate chain and posterior distributions.  -->
  <!-- # https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html -->
  <!-- # plot(fit_qPSDN_t2nc, pars = c("PSDN_SDM")) -->
  
  <!-- # ### Predictions ### -->
  <!-- # pred_data <- data.frame(PSDN_SDM = c(0.5, 0.25), MSQD_SDM = c(0.5), Port_ID = 1) -->
  <!-- # predict(fit_qPSDN, newdata = pred_data, re_formula = NA) -->
  
  <!-- # ## Compare models ## -->
  <!-- # loo(fit1, fit2) -->
  
  
  <!-- ### Market squid ### -->
  
  ```{r msqd_data, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'}
# Select data for estimation, replace N/A landings to zero #
est_data_msqd <- est_data %>%
  dplyr::select(port_num, Port, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_SDM_60, NANC_SDM_20, Landing_year) %>%
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>% 
  dplyr::mutate(dClose = ifelse(Landing_year >= 2015,1,0)) %>%
  dplyr::mutate(dOpen = ifelse(Landing_year < 2015,1,0)) %>%
  dplyr::mutate(PSDN_SDM_60_dOpen = PSDN_SDM_60 * dOpen) %>%
  filter(Landing_year >= 2000)

# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data_msqd$MSQD_Landings, est_data_msqd$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalMSQD = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalMSQD)

### NOTE: If I use drop_na, some ports without MSQD landings are excluded. ###
est_data_msqd <- est_data_msqd %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalMSQD==1) %>%
  drop_na() 

# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data_msqd$Landing_year))));
est_data_msqd$port_ID <- udpipe::unique_identifier(est_data_msqd, fields = "port_num", start_from = 1) 
portID_names_MSQD <- est_data_msqd %>%
  dplyr::select(Port, port_ID) %>%
  unique()

### Estimate using BRMS package ###
est_data_msqd$dClose    <- factor(est_data_msqd$dClose)
est_data_msqd$port_ID   <- factor(est_data_msqd$port_ID)
class(est_data_msqd$dClose)
class(est_data_msqd$port_ID)
class(est_data_msqd$dOpen)
```


```{r msqd_est_price, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
fit_qMSQD_price <- brm(bf(MSQD_Landings ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose 
                          + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose | port_ID),
                          hu ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose 
                          + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose | port_ID)),
                       data = est_data_msqd,
                       family = hurdle_gamma(),
                       control = list(adapt_delta = 0.999, max_treedepth = 20),
                       chains = 4, cores = 4)
save.image (file = "stan_fit.RData")
```

```{r pp_check_msqd, eval=FALSE, include=FALSE}
g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid') +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) + 
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic"))  + 
  xlim(0.1, 15000) + xlab("Landing (tons)")

```



# Results

## Landing model


### Graphical posterior predictive

```{r y_rep, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 

```

```{r shinystan, eval=FALSE, include=FALSE}
shinystan::launch_shinystan(fit_qPSDN_price) 
shinystan::launch_shinystan(fit_qMSQD_price) 
shinystan::launch_shinystan(fit_qNANC_price) 
```

```{r y_rep_zero, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 <- pp_check(fit_qPSDN_price) + ggtitle('(a) Pacific sardine')  + theme(legend.position = "none") + xlim(0, 0.1) 
g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid')     + theme(legend.position = "none") + xlim(0, 0.1) 
g3 <- pp_check(fit_qNANC_price) + ggtitle('(c) Northern anchovy') + theme(legend.position = "right", 
                                                                          plot.title = element_text(size=9, face="bold.italic")) + xlim(0, 0.1) 



g1 + g2 + g3
```

```{r model-comparision, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'}
fit_qMSQD <- add_criterion(fit_qMSQD, "loo")
fit_qMSQD_90 <- add_criterion(fit_qMSQD_90, "loo")

comp <- loo_compare(loo(fit_qMSQD), loo(fit_qMSQD_90)) %>%
  as.data.frame() %>%
  select(elpd_loo, elpd_diff, se_diff, p_loo, looic) %>%
  dplyr::rename(
    "ELPD-Diff" = elpd_diff, "SE-Diff" = se_diff, 
    "ELPD-LOO" = elpd_loo, "P-LOO" = p_loo, "LOOIC" = looic
  )

mw1 <- model_weights(fit_qMSQD, fit_qMSQD_90, weights = "loo")[rownames(comp)]

comp %>%
  cbind("Akaike-Weight" = mw1) %>%
  apa_table(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Comparison of models fit1 to fit3 based on approximate leave-one-out cross-validation. Market squid landigns model.",
    note =  "ELPD-LOO = expected log posterior predictive density (higher is better); ELPD-DIFF = difference in ELPD values compared to the best model. SE-DIFF = standard error of the ELPD difference. P-LOO = effective number of model parameters (lower is better); LOOIC: leave-one-out information criterion (lower is better); Akaike-Weight = Model weight based on the LOOIC values (higher is better).",
    align = c("l", rep("r", 6))
  )
```


### Own species distribution effect

```{r by_port_sdm, eval=FALSE, fig.cap=, include=FALSE}
# PSDN plots
conditions <- data.frame(port_ID = unique(est_data_psdn$port_ID))
rownames(conditions) <- unique(est_data_psdn$Port)
conditions_psdn <- conditions %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 1 | port_ID == 2 | port_ID == 3) %>%
  column_to_rownames('port_name')

c_eff_psdn <- (conditional_effects
               (fit_qPSDN_price, "PSDN_SDM_60", surface=TRUE, conditions = conditions_psdn, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g1 <- plot(c_eff_psdn, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(a) Pacific sardine')+ 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = "Landings (tons)")

# MSQD plots
conditions2 <- data.frame(port_ID = unique(est_data_msqd$port_ID))
rownames(conditions2) <- unique(est_data_msqd$Port)
conditions_msqd <- conditions2 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 4  | port_ID == 5  | port_ID == 8) %>%
  column_to_rownames('port_name')
c_eff_msqd <- (conditional_effects
               (fit_qMSQD_price, "MSQD_SDM_90", surface=TRUE, conditions = conditions_msqd, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g2 <- plot(c_eff_msqd, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(b) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank()) 

# NANC plots
conditions3 <- data.frame(port_ID = unique(est_data_nanc$port_ID))
rownames(conditions3) <- unique(est_data_nanc$Port)
conditions_nanc <- conditions3 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 3  | port_ID == 4  | port_ID == 5) %>%
  column_to_rownames('port_name')
c_eff_nanc <- (conditional_effects
               (fit_qNANC_price, "NANC_SDM_20", surface=TRUE, conditions = conditions_nanc, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g3 <- plot(c_eff_nanc, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(c) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = element_blank()) 

# Merge plots
g1 + g2 + g3
```

### Interaction effects

```{r int_effect_PSDN_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
c_eff_int_psdn_msqd <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))
c_eff_int_psdn_nanc <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))
c_eff_int_msqd_nanc <- (conditional_effects(fit_qPSDN_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))

g1_PSDN <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(a) Pacific sardine x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) +
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 

g2_PSDN <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(b) Pacific sardine x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 

g3_PSDN <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(c) Northern anchovy x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_PSDN_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_PSDN / g2_PSDN
```

```{r int_effect_MSQD_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
c_eff_int_msqd_psdn <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))
c_eff_int_msqd_nanc <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))
c_eff_int_psdn_nanc <- (conditional_effects(fit_qMSQD_price, "PSDN_SDM_60_dOpen:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))

g1_MSQD <-  plot(c_eff_int_msqd_psdn, plot = FALSE)[[1]] + ggtitle('(a) Market squid x Pacific sardine') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) +
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): PSDN") 

g2_MSQD <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(b) Market squid x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 

g3_MSQD <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_MSQD_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_MSQD / g2_MSQD 
```

```{r int_effect_NANC_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
conditions_nanc <- conditions_nanc %>% filter(port_ID == 3  | port_ID == 5)

c_eff_int_nanc_psdn <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:PSDN_SDM_60_dOpen", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))
c_eff_int_nanc_msqd <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))
c_eff_int_psdn_msqd <- (conditional_effects(fit_qNANC_price, "PSDN_SDM_60_dOpen:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))

g1_NANC <-  plot(c_eff_int_nanc_psdn, plot = FALSE)[[1]] + ggtitle('(a) Northern anchovy x Pacific sardine') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) +
  scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): PSDN") 

g2_NANC <-  plot(c_eff_int_nanc_msqd, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): MSQD") 

g3_NANC <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_NANC_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_NANC / g2_NANC 
```

### Pacific sardine closure

```{r by_port_msqd_dclose, eval=FALSE, fig.cap=, include=FALSE}
# conditions_dClose <- data.frame(port_ID = unique(est_data_msqd$dClose))
# rownames(conditions_dClose) <- unique(est_data_msqd$dClose)
c_eff_close_msqd <- (conditional_effects(fit_qMSQD_price, "dClose", conditions = conditions_msqd, re_formula = NULL))
g1 <-  plot(c_eff_close_msqd, plot = FALSE)[[1]] + ggtitle('(a) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Landings (tons)") 

c_eff_close_nanc <- (conditional_effects(fit_qNANC_price, "dClose", conditions = conditions_nanc, re_formula = NULL))
g2 <- plot(c_eff_close_nanc, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  scale_x_discrete(name = "Closure? (1 = True; 0 = False)") +
  scale_y_continuous(name = "Landings (tons)") 

g1 / g2
```

# OTHER CODE

<!-- ```{r int_effect_sep, eval=FALSE, include=FALSE} -->
  
  <!-- hu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  <!-- mu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  
  <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
  
  <!-- ``` -->
  
  <!-- ```{r int_effect_sep_msqd, eval=FALSE, fig.cap=, include=FALSE} -->
  
  <!-- hu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  <!-- mu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  
  <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
  
  <!-- ``` -->
  
  
  <!-- ## Effort susbsitution ##  -->
  
  <!-- * Time series of number of trips (as a proxy for effort) for all the species -->
  <!-- * @richerson2017 use a method identify the nature of the outliers in an ARMA time series model (read more if interested)  -->
  <!--     + I propose to estimate a system of simultaneous equations (VECM model) to study equilibrium of effort and short-run and long-run effects of the closure (structural breaks) -->
  <!--     + Have a long-run equation for each species (simultaneously estimated) and test for structural break in this long-run relationship. -->
  
  
  <!-- ## Seasonality changes ## -->
  
  <!-- * Seasonality can be studied calculating monthly share of total trips by species, regress it using month dummiues and see any is there any structural change after the closure [@richerson2017].  -->
  
  <!-- <!-- Shall we study also effort as number of trips, and seasonality using time series -->
  
  
  