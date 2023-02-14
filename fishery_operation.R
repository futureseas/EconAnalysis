#################################
### Fishery operation results ###
#################################

rm(list = ls(all.names = TRUE)) 
gc()
# load("stan_fit.RData")

#---------------------------
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

#---------------------------
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
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")

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


###########################################
### Shifting Fishery Dynamics Over Time ###
###########################################

#----------------------------------
## Figure 1. Average annual landings, prices and revenues by species ##

# Landings
landings.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR + VESSEL_NUM, FUN=sumfun, data=PacFIN.month)
landings.year.2000_2014 <- landings.year %>% filter(LANDING_YEAR <2015) %>% mutate(period="2000-2014")
landings.year.2015_2020 <- landings.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")

landings.avg.year.2000_2014 <- landings.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, period) %>%
  summarize(avg_landings = mean(LANDED_WEIGHT_MTONS.sum.sum)) %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(avg_landings_species = mean(avg_landings))

landings.avg.year.2015_2020  <- landings.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, period) %>%
  summarize(avg_landings = mean(LANDED_WEIGHT_MTONS.sum.sum)) %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(avg_landings_species = mean(avg_landings))

landings.avg.year <- rbind.data.frame(landings.avg.year.2015_2020,landings.avg.year.2000_2014)

g1 <- ggplot(landings.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=avg_landings_species)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(a) Average annual vessel landings") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Average landings (tons)") +
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
revenue.year <- summaryBy(AFI_EXVESSEL_REVENUE.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR + VESSEL_NUM, FUN=sumfun, da=PacFIN.month)
revenue.year.2000_2014 <- revenue.year %>% filter(LANDING_YEAR <2015)  %>% mutate(period="2000-2014")
revenue.year.2015_2020 <- revenue.year %>% filter(LANDING_YEAR >=2015) %>% mutate(period="2015-2020")


revenue.avg.year.2000_2014 <- revenue.year.2000_2014 %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, period) %>%
  summarize(avg_revenue = mean(AFI_EXVESSEL_REVENUE.sum.sum)) %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(avg_revenue_species = mean(avg_revenue))

revenue.avg.year.2015_2020 <- revenue.year.2015_2020 %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, period) %>%
  summarize(avg_revenue = mean(AFI_EXVESSEL_REVENUE.sum.sum)) %>% group_by(PACFIN_SPECIES_CODE, period) %>%
  summarize(avg_revenue_species = mean(avg_revenue))

revenue.avg.year <- rbind.data.frame(revenue.avg.year.2015_2020,revenue.avg.year.2000_2014)

g3 <- ggplot(revenue.avg.year, aes(fill=period, x=PACFIN_SPECIES_CODE, y=avg_revenue_species)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("(b) Average annual vessel revenue") + 
  theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) + theme(legend.position = "none") +
  xlab("") + ylab("Average revenues (Millions of USD)") +  scale_y_continuous(labels = label_number(scale = 1e-6)) +
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
## Figure 2. Annual average landings by port area ##

# Calculate vessel landings by port and year
landings.by.port.year <- PacFIN.month %>% 
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE, VESSEL_NUM) %>%
  summarize(annual_landings = sum(LANDED_WEIGHT_MTONS.sum)) %>%
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, LANDING_YEAR, AGENCY_CODE) %>%
  summarize(avg_annual_landings = mean(annual_landings))

landings.by.port.year.PRE <- landings.by.port.year %>% filter(LANDING_YEAR <2015)
landings.by.port.avg.year <- landings.by.port.year.PRE %>%
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
  summarize(LANDED_WEIGHT_MTONS.sum.sum.mean = mean(avg_annual_landings)) %>% 
  filter(
    PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | 
      PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "MNA" | 
      PORT_AREA_CODE == "SFA" | PORT_AREA_CODE == "MNA" | 
      PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | 
      PORT_AREA_CODE == "MRA" | PORT_AREA_CODE == "NPS") %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                      "LAA", "SBA", "MRA", "MNA", "SFA", "CLO", "CLW", "CWA", "NPS"))

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
  xlab("") + 
  theme(axis.title = element_text(size = 9)) +
  ylab("Average annual landings (tons)") + theme(legend.position="none") +
  guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=c(
    "LAA" = "Los\nAngeles", "SBA" = "Santa\nBarbara", "MNA" = "Monterrey", 
    "SFA" = "San\nFrancisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
    "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", 
    "MRA" = "Morro\nBay", "NPS" = "North Puget\nSound")) +
  scale_color_brewer(palette="Set2") + ggtitle("(a) 2000-2014")


### Post

landings.by.port.year.POST <- landings.by.port.year %>% filter(LANDING_YEAR >= 2015)
landings.by.port.avg.year <- landings.by.port.year.POST %>%
  group_by(PACFIN_SPECIES_CODE, PORT_AREA_CODE, AGENCY_CODE) %>%
  summarize(LANDED_WEIGHT_MTONS.sum.sum.mean = mean(avg_annual_landings)) %>% 
  filter(
    PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | 
      PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "MNA" | 
      PORT_AREA_CODE == "SFA" | PORT_AREA_CODE == "MNA" | 
      PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | 
      PORT_AREA_CODE == "MRA" | PORT_AREA_CODE == "NPS") %>%
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, 
                                      "LAA", "SBA", "MRA", "MNA", "SFA", "CLO", "CLW", "CWA", "NPS"))

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
  ylab("Average annual landings (tons)") + guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=c(
    "LAA" = "Los\nAngeles", "SBA" = "Santa\nBarbara", "MNA" = "Monterrey", 
    "SFA" = "San\nFrancisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
    "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", 
    "MRA" = "Morro\nBay", "NPS" = "North Puget\nSound")) +
  scale_color_brewer(palette="Set2") + ggtitle("(b) 2015-2020") +    
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 500))

gg1 / gg2
rm(landings.by.port.avg.year, landings.by.port.year, 
   landings.by.port.year.PRE, landings.by.port.year.POST, states_names, gg1, gg2)



#----------------------------------
## Figure 3. Evolution of annual total landings and average annual prices ##

## Calculate average price and total landings by species per month
landing.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~
                        PACFIN_SPECIES_CODE + LANDING_YEAR + VESSEL_NUM, FUN=sum_mean_fun, data=PacFIN.month)

landing.year <- landing.year %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarize(LANDED_WEIGHT_MTONS.sum.sum = mean(LANDED_WEIGHT_MTONS.sum.sum))

price.year <- summaryBy(AFI_PRICE_PER_MTON.mean ~
                                  PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month)

landing.price.year <- merge(landing.year, price.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR"))
landing.price.year.sel <- landing.price.year %>% 
    dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean",
                  "PACFIN_SPECIES_CODE", "LANDING_YEAR")

# Graph landings v/s number of vessels
coeff1 <- 1
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
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))

coeff2 <- 3.5
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "PSDN")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff2 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))

g2 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff2, name = "Price (USD/Ton)")) + 
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Pacific sardine")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

coeff3 <- 0.8
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "NANC")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff3
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g3 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff3, name = "Price (USD/Ton)")) + 
  theme(legend.position="bottom") + scale_x_continuous(name = "Year") +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(c) Northern anchovy")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

(g1 / g2 / g3)

rm(df, g1, g2, g3, landing.year, landing.price.year.sel, coeff1, coeff2, coeff3, price.year)


#----------------------------------
## Figure 3X. Evolution of annual total landings and average fishmeal prices ##

## Calculate average price and total landings by species per month
landing.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~
                            PACFIN_SPECIES_CODE + LANDING_YEAR + VESSEL_NUM, FUN=sum_mean_fun, data=PacFIN.month)

landing.year <- landing.year %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarize(LANDED_WEIGHT_MTONS.sum.sum = mean(LANDED_WEIGHT_MTONS.sum.sum))


### Include world's fish meal price as instrument
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
PacFIN.month <- merge(PacFIN.month, fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
Deflactor <- read.csv(file = "C:\\Data\\PacFIN data\\deflactor.csv")
PacFIN.month <- merge(PacFIN.month, Deflactor, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
rm(Deflactor)
PacFIN.month$Price.Fishmeal.AFI <- PacFIN.month$Price.Fishmeal*PacFIN.month$defl
rm(fish.meal)

price.year <- summaryBy(Price.Fishmeal.AFI ~
                          PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month)

landing.price.year <- merge(landing.year, price.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR"))
landing.price.year.sel <- landing.price.year %>% 
  dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean",
                "PACFIN_SPECIES_CODE", "LANDING_YEAR")

# Graph landings v/s number of vessels
coeff1 <- 0.75
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD")
df$Price.Fishmeal.AFI.mean <- df$Price.Fishmeal.AFI.mean * coeff1 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"))
g1 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  ggtitle("(a) Market squid") +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff1, name = "Price (USD/Ton)")) +
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + # ggtitle("(b) Market Squid")  +  
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"),
                     labels=c("Price.Fishmeal.AFI.mean" = "Price fishmeal", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))

coeff2 <- 0.75
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "PSDN")
df$Price.Fishmeal.AFI.mean <- df$Price.Fishmeal.AFI.mean * coeff2 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"))

g2 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff2, name = "Price (USD/Ton)")) + 
  theme(legend.position="none") + scale_x_continuous(name = element_blank()) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Pacific sardine")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"),
                     labels=c("Price.Fishmeal.AFI.mean" = "Price fishmeal", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

coeff3 <- 0.5
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "NANC")
df$Price.Fishmeal.AFI.mean <- df$Price.Fishmeal.AFI.mean * coeff3
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"))
g3 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff3, name = "Price (USD/Ton)")) + 
  theme(legend.position="bottom") + scale_x_continuous(name = "Year") +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(c) Northern anchovy")  + guides(colour=guide_legend(title="Variables: ")) + 
  scale_color_brewer(palette="Paired",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "Price.Fishmeal.AFI.mean"),
                     labels=c("Price.Fishmeal.AFI.mean" = "Price fishmeal", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))  

(g1 / g2 / g3)

rm(df, g1, g2, g3, landing.year, landing.price.year.sel, coeff1, coeff2, coeff3, price.year)




#----------------------------------
## Figure 4. Landings v/s probability of presence by port area ##

nvessel.year <- PacFIN.month %>% dplyr::filter(LANDED_WEIGHT_MTONS.sum > 0) %>%
  dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% unique() %>%
  mutate(n_vessel = 1) %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
  summarise(n_vessel = sum(n_vessel))

PacFIN.month.CPS.nvessels <- left_join(PacFIN.month.CPS, nvessel.year, 
                                       by = c("LANDING_YEAR", "PACFIN_SPECIES_CODE"))

sdm.by.species <- PacFIN.month.CPS.nvessels %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, PSDN_SDM_60, MSQD_SDM_90_JS_CPUE,
                MSQD_SDM_90, MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_v2, DCRB_LANDING, 
                NANC_SDM_20, MSQD_recruitment, LANDED_WEIGHT_MTONS.sum, 
                PACFIN_SPECIES_CODE, PORT_AREA_CODE, n_vessel) %>% 
  filter(LANDING_YEAR >= 2000 & LANDING_YEAR <= 2019) %>%
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE, PACFIN_SPECIES_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_CPUE, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm=TRUE),
            MSQD_SPAWN_SDM_90_v2 = mean(MSQD_SPAWN_SDM_90_v2, na.rm=TRUE),
            MSQD_recruitment = mean(MSQD_recruitment, na.rm=TRUE),
            DCRB_LANDING = mean(DCRB_LANDING, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
            n_vessel = mean(n_vessel, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE",
                          "PSDN_SDM_60", "NANC_SDM_20", "MSQD_SDM_90_JS_cpue", "MSQD_SPAWN_SDM_90",
                          "MSQD_SDM_90", "MSQD_SPAWN_SDM_90_v2", "MSQD_recruitment", "DCRB_LANDING"), 
              names_from = PACFIN_SPECIES_CODE,
              values_from = c("LANDED_WEIGHT_MTONS", "n_vessel")) %>% 
  dplyr::rename(Landings_PSDN = LANDED_WEIGHT_MTONS_PSDN) %>% 
  dplyr::rename(Landings_MSQD = LANDED_WEIGHT_MTONS_MSQD) %>% 
  dplyr::rename(Landings_NANC = LANDED_WEIGHT_MTONS_NANC)


sdm.by.species.v2 <- PacFIN.month.CPS %>%
  dplyr::select(LANDING_YEAR, PSDN_SDM_60, MSQD_SDM_90_JS_CPUE,
                MSQD_SDM_90, MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_v2, DCRB_LANDING, 
                NANC_SDM_20, MSQD_recruitment, LANDED_WEIGHT_MTONS.sum, 
                PACFIN_SPECIES_CODE, PORT_AREA_CODE, VESSEL_NUM) %>% 
  filter(LANDING_YEAR >= 2000 & LANDING_YEAR <= 2019) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE, PACFIN_SPECIES_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = mean(LANDED_WEIGHT_MTONS, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c("LANDING_YEAR", "PORT_AREA_CODE",
                          "PSDN_SDM_60", "NANC_SDM_20", "MSQD_SPAWN_SDM_90"), 
              names_from = PACFIN_SPECIES_CODE,
              values_from = c("LANDED_WEIGHT_MTONS")) %>% 
  dplyr::rename(Landings_PSDN = PSDN) %>% 
  dplyr::rename(Landings_MSQD = MSQD) %>% 
  dplyr::rename(Landings_NANC = NANC)


area_names <- as_labeller(c(`LAA` = "Los Angeles", `SBA` = "Santa Barbara",
                            `MNA` = "Monterey", `CLO` = "Columbia River (OR)", `CLW` = "Columbia River (WA)", 
                            `CWA` = "Washington Coastal Ports", `SFA` = "San Francisco"))

# sdm.by.species$Date <- zoo::as.yearmon(paste(sdm.by.species$LANDING_YEAR, sdm.by.species$LANDING_MONTH, sep='-'))


# Create sardine plot
sdm.by.species.PSDN <- sdm.by.species.v2 %>% 
  filter(PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "CLO"| PORT_AREA_CODE == "LAA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "MNA", "CLO")) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            Landings_PSDN = sum(Landings_PSDN, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_PSDN)) %>%
  mutate(RATIO = ifelse(is.na(RATIO), 0, RATIO))



# Create anchovy plot
sdm.by.species.NANC <- sdm.by.species.v2 %>% 
  filter(PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "SBA", "MNA", "CLO")) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            Landings_NANC = sum(Landings_NANC, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_NANC)) %>%
  mutate(RATIO = ifelse(is.na(RATIO), 0, RATIO))


# Plot all squid SDMs outputs

sdm.by.species.MSQD <- sdm.by.species.v2 %>% 
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "SBA", "MNA")) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm = TRUE),
            Landings_MSQD = sum(Landings_MSQD, na.rm = TRUE)) %>% 
  mutate(RATIO = (Landings_MSQD)) %>%
  mutate(RATIO = ifelse(is.na(RATIO), 0, RATIO))


cors1 <- plyr::ddply(sdm.by.species.PSDN, c("PORT_AREA_CODE"), summarise, cor = round(cor(RATIO, PSDN_SDM_60), 2))
g1 <- ggplot(sdm.by.species.PSDN) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = PSDN_SDM_60*coeff1),
            size = 0.5, color = "blue", linetype = "dashed") +
  geom_text(data = cors1, 
            aes(x = Inf, y = Inf, 
                label = paste("r=", cor, sep="")),
            hjust = 1, vjust = 1) +
  facet_grid(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")), labeller = area_names) +
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings by active vessel", sec.axis = sec_axis(~./coeff1, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  ggtitle("(b) Pacific sardine (60 km radius)") 

cors2 <- plyr::ddply(sdm.by.species.NANC, c("PORT_AREA_CODE"), summarise, cor = round(cor(RATIO, NANC_SDM_20), 2))
g2 <-  ggplot(sdm.by.species.NANC) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO, color = "Landings by active vessel"), size = 0.5) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = NANC_SDM_20*coeff2, color = "Probability of presence"),
            size = 0.5, linetype = "dashed") +
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("MNA", "SBA", "CLO")), labeller = area_names,  ncol = 4) +
  geom_text(data = cors2, 
            aes(x = Inf, y = Inf, 
                label = paste("r=", cor, sep="")),
            hjust = 1, vjust = 1) +
  scale_x_continuous(name = "Year")  +
  scale_y_continuous(name = "Landings by active vessel", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8),
        legend.position="bottom") +
  ggtitle("(c) Northern anchovy (20 km radius)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Landings by active vessel" = "grey", "Probability of presence" = "blue"))

cors4 <- plyr::ddply(sdm.by.species.MSQD, c("PORT_AREA_CODE"), summarise, cor = round(cor(RATIO, MSQD_SPAWN_SDM_90), 2))
g4 <- ggplot(sdm.by.species.MSQD) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = RATIO), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90*coeff4),
            size = 0.5, linetype = "dashed", color = "blue") +
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 4) +
  geom_text(data = cors4, 
            aes(x = Inf, y = Inf, 
                label = paste("r=", cor, sep="")),
            hjust = 1, vjust = 1) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Landings by active vessel", sec.axis = sec_axis(~./coeff4, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") +
  ggtitle("(a) Market squid (spawning aggregation model; 90 km radius)")

coeff4 <- 15000
coeff1 <- 3000
coeff2 <- 1500

g4 / g1 / g2

# rm(g1, g2, g4, cors1, cors2, cors4, sdm.by.species.MSQD, sdm.by.species.NANC, sdm.by.species.PSDN,
#    coeff1, coeff2, coeff4, area_names)


#####################
### Other figures ###
#####################


#----------------------------------
## Figure X1. Evolution of pacific sardine and squid landings and the number of vessels landing squid
#
# nvessel.year <- PacFIN.month %>% dplyr::filter(LANDED_WEIGHT_MTONS.sum > 0) %>%
#   dplyr::select(PACFIN_SPECIES_CODE, LANDING_YEAR, VESSEL_NUM) %>% unique() %>%
#   mutate(n_vessel = 1) %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR) %>%
#   summarise(n_vessel = sum(n_vessel))
# 
# landing.price.year.sel <- summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~
#   PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month) %>%
#   inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
#   dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")
# 
# 
# # Graph landing v/s number of  vessels
# coeff <- 1500
# df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD" | PACFIN_SPECIES_CODE == "PSDN") %>%
#   mutate(n_vessel = ifelse(PACFIN_SPECIES_CODE == "MSQD", n_vessel, 0)) %>%
#   mutate(LANDED_WEIGHT_MTONS.sum.sum = ifelse(PACFIN_SPECIES_CODE == "PSDN",
#                                               LANDED_WEIGHT_MTONS.sum.sum,
#                                               ifelse(PACFIN_SPECIES_CODE == "MSQD",
#                                                      LANDED_WEIGHT_MTONS.sum.sum,
#                                                      0))) %>%
#   group_by(LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
#   summarise(n_vessel = sum(n_vessel), LANDED_WEIGHT_MTONS.sum.sum = sum(LANDED_WEIGHT_MTONS.sum.sum)) %>%
#   reshape2::melt(id.vars=c("LANDING_YEAR", "PACFIN_SPECIES_CODE")) %>%
#   reshape2::dcast(LANDING_YEAR ~ PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
#   dplyr::select(-c("PSDN_n_vessel"))
# 
# 
# df$MSQD_n_vessel <- df$MSQD_n_vessel * coeff
# df <- gather(df, key = Variable, value = value,
#              c("MSQD_n_vessel", "MSQD_LANDED_WEIGHT_MTONS.sum.sum", "PSDN_LANDED_WEIGHT_MTONS.sum.sum"))
# ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) +
#   geom_line(aes(linetype=Variable), size=1) +
#   geom_point() +
#   scale_linetype_manual(values=c("solid", "dashed", "solid"), labels=c(
#                               "PSDN_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of Pacific sardine",
#                               "MSQD_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of market squid",
#                               "MSQD_n_vessel" = "Number of squid vessels")) +
#   scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff, name = "Number of vessels landing market squid")) +
#   theme(legend.position="bottom") + scale_x_continuous(name = element_blank()) +
#   theme(plot.title = element_text(size=9, face="bold.italic")) +
#   scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels=c(
#                            "PSDN_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of Pacific sardine",
#                            "MSQD_LANDED_WEIGHT_MTONS.sum.sum" = "Landings of market squid",
#                            "MSQD_n_vessel" = "Number of squid vessels"))
# 
# rm(df, coeff, landing.price.year.sel)


#----------------------------------
# # Figure X2. Evolution of squid landings per vessel and number of sets by MSQD vessels
#  
# landing.price.year.sel <- summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~
#                                       PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month) %>%
#   inner_join(nvessel.year, by = c("PACFIN_SPECIES_CODE", "LANDING_YEAR")) %>%
#   dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "n_vessel", "PACFIN_SPECIES_CODE", "LANDING_YEAR")
# 
# df2 <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD" | PACFIN_SPECIES_CODE == "PSDN") %>%
#   mutate(n_vessel = ifelse(PACFIN_SPECIES_CODE == "MSQD", n_vessel, 0)) %>%
#   mutate(LANDED_WEIGHT_MTONS.sum.sum = ifelse(PACFIN_SPECIES_CODE == "PSDN",
#                                               LANDED_WEIGHT_MTONS.sum.sum,
#                                               ifelse(PACFIN_SPECIES_CODE == "MSQD",
#                                                      LANDED_WEIGHT_MTONS.sum.sum,
#                                                      0))) %>%
#   group_by(LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
#   summarise(n_vessel = sum(n_vessel), LANDED_WEIGHT_MTONS.sum.sum = sum(LANDED_WEIGHT_MTONS.sum.sum)) %>%
#   reshape2::melt(id.vars=c("LANDING_YEAR", "PACFIN_SPECIES_CODE")) %>%
#   reshape2::dcast(LANDING_YEAR ~ PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
#   dplyr::select(-c("PSDN_n_vessel")) %>% mutate(r_landing_vessels = MSQD_LANDED_WEIGHT_MTONS.sum.sum / MSQD_n_vessel)
# 
# 
# gg1 <- ggplot(df2, aes(x=LANDING_YEAR, y = r_landing_vessels)) +
#   geom_line() +
#   ggtitle("(a) Average landing by active vessels") +
#   geom_point() +
#   scale_linetype_manual(values=c("dashed")) +
#   scale_y_continuous(name = "Ratio (Average landing by active vessels)") + xlab("Landing year")
# 
# 
# rm(df2)
# 
# 
#   # Load data #
#   sqd.logbook.vessel <- read.csv("C:\\Data\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
#     dplyr::rename(lat = SetLatitude) %>%
#     dplyr::rename(lon = SetLongitude) %>%
#     dplyr::rename(LogSerialNumber = ?..LogSerialNumber) %>%
#     mutate(vessel="CA Vessel") %>%
#     mutate(date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
#     mutate(LANDING_YEAR = year(date)) %>%
#     mutate(month = month(date)) %>%
#     dplyr::rename(effort = "CatchEstimate")
# 
#   max_n_set.by.trip <- sqd.logbook.vessel %>%
#     filter(SetNumber>0 & SetNumber<99) %>%
#     group_by(LogSerialNumber) %>%
#     filter(SetNumber == max(SetNumber, na.rm=TRUE)) %>%
#     dplyr::select(c('LogSerialNumber', 'SetNumber', 'LANDING_YEAR')) %>% unique() %>%
#     group_by(LANDING_YEAR) %>% summarise(avg_set = mean(SetNumber))
# 
# 
# gg2 <- max_n_set.by.trip %>%
#   ggplot(aes(x=LANDING_YEAR, y=avg_set)) +
#   ggtitle("(b) Average number of sets") +
#     geom_line() +
#     geom_point() +
#   scale_y_continuous(name = "Average number of sets") + xlab("Landing year")
# 
# gg1 + gg2
#   
# rm(gg1, gg2, landing.price.year.sel, max_n_set.by.trip, sqd.logbook.vessel) 

#---------------------------------------------------------------------------------------------------------
# Figure X3. Seasonality on the probability of presence by species

# str(sdm.by.species)
# 
# sdm.by.species.LONG <- sdm.by.species %>% ungroup() %>%
#   dplyr::select('LANDING_MONTH', 'PORT_AREA_CODE',
#                 'PSDN_SDM_60', 'NANC_SDM_20', 'MSQD_SPAWN_SDM_90') %>%
#                 #, 'DCRB_LANDING') %>%
#   dplyr::mutate(MSQD_SPAWN_SDM_90_z = ((MSQD_SPAWN_SDM_90 - mean(MSQD_SPAWN_SDM_90, na.rm = TRUE))/sd(MSQD_SPAWN_SDM_90, na.rm = TRUE))) %>%
#   dplyr::mutate(PSDN_SDM_60_z = ((PSDN_SDM_60 - mean(PSDN_SDM_60, na.rm = TRUE))/sd(PSDN_SDM_60, na.rm = TRUE))) %>%
#   dplyr::mutate(NANC_SDM_20_z = ((NANC_SDM_20 - mean(NANC_SDM_20, na.rm = TRUE))/sd(NANC_SDM_20, na.rm = TRUE))) %>%
#   #dplyr::mutate(DCRB_LANDING_z = ((DCRB_LANDING - mean(DCRB_LANDING, na.rm = TRUE))/sd(DCRB_LANDING, na.rm = TRUE))) %>%
#   group_by(LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarize(NANC = mean(NANC_SDM_20_z, na.rm = TRUE), MSQD = mean(MSQD_SPAWN_SDM_90_z, na.rm = TRUE),
#             PSDN = mean(PSDN_SDM_60_z, na.rm = TRUE)) %>%
#               #, DCRB = mean(DCRB_LANDING_z, na.rm = TRUE)/4) %>%
#   gather(Species, SDM, c('NANC', 'PSDN', 'MSQD'), factor_key=TRUE)
#   #, 'DCRB'), factor_key=TRUE)
# 
# sdm.by.species.LONG$Month <- as.factor(sdm.by.species.LONG$LANDING_MONTH)
# # sdm.by.species.LONG <- transform(sdm.by.species.LONG, MonthAbb = month.abb[LANDING_MONTH])
# 
# # %>%
# #   mutate(PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "NANC_SDM_20", "Northern anchovy", PACFIN_SPECIES_CODE)) %>%
# #   mutate(PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "2", "Pacific sardine", PACFIN_SPECIES_CODE)) %>%
# #   mutate(PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "3", "Market squid", PACFIN_SPECIES_CODE))
# 
# # port_names <- as_labeller(c(`LAA` = "Los Angeles", `SBA` = "Santa Barbara"))
# # , labeller = port_name
# 
# ggplot(sdm.by.species.LONG, aes(fill=Species, y=SDM, x=Month)) +
#   geom_bar(position="dodge", stat="identity") +
#   facet_wrap(~ PORT_AREA_CODE) + ylab("Probability of presence (z-value)")
#   theme(strip.text.x = element_text(size = 7))
# 
#   rm(sdm.by.species.LONG, area_names)

#---------------------------------------------------------------------------------------------------------
## Figure X4. Percentage of squid landing from total landings.

# # I wonder if this perception is due to the fact that some vessels actually changed
# # their catch composition, but that something else was limiting landings - perhaps availability.
# # Could you plot annual squid landings relative to total annual landings the above for the CPS LE vessels?
# 
# ## Calculate average price and total landings by species per month
# landing.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month) %>%
#   dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "PACFIN_SPECIES_CODE", "LANDING_YEAR")
#   landing.year.total <- landing.year %>% group_by(LANDING_YEAR) %>% summarise(total_year = sum(LANDED_WEIGHT_MTONS.sum.sum))
# 
# # Graph landings v/s number of vessels
# df <- landing.year %>% merge(landing.year.total, by = c("LANDING_YEAR"), all.x = TRUE) %>%
#   filter(PACFIN_SPECIES_CODE == "MSQD") %>% mutate(relative_landings = LANDED_WEIGHT_MTONS.sum.sum/total_year) %>%
#   dplyr::select("LANDING_YEAR", "relative_landings")
# 
# ggplot(df, aes(x=LANDING_YEAR, y = relative_landings)) + geom_line(size=1) +
#   scale_y_percent(name = "% of total landings")
# 
# rm(df, landing.year, landing.year.total)


#----------------------------------
# # Figure X5. Fishing seasons ##
# 
# port_area_names <- as_labeller(c("LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey",
#                                  "CLO" = "Columbia\nRiver (OR)", "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports"))
# 
# # port_names <- as_labeller(c("SP" = "San Pedro", "HNM" = "Hueneme", "MNT" = "Monterrey", "MOS" = "Moss Landing",
# #                             "LWC" = "Ilwaco / Chinook", "AST" = "Astoria",
# #                             "PRN" = "Princeton/ Half Moon\nBay", "SF" = "San Francisco",
# #                             "SLT" = "Sausalito", "TRM" = "Terminal Island",
# #                             "VEN" = "Ventura", "WPT" = "Westport"))
# 
# q.psdn.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_PSDN = sum(LANDED_WEIGHT_MTONS.sum)) %>%
#   group_by(LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_PSDN = mean(Landings_PSDN)) %>%
#   filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA" |
#            PORT_AREA_CODE == "CLO")
# 
# g1 <- ggplot(data=q.psdn.by.month, aes(x=LANDING_MONTH, y=Landings_PSDN)) +
#   geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
#   scale_x_continuous(name = "", breaks=1:12) +
#   scale_y_continuous(name = "Landings (M tons)" ) +
#   facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")),
#              labeller = port_area_names, ncol = 1) + ggtitle("(a) Pacific sardine") +
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))
# 
# q.msqd.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
#   group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_MSQD = sum(LANDED_WEIGHT_MTONS.sum)) %>%
#   group_by(LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_MSQD = mean(Landings_MSQD)) %>%
#   filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" |
#            PORT_AREA_CODE == "MNA")
# 
# g2 <- ggplot(data=q.msqd.by.month, aes(x=LANDING_MONTH, y=Landings_MSQD)) +
#   geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
#   scale_x_continuous(name = "Month", breaks=1:12) +
#   scale_y_continuous(name = "" ) +
#   facet_wrap(~factor(PORT_AREA_CODE, levels = c("LAA", "SBA", "MNA")),
#              labeller = port_area_names, ncol = 1) + ggtitle("(b) Market squid") +
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))
# 
# q.nanc.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("NANC")) %>%
#   group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_NANC = sum(LANDED_WEIGHT_MTONS.sum)) %>%
#   group_by(LANDING_MONTH, PORT_AREA_CODE) %>%
#   summarise(Landings_NANC = mean(Landings_NANC)) %>%
#   filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" |
#            PORT_AREA_CODE == "CWA")
# 
# g3 <- ggplot(data=q.nanc.by.month, aes(x=LANDING_MONTH, y=Landings_NANC)) +
#   geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
#   scale_x_continuous(name = "", breaks=1:12) +
#   scale_y_continuous(name = "" ) +
#   facet_wrap(~factor(PORT_AREA_CODE, levels = c("SBA", "MNA", "CWA")),
#              labeller = port_area_names, ncol = 1) + ggtitle("(c) Northern anchovy") +
#   theme(plot.title = element_text(size=9, face="bold.italic"), axis.text = element_text(size = 6))
# 
# 
# g1 + g2 + g3
# 
# rm(g1, g2, g3, q.psdn.by.month, q.msqd.by.month, q.nanc.by.month, port_area_names)

#----------------------------------
# # Figure X6. Yearly total landing, average price and total number of vessel by CPS species overtime (1981 - 2020).
# 
# landing.price.year.CPS <- landing.price.year %>% filter(PACFIN_SPECIES_CODE != "OTHER")
# nvessel.year.CPS <- nvessel.year %>% filter(PACFIN_SPECIES_CODE != "OTHER")
# 
# 
# g1 <- ggplot(landing.price.year.CPS,
#              aes(x = LANDING_YEAR, y = LANDED_WEIGHT_MTONS.sum.sum, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Landings (MTons)") +
#   theme(legend.position = "right", plot.title = element_text(size=9, face="bold.italic"),
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(b) Landings") +   guides(colour=guide_legend(title="Species: ")) +
#   scale_color_brewer(palette="Set2", labels=c("MSQD" = "Market Squid",
#                                               "NANC" = "Northern Anchovy",
#                                               "OMCK" = "Mackerels",
#                                               "PSDN" = "Pacific Sardine"))
# 
# g2 <- ggplot(landing.price.year.CPS,
#              aes(x = LANDING_YEAR, y = AFI_PRICE_PER_MTON.mean.mean, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Price ($/MTon)")  +
#   scale_color_brewer(palette="Set2", labels=c("MSQD" = "Market Squid",
#                                               "NANC" = "Northern Anchovy",
#                                               "OMCK" = "Mackerels",
#                                               "PSDN" = "Pacific Sardine")) +  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic"),
#                                               axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(a) Prices") +   guides(colour=guide_legend(title="Species: "))
# 
# g3 <- ggplot(nvessel.year.CPS, aes(x = LANDING_YEAR, y = n_vessel, group = PACFIN_SPECIES_CODE, colour = PACFIN_SPECIES_CODE)) +
#   geom_line(size=1) + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "# of vessels") +   theme(legend.position="none")  +
#   scale_color_brewer(palette="Set2", labels=c("MSQD" = "Market Squid",
#                                               "NANC" = "Northern Anchovy",
#                                               "OMCK" = "Mackerels",
#                                               "PSDN" = "Pacific Sardine")) +  theme(plot.title = element_text(size=9, face="bold.italic"),
#                                               axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   ggtitle("(c) Number of vessels") +   guides(colour=guide_legend(title="Species: "))
# 
# (g2 + g1) / g3
# 
# rm(g1, g2, g3)


########################
### Switching graphs ###
########################

# #---------------------------------
# # ## Figure X7. Histogram of percentage of the catch that come from squid (conditional vessel also landed sardine) 
# 
# # PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
# #   PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
# # 
# #   # Merge different decades of data #
# # PacFIN <- rbind.data.frame(PacFIN_2000_2009, PacFIN_2010_2020)
# #   rm(PacFIN_2000_2009, PacFIN_2010_2020)
# #   gc()
# # 
# # psdn.landings.by.day.vessel <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
# #   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, LANDED_WEIGHT_MTONS) %>%
# #   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
# #   group_by(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
# #   summarise(Landed_PSDN = sum(LANDED_WEIGHT_MTONS)) %>% mutate(harv.psdn = 1)
# # 
# # # If a vessel harvest Pacific sardine in a day, how many also harvest squid.
# # squid.cond.psdn <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
# #   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, LANDED_WEIGHT_MTONS) %>%
# #   merge(psdn.landings.by.day.vessel, by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
# #   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>%
# #   mutate(perc_land_squid = LANDED_WEIGHT_MTONS / (LANDED_WEIGHT_MTONS + Landed_PSDN))
# # 
# # write.csv(squid.cond.psdn,"C:\\Data\\day_switch_land.csv", row.names = FALSE)
# df <- read.csv(file = "C:\\Data\\day_switch_land.csv")
# hist(df$perc_land_squid, col = 'skyblue3', breaks = 100)
# plot.ecdf(df$perc_land_squid)


# ---------------------------------
# ## Figure X8. Percentage of vessel harvesting squid conditional on sardine in average during a day of a year.

# # PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
# #   PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
# # 
# #   # Merge different decades of data #
# # PacFIN <- rbind.data.frame(PacFIN_2000_2009, PacFIN_2010_2020)
# #   rm(PacFIN_2000_2009, PacFIN_2010_2020)
# #   gc()
# # 
# # psdn.by.day <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
# #   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
# #   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
# #   unique() %>% mutate(harv.psdn = 1)
# # 
# # # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# # squid.cond.psdn <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
# #   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
# #   merge(psdn.by.day, by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
# #   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()
# # 
# # # Calculate number of vessels harvesting sardine and squid and percentage per year
# # n_vessel_psdn <- psdn.by.day %>% group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
# #   summarize(n_vessel_psdn = sum(harv.psdn, na.rm = TRUE))
# # # %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
# # #   summarize(n_vessel_psdn = mean(n_vessel_psdn, na.rm = TRUE))
# # 
# # n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
# #   summarize(n_vessel_msqd = sum(harv.psdn, na.rm = TRUE))
# # # %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
# # #   summarize(n_vessel_msqd = mean(n_vessel_msqd, na.rm = TRUE))
# # 
# # df <- merge(n_vessel_psdn, n_vessel_msqd_c_psdn, by=c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
# #     mutate(n_vessel_msqd = if_else(is.na(n_vessel_msqd), 0, n_vessel_msqd)) %>% group_by(LANDING_MONTH, LANDING_DAY) %>%
# #     summarize(n_vessel_msqd = sum(n_vessel_msqd, na.rm = TRUE), n_vessel_psdn = sum(n_vessel_psdn, na.rm = TRUE)) %>%
# #     mutate(perc = (n_vessel_msqd / n_vessel_psdn)) %>% select(LANDING_MONTH, LANDING_DAY, perc)
# # 
# # df$MonthDay <- paste( month.abb[df$LANDING_MONTH], df$LANDING_DAY, sep="-" )
# # 
# # write.csv(df,"C:\\Data\\day_switch.csv", row.names = FALSE)
# df <- read.csv(file = "C:\\Data\\day_switch.csv")
# ggplot(data=df, aes(x=MonthDay, y=perc)) +
#   geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
#   scale_x_discrete(name = "Day", breaks=1:366) +
#   scale_y_percent(name = "Percentage of vessels harvesting squid" )
# rm(df)


#---------------------------------
# ## Figure X9. Percentage of vessel landings squid (or sardine) conditional on the other species landing over time.

# psdn.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   unique() %>% mutate(harv.psdn = 1)
# msqd.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
#   unique() %>% mutate(harv.msqd = 1)
# 
# # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# squid.cond.psdn <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% merge(psdn.by.year,
#                                                                   by = c("VESSEL_NUM", "LANDING_YEAR"), all.y = TRUE) %>%
#   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()
# 
# # Calculate number of vessels harvesting sardine and squid and percentage per year
# n_vessel_psdn <- psdn.by.year %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_psdn = sum(harv.psdn))
# n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_msqd = sum(harv.psdn))
# df <- n_vessel_msqd_c_psdn %>% mutate(perc = n_vessel_psdn$n_vessel_psdn) %>%
#   mutate(n_vessel_only_psdn = perc) %>%  # - n_vessel_msqd
#   mutate(perc = (n_vessel_msqd / perc))
# 
# coeff = 115
# g1 <- ggplot(df) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_only_psdn,
#                           color = "# of vessels landing sardine"), size = 1) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_msqd,
#                           color = "# of vessels landing squid (c/ landing sardine)"), size = 1) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = perc*coeff,
#                           color = "% of vessels landing squid (c/ landings sardine)"),
#             size = 1, linetype = "dashed") + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Number of vessels",
#                      sec.axis = sec_axis(~./coeff, name = "% vessels harvesting squid" )) +
#   theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +  scale_color_manual(name = "Variables: ",
#                                                                                                        values = c("# of vessels landing sardine" = "green",
#                                                                                                                   "# of vessels landing squid (c/ landing sardine)" = "blue",
#                                                                                                                   "% of vessels landing squid (c/ landings sardine)" = "grey")) +
#   theme(legend.position="right")
# 
# rm(psdn.by.year, squid.cond.psdn, n_vessel_psdn, n_vessel_msqd_c_psdn, df, coeff)
# 
# msqd.by.year <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2020) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
#   unique() %>% mutate(harv.msqd = 1)
# 
# # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# psdn.cond.msqd <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2020) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR) %>% merge(msqd.by.year,
#                                                                   by = c("VESSEL_NUM", "LANDING_YEAR"), all.y = TRUE) %>%
#   filter(harv.msqd == 1) %>% filter(PACFIN_SPECIES_CODE.x == "PSDN") %>% unique()
# 
# # Calculate number of vessels harvesting sardine and squid and percentage per year
# n_vessel_msqd <- msqd.by.year %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_msqd = sum(harv.msqd))
# n_vessel_psdn_c_msqd <- psdn.cond.msqd %>% group_by(LANDING_YEAR) %>% summarize(n_vessel_psdn = sum(harv.msqd))
# df <- n_vessel_psdn_c_msqd %>%  mutate(n_vessel_msqd = n_vessel_msqd$n_vessel_msqd) %>%
#   mutate(perc = n_vessel_psdn / n_vessel_msqd)
# 
# coeff = 200
# g2 <- ggplot(df) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_msqd,
#                           color = "# of vessels landing squid"), size = 1) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = n_vessel_psdn,
#                           color = "# of vessels landing sardine (c/ landing squid)"), size = 1) +
#   geom_line(mapping = aes(x = LANDING_YEAR, y = perc*coeff,
#                           color = "% of vessels landing sardine (c/ landings squid)"),
#             size = 1, linetype = "dashed") + scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Number of vessels",
#                      sec.axis = sec_axis(~./coeff, name = "% vessels harvesting squid" )) +
#   theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +  scale_color_manual(name = "Variables: ",
#                                                                                                        values = c("# of vessels landing squid" = "green",
#                                                                                                                   "# of vessels landing sardine (c/ landing squid)" = "blue",
#                                                                                                                   "% of vessels landing sardine (c/ landings squid)" = "grey")) +
#   theme(legend.position="right")
# 
# g1 / g2
# 
# rm(msqd.by.year, psdn.cond.msqd, n_vessel_msqd, n_vessel_psdn_c_msqd, df, coeff, g1, g2)


#---------------------------------
# ## Figure X10. Switching by month. Percentage of vessel harvesting squid.

# psdn.by.month <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH) %>% filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   unique() %>% mutate(harv.psdn = 1)
# 
# # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# squid.cond.psdn <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH) %>% merge(psdn.by.month,
#                                                                                  by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH"), all.y = TRUE) %>%
#   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>% unique()
# 
# # Calculate number of vessels harvesting sardine and squid and percentage per year
# n_vessel_psdn <- psdn.by.month %>% group_by(LANDING_YEAR, LANDING_MONTH) %>%
#   summarize(n_vessel_psdn = sum(harv.psdn, na.rm = TRUE))
# 
# n_vessel_msqd_c_psdn <- squid.cond.psdn %>% group_by(LANDING_YEAR, LANDING_MONTH) %>%
#   summarize(n_vessel_msqd = sum(harv.psdn, na.rm = TRUE))
# 
# df <- merge(n_vessel_psdn, n_vessel_msqd_c_psdn, by=c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>%
#   mutate(n_vessel_msqd = if_else(is.na(n_vessel_msqd), 0, n_vessel_msqd)) %>% group_by(LANDING_MONTH) %>%
#   summarize(n_vessel_msqd = sum(n_vessel_msqd, na.rm = TRUE), n_vessel_psdn = sum(n_vessel_psdn, na.rm = TRUE)) %>%
#   mutate(perc = (n_vessel_msqd / n_vessel_psdn)) %>% select(LANDING_MONTH, perc)
# 
# ggplot(data=df, aes(x=LANDING_MONTH, y=perc)) +
#   geom_bar(stat="identity", fill=rgb(0.1,0.4,0.5,0.7), width=0.4) +
#   scale_x_continuous(name = "Month", breaks=1:12) +
#   scale_y_percent(name = "Percentage of vessels harvesting squid" )
# 
# rm(psdn.by.month, squid.cond.psdn, n_vessel_psdn, n_vessel_msqd_c_psdn, df)