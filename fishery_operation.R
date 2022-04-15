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


###########################
### Descriptive figures ###
###########################

#----------------------------------
## Figure 1. Annual average landings by port area ##

# Calculate landings by port and year
landings.by.port.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ 
                                     PACFIN_SPECIES_CODE + PORT_AREA_CODE + LANDING_YEAR + AGENCY_CODE, FUN=sumfun, data=PacFIN.month)
landings.by.port.year <- landings.by.port.year %>% filter(LANDING_YEAR <2015)
landings.by.port.avg.year <- summaryBy(LANDED_WEIGHT_MTONS.sum.sum ~ PACFIN_SPECIES_CODE + PORT_AREA_CODE 
                                       + AGENCY_CODE, FUN=meanfun, data=landings.by.port.year) %>%
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


ggplot(landings.by.port.avg.year, aes(fill=PACFIN_SPECIES_CODE, 
                                      y=LANDED_WEIGHT_MTONS.sum.sum.mean, x=PORT_AREA_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ AGENCY_CODE, scale="free", space="free_x", labeller = states_names) + 
  theme(strip.text.x = element_text(size = 7)) +
  theme(legend.position="bottom") + 
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +
  theme(axis.text.x = element_text(angle=90, hjust=0.95, vjust=0.45)) + xlab("Ports") + 
  ylab("Landings (tons)") + guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels=c(
    "LAA" = "Los Angeles", "SBA" = "Santa Barbara", "MNA" = "Monterrey", 
    "SFA" = "San Francisco", "ERA" = "Eureka", "CLO" = "Columbia\nRiver (OR)", 
    "CLW" = "Columbia\nRiver (WA)", "CWA" = "Washington\nCoastal Ports", "NPS" = "North Puget\nSound")) +
  scale_color_brewer(palette="Set2")

# "SP" = "San\nPedro", "HNM" = "Hueneme", 
# "MNT" = "Monterrey", "MOS" = "Moss\nLanding", 
# "LWC" = "Ilwaco /\nChinook", "AST" = "Astoria", 
# "PRN" = "Princeton/\nHalf Moon\nBay", "TRM" = "Terminal\nIsland",
# "VEN" = "Ventura", "WIN" = "Winchester\nBay",
# "WLM" = "Willmington", "WPT" = "Westport", "NEW" = "Newport", 
# "ERK" = "Eureka"

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
coeff1 <- 200
df <- landing.price.year.sel %>% filter(PACFIN_SPECIES_CODE == "MSQD")
df$AFI_PRICE_PER_MTON.mean.mean <- df$AFI_PRICE_PER_MTON.mean.mean * coeff1 
df <- gather(df, key = Variable, value = value,
             c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"))
g1 <- ggplot(df, aes(x=LANDING_YEAR, y = value, colour = Variable)) + geom_line(size=1) +
  geom_point() +
  scale_y_continuous(name = "Landings (M Tons)", sec.axis = sec_axis(~./coeff1, name = "Price (USD/Ton)")) +
  theme(legend.position="bottom") + scale_x_continuous(name = element_blank()) + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + # ggtitle("(b) Market Squid")  +  
  scale_color_brewer(palette="Set2",
                     limits = c("LANDED_WEIGHT_MTONS.sum.sum", "AFI_PRICE_PER_MTON.mean.mean"),
                     labels=c("AFI_PRICE_PER_MTON.mean.mean" = "Price", "LANDED_WEIGHT_MTONS.sum.sum" = "Landings"))

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

(g1)
rm(df, g1, g2, landing.price.year, landing.price.year.sel, coeff1, coeff2)



#----------------------------------
## Figure 5. Evolution of pacific sardine and squid landings and the number of vessels landing squid

# Create dataframes
nvessel.year <- PacFIN.month %>% 
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

rm(df, nvessel.year, coeff)



#----------------------------------
## Figure 6. Landings v/s probability of presence by port area ##

sdm.by.species <- PacFIN.month.CPS %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, PSDN_SDM_60, MSQD_SDM_90_JS_CPUE, MSQD_SDM_90, MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_v2, 
                NANC_SDM_20, MSQD_recruitment, LANDED_WEIGHT_MTONS.sum, PACFIN_SPECIES_CODE, PORT_AREA_CODE) %>% 
  filter(LANDING_YEAR >= 1998 & LANDING_YEAR <= 2019) %>%
  group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE, PACFIN_SPECIES_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_CPUE, na.rm = TRUE),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=TRUE),
            MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm=TRUE),
            MSQD_SPAWN_SDM_90_v2 = mean(MSQD_SPAWN_SDM_90_v2, na.rm=TRUE),
            MSQD_recruitment = mean(MSQD_recruitment, na.rm=TRUE),
            LANDED_WEIGHT_MTONS = sum(LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
  spread(PACFIN_SPECIES_CODE, LANDED_WEIGHT_MTONS) %>% 
  dplyr::rename(Landings_PSDN = PSDN) %>% dplyr::rename(Landings_MSQD = MSQD) %>% 
  dplyr::rename(Landings_NANC = NANC)

area_names <- as_labeller(c(`LAA` = "Los Angeles", `SBA` = "Santa Barbara",
                            `MNA` = "Monterey", `CLO` = "Columbia River (OR)", 
                            `CWA` = "Washington Coastal Ports"))

# Create sardine plot
sdm.by.species.PSDN <- sdm.by.species %>% 
  filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "CLO") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "LAA", "MNA", "CLO")) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE), 
            Landings_PSDN = sum(Landings_PSDN, na.rm = TRUE))
coeff1 <- 60000
g1 <- ggplot(sdm.by.species.PSDN) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_PSDN), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = PSDN_SDM_60*coeff1), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "MNA", "CLO")), labeller = area_names,  ncol = 4) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff1, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(a) Pacific sardine (60 km radius)")


# Create anchovy plot
sdm.by.species.NANC <- sdm.by.species %>% 
  filter(PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA") %>% 
  mutate(PORT_AREA_CODE = fct_relevel(PORT_AREA_CODE, "SBA", "MNA", "CWA")) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(NANC_SDM_20 = mean(NANC_SDM_20, na.rm = TRUE), 
            Landings_NANC = sum(Landings_NANC, na.rm = TRUE))
coeff2 <- 12000
g2 <-  ggplot(sdm.by.species.NANC) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_NANC, color = "Landings"), size = 0.5) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = NANC_SDM_20*coeff2, color = "Probability of presence"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("SBA", "MNA", "CWA")), labeller = area_names,  ncol = 4) + 
  scale_x_continuous(name = "Year")  +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff2, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8),
        legend.position="bottom") + 
  ggtitle("(b) Northern anchovy (20 km radius)") +  
  scale_color_manual(name = "Variable: ", 
                     values = c("Landings" = "grey", "Probability of presence" = "blue"))

# Plot Sardine and Anchovy
g1 / g2


# Plot all squid SDMs outputs

sdm.by.species$Date <- zoo::as.yearmon(paste(sdm.by.species$LANDING_YEAR, sdm.by.species$LANDING_MONTH, sep='-'))

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
            Landings_MSQD = sum(Landings_MSQD, na.rm = TRUE))


coeff3 <- 500000
g3 <-  ggplot(sdm.by.species.MSQD) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SPAWN_SDM_90_v2*coeff3), 
            size = 0.5, linetype = "dashed", color = "blue") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
  scale_x_continuous(name = element_blank(), labels = NULL)  +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff3, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="bottom") + 
  ggtitle("(a) Market squid (spawning aggregation model; 90 km radius; August - October)") 

coeff4 <- 100000
ggplot(sdm.by.species.MSQD) + 
  geom_line(mapping = aes(x = Date, y = Landings_MSQD, color = "Landings"), size = 0.5) +
  geom_line(mapping = aes(x = Date, y = MSQD_SPAWN_SDM_90*coeff4, color = "SDM output"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 1) + 
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff4, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") + 
  ggtitle("(b) Market squid (spawning aggregation model; 90 km radius)") +  
  scale_color_manual(name = "Variable: ", 
                     values = c("Landings" = "grey", "SDM output" = "blue"))

coeff4 <- 100000
ggplot(sdm.by.species.MSQD) + 
  geom_line(mapping = aes(x = Date, y = Landings_MSQD, color = "Landings"), size = 0.5) +
  geom_line(mapping = aes(x = Date, y = MSQD_SDM_90*coeff4, color = "SDM output"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 1) + 
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff4, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="right") + 
  ggtitle("(b) Market squid (90 km radius)") +  
  scale_color_manual(name = "Variable: ", 
                     values = c("Landings" = "grey", "SDM output" = "blue"))

coeff5 <- 8000
g5 <-  ggplot(sdm.by.species.MSQD) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD), size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_SDM_90_JS_cpue*coeff5), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff5, name = "Abundance (CPUE)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(c) Market squid (Justin Suca's abundance; August - October; 90 km radius)")


coeff6 <- 10000
g6 <- ggplot(sdm.by.species.MSQD) + 
  geom_line(mapping = aes(x = LANDING_YEAR, y = Landings_MSQD, color = "Landings"), size = 0.5) +
  geom_line(mapping = aes(x = LANDING_YEAR, y = MSQD_recruitment*coeff6, color = "SDM output"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ factor(PORT_AREA_CODE, levels=c("LAA", "SBA", "MNA")), labeller = area_names,  ncol = 3) + 
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff6, name = "Index")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(d) Market squid (Recrutiment index)") +  
  scale_color_manual(name = "Variable: ", 
                     values = c("Landings" = "grey", "SDM output" = "blue"))

g3 / g4
g5 / g6

# rm(g1, g2, g3, g4, g5, g6, sdm.by.species, coeff1, coeff2, coeff3, coeff4, coeff5, coeff6 , area_names, landing.price.year.sel)
#  

#---------------------------------------------------------------------------------------------------------
## Figure 7?
# I wonder if this perception is due to the fact that some vessels actually changed 
# their catch composition, but that something else was limiting landings - perhaps availability. 
# Could you plot annual squid landings relative to total annual landings the above for the CPS LE vessels?

## Calculate average price and total landings by species per month
landing.year <- summaryBy(LANDED_WEIGHT_MTONS.sum ~ PACFIN_SPECIES_CODE + LANDING_YEAR, FUN=sum_mean_fun, data=PacFIN.month) %>% 
  dplyr::select("LANDED_WEIGHT_MTONS.sum.sum", "PACFIN_SPECIES_CODE", "LANDING_YEAR")
  landing.year.total <- landing.year %>% group_by(LANDING_YEAR) %>% summarise(total_year = sum(LANDED_WEIGHT_MTONS.sum.sum))

# Graph landings v/s number of vessels
df <- landing.year %>% merge(landing.year.total, by = c("LANDING_YEAR"), all.x = TRUE) %>% 
  filter(PACFIN_SPECIES_CODE == "MSQD") %>% mutate(relative_landings = LANDED_WEIGHT_MTONS.sum.sum/total_year) %>%
  dplyr::select("LANDING_YEAR", "relative_landings")

ggplot(df, aes(x=LANDING_YEAR, y = relative_landings)) + geom_line(size=1) +
  scale_y_percent(name = "% of total landings") 

rm(df, landing.year, landing.year.total)



#---------------------------------------
### Other Figures ###
#
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
#
#
#
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
#
#
#
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
#
#
#
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

#------------------------------------------------------------------------
### Switching observed ###
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
# # # If a vessel harvest Pacific sardine in a year, how many also harvest squid.
# # squid.cond.psdn <- PacFIN %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
# #   select(VESSEL_NUM, PACFIN_SPECIES_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, LANDED_WEIGHT_MTONS) %>%
# #   merge(psdn.landings.by.day.vessel, by = c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), all.x = TRUE) %>%
# #   filter(harv.psdn == 1) %>% filter(PACFIN_SPECIES_CODE.x == "MSQD") %>%
# #   mutate(perc_land_squid = LANDED_WEIGHT_MTONS / (LANDED_WEIGHT_MTONS + Landed_PSDN))
# #
# # write.csv(squid.cond.psdn,"C:\\Data\\day_switch_land.csv", row.names = FALSE)
# df <- read.csv(file = "C:\\Data\\day_switch_land.csv")
# hist(df$perc_land_squid, col = 'skyblue3', breaks = 10)
# plot.ecdf(df$perc_land_squid)
# 
# 
# ## Switching_day
# 
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
# 
# 
# ## Switching graph
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
# 
# 
# 
# ## Switching_month
# 
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