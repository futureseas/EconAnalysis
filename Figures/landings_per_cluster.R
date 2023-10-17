#################################
### Fishery operation results ###
#################################

rm(list = ls(all.names = TRUE)) 
gc()
# load("stan_fit.RData")

#---------------------------
### Load packages
library(brms)
library(cluster)
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
library(rstan)
library(scales)
library(sjlabelled)
library(tidyr)
library(tidyverse)
library(viridis)
library(zoo)

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

landings.year <-  PacFIN.month.CPS %>% group_by(PACFIN_SPECIES_CODE, LANDING_YEAR, group_all) %>%
  summarize(Landings = sum(LANDED_WEIGHT_MTONS.sum)) %>% group_by(PACFIN_SPECIES_CODE, group_all) %>%
  summarize(Landings.annual.mean = sum(Landings)) %>% filter(group_all !=8)
  
cond_label <- as_labeller(c("1" = "Southern CCS small-scale\nsquid-specialists",
                            "2" = "Southern CCS small-scale\nCPS-opportunists",
                            "3" = "PNW sardine\nopportunists",
                            "4" = "Southern CCS industrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid generalists",
                            "6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish diverse",
                            "8" = "PNW albacore-crab\ngeneralists"))


ggplot(landings.year, aes(fill =  PACFIN_SPECIES_CODE, y=Landings.annual.mean , x=PACFIN_SPECIES_CODE)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ group_all, labeller = cond_label, scale="free", space="free_x") + 
  theme(strip.text.x = element_text(size = 9), axis.ticks.x=element_blank()) +
  theme(legend.position="bottom") + 
  theme(axis.title = element_text(size = 9)) +
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
                                            "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
                                            "OTHER" = "Non-CPS")) +  xlab("") + 
  ylab("Average annual landings (tons)") + guides(fill=guide_legend(title="Species: "))  + 
  scale_x_discrete(labels= element_blank() ) +
  scale_color_brewer(palette="Set2")
