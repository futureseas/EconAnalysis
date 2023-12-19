################################
## Annual participation model ##
################################

### WORK TO DO: Add SDMs


library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

rm(list = ls())
gc()

## Read data (vessel has more than 20 trips in a year to be considered active)
annual.part <- readRDS(file = "C:\\Data\\PacFIN data\\vessel_part_year.RDS")
  

# Expand data 
annual.part <- complete(annual.part, VESSEL_NUM, set_year) %>%
mutate(active_year = ifelse(is.na(active_year), 0, active_year)) %>%
mutate(prev_years_active = RcppRoll::roll_sum(active_year, 5, fill = NA, align = "right", na.rm = TRUE))  %>%
filter(set_year >= 2005, set_year <= 2020) 
  
## Filter data
annual.part <- annual.part %>%  
  group_by(VESSEL_NUM) %>%
  mutate(n_years = sum(active_year)) %>% ungroup() %>%
  filter(n_years >= 5)

# ## Check how many years each vessel participate
# n_years_active <- annual.part %>% group_by(VESSEL_NUM) %>%
#   summarize(n_years = sum(active_year))
# n_vessels_per_year <- annual.part %>% group_by(set_year) %>%
#   summarize(n_years = sum(active_year))

# # Plor participation
# ggplot(annual.part.exp, aes(x=set_year, y=VESSEL_NUM, color=active_year)) +
#   geom_point(size=1)



#################################
### Add explanatory variables ###
#################################


## Add closure
annual.part <- annual.part %>%
  dplyr::mutate(PSDN.Closure = 
    ifelse(set_year >= 2015, 1, 0))

## Add center of gravity
raw_inputs <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\RAW_cluster_inputs.csv")
annual.part <- merge(annual.part, raw_inputs, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
  dplyr::filter(group_all == 4) %>% mutate(PSDN.Closure = as.factor(PSDN.Closure))


## Diversity index
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)
HHI<-aggregate(AFI_EXVESSEL_REVENUE~ PACFIN_SPECIES_COMMON_NAME + VESSEL_NUM, data=Tickets, FUN=sum) 
HHI<-reshape2::dcast(HHI, VESSEL_NUM ~ PACFIN_SPECIES_COMMON_NAME, value.var="AFI_EXVESSEL_REVENUE", fill=0)
rownames(HHI) <- HHI[,1]
HHI <- HHI[,-1]
HHI<-as.data.frame(vegan::diversity(HHI, index="invsimpson"))
HHI$VESSEL_NUM <- rownames(HHI)
names(HHI)<-c("diversity_all", "VESSEL_NUM")
HHI$diversity[which(!is.finite(HHI$diversity_all))] <- 0
annual.part <- merge(annual.part, HHI, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) 


#############
## Add SDM ##
#############

#### Merge data with SDM Pacific Sardine
SDM_PSDN <- read.csv(file = here::here("Landings", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  group_by(LANDING_YEAR) %>% summarize(lagged_PSDN_SDM_60 = mean(SDM_60, na.rm = TRUE)) %>%
  rename(set_year = LANDING_YEAR) %>% mutate(set_year = set_year + 1)

SDM_MSQD <- read.csv(file = here::here("Landings", "SDM", "MSQD_SDM_port_month.csv"))%>% 
  group_by(LANDING_YEAR) %>% summarize(lagged_MSQD_SDM_90 = mean(SDM_90, na.rm = TRUE)) %>%
  rename(set_year = LANDING_YEAR) %>% mutate(set_year = set_year + 1)

SDM_MSQD_Spawn <- read.csv(file = here::here("Landings", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  group_by(LANDING_YEAR) %>% summarize(lagged_MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90, na.rm = TRUE)) %>%
  rename(set_year = LANDING_YEAR) %>% mutate(set_year = set_year + 1)

SDM_NANC <- read.csv(file = here::here("Landings", "SDM", "NANC_SDM_port_month.csv"))%>% 
  group_by(LANDING_YEAR) %>% summarize(lagged_NANC_SDM_20 = mean(SDM_20, na.rm = TRUE)) %>%
  rename(set_year = LANDING_YEAR) %>% mutate(set_year = set_year + 1)

annual.part <- merge(annual.part, SDM_PSDN, by = c("set_year"), all.x = TRUE)
annual.part <- merge(annual.part, SDM_NANC, by = c("set_year"), all.x = TRUE)
annual.part <- merge(annual.part, SDM_MSQD, by = c("set_year"), all.x = TRUE) 
annual.part <- merge(annual.part, SDM_MSQD_Spawn, by = c("set_year"), all.x = TRUE) 


######################
### Estimate model ###
######################
library(brms)

set.seed(1234)

logit <- brm(active_year ~ prev_years_active + 
               PSDN.Closure + 
               lagged_PSDN_SDM_60 + 
               lagged_MSQD_SDM_90 + 
               lagged_NANC_SDM_20 +
               diversity_all +
               (1 | VESSEL_NUM), 
            data = annual.part, 
            seed = 123,
            family = bernoulli(link = "logit"), 
            warmup = 500, 
            iter = 2000,
            chain = 1, cores = 4)

summary(logit)
plot(conditional_effects(logit), points = TRUE)


### Obtain AUC
library(tidybayes)
library(ROCR)

Prob <- predict(logit, type="response")
Prob <- Prob[,1]
Pred <- ROCR::prediction(Prob, as.vector(pull(annual.part, active_year)))
AUC <- ROCR::performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

# 97.7% already!
