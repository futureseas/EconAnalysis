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
  dplyr::filter(group_all == 4 | group_all == 5) %>% mutate(PSDN.Closure = as.factor(PSDN.Closure))


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



######################
### Estimate model ###
######################
library(brms)

set.seed(1234)

logit <- brm(active_year ~ LAT + DISTANCE_A + PSDN.Closure + prev_years_active + diversity_all +
               (1 + PSDN.Closure | group_all) + 
               (1 | VESSEL_NUM), 
            data = annual.part, 
            seed = 123,
            family = bernoulli(link = "logit"), 
            warmup = 750, 
            iter = 2000,
            chain = 2, cores = 4)

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

# 92% already!
