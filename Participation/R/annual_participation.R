################################
## Annual participation model ##
################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

rm(list = ls())
gc()

## Read data (vessel has more than 20 trips in a year to be considered active)
annual.part <- readRDS(file = "C:\\Data\\PacFIN data\\vessel_part_year.RDS")

## Filter data
annual.part <- annual.part %>%  filter(set_year >= 2005, set_year <= 2020) %>% 
  group_by(VESSEL_NUM) %>%
  mutate(n_years = sum(active_year)) %>% ungroup() %>%
  filter(n_years >= 5)

## Check how many years each vessel participate
n_years_active <- annual.part %>% group_by(VESSEL_NUM) %>%
  summarize(n_years = sum(active_year))
n_vessels_per_year <- annual.part %>% group_by(set_year) %>%
  summarize(n_years = sum(active_year))

# Expand data 
annual.part <- complete(annual.part, VESSEL_NUM, set_year) %>%
  mutate(active_year = ifelse(is.na(active_year), 0, active_year))

# # Plor participation
# ggplot(annual.part.exp, aes(x=set_year, y=VESSEL_NUM, color=active_year)) +
#   geom_point(size=1)



#################################
### Add explanatory variables ###
#################################


## Add closure
annual.part <- annual.part %>%
  dplyr::mutate(PSDN.Closure = 
    ifelse(set_year >= 2005, 1, 0))

## Add center of gravity
raw_inputs <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\RAW_cluster_inputs.csv")
annual.part <- merge(annual.part, raw_inputs, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)



######################
### Estimate model ###
######################
library(brms)

set.seed(1234)

logit <- brm(active_year ~ LAT + (1 | group_all), 
            data = annual.part,
            family = bernoulli,
            cluster = 4)






