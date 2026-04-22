##                          ##
# Annual participation model #
##                          ##  

library(tidyverse)
library(data.table)
library(raster)
library(doParallel)
library(brms)
library(tidybayes)
library(ROCR)
library(zoo)


rm(list = ls())
gc()
setwd("C:/GitHub/EconAnalysis/FuturePredictions")


#---- Read data (vessel has more than 20 trips in a year to be considered active) ----

c4 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c4_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 4) %>% filter(selection != "No-Participation")
c5 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c5_full_v2_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 5) %>% dplyr::select(-c(DCRB.Closure.WA)) %>%
  filter(selection != "No-Participation")
c6 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c6_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 6)  %>% filter(selection != "No-Participation")
c7 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c7_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 7)  %>% filter(selection != "No-Participation")

annual.part <- rbind(c4, c5, c6, c7)

rm(c4, c5, c6, c7)


#---- Descriptive statistics ---
#
# ## ---- Check how many years each vessel participate ----
#
# n_years_active <- annual.part %>% group_by(fished_VESSEL_anon, set_year, group_all) %>%
#   summarize(total_catch = sum(mean_catch)) %>% dplyr::filter(total_catch > 0) %>%
#   mutate(active_year = 1) %>%
#   group_by(fished_VESSEL_anon, group_all) %>%
#   summarize(n_years = sum(active_year))
#
# ## ---- Plot participation ---
# n_years_active <- annual.part %>%
#   group_by(fished_VESSEL_anon, set_year, group_all) %>%
#   summarize(total_catch = sum(mean_catch), .groups = "drop") %>%
#   filter(total_catch > 0) %>%
#   mutate(
#     active_year = 1,
#     vessel_group_id = paste(fished_VESSEL_anon, group_all, sep = "_")
#   )
#
# ggplot(n_years_active, aes(x=set_year, y=vessel_group_id, color=active_year)) +
#   geom_point(size=1)


#---- Add new SDMs (GAM Boost) ----
MSQD_sdm_data_MA <- read.csv("SDM/Historical/MSQD_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_MSQD = mean(SDM_90, na.rm = TRUE)) %>% as.data.frame()

PSDN_sdm_data_MA <- read.csv("SDM/Historical/PSDN_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_PSDN = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

NANC_sdm_data_MA <- read.csv("SDM/Historical/NANC_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_NANC = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

CMCK_sdm_data_MA <- read.csv("SDM/Historical/CMCK_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_CMCK = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

JMCK_sdm_data_MA <- read.csv("SDM/Historical/JMCK_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_JMCK = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()


## ---- Prepare the long_data by extracting port codes from selection names ----

annual.part <- annual.part %>%
  mutate(
    SPECIES = toupper(substr(selection, 5, 8)),
    date = as.Date(set_date)
  )


## ---- Prepare database adding general variables ----


vessel_vars <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  group_by(VESSEL_NUM) %>%
  summarize(
    lat_cgo = mean(lat_cg, na.rm = TRUE),
    group_all = mean(group_all, na.rm = TRUE),
    length = mean(Vessel.length, na.rm = TRUE),
    .groups = "drop"
  )

general_vars <- annual.part %>%
  group_by(set_year) %>%
  summarize(
    diesel_price = mean(diesel_price, na.rm = TRUE),
    unem_rate = mean(unem_rate, na.rm = TRUE),
    fishmeal_price = mean(Price.Fishmeal.AFI, na.rm = TRUE),
    .groups = "drop"
  )

prices_vars <- annual.part %>%
  group_by(set_year, SPECIES) %>%
  summarize(
    mean_price = mean(mean_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(SPECIES %in% c("PSDN", "MSQD", "NANC", "CMCK"))
prices_vars <- bind_rows(prices_vars, tibble(
  set_year = 2017,
  SPECIES = "PSDN",
  mean_price = 167
))
prices_wide <- prices_vars %>%
  pivot_wider(
    names_from = SPECIES,
    values_from = mean_price,
    names_prefix = "mean_price_"
  )

all_vessel_years <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  distinct(VESSEL_NUM) %>%
  crossing(set_year = seq(min(annual.part$set_year), max(annual.part$set_year)))

vessel_part <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  group_by(VESSEL_NUM, set_year, SPECIES) %>%
  summarize(total_catch = sum(mean_catch, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SPECIES, values_from = total_catch) %>%
  dplyr::select(VESSEL_NUM, set_year) %>%
  mutate(year_participation = 1)

vessel_part_expanded <- all_vessel_years %>%
  left_join(vessel_part, by = c("VESSEL_NUM", "set_year")) %>%
  mutate(year_participation = ifelse(is.na(year_participation), 0, year_participation))


database <- left_join(vessel_part_expanded, general_vars, by = c("set_year"))
database <- left_join(database, prices_wide, by = c("set_year"))
database <- left_join(database, vessel_vars, by = c("VESSEL_NUM"))


## ---- Merge SDM data ----
database <- database %>%
  left_join(MSQD_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(PSDN_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(NANC_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(CMCK_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR"))

## ---- Add closure ----
database <- database %>%
  mutate(PSDN_closure = ifelse(set_year>2015, 1, 0)) 

## ---- Lagged values  ----
database <- database %>%
  arrange(group_all, VESSEL_NUM, set_year) %>%
  group_by(group_all, VESSEL_NUM) %>%
  mutate(
    lag_fishmeal_price = lag(fishmeal_price),
    lag_diesel_price = lag(diesel_price),
    lag_unem_rate = lag(unem_rate),
    lag_mean_price_MSQD = lag(mean_price_MSQD),
    lag_mean_price_PSDN = lag(mean_price_PSDN),
    lag_mean_price_NANC = lag(mean_price_NANC),
    lag_mean_price_CMCK = lag(mean_price_CMCK),
    lag_mean_avail_MSQD = lag(mean_avail_MSQD),
    lag_mean_avail_PSDN = lag(mean_avail_PSDN),
    lag_mean_avail_NANC = lag(mean_avail_NANC),
    lag_mean_avail_CMCK = lag(mean_avail_CMCK)
  ) %>%
  ungroup()



#---- Estimate model ----

rm(list = setdiff(ls(), "database"))
saveRDS(database, "annual_data_part.RDS")
database <- readRDS("annual_data_part.RDS") %>% filter(!is.na(lag_unem_rate)) %>%
  group_by(group_all) %>%
  mutate(across(
    c(starts_with("lag_"), length, lat_cgo, fishmeal_price),
    ~ scale(.)[, 1]
  )) %>%
  ungroup()

priors <- c(
  set_prior("normal(0, 1)", class = "b"),  # weakly informative
  set_prior("normal(1, 0.5)", class = "b", coef = "lag_mean_avail_MSQD"),
  set_prior("normal(1, 0.5)", class = "b", coef = "lag_mean_avail_PSDN"),
  set_prior("normal(0.5, 0.5)", class = "b", coef = "lag_fishmeal_price"),
  set_prior("normal(-0.5, 0.5)", class = "b", coef = "PSDN_closure"),
  set_prior("normal(0, 2)", class = "Intercept")
)

# library(corrplot)
# corr_vars <- database %>%
#   dplyr::select(starts_with("lag_"), diesel_price, unem_rate, length, lat_cgo) %>%
#   na.omit()
# corrplot::corrplot(cor(corr_vars), method = "circle")


annual_logit <- brm(year_participation ~ 
                      unem_rate + 
                      lag_fishmeal_price +
                      lag_mean_avail_MSQD + lag_mean_avail_PSDN + 
                      lag_mean_avail_MSQD:lag_mean_avail_PSDN + 
                      length + lat_cgo + 
                      PSDN_closure + (1 + PSDN_closure | group_all),
                    data = database, 
                    seed = 123, 
                    family = bernoulli(link = "logit"), 
                    warmup = 1000,
                    iter = 3000, 
                    chains = 4, 
                    cores = 4,
                    control = list(adapt_delta = 0.999), 
                    prior = priors)
            summary(annual_logit)
            
            saveRDS(annual_logit, "annual_logit.RDS")
           
         
##---- Check model ----
pp_check(annual_logit)
shinystan::launch_shinystan(annual_logit)


##---- How accurate the model is? 81% ----

Prob <- predict(annual_logit, type = "response")[, 1]
true_labels <- database$year_participation
Pred <- ROCR::prediction(Prob, true_labels)
AUC <- ROCR::performance(Pred, measure = "auc")
AUC_value <- AUC@y.values[[1]]
print(paste("AUC:", round(AUC_value, 3)))

 
##---- Plot marginal effects ----    
conditional_effects(annual_logit)


