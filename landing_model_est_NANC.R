###########################
### NANC Landing model ###
###########################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


## Read packages 
library("brms")
library("sjPlot")
library("sjlabelled")
library("sjmisc")
library("insight")
library("httr")
library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")
library('doBy')

## Read dataset
dataset <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation.csv")

#-----------------------------------------------
## Create dataset for estimation and run landing models 

### Market squid ###

#### Select data for estimation, replace N/A landings to zero ####
dataset_nanc <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_z,
                NANC_Landings, 
                NANC_Price, NANC_Price_z, 
                MSQD_Price, MSQD_Price_z, 
                PSDN_Price, PSDN_Price_z, 
                PSDN_SDM_60, PSDN_SDM_60_z,
                NANC_SDM_20, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open,
                Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                Length, Length_z) %>% 
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, NANC_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.0001, 0, NANC_Landings)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_NANC_Landings = log(NANC_Landings)) %>%
  filter(group_all == 6 | group_all == 7) %>%
  filter(PORT_AREA_CODE != "CLO") %>% filter(PORT_AREA_CODE != "CLW") %>%
  drop_na()


#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_nanc$port_ID            <- factor(dataset_nanc$PORT_AREA_CODE)
dataset_nanc$cluster            <- factor(dataset_nanc$group_all)
class(dataset_nanc$port_ID)
class(dataset_nanc$cluster)
class(dataset_nanc$PSDN.Total.Closure)

dataset_nanc_landing <- dataset_nanc %>%
  dplyr::filter(NANC_Landings > 0)

# #### Check number of observation by port area ####
n_obs_port_area <- dataset_nanc_landing %>% group_by(PORT_AREA_CODE) %>%
  summarize(n_obs = n()) %>% mutate(per = scales::percent(n_obs / sum(n_obs)))

### Descriptive statistics 
desc_data <- dataset_nanc_landing %>%
  subset(select = c(Length, NANC_Landings, NANC_Price, 
                    MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,  
                    Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fishmeal price", vars)) 

gs4_create("SummaryMonthly_Q_NANC", sheets = table)
rm(desc_data, table)


#-------------------------------------------------------------------
# ## Check stationarity in the panel dataset
library("plm")

dataset_nanc_landing$Date <-
  zoo::as.yearmon(paste(dataset_nanc_landing$LANDING_YEAR, dataset_nanc_landing$LANDING_MONTH), "%Y %m")

pDataset <- dataset_nanc_landing %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>%
  group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group >= 12) %>% drop_na()
pDataset <- pdata.frame(pDataset, index = c('Unique_ID', 'Date'))

# duplicate_indexes <- dataset_msqd %>%
#   group_by(PORT_AREA_CODE, Date, VESSEL_NUM) %>% mutate(dupe = n()>1)

purtest(pDataset$NANC_Landings, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$NANC_Price, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$NANC_SDM_20, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)

## -------------------------------------------------------------------
### Market squid landing model ###
write.csv(dataset_nanc_landing,"C:\\Data\\PacFIN data\\dataset_estimation_NANC.csv", row.names = FALSE)
price_model <- bf(NANC_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))

# Model 1: Original
# Model 2: Relative abundance.
# Model 3: Relative abundance and relative prices?



# ### MODEL 1 ###
# 
# ## Define landing equation
# landing_model_MODEL1 <- bf(log(NANC_Landings) ~
#   1 + NANC_SDM_20_z + NANC_Price_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z:PSDN_SDM_60_z:PSDN.Open + MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:PSDN.Open + PSDN.Total.Closure + Length_z +
#  (1 + NANC_SDM_20_z + NANC_Price_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z:PSDN_SDM_60_z:PSDN.Open + MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:PSDN.Open + PSDN.Total.Closure | cluster) +
#  (1 + NANC_SDM_20_z + NANC_Price_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z:PSDN_SDM_60_z:PSDN.Open + MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:PSDN.Open | port_ID))
# 
# ## Create priors
# get_prior(data = dataset_nanc_landing,
#            family = gaussian,
#            price_model + landing_model_MODEL1 + set_rescor(TRUE))
# prior_lognormal_MODEL1 <- c(
#   prior(lognormal(0,1), class = b,     resp = NANCPricez,      coef = Price.Fishmeal.AFI_z),
#   prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = Length_z),
#   prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = NANC_Price_z),
#   prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = NANC_SDM_20_z),
#   prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = NANC_SDM_20_z:PSDN_SDM_60_z:PSDN.Open),
#   prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = NANC_SDM_20_z:MSQD.Open:MSQD_SPAWN_SDM_90_z),
#   prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = PSDN_SDM_60_z:PSDN.Open),
#   prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = MSQD.Open:MSQD_SPAWN_SDM_90_z),
#   prior(exponential(1), class = sigma, resp = NANCPricez),
#   prior(exponential(1), class = sigma, resp = logNANCLandings),
#   prior(lkj(2),         class = rescor))
# 
# ## Estimate model
# set.seed(123)
# fit_qNANC_MODEL1 <-
#   brm(data = dataset_nanc_landing,
#       family = gaussian,
#       price_model + landing_model_MODEL1 + set_rescor(TRUE),
#       prior = prior_lognormal_MODEL1,
#       iter = 2000, warmup = 1000, chains = 4, cores = 4,
#       control = list(max_treedepth = 15, adapt_delta = 0.99),
#       file = "Estimations/fit_qNANC_MODEL1")
# 
# fit_qNANC_MODEL1 <- add_criterion(fit_qNANC_MODEL1, "loo", overwrite = TRUE)


### MODEL 2 ###

## Define landing equation
landing_model_MODEL2 <- bf(log(NANC_Landings) ~
  b1 + b2 * NANC_SDM_20_z + b3 * NANC_Price_z + b4 * (NANC_SDM_20_z/MSQD_SPAWN_SDM_90_z) * MSQD.Open +
  b5 * (NANC_SDM_20_z/PSDN_SDM_60_z) * PSDN.Open + b8 * PSDN.Total.Closure + b9 * Length_z,
  b1 ~ 1 + (1 | cluster) + (1 | port_ID),
  b2 ~ 1 + (1 | cluster) + (1 | port_ID),
  b3 ~ 1 + (1 | cluster) + (1 | port_ID),
  b4 ~ 1 + (1 | cluster) + (1 | port_ID),
  b5 ~ 1 + (1 | cluster) + (1 | port_ID),
  b8 ~ 1 + (1 | cluster),
  b9 ~ 1,
  nl = TRUE)

## Create priors
get_prior(data = dataset_nanc_landing,
          family = gaussian,
          landing_model_MODEL2)
prior_lognormal_MODEL2 <- c(
  prior(lognormal(0,1), nlpar = b9),
  prior(lognormal(0,1), nlpar = b2),
  prior(lognormal(0,1), nlpar = b3),
  prior(normal(0,1),    nlpar = b4),
  prior(normal(0,1),    nlpar = b5),
  prior(exponential(1), class = sigma, resp = NANCPricez),
  prior(exponential(1), class = sigma, resp = logNANCLandings),
  prior(lkj(2),         class = rescor))
# prior(lognormal(0,1), class = b,     resp = NANCPricez,      coef = Price.Fishmeal.AFI_z),
# prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = Length_z),


## Estimate model
set.seed(123)
fit_qNANC_MODEL2 <-
  brm(data = dataset_nanc_landing,
      family = gaussian,
      price_model + landing_model_MODEL2 + set_rescor(TRUE),
      prior = prior_lognormal_MODEL2,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(max_treedepth = 15, adapt_delta = 0.99),
      file = "Estimations/fit_qNANC_MODEL2")

fit_qNANC_MODEL2 <- add_criterion(fit_qNANC_MODEL2, "loo", overwrite = TRUE)



### MODEL 3 ###

## Define landing equation
landing_model_MODEL3 <- bf(log(NANC_Landings) ~
  b1 + b2 * NANC_SDM_20_z + b4 * (NANC_SDM_20_z/MSQD_SPAWN_SDM_90_z) * MSQD.Open +
    b5 * (NANC_SDM_20_z/PSDN_SDM_60_z) * PSDN.Open + b6 * (NANC_Price_z/MSQD_Price_z) * MSQD.Open +
    b7 * (NANC_Price_z/PSDN_Price_z) * PSDN.Open + b8 * PSDN.Total.Closure + b9 * Length_z,
  b1 ~ 1 + (1 | cluster) + (1 | port_ID),
  b2 ~ 1 + (1 | cluster) + (1 | port_ID),
  b4 ~ 1 + (1 | cluster) + (1 | port_ID),
  b5 ~ 1 + (1 | cluster) + (1 | port_ID),
  b6 ~ 1 + (1 | cluster) + (1 | port_ID),
  b7 ~ 1 + (1 | cluster) + (1 | port_ID),
  b8 ~ 1 + (1 | cluster),
  b9 ~ 1,
  nl = TRUE)

get_prior(data = dataset_nanc_landing,
          family = gaussian,
          landing_model_MODEL3)

## Create priors
prior_lognormal_MODEL3 <- c(
  prior(lognormal(0,1), nlpar = b9),
  prior(lognormal(0,1), nlpar = b2),
  prior(normal(0,1),    nlpar = b4),
  prior(normal(0,1),    nlpar = b5),
  prior(normal(0,1),    nlpar = b6),
  prior(normal(0,1),    nlpar = b7),
  prior(exponential(1), class = sigma))


## Estimate model
set.seed(123)
fit_qNANC_MODEL3 <-
  brm(data = dataset_nanc_landing,
      family = gaussian,
      landing_model_MODEL3,
      prior = prior_lognormal_MODEL3,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(max_treedepth = 15, adapt_delta = 0.99),
      file = "Estimations/fit_qNANC_MODEL3")

fit_qNANC_MODEL3 <- add_criterion(fit_qNANC_MODEL3, "loo", overwrite = TRUE)


