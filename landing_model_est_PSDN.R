###########################
### PSDN Landing model ###
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
dataset_psdn <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_z,
                PSDN_Landings, 
                PSDN_Price, PSDN_Price_z, 
                PSDN_SDM_60, NANC_SDM_20, 
                DCRB_LANDING, DCRB_LANDING_z,
                PSDN_SDM_60_z, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open,
                Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                Length, Length_z) %>% 
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, PSDN_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.0001, 0, PSDN_Landings)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(WA.Restriction = ifelse(LANDING_MONTH <= 3, 1, 0)) %>%
  dplyr::mutate(ln_PSDN_Landings = log(PSDN_Landings)) %>%
  filter(group_all == 1 | group_all == 3 | group_all == 4 | group_all == 5 | group_all == 6 | group_all == 7) %>%
  filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA"  | 
           PORT_AREA_CODE == "CLO"  | PORT_AREA_CODE == "CWA"  | PORT_AREA_CODE == "CLW" | 
           PORT_AREA_CODE == "SDA") %>% drop_na()
 

#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_psdn$port_ID            <- factor(dataset_psdn$PORT_AREA_CODE)
dataset_psdn$cluster            <- factor(dataset_psdn$group_all)
class(dataset_psdn$port_ID)
class(dataset_psdn$cluster)


dataset_psdn_landing <- dataset_psdn %>%
  dplyr::filter(PSDN_Landings > 0) %>%
  dplyr::filter(PSDN.Open == 1) %>%
  dplyr::filter(PSDN.Total.Closure == 0) 


#### Check number of observation by port area ####
n_obs_port_area <- dataset_psdn_landing %>% group_by(PORT_AREA_CODE) %>%
  summarize(n_obs = n()) %>% mutate(per = scales::percent(n_obs / sum(n_obs)))


### Descriptive statistics 
desc_data <- dataset_psdn_landing %>%
  subset(select = c(Length, PSDN_Landings, PSDN_Price, 
                    MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,DCRB_LANDING,
                    MSQD.Open, Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Landings: DCRB", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fraction of month open: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Fishmeal price", vars)) 

# gs4_create("SummaryMonthly_Q_PSDN", sheets = table)
rm(desc_data, table)


#-------------------------------------------------------------------
# ## Check stationarity in the panel dataset
library("plm")

dataset_psdn_landing$Date <-
  zoo::as.yearmon(paste(dataset_psdn_landing$LANDING_YEAR, dataset_psdn_landing$LANDING_MONTH), "%Y %m")

pDataset <- dataset_psdn_landing %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>%
  group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group >= 12) %>% drop_na()
pDataset <- pdata.frame(pDataset, index = c('Unique_ID', 'Date'))

# duplicate_indexes <- dataset_msqd %>%
#   group_by(PORT_AREA_CODE, Date, VESSEL_NUM) %>% mutate(dupe = n()>1)

purtest(pDataset$PSDN_Landings, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$PSDN_Price, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$NANC_SDM_20, pmax = 4, exo = "intercept", test = "Pm")
# purtest(pDataset$DCRB_LANDING, pmax = 1, exo = "intercept", test = "Pm")
purtest(pDataset$Price.Fishmeal.AFI, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)


## -------------------------------------------------------------------
### Market squid landing model ###
write.csv(dataset_psdn_landing,"C:\\Data\\PacFIN data\\dataset_estimation_PSDN.csv", row.names = FALSE)


price_model   <- bf(PSDN_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(PSDN_Landings) ~
                       1 + PSDN_SDM_60_z + PSDN_Price_z + PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:NANC_SDM_20_z + PSDN_SDM_60_z:DCRB_LANDING_z + MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z  + DCRB_LANDING_z + WA.Restriction + Length_z +
                      (1 + PSDN_SDM_60_z + PSDN_Price_z + PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:NANC_SDM_20_z + PSDN_SDM_60_z:DCRB_LANDING_z + MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z  + DCRB_LANDING_z + WA.Restriction | cluster) +
                      (1 + PSDN_SDM_60_z + PSDN_Price_z + PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z:MSQD.Open + PSDN_SDM_60_z:NANC_SDM_20_z + PSDN_SDM_60_z:DCRB_LANDING_z + MSQD_SPAWN_SDM_90_z:MSQD.Open + NANC_SDM_20_z  + DCRB_LANDING_z + WA.Restriction | port_ID))

get_prior(data = dataset_psdn_landing,
          family = gaussian,
          price_model + landing_model + set_rescor(TRUE))

# Create priors
prior_lognormal <- c(
  prior(lognormal(0,1), class = b,     resp = PSDNPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = Length_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_Price_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60_z),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60_z:NANC_SDM_20_z),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = NANC_SDM_20_z),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = MSQD_SPAWN_SDM_90_z:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60_z:DCRB_LANDING_z),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = DCRB_LANDING_z),
  prior(exponential(1), class = sigma, resp = PSDNPricez),
  prior(exponential(1), class = sigma, resp = logPSDNLandings),
  prior(lkj(2),         class = rescor))

set.seed(123)
fit_qPSDN_v2 <-
  brm(data = dataset_psdn_landing,
      family = gaussian,
      price_model + landing_model + set_rescor(TRUE),
      prior = prior_lognormal,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(max_treedepth = 15, adapt_delta = 0.99),
      file = "Estimations/fit_qPSDN_v2")

fit_qPSDN <- add_criterion(fit_qPSDN_v2, "loo", overwrite = TRUE)


