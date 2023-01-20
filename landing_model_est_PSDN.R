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
                PSDN_SDM_60_z, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open,
                Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price.AFI, diesel.price.AFI_z,
                Length, Length_z, 
                wages.AFI, wages.AFI_z) %>% 
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, PSDN_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.0001, 0, PSDN_Landings)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(WA.Restriction = ifelse(LANDING_MONTH <= 3, 1, 0)) %>%
  dplyr::mutate(ln_PSDN_Landings = log(PSDN_Landings)) %>%
  filter(group_all == 3 | group_all == 4 | group_all == 5 | group_all == 6 | group_all == 7) %>%
  mutate(cluster_port = paste(group_all, PORT_AREA_CODE, sep = "-", collapse = NULL)) %>%
  mutate(diesel.price.AFI_z = -1*diesel.price.AFI_z) %>%
  mutate(wages.AFI_z = -1*wages.AFI_z) %>%
  drop_na()
  
  
  # filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA"  | 
  #          PORT_AREA_CODE == "CLO"  | PORT_AREA_CODE == "CWA") %>% drop_na()
  # dataset_psdn %>% group_by(PORT_AREA_CODE) %>% summarize(n_freq = n()/nrow(dataset_psdn))


#### Convert variables to factor 
dataset_psdn$port_cluster_ID    <- factor(dataset_psdn$cluster_port)
dataset_psdn$port_ID            <- factor(dataset_psdn$PORT_AREA_CODE)
class(dataset_psdn$port_ID)
class(dataset_psdn$port_cluster_ID)


dataset_psdn_landing <- dataset_psdn %>%
  dplyr::filter(PSDN_Landings > 0) %>%
  dplyr::filter(PSDN.Open == 1) %>%
  dplyr::filter(PSDN.Total.Closure == 0) %>%
  mutate(n_total = n()) %>%
  group_by(cluster_port) %>% 
  mutate(obs = n(), perc = n()/n_total) %>% 
  filter(perc > 0.02) 

### Check number of observations
dataset_psdn_landing %>% select('cluster_port', 'obs', 'perc') %>% unique()
dataset_psdn_landing %>% group_by(PORT_AREA_CODE) %>%
  summarize(n_obs = n()) %>% mutate(per = scales::percent(n_obs / sum(n_obs)))


### Descriptive statistics
desc_data <- dataset_psdn_landing %>%
  subset(select = c(Length, PSDN_Landings, PSDN_Price,
                    MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,
                    MSQD.Open, Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fraction of month open: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Fishmeal price", vars))

# gs4_create("SummaryMonthly_Q_PSDN", sheets = table)
# rm(desc_data, table)

### Correlation between diesel price and fishmeal price
round(cor(dataset_psdn_landing$Price.Fishmeal.AFI, dataset_psdn_landing$diesel.price.AFI_z), 2)
plyr::ddply(dataset_psdn_landing, c("PORT_AREA_CODE"), summarise, cor = round(cor(Price.Fishmeal.AFI, diesel.price.AFI_z), 2))


#-------------------------------------------------------------------
## Check stationarity in the panel dataset
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
# purtest(pDataset$Price.Fishmeal.AFI, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)


## -------------------------------------------------------------------
### Sardine landing model ###

#### Check correlation
dataset_select <- dataset_psdn_landing %>% ungroup() %>% 
  dplyr::select(MSQD_SPAWN_SDM_90,
                PSDN_SDM_60,
                NANC_SDM_20,
                Length_z,
                PSDN_Price_z,
                Price.Fishmeal.AFI_z,
                diesel.price.AFI_z,
                wages.AFI_z,
                PSDN_Landings)
res <- as.data.frame(cor(dataset_select))
round(res, 2)


#### Estimate model
write.csv(dataset_psdn_landing,"C:\\Data\\PacFIN data\\dataset_estimation_PSDN.csv", row.names = FALSE)
price_model   <- bf(PSDN_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(PSDN_Landings) ~
                       1 + PSDN_SDM_60 + PSDN_Price_z + PSDN_SDM_60:MSQD_SPAWN_SDM_90:MSQD.Open + PSDN_SDM_60:NANC_SDM_20 + MSQD_SPAWN_SDM_90:MSQD.Open + NANC_SDM_20 + WA.Restriction + wages.AFI_z + Length_z +
                      (1 + PSDN_SDM_60 + PSDN_Price_z + PSDN_SDM_60:MSQD_SPAWN_SDM_90:MSQD.Open + PSDN_SDM_60:NANC_SDM_20 + MSQD_SPAWN_SDM_90:MSQD.Open + NANC_SDM_20 + WA.Restriction + wages.AFI_z | port_cluster_ID))

get_prior(data = dataset_psdn_landing,
          family = gaussian,
          price_model + landing_model + set_rescor(TRUE))

# Create priors
prior_lognormal <- c(
  prior(lognormal(0,1), class = b,     resp = PSDNPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = Length_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_Price_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = wages.AFI_z),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60:NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60:MSQD_SPAWN_SDM_90:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = MSQD_SPAWN_SDM_90:MSQD.Open),
  prior(exponential(1), class = sigma, resp = PSDNPricez),
  prior(exponential(1), class = sigma, resp = logPSDNLandings),
  prior(lkj(2),         class = rescor))

set.seed(66)
fit_qPSDN <-
  brm(data = dataset_psdn_landing,
      family = gaussian,
      price_model + landing_model + set_rescor(TRUE),
      prior = prior_lognormal,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(max_treedepth = 15, adapt_delta = 0.99),
      file = "Estimations/fit_qPSDN_wages")


