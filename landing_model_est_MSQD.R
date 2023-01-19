
###########################
### SQUID Landing model ###
###########################

#----------------------------
# Setup #

## Take month out from price eq factor(LANDING_MONTH) #
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
dataset_msqd <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_Price, NANC_Price, 
                PSDN_SDM_60, NANC_SDM_20,
                MSQD_Price_z, PSDN_Price_z, MSQD_SPAWN_SDM_90_z, MSQD_SDM_90_z, 
                PSDN_SDM_60_z, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open, Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price, diesel.price.AFI, diesel.price_z, diesel.price.AFI_z,
                Length, Length_z, avg_set_MSQD, avg_set_MSQD_z,
                wages_z, wages.AFI_z) %>% 
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings< 0.0001, 0, MSQD_Landings)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_MSQD_Landings = log(MSQD_Landings)) %>%
  filter(group_all == 1 | group_all == 2 | group_all == 4 | group_all == 5 | group_all == 7) %>%
  mutate(cluster_port = paste(group_all, PORT_AREA_CODE, sep = "-", collapse = NULL)) %>%
  drop_na()

# filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA") %>%
# dataset_msqd %>% group_by(PORT_AREA_CODE) %>% summarize(n_freq = n()/nrow(dataset_msqd))


#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_msqd$port_cluster_ID    <- factor(dataset_msqd$cluster_port)
dataset_msqd$port_ID            <- factor(dataset_msqd$PORT_AREA_CODE)

class(dataset_msqd$port_ID)
class(dataset_msqd$port_cluster_ID)
class(dataset_msqd$PSDN.Total.Closure)


dataset_msqd_landing <- dataset_msqd %>%
  dplyr::filter(MSQD_Landings > 0) %>%
  dplyr::filter(MSQD.Open == 1) %>%
  mutate(n_total = n()) %>%
  group_by(cluster_port) %>% 
  mutate(obs = n(), perc = n()/n_total) %>% 
  filter(perc > 0.04)

### Check number of observations
dataset_msqd_landing %>% select('cluster_port', 'obs', 'perc') %>% unique()
dataset_msqd_landing %>% group_by(PORT_AREA_CODE) %>%
  summarize(n_obs = n()) %>% mutate(per = scales::percent(n_obs / sum(n_obs)))
nrow(dataset_msqd_landing)

### Descriptive statistics
desc_data <- dataset_msqd_landing %>%
  subset(select = c(Length, MSQD_Landings, MSQD_Price,
                    MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,
                    PSDN.Open, Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fraction of month open: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Fishmeal price", vars))
# 
# gs4_create("SummaryMonthly_Q_MSQD", sheets = table)
# rm(desc_data, table)


#-------------------------------------------------------------------
## Check stationarity in the panel dataset
library("plm")

dataset_msqd_landing$Date <-
  zoo::as.yearmon(paste(dataset_msqd_landing$LANDING_YEAR, dataset_msqd_landing$LANDING_MONTH), "%Y %m")

pDataset <- dataset_msqd_landing %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>%
  group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group >= 12) %>% drop_na()
pDataset <- pdata.frame(pDataset, index = c('Unique_ID', 'Date'))

# duplicate_indexes <- dataset_msqd %>%
#   group_by(PORT_AREA_CODE, Date, VESSEL_NUM) %>% mutate(dupe = n()>1)

purtest(pDataset$MSQD_Landings, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_Price, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$NANC_SDM_20, pmax = 4, exo = "intercept", test = "Pm")
# purtest(pDataset$Price.Fishmeal.AFI, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)

## -------------------------------------------------------------------
### Market squid landing model ###

dataset_msqd_landing <- dataset_msqd_landing %>% mutate(wages.AFI_z = -(wages.AFI_z))
dataset_select <- dataset_msqd_landing %>% ungroup() %>% #filter(LANDING_YEAR < 2015) %>%
  dplyr::select(MSQD_SPAWN_SDM_90,
                PSDN_SDM_60,
                NANC_SDM_20,
                Length_z,
                MSQD_Price_z,
                Price.Fishmeal.AFI_z,
                diesel.price.AFI_z,
                wages.AFI_z,
                MSQD_Landings)
res <- as.data.frame(cor(dataset_select))
round(res, 2)

write.csv(dataset_msqd_landing,"C:\\Data\\PacFIN data\\dataset_estimation_MSQD.csv", row.names = FALSE)

price_model   <- bf(MSQD_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(MSQD_Landings) ~
  1 + MSQD_SPAWN_SDM_90 + MSQD_Price_z + PSDN_SDM_60:PSDN.Open:MSQD_SPAWN_SDM_90 + NANC_SDM_20:MSQD_SPAWN_SDM_90 + PSDN_SDM_60:PSDN.Open + NANC_SDM_20 + PSDN.Total.Closure + wages.AFI_z + Length_z +
 (1 + MSQD_SPAWN_SDM_90 + MSQD_Price_z + PSDN_SDM_60:PSDN.Open:MSQD_SPAWN_SDM_90 + NANC_SDM_20:MSQD_SPAWN_SDM_90 + PSDN_SDM_60:PSDN.Open + NANC_SDM_20 + PSDN.Total.Closure + wages.AFI_z | port_cluster_ID))


# Create priors
prior_lognormal <- c(
  prior(lognormal(0,1), class = b,     resp = MSQDPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b,     resp = logMSQDLandings, coef = Length_z),
  prior(lognormal(0,1), class = b,     resp = logMSQDLandings, coef = MSQD_Price_z),
  prior(lognormal(0,1), class = b,     resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90),
  prior(lognormal(0,1), class = b,     resp = logMSQDLandings, coef = wages.AFI_z),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90:NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90:PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = PSDN.Total.Closure),
  prior(exponential(1), class = sigma, resp = MSQDPricez),
  prior(exponential(1), class = sigma, resp = logMSQDLandings),
  prior(lkj(2),         class = rescor))

set.seed(66)
 fit_qMSQD_wages <-
   brm(data = dataset_msqd_landing,
       family = gaussian,
       price_model + landing_model + set_rescor(TRUE),
       prior = prior_lognormal,
       iter = 2000, warmup = 1000, chains = 4, cores = 4,
       control = list(max_treedepth = 15, adapt_delta = 0.99),
       file = "Estimations/fit_qMSQD_wages")

fit_qMSQD_wages <- add_criterion(fit_qMSQD_wages, "loo", overwrite = TRUE)


