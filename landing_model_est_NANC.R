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
                PSDN_SDM_60, PSDN_SDM_60_z,
                NANC_SDM_20, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open,
                Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price.AFI_z,
                Length, Length_z) %>% 
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, NANC_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.0001, 0, NANC_Landings)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_NANC_Landings = log(NANC_Landings)) %>%
  mutate(cluster_port = paste(group_all, PORT_AREA_CODE, sep = "-", collapse = NULL)) %>%
    filter(group_all == 6 | group_all == 7) %>%
  drop_na()

# filter(PORT_AREA_CODE != "CLO") %>%
# filter(PORT_AREA_CODE != "CLW") %>%
# dataset_nanc %>% group_by(PORT_AREA_CODE) %>% summarize(n_freq = n()/nrow(dataset_nanc))


#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_nanc$port_ID            <- factor(dataset_nanc$PORT_AREA_CODE)
dataset_nanc$port_cluster_ID    <- factor(dataset_nanc$cluster_port)
class(dataset_nanc$port_ID)
class(dataset_nanc$port_cluster_ID)
class(dataset_nanc$PSDN.Total.Closure)

dataset_nanc_landing <- dataset_nanc %>%
  dplyr::filter(NANC_Landings > 0) %>%
  mutate(n_total = n()) %>%
  group_by(cluster_port) %>% 
  mutate(obs = n(), perc = n()/n_total) %>% 
  filter(perc > 0.02)

### Check number of observations
dataset_nanc_landing %>% select('cluster_port', 'obs', 'perc') %>% unique()
dataset_nanc_landing %>% group_by(PORT_AREA_CODE) %>%
  summarize(n_obs = n()) %>% mutate(per = scales::percent(n_obs / sum(n_obs)))
nrow(dataset_nanc_landing)

# ### Descriptive statistics 
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

# gs4_create("SummaryMonthly_Q_NANC_v4", sheets = table)
# rm(desc_data, table)


### Correlation between diesel price and fishmeal price
round(cor(dataset_nanc_landing$Price.Fishmeal.AFI, dataset_nanc_landing$diesel.price.AFI_z), 2)
plyr::ddply(dataset_nanc_landing, c("PORT_AREA_CODE"), summarise, cor = round(cor(Price.Fishmeal.AFI, diesel.price.AFI_z), 2))


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
# purtest(pDataset$Price.Fishmeal.AFI, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)




## -------------------------------------------------------------------
### Anchovy landing model ###
write.csv(dataset_nanc_landing,"C:\\Data\\PacFIN data\\dataset_estimation_NANC.csv", row.names = FALSE)

## Define landing equation
price_model <- bf(NANC_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(NANC_Landings) ~
  1 + NANC_SDM_20 + NANC_Price_z + NANC_SDM_20:MSQD_SPAWN_SDM_90:MSQD.Open + NANC_SDM_20:PSDN_SDM_60:PSDN.Open + MSQD_SPAWN_SDM_90:MSQD.Open + PSDN_SDM_60:PSDN.Open + PSDN.Total.Closure + Length_z +
 (1 + NANC_SDM_20 + NANC_Price_z + NANC_SDM_20:MSQD_SPAWN_SDM_90:MSQD.Open + NANC_SDM_20:PSDN_SDM_60:PSDN.Open + MSQD_SPAWN_SDM_90:MSQD.Open + PSDN_SDM_60:PSDN.Open + PSDN.Total.Closure | port_cluster_ID))

## Create priors
get_prior(data = dataset_nanc_landing,
           family = gaussian,
           price_model + landing_model + set_rescor(TRUE))

prior_lognormal <- c(
  prior(lognormal(0,1), class = b,     resp = NANCPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = Length_z),
  prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = NANC_Price_z),
  prior(lognormal(0,1), class = b,     resp = logNANCLandings, coef = NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = NANC_SDM_20:PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = NANC_SDM_20:MSQD_SPAWN_SDM_90:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = MSQD_SPAWN_SDM_90:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logNANCLandings, coef = PSDN.Total.Closure),
  prior(exponential(1), class = sigma, resp = NANCPricez),
  prior(exponential(1), class = sigma, resp = logNANCLandings),
  prior(lkj(2),         class = rescor))

## Estimate model
set.seed(66)
fit_qNANC <-
  brm(data = dataset_nanc_landing,
      family = gaussian,
      price_model + landing_model + set_rescor(TRUE),
      prior = prior_lognormal,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(max_treedepth = 15, adapt_delta = 0.99),
      file = "Estimations/fit_qNANC")

fit_qNANC <- add_criterion(fit_qNANC, "loo", overwrite = TRUE)
