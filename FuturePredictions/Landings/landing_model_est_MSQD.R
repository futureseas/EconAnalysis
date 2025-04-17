
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


#-----------------------------------------------
## Obtain dataset for estimation and run landing models with new SDMs

data <- readRDS(here::here("Landings", "Predictions", "prediction_MSQD.rds"))

price_model   <- bf(MSQD_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(MSQD_Landings) ~
  1 + MSQD_SPAWN_SDM_90 + MSQD_Price_z + PSDN_SDM_60:PSDN.Open:MSQD_SPAWN_SDM_90 + NANC_SDM_20:MSQD_SPAWN_SDM_90 + PSDN_SDM_60:PSDN.Open + NANC_SDM_20 + PSDN.Total.Closure + Length_z +
 (1 + MSQD_SPAWN_SDM_90 + MSQD_Price_z + PSDN_SDM_60:PSDN.Open:MSQD_SPAWN_SDM_90 + NANC_SDM_20:MSQD_SPAWN_SDM_90 + PSDN_SDM_60:PSDN.Open + NANC_SDM_20 + PSDN.Total.Closure | port_cluster_ID))

# Create priors
prior_lognormal <- c(
  prior(lognormal(0,1), class = b, resp = MSQDPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b, resp = logMSQDLandings, coef = Length_z),
  prior(lognormal(0,1), class = b, resp = logMSQDLandings, coef = MSQD_Price_z),
  prior(lognormal(0,1), class = b, resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90:NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = MSQD_SPAWN_SDM_90:PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = PSDN_SDM_60:PSDN.Open),
  prior(normal(0,1),    class = b,     resp = logMSQDLandings, coef = PSDN.Total.Closure),
  prior(exponential(1), class = sigma, resp = MSQDPricez),
  prior(exponential(1), class = sigma, resp = logMSQDLandings),
  prior(lkj(2),         class = rescor))

set.seed(123)

init_fun <- function() list(
  b = rep(1.1, 10),
  sigma = 1,
  L = diag(2)  # para la correlación residual
)


unlink(here::here("FuturePredictions", "Landings", "fit_qMSQD_check.rds"))
fit_qMSQD <- brm(
  data = data,
  family = gaussian,
  price_model + landing_model + set_rescor(TRUE),
  prior = prior_lognormal,
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  init = init_fun,
  refresh = 10,
  control = list(max_treedepth = 15, adapt_delta = 0.99),
  file = here::here("FuturePredictions", "Landings", "fit_qMSQD_check"))

 