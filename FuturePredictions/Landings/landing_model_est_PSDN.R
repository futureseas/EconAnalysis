#
#---- PSDN Landing model ----
#

##---- Setup ----

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

setwd("C:/GitHub/EconAnalysis/FuturePredictions")


##---- Data ----

## Obtain dataset for estimation and run landing models with new SDMs

data <- readRDS("Landings/Data/prediction_PSDN.rds") %>%
  select(9:ncol(.))%>%
  select(-MSQD_SPAWN_SDM_90, -MSQD_SPAWN_SDM_90_z,
         -PSDN_SDM_60, -PSDN_SDM_60_z, 
         -NANC_SDM_20, -NANC_SDM_20)

## Add new SDMs (GAM Boost)
MSQD_sdm_data <- read.csv("SDM/Historical/MSQD_SDM_port_day.csv") %>%
  group_by(LANDING_MONTH, PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(MSQD_SDM_90 = mean(SDM_90, na.rm = TRUE), .groups = "drop")

PSDN_sdm_data <- read.csv("SDM/Historical/PSDN_SDM_port_day.csv") %>%
  group_by(LANDING_MONTH, PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(PSDN_SDM_60 = mean(SDM_60, na.rm = TRUE), .groups = "drop")

NANC_sdm_data <- read.csv("SDM/Historical/NANC_SDM_port_day_merged.csv") %>%
  group_by(LANDING_MONTH, PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(NANC_SDM_60 = mean(SDM_60, na.rm = TRUE), 
            NANC_SDM_20 = mean(SDM_20, na.rm = TRUE), 
            .groups = "drop")

## Merge data
data <- data %>%
  left_join(MSQD_sdm_data, by = c("PORT_AREA_CODE", "LANDING_MONTH", "LANDING_YEAR")) %>%
  left_join(PSDN_sdm_data, by = c("PORT_AREA_CODE", "LANDING_MONTH", "LANDING_YEAR")) %>%
  left_join(NANC_sdm_data, by = c("PORT_AREA_CODE", "LANDING_MONTH", "LANDING_YEAR"))



##---- Equations ----

price_model   <- bf(PSDN_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(PSDN_Landings) ~
                       1 + PSDN_SDM_60 + PSDN_Price_z + PSDN_SDM_60:MSQD_SDM_90:MSQD.Open + PSDN_SDM_60:NANC_SDM_20 + MSQD_SDM_90:MSQD.Open + NANC_SDM_20 + WA.Restriction + Length_z +
                      (1 + PSDN_SDM_60 + PSDN_Price_z + PSDN_SDM_60:MSQD_SDM_90:MSQD.Open + PSDN_SDM_60:NANC_SDM_20 + MSQD_SDM_90:MSQD.Open + NANC_SDM_20 + WA.Restriction | port_cluster_ID))


# Create priors
get_prior(data = data,
          family = gaussian,
          price_model + landing_model + set_rescor(TRUE))
prior_lognormal <- c(
  prior(lognormal(0,1), class = b,     resp = PSDNPricez,      coef = Price.Fishmeal.AFI_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = Length_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_Price_z),
  prior(lognormal(0,1), class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60:NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = PSDN_SDM_60:MSQD_SDM_90:MSQD.Open),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = NANC_SDM_20),
  prior(normal(0,1),    class = b,     resp = logPSDNLandings, coef = MSQD_SDM_90:MSQD.Open),
  prior(exponential(1), class = sigma, resp = PSDNPricez),
  prior(exponential(1), class = sigma, resp = logPSDNLandings),
  prior(lkj(2),         class = rescor))


##---- Estimations ----

set.seed(66)
init_fun <- function() list(
  b = rep(1.1, 10),
  sigma = 1,
  L = diag(2)  # para la correlación residual
)
unlink("Landings/Estimations/fit_qPSDN_boost_GAM.rds")

fit_qPSDN <- brm(
  data = data,
  family = gaussian,
  price_model + landing_model + set_rescor(TRUE),
  prior = prior_lognormal,
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  init = init_fun,
  refresh = 10,
  control = list(max_treedepth = 15, adapt_delta = 0.99),
  file = "Landings/Estimations/fit_qPSDN_boost_GAM.rds")

shinystan::launch_shinystan(fit_qPSDN)
