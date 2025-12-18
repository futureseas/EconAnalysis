# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(slider)
library(zoo)

### Initialise code
apollo_initialise()

setwd("D:/GitHub/EconAnalysis/Participation")


### Set core controls
apollo_control = list(
  modelName       = "NL_participation_model_c4_lastday",
  modelDescr      = "Participation, location and target species decisions",
  indivID         = "fished_vessel_anon", 
  outputDirectory = "output",
  panelData       = TRUE,
  nCores          = 18,
  workInLogs      = TRUE
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


### Read database 
# library(readstata13)
# data <- read.dta13("H:/My Drive/Data/Anonymised data/part_model_c4.dta")
# saveRDS(data, "H:/My Drive/Data/Anonymised data/part_model_c4.rds")

# google_dir <- "H:/My Drive/"
google_dir <- "G:/Mi unidad/"

long_data <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c4.rds")) %>%
  select(set_date, fished_haul_anon, fished_vessel_anon, selection, fished,
         mean_avail, mean_price, wind_max_220_mh, dist_to_cog, dist_port_to_catch_area_zero,
         psdnclosure, btnaclosure, dummy_last_day, unem_rate, d_d, d_cd, weekend) %>%
  mutate(
    selection = tolower(gsub("-", "_", selection)),
    date = as.Date(set_date)
  ) %>%
  mutate(
    # outside option
    PORT_AREA_CODE = case_when(
      selection == "no_participation" ~ "NOPART",
      TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))
    )
  ) %>%
  mutate(
    # outside option
    sp4 = case_when(
      selection == "no_participation" ~ "NOPART",
      TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$"))
    )
  )


## Add SDMs (daily, MA 7, MA 14) 
prepare_sdm <- function(sdm_rds,
                        species_code,
                        sdm_col,          # ej "SDM_90"
                        port_var = "PORT_AREA_CODE") {
  
  sdm <- readRDS(sdm_rds) %>%
    mutate(
      date = if(!"date" %in% names(.))
        make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)
      else date,
      across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))
    ) %>%
    arrange(.data[[port_var]], date) %>%
    group_by(.data[[port_var]]) %>%
    mutate(
      !!paste0(species_code, "_daily") := .data[[sdm_col]],
      !!paste0(species_code, "_t1")    := lag(.data[[sdm_col]], 1),
      !!paste0(species_code, "_MA7_t1")  := slide_dbl(lag(.data[[sdm_col]],1), mean, .before = 6),
      !!paste0(species_code, "_MA14_t1") := slide_dbl(lag(.data[[sdm_col]],1), mean, .before = 13),
      !!paste0(species_code, "_MA30_t1") := slide_dbl(lag(.data[[sdm_col]],1), mean, .before = 29)
    ) %>%
    ungroup() %>%
    select(
      .data[[port_var]], date,
      starts_with(paste0(species_code, "_"))
    )
  
  return(sdm)
}

MSQD_sdm <- prepare_sdm("SDMs/sdm_msqd.rds", "MSQD", "MSQD_SDM_90")
PSDN_sdm <- prepare_sdm("SDMs/sdm_psdn.rds", "PSDN", "PSDN_SDM_60")
NANC_sdm <- prepare_sdm("SDMs/sdm_nanc.rds", "NANC", "NANC_SDM_60")
JMCK_sdm <- prepare_sdm("SDMs/sdm_jmck.rds", "JMCK", "JMCK_SDM_60")
CMCK_sdm <- prepare_sdm("SDMs/sdm_cmck.rds", "CMCK", "CMCK_SDM_60")
PHRG_sdm <- prepare_sdm("SDMs/sdm_phrg.rds", "PHRG", "PHRG_SDM_20")
ALBC_sdm <- prepare_sdm("SDMs/sdm_albc.rds", "ALBC", "ALBC_SDM_90")
)


sdm_all <- Reduce(
  function(x, y) left_join(x, y, by = c("PORT_AREA_CODE", "date")),
  list(MSQD_sdm, PSDN_sdm, NANC_sdm, JMCK_sdm, CMCK_sdm, PHRG_sdm)
)


##### Unir SDM con LONG DATA

long_data2 <- long_data %>%
  left_join(sdm_all, by = c("PORT_AREA_CODE","date"))

long_data2 <- long_data2 %>%
  mutate(
    mean_avail_MA30 = case_when(
      sp4 == "MSQD" ~ MSQD_MA30_t1,
      sp4 == "PSDN" ~ PSDN_MA30_t1,
      sp4 == "NANC" ~ NANC_MA30_t1,
      sp4 == "JMCK" ~ JMCK_MA30_t1,
      sp4 == "CMCK" ~ CMCK_MA30_t1,
      sp4 == "PHRG" ~ PHRG_MA30_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    ),
    mean_avail_MA14 = case_when(
      sp4 == "MSQD" ~ MSQD_MA14_t1,
      sp4 == "PSDN" ~ PSDN_MA14_t1,
      sp4 == "NANC" ~ NANC_MA14_t1,
      sp4 == "JMCK" ~ JMCK_MA14_t1,
      sp4 == "CMCK" ~ CMCK_MA14_t1,
      sp4 == "PHRG" ~ PHRG_MA14_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    ),
    mean_avail_MA7 = case_when(
      sp4 == "MSQD" ~ MSQD_MA7_t1,
      sp4 == "PSDN" ~ PSDN_MA7_t1,
      sp4 == "NANC" ~ NANC_MA7_t1,
      sp4 == "JMCK" ~ JMCK_MA7_t1,
      sp4 == "CMCK" ~ CMCK_MA7_t1,
      sp4 == "PHRG" ~ PHRG_MA7_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    ),
    avail_t1_daily = case_when(
      sp4 == "MSQD" ~ MSQD_t1,
      sp4 == "PSDN" ~ PSDN_t1,
      sp4 == "NANC" ~ NANC_t1,
      sp4 == "JMCK" ~ JMCK_t1,
      sp4 == "CMCK" ~ CMCK_t1,
      sp4 == "PHRG" ~ PHRG_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    )
  )



##### Long to wide

database <- long_data %>%
  pivot_wider(
    names_from = selection,                  # Unique values for alternatives
    values_from = c(set_date, fished, mean_avail, mean_price, wind_max_220_mh, dist_to_cog, 
                    dist_port_to_catch_area_zero, dummy_last_day, 
                    unem_rate, d_d, d_cd)) %>%
  dplyr::mutate(unem_rate = unem_rate_sfa_nanc)

database$choice <- ifelse(database$fished_sfa_nanc == 1, 1,                              
                   ifelse(database$fished_laa_nanc == 1, 2,
                   ifelse(database$fished_laa_cmck == 1, 3,                              
                   ifelse(database$fished_laa_msqd == 1, 4,
                   ifelse(database$fished_laa_ytna == 1, 5,
                   ifelse(database$fished_mna_msqd == 1, 6,                              
                   ifelse(database$fished_sba_msqd == 1, 7,
                   ifelse(database$fished_laa_btna == 1, 8,                              
                   ifelse(database$fished_sfa_msqd == 1, 9,
                   ifelse(database$fished_mna_psdn == 1, 10,                              
                   ifelse(database$fished_sba_cmck == 1, 11,
                   ifelse(database$fished_mra_msqd == 1, 12,                              
                   ifelse(database$fished_laa_psdn == 1, 13,
                   ifelse(database$fished_mna_nanc == 1, 14,
                   ifelse(database$fished_no_participation == 1, 15, NA)
                   ))))))))))))))      

### Add a numeric alternative column to your dataset
database$choice <- as.integer(database$choice)
database <- database[order(database$fished_vessel_anon, database$fished_haul_anon), ]














# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(B_mean_avail                   = 0.7,
              B_mean_price                   = 0.1,
              B_wind_max_220_mh              = -0.03,
              B_dist_to_cog                  = -0.003,
              B_dist_port_to_catch_area_zero = -0.005,
              B_dummy_last_day               = 1.1,
              B_unem_rate_part               = 0.15,
              B_d_d                          = -1.4,
              B_d_cd                         = -1.5,
              w_cmck                         = -4,
              w_msqd                         = -4,
              w_psdn                         = -4,
              c_psdn                         = -0.5,
              w_nanc                         = -4,
              w_tuna                         = -4,
              c_btna                         = -4,
              asc_sfa_nanc                   = -2.75,
              asc_laa_nanc                   = -2.75,
              asc_laa_cmck                   = -2.75,
              asc_laa_msqd                   = -2.75,
              asc_laa_ytna                   = -2.75,
              asc_mna_msqd                   = -2.75,
              asc_sba_msqd                   = -2.75,
              asc_laa_btna                   = -2.75,
              asc_sfa_msqd                   = -2.75,
              asc_mna_psdn                   = -2.75,
              asc_sba_cmck                   = -2.75,
              asc_mra_msqd                   = -2.75,
              asc_laa_psdn                   = -2.75,
              asc_mna_nanc                   = -2.75,
              lambda_part                    = 0.5, 
              lambda_cmck                    = 0.5,
              lambda_msqd                    = 0.5,
              lambda_psdn                    = 0.5,
              lambda_nanc                    = 0.5,
              lambda_tuna                    = 0.5,
              asc_no_participation           = 0,
              B_unem_rate_nopart             = 0,
              w_nopart                       = 0)

# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation", "B_unem_rate_nopart", "w_nopart")


### Read in starting values for at least some parameters from existing model output file
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"NL_participation_model",overwriteFixed=FALSE)


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
# 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["sfa_nanc"]]         = asc_sfa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_sfa_nanc + B_mean_price * mean_price_sfa_nanc + B_wind_max_220_mh * wind_max_220_mh_sfa_nanc + B_d_d * d_d_sfa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_nanc + B_dummy_last_day * dummy_last_day_sfa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_nanc  
  V[["laa_nanc"]]         = asc_laa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_laa_nanc + B_mean_price * mean_price_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_d_d * d_d_laa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_last_day * dummy_last_day_laa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_nanc  
  V[["laa_cmck"]]         = asc_laa_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_d_d * d_d_laa_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_last_day * dummy_last_day_laa_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_cmck  
  V[["laa_msqd"]]         = asc_laa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_d_d * d_d_laa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_last_day * dummy_last_day_laa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_msqd  
  V[["laa_ytna"]]         = asc_laa_ytna + w_tuna * weekend                        + B_mean_avail * mean_avail_laa_ytna + B_mean_price * mean_price_laa_ytna + B_wind_max_220_mh * wind_max_220_mh_laa_ytna + B_d_d * d_d_laa_ytna + B_d_cd * d_cd_laa_ytna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_ytna + B_dummy_last_day * dummy_last_day_laa_ytna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_ytna  
  V[["mna_msqd"]]         = asc_mna_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_d_d * d_d_mna_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_last_day * dummy_last_day_mna_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_msqd  
  V[["sba_msqd"]]         = asc_sba_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_d_d * d_d_sba_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_last_day * dummy_last_day_sba_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_msqd   
  V[["laa_btna"]]         = asc_laa_btna + w_tuna * weekend + c_btna * btnaclosure + B_mean_avail * mean_avail_laa_btna + B_mean_price * mean_price_laa_btna + B_wind_max_220_mh * wind_max_220_mh_laa_btna + B_d_d * d_d_laa_btna + B_d_cd * d_cd_laa_btna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_btna + B_dummy_last_day * dummy_last_day_laa_btna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_btna   
  V[["sfa_msqd"]]         = asc_sfa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_d_d * d_d_sfa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_last_day * dummy_last_day_sfa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_msqd   
  V[["mna_psdn"]]         = asc_mna_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_d_d * d_d_mna_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_last_day * dummy_last_day_mna_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_psdn   
  V[["sba_cmck"]]         = asc_sba_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_sba_cmck + B_mean_price * mean_price_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_d_d * d_d_sba_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_last_day * dummy_last_day_sba_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_cmck   
  V[["mra_msqd"]]         = asc_mra_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_d_d * d_d_mra_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_last_day * dummy_last_day_mra_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mra_msqd   
  V[["laa_psdn"]]         = asc_laa_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_d_d * d_d_laa_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_last_day * dummy_last_day_laa_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_psdn   
  V[["mna_nanc"]]         = asc_mna_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_d_d * d_d_mna_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_last_day * dummy_last_day_mna_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_nanc   
  V[["no_participation"]] = asc_no_participation + w_nopart * weekend + B_unem_rate_nopart * unem_rate
  

  ### Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      cmck = lambda_cmck, msqd = lambda_msqd, psdn = lambda_psdn, 
                      nanc = lambda_nanc, tuna = lambda_tuna)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part")
  nlStructure[["part"]] = c("cmck", "msqd", "psdn", "nanc", "tuna")
  nlStructure[["cmck"]] = c("laa_cmck", "sba_cmck")
  nlStructure[["msqd"]] = c("laa_msqd", "mna_msqd", "mra_msqd", "sba_msqd", "sfa_msqd")
  nlStructure[["psdn"]] = c("laa_psdn", "mna_psdn")
  nlStructure[["nanc"]] = c("laa_nanc", "mna_nanc", "sfa_nanc")
  nlStructure[["tuna"]] = c("laa_ytna", "laa_btna")

  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
      sfa_nanc = 1, laa_nanc = 2, laa_cmck = 3, laa_msqd = 4, laa_ytna = 5, mna_msqd = 6, sba_msqd = 7, laa_btna = 8, 
      sfa_msqd = 9, mna_psdn = 10, sba_cmck = 11, mra_msqd = 12, laa_psdn = 13, mna_nanc = 14, no_participation = 15),
    avail        = 1,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(constraints=c("lambda_part < 1 + 1e-10",
                                                             "lambda_part - lambda_cmck > -1e-10",
                                                             "lambda_part - lambda_msqd > -1e-10",
                                                             "lambda_part - lambda_psdn > -1e-10",
                                                             "lambda_part - lambda_nanc > -1e-10",
                                                             "lambda_part - lambda_tuna > -1e-10",
                                                             "lambda_cmck > 0",
                                                             "lambda_msqd > 0",
                                                             "lambda_psdn > 0",
                                                             "lambda_nanc > 0",
                                                             "lambda_tuna > 0")))


# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model,modelOutput_settings = list(printT1=1))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model,saveOutput_settings = list(printT1=1))

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()


# ################################################################# #
##### Create Table                                               ####
# ################################################################# #

summary_results <- summary(model)







