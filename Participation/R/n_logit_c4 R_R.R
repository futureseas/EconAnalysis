
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
  modelName       = "NL_participation_model_c4_R&R",
  modelDescr      = "Participation, location and target species decisions",
  indivID         = "fished_vessel_anon", 
  outputDirectory = "R/output",
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
         psdnclosure, btnaclosure, dummy_prev_days, dummy_prev_year_days, unem_rate, d_d, d_cd, weekend) %>%
  mutate(
    selection = tolower(gsub("-", "_", selection)),
    date      = as.Date(set_date)
  ) %>%
  mutate(
    PORT_AREA_CODE = case_when(
      selection == "no_participation" ~ "NOPART",
      TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))
    ),
    sp4 = case_when(
      selection == "no_participation" ~ "NOPART",
      TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$"))
    )
  )


## Add SDMs (daily, MA 7, MA 14) 
prepare_sdm <- function(sdm_rds,
                        species_code,
                        sdm_col,
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
      !!paste0(species_code, "_daily")  := .data[[sdm_col]],
      !!paste0(species_code, "_t1")     := dplyr::lag(.data[[sdm_col]], 1),
      !!paste0(species_code, "_MA7_t1")  := slide_dbl(dplyr::lag(.data[[sdm_col]], 1),
                                                      ~ mean(.x, na.rm = TRUE), .before = 6),
      !!paste0(species_code, "_MA14_t1") := slide_dbl(dplyr::lag(.data[[sdm_col]], 1),
                                                      ~ mean(.x, na.rm = TRUE), .before = 13),
      !!paste0(species_code, "_MA30_t1") := slide_dbl(dplyr::lag(.data[[sdm_col]], 1),
                                                      ~ mean(.x, na.rm = TRUE), .before = 29)
    ) %>%
    ungroup() %>%
    select(.data[[port_var]], date, starts_with(paste0(species_code, "_")))
  
  return(sdm)
}

MSQD_sdm <- prepare_sdm("SDMs/sdm_msqd.rds", "MSQD", "MSQD_SDM_90")
PSDN_sdm <- prepare_sdm("SDMs/sdm_psdn.rds", "PSDN", "PSDN_SDM_60")
NANC_sdm <- prepare_sdm("SDMs/sdm_nanc.rds", "NANC", "NANC_SDM_60")
JMCK_sdm <- prepare_sdm("SDMs/sdm_jmck.rds", "JMCK", "JMCK_SDM_60")
CMCK_sdm <- prepare_sdm("SDMs/sdm_cmck.rds", "CMCK", "CMCK_SDM_60")
PHRG_sdm <- prepare_sdm("SDMs/sdm_phrg.rds", "PHRG", "PHRG_SDM_20")
ALBC_sdm <- prepare_sdm("SDMs/sdm_albc.rds", "ALBC", "albc_SDM_90")


sdm_all <- Reduce(
  function(x, y) left_join(x, y, by = c("PORT_AREA_CODE", "date")),
  list(MSQD_sdm, PSDN_sdm, NANC_sdm, JMCK_sdm, CMCK_sdm, PHRG_sdm, ALBC_sdm)
)

### Add CPU Index

cpue_raw <- readRDS("D:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds") %>%
  mutate(date = make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  select(PORT_AREA_CODE, Species_Dominant, date, CPUE_index)

cpue_expanded <- cpue_raw %>%
  group_by(PORT_AREA_CODE, Species_Dominant) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  ungroup() %>%
  arrange(PORT_AREA_CODE, Species_Dominant, date) %>%
  group_by(PORT_AREA_CODE, Species_Dominant) %>%
  mutate(
    CPUE_MA30_t1 = slide_dbl(
      dplyr::lag(CPUE_index, 1),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
      .before = 29
    ),
    CPUE_MA90_t1 = slide_dbl(
      dplyr::lag(CPUE_index, 1),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
      .before = 89
    )
  ) %>%
  ungroup()



# ################################################################# #
#### MERGE SDM + CPUE INTO LONG DATA                             ####
# ################################################################# #

long_data2 <- long_data %>%
  left_join(sdm_all, by = c("PORT_AREA_CODE", "date")) %>%
  left_join(
    cpue_expanded,
    by = c("PORT_AREA_CODE" = "PORT_AREA_CODE",
           "date" = "date",
           "sp4"  = "Species_Dominant")
  ) %>%
  mutate(
    CPUE_avail_t1      = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
    used_90d_fallback  = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1)),
    is_CPUE            = as.integer(sp4 %in% c("BTNA", "YTNA"))
  )


long_data2 <- long_data2 %>%
  mutate(
    CPUE_avail_t1 = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
    used_90d_fallback = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1))
  )


# ################################################################# #
#### BUILD MULTIPLE AVAILABILITY MEASURES                         ####
# ################################################################# #

long_data2 <- long_data2 %>%
  mutate(
    mean_avail_daily = case_when(
      sp4 == "MSQD" ~ MSQD_daily,
      sp4 == "PSDN" ~ PSDN_daily,
      sp4 == "NANC" ~ NANC_daily,
      sp4 == "JMCK" ~ JMCK_daily,
      sp4 == "CMCK" ~ CMCK_daily,
      sp4 == "PHRG" ~ PHRG_daily,
      sp4 == "ALBC" ~ ALBC_daily,
      sp4 %in% c("BTNA","YTNA") ~ CPUE_avail_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    ),
    mean_avail_MA30 = case_when(
      sp4 == "MSQD" ~ MSQD_MA30_t1,
      sp4 == "PSDN" ~ PSDN_MA30_t1,
      sp4 == "NANC" ~ NANC_MA30_t1,
      sp4 == "JMCK" ~ JMCK_MA30_t1,
      sp4 == "CMCK" ~ CMCK_MA30_t1,
      sp4 == "PHRG" ~ PHRG_MA30_t1,
      sp4 == "ALBC" ~ ALBC_MA30_t1,
      sp4 %in% c("BTNA","YTNA") ~ CPUE_avail_t1,
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
      sp4 == "ALBC" ~ ALBC_MA14_t1,
      sp4 %in% c("BTNA","YTNA") ~ CPUE_avail_t1,
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
      sp4 == "ALBC" ~ ALBC_MA7_t1,
      sp4 %in% c("BTNA","YTNA") ~ CPUE_avail_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    ),
    mean_avail_t1_daily = case_when(
      sp4 == "MSQD" ~ MSQD_t1,
      sp4 == "PSDN" ~ PSDN_t1,
      sp4 == "NANC" ~ NANC_t1,
      sp4 == "JMCK" ~ JMCK_t1,
      sp4 == "CMCK" ~ CMCK_t1,
      sp4 == "PHRG" ~ PHRG_t1,
      sp4 == "ALBC" ~ ALBC_t1,
      sp4 %in% c("BTNA","YTNA") ~ CPUE_avail_t1,
      sp4 == "NOPART" ~ 0,
      TRUE ~ NA_real_
    )
  )



# ################################################################# #
#### AVAILABILITY SPEC SWITCH + CONSISTENT MISSING HANDLING       ####
# ################################################################# #
# NOTE: In this application, mean_price has no missing.
# We will (i) define d_missing_avail by spec, (ii) update d_d/d_cd/d_c
# using the new missing availability, and (iii) handle CPUE dummies.

avail_specs <- list(
  daily   = "mean_avail_daily",
  MA7     = "mean_avail_MA7",
  MA14    = "mean_avail_MA14",
  MA30    = "mean_avail_MA30",
  t1daily = "mean_avail_t1_daily"
)

prepare_avail_spec <- function(data, avail_var){
  
  data %>%
    mutate(
      # availability used in THIS model
      mean_avail = .data[[avail_var]],
      
      # # missing availability (spec-specific)
      d_missing_avail = as.integer(is.na(mean_avail)),
      # 
      # # NA -> 0 (so utilities can be computed)
      mean_avail = tidyr::replace_na(mean_avail, 0),
      # 
      # infer "distance missing" from original d_d / d_cd (built under MA30)
      dist_miss = as.integer(d_d == 1 | d_cd == 1),
      # 
      # # update missing-pattern dummies using the NEW availability missing
      # d_cd = as.integer(dist_miss == 1 & d_missing_avail == 1),
      # d_d  = as.integer(dist_miss == 1 & d_missing_avail == 0),
      d_c  = as.integer(dist_miss == 0 & d_missing_avail == 1),
      # 
      # CPUE: missing only for CPUE alternatives
      d_missing_cpue = as.integer(is_CPUE == 1 & d_missing_avail == 1),
      
    ) %>%
    select(-dist_miss)
}

data_by_avail <- lapply(avail_specs, function(v){
  prepare_avail_spec(long_data2, v)
})

names(data_by_avail)
# "daily" "MA7" "MA14" "MA30" "t1daily"

data <- data_by_avail$MA30





# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(asc_sfa_nanc                   = -2.75,
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
              c_psdn                         = -0.4,
              w_nanc                         = -4,
              w_cmck                         = -4,
              w_msqd                         = -4,
              w_tuna                         = -4,
              w_psdn                         = -4,
              theta_part = 0,
              theta_cmck = 12,
              theta_msqd = 0,
              theta_psdn = 0,
              theta_nanc = 0,
              theta_tuna = 12,
              B_mean_avail                   = 1.15,
              B_mean_price                   = 0.3,
              B_wind_max_220_mh              = -0.05,
              B_d_d                          = -1,
              B_d_cd                         = -1,
              B_dist_port_to_catch_area_zero = -0.005,
              B_dummy_prev_days              = 1.2,
              B_dummy_prev_year_days         = 0.15,
              B_unem_rate_part               = 0.2,
              asc_no_participation           = 0,
              B_unem_rate_nopart             = 0,
              w_nopart                       = 0,
              B_dist_to_cog                  = -0.001)

# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation", "B_unem_rate_nopart", "w_nopart", "theta_cmck", "theta_tuna")


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
  
  lambda_part <- 1/(1 + exp(-theta_part))  # (0,1)
  lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
  lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
  lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))
  
  ### Create list of probabilities P
  P = list()
  
  
# 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["sfa_nanc"]]         = asc_sfa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_sfa_nanc + B_mean_price * mean_price_sfa_nanc + B_wind_max_220_mh * wind_max_220_mh_sfa_nanc + B_d_d * d_d_sfa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days * dummy_prev_days_sfa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sfa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_nanc  
  V[["laa_nanc"]]         = asc_laa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_laa_nanc + B_mean_price * mean_price_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_d_d * d_d_laa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_nanc  
  V[["laa_cmck"]]         = asc_laa_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_d_d * d_d_laa_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_cmck  
  V[["laa_msqd"]]         = asc_laa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_d_d * d_d_laa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_msqd  
  V[["laa_ytna"]]         = asc_laa_ytna + w_tuna * weekend                        + B_mean_avail * mean_avail_laa_ytna + B_mean_price * mean_price_laa_ytna + B_wind_max_220_mh * wind_max_220_mh_laa_ytna + B_d_d * d_d_laa_ytna + B_d_cd * d_cd_laa_ytna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days * dummy_prev_days_laa_ytna + B_dummy_prev_year_days * dummy_prev_year_days_laa_ytna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_ytna  
  V[["mna_msqd"]]         = asc_mna_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_d_d * d_d_mna_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_msqd  
  V[["sba_msqd"]]         = asc_sba_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_d_d * d_d_sba_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_msqd   
  V[["laa_btna"]]         = asc_laa_btna + w_tuna * weekend                        + B_mean_avail * mean_avail_laa_btna + B_mean_price * mean_price_laa_btna + B_wind_max_220_mh * wind_max_220_mh_laa_btna + B_d_d * d_d_laa_btna + B_d_cd * d_cd_laa_btna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days * dummy_prev_days_laa_btna + B_dummy_prev_year_days * dummy_prev_year_days_laa_btna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_btna   
  V[["sfa_msqd"]]         = asc_sfa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_d_d * d_d_sfa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_msqd   
  V[["mna_psdn"]]         = asc_mna_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_d_d * d_d_mna_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_psdn   
  V[["sba_cmck"]]         = asc_sba_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_sba_cmck + B_mean_price * mean_price_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_d_d * d_d_sba_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_cmck   
  V[["mra_msqd"]]         = asc_mra_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_d_d * d_d_mra_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mra_msqd   
  V[["laa_psdn"]]         = asc_laa_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_d_d * d_d_laa_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_psdn   
  V[["mna_nanc"]]         = asc_mna_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_d_d * d_d_mna_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_nanc   
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

  
  
  avail <- list(
    sfa_nanc = 1,
    laa_nanc = 1,
    laa_cmck = 1,
    laa_msqd = 1,
    laa_ytna = 1,
    mna_msqd = 1,
    sba_msqd = 1,
    laa_btna = 1 - btnaclosure,   
    sfa_msqd = 1,
    mna_psdn = 1,
    sba_cmck = 1,
    mra_msqd = 1,
    laa_psdn = 1,
    mna_nanc = 1,
    no_participation = 1
  )
  
  
  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
      sfa_nanc = 1, laa_nanc = 2, laa_cmck = 3, laa_msqd = 4, laa_ytna = 5, mna_msqd = 6, sba_msqd = 7, laa_btna = 8, 
      sfa_msqd = 9, mna_psdn = 10, sba_cmck = 11, mra_msqd = 12, laa_psdn = 13, mna_nanc = 14, no_participation = 15),
    avail        = avail,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
# model = apollo_estimate(apollo_beta, apollo_fixed,
#                         apollo_probabilities, apollo_inputs,
#                         estimate_settings=list(constraints = c(
#   "lambda_part < 1 - 1e-6",
#   "lambda_part > 1e-6",
#   "lambda_cmck > 1e-6", "lambda_cmck < lambda_part - 1e-6",
#   "lambda_msqd > 1e-6", "lambda_msqd < lambda_part - 1e-6",
#   "lambda_psdn > 1e-6", "lambda_psdn < lambda_part - 1e-6",
#   "lambda_nanc > 1e-6", "lambda_nanc < lambda_part - 1e-6",
#   "lambda_tuna > 1e-6", "lambda_tuna < lambda_part - 1e-6"
# )))


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list())

# model <- apollo_addCovariance(model, apollo_inputs)  # computes Hessian + classical/robust cov
apollo_modelOutput(model)



# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model,modelOutput_settings = list(printT1=1))
# 
# # ----------------------------------------------------------------- #
# #---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# # ----------------------------------------------------------------- #
# 
apollo_saveOutput(model,saveOutput_settings = list(printT1=1))
# 
# # ################################################################# #
# ##### POST-PROCESSING                                            ####
# # ################################################################# #
# 
# ### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()
# 
# 
# # ################################################################# #
# ##### Create Table                                               ####
# # ################################################################# #
# 
summary_results <- summary(model)
estimates <- summary_results$   # Parameter estimates
p_values = summary_results$pValue     # p-values







