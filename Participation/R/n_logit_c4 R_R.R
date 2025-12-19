
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


## Add SDMs (daily, lag t-1, MA7/14/30 over calendar days, keeping missing as NA)
prepare_sdm <- function(sdm_rds,
                        species_code,
                        sdm_col,
                        port_var = "PORT_AREA_CODE") {
  
  stopifnot(file.exists(sdm_rds))
  
  sdm <- readRDS(sdm_rds) %>%
    dplyr::mutate(
      date = if(!"date" %in% names(.))
        lubridate::make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)
      else as.Date(date)
    ) %>%
    dplyr::arrange(.data[[port_var]], date) %>%
    dplyr::group_by(.data[[port_var]]) %>%
    
    # 1) Ensure a complete daily calendar per port (keeps missing SDM days as NA)
    tidyr::complete(date = seq(min(date, na.rm = TRUE),
                               max(date, na.rm = TRUE),
                               by = "day")) %>%
    dplyr::arrange(date) %>%
    
    # 2) Build lag and moving averages using calendar-day windows
    dplyr::mutate(
      !!paste0(species_code, "_daily") := .data[[sdm_col]],
      
      .xlag := dplyr::lag(.data[[sdm_col]], 1),
      
      !!paste0(species_code, "_t1") := .xlag,
      
      !!paste0(species_code, "_MA7_t1")  :=
        slider::slide_dbl(.xlag, ~ mean(.x, na.rm = TRUE), .before = 6),
      
      !!paste0(species_code, "_MA14_t1") :=
        slider::slide_dbl(.xlag, ~ mean(.x, na.rm = TRUE), .before = 13),
      
      !!paste0(species_code, "_MA30_t1") :=
        slider::slide_dbl(.xlag, ~ mean(.x, na.rm = TRUE), .before = 29)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[port_var]], date,
                  dplyr::starts_with(paste0(species_code, "_"))) %>%
    # clean helper column if it somehow survived
    dplyr::select(-dplyr::any_of(".xlag"))
  
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

prepare_avail_spec <- function(data, avail_var,
                               dist_var = "dist_port_to_catch_area_zero",
                               dist_zero_is_missing = TRUE){
  
  data %>%
    mutate(
      # availability usada en ESTE spec (todavía con NA)
      mean_avail_raw = .data[[avail_var]],
      
      # missing de availability (SDM/CPUE)
      d_missing_avail = as.integer(is.na(mean_avail_raw)),
      
      # distance missing (según tu regla)
      dist_miss = case_when(
        is.na(.data[[dist_var]]) ~ 1L,
        dist_zero_is_missing & .data[[dist_var]] == 0 ~ 1L,
        TRUE ~ 0L
      ),
      
      # dummies de patrón (mutuamente excluyentes si lo deseas)
      d_cd = as.integer(dist_miss == 1 & d_missing_avail == 1),
      d_d  = as.integer(dist_miss == 1 & d_missing_avail == 0),
      d_c  = as.integer(dist_miss == 0 & d_missing_avail == 1),
      
      # ya para utilidad: NA -> 0
      mean_avail = tidyr::replace_na(mean_avail_raw, 0)
    ) %>%
    select(-mean_avail_raw, -dist_miss)
}

data_by_avail <- lapply(avail_specs, function(v){
  prepare_avail_spec(long_data2, v)
})
names(data_by_avail)
# "daily" "MA7" "MA14" "MA30" "t1daily"



#### Create database to estimate

build_database <- function(long_data_spec, alts){
  id_vars <- c("fished_haul_anon", "fished_vessel_anon", "set_date")
  
  # corregir desempleo (case-specific) para no_participation
  long_data_spec <- long_data_spec %>%
    group_by(across(all_of(id_vars))) %>%
    mutate(
      unem_rate_case = {
        v <- unem_rate[selection != "no_participation" & !is.na(unem_rate)]
        if (length(v) == 0) NA_real_ else v[1]
      },
      unem_rate = unem_rate_case
    ) %>%
    ungroup() %>%
    select(-unem_rate_case)
  
  # case-specific
  case_data <- long_data_spec %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(
      unem_rate   = first(unem_rate),
      weekend     = first(weekend),
      psdnclosure = first(psdnclosure),
      btnaclosure = first(btnaclosure),
      .groups = "drop"
    )
  
  # alt-specific (incluye is_CPUE y d_c)
  alt_data <- long_data_spec %>%
    select(all_of(id_vars), selection,
           fished, mean_avail, mean_price, wind_max_220_mh, dist_to_cog,
           dist_port_to_catch_area_zero, dummy_prev_days, dummy_prev_year_days,
           d_d, d_cd, d_c, is_CPUE) %>%
    pivot_wider(
      id_cols = all_of(id_vars),
      names_from  = selection,
      values_from = c(fished, mean_avail, mean_price, wind_max_220_mh, dist_to_cog,
                      dist_port_to_catch_area_zero, dummy_prev_days, dummy_prev_year_days,
                      d_d, d_cd, d_c, is_CPUE)
    )
  
  database <- alt_data %>% left_join(case_data, by = id_vars)
  
  # construir choice (según orden alts)
  fished_cols <- paste0("fished_", alts)
  missing_cols <- setdiff(fished_cols, names(database))
  if (length(missing_cols) > 0) database[missing_cols] <- 0
  
  database$choice <- apply(as.matrix(database[, fished_cols]), 1, function(r) {
    k <- which(r == 1)[1]
    if (length(k) == 0 || is.na(k)) return(NA_integer_)
    k
  })
  
  database %>%
    mutate(choice = as.integer(choice)) %>%
    arrange(fished_vessel_anon, fished_haul_anon)
}


alts <- c(
  "sfa_nanc","laa_nanc","laa_cmck","laa_msqd","laa_ytna",
  "mna_msqd","sba_msqd","laa_btna","sfa_msqd","mna_psdn",
  "sba_cmck","mra_msqd","laa_psdn","mna_nanc","no_participation"
)

# Asegúrate de que long_data2 tiene todas las columnas que selects luego
keep_cols <- c("fished_haul_anon","fished_vessel_anon","selection","fished","mean_avail","mean_price",
               "wind_max_220_mh","dist_to_cog","dist_port_to_catch_area_zero",
               "psdnclosure","btnaclosure","dummy_prev_days","dummy_prev_year_days",
               "unem_rate","d_d","d_cd","d_c","is_CPUE","weekend","set_date")

databases <- lapply(names(data_by_avail), function(nm){
  build_database(data_by_avail[[nm]] %>% select(all_of(keep_cols)), alts)
})
names(databases) <- names(data_by_avail)   # daily, MA7, MA14, MA30, t1daily


################################################################
#### DEFINE MODEL PARAMETERS                                ####
################################################################

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
              B_d_c                          = 0,
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


### Compute probabilities of nested logit

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  
  lambda_part <- 1/(1 + exp(-theta_part))  # (0,1)
  lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
  lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
  lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["sfa_nanc"]]         = asc_sfa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_sfa_nanc + B_mean_price * mean_price_sfa_nanc + B_wind_max_220_mh * wind_max_220_mh_sfa_nanc + B_d_d * d_d_sfa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days * dummy_prev_days_sfa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sfa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_nanc + (B_d_c * use_d_c) * d_c_sfa_nanc   
  V[["laa_nanc"]]         = asc_laa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_laa_nanc + B_mean_price * mean_price_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_d_d * d_d_laa_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_nanc + (B_d_c * use_d_c) * d_c_laa_nanc   
  V[["laa_cmck"]]         = asc_laa_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_d_d * d_d_laa_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_cmck + (B_d_c * use_d_c) * d_c_laa_cmck   
  V[["laa_msqd"]]         = asc_laa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_d_d * d_d_laa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_msqd + (B_d_c * use_d_c) * d_c_laa_msqd   
  V[["laa_ytna"]]         = asc_laa_ytna + w_tuna * weekend                        + B_mean_avail * mean_avail_laa_ytna + B_mean_price * mean_price_laa_ytna + B_wind_max_220_mh * wind_max_220_mh_laa_ytna + B_d_d * d_d_laa_ytna + B_d_cd * d_cd_laa_ytna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days * dummy_prev_days_laa_ytna + B_dummy_prev_year_days * dummy_prev_year_days_laa_ytna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_ytna + (B_d_c * use_d_c) * d_c_laa_ytna   
  V[["mna_msqd"]]         = asc_mna_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_d_d * d_d_mna_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_msqd + (B_d_c * use_d_c) * d_c_mna_msqd   
  V[["sba_msqd"]]         = asc_sba_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_d_d * d_d_sba_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_msqd + (B_d_c * use_d_c) * d_c_sba_msqd    
  V[["laa_btna"]]         = asc_laa_btna + w_tuna * weekend                        + B_mean_avail * mean_avail_laa_btna + B_mean_price * mean_price_laa_btna + B_wind_max_220_mh * wind_max_220_mh_laa_btna + B_d_d * d_d_laa_btna + B_d_cd * d_cd_laa_btna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days * dummy_prev_days_laa_btna + B_dummy_prev_year_days * dummy_prev_year_days_laa_btna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_btna + (B_d_c * use_d_c) * d_c_laa_btna    
  V[["sfa_msqd"]]         = asc_sfa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_d_d * d_d_sfa_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_msqd + (B_d_c * use_d_c) * d_c_sfa_msqd    
  V[["mna_psdn"]]         = asc_mna_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_d_d * d_d_mna_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_psdn + (B_d_c * use_d_c) * d_c_mna_psdn    
  V[["sba_cmck"]]         = asc_sba_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_sba_cmck + B_mean_price * mean_price_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_d_d * d_d_sba_cmck                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_cmck + (B_d_c * use_d_c) * d_c_sba_cmck    
  V[["mra_msqd"]]         = asc_mra_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_d_d * d_d_mra_msqd                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mra_msqd + (B_d_c * use_d_c) * d_c_mra_msqd    
  V[["laa_psdn"]]         = asc_laa_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_d_d * d_d_laa_psdn                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_psdn + (B_d_c * use_d_c) * d_c_laa_psdn    
  V[["mna_nanc"]]         = asc_mna_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_d_d * d_d_mna_nanc                          + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_nanc + (B_d_c * use_d_c) * d_c_mna_nanc    
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
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #


models <- list()



has_dc_variation <- function(db){
  dc_cols <- grep("^d_c_", names(db), value = TRUE)
  if(length(dc_cols)==0) return(FALSE)
  any(sapply(dc_cols, function(v){
    x <- db[[v]]
    ux <- unique(x[!is.na(x)])
    length(ux) > 1
  }))
}

use_d_c_by_spec <- c(daily=1, MA7=1, MA14=0, MA30=0, t1daily=1)

for(spec in names(databases)){
  
  apollo_control$modelName <- paste0("NL_participation_model_c4_R&R_", spec)
  database <- databases[[spec]]
  
  # decide si d_c entra (tiene variación Y tú quieres usarlo)
  dc_ok <- has_dc_variation(database) && (use_d_c_by_spec[[spec]] == 1)
  
  # setea fixed consistente ANTES de validateInputs
  apollo_fixed_spec <- apollo_fixed
  if(!dc_ok) apollo_fixed_spec <- unique(c(apollo_fixed_spec, "B_d_c"))
  apollo_fixed <- apollo_fixed_spec
  
  apollo_inputs <- apollo_validateInputs()
  
  # flag para multiplicar dentro de utilidad (evita problemas si B_d_c fijo o no)
  apollo_inputs$use_d_c <- as.numeric(dc_ok)
  
  # starting values consistentes con fixed
  apollo_beta_spec <- apollo_readBeta(apollo_beta, apollo_fixed, "NL_participation_model", overwriteFixed=FALSE)
  
  cat("\n---", spec, " dc_ok=", dc_ok,
      " | use_d_c=", apollo_inputs$use_d_c,
      " | B_d_c fixed=", ("B_d_c" %in% apollo_fixed), "\n")
  
  model_spec <- apollo_estimate(apollo_beta_spec, apollo_fixed,
                                apollo_probabilities, apollo_inputs,
                                estimate_settings=list())
  
  apollo_modelOutput(model_spec)
  apollo_saveOutput(model_spec, saveOutput_settings=list(printT1=1))
  models[[spec]] <- model_spec
  summary(model_spec)
}

comp <- data.frame(
  spec = names(models),
  LL   = sapply(models, function(m) m$maximum),
  k    = sapply(models, function(m) length(m$estimate)),
  AIC  = sapply(models, function(m) 2*length(m$estimate) - 2*m$maximum),
  BIC  = sapply(models, function(m) log(models[[1]]$nObs)*length(m$estimate) - 2*m$maximum)
)
print(comp[order(comp$AIC), ])
