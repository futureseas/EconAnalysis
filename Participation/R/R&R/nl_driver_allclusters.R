# =====================================================================
# Nested Logit (Apollo) driver - multi-cluster + multi-availability specs
# Author: Felipe J. Quezada (with ChatGPT)
# Date: 2025-12-19
#
# Goal:
#   1) Load & standardize long-format logbook choice data for clusters c4/c5/c6/c7
#   2) Merge SDM + CPUE-based availability ONCE
#   3) Create multiple availability specifications: daily, t1daily, MA7_t1, MA14_t1, MA30_t1
#   4) Build wide "database" objects (one per spec) per cluster
#   5) Estimate NL models per cluster/spec using cluster-specific configs
#
# Notes:
#   - c5/c6/c7 have different alternatives+nests -> we keep configs separate but reuse the same pipeline.
#   - This script is designed to be the SINGLE entry point. Keep cluster configs below in one list.
# =====================================================================

rm(list=ls())

# ---------------------------
# Libraries
# ---------------------------
library(apollo)
library(tidyverse)
library(slider)
library(lubridate)

# ---------------------------
# Apollo init
# ---------------------------
apollo_initialise()

# ---------------------------
# User paths (EDIT ME)
# ---------------------------
# google_dir <- "H:/My Drive/"
google_dir <- "G:/Mi unidad/"
project_dir <- "D:/GitHub/EconAnalysis/Participation"

setwd(project_dir)

# ---------------------------
# Apollo control (common)
# ---------------------------
apollo_control = list(
  modelName       = "NL_participation_driver",
  modelDescr      = "Participation, location and target species decisions (multi-cluster, multi-spec)",
  indivID         = "fished_vessel_anon",
  outputDirectory = "R/output",
  panelData       = TRUE,
  nCores          = 18,
  workInLogs      = TRUE
)

# =====================================================================
# 1) Helpers: SDM/CPUE availability preparation (calendar-day windows)
# =====================================================================

prepare_sdm <- function(sdm_rds,
                        species_code,
                        sdm_col,
                        port_var = "PORT_AREA_CODE"){

  stopifnot(file.exists(sdm_rds))

  sdm <- readRDS(sdm_rds) %>%
    mutate(
      date = if(!"date" %in% names(.))
        make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)
      else as.Date(date)
    ) %>%
    arrange(.data[[port_var]], date) %>%
    group_by(.data[[port_var]]) %>%

    # Complete daily calendar per port (keeps missing SDM days as NA)
    tidyr::complete(date = seq(min(date, na.rm=TRUE),
                               max(date, na.rm=TRUE),
                               by="day")) %>%
    arrange(date) %>%

    # Build lag and moving averages using calendar-day windows
    mutate(
      !!paste0(species_code, "_daily") := .data[[sdm_col]],
      .xlag := lag(.data[[sdm_col]], 1),
      !!paste0(species_code, "_t1") := .xlag,
      !!paste0(species_code, "_MA7_t1")  := slider::slide_dbl(.xlag, ~ mean(.x, na.rm=TRUE), .before=6),
      !!paste0(species_code, "_MA14_t1") := slider::slide_dbl(.xlag, ~ mean(.x, na.rm=TRUE), .before=13),
      !!paste0(species_code, "_MA30_t1") := slider::slide_dbl(.xlag, ~ mean(.x, na.rm=TRUE), .before=29)
    ) %>%
    ungroup() %>%
    select(.data[[port_var]], date, starts_with(paste0(species_code, "_"))) %>%
    select(-any_of(".xlag"))

  return(sdm)
}

prepare_cpue <- function(cpue_rds,
                         port_var = "PORT_AREA_CODE",
                         sp_var   = "Species_Dominant"){

  stopifnot(file.exists(cpue_rds))

  cpue_raw <- readRDS(cpue_rds) %>%
    mutate(date = make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
    select(.data[[port_var]], .data[[sp_var]], date, CPUE_index)

  cpue_expanded <- cpue_raw %>%
    group_by(.data[[port_var]], .data[[sp_var]]) %>%
    complete(date = seq(min(date), max(date), by="day")) %>%
    ungroup() %>%
    arrange(.data[[port_var]], .data[[sp_var]], date) %>%
    group_by(.data[[port_var]], .data[[sp_var]]) %>%
    mutate(
      CPUE_MA30_t1 = slide_dbl(
        lag(CPUE_index, 1),
        ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm=TRUE),
        .before=29
      ),
      CPUE_MA90_t1 = slide_dbl(
        lag(CPUE_index, 1),
        ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm=TRUE),
        .before=89
      )
    ) %>%
    ungroup()

  return(cpue_expanded)
}

# =====================================================================
# 2) Helpers: standardize long data (shared columns across clusters)
# =====================================================================

standardize_long <- function(df){
  df %>%
    mutate(
      selection = tolower(gsub("-", "_", selection)),
      date      = as.Date(set_date),
      PORT_AREA_CODE = case_when(
        selection == "no_participation" ~ "NOPART",
        TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))
      ),
      sp4 = case_when(
        selection == "no_participation" ~ "NOPART",
        TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$"))
      )
    )
}

# =====================================================================
# 3) Helpers: create availability specs & missing flags (one place)
# =====================================================================

# Which availability specifications to create (you asked to run 5 models)
avail_specs <- list(
  daily   = "mean_avail_daily",
  MA7     = "mean_avail_MA7",
  MA14    = "mean_avail_MA14",
  MA30    = "mean_avail_MA30",
  t1daily = "mean_avail_t1_daily"
)

# Build mean_avail_* columns from SDM/CPUE merged fields
build_avail_columns <- function(long_data_merged){

  long_data_merged %>%
    mutate(
      # daily
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
      # MA30_t1
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
      # MA14_t1
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
      # MA7_t1
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
      # lag t-1 daily
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
}

# Apply missing rules consistently for a given spec
prepare_avail_spec <- function(data,
                               avail_var,
                               dist_var = "dist_port_to_catch_area_zero",
                               dist_zero_is_missing = TRUE){

  data %>%
    mutate(
      mean_avail_raw = .data[[avail_var]],
      d_missing_avail = as.integer(is.na(mean_avail_raw)),

      dist_miss = case_when(
        is.na(.data[[dist_var]]) ~ 1L,
        dist_zero_is_missing & .data[[dist_var]] == 0 ~ 1L,
        TRUE ~ 0L
      ),

      # Your 3-way pattern dummies
      d_cd = as.integer(dist_miss == 1 & d_missing_avail == 1),
      d_d  = as.integer(dist_miss == 1 & d_missing_avail == 0),
      d_c  = as.integer(dist_miss == 0 & d_missing_avail == 1),

      # For utility: NA -> 0
      mean_avail = tidyr::replace_na(mean_avail_raw, 0)
    ) %>%
    select(-mean_avail_raw, -dist_miss)
}

# =====================================================================
# 4) Helpers: build wide Apollo database (long -> wide) given alternatives
# =====================================================================

build_database <- function(long_data_spec,
                           alts,
                           id_vars = c("fished_haul_anon","fished_vessel_anon","set_date"),
                           case_vars = c("unem_rate","weekend","psdnclosure","btnaclosure","msqdclosure","waclosure","dcrbclosurewad"),
                           alt_vars  = c("fished","mean_avail","mean_price","mean_price_3","wind_max_220_mh","dist_to_cog",
                                         "dist_port_to_catch_area_zero","dummy_prev_days","dummy_prev_year_days","d_d","d_cd","d_c")){

  # 1) Make case-level data (first non-missing by case)
  case_keep <- intersect(case_vars, names(long_data_spec))

  case_data <- long_data_spec %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(across(all_of(case_keep), ~ dplyr::first(.x)), .groups="drop")

  # 2) Alt-level data
  alt_keep <- intersect(alt_vars, names(long_data_spec))

  alt_data <- long_data_spec %>%
    select(all_of(id_vars), selection, all_of(alt_keep)) %>%
    pivot_wider(
      id_cols     = all_of(id_vars),
      names_from  = selection,
      values_from = all_of(alt_keep)
    )

  database <- alt_data %>% left_join(case_data, by=id_vars)

  # 3) Build choice index using fished_*
  fished_cols <- paste0("fished_", alts)
  missing_cols <- setdiff(fished_cols, names(database))
  if(length(missing_cols) > 0) database[missing_cols] <- 0

  database$choice <- apply(as.matrix(database[, fished_cols]), 1, function(r){
    k <- which(r == 1)[1]
    if(length(k)==0 || is.na(k)) return(NA_integer_)
    k
  })

  database %>%
    mutate(choice = as.integer(choice)) %>%
    arrange(fished_vessel_anon, fished_haul_anon)
}

# =====================================================================
# 5) Cluster configs: specify (i) data path, (ii) alternatives, (iii) model code
#     IMPORTANT: only "config" differs across clusters; pipeline is shared.
# =====================================================================

cluster_configs <- list(

  # -------------------------
  # Cluster c4 (your final code)
  # -------------------------
  c4 = list(
    data_rds = paste0(google_dir, "Data/Anonymised data/part_model_c4.rds"),

    # Alternatives order must match your nl_settings$alternatives mapping
    alts = c(
      "sfa_nanc","laa_nanc","laa_cmck","laa_msqd","laa_ytna",
      "mna_msqd","sba_msqd","laa_btna","sfa_msqd","mna_psdn",
      "sba_cmck","mra_msqd","laa_psdn","mna_nanc","no_participation"
    ),

    # Cluster-specific: functions that define apollo_beta/apollo_fixed/apollo_probabilities
    # We keep them in one place so c5/c6/c7 can be copied cleanly.
    model_builder = function(){

      apollo_beta=c(
        asc_sfa_nanc=-2.75, asc_laa_nanc=-2.75, asc_laa_cmck=-2.75, asc_laa_msqd=-2.75,
        asc_laa_ytna=-2.75, asc_mna_msqd=-2.75, asc_sba_msqd=-2.75, asc_laa_btna=-2.75,
        asc_sfa_msqd=-2.75, asc_mna_psdn=-2.75, asc_sba_cmck=-2.75, asc_mra_msqd=-2.75,
        asc_laa_psdn=-2.75, asc_mna_nanc=-2.75,

        c_psdn=-0.4,
        w_nanc=-4, w_cmck=-4, w_msqd=-4, w_tuna=-4, w_psdn=-4,

        theta_part=0, theta_cmck=12, theta_msqd=0, theta_psdn=0, theta_nanc=0, theta_tuna=12,

        B_mean_avail=1.15, B_mean_price=0.3, B_wind_max_220_mh=-0.05,
        B_d_d=-1, B_d_cd=-1, B_d_c=0,
        B_dist_port_to_catch_area_zero=-0.005,
        B_dummy_prev_days=1.2, B_dummy_prev_year_days=0.15,
        B_unem_rate_part=0.2,
        asc_no_participation=0, B_unem_rate_nopart=0, w_nopart=0,
        B_dist_to_cog=-0.001
      )

      apollo_fixed = c("asc_no_participation","B_unem_rate_nopart","w_nopart","theta_cmck","theta_tuna")

      apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

        apollo_attach(apollo_beta, apollo_inputs)
        on.exit(apollo_detach(apollo_beta, apollo_inputs))

        P = list()
        V = list()

        lambda_part <- 1/(1 + exp(-theta_part))
        lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
        lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
        lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
        lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
        lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))

        V[["sfa_nanc"]]         = asc_sfa_nanc + w_nanc*weekend + B_mean_avail*mean_avail_sfa_nanc + B_mean_price*mean_price_sfa_nanc + B_wind_max_220_mh*wind_max_220_mh_sfa_nanc + B_d_d*d_d_sfa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days*dummy_prev_days_sfa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_sfa_nanc + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_sfa_nanc + B_d_c*d_c_sfa_nanc
        V[["laa_nanc"]]         = asc_laa_nanc + w_nanc*weekend + B_mean_avail*mean_avail_laa_nanc + B_mean_price*mean_price_laa_nanc + B_wind_max_220_mh*wind_max_220_mh_laa_nanc + B_d_d*d_d_laa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days*dummy_prev_days_laa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_laa_nanc + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_nanc + B_d_c*d_c_laa_nanc
        V[["laa_cmck"]]         = asc_laa_cmck + w_cmck*weekend + B_mean_avail*mean_avail_laa_cmck + B_mean_price*mean_price_laa_cmck + B_wind_max_220_mh*wind_max_220_mh_laa_cmck + B_d_d*d_d_laa_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days*dummy_prev_days_laa_cmck + B_dummy_prev_year_days*dummy_prev_year_days_laa_cmck + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_cmck + B_d_c*d_c_laa_cmck
        V[["laa_msqd"]]         = asc_laa_msqd + w_msqd*weekend + B_mean_avail*mean_avail_laa_msqd + B_mean_price*mean_price_laa_msqd + B_wind_max_220_mh*wind_max_220_mh_laa_msqd + B_d_d*d_d_laa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days*dummy_prev_days_laa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_laa_msqd + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_msqd + B_d_c*d_c_laa_msqd
        V[["laa_ytna"]]         = asc_laa_ytna + w_tuna*weekend + B_mean_avail*mean_avail_laa_ytna + B_mean_price*mean_price_laa_ytna + B_wind_max_220_mh*wind_max_220_mh_laa_ytna + B_d_d*d_d_laa_ytna + B_d_cd*d_cd_laa_ytna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days*dummy_prev_days_laa_ytna + B_dummy_prev_year_days*dummy_prev_year_days_laa_ytna + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_ytna + B_d_c*d_c_laa_ytna
        V[["mna_msqd"]]         = asc_mna_msqd + w_msqd*weekend + B_mean_avail*mean_avail_mna_msqd + B_mean_price*mean_price_mna_msqd + B_wind_max_220_mh*wind_max_220_mh_mna_msqd + B_d_d*d_d_mna_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days*dummy_prev_days_mna_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mna_msqd + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_mna_msqd + B_d_c*d_c_mna_msqd
        V[["sba_msqd"]]         = asc_sba_msqd + w_msqd*weekend + B_mean_avail*mean_avail_sba_msqd + B_mean_price*mean_price_sba_msqd + B_wind_max_220_mh*wind_max_220_mh_sba_msqd + B_d_d*d_d_sba_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days*dummy_prev_days_sba_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sba_msqd + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_sba_msqd + B_d_c*d_c_sba_msqd
        V[["laa_btna"]]         = asc_laa_btna + w_tuna*weekend + B_mean_avail*mean_avail_laa_btna + B_mean_price*mean_price_laa_btna + B_wind_max_220_mh*wind_max_220_mh_laa_btna + B_d_d*d_d_laa_btna + B_d_cd*d_cd_laa_btna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days*dummy_prev_days_laa_btna + B_dummy_prev_year_days*dummy_prev_year_days_laa_btna + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_btna + B_d_c*d_c_laa_btna
        V[["sfa_msqd"]]         = asc_sfa_msqd + w_msqd*weekend + B_mean_avail*mean_avail_sfa_msqd + B_mean_price*mean_price_sfa_msqd + B_wind_max_220_mh*wind_max_220_mh_sfa_msqd + B_d_d*d_d_sfa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days*dummy_prev_days_sfa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sfa_msqd + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_sfa_msqd + B_d_c*d_c_sfa_msqd
        V[["mna_psdn"]]         = asc_mna_psdn + w_psdn*weekend + c_psdn*psdnclosure + B_mean_avail*mean_avail_mna_psdn + B_mean_price*mean_price_mna_psdn + B_wind_max_220_mh*wind_max_220_mh_mna_psdn + B_d_d*d_d_mna_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days*dummy_prev_days_mna_psdn + B_dummy_prev_year_days*dummy_prev_year_days_mna_psdn + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_mna_psdn + B_d_c*d_c_mna_psdn
        V[["sba_cmck"]]         = asc_sba_cmck + w_cmck*weekend + B_mean_avail*mean_avail_sba_cmck + B_mean_price*mean_price_sba_cmck + B_wind_max_220_mh*wind_max_220_mh_sba_cmck + B_d_d*d_d_sba_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days*dummy_prev_days_sba_cmck + B_dummy_prev_year_days*dummy_prev_year_days_sba_cmck + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_sba_cmck + B_d_c*d_c_sba_cmck
        V[["mra_msqd"]]         = asc_mra_msqd + w_msqd*weekend + B_mean_avail*mean_avail_mra_msqd + B_mean_price*mean_price_mra_msqd + B_wind_max_220_mh*wind_max_220_mh_mra_msqd + B_d_d*d_d_mra_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days*dummy_prev_days_mra_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mra_msqd + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_mra_msqd + B_d_c*d_c_mra_msqd
        V[["laa_psdn"]]         = asc_laa_psdn + w_psdn*weekend + c_psdn*psdnclosure + B_mean_avail*mean_avail_laa_psdn + B_mean_price*mean_price_laa_psdn + B_wind_max_220_mh*wind_max_220_mh_laa_psdn + B_d_d*d_d_laa_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days*dummy_prev_days_laa_psdn + B_dummy_prev_year_days*dummy_prev_year_days_laa_psdn + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_laa_psdn + B_d_c*d_c_laa_psdn
        V[["mna_nanc"]]         = asc_mna_nanc + w_nanc*weekend + B_mean_avail*mean_avail_mna_nanc + B_mean_price*mean_price_mna_nanc + B_wind_max_220_mh*wind_max_220_mh_mna_nanc + B_d_d*d_d_mna_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days*dummy_prev_days_mna_nanc + B_dummy_prev_year_days*dummy_prev_year_days_mna_nanc + B_unem_rate_part*unem_rate + B_dist_to_cog*dist_to_cog_mna_nanc + B_d_c*d_c_mna_nanc

        V[["no_participation"]] = asc_no_participation + w_nopart*weekend + B_unem_rate_nopart*unem_rate

        nlNests      = list(root=1, part=lambda_part,
                            cmck=lambda_cmck, msqd=lambda_msqd, psdn=lambda_psdn,
                            nanc=lambda_nanc, tuna=lambda_tuna)

        nlStructure= list(
          root = c("no_participation","part"),
          part = c("cmck","msqd","psdn","nanc","tuna"),
          cmck = c("laa_cmck","sba_cmck"),
          msqd = c("laa_msqd","mna_msqd","mra_msqd","sba_msqd","sfa_msqd"),
          psdn = c("laa_psdn","mna_psdn"),
          nanc = c("laa_nanc","mna_nanc","sfa_nanc"),
          tuna = c("laa_ytna","laa_btna")
        )

        avail = list(
          sfa_nanc=1, laa_nanc=1, laa_cmck=1, laa_msqd=1, laa_ytna=1,
          mna_msqd=1, sba_msqd=1,
          laa_btna = 1 - btnaclosure,
          sfa_msqd=1, mna_psdn=1, sba_cmck=1, mra_msqd=1, laa_psdn=1, mna_nanc=1,
          no_participation=1
        )

        nl_settings <- list(
          alternatives = c(
            sfa_nanc=1, laa_nanc=2, laa_cmck=3, laa_msqd=4, laa_ytna=5,
            mna_msqd=6, sba_msqd=7, laa_btna=8, sfa_msqd=9, mna_psdn=10,
            sba_cmck=11, mra_msqd=12, laa_psdn=13, mna_nanc=14, no_participation=15
          ),
          avail=avail, choiceVar=choice, utilities=V, nlNests=nlNests, nlStructure=nlStructure
        )

        P[["model"]] = apollo_nl(nl_settings, functionality)
        P = apollo_panelProd(P, apollo_inputs, functionality)
        P = apollo_prepareProb(P, apollo_inputs, functionality)
        return(P)
      }

      list(apollo_beta=apollo_beta, apollo_fixed=apollo_fixed, apollo_probabilities=apollo_probabilities)
    },

    # if you want "disable d_c by spec" logic like c4:
    use_d_c_by_spec = c(daily=1, MA7=1, MA14=0, MA30=0, t1daily=1)
  )

  # -------------------------
  # TODO: Add c5/c6/c7 blocks here
  #   Copy from your scripts and paste inside model_builder() exactly.
  #   Use the same pattern as c4:
  #     data_rds, alts, model_builder, (optional) use_d_c_by_spec
  # -------------------------
)

# =====================================================================
# 6) Global SDM + CPUE merge (done ONCE), then split by cluster/spec
# =====================================================================

# SDM files (EDIT if different)
sdm_files <- list(
  MSQD = list(path="SDMs/sdm_msqd.rds", col="MSQD_SDM_90"),
  PSDN = list(path="SDMs/sdm_psdn.rds", col="PSDN_SDM_60"),
  NANC = list(path="SDMs/sdm_nanc.rds", col="NANC_SDM_60"),
  JMCK = list(path="SDMs/sdm_jmck.rds", col="JMCK_SDM_60"),
  CMCK = list(path="SDMs/sdm_cmck.rds", col="CMCK_SDM_60"),
  PHRG = list(path="SDMs/sdm_phrg.rds", col="PHRG_SDM_20"),
  ALBC = list(path="SDMs/sdm_albc.rds", col="albc_SDM_90")
)

message("Preparing SDMs...")
sdm_list <- purrr::imap(sdm_files, ~ prepare_sdm(.x$path, .y, .x$col))
sdm_all  <- Reduce(function(x,y) left_join(x,y, by=c("PORT_AREA_CODE","date")), sdm_list)

message("Preparing CPUE...")
cpue_expanded <- prepare_cpue(file.path(project_dir, "R/CPUE_index.rds"))

# =====================================================================
# 7) Load clusters -> bind -> merge SDMs/CPUE -> create availability cols
# =====================================================================

load_cluster_long <- function(cfg){
  stopifnot(file.exists(cfg$data_rds))
  readRDS(cfg$data_rds) %>%
    # keep minimal shared columns (allow extra; we intersect later)
    select(
      set_date, fished_haul_anon, fished_vessel_anon, selection, fished,
      mean_price, mean_price_3, wind_max_220_mh, dist_to_cog, dist_port_to_catch_area_zero,
      psdnclosure, btnaclosure, msqdclosure, waclosure, dcrbclosurewad,
      dummy_prev_days, dummy_prev_year_days, unem_rate, d_d, d_cd, d_c, weekend
    ) %>%
    standardize_long()
}

message("Loading and stacking clusters...")
long_all <- purrr::imap_dfr(cluster_configs, ~ mutate(load_cluster_long(.x), cluster=.y))

message("Merging SDMs and CPUE...")
long_all2 <- long_all %>%
  left_join(sdm_all, by=c("PORT_AREA_CODE","date")) %>%
  left_join(cpue_expanded, by=c("PORT_AREA_CODE","date","sp4"="Species_Dominant")) %>%
  mutate(
    CPUE_avail_t1      = coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
    used_90d_fallback  = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1))
  )

message("Building availability columns...")
long_all2 <- build_avail_columns(long_all2)

# =====================================================================
# 8) Build databases per cluster per spec (daily/MA7/MA14/MA30/t1daily)
# =====================================================================

message("Building databases (wide) per cluster/spec...")
databases_by_cluster <- list()

for(cl in names(cluster_configs)){
  cfg <- cluster_configs[[cl]]

  long_cl <- long_all2 %>% filter(cluster == cl)

  # Build data_by_avail list
  data_by_avail <- lapply(avail_specs, function(v){
    prepare_avail_spec(long_cl, v)
  })

  # Build wide databases
  keep_cols <- c("fished_haul_anon","fished_vessel_anon","set_date",
                 "selection","fished","mean_avail","mean_price","mean_price_3",
                 "wind_max_220_mh","dist_to_cog","dist_port_to_catch_area_zero",
                 "psdnclosure","btnaclosure","msqdclosure","waclosure","dcrbclosurewad",
                 "dummy_prev_days","dummy_prev_year_days","unem_rate",
                 "d_d","d_cd","d_c","weekend")

  dbs <- lapply(names(data_by_avail), function(nm){
    build_database(data_by_avail[[nm]] %>% select(any_of(keep_cols)), cfg$alts)
  })
  names(dbs) <- names(data_by_avail)

  databases_by_cluster[[cl]] <- dbs
}

# =====================================================================
# 9) Run models: per cluster, estimate 5 specs, store LL/AIC/BIC
# =====================================================================

has_dc_variation <- function(db){
  dc_cols <- grep("^d_c_", names(db), value=TRUE)
  if(length(dc_cols)==0) return(FALSE)
  any(sapply(dc_cols, function(v) any(db[[v]]==1, na.rm=TRUE)))
}

models <- list()
comp_all <- list()

for(cl in names(cluster_configs)){

  cfg <- cluster_configs[[cl]]
  built <- cfg$model_builder()

  # Base apollo objects for this cluster
  apollo_beta_base <- built$apollo_beta
  apollo_fixed_base <- built$apollo_fixed
  apollo_probabilities <- built$apollo_probabilities

  # Optionally use-d_c toggles by spec (default: always on)
  use_d_c_by_spec <- cfg$use_d_c_by_spec
  if(is.null(use_d_c_by_spec)){
    use_d_c_by_spec <- setNames(rep(1, length(avail_specs)), names(avail_specs))
  }

  models[[cl]] <- list()

  for(spec in names(databases_by_cluster[[cl]])){

    apollo_control$modelName <- paste0("NL_", cl, "_", spec)
    database <- databases_by_cluster[[cl]][[spec]]

    # Decide if d_c can be estimated or must be fixed (if no variation or turned off)
    dc_ok <- has_dc_variation(database) && (use_d_c_by_spec[[spec]] == 1)

    apollo_fixed_spec <- apollo_fixed_base
    if(!dc_ok) apollo_fixed_spec <- unique(c(apollo_fixed_spec, "B_d_c"))
    apollo_fixed <- apollo_fixed_spec  # IMPORTANT: must be global before validateInputs

    # Validate
    apollo_inputs <- apollo_validateInputs()

    # Pass a simple scalar flag (prevents multicore splitting warnings: it's not obs-specific)
    apollo_inputs$use_d_c <- as.numeric(dc_ok)

    # Starting values consistent with fixed
    apollo_beta_spec <- apollo_readBeta(apollo_beta_base, apollo_fixed, "NL_participation_model", overwriteFixed=FALSE)

    cat("\n==============================\n",
        "Estimating:", cl, " spec:", spec, "\n",
        "dc_ok:", dc_ok, " | B_d_c fixed:", ("B_d_c" %in% apollo_fixed), "\n",
        "==============================\n")

    model_spec <- apollo_estimate(apollo_beta_spec, apollo_fixed,
                                 apollo_probabilities, apollo_inputs,
                                 estimate_settings=list())

    apollo_modelOutput(model_spec)
    apollo_saveOutput(model_spec, saveOutput_settings=list(printT1=1))

    models[[cl]][[spec]] <- model_spec
  }

  # Compare specs within cluster
  comp <- data.frame(
    cluster = cl,
    spec    = names(models[[cl]]),
    LL      = sapply(models[[cl]], function(m) m$maximum),
    k       = sapply(models[[cl]], function(m) length(m$estimate)),
    AIC     = sapply(models[[cl]], function(m) 2*length(m$estimate) - 2*m$maximum),
    BIC     = sapply(models[[cl]], function(m) log(models[[cl]][[1]]$nObs)*length(m$estimate) - 2*m$maximum)
  )
  comp_all[[cl]] <- comp
  print(comp[order(comp$AIC), ])
}

comp_all_df <- bind_rows(comp_all)
write.csv(comp_all_df, file=file.path(project_dir, "R/output/NL_specs_comparison_all_clusters.csv"), row.names=FALSE)

message("DONE. Saved comparison table to: R/output/NL_specs_comparison_all_clusters.csv")
