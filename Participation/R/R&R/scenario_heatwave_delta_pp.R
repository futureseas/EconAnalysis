############################################################
# Scenario: Heatwave-like SDM availability redistribution (daily)
# Output: Δ probabilities (pp) for participation, species, and port by segment
#
# What this script does:
# 1) Loads port coordinates (port_areas.csv)
# 2) Loads SDMs (daily) and merges into sdm_all (PORT_AREA_CODE x date)
# 3) For each cluster (c4–c7):
#    - Rebuilds long_data2 exactly like your run_cluster() pre-processing
#    - Builds two scenario datasets:
#        (a) BASELINE availability = mean SDM availability by port over 2013–2014
#        (b) HEATWAVE availability = mean SDM availability by port over an objective "peak northward shift" window in 2016
#    - Rebuilds wide databases (same alts/case_vars) for BASELINE and HEATWAVE
#    - Uses your already-estimated DAILY model (res_cX$models$daily$model$estimate) to predict probabilities
#    - Aggregates probabilities to: participation, species (ports aggregated), port (species aggregated), and alternative
#    - Writes results to CSV/XLSX
#
# Assumptions:
# - You have these files in your working directory (as in your pipeline):
#     res_c4.rds, res_c5.rds, res_c6.rds, res_c7.rds
#     apollo_probabilities_c4.rds ... apollo_probabilities_c7.rds  (optional; you can source R scripts instead)
# - You have the anonymised long RDS for each cluster:
#     G:/Mi unidad/Data/Anonymised data/part_model_c4.rds  etc.
# - SDM RDS files exist in sdm_dir (SDMs/): sdm_msqd.rds, sdm_psdn.rds, ...
#
# Notes:
# - "Δpp" reported as (heatwave - baseline) * 100 (percentage points).
# - Baseline period default = 2013–2014; peak window default = 60 days in 2016.
############################################################

rm(list=ls())

library(apollo)
library(tidyverse)
library(slider)
library(lubridate)
library(readr)
library(writexl)

apollo_initialise()

# -----------------------------
# USER PATHS (edit if needed)
# -----------------------------
setwd("C:/GitHub/EconAnalysis/Participation")
google_dir <- "H:/My Drive"             # adjust if needed
sdm_dir    <- "SDMs"
cpue_rds   <- "C:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds"

# Port coordinates file (you uploaded this)
port_csv_path <- "C:/GitHub/EconAnalysis/Data/Ports/port_areas.csv"

# Cluster data files (anonymised)
cluster_rds <- list(
  c4 = file.path(google_dir, "Data/Anonymised data/part_model_c4.rds"),
  c5 = file.path(google_dir, "Data/Anonymised data/part_model_c5.rds"),
  c6 = file.path(google_dir, "Data/Anonymised data/part_model_c6.rds"),
  c7 = file.path(google_dir, "Data/Anonymised data/part_model_c7.rds")
)

# Scenario settings
baseline_start <- as.Date("2013-01-01")
baseline_end   <- as.Date("2014-12-31")
peak_year      <- 2016
window_days    <- 60
anchor_spec    <- "MSQD"   # anchor species to pick the "max northward" window

sdm_species <- c("MSQD","PSDN","NANC","JMCK","CMCK","PHRG","ALBC")
no_sdm_species <- c("BTNA","YTNA","DCRB","SOCK")  # your CPUE fallback species

# -----------------------------
# Load port coordinates -> port_lookup
# -----------------------------
port_lookup <- read_csv(port_csv_path, show_col_types = FALSE) %>%
  transmute(
    PORT_AREA_CODE = toupper(port_group_code),
    lat = lat,
    lon = lon
  ) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  distinct(PORT_AREA_CODE, .keep_all = TRUE)

# Quick sanity
message("Port coords loaded: ", nrow(port_lookup), " rows.")
if(any(port_lookup$lat < 0, na.rm=TRUE)) warning("Negative latitude found -> check sign.")
if(any(port_lookup$lat < 30 | port_lookup$lat > 55, na.rm=TRUE)) warning("Lat outside expected West Coast range (~32–49).")

# -----------------------------
# SDM helpers (your function; kept intact)
# -----------------------------
prepare_sdm <- function(sdm_rds, species_code, sdm_col, port_var="PORT_AREA_CODE"){
  stopifnot(file.exists(sdm_rds))
  sdm <- readRDS(sdm_rds) %>%
    mutate(date = if(!"date" %in% names(.)) make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) else as.Date(date)) %>%
    arrange(.data[[port_var]], date) %>%
    group_by(.data[[port_var]]) %>%
    complete(date = seq(min(date, na.rm=TRUE), max(date, na.rm=TRUE), by="day")) %>%
    arrange(date) %>%
    mutate(
      !!paste0(species_code,"_daily") := .data[[sdm_col]],
      .xlag := lag(.data[[sdm_col]], 1),
      !!paste0(species_code,"_t1") := .xlag,
      !!paste0(species_code,"_MA7_t1")  := slide_dbl(.xlag, ~mean(.x, na.rm=TRUE), .before=6),
      !!paste0(species_code,"_MA14_t1") := slide_dbl(.xlag, ~mean(.x, na.rm=TRUE), .before=13),
      !!paste0(species_code,"_MA30_t1") := slide_dbl(.xlag, ~mean(.x, na.rm=TRUE), .before=29)
    ) %>%
    ungroup() %>%
    select(.data[[port_var]], date, starts_with(paste0(species_code,"_"))) %>%
    select(-any_of(".xlag"))
  return(sdm)
}

# ---- SDM (global) ----
MSQD_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_msqd.rds"), "MSQD", "MSQD_SDM_90")
PSDN_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_psdn.rds"), "PSDN", "PSDN_SDM_60")
NANC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_nanc.rds"), "NANC", "NANC_SDM_60")
JMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_jmck.rds"), "JMCK", "JMCK_SDM_60")
CMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_cmck.rds"), "CMCK", "CMCK_SDM_60")
PHRG_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_phrg.rds"), "PHRG", "PHRG_SDM_20")
ALBC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_albc.rds"), "ALBC", "albc_SDM_90")

sdm_all <- Reduce(function(x,y) left_join(x,y, by=c("PORT_AREA_CODE","date")),
                  list(MSQD_sdm, PSDN_sdm, NANC_sdm, JMCK_sdm, CMCK_sdm, PHRG_sdm, ALBC_sdm))

# -----------------------------
# CPUE fallback (as in your pipeline)
# -----------------------------
cpue_raw <- readRDS(cpue_rds) %>%
  mutate(date = make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  select(PORT_AREA_CODE, Species_Dominant, date, CPUE_index)

cpue_expanded <- cpue_raw %>%
  group_by(PORT_AREA_CODE, Species_Dominant) %>%
  complete(date = seq(min(date), max(date), by="day")) %>%
  ungroup() %>%
  arrange(PORT_AREA_CODE, Species_Dominant, date) %>%
  group_by(PORT_AREA_CODE, Species_Dominant) %>%
  mutate(
    CPUE_MA30_t1 = slide_dbl(lag(CPUE_index,1), ~ if(all(is.na(.x))) NA_real_ else mean(.x, na.rm=TRUE), .before=29),
    CPUE_MA90_t1 = slide_dbl(lag(CPUE_index,1), ~ if(all(is.na(.x))) NA_real_ else mean(.x, na.rm=TRUE), .before=89)
  ) %>%
  ungroup()

# -----------------------------
# Your availability recode (unchanged)
# -----------------------------
prepare_avail_spec <- function(data, avail_var,
                               dist_var="dist_port_to_catch_area_zero",
                               dist_zero_is_missing=TRUE){
  data %>%
    mutate(
      mean_avail_raw   = .data[[avail_var]],
      d_missing_avail  = as.integer(is.na(mean_avail_raw)),
      dist_miss = case_when(
        is.na(.data[[dist_var]]) ~ 1L,
        dist_zero_is_missing & .data[[dist_var]] == 0 ~ 1L,
        TRUE ~ 0L
      ),
      d_cd = as.integer(dist_miss==1 & d_missing_avail==1),
      d_d  = as.integer(dist_miss==1 & d_missing_avail==0),
      d_c  = as.integer(dist_miss==0 & d_missing_avail==1),
      mean_avail = replace_na(mean_avail_raw, 0)
    ) %>%
    select(-mean_avail_raw, -dist_miss)
}

# -----------------------------
# Your wide builder (unchanged)
# -----------------------------
build_database <- function(long_data_spec, alts, case_vars){
  id_vars <- c("fished_haul_anon","fished_vessel_anon","set_date")

  case_data <- long_data_spec %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(across(all_of(case_vars), ~ first(.x)), .groups="drop")

  alt_data <- long_data_spec %>%
    select(all_of(id_vars), selection, any_of(c("fished", "mean_avail", "mean_price", "mean_price_3", "wind_max_220_mh", "dist_to_cog",
           "dist_port_to_catch_area_zero", "dummy_prev_days", "dummy_prev_year_days",
           "d_d", "d_cd", "d_c", "unem_rate", "waclosure", "dcrbclosurewad"))) %>%
    pivot_wider(
      id_cols = all_of(id_vars),
      names_from  = selection,
      values_from = any_of(c("fished", "mean_avail", "mean_price", "mean_price_3", "wind_max_220_mh", "dist_to_cog",
                             "dist_port_to_catch_area_zero", "dummy_prev_days", "dummy_prev_year_days",
                             "d_d", "d_cd", "d_c", "unem_rate", "waclosure", "dcrbclosurewad"))
    )

  database <- alt_data %>% left_join(case_data, by=id_vars)

  fished_cols <- paste0("fished_", alts)
  missing_cols <- setdiff(fished_cols, names(database))
  if(length(missing_cols)>0) database[missing_cols] <- 0

  database$choice <- apply(as.matrix(database[, fished_cols]), 1, function(r){
    k <- which(r==1)[1]
    if(length(k)==0 || is.na(k)) return(NA_integer_)
    k
  })

  database %>% mutate(choice=as.integer(choice)) %>% arrange(fished_vessel_anon, fished_haul_anon)
}

# =========================================================
# SCENARIO WINDOW (objective): max northward shift in 2016
# =========================================================
sdm_centroid_anchor <- function(sdm_all, species="MSQD", port_lookup){
  colA <- paste0(species, "_daily")
  stopifnot(colA %in% names(sdm_all))
  sdm_all %>%
    select(PORT_AREA_CODE, date, all_of(colA)) %>%
    left_join(port_lookup, by="PORT_AREA_CODE") %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    group_by(date) %>%
    summarise(
      lat_c = weighted.mean(lat, w=.data[[colA]], na.rm=TRUE),
      lon_c = weighted.mean(lon, w=.data[[colA]], na.rm=TRUE),
      .groups="drop"
    )
}

pick_peak_window <- function(centroid_df,
                             baseline_start, baseline_end,
                             peak_year=2016,
                             window_days=60){
  base <- centroid_df %>% filter(date >= baseline_start, date <= baseline_end)
  lat0 <- mean(base$lat_c, na.rm=TRUE)
  lon0 <- mean(base$lon_c, na.rm=TRUE)

  df <- centroid_df %>%
    filter(lubridate::year(date) == peak_year) %>%
    arrange(date) %>%
    mutate(
      shift = as.numeric(scale(lat_c - lat0)) + as.numeric(scale(lon_c - lon0)),
      shift_roll = slide_dbl(shift, mean, .before=window_days-1, .complete=TRUE),
      end_date = date,
      start_date = date - (window_days-1)
    )

  best <- df %>% slice_max(shift_roll, n=1, with_ties=FALSE)
  list(start_date = best$start_date,
       end_date   = best$end_date,
       window_days = window_days,
       best_row = best)
}

cent <- sdm_centroid_anchor(sdm_all, species=anchor_spec, port_lookup=port_lookup)
win  <- pick_peak_window(cent, baseline_start, baseline_end, peak_year=peak_year, window_days=window_days)
message("Peak window (objective): ", win$start_date, " to ", win$end_date,
        " (", win$window_days, " days) | anchor=", anchor_spec)

# =========================================================
# Period means by PORT_AREA_CODE × species (daily)
# =========================================================
sdm_period_means <- function(sdm_all, start_date, end_date, species_codes){
  cols <- paste0(species_codes, "_daily")
  stopifnot(all(cols %in% names(sdm_all)))
  sdm_all %>%
    filter(date >= start_date, date <= end_date) %>%
    select(PORT_AREA_CODE, date, all_of(cols)) %>%
    pivot_longer(cols = all_of(cols), names_to="spec_col", values_to="avail") %>%
    mutate(sp4 = gsub("_daily$","", spec_col)) %>%
    group_by(PORT_AREA_CODE, sp4) %>%
    summarise(avail_mean = mean(avail, na.rm=TRUE), .groups="drop")
}

base_means <- sdm_period_means(sdm_all, baseline_start, baseline_end, sdm_species) %>%
  rename(avail_base = avail_mean)

hw_means <- sdm_period_means(sdm_all, win$start_date, win$end_date, sdm_species) %>%
  rename(avail_hw = avail_mean)

# =========================================================
# Prediction helpers
# =========================================================
safe_get_Pmodel <- function(P){
  # Tries to extract the alt-by-row probability object
  if(is.list(P) && !is.null(P$model)) return(P$model)
  if(is.matrix(P) || is.data.frame(P)) return(P)
  stop("Could not extract P$model from apollo_probabilities output.")
}

aggregate_probs <- function(Pmat, alts){
  # Pmat: matrix/data.frame with columns named as alternatives
  Pmat <- as.data.frame(Pmat)
  # Ensure columns exist
  miss <- setdiff(alts, names(Pmat))
  if(length(miss)>0) stop(paste("Missing alt cols in P:", paste(miss, collapse=", ")))

  # participation
  p_part <- 1 - Pmat[["no_participation"]]

  # species = suffix after last underscore; ports aggregated
  sp <- ifelse(alts=="no_participation", "NOPART", toupper(sub(".*_", "", alts)))
  # port = prefix before first underscore
  prt <- ifelse(alts=="no_participation", "NOPART", toupper(sub("_.*", "", alts)))

  # Species probs per obs
  Psp <- map_dfc(unique(sp[sp!="NOPART"]), function(s){
    cols <- alts[sp==s]
    tibble(!!s := rowSums(Pmat[, cols, drop=FALSE]))
  })

  # Port probs per obs
  Pport <- map_dfc(unique(prt[prt!="NOPART"]), function(p){
    cols <- alts[prt==p]
    tibble(!!p := rowSums(Pmat[, cols, drop=FALSE]))
  })

  list(
    alt_mean = colMeans(Pmat[, alts, drop=FALSE]),
    part_mean = mean(p_part),
    sp_mean = colMeans(Psp),
    port_mean = colMeans(Pport)
  )
}

predict_on_database <- function(model_obj, prob_fun, database_wide, apollo_control, use_d_c_val=0){
  # Use estimated parameters from model_obj
  beta_hat <- model_obj$estimate

  # Build apollo inputs for prediction
  assign("database", database_wide, envir = .GlobalEnv)
  assign("apollo_control", apollo_control, envir = .GlobalEnv)

  # To validate inputs, apollo_beta must exist
  assign("apollo_beta", beta_hat, envir = .GlobalEnv)
  assign("apollo_fixed", character(0), envir = .GlobalEnv)  # <- clave

  apollo_inputs <- apollo_validateInputs()
  apollo_inputs$use_d_c <- as.numeric(use_d_c_val)

  P <- prob_fun(beta_hat, apollo_inputs, functionality="prediction")
  Pmat <- safe_get_Pmodel(P)
  return(Pmat)
}

# =========================================================
# Cluster metadata (alts + case_vars)
# IMPORTANT: alts must match your nl_settings names
# =========================================================
cluster_meta <- list(
  c4 = list(
    alts=c("sfa_nanc","laa_nanc","laa_cmck","laa_msqd","laa_ytna","mna_msqd","sba_msqd","laa_btna",
           "sfa_msqd","mna_psdn","sba_cmck","mra_msqd","laa_psdn","mna_nanc","no_participation"),
    case_vars=c("weekend","psdnclosure","btnaclosure")
  ),
  c5 = list(
    alts=c("mna_msqd","sba_msqd","mra_msqd","laa_msqd","npa_msqd","sfa_msqd","cba_msqd",
           "laa_psdn","clo_psdn","cwa_psdn","clw_psdn","sba_cmck","laa_cmck","laa_nanc","cwa_albc",
           "cwa_dcrb","clw_dcrb","no_participation"),
    case_vars=c("weekend","psdnclosure","msqdclosure")
  ),
  c6 = list(
    alts=c("cba_psdn","clo_psdn","clw_psdn","cwa_psdn","clo_nanc","clw_nanc","cwa_nanc",
           "clo_cmck","cwa_dcrb","nps_sock","no_participation"),
    case_vars=c("weekend","psdnclosure","msqdclosure")
  ),
  c7 = list(
    alts=c("laa_cmck","mna_cmck","laa_jmck","mna_jmck","laa_msqd","mra_msqd","sba_msqd","sfa_msqd",
           "mna_msqd","laa_psdn","mna_psdn","mna_nanc","sba_nanc","sda_nanc","no_participation"),
    case_vars=c("weekend","psdnclosure","msqdclosure")
  )
)

# =========================================================
# Load estimated models + probability functions
# =========================================================
res_c4 <- readRDS("res_c4.rds")
res_c5 <- readRDS("res_c5.rds")
res_c6 <- readRDS("res_c6.rds")
res_c7 <- readRDS("res_c7.rds")

res_list <- list(c4=res_c4, c5=res_c5, c6=res_c6, c7=res_c7)

# If you saved prob-funs as RDS (recommended). Otherwise, source your model scripts.
apollo_probabilities_c4 <- readRDS("apollo_probabilities_c4.rds")
apollo_probabilities_c5 <- readRDS("apollo_probabilities_c5.rds")
apollo_probabilities_c6 <- readRDS("apollo_probabilities_c6.rds")
apollo_probabilities_c7 <- readRDS("apollo_probabilities_c7.rds")

prob_fun_list <- list(
  c4 = apollo_probabilities_c4,
  c5 = apollo_probabilities_c5,
  c6 = apollo_probabilities_c6,
  c7 = apollo_probabilities_c7
)

# =========================================================
# MAIN LOOP: build scenario DBs + predict + deltas
# =========================================================
scenario_results <- list()

for(cl in names(cluster_rds)){
  message("\n==============================")
  message("Cluster: ", cl)
  message("==============================")

  # 1) Read long data and build long_data2 as in run_cluster()
  long_data <- readRDS(cluster_rds[[cl]]) %>%
    select(any_of(c("set_date","fished_haul_anon","fished_vessel_anon","selection","fished",
                    "mean_avail","mean_price","mean_price_3","wind_max_220_mh","dist_to_cog","dist_port_to_catch_area_zero",
                    "psdnclosure","btnaclosure","msqdclosure","waclosure","dcrbclosure","dcrbclosurewad",
                    "dummy_prev_days","dummy_prev_year_days","unem_rate","d_d","d_cd","weekend"))) %>%
    mutate(selection = tolower(gsub("-", "_", selection)),
           date = as.Date(set_date)) %>%
    mutate(
      PORT_AREA_CODE = case_when(selection=="no_participation" ~ "NOPART",
                                 TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))),
      sp4 = case_when(selection=="no_participation" ~ "NOPART",
                      TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$")))
    )

  # Port code coverage check
  miss_ports <- setdiff(unique(long_data$PORT_AREA_CODE[long_data$PORT_AREA_CODE!="NOPART"]), port_lookup$PORT_AREA_CODE)
  if(length(miss_ports)>0){
    warning("Ports in data not in port_lookup: ", paste(miss_ports, collapse=", "),
            "\n-> These observations will still run; but centroid/shift selection ignores missing coords.")
  }
  
  long_data <- long_data %>%
    filter(set_date >= baseline_start, set_date <= baseline_end)
  
  # sanity
  message("Prediction sample range: ",
          min(long_data$set_date, na.rm=TRUE), " to ",
          max(long_data$set_date, na.rm=TRUE),
          " | n=", nrow(long_data))

  long_data2 <- long_data %>%
    left_join(sdm_all, by = c("PORT_AREA_CODE", "date")) %>%
    left_join(cpue_expanded, by = c("PORT_AREA_CODE", "date", "sp4" = "Species_Dominant")) %>%
    mutate(
      CPUE_avail_t1 = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
      used_90d_fallback = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1)),
      is_CPUE = as.integer(sp4 %in% no_sdm_species)
    ) %>%
    mutate(
      mean_avail_daily = case_when(
        sp4=="MSQD" ~ MSQD_daily,
        sp4=="PSDN" ~ PSDN_daily,
        sp4=="NANC" ~ NANC_daily,
        sp4=="JMCK" ~ JMCK_daily,
        sp4=="CMCK" ~ CMCK_daily,
        sp4=="PHRG" ~ PHRG_daily,
        sp4=="ALBC" ~ ALBC_daily,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      )
    )

  # 2) Attach baseline and heatwave port-level means (SDM species only)
  long_scn <- long_data2 %>%
    left_join(base_means, by=c("PORT_AREA_CODE","sp4")) %>%
    left_join(hw_means,   by=c("PORT_AREA_CODE","sp4")) %>%
    mutate(
      mean_avail_daily_base = if_else(sp4 %in% sdm_species, avail_base, mean_avail_daily),
      mean_avail_daily_hw   = if_else(sp4 %in% sdm_species, avail_hw,   mean_avail_daily)
    )

  # 3) Build wide databases for baseline + heatwave (daily only)
  meta <- cluster_meta[[cl]]
  alts <- meta$alts
  case_vars <- meta$case_vars

  long_base_ready <- prepare_avail_spec(long_scn, "mean_avail_daily_base")
  long_hw_ready   <- prepare_avail_spec(long_scn, "mean_avail_daily_hw")

  db_base <- build_database(long_base_ready, alts, case_vars)
  db_hw   <- build_database(long_hw_ready,   alts, case_vars)

  # 4) Predict with estimated DAILY model
  model_daily <- res_list[[cl]]$models[["daily"]]$model
  stopifnot(!is.null(model_daily))

  apollo_control <- res_list[[cl]]$models[["daily"]]$apollo_control
  apollo_control$nCores <- 16
  apollo_control$modelName <- paste0("Scenario_", cl, "_daily_prediction")

  prob_fun <- prob_fun_list[[cl]]

  P_base <- predict_on_database(model_daily, prob_fun, db_base, apollo_control, use_d_c_val=0)
  P_hw   <- predict_on_database(model_daily, prob_fun, db_hw,   apollo_control, use_d_c_val=0)

  agg_base <- aggregate_probs(P_base, alts)
  agg_hw   <- aggregate_probs(P_hw,   alts)

  # 5) Deltas (pp)
  delta_part_pp <- (agg_hw$part_mean - agg_base$part_mean) * 100

  delta_sp_pp <- (agg_hw$sp_mean - agg_base$sp_mean) * 100
  delta_port_pp <- (agg_hw$port_mean - agg_base$port_mean) * 100
  delta_alt_pp <- (agg_hw$alt_mean - agg_base$alt_mean) * 100

  # Make tidy tables
  part_tbl <- tibble(cluster=cl, metric="participation", group="ALL", delta_pp=delta_part_pp)

  sp_tbl <- tibble(cluster=cl, metric="species", group=names(delta_sp_pp),
                   delta_pp=as.numeric(delta_sp_pp)) %>% arrange(desc(abs(delta_pp)))

  port_tbl <- tibble(cluster=cl, metric="port", group=names(delta_port_pp),
                     delta_pp=as.numeric(delta_port_pp)) %>% arrange(desc(abs(delta_pp)))

  alt_tbl <- tibble(cluster=cl, metric="alternative", group=names(delta_alt_pp),
                    delta_pp=as.numeric(delta_alt_pp)) %>% arrange(desc(abs(delta_pp)))

  scenario_results[[cl]] <- list(
    window = win,
    participation = part_tbl,
    species = sp_tbl,
    port = port_tbl,
    alternative = alt_tbl
  )

  message("Done ", cl, ".  Δ participation (pp) = ", round(delta_part_pp, 3))
}

# =========================================================
# Export results
# =========================================================
all_part <- bind_rows(lapply(scenario_results, `[[`, "participation"))
all_sp   <- bind_rows(lapply(scenario_results, `[[`, "species"))
all_port <- bind_rows(lapply(scenario_results, `[[`, "port"))
all_alt  <- bind_rows(lapply(scenario_results, `[[`, "alternative"))

dir.create(file.path("R","output","scenarios"), showWarnings = FALSE, recursive = TRUE)

write_csv(all_part, file.path("R","output","scenarios","scenario_delta_participation_pp.csv"))
write_csv(all_sp,   file.path("R","output","scenarios","scenario_delta_species_pp.csv"))
write_csv(all_port, file.path("R","output","scenarios","scenario_delta_port_pp.csv"))
write_csv(all_alt,  file.path("R","output","scenarios","scenario_delta_alternative_pp.csv"))

writexl::write_xlsx(
  list(
    participation = all_part,
    species = all_sp,
    port = all_port,
    alternative = all_alt,
    scenario_window = tibble(anchor=anchor_spec,
                             baseline_start=baseline_start,
                             baseline_end=baseline_end,
                             peak_year=peak_year,
                             window_days=window_days,
                             window_start=win$start_date,
                             window_end=win$end_date)
  ),
  path = file.path("R","output","scenarios","scenario_heatwave_delta_pp.xlsx")
)

message("\nAll scenario outputs written to: R/output/scenarios/")


# =========================================================
# FIGURE: Heatwave-like scenario — Δpp by species × cluster
# (ports aggregated; pp = percentage points)
# Input: R/output/scenarios/scenario_delta_species_pp.csv
# Output: R/output/scenarios/Fig_heatwave_delta_species_pp.png
# =========================================================

library(tidyverse)

in_file  <- file.path("R","output","scenarios","scenario_delta_species_pp.csv")
out_png  <- file.path("R","output","scenarios","Fig_heatwave_delta_species_pp.png")
out_pdf  <- file.path("R","output","scenarios","Fig_heatwave_delta_species_pp.pdf")

df <- readr::read_csv(in_file, show_col_types = FALSE)

# Espera columnas tipo:
# cluster, level (="species"), group (species), delta_pp, baseline_prob, scenario_prob
# Si tu script usa otros nombres, mira names(df) y ajusta group/delta_pp.

# Limpieza / orden bonito de especies (opcional)
species_order <- c("MSQD","NANC","PSDN","CMCK","JMCK","ALBC","DCRB","SOCK","BTNA","YTNA","PHRG","OTHR","MCKL","NOPART")

df_plot <- df %>%
  mutate(
    cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
    species = toupper(group),
    species = factor(species, levels = intersect(species_order, unique(species)))
  ) %>%
  filter(!is.na(delta_pp)) %>%
  # opcional: muestra solo especies con cambio relevante
  # filter(abs(delta_pp) >= 0.05) %>%
  arrange(cluster, desc(delta_pp))

p <- ggplot(df_plot, aes(x = species, y = delta_pp)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ cluster, ncol = 2, scales = "free_y") +
  labs(
    x = NULL,
    y = expression(Delta*" probability (pp)"),
    title = "Heatwave-like availability redistribution scenario",
    subtitle = "Change in predicted choice probability by target species (ports aggregated)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

ggsave(out_png, p, width = 8.5, height = 6.5, dpi = 300)
ggsave(out_pdf, p, width = 8.5, height = 6.5)

print(p)


# =========================================================
# FIGURE: Heatwave-like scenario — Δpp by port × cluster
# (species aggregated; pp = percentage points)
# Input: R/output/scenarios/scenario_delta_port_pp.csv
# Output: R/output/scenarios/Fig_heatwave_delta_port_pp.png
# =========================================================

library(tidyverse)

in_file <- file.path("R","output","scenarios","scenario_delta_port_pp.csv")
out_png <- file.path("R","output","scenarios","Fig_heatwave_delta_port_pp.png")
out_pdf <- file.path("R","output","scenarios","Fig_heatwave_delta_port_pp.pdf")

df <- readr::read_csv(in_file, show_col_types = FALSE)

# Espera columnas tipo:
# cluster, level (="port"), group (port code), delta_pp, baseline_prob, scenario_prob
# (si tu script usa otros nombres, ajusta "group"/"delta_pp" abajo)

df_plot <- df %>%
  mutate(
    cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
    port = toupper(group)
  ) %>%
  filter(!is.na(delta_pp)) %>%
  # opcional: filtra puertos muy pequeños
  # filter(abs(delta_pp) >= 0.05) %>%
  group_by(cluster) %>%
  mutate(port = fct_reorder(port, delta_pp)) %>%
  ungroup()

p <- ggplot(df_plot, aes(x = port, y = delta_pp)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ cluster, ncol = 2, scales = "free_y") +
  labs(
    x = NULL,
    y = expression(Delta*" probability (pp)"),
    title = "Heatwave-like availability redistribution scenario",
    subtitle = "Change in predicted choice probability by landing port (species aggregated)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

ggsave(out_png, p, width = 8.5, height = 6.5, dpi = 300)
ggsave(out_pdf, p, width = 8.5, height = 6.5)

print(p)


# =========================================================
# FIGURE: Heatwave-like scenario — Δpp by alternative × cluster
# (includes no_participation)
# =========================================================

library(tidyverse)

in_file <- file.path("R","output","scenarios","scenario_delta_alternative_pp.csv")
out_png <- file.path("R","output","scenarios","Fig_heatwave_delta_alternative_pp.png")
out_pdf <- file.path("R","output","scenarios","Fig_heatwave_delta_alternative_pp.pdf")

df <- readr::read_csv(in_file, show_col_types = FALSE)

# Expected columns: cluster, metric=="alternative", group, delta_pp
df_plot <- df %>%
  filter(metric == "alternative") %>%
  mutate(
    cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
    alternative = tolower(group)
  ) %>%
  group_by(cluster) %>%
  mutate(
    rk_pos = rank(-delta_pp, ties.method = "first"),
    rk_neg = rank( delta_pp, ties.method = "first")
  ) %>%
  filter(rk_pos <= 6 | rk_neg <= 6) %>%
  ungroup() %>%
  group_by(cluster) %>%
  mutate(alternative = fct_reorder(alternative, delta_pp)) %>%
  ungroup()

p <- ggplot(df_plot, aes(x = alternative, y = delta_pp)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ cluster, ncol = 2, scales = "free_y") +
  labs(
    x = NULL,
    y = expression(Delta*" probability (pp)"),
    title = "Heatwave-like availability redistribution scenario",
    subtitle = "Change in predicted choice probabilities by alternative (including non-participation)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )

ggsave(out_png, p, width = 9, height = 7, dpi = 300)
ggsave(out_pdf, p, width = 9, height = 7)

print(p)
