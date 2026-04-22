############################################################
# Scenario: Dynamic-lite SDM daily re-mapping (same months) 
# Output: Δ probabilities (pp) for participation, species, port, alternative by cluster
#
# What this script does:
# 1) Loads port coordinates (port_areas.csv)
# 2) Loads SDMs (daily) and merges into sdm_all (PORT_AREA_CODE x date)
# 3) Chooses TWO 90-day windows that are:
#      - the SAME months/days ("same season", same start month-day)
#      - as dissimilar as possible (south vs north) using an anchor species centroid (default MSQD)
#      - across the full 2013–2017 model estimation support (but you can change year range)
# 4) Builds a daily date mapping table: baseline dates (south-year) -> scenario dates (north-year)
# 5) For each cluster (c4–c7):
#    - Rebuilds long_data2 like your run_cluster() pre-processing
#    - Creates BASELINE = original SDM daily availability (as merged by date)
#    - Creates SCENARIO = SDM daily availability re-mapped using the date mapping table
#      (so each baseline day uses the scenario-year SDM from the matched day_idx)
#    - Rebuilds wide databases for BASELINE and SCENARIO
#    - Uses your already-estimated DAILY model to predict probabilities
#    - Aggregates probabilities to participation, species, port, and alternative
#    - Exports CSV/XLSX and makes figures
#
# Notes:
# - "Δpp" reported as (scenario - baseline) * 100 (percentage points).
# - By default, leap day (Feb 29) is dropped so seasons match cleanly.
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
setwd("D:/GitHub/EconAnalysis/Participation")

# Your Google Drive / project paths
google_dir <- "G:/Mi unidad"             # adjust if needed
sdm_dir    <- "SDMs"
cpue_rds   <- "D:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds"

# Port coordinates file
# If you want to use the uploaded CSV in this environment, set:
# port_csv_path <- "/mnt/data/port_areas.csv"
port_csv_path <- "D:/GitHub/EconAnalysis/Data/Ports/port_areas.csv"

# Cluster long data files (anonymised)
cluster_rds <- list(
  c4 = file.path(google_dir, "Data/Anonymised data/part_model_c4.rds"),
  c5 = file.path(google_dir, "Data/Anonymised data/part_model_c5.rds"),
  c6 = file.path(google_dir, "Data/Anonymised data/part_model_c6.rds"),
  c7 = file.path(google_dir, "Data/Anonymised data/part_model_c7.rds")
)

# Model objects (estimated already)
res_c4 <- readRDS("res_c4.rds")
res_c5 <- readRDS("res_c5.rds")
res_c6 <- readRDS("res_c6.rds")
res_c7 <- readRDS("res_c7.rds")
res_list <- list(c4=res_c4, c5=res_c5, c6=res_c6, c7=res_c7)

# Probability functions (either RDS or source your scripts)
apollo_probabilities_c4 <- readRDS("apollo_probabilities_c4.rds")
apollo_probabilities_c5 <- readRDS("apollo_probabilities_c5.rds")
apollo_probabilities_c6 <- readRDS("apollo_probabilities_c6.rds")
apollo_probabilities_c7 <- readRDS("apollo_probabilities_c7.rds")
prob_fun_list <- list(c4=apollo_probabilities_c4, c5=apollo_probabilities_c5,
                      c6=apollo_probabilities_c6, c7=apollo_probabilities_c7)

# -----------------------------
# Scenario settings (dynamic-lite)
# -----------------------------
window_days <- 90
anchor_spec <- "MSQD"   # anchor species to define south vs north season
min_years   <- 3        # minimum years with full-window coverage for a candidate season

# SDM species codes available in your sdm_all
sdm_species <- c("MSQD","PSDN","NANC","JMCK","CMCK","PHRG","ALBC")
no_sdm_species <- c("BTNA","YTNA","DCRB","SOCK")  # CPUE fallback species (if needed)

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

message("Port coords loaded: ", nrow(port_lookup), " rows.")
if(any(port_lookup$lat < 0, na.rm=TRUE)) warning("Negative latitude found -> check sign.")
if(any(port_lookup$lat < 30 | port_lookup$lat > 55, na.rm=TRUE)) warning("Lat outside expected West Coast range (~32–49).")

# -----------------------------
# SDM helpers
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
# CPUE fallback (optional)
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
# Availability recode (kept from your pipeline)
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
# Wide builder (kept)
# -----------------------------
build_database <- function(long_data_spec, alts, case_vars){
  id_vars <- c("fished_haul_anon","fished_vessel_anon","set_date")

  case_data <- long_data_spec %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(across(all_of(case_vars), ~ first(.x)), .groups="drop")

  alt_data <- long_data_spec %>%
    select(all_of(id_vars), selection, any_of(c("fished", "mean_avail", "mean_price", "mean_price_3", "wind_max_220_mh", "dist_to_cog",
           "dist_port_to_catch_area_zero", "dummy_prev_days", "dummy_prev_year_days",
           "d_d", "d_cd", "d_c", "unem_rate", "waclosure", "dcrbclosurewad", "psdnclosure", "btnaclosure", "msqdclosure", "weekend"))) %>%
    pivot_wider(
      id_cols = all_of(id_vars),
      names_from  = selection,
      values_from = any_of(c("fished", "mean_avail", "mean_price", "mean_price_3", "wind_max_220_mh", "dist_to_cog",
                             "dist_port_to_catch_area_zero", "dummy_prev_days", "dummy_prev_year_days",
                             "d_d", "d_cd", "d_c", "unem_rate", "waclosure", "dcrbclosurewad", "psdnclosure", "btnaclosure", "msqdclosure", "weekend"))
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
# Dynamic-lite season picker: same months, most south vs most north
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

make_centroid_md <- function(centroid_df){
  centroid_df %>%
    mutate(
      year = lubridate::year(date),
      md   = format(date, "%m-%d")
    ) %>%
    filter(md != "02-29")
}

season_lat_by_year <- function(cent_md, start_md, window_days=90){
  starts <- cent_md %>%
    filter(md == start_md) %>%
    group_by(year) %>%
    summarise(start_date = min(date), .groups="drop")

  df <- cent_md %>%
    inner_join(starts, by="year") %>%
    mutate(day_idx = as.integer(date - start_date) + 1L) %>%
    filter(day_idx >= 1L, day_idx <= window_days)

  coverage <- df %>% count(year, name="n_days")
  ok_years <- coverage %>% filter(n_days == window_days) %>% pull(year)
  df_ok <- df %>% filter(year %in% ok_years)

  df_ok %>%
    group_by(year) %>%
    summarise(
      lat_mean = mean(lat_c, na.rm=TRUE),
      lon_mean = mean(lon_c, na.rm=TRUE),
      .groups="drop"
    )
}

pick_most_dissimilar_same_months <- function(centroid_df, window_days=90, min_years=3){
  cent_md <- make_centroid_md(centroid_df)

  md_counts <- cent_md %>%
    group_by(md) %>%
    summarise(n_years = n_distinct(year), .groups="drop") %>%
    filter(n_years >= min_years)

  candidates <- md_counts$md
  score_tbl <- purrr::map_dfr(candidates, function(md0){
    lat_tbl <- season_lat_by_year(cent_md, md0, window_days=window_days)
    if(nrow(lat_tbl) < min_years) return(NULL)
    tibble(
      start_md = md0,
      n_years  = nrow(lat_tbl),
      lat_min  = min(lat_tbl$lat_mean, na.rm=TRUE),
      lat_max  = max(lat_tbl$lat_mean, na.rm=TRUE),
      lat_range = lat_max - lat_min
    )
  })

  if(nrow(score_tbl) == 0) stop("No feasible start_md found with enough full-window years.")
  best <- score_tbl %>% slice_max(lat_range, n=1, with_ties=FALSE)

  lat_tbl <- season_lat_by_year(cent_md, best$start_md, window_days=window_days) %>%
    arrange(lat_mean)

  base_year <- lat_tbl %>% slice_min(lat_mean, n=1, with_ties=FALSE) %>% pull(year)
  scen_year <- lat_tbl %>% slice_max(lat_mean, n=1, with_ties=FALSE) %>% pull(year)

  base_start <- as.Date(paste0(base_year, "-", best$start_md))
  scen_start <- as.Date(paste0(scen_year, "-", best$start_md))

  base_dates <- seq(base_start, by="day", length.out=window_days)
  scen_dates <- seq(scen_start, by="day", length.out=window_days)

  base_dates2 <- base_dates[format(base_dates, "%m-%d") != "02-29"]
  scen_dates2 <- scen_dates[format(scen_dates, "%m-%d") != "02-29"]
  L <- min(length(base_dates2), length(scen_dates2))
  base_dates2 <- base_dates2[seq_len(L)]
  scen_dates2 <- scen_dates2[seq_len(L)]

  map_tbl <- tibble(day_idx = seq_len(L),
                    date_base = base_dates2,
                    date_scen = scen_dates2)

  list(
    start_md = best$start_md,
    window_days = L,
    base_year = base_year,
    scen_year = scen_year,
    base_start = min(map_tbl$date_base),
    base_end   = max(map_tbl$date_base),
    scen_start = min(map_tbl$date_scen),
    scen_end   = max(map_tbl$date_scen),
    lat_range  = best$lat_range,
    map_tbl    = map_tbl,
    lat_by_year = lat_tbl
  )
}

cent <- sdm_centroid_anchor(sdm_all, species=anchor_spec, port_lookup=port_lookup)
sel  <- pick_most_dissimilar_same_months(cent, window_days=window_days, min_years=min_years)

message("AUTO season start_md = ", sel$start_md,
        " | window_days = ", sel$window_days,
        "\nBASE (south) year = ", sel$base_year, " : ", sel$base_start, " to ", sel$base_end,
        "\nSCEN (north) year = ", sel$scen_year, " : ", sel$scen_start, " to ", sel$scen_end,
        "\nLat range (mean centroid) = ", round(sel$lat_range, 4))

map_tbl <- sel$map_tbl

# Build a scenario SDM table aligned to baseline dates
sdm_cols <- paste0(sdm_species, "_daily")
sdm_scen_aligned <- sdm_all %>%
  inner_join(map_tbl, by = c("date" = "date_scen")) %>%
  transmute(PORT_AREA_CODE,
            date = date_base,
            across(all_of(sdm_cols), identity))

# =========================================================
# Prediction helpers
# =========================================================
safe_get_Pmodel <- function(P){
  if(is.list(P) && !is.null(P$model)) return(P$model)
  if(is.matrix(P) || is.data.frame(P)) return(P)
  stop("Could not extract P$model from apollo_probabilities output.")
}

aggregate_probs <- function(Pmat, alts){
  Pmat <- as.data.frame(Pmat)
  miss <- setdiff(alts, names(Pmat))
  if(length(miss)>0) stop(paste("Missing alt cols in P:", paste(miss, collapse=", ")))

  p_part <- 1 - Pmat[["no_participation"]]

  sp <- ifelse(alts=="no_participation", "NOPART", toupper(sub(".*_", "", alts)))
  prt <- ifelse(alts=="no_participation", "NOPART", toupper(sub("_.*", "", alts)))

  Psp <- map_dfc(unique(sp[sp!="NOPART"]), function(s){
    cols <- alts[sp==s]
    tibble(!!s := rowSums(Pmat[, cols, drop=FALSE]))
  })

  Pport <- map_dfc(unique(prt[prt!="NOPART"]), function(p){
    cols <- alts[prt==p]
    tibble(!!p := rowSums(Pmat[, cols, drop=FALSE]))
  })

  list(
    alt_mean  = colMeans(Pmat[, alts, drop=FALSE]),
    part_mean = mean(p_part),
    sp_mean   = colMeans(Psp),
    port_mean = colMeans(Pport)
  )
}

predict_on_database <- function(model_obj, prob_fun, database_wide, apollo_control, use_d_c_val=0){
  beta_hat <- model_obj$estimate

  assign("database", database_wide, envir = .GlobalEnv)
  assign("apollo_control", apollo_control, envir = .GlobalEnv)
  assign("apollo_beta", beta_hat, envir = .GlobalEnv)
  assign("apollo_fixed", character(0), envir = .GlobalEnv)  # important: don't fix everything

  apollo_inputs <- apollo_validateInputs()
  apollo_inputs$use_d_c <- as.numeric(use_d_c_val)

  P <- prob_fun(beta_hat, apollo_inputs, functionality="prediction")
  safe_get_Pmodel(P)
}

# =========================================================
# Cluster metadata: alts + case_vars
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
# MAIN LOOP
# =========================================================
scenario_results <- list()

for(cl in names(cluster_rds)){
  message("\n==============================")
  message("Cluster: ", cl)
  message("==============================")

  meta <- cluster_meta[[cl]]
  alts <- meta$alts
  case_vars <- meta$case_vars

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

  # join baseline SDM by actual date
  long_data2 <- long_data %>%
    left_join(sdm_all, by = c("PORT_AREA_CODE", "date")) %>%
    # join scenario SDM aligned to baseline dates
    left_join(sdm_scen_aligned, by = c("PORT_AREA_CODE","date"), suffix = c("", "_SCEN")) %>%
    left_join(cpue_expanded, by = c("PORT_AREA_CODE", "date", "sp4" = "Species_Dominant")) %>%
    mutate(
      CPUE_avail_t1 = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
      used_90d_fallback = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1)),
      is_CPUE = as.integer(sp4 %in% no_sdm_species)
    ) %>%
    # baseline availability = daily SDM (or CPUE fallback)
    mutate(
      mean_avail_daily_base = case_when(
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
      ),
      # scenario availability = aligned SDM from scenario-year, matched day-by-day
      mean_avail_daily_scen = case_when(
        sp4=="MSQD" ~ MSQD_daily_SCEN,
        sp4=="PSDN" ~ PSDN_daily_SCEN,
        sp4=="NANC" ~ NANC_daily_SCEN,
        sp4=="JMCK" ~ JMCK_daily_SCEN,
        sp4=="CMCK" ~ CMCK_daily_SCEN,
        sp4=="PHRG" ~ PHRG_daily_SCEN,
        sp4=="ALBC" ~ ALBC_daily_SCEN,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1, # optional: implement CPUE matching similarly if desired
        sp4=="NOPART" ~ 0,
        TRUE ~ mean_avail_daily_base
      )
    )

  # Only evaluate predictions on baseline window dates (so your scenario is apples-to-apples)
  long_data2 <- long_data2 %>%
    filter(date >= sel$base_start, date <= sel$base_end)

  # Prepare availability specs for wide data
  long_base_ready <- prepare_avail_spec(long_data2, "mean_avail_daily_base")
  long_scen_ready <- prepare_avail_spec(long_data2, "mean_avail_daily_scen")

  db_base <- build_database(long_base_ready, alts, case_vars)
  db_scen <- build_database(long_scen_ready, alts, case_vars)

  model_daily <- res_list[[cl]]$models[["daily"]]$model
  stopifnot(!is.null(model_daily))

  apollo_control <- res_list[[cl]]$models[["daily"]]$apollo_control
  apollo_control$nCores <- 16
  apollo_control$modelName <- paste0("Scenario_", cl, "_daily_prediction_dynamic_lite")

  prob_fun <- prob_fun_list[[cl]]

  P_base <- predict_on_database(model_daily, prob_fun, db_base, apollo_control, use_d_c_val=0)
  P_scen <- predict_on_database(model_daily, prob_fun, db_scen, apollo_control, use_d_c_val=0)

  agg_base <- aggregate_probs(P_base, alts)
  agg_scen <- aggregate_probs(P_scen, alts)

  delta_part_pp <- (agg_scen$part_mean - agg_base$part_mean) * 100
  delta_sp_pp   <- (agg_scen$sp_mean   - agg_base$sp_mean)   * 100
  delta_port_pp <- (agg_scen$port_mean - agg_base$port_mean) * 100
  delta_alt_pp  <- (agg_scen$alt_mean  - agg_base$alt_mean)  * 100

  part_tbl <- tibble(cluster=cl, metric="participation", group="ALL", delta_pp=delta_part_pp)

  sp_tbl <- tibble(cluster=cl, metric="species", group=names(delta_sp_pp),
                   delta_pp=as.numeric(delta_sp_pp)) %>% arrange(desc(abs(delta_pp)))

  port_tbl <- tibble(cluster=cl, metric="port", group=names(delta_port_pp),
                     delta_pp=as.numeric(delta_port_pp)) %>% arrange(desc(abs(delta_pp)))

  alt_tbl <- tibble(cluster=cl, metric="alternative", group=names(delta_alt_pp),
                    delta_pp=as.numeric(delta_alt_pp)) %>% arrange(desc(abs(delta_pp)))

  scenario_results[[cl]] <- list(
    season = tibble(anchor=anchor_spec, start_md=sel$start_md, window_days=sel$window_days,
                    base_year=sel$base_year, base_start=sel$base_start, base_end=sel$base_end,
                    scen_year=sel$scen_year, scen_start=sel$scen_start, scen_end=sel$scen_end,
                    lat_range=sel$lat_range),
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
season_tbl <- bind_rows(lapply(scenario_results, `[[`, "season")) %>% distinct()

out_dir <- file.path("R","output","scenarios_dynamic_lite")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(all_part, file.path(out_dir,"scenario_dynamic_lite_delta_participation_pp.csv"))
write_csv(all_sp,   file.path(out_dir,"scenario_dynamic_lite_delta_species_pp.csv"))
write_csv(all_port, file.path(out_dir,"scenario_dynamic_lite_delta_port_pp.csv"))
write_csv(all_alt,  file.path(out_dir,"scenario_dynamic_lite_delta_alternative_pp.csv"))
write_csv(season_tbl, file.path(out_dir,"scenario_dynamic_lite_season_selected.csv"))

writexl::write_xlsx(
  list(
    participation = all_part,
    species = all_sp,
    port = all_port,
    alternative = all_alt,
    season_selected = season_tbl,
    date_mapping = map_tbl,
    lat_by_year = sel$lat_by_year
  ),
  path = file.path(out_dir,"scenario_dynamic_lite_delta_pp.xlsx")
)

message("\nAll scenario outputs written to: ", out_dir)

# =========================================================
# FIGURES
# =========================================================
# Species
df <- readr::read_csv(file.path(out_dir,"scenario_dynamic_lite_delta_species_pp.csv"), show_col_types = FALSE)
df_plot <- df %>%
  mutate(cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
         species = toupper(group)) %>%
  filter(!is.na(delta_pp)) %>%
  group_by(cluster) %>%
  mutate(species = fct_reorder(species, delta_pp)) %>%
  ungroup()

p1 <- ggplot(df_plot, aes(x=species, y=delta_pp)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  coord_flip() +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  labs(x=NULL, y=expression(Delta*" probability (pp)"),
       title="Dynamic-lite scenario (same months): SDM daily re-mapping",
       subtitle=paste0("Baseline year (south)=", sel$base_year, " vs Scenario year (north)=", sel$scen_year,
                       " | start_md=", sel$start_md, " | window=", sel$window_days, " days")) +
  theme_bw(base_size=12) +
  theme(legend.position="none", panel.grid.minor = element_blank())

ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_species_pp.png"), p1, width=8.5, height=6.5, dpi=300)
ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_species_pp.pdf"), p1, width=8.5, height=6.5)

# Port
df <- readr::read_csv(file.path(out_dir,"scenario_dynamic_lite_delta_port_pp.csv"), show_col_types = FALSE)
df_plot <- df %>%
  mutate(cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
         port = toupper(group)) %>%
  filter(!is.na(delta_pp)) %>%
  group_by(cluster) %>%
  mutate(port = fct_reorder(port, delta_pp)) %>%
  ungroup()

p2 <- ggplot(df_plot, aes(x=port, y=delta_pp)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  coord_flip() +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  labs(x=NULL, y=expression(Delta*" probability (pp)"),
       title="Dynamic-lite scenario (same months): SDM daily re-mapping",
       subtitle="Δ predicted probabilities by port (species aggregated)") +
  theme_bw(base_size=12) +
  theme(legend.position="none", panel.grid.minor = element_blank())

ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_port_pp.png"), p2, width=8.5, height=6.5, dpi=300)
ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_port_pp.pdf"), p2, width=8.5, height=6.5)

# Alternative (top +/- per cluster)
df <- readr::read_csv(file.path(out_dir,"scenario_dynamic_lite_delta_alternative_pp.csv"), show_col_types = FALSE)
df_plot <- df %>%
  mutate(cluster = factor(cluster, levels = c("c4","c5","c6","c7")),
         alternative = tolower(group)) %>%
  group_by(cluster) %>%
  mutate(rk_pos = rank(-delta_pp, ties.method="first"),
         rk_neg = rank( delta_pp, ties.method="first")) %>%
  filter(rk_pos <= 6 | rk_neg <= 6) %>%
  ungroup() %>%
  group_by(cluster) %>%
  mutate(alternative = fct_reorder(alternative, delta_pp)) %>%
  ungroup()

p3 <- ggplot(df_plot, aes(x=alternative, y=delta_pp)) +
  geom_hline(yintercept = 0, linewidth=0.3) +
  geom_col() +
  coord_flip() +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  labs(x=NULL, y=expression(Delta*" probability (pp)"),
       title="Dynamic-lite scenario (same months): SDM daily re-mapping",
       subtitle="Top positive/negative Δ alternatives (including no_participation)") +
  theme_bw(base_size=12) +
  theme(panel.grid.minor = element_blank(), legend.position="none")

ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_alternative_pp.png"), p3, width=9, height=7, dpi=300)
ggsave(file.path(out_dir,"Fig_dynamic_lite_delta_alternative_pp.pdf"), p3, width=9, height=7)

message("\nFigures written to: ", out_dir)
