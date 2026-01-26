############################################################
# Scenario: Dynamic-lite, SAME-MONTH windows (most dissimilar),
# using DAILY SDMs aligned day-by-day between two windows.
#
# Goal:
# - Choose TWO periods WITHIN your choice-data support (2013–2017),
#   SAME months block (same season), one "most south" and one "most north"
#   based on MSQD SDM latitude centroid.
# - Simulate a “northward shift” by swapping the daily SDM time-series
#   from the north window onto the baseline dates (day-by-day alignment),
#   keeping ALL other covariates exactly as observed in the BASE window.
#
# Outputs (pp = percentage points):
# - scenario_dynamic_same_months_windows.csv
# - scenario_dynamic_same_months_delta_{participation,species,port,alternative}_pp.csv
# - scenario_dynamic_same_months_delta_pp.xlsx
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

google_dir <- "G:/Mi unidad"             # adjust if needed
sdm_dir    <- "SDMs"
cpue_rds   <- "D:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds"

# Port coordinates file
port_csv_path <- "D:/GitHub/EconAnalysis/Data/Ports/port_areas.csv"

# Cluster data files (anonymised)
cluster_rds <- list(
  c4 = file.path(google_dir, "Data/Anonymised data/part_model_c4.rds"),
  c5 = file.path(google_dir, "Data/Anonymised data/part_model_c5.rds"),
  c6 = file.path(google_dir, "Data/Anonymised data/part_model_c6.rds"),
  c7 = file.path(google_dir, "Data/Anonymised data/part_model_c7.rds")
)

# Window settings
window_days <- 60        # length of window (days)
n_months    <- 3         # months block (e.g., Jan-Feb-Mar)
anchor_spec <- "MSQD"    # anchor used to choose windows

sdm_species <- c("MSQD","PSDN","NANC","JMCK","CMCK","PHRG","ALBC")
no_sdm_species <- c("BTNA","YTNA","DCRB","SOCK")  # CPUE fallback species

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

# -----------------------------
# SDM helper
# -----------------------------
prepare_sdm <- function(sdm_rds, species_code, sdm_col, port_var="PORT_AREA_CODE"){
  stopifnot(file.exists(sdm_rds))
  sdm <- readRDS(sdm_rds) %>%
    mutate(date = if(!"date" %in% names(.)) make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) else as.Date(date)) %>%
    mutate(!!port_var := toupper(.data[[port_var]])) %>%
    arrange(.data[[port_var]], date) %>%
    group_by(.data[[port_var]]) %>%
    complete(date = seq(min(date, na.rm=TRUE), max(date, na.rm=TRUE), by="day")) %>%
    arrange(date) %>%
    mutate(!!paste0(species_code,"_daily") := .data[[sdm_col]]) %>%
    ungroup() %>%
    select(.data[[port_var]], date, starts_with(paste0(species_code,"_daily")))
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
# CPUE fallback
# -----------------------------
cpue_raw <- readRDS(cpue_rds) %>%
  mutate(date = make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY),
         PORT_AREA_CODE = toupper(PORT_AREA_CODE),
         Species_Dominant = toupper(Species_Dominant)) %>%
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
# Availability recode (as your pipeline)
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
# Wide builder (as your pipeline)
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

# -----------------------------
# Anchor centroid and window picker
# (NO charToDate; uses real dates already in centroid_df)
# -----------------------------
sdm_centroid_anchor <- function(sdm_all_local, species="MSQD", port_lookup_local){
  colA <- paste0(species, "_daily")
  stopifnot(colA %in% names(sdm_all_local))
  
  df <- sdm_all_local %>%
    mutate(PORT_AREA_CODE = toupper(PORT_AREA_CODE)) %>%
    left_join(port_lookup_local %>% mutate(PORT_AREA_CODE=toupper(PORT_AREA_CODE)),
              by="PORT_AREA_CODE") %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    mutate(w = replace_na(.data[[colA]], 0))
  
  df %>%
    group_by(date) %>%
    summarise(
      wsum  = sum(w),
      lat_c = ifelse(wsum > 0, weighted.mean(lat, w = w), NA_real_),
      lon_c = ifelse(wsum > 0, weighted.mean(lon, w = w), NA_real_),
      .groups="drop"
    ) %>%
    filter(!is.na(lat_c) & !is.na(lon_c))
}

pick_most_dissimilar_same_month_block <- function(centroid_df,
                                                  window_days = 90,
                                                  n_months = 3){
  stopifnot(all(c("date","lat_c") %in% names(centroid_df)))
  
  df <- centroid_df %>%
    mutate(y = year(date), m = month(date)) %>%
    filter(!is.na(lat_c)) %>%
    arrange(date)
  
  if(nrow(df) == 0) stop("centroid_df has no valid rows.")
  
  start_months <- 1:(12 - n_months + 1)
  best <- NULL
  
  for(m0 in start_months){
    months_block <- m0:(m0 + n_months - 1)
    
    df_block <- df %>% filter(m %in% months_block)
    
    cand <- df_block %>%
      group_by(y) %>%
      summarise(min_date = min(date), max_date = max(date), .groups="drop") %>%
      mutate(start_min = min_date, start_max = max_date - (window_days - 1)) %>%
      filter(start_max >= start_min)
    
    if(nrow(cand) < 2) next
    
    win_tbl <- purrr::map_dfr(seq_len(nrow(cand)), function(i){
      y0 <- cand$y[i]
      s0 <- cand$start_min[i]
      s1 <- cand$start_max[i]
      starts <- seq(s0, s1, by="day")
      
      purrr::map_dfr(starts, function(sd){
        ed <- sd + (window_days - 1)
        lat_mean <- df_block %>%
          filter(date >= sd, date <= ed, y == y0) %>%
          summarise(lat_mean = mean(lat_c, na.rm=TRUE)) %>%
          pull(lat_mean)
        
        tibble(y=y0, start_date=sd, end_date=ed, lat_mean=lat_mean, block_start_month=m0)
      })
    }) %>%
      filter(!is.na(lat_mean))
    
    if(n_distinct(win_tbl$y) < 2) next
    
    w_min <- win_tbl %>% slice_min(lat_mean, n=1, with_ties=FALSE)
    w_max <- win_tbl %>% slice_max(lat_mean, n=1, with_ties=FALSE)
    
    dissim <- w_max$lat_mean - w_min$lat_mean
    
    if(is.null(best) || dissim > best$dissim){
      
      # ---- NEW: enforce "oldest date = BASE" ----
      pair <- bind_rows(w_min, w_max) %>% arrange(start_date)
      w_old <- pair %>% slice(1)
      w_new <- pair %>% slice(2)
      
      best <- list(
        dissim = dissim,
        months_block = months_block,
        base = w_old,   # oldest
        scen = w_new    # newest
      )
    }
  }
  
  if(is.null(best)){
    stop("No feasible same-month windows found. Try smaller window_days (e.g., 60) or n_months=4.")
  }
  
  list(months_block=best$months_block, base=best$base, scen=best$scen, dissim=best$dissim)
}

# -----------------------------
# Scenario SDM aligner:
# maps SCEN dates onto BASE dates 1:1 by day offset
# -----------------------------
make_sdm_aligned <- function(sdm_all_local, base_start, base_end, scen_start){
  base_dates <- seq(base_start, base_end, by="day")
  scen_dates <- scen_start + (0:(length(base_dates)-1))
  map_df <- tibble(date = base_dates, date_scen = scen_dates)
  
  base_sdm <- sdm_all_local %>%
    filter(date %in% base_dates) %>%
    rename_with(~ paste0(., "_BASE"), ends_with("_daily"))
  
  scen_sdm <- sdm_all_local %>%
    filter(date %in% scen_dates) %>%
    rename(date_scen = date) %>%
    rename_with(~ paste0(., "_SCEN"), ends_with("_daily"))
  
  base_sdm %>%
    left_join(map_df, by = c("date" = "date")) %>%
    left_join(scen_sdm, by = c("PORT_AREA_CODE", "date_scen"))
}

# -----------------------------
# Prediction helpers
# -----------------------------
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
    alt_mean = colMeans(Pmat[, alts, drop=FALSE]),
    part_mean = mean(p_part),
    sp_mean = colMeans(Psp),
    port_mean = colMeans(Pport)
  )
}

predict_on_database <- function(model_obj, prob_fun, database_wide, apollo_control, use_d_c_val=0){
  beta_hat <- model_obj$estimate
  
  assign("database", database_wide, envir = .GlobalEnv)
  assign("apollo_control", apollo_control, envir = .GlobalEnv)
  assign("apollo_beta", beta_hat, envir = .GlobalEnv)
  assign("apollo_fixed", character(0), envir = .GlobalEnv)
  
  apollo_inputs <- apollo_validateInputs()
  apollo_inputs$use_d_c <- as.numeric(use_d_c_val)
  
  P <- prob_fun(beta_hat, apollo_inputs, functionality="prediction")
  safe_get_Pmodel(P)
}

# -----------------------------
# Cluster metadata
# -----------------------------
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

# -----------------------------
# Load estimated models + probability functions
# -----------------------------
res_list <- list(
  c4 = readRDS("res_c4.rds"),
  c5 = readRDS("res_c5.rds"),
  c6 = readRDS("res_c6.rds"),
  c7 = readRDS("res_c7.rds")
)

prob_fun_list <- list(
  c4 = readRDS("apollo_probabilities_c4.rds"),
  c5 = readRDS("apollo_probabilities_c5.rds"),
  c6 = readRDS("apollo_probabilities_c6.rds"),
  c7 = readRDS("apollo_probabilities_c7.rds")
)


# =========================================================
# GLOBAL WINDOW PICK (one scenario shared by all clusters)
# =========================================================

# 1) Get date support for each cluster
range_tbl <- purrr::map_dfr(names(cluster_rds), function(cl){
  ld <- readRDS(cluster_rds[[cl]]) %>%
    mutate(date = as.Date(set_date))
  tibble(
    cluster = cl,
    data_min = min(ld$date, na.rm=TRUE),
    data_max = max(ld$date, na.rm=TRUE)
  )
})

# 2) Intersection of supports so ALL clusters can use the same window
global_min <- max(range_tbl$data_min, na.rm=TRUE)
global_max <- min(range_tbl$data_max, na.rm=TRUE)

message("\nGLOBAL overlap support: ", global_min, " to ", global_max)

# 3) Build centroid anchor on the global support and pick ONE window pair
sdm_all_global <- sdm_all %>% filter(date >= global_min, date <= global_max)
cent_global <- sdm_centroid_anchor(sdm_all_global, species = anchor_spec, port_lookup_local = port_lookup)

picked_global <- NULL
for(wd in c(window_days, 80, 70, 60)){
  try_pick <- try(pick_most_dissimilar_same_month_block(cent_global, window_days=wd, n_months=n_months), silent=TRUE)
  if(!inherits(try_pick, "try-error")){
    picked_global <- try_pick
    window_days_eff_global <- wd
    break
  }
}
if(is.null(picked_global)){
  stop("Could not find a GLOBAL feasible same-month window. Try lowering window_days or n_months.")
}

win_base_global <- list(
  start_date = picked_global$base$start_date,
  end_date   = picked_global$base$end_date
)
win_scen_global <- list(
  start_date = picked_global$scen$start_date,
  end_date   = picked_global$scen$end_date
)

message("GLOBAL SAME-MONTH BLOCK: ", paste(picked_global$months_block, collapse="-"),
        " | window_days=", window_days_eff_global)
message("GLOBAL BASE: ", win_base_global$start_date, " to ", win_base_global$end_date,
        " | year=", lubridate::year(win_base_global$start_date))
message("GLOBAL SCEN: ", win_scen_global$start_date, " to ", win_scen_global$end_date,
        " | year=", lubridate::year(win_scen_global$start_date))



# win_base_global <- list(start_date = as.Date("2013-05-01"),
#                         end_date   = as.Date("2013-07-01"))
# win_scen_global <- list(start_date = as.Date("2015-01-01"),
#                         end_date   = as.Date("2015-03-01"))
# 
# message("FORCED GLOBAL WINDOWS:")
# message("BASE (oldest): ", win_base_global$start_date, " to ", win_base_global$end_date,
#         " | year=", lubridate::year(win_base_global$start_date))
# message("SCEN (newest): ", win_scen_global$start_date, " to ", win_scen_global$end_date,
#         " | year=", lubridate::year(win_scen_global$start_date))
# 
# 
cent_base <- sdm_centroid_anchor(
  sdm_all_local = sdm_all %>% filter(date >= win_base_global$start_date, date <= win_base_global$end_date),
  species = anchor_spec,
  port_lookup_local = port_lookup
)

cent_scen <- sdm_centroid_anchor(
  sdm_all_local = sdm_all %>% filter(date >= win_scen_global$start_date, date <= win_scen_global$end_date),
  species = anchor_spec,
  port_lookup_local = port_lookup
)

lat_mean_base <- cent_base %>% summarise(lat_mean = mean(lat_c, na.rm=TRUE)) %>% pull(lat_mean)
lat_mean_scen <- cent_scen %>% summarise(lat_mean = mean(lat_c, na.rm=TRUE)) %>% pull(lat_mean)

message("BASE (forced): ", win_base_global$start_date, " to ", win_base_global$end_date,
        " | lat_mean=", round(lat_mean_base, 3))
message("SCEN (forced): ", win_scen_global$start_date, " to ", win_scen_global$end_date,
        " | lat_mean=", round(lat_mean_scen, 3))





# =========================================================
# MAIN LOOP (per cluster)
# =========================================================
scenario_results <- list()
window_tbl <- list()

for(cl in names(cluster_rds)){
  message("\n==============================")
  message("Cluster: ", cl)
  message("==============================")
  
  meta <- cluster_meta[[cl]]
  alts <- meta$alts
  case_vars <- meta$case_vars
  
  long_data <- readRDS(cluster_rds[[cl]]) %>%
    select(any_of(c("set_date","fished_haul_anon","fished_vessel_anon","selection","fished",
                    "mean_price","mean_price_3","wind_max_220_mh","dist_to_cog","dist_port_to_catch_area_zero",
                    "psdnclosure","btnaclosure","msqdclosure","waclosure","dcrbclosurewad",
                    "dummy_prev_days","dummy_prev_year_days","unem_rate","d_d","d_cd","weekend"))) %>%
    mutate(selection = tolower(gsub("-", "_", selection)),
           date = as.Date(set_date),
           PORT_AREA_CODE = case_when(selection=="no_participation" ~ "NOPART",
                                      TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))),
           sp4 = case_when(selection=="no_participation" ~ "NOPART",
                           TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$"))))
  
  # RESTRICT everything to the CHOICE DATA SUPPORT (THIS FIXES YOUR 2009 WINDOW PROBLEM)
  data_min <- min(long_data$date, na.rm=TRUE)
  data_max <- max(long_data$date, na.rm=TRUE)
  
  sdm_all_use <- sdm_all %>% filter(date >= data_min, date <= data_max)
  
  # cent_use <- sdm_centroid_anchor(sdm_all_use, species=anchor_spec, port_lookup_local=port_lookup)
  # 
  # # If a cluster cannot support 90-day windows, auto-try shorter
  # picked <- NULL
  # for(wd in c(window_days, 80, 70, 60)){
  #   try_pick <- try(pick_most_dissimilar_same_month_block(cent_use, window_days=wd, n_months=n_months), silent=TRUE)
  #   if(!inherits(try_pick, "try-error")){
  #     picked <- try_pick
  #     window_days_eff <- wd
  #     break
  #   }
  # }
  # 
  # if(is.null(picked)){
  #   stop("Could not find feasible same-month windows for ", cl,
  #        ". Try lowering window_days or n_months.")
  # }
  # 
  # win_base <- list(start_date = picked$base$start_date, end_date = picked$base$end_date)
  # win_scen <- list(start_date = picked$scen$start_date, end_date = picked$scen$end_date)
  # 
  # message("Chosen SAME-MONTH BLOCK: ", paste(picked$months_block, collapse="-"),
  #         " | window_days=", window_days_eff)
  # message("BASE (oldest): ", win_base$start_date, " to ", win_base$end_date,
  #         " | year=", year(win_base$start_date),
  #         " | lat_mean=", round(picked$base$lat_mean, 3))
  # message("SCEN (newest): ", win_scen$start_date, " to ", win_scen$end_date,
  #         " | year=", year(win_scen$start_date),
  #         " | lat_mean=", round(picked$scen$lat_mean, 3))
  # 
  # stopifnot(win_base$start_date >= data_min, win_base$end_date <= data_max,
  #           win_scen$start_date >= data_min, win_scen$end_date <= data_max)
  
  
  ok <- (win_base_global$start_date >= data_min &&
           win_base_global$end_date   <= data_max &&
           win_scen_global$start_date >= data_min &&
           win_scen_global$end_date   <= data_max)
  
  if(!ok){
    message("WARNING: cluster ", cl, " does NOT support the forced windows. ",
            "Support is ", data_min, " to ", data_max)
    # Option A: skip this cluster
    next
    # Option B: stop and fix
    # stop("Forced windows not feasible for cluster ", cl)
  }

  # Use the global scenario windows for every cluster
  win_base <- win_base_global
  win_scen <- win_scen_global
  window_days_eff <- window_days_eff_global
  picked <- picked_global
  
  message("Using GLOBAL SAME-MONTH BLOCK: ", paste(picked$months_block, collapse="-"),
          " | window_days=", window_days_eff)
  message("BASE (global): ", win_base$start_date, " to ", win_base$end_date)
  message("SCEN (global): ", win_scen$start_date, " to ", win_scen$end_date)
  
  # sanity check: should be feasible for every cluster by construction
  stopifnot(win_base$start_date >= data_min, win_base$end_date <= data_max,
            win_scen$start_date >= data_min, win_scen$end_date <= data_max)
  
  
  window_tbl[[cl]] <- tibble(
    cluster = cl,
    anchor = anchor_spec,
    months_block = paste(picked$months_block, collapse="-"),
    window_days = window_days_eff,
    base_start = win_base$start_date, base_end = win_base$end_date, base_lat_mean = picked$base$lat_mean,
    scen_start = win_scen$start_date, scen_end = win_scen$end_date, scen_lat_mean = picked$scen$lat_mean,
    dissim_lat = picked$dissim
  )
  
  # Align SCEN SDMs onto BASE dates (daily)
  sdm_aligned <- make_sdm_aligned(sdm_all_use, win_base$start_date, win_base$end_date, win_scen$start_date)
  
  # Keep only BASE dates for prediction (other covariates fixed)
  long_win <- long_data %>% filter(date >= win_base$start_date, date <= win_base$end_date)
  
  long_data2 <- long_win %>%
    left_join(sdm_aligned, by = c("PORT_AREA_CODE","date")) %>%
    left_join(cpue_expanded, by = c("PORT_AREA_CODE","date", "sp4" = "Species_Dominant")) %>%
    mutate(
      CPUE_avail_t1 = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1)
    ) %>%
    mutate(
      mean_avail_daily_base = case_when(
        sp4=="MSQD" ~ MSQD_daily_BASE,
        sp4=="PSDN" ~ PSDN_daily_BASE,
        sp4=="NANC" ~ NANC_daily_BASE,
        sp4=="JMCK" ~ JMCK_daily_BASE,
        sp4=="CMCK" ~ CMCK_daily_BASE,
        sp4=="PHRG" ~ PHRG_daily_BASE,
        sp4=="ALBC" ~ ALBC_daily_BASE,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      ),
      mean_avail_daily_scen = case_when(
        sp4=="MSQD" ~ MSQD_daily_SCEN,
        sp4=="PSDN" ~ PSDN_daily_SCEN,
        sp4=="NANC" ~ NANC_daily_SCEN,
        sp4=="JMCK" ~ JMCK_daily_SCEN,
        sp4=="CMCK" ~ CMCK_daily_SCEN,
        sp4=="PHRG" ~ PHRG_daily_SCEN,
        sp4=="ALBC" ~ ALBC_daily_SCEN,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  if(nrow(long_data2)==0){
    stop("long_data2 is empty for ", cl, ". Check windows and joins.")
  }
  
  long_base_ready <- prepare_avail_spec(long_data2, "mean_avail_daily_base")
  long_scen_ready <- prepare_avail_spec(long_data2, "mean_avail_daily_scen")
  
  db_base <- build_database(long_base_ready, alts, case_vars)
  db_scen <- build_database(long_scen_ready, alts, case_vars)
  
  if(nrow(db_base)==0 || nrow(db_scen)==0){
    stop("Wide database is empty for ", cl, ".")
  }
  
  model_daily <- res_list[[cl]]$models[["daily"]]$model
  apollo_control <- res_list[[cl]]$models[["daily"]]$apollo_control
  apollo_control$nCores <- 16
  apollo_control$modelName <- paste0("Scenario_", cl, "_dynamic_lite_same_months")
  
  prob_fun <- prob_fun_list[[cl]]
  
  P_base <- predict_on_database(model_daily, prob_fun, db_base, apollo_control, use_d_c_val=0)
  P_scen <- predict_on_database(model_daily, prob_fun, db_scen, apollo_control, use_d_c_val=0)
  
  agg_base <- aggregate_probs(P_base, alts)
  agg_scen <- aggregate_probs(P_scen, alts)
  
  delta_part_pp <- (agg_scen$part_mean - agg_base$part_mean) * 100
  delta_sp_pp   <- (agg_scen$sp_mean   - agg_base$sp_mean)   * 100
  delta_port_pp <- (agg_scen$port_mean - agg_base$port_mean) * 100
  delta_alt_pp  <- (agg_scen$alt_mean  - agg_base$alt_mean)  * 100
  
  scenario_results[[cl]] <- list(
    participation = tibble(cluster=cl, metric="participation", group="ALL", delta_pp=delta_part_pp),
    species = tibble(cluster=cl, metric="species", group=names(delta_sp_pp), delta_pp=as.numeric(delta_sp_pp)) %>% arrange(desc(abs(delta_pp))),
    port = tibble(cluster=cl, metric="port", group=names(delta_port_pp), delta_pp=as.numeric(delta_port_pp)) %>% arrange(desc(abs(delta_pp))),
    alternative = tibble(cluster=cl, metric="alternative", group=names(delta_alt_pp), delta_pp=as.numeric(delta_alt_pp)) %>% arrange(desc(abs(delta_pp)))
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
all_win  <- bind_rows(window_tbl)

dir.create(file.path("R","output","scenarios"), showWarnings = FALSE, recursive = TRUE)

write_csv(all_win,  file.path("R","output","scenarios","scenario_dynamic_same_months_windows.csv"))
write_csv(all_part, file.path("R","output","scenarios","scenario_dynamic_same_months_delta_participation_pp.csv"))
write_csv(all_sp,   file.path("R","output","scenarios","scenario_dynamic_same_months_delta_species_pp.csv"))
write_csv(all_port, file.path("R","output","scenarios","scenario_dynamic_same_months_delta_port_pp.csv"))
write_csv(all_alt,  file.path("R","output","scenarios","scenario_dynamic_same_months_delta_alternative_pp.csv"))

writexl::write_xlsx(
  list(
    windows = all_win,
    participation = all_part,
    species = all_sp,
    port = all_port,
    alternative = all_alt
  ),
  path = file.path("R","output","scenarios","scenario_dynamic_same_months_delta_pp.xlsx")
)

message("\nAll scenario outputs written to: R/output/scenarios/")




# FIGURES

library(dplyr)
library(stringr)
library(ggplot2)

# ----------------------------
# 0) Pretty labels
# ----------------------------

# (A) Optional dictionaries (edit to your taste)
port_labels <- c(
  laa = "Los Angeles",
  sfa = "San Francisco",
  mna = "Monterey",
  sba = "Santa Barbara",
  mra = "Morro Bay",
  cwa = "Coastal Washington Ports",
  clo = "Columbia River (Oregon)", # if used
  clw = "Columbia River (Washington)",
  cba = "Coos Bay",
  nps = "North Puget Sound",
  npa = "Newport"
)

spec_labels <- c(
  psdn = "Pacific sardine",
  nanc = "Northern anchovy",
  msqd = "Market squid",
  cmck = "Chub mackerel",
  jmck = "Jack mackerel",
  ytna = "Yellowfin tuna",  # <-- adjust if needed
  btna = "Bluefin tuna",
  dcrb = "Dungeness Crab",
  albc = "Albacore", 
  sock = "Sockeye", # <-- adjust if needed
  cpue = "CPUE index"                # if appears
)

# Helper to build a pretty alternative label
pretty_alt_label <- function(x){
  x <- as.character(x)
  if(x == "no_participation") return("No participation")
  
  # expected format: "laa_msqd" (port_species)
  port <- str_extract(x, "^[^_]+")
  spec <- str_extract(x, "(?<=_).+$")
  
  port_pretty <- ifelse(port %in% names(port_labels), port_labels[[port]], toupper(port))
  spec_pretty <- ifelse(spec %in% names(spec_labels), spec_labels[[spec]], toupper(spec))
  
  paste0(port_pretty, " — ", spec_pretty)
}

# Apply to dataframes
all_alt <- all_alt %>%
  mutate(group_label = vapply(group, pretty_alt_label, character(1)))

# For ports/species plots you can also label them (optional)
pretty_port <- function(x){
  x <- as.character(x)
  ifelse(x %in% names(port_labels), port_labels[[x]], x)
}
pretty_spec <- function(x){
  x <- as.character(x)
  ifelse(x %in% names(spec_labels), spec_labels[[x]], x)
}

all_port <- all_port %>% mutate(group_label = vapply(group, pretty_port, character(1)))
all_sp   <- all_sp   %>% mutate(group_label = vapply(group, pretty_spec, character(1)))

# ----------------------------
# 1) Plot helper (English labels, facet by cluster)
# ----------------------------
out_dir <- file.path("R","output","scenarios")
fig_dir <- file.path(out_dir, "figs")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

plot_delta_facet <- function(df, title, ylab, fname, topN = NULL, force_include = NULL){
  
  df2 <- df %>%
    mutate(delta_pp = as.numeric(delta_pp))
  
  # keep topN by abs(delta_pp) per cluster
  if(!is.null(topN)){
    df2 <- df2 %>%
      group_by(cluster) %>%
      slice_max(order_by = abs(delta_pp), n = topN, with_ties = FALSE) %>%
      ungroup()
  }
  
  # optionally force-include certain labels (e.g., "No participation")
  if(!is.null(force_include)){
    df_force <- df %>% filter(group %in% force_include | group_label %in% force_include)
    df2 <- bind_rows(df2, df_force) %>% distinct(cluster, group, .keep_all = TRUE)
  }
  
  # reorder within each facet
  df2 <- df2 %>%
    group_by(cluster) %>%
    mutate(group_f = reorder(group_label, delta_pp)) %>%
    ungroup()
  
  p <- ggplot(df2, aes(x = delta_pp, y = group_f)) +
    geom_col() +
    facet_wrap(~ cluster, scales = "free_y") +
    labs(
      x = "Change in predicted probability (percentage points)",
      y = ylab,
      title = title
    ) +
    theme_bw()
  
  ggsave(file.path(fig_dir, fname), p, width = 13, height = 8, dpi = 300)
  p
}

# ----------------------------
# 2) Figures (English)
# ----------------------------

# Species
p_species <- plot_delta_facet(
  df    = all_sp,
  title = "Scenario vs baseline: changes by species (Top 10 by |Δ| within fleet segment)",
  ylab  = "Species",
  fname = "delta_by_species_top10_EN.png",
  topN  = 5
)

# Port
p_port <- plot_delta_facet(
  df    = all_port,
  title = "Scenario vs baseline: changes by landing port (Top 10 by |Δ| within fleet segment)",
  ylab  = "Landing port",
  fname = "delta_by_port_top10_EN.png",
  topN  = 5
)

# All alternatives (include no_participation)
p_alt <- plot_delta_facet(
  df    = all_alt,
  title = "Scenario vs baseline: changes by alternative (port × target species), incl. No participation (Top 10 by |Δ| within fleet segment)",
  ylab  = "Alternative",
  fname = "delta_by_alternative_top15_EN.png",
  topN  = 10,
  force_include = c("no_participation", "No participation")  # ensure it always appears
)
print(p_port)
print(p_species)
print(p_alt)

# # ----------------------------
# # 3) Change in participation (pp) by cluster
# #    participation = 1 - P(no_participation)
# #    => Δparticipation = - ΔP(no_participation)
# # ----------------------------
# part_change <- all_alt %>%
#   filter(group == "no_participation") %>%
#   transmute(
#     cluster,
#     delta_no_participation_pp = as.numeric(delta_pp),
#     delta_participation_pp    = -as.numeric(delta_pp)
#   ) %>%
#   arrange(cluster)
# 
# print(part_change)

# # Optional: a tiny bar chart for Δparticipation (pp)
# p_part <- ggplot(part_change, aes(x = cluster, y = delta_participation_pp)) +
#   geom_col() +
#   labs(
#     x = "Fleet segment",
#     y = "Change in participation probability (pp)",
#     title = "Scenario vs baseline: ΔParticipation (pp)"
#   ) +
#   theme_bw()
# 
# ggsave(file.path(fig_dir, "delta_participation_EN.png"), p_part,
#        width = 8, height = 4.5, dpi = 300)
# 
# print(p_part)
