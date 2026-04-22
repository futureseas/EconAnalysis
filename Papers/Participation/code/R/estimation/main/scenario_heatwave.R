############################################################
# Heatwave-like availability redistribution scenario
# (SDMs daily at PORT_AREA_CODE)
# - Objective peak window in 2016 = max northward/offshore SDM centroid shift
# - Baseline = 2013–2014 average SDM availability
# - Scenario = peak-window 2016 average SDM availability
############################################################

library(dplyr)
library(tidyr)
library(slider)
library(lubridate)

# -----------------------------
# 0) YOUR SDM PIPELINE (daily only)
# -----------------------------
sdm_dir <- "SDMs"

prepare_sdm <- function(sdm_rds, species_code, sdm_col, port_var="PORT_AREA_CODE"){
  stopifnot(file.exists(sdm_rds))
  sdm <- readRDS(sdm_rds) %>%
    mutate(date = if(!"date" %in% names(.)) make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY) else as.Date(date)) %>%
    arrange(.data[[port_var]], date) %>%
    group_by(.data[[port_var]]) %>%
    complete(date = seq(min(date, na.rm=TRUE), max(date, na.rm=TRUE), by="day")) %>%
    arrange(date) %>%
    mutate(
      !!paste0(species_code,"_daily") := .data[[sdm_col]]
    ) %>%
    ungroup() %>%
    select(.data[[port_var]], date, starts_with(paste0(species_code,"_"))) 
  return(sdm)
}

# Load SDMs (daily only)
MSQD_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_msqd.rds"), "MSQD", "MSQD_SDM_90")
PSDN_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_psdn.rds"), "PSDN", "PSDN_SDM_60")
NANC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_nanc.rds"), "NANC", "NANC_SDM_60")
JMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_jmck.rds"), "JMCK", "JMCK_SDM_60")
CMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_cmck.rds"), "CMCK", "CMCK_SDM_60")
PHRG_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_phrg.rds"), "PHRG", "PHRG_SDM_20")
ALBC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_albc.rds"), "ALBC", "albc_SDM_90")

sdm_list <- list(MSQD=MSQD_sdm, PSDN=PSDN_sdm, NANC=NANC_sdm, JMCK=JMCK_sdm,
                 CMCK=CMCK_sdm, PHRG=PHRG_sdm, ALBC=ALBC_sdm)

daily_col <- function(spec_code) paste0(spec_code, "_daily")

# -----------------------------
# 1) REQUIRED: PORT_AREA_CODE -> lat (+ optional offshore_km)
# -----------------------------
# You MUST provide this lookup.
# Minimum: PORT_AREA_CODE, lat
# Optional: offshore_km (distance-to-coast proxy)
#
# Example skeleton (REPLACE with your real values):
# port_lookup <- tibble::tibble(
#   PORT_AREA_CODE = c("CLO","CLW","CWA","LAA","MNA","SBA","SFA","NPA","CBA"),
#   lat = c(34.6, 35.0, 47.6, 34.0, 45.9, 34.4, 37.8, 40.8, 35.6)
# )
#
stopifnot(exists("port_lookup"))
stopifnot(all(c("PORT_AREA_CODE","lat") %in% names(port_lookup)))

# -----------------------------
# 2) Centroid + peak-window selection (objective)
# -----------------------------
sdm_daily_centroid <- function(sdm_df, spec_code, port_lookup,
                               port_var="PORT_AREA_CODE",
                               lat_col="lat",
                               offshore_col=NULL){
  colA <- daily_col(spec_code)
  stopifnot(colA %in% names(sdm_df))
  stopifnot(port_var %in% names(sdm_df))
  stopifnot("date" %in% names(sdm_df))
  
  sdm_df %>%
    left_join(port_lookup, by=setNames("PORT_AREA_CODE", port_var)) %>%
    filter(!is.na(.data[[lat_col]])) %>%
    group_by(date) %>%
    summarise(
      lat_c = weighted.mean(.data[[lat_col]], w = .data[[colA]], na.rm=TRUE),
      off_c = if(!is.null(offshore_col) && offshore_col %in% names(.))
        weighted.mean(.data[[offshore_col]], w = .data[[colA]], na.rm=TRUE) else NA_real_,
      .groups="drop"
    )
}

pick_peak_window_2016 <- function(centroid_df,
                                  baseline_start="2013-01-01",
                                  baseline_end  ="2014-12-31",
                                  year_peak=2016,
                                  window_days=60){
  baseline_start <- as.Date(baseline_start)
  baseline_end   <- as.Date(baseline_end)
  
  base <- centroid_df %>%
    filter(date >= baseline_start, date <= baseline_end)
  
  lat0 <- mean(base$lat_c, na.rm=TRUE)
  off0 <- if(all(is.na(base$off_c))) NA_real_ else mean(base$off_c, na.rm=TRUE)
  
  df16 <- centroid_df %>%
    filter(year(date) == year_peak) %>%
    arrange(date) %>%
    mutate(
      