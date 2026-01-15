# Nested logit model using 30 days state dependency # 

rm(list=ls())

library(apollo)
library(tidyverse)
library(slider)
library(lubridate)

apollo_initialise()

setwd("D:/GitHub/EconAnalysis/Participation")
google_dir <- "G:/Mi unidad/"  # adjust if needed
cpue_rds   <- "D:/GitHub/EconAnalysis/Participation/R/CPUE_index.rds"
sdm_dir    <- "SDMs"


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



# ---- SDM + CPUE (global) ----
MSQD_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_msqd.rds"), "MSQD", "MSQD_SDM_90")
PSDN_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_psdn.rds"), "PSDN", "PSDN_SDM_60")
NANC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_nanc.rds"), "NANC", "NANC_SDM_60")
JMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_jmck.rds"), "JMCK", "JMCK_SDM_60")
CMCK_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_cmck.rds"), "CMCK", "CMCK_SDM_60")
PHRG_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_phrg.rds"), "PHRG", "PHRG_SDM_20")
ALBC_sdm <- prepare_sdm(file.path(sdm_dir,"sdm_albc.rds"), "ALBC", "albc_SDM_90")

sdm_all <- Reduce(function(x,y) left_join(x,y, by=c("PORT_AREA_CODE","date")),
                  list(MSQD_sdm, PSDN_sdm, NANC_sdm, JMCK_sdm, CMCK_sdm, PHRG_sdm, ALBC_sdm))

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


no_sdm_species <- c("BTNA","YTNA","DCRB","SOCK")

run_cluster <- function(cluster_id, rds_file, alts, apollo_beta_base, apollo_fixed_base,
                        apollo_prob_fun, case_vars,
                        use_d_c_by_spec = c(daily=1, MA7=1, MA14=0, MA30=0, t1daily=1)){
  message("\n===== Running ", cluster_id, " =====")

  apollo_control = list(
    modelName       = paste0("NL_participation_model_", cluster_id, "_R&R"),
    modelDescr      = "Participation, location and target species decisions",
    indivID         = "fished_vessel_anon",
    outputDirectory = file.path("R","output"),
    panelData       = TRUE,
    nCores          = 18,
    workInLogs      = TRUE
  )

  long_data <- readRDS(rds_file) %>%
    select(any_of(c("set_date","fished_haul_anon","fished_vessel_anon","selection","fished",
                    "mean_avail","mean_price", "mean_price_3", "wind_max_220_mh","dist_to_cog","dist_port_to_catch_area_zero",
                    "psdnclosure","btnaclosure","msqdclosure","waclosure", "dcrbclosure", "dcrbclosurewad",
                    "dummy_prev_days","dummy_prev_year_days","unem_rate","d_d","d_cd","weekend"))) %>%
    mutate(selection = tolower(gsub("-", "_", selection)),
           date = as.Date(set_date)) %>%
    mutate(
      PORT_AREA_CODE = case_when(selection=="no_participation" ~ "NOPART",
                                 TRUE ~ str_to_upper(str_extract(selection, "^[a-z]{3}"))),
      sp4 = case_when(selection=="no_participation" ~ "NOPART",
                      TRUE ~ str_to_upper(str_extract(selection, "[a-z]{4}$")))
    )

  long_data2 <- long_data %>%
    left_join(sdm_all, by = c("PORT_AREA_CODE", "date")) %>%
    left_join(
      cpue_expanded,
      by = c("PORT_AREA_CODE", "date", "sp4" = "Species_Dominant")
    ) %>%
    mutate(
      CPUE_avail_t1 = dplyr::coalesce(CPUE_MA30_t1, CPUE_MA90_t1),
      used_90d_fallback = as.integer(is.na(CPUE_MA30_t1) & !is.na(CPUE_MA90_t1)),
      is_CPUE = as.integer(sp4 %in% no_sdm_species)
    )
  
  
  long_data2 <- long_data2 %>%
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
      ),
      mean_avail_MA7 = case_when(
        sp4=="MSQD" ~ MSQD_MA7_t1,
        sp4=="PSDN" ~ PSDN_MA7_t1,
        sp4=="NANC" ~ NANC_MA7_t1,
        sp4=="JMCK" ~ JMCK_MA7_t1,
        sp4=="CMCK" ~ CMCK_MA7_t1,
        sp4=="PHRG" ~ PHRG_MA7_t1,
        sp4=="ALBC" ~ ALBC_MA7_t1,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      ),
      mean_avail_MA14 = case_when(
        sp4=="MSQD" ~ MSQD_MA14_t1,
        sp4=="PSDN" ~ PSDN_MA14_t1,
        sp4=="NANC" ~ NANC_MA14_t1,
        sp4=="JMCK" ~ JMCK_MA14_t1,
        sp4=="CMCK" ~ CMCK_MA14_t1,
        sp4=="PHRG" ~ PHRG_MA14_t1,
        sp4=="ALBC" ~ ALBC_MA14_t1,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      ),
      mean_avail_MA30 = case_when(
        sp4=="MSQD" ~ MSQD_MA30_t1,
        sp4=="PSDN" ~ PSDN_MA30_t1,
        sp4=="NANC" ~ NANC_MA30_t1,
        sp4=="JMCK" ~ JMCK_MA30_t1,
        sp4=="CMCK" ~ CMCK_MA30_t1,
        sp4=="PHRG" ~ PHRG_MA30_t1,
        sp4=="ALBC" ~ ALBC_MA30_t1,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      ),
      mean_avail_t1_daily = case_when(
        sp4=="MSQD" ~ MSQD_t1,
        sp4=="PSDN" ~ PSDN_t1,
        sp4=="NANC" ~ NANC_t1,
        sp4=="JMCK" ~ JMCK_t1,
        sp4=="CMCK" ~ CMCK_t1,
        sp4=="PHRG" ~ PHRG_t1,
        sp4=="ALBC" ~ ALBC_t1,
        sp4 %in% no_sdm_species ~ CPUE_avail_t1,
        sp4=="NOPART" ~ 0,
        TRUE ~ NA_real_
      )
    )

  avail_specs <- list(daily="mean_avail_daily", MA7="mean_avail_MA7", MA14="mean_avail_MA14",
                     MA30="mean_avail_MA30", t1daily="mean_avail_t1_daily")
  data_by_avail <- lapply(avail_specs, function(v) prepare_avail_spec(long_data2, v))
  databases <- lapply(names(data_by_avail), function(nm) build_database(data_by_avail[[nm]], alts, case_vars))
  names(databases) <- names(data_by_avail)
  
  models <- list()
  
  for(spec in names(databases)){
    
    apollo_control$modelName <- paste0("NL_participation_model_", cluster_id, "_R&R_", spec)
    
    database <- databases[[spec]]
    assign("database", database, envir = .GlobalEnv)
    
    dc_cols <- grep("^d_c_", names(database), value = TRUE)
    has_dc  <- (length(dc_cols) > 0) && any(sapply(dc_cols, function(v) any(database[[v]] == 1, na.rm = TRUE)))
    want_dc <- isTRUE(use_d_c_by_spec[[spec]])
    dc_ok   <- has_dc && want_dc
    
    apollo_beta  <- apollo_beta_base
    apollo_fixed <- apollo_fixed_base
    if(!dc_ok) apollo_fixed <- unique(c(apollo_fixed, "B_d_c"))
    apollo_fixed <- intersect(apollo_fixed, names(apollo_beta))
    
    
    # >>> CLAVE: subir ambos al GlobalEnv ANTES de validateInputs
    assign("apollo_beta",  apollo_beta,  envir = .GlobalEnv)
    assign("apollo_fixed", apollo_fixed, envir = .GlobalEnv)
    assign("database",     database,     envir = .GlobalEnv)
    assign("apollo_control", apollo_control, envir = .GlobalEnv)

    apollo_inputs <- apollo_validateInputs()
    
    apollo_inputs$use_d_c <- as.numeric(dc_ok)
    
    # apollo_beta <- apollo_readBeta(apollo_beta, apollo_fixed,
    #                                paste0("NL_participation_model_", cluster_id),
    #                                overwriteFixed = FALSE)
    
    cat("\n---", cluster_id, spec,
        "dc_ok=", dc_ok,
        "B_d_c fixed=", ("B_d_c" %in% apollo_fixed), "\n")
    
    
    assign("apollo_beta", apollo_beta, envir = .GlobalEnv)
    
  model_spec <- apollo_estimate(
      apollo_beta, apollo_fixed,
      apollo_prob_fun, apollo_inputs,
      estimate_settings = list(
        estimationRoutine = "bfgs"
      )
    )
    
  apollo_saveOutput(model_spec, saveOutput_settings = list(printT1 = 1))
    
  models[[spec]] <- list(
      model          = model_spec,
      database_wide  = database,        # <- CLAVE
      apollo_control = apollo_control,  # <- CLAVE
      apollo_beta    = apollo_beta,     # opcional, útil para debugging
      apollo_fixed   = apollo_fixed     # opcional
    )
    
  }
  
  
  unwrap_model <- function(x){
    if(is.list(x) && !is.null(x$model)) return(x$model)
    return(x)
  }
  
  get_nObs <- function(x){
    # x puede ser modelo o list(model=..., inputs=...)
    if(is.list(x) && !is.null(x$model)){
      m <- x$model
      inp <- x$inputs
      n <- m$nObs
      if(is.null(n)) n <- inp$nObs
      if(is.null(n)) n <- nrow(inp$database)
      return(as.numeric(n))
    } else {
      m <- x
      n <- m$nObs
      if(is.null(n) && !is.null(m$apollo_inputs)) n <- m$apollo_inputs$nObs
      if(is.null(n) && !is.null(m$apollo_inputs$database)) n <- nrow(m$apollo_inputs$database)
      return(as.numeric(n))
    }
  }
  
  # --- dentro de run_cluster(), reemplaza tu comp por esto ---
  comp <- data.frame(
    cluster = cluster_id,
    spec    = names(models),
    LL      = sapply(models, function(x) unwrap_model(x)$maximum),
    k       = sapply(models, function(x) length(unwrap_model(x)$estimate)),
    nObs    = sapply(models, get_nObs)
  ) %>%
    mutate(
      AIC = 2*k - 2*LL,
      BIC = log(nObs)*k - 2*LL
    )
  
  return(list(models=models, comp=comp))
}


# ===================== Cluster c4 =====================

apollo_beta_c4 <- c(
  asc_sfa_nanc                    = -4.897334,
  asc_laa_nanc                    = -5.280470,
  asc_laa_cmck                    = -4.176291,
  asc_laa_msqd                    = -3.220271,
  asc_laa_ytna                    = -5.860792,
  asc_mna_msqd                    = -3.697772,
  asc_sba_msqd                    = -3.210438,
  asc_laa_btna                    = -7.265418,
  asc_sfa_msqd                    = -3.748681,
  asc_mna_psdn                    = -4.484919,
  asc_sba_cmck                    = -4.600359,
  asc_mra_msqd                    = -4.334389,
  asc_laa_psdn                    = -4.774108,
  asc_mna_nanc                    = -4.518065,
  c_psdn                          = -1.317069,
  w_msqd                          = -3.629525,
  theta_part                      =  3.757583,
  theta_cmck                      = 12.000000,
  theta_msqd                      =  1.356727,
  theta_psdn                      =  0.539975,
  theta_nanc                      =  0.021872,
  theta_tuna                      = 12.000000,
  B_mean_avail_nanc               =  1.353373,
  B_mean_avail_cmck               = -3.943998,
  B_mean_avail_msqd               =  2.448324,
  B_mean_avail_ytna               =  1.601632,
  B_mean_avail_btna               =  3.347774,
  B_mean_avail_psdn               = -1.776611,
  B_mean_price                    =  0.255586,
  B_wind_max_220_mh               = -0.050706,
  B_d_d                           = -2.249150,
  B_d_cd                          = -2.290440,
  B_d_c                           = -0.962611,
  B_dist_port_to_catch_area_zero  = -0.010698,
  B_dummy_prev_days               =  2.631406,
  B_dummy_prev_year_days          =  0.261214,
  B_unem_rate_part                =  0.196018,
  asc_no_participation            =  0.000000,
  B_dist_to_cog                   = -0.002598)

apollo_fixed_c4 <- c("asc_no_participation", "theta_cmck", "theta_tuna")

apollo_probabilities_c4 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  if(use_d_c == 0){
    B_d_c <- 0
  }
  
  lambda_part <- 1/(1 + exp(-theta_part))
  lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
  lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
  lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))
  P <- list(); V <- list()
  V[["sfa_nanc"]] <- asc_sfa_nanc + B_mean_avail_nanc*mean_avail_sfa_nanc + B_mean_price*mean_price_sfa_nanc + B_wind_max_220_mh*wind_max_220_mh_sfa_nanc + B_d_d*d_d_sfa_nanc + B_d_cd*d_cd_sfa_nanc + B_d_c*d_c_sfa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days*dummy_prev_days_sfa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_sfa_nanc + B_unem_rate_part*unem_rate_sfa_nanc + B_dist_to_cog*dist_to_cog_sfa_nanc + (B_d_c*use_d_c) * d_c_sfa_nanc                                        
  V[["laa_nanc"]] <- asc_laa_nanc + B_mean_avail_nanc*mean_avail_laa_nanc + B_mean_price*mean_price_laa_nanc + B_wind_max_220_mh*wind_max_220_mh_laa_nanc + B_d_d*d_d_laa_nanc + B_d_cd*d_cd_laa_nanc + B_d_c*d_c_laa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days*dummy_prev_days_laa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_laa_nanc + B_unem_rate_part*unem_rate_laa_nanc + B_dist_to_cog*dist_to_cog_laa_nanc + (B_d_c*use_d_c) * d_c_laa_nanc                                        
  V[["laa_cmck"]] <- asc_laa_cmck + B_mean_avail_cmck*mean_avail_laa_cmck + B_mean_price*mean_price_laa_cmck + B_wind_max_220_mh*wind_max_220_mh_laa_cmck + B_d_d*d_d_laa_cmck + B_d_cd*d_cd_laa_cmck + B_d_c*d_c_laa_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days*dummy_prev_days_laa_cmck + B_dummy_prev_year_days*dummy_prev_year_days_laa_cmck + B_unem_rate_part*unem_rate_laa_cmck + B_dist_to_cog*dist_to_cog_laa_cmck + (B_d_c*use_d_c) * d_c_laa_cmck                                        
  V[["laa_msqd"]] <- asc_laa_msqd + B_mean_avail_msqd*mean_avail_laa_msqd + B_mean_price*mean_price_laa_msqd + B_wind_max_220_mh*wind_max_220_mh_laa_msqd + B_d_d*d_d_laa_msqd + B_d_cd*d_cd_laa_msqd + B_d_c*d_c_laa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days*dummy_prev_days_laa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_laa_msqd + B_unem_rate_part*unem_rate_laa_msqd + B_dist_to_cog*dist_to_cog_laa_msqd + (B_d_c*use_d_c) * d_c_laa_msqd + w_msqd * weekend                        
  V[["laa_ytna"]] <- asc_laa_ytna + B_mean_avail_ytna*mean_avail_laa_ytna + B_mean_price*mean_price_laa_ytna + B_wind_max_220_mh*wind_max_220_mh_laa_ytna + B_d_d*d_d_laa_ytna + B_d_cd*d_cd_laa_ytna + B_d_c*d_c_laa_ytna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days*dummy_prev_days_laa_ytna + B_dummy_prev_year_days*dummy_prev_year_days_laa_ytna + B_unem_rate_part*unem_rate_laa_ytna + B_dist_to_cog*dist_to_cog_laa_ytna + (B_d_c*use_d_c) * d_c_laa_ytna                                         
  V[["mna_msqd"]] <- asc_mna_msqd + B_mean_avail_msqd*mean_avail_mna_msqd + B_mean_price*mean_price_mna_msqd + B_wind_max_220_mh*wind_max_220_mh_mna_msqd + B_d_d*d_d_mna_msqd + B_d_cd*d_cd_mna_msqd + B_d_c*d_c_mna_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days*dummy_prev_days_mna_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mna_msqd + B_unem_rate_part*unem_rate_mna_msqd + B_dist_to_cog*dist_to_cog_mna_msqd + (B_d_c*use_d_c) * d_c_mna_msqd + w_msqd * weekend                        
  V[["sba_msqd"]] <- asc_sba_msqd + B_mean_avail_msqd*mean_avail_sba_msqd + B_mean_price*mean_price_sba_msqd + B_wind_max_220_mh*wind_max_220_mh_sba_msqd + B_d_d*d_d_sba_msqd + B_d_cd*d_cd_sba_msqd + B_d_c*d_c_sba_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days*dummy_prev_days_sba_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sba_msqd + B_unem_rate_part*unem_rate_sba_msqd + B_dist_to_cog*dist_to_cog_sba_msqd + (B_d_c*use_d_c) * d_c_sba_msqd + w_msqd * weekend                        
  V[["laa_btna"]] <- asc_laa_btna + B_mean_avail_btna*mean_avail_laa_btna + B_mean_price*mean_price_laa_btna + B_wind_max_220_mh*wind_max_220_mh_laa_btna + B_d_d*d_d_laa_btna + B_d_cd*d_cd_laa_btna + B_d_c*d_c_laa_btna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days*dummy_prev_days_laa_btna + B_dummy_prev_year_days*dummy_prev_year_days_laa_btna + B_unem_rate_part*unem_rate_laa_btna + B_dist_to_cog*dist_to_cog_laa_btna + (B_d_c*use_d_c) * d_c_laa_btna                          
  V[["sfa_msqd"]] <- asc_sfa_msqd + B_mean_avail_msqd*mean_avail_sfa_msqd + B_mean_price*mean_price_sfa_msqd + B_wind_max_220_mh*wind_max_220_mh_sfa_msqd + B_d_d*d_d_sfa_msqd + B_d_cd*d_cd_sfa_msqd + B_d_c*d_c_sfa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days*dummy_prev_days_sfa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sfa_msqd + B_unem_rate_part*unem_rate_sfa_msqd + B_dist_to_cog*dist_to_cog_sfa_msqd + (B_d_c*use_d_c) * d_c_sfa_msqd + w_msqd * weekend                        
  V[["mna_psdn"]] <- asc_mna_psdn + B_mean_avail_psdn*mean_avail_mna_psdn + B_mean_price*mean_price_mna_psdn + B_wind_max_220_mh*wind_max_220_mh_mna_psdn + B_d_d*d_d_mna_psdn + B_d_cd*d_cd_mna_psdn + B_d_c*d_c_mna_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days*dummy_prev_days_mna_psdn + B_dummy_prev_year_days*dummy_prev_year_days_mna_psdn + B_unem_rate_part*unem_rate_mna_psdn + B_dist_to_cog*dist_to_cog_mna_psdn + (B_d_c*use_d_c) * d_c_mna_psdn + c_psdn * psdnclosure 
  V[["sba_cmck"]] <- asc_sba_cmck + B_mean_avail_cmck*mean_avail_sba_cmck + B_mean_price*mean_price_sba_cmck + B_wind_max_220_mh*wind_max_220_mh_sba_cmck + B_d_d*d_d_sba_cmck + B_d_cd*d_cd_sba_cmck + B_d_c*d_c_sba_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days*dummy_prev_days_sba_cmck + B_dummy_prev_year_days*dummy_prev_year_days_sba_cmck + B_unem_rate_part*unem_rate_sba_cmck + B_dist_to_cog*dist_to_cog_sba_cmck + (B_d_c*use_d_c) * d_c_sba_cmck                      
  V[["mra_msqd"]] <- asc_mra_msqd + B_mean_avail_msqd*mean_avail_mra_msqd + B_mean_price*mean_price_mra_msqd + B_wind_max_220_mh*wind_max_220_mh_mra_msqd + B_d_d*d_d_mra_msqd + B_d_cd*d_cd_mra_msqd + B_d_c*d_c_mra_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days*dummy_prev_days_mra_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mra_msqd + B_unem_rate_part*unem_rate_mra_msqd + B_dist_to_cog*dist_to_cog_mra_msqd + (B_d_c*use_d_c) * d_c_mra_msqd + w_msqd * weekend                        
  V[["laa_psdn"]] <- asc_laa_psdn + B_mean_avail_psdn*mean_avail_laa_psdn + B_mean_price*mean_price_laa_psdn + B_wind_max_220_mh*wind_max_220_mh_laa_psdn + B_d_d*d_d_laa_psdn + B_d_cd*d_cd_laa_psdn + B_d_c*d_c_laa_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days*dummy_prev_days_laa_psdn + B_dummy_prev_year_days*dummy_prev_year_days_laa_psdn + B_unem_rate_part*unem_rate_laa_psdn + B_dist_to_cog*dist_to_cog_laa_psdn + (B_d_c*use_d_c) * d_c_laa_psdn + c_psdn * psdnclosure 
  V[["mna_nanc"]] <- asc_mna_nanc + B_mean_avail_nanc*mean_avail_mna_nanc + B_mean_price*mean_price_mna_nanc + B_wind_max_220_mh*wind_max_220_mh_mna_nanc + B_d_d*d_d_mna_nanc + B_d_cd*d_cd_mna_nanc + B_d_c*d_c_mna_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days*dummy_prev_days_mna_nanc + B_dummy_prev_year_days*dummy_prev_year_days_mna_nanc + B_unem_rate_part*unem_rate_mna_nanc + B_dist_to_cog*dist_to_cog_mna_nanc + (B_d_c*use_d_c) * d_c_mna_nanc                      
  V[["no_participation"]] <- asc_no_participation 

  nlNests <- list(root=1, part=lambda_part, cmck=lambda_cmck, msqd=lambda_msqd, psdn=lambda_psdn, nanc=lambda_nanc, tuna=lambda_tuna)
  
  nlStructure <- list()
  nlStructure[["root"]] <- c("no_participation","part")
  nlStructure[["part"]] <- c("cmck","msqd","psdn","nanc","tuna")
  nlStructure[["cmck"]] <- c("laa_cmck","sba_cmck")
  nlStructure[["msqd"]] <- c("laa_msqd","mna_msqd","mra_msqd","sba_msqd","sfa_msqd")
  nlStructure[["psdn"]] <- c("laa_psdn","mna_psdn")
  nlStructure[["nanc"]] <- c("laa_nanc","mna_nanc","sfa_nanc")
  nlStructure[["tuna"]] <- c("laa_ytna","laa_btna")
  
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
  nl_settings <- list(
    alternatives = c(sfa_nanc=1, laa_nanc=2, laa_cmck=3, laa_msqd=4, laa_ytna=5, mna_msqd=6, sba_msqd=7, laa_btna=8, sfa_msqd=9, mna_psdn=10, sba_cmck=11, mra_msqd=12, laa_psdn=13, mna_nanc=14, no_participation=15),
    avail=avail, choiceVar=choice, utilities=V, nlNests=nlNests, nlStructure=nlStructure
  )
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # --- OOS/prediction: NO panelProd (evita underflow a 0) ---
  if(functionality == "prediction"){
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  # --- Estimación normal ---
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
}


# ===================== Cluster c5 (from n_logit_c5.R) =====================

apollo_beta_c5 = c(
  B_mean_avail_msqd              =    1.748535,
  B_mean_avail_psdn              =    0.226588,
  B_mean_avail_cmck              =   -1.484157,
  B_mean_avail_nanc              =    4.193747,
  B_mean_avail_albc              =    2.202953,
  B_mean_avail_dcrb              =    5.741342,
  B_wind_max_220_mh              =   -0.065261,
  B_dist_to_cog                  = -8.8681e-04,
  B_dist_port_to_catch_area_zero =   -0.005776,
  B_dummy_prev_days              =    2.861858,
  B_dummy_prev_year_days         =    0.366907,
  B_unem_rate                    =    0.414837,
  B_d_d                          =   -2.016306,
  B_d_c                          =   -0.909442,
  B_d_cd                         =   -9.885604,
  c_psdn                         =   -2.032304,
  c_dcrb                         =   -1.485335,
  B_mean_price_part              =    4.600908,
  B_mean_price_crab              =   -0.038246,
  asc_mna_msqd                   =   -9.029365,
  asc_sba_msqd                   =   -8.888468,
  asc_mra_msqd                   =   -9.946981,
  asc_laa_msqd                   =   -9.389892,
  asc_npa_msqd                   =   -9.545131,
  asc_sfa_msqd                   =   -9.460032,
  asc_cba_msqd                   =   -9.295058,
  asc_laa_psdn                   =   -7.649731,
  asc_clo_psdn                   =   -5.486854,
  asc_cwa_psdn                   =   -3.982644,
  asc_clw_psdn                   =   -4.326057,
  asc_sba_cmck                   =   -7.286376,
  asc_laa_cmck                   =   -8.460761,
  asc_laa_nanc                   =   -8.756927,
  asc_cwa_albc                   =  -19.907392,
  asc_cwa_dcrb                   =   -6.945807,
  asc_clw_dcrb                   =   -7.086131,
  theta_part                     =    1.109144,
  theta_cmck                     =   10.761289,
  theta_msqd                     =   11.124362,
  theta_psdn                     =    0.143028,
  theta_part_crab                =   10.694908,
  w_msqd                         =   -3.940726,
  asc_no_participation           =    0.000000)

apollo_fixed_c5 = c("asc_no_participation", "theta_cmck", "theta_part_crab", "theta_msqd", "theta_psdn",
                    "asc_mna_msqd", "asc_sba_msqd", "asc_mra_msqd", "asc_laa_msqd",
                    "asc_npa_msqd", "asc_sfa_msqd", "asc_cba_msqd", "asc_laa_psdn",
                    "asc_clo_psdn", "asc_cwa_psdn", "asc_clw_psdn", "asc_sba_cmck",
                    "asc_laa_cmck", "asc_laa_nanc", "asc_cwa_albc", "asc_cwa_dcrb", "asc_clw_dcrb",
                    "B_d_cd")


apollo_probabilities_c5 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  if(use_d_c == 0){
    B_d_c <- 0
  }
  
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_cmck <- lambda_part * (1 / (1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1 / (1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))
  lambda_part_crab <- 1 / (1 + exp(-theta_part_crab))
  
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_price_part * mean_price_3_mna_msqd + B_mean_avail_msqd * mean_avail_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_unem_rate * unem_rate_mna_msqd                                    + B_d_d * d_d_mna_msqd + (B_d_c*use_d_c) * d_c_mna_msqd + w_msqd * weekend                            
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_price_part * mean_price_3_sba_msqd + B_mean_avail_msqd * mean_avail_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_unem_rate * unem_rate_sba_msqd                                    + B_d_d * d_d_sba_msqd + (B_d_c*use_d_c) * d_c_sba_msqd + w_msqd * weekend                            
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_price_part * mean_price_3_mra_msqd + B_mean_avail_msqd * mean_avail_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_unem_rate * unem_rate_mra_msqd                                    + B_d_d * d_d_mra_msqd + (B_d_c*use_d_c) * d_c_mra_msqd + w_msqd * weekend                            
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_price_part * mean_price_3_laa_msqd + B_mean_avail_msqd * mean_avail_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_unem_rate * unem_rate_laa_msqd                                    + B_d_d * d_d_laa_msqd + (B_d_c*use_d_c) * d_c_laa_msqd + w_msqd * weekend                            
  V[["npa_msqd"]]         = asc_npa_msqd + B_mean_price_part * mean_price_3_npa_msqd + B_mean_avail_msqd * mean_avail_npa_msqd + B_wind_max_220_mh * wind_max_220_mh_npa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_npa_msqd + B_dummy_prev_days * dummy_prev_days_npa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_npa_msqd + B_dist_to_cog * dist_to_cog_npa_msqd + B_unem_rate * unem_rate_npa_msqd                                    + B_d_d * d_d_npa_msqd + (B_d_c*use_d_c) * d_c_npa_msqd     
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_price_part * mean_price_3_sfa_msqd + B_mean_avail_msqd * mean_avail_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_unem_rate * unem_rate_sfa_msqd                                    + B_d_d * d_d_sfa_msqd + (B_d_c*use_d_c) * d_c_sfa_msqd + w_msqd * weekend                            
  V[["cba_msqd"]]         = asc_cba_msqd + B_mean_price_part * mean_price_3_cba_msqd + B_mean_avail_msqd * mean_avail_cba_msqd + B_wind_max_220_mh * wind_max_220_mh_cba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_msqd + B_dummy_prev_days * dummy_prev_days_cba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_cba_msqd + B_dist_to_cog * dist_to_cog_cba_msqd + B_unem_rate * unem_rate_cba_msqd                                    + B_d_d * d_d_cba_msqd + (B_d_c*use_d_c) * d_c_cba_msqd                              
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_price_part * mean_price_3_laa_psdn + B_mean_avail_psdn * mean_avail_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_unem_rate * unem_rate_laa_psdn + c_psdn * psdnclosure             + B_d_d * d_d_laa_psdn + (B_d_c*use_d_c) * d_c_laa_psdn     
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn + B_mean_avail_psdn * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_unem_rate * unem_rate_clo_psdn + c_psdn * psdnclosure             + B_d_d * d_d_clo_psdn + (B_d_c*use_d_c) * d_c_clo_psdn                             
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn + B_mean_avail_psdn * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_unem_rate * unem_rate_cwa_psdn + c_psdn * psdnclosure             + B_d_d * d_d_cwa_psdn + (B_d_c*use_d_c) * d_c_cwa_psdn                            
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn + B_mean_avail_psdn * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_unem_rate * unem_rate_clw_psdn + c_psdn * psdnclosure             + B_d_d * d_d_clw_psdn + (B_d_c*use_d_c) * d_c_clw_psdn                            
  V[["sba_cmck"]]         = asc_sba_cmck + B_mean_price_part * mean_price_3_sba_cmck + B_mean_avail_cmck * mean_avail_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_dist_to_cog * dist_to_cog_sba_cmck + B_unem_rate * unem_rate_sba_cmck                                    + B_d_d * d_d_sba_cmck + (B_d_c*use_d_c) * d_c_sba_cmck                             
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_price_part * mean_price_3_laa_cmck + B_mean_avail_cmck * mean_avail_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_unem_rate * unem_rate_laa_cmck                                    + B_d_d * d_d_laa_cmck + (B_d_c*use_d_c) * d_c_laa_cmck                             
  V[["laa_nanc"]]         = asc_laa_nanc + B_mean_price_part * mean_price_3_laa_nanc + B_mean_avail_nanc * mean_avail_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_dist_to_cog * dist_to_cog_laa_nanc + B_unem_rate * unem_rate_laa_nanc                                    + B_d_d * d_d_laa_nanc + (B_d_c*use_d_c) * d_c_laa_nanc          
  V[["cwa_albc"]]         = asc_cwa_albc + B_mean_price_part * mean_price_3_cwa_albc + B_mean_avail_albc * mean_avail_cwa_albc + B_wind_max_220_mh * wind_max_220_mh_cwa_albc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_albc + B_dummy_prev_days * dummy_prev_days_cwa_albc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_albc + B_dist_to_cog * dist_to_cog_cwa_albc + B_unem_rate * unem_rate_cwa_albc                                    + B_d_d * d_d_cwa_albc + (B_d_c*use_d_c) * d_c_cwa_albc                             
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_price_crab * mean_price_3_cwa_dcrb + B_mean_avail_dcrb * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_unem_rate * unem_rate_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb + B_d_d * d_d_cwa_dcrb + (B_d_c*use_d_c) * d_c_cwa_dcrb + B_d_cd * d_cd_cwa_dcrb                             
  V[["clw_dcrb"]]         = asc_clw_dcrb + B_mean_price_crab * mean_price_3_clw_dcrb + B_mean_avail_dcrb * mean_avail_clw_dcrb + B_wind_max_220_mh * wind_max_220_mh_clw_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_dcrb + B_dummy_prev_days * dummy_prev_days_clw_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_clw_dcrb + B_dist_to_cog * dist_to_cog_clw_dcrb + B_unem_rate * unem_rate_clw_dcrb + c_dcrb * dcrbclosurewad_clw_dcrb + B_d_d * d_d_clw_dcrb + (B_d_c*use_d_c) * d_c_clw_dcrb + B_d_cd * d_cd_clw_dcrb                             
  V[["no_participation"]] = asc_no_participation
  
  ### Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      cmck = lambda_cmck, msqd = lambda_msqd, psdn = lambda_psdn, 
                      part_crab = lambda_part_crab)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part", "part_crab")
  nlStructure[["part"]] = c("msqd", "cmck", "psdn", "laa_nanc", "cwa_albc" )
  nlStructure[["msqd"]] = c("mna_msqd", "sba_msqd", "mra_msqd", "laa_msqd", "npa_msqd", "sfa_msqd", "cba_msqd" )
  nlStructure[["cmck"]] = c("sba_cmck", "laa_cmck")
  nlStructure[["psdn"]] = c("laa_psdn", "clo_psdn", "cwa_psdn", "clw_psdn" )
  nlStructure[["part_crab"]] = c("cwa_dcrb", "clw_dcrb")


N <- nrow(apollo_inputs$database)
  
avail <- list(
  mna_msqd = 1 - msqdclosure,
  sba_msqd = 1 - msqdclosure,
  mra_msqd = 1 - msqdclosure,
  laa_msqd = 1 - msqdclosure,
  npa_msqd = 1,
  sfa_msqd = 1 - msqdclosure,
  cba_msqd = 1,
  laa_psdn = 1,
  clo_psdn = 1,
  cwa_psdn = 1 - waclosure_cwa_psdn,
  clw_psdn = 1 - waclosure_clw_psdn,
  sba_cmck = 1,
  laa_cmck = 1,
  laa_nanc = 1,
  cwa_albc = 1,
  cwa_dcrb = 1,
  clw_dcrb = 1,
  no_participation = 1
)


  nl_settings <- list(
    alternatives = c(
	  mna_msqd = 1, sba_msqd = 2, mra_msqd = 3, laa_msqd = 4, npa_msqd = 5, sfa_msqd = 6,  
	  cba_msqd = 7, laa_psdn = 8, clo_psdn = 9, cwa_psdn = 10, clw_psdn = 11, sba_cmck = 12,
	  laa_cmck = 13, laa_nanc = 14, cwa_albc = 15, cwa_dcrb = 16, clw_dcrb = 17, no_participation = 18),
    avail = avail,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # --- OOS/prediction: NO panelProd (evita underflow a 0) ---
  if(functionality == "prediction"){
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  # --- Estimación normal ---
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  

}


# ===================== Cluster c6 (from n_logit_c6.R) =====================

apollo_beta_c6=c(
  B_mean_avail_psdn               = -1.993196,
  B_mean_avail_nanc               =  1.395448,
  B_mean_avail_othr               = 10.216345,
  B_mean_avail_cmck               =  2.262217,
  B_wind_max_220_mh               = -0.052962,
  B_dist_to_cog                   = -0.004738,
  B_dist_port_to_catch_area_zero  = -0.016642,
  B_dummy_prev_days               =  2.314201,
  B_dummy_prev_year_days          =  0.300067,
  B_unem_rate                     =  0.257505,
  B_d_c                           =  0,
  B_d_d                           = -1.100474,
  B_d_cd                          = -8.132902,
  c_dcrb                          = -0.293244,
  B_mean_price_part               =  3.386718,
  B_mean_price_othr               =  0.019998,
  asc_cba_psdn                    = -2.322374,
  asc_clo_psdn                    = -2.990346,
  asc_clw_psdn                    = -2.785120,
  asc_cwa_psdn                    = -2.563083,
  asc_clo_nanc                    = -5.013189,
  asc_clw_nanc                    = -7.673003,
  asc_cwa_nanc                    = -4.220165,
  asc_clo_cmck                    = -4.783469,
  asc_cwa_dcrb                    = -9.176758,
  asc_nps_sock                    = -8.333882,
  theta_part                      =  0.182348,
  theta_nanc                      = 13.345222,
  theta_psdn                      =  1.240982,
  asc_no_participation            =  0.000000)

apollo_fixed_c6 <- c("asc_no_participation", "B_mean_price_othr",
                     "theta_part", "theta_nanc", "theta_psdn", "B_d_d", "B_d_cd", "c_dcrb",
                     "asc_cba_psdn", "asc_clo_psdn", "asc_clw_psdn", "asc_cwa_psdn",
                     "asc_clo_nanc", "asc_clw_nanc", "asc_cwa_nanc", "asc_clo_cmck",
                     "asc_cwa_dcrb", "asc_nps_sock", "B_unem_rate", "B_d_c")

apollo_probabilities_c6 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  if(use_d_c == 0){
    B_d_c <- 0
  }
  
  ### COnstruir lambda
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_nanc <- lambda_part * (1 / (1 + exp(-theta_nanc)))
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))
  
  
  ### Create list of probabilities P
  P = list()
  
# 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["cba_psdn"]]         = asc_cba_psdn + B_mean_avail_psdn * mean_avail_cba_psdn + B_wind_max_220_mh * wind_max_220_mh_cba_psdn + B_dist_to_cog * dist_to_cog_cba_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_psdn + B_dummy_prev_days * dummy_prev_days_cba_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cba_psdn  + B_unem_rate * unem_rate_cba_psdn + (B_d_c*use_d_c) * d_c_cba_psdn + B_d_d * d_d_cba_psdn + B_mean_price_part * mean_price_3_cba_psdn
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_avail_psdn * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn  + B_unem_rate * unem_rate_clo_psdn + (B_d_c*use_d_c) * d_c_clo_psdn + B_d_d * d_d_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_avail_psdn * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn  + B_unem_rate * unem_rate_clw_psdn + (B_d_c*use_d_c) * d_c_clw_psdn + B_d_d * d_d_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn 
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_avail_psdn * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn  + B_unem_rate * unem_rate_cwa_psdn + (B_d_c*use_d_c) * d_c_cwa_psdn + B_d_d * d_d_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn
  V[["clo_nanc"]]         = asc_clo_nanc + B_mean_avail_nanc * mean_avail_clo_nanc + B_wind_max_220_mh * wind_max_220_mh_clo_nanc + B_dist_to_cog * dist_to_cog_clo_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_nanc + B_dummy_prev_days * dummy_prev_days_clo_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clo_nanc  + B_unem_rate * unem_rate_clo_nanc + (B_d_c*use_d_c) * d_c_clo_nanc + B_d_d * d_d_clo_nanc + B_mean_price_part * mean_price_3_clo_nanc
  V[["clw_nanc"]]         = asc_clw_nanc + B_mean_avail_nanc * mean_avail_clw_nanc + B_wind_max_220_mh * wind_max_220_mh_clw_nanc + B_dist_to_cog * dist_to_cog_clw_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_nanc + B_dummy_prev_days * dummy_prev_days_clw_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clw_nanc  + B_unem_rate * unem_rate_clw_nanc + (B_d_c*use_d_c) * d_c_clw_nanc + B_d_d * d_d_clw_nanc + B_mean_price_part * mean_price_3_clw_nanc
  V[["cwa_nanc"]]         = asc_cwa_nanc + B_mean_avail_nanc * mean_avail_cwa_nanc + B_wind_max_220_mh * wind_max_220_mh_cwa_nanc + B_dist_to_cog * dist_to_cog_cwa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_nanc + B_dummy_prev_days * dummy_prev_days_cwa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_nanc  + B_unem_rate * unem_rate_cwa_nanc + (B_d_c*use_d_c) * d_c_cwa_nanc + B_d_d * d_d_cwa_nanc + B_mean_price_part * mean_price_3_cwa_nanc
  V[["clo_cmck"]]         = asc_clo_cmck + B_mean_avail_cmck * mean_avail_clo_cmck + B_wind_max_220_mh * wind_max_220_mh_clo_cmck + B_dist_to_cog * dist_to_cog_clo_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_cmck + B_dummy_prev_days * dummy_prev_days_clo_cmck + B_dummy_prev_year_days * dummy_prev_year_days_clo_cmck  + B_unem_rate * unem_rate_clo_cmck + (B_d_c*use_d_c) * d_c_clo_cmck + B_d_d * d_d_clo_cmck + B_mean_price_part * mean_price_3_clo_cmck 
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_avail_othr * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb  + B_unem_rate * unem_rate_cwa_dcrb + (B_d_c*use_d_c) * d_c_cwa_dcrb + B_d_d * d_d_cwa_dcrb + B_mean_price_othr * mean_price_3_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb
  V[["nps_sock"]]         = asc_nps_sock + B_mean_avail_othr * mean_avail_nps_sock + B_wind_max_220_mh * wind_max_220_mh_nps_sock + B_dist_to_cog * dist_to_cog_nps_sock + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_nps_sock + B_dummy_prev_days * dummy_prev_days_nps_sock + B_dummy_prev_year_days * dummy_prev_year_days_nps_sock  + B_unem_rate * unem_rate_nps_sock + (B_d_c*use_d_c) * d_c_nps_sock + B_d_d * d_d_nps_sock + B_mean_price_othr * mean_price_3_nps_sock + B_d_cd * d_cd_nps_sock                         
  V[["no_participation"]] = asc_no_participation 
  
  ## Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      nanc = lambda_nanc, psdn = lambda_psdn)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part", "cwa_dcrb", "nps_sock")
  nlStructure[["part"]] = c("nanc", "psdn", "clo_cmck")
  nlStructure[["nanc"]] = c("clo_nanc", "clw_nanc", "cwa_nanc")
  nlStructure[["psdn"]] = c("cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn")   ### Define settings for NL model
  
  avail <- list(
    cba_psdn         = 1 - psdnclosure,
    clo_psdn         = 1 - psdnclosure,
    clw_psdn         = (1 - psdnclosure) * (1 - waclosure_clw_psdn),
    cwa_psdn         = (1 - psdnclosure) * (1 - waclosure_cwa_psdn),
    clo_nanc         = 1,
    clw_nanc         = 1,
    cwa_nanc         = 1,
    clo_cmck         = 1,
    cwa_dcrb         = 1,
    nps_sock         = 1,
    no_participation = 1)
  
  
  nl_settings <- list(
    alternatives = c(cba_psdn =1,
                     clo_psdn =2,
                     clw_psdn =3,
                     cwa_psdn =4,
                     clo_nanc =5,
                     clw_nanc =6,
                     cwa_nanc =7,
                     clo_cmck =8,
                     cwa_dcrb =9,
                     nps_sock =10,
                     no_participation = 11),
    avail        = avail,
    choiceVar    = choice,
    utilities    = V
    ,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # --- OOS/prediction: NO panelProd (evita underflow a 0) ---
  if(functionality == "prediction"){
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  # --- Estimación normal ---
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
}

# ===================== Cluster c7 (from n_logit_c7.R) =====================

apollo_beta_c7 = c(
  B_mean_avail_mckl               = -1.923781,
  B_mean_avail_msqd               =  1.605385,
  B_mean_avail_psdn               =  0.230033,
  B_mean_avail_nanc               = -1.880464,
  B_mean_price                    =  0.718037,
  B_wind_max_220_mh               = -0.040749,
  B_dist_to_cog                   = -0.003076,
  B_dist_port_to_catch_area_zero  = -0.006335,
  B_dummy_prev_days               =  3.088275,
  B_dummy_prev_year_days          =  0.133980,
  c_psdn                          = -1.370940,
  B_d_d                           = -0.515820,
  B_unem_rate                     =  0.088761,
  w_msqd                          = -3.952120,
  asc_laa_cmck                    = -3.913923,
  asc_mna_cmck                    = -5.338789,
  asc_laa_jmck                    = -4.176227,
  asc_mna_jmck                    = -5.113068,
  asc_laa_msqd                    = -3.692253,
  asc_mra_msqd                    = -4.633509,
  asc_sba_msqd                    = -3.710028,
  asc_sfa_msqd                    = -4.128870,
  asc_mna_msqd                    = -3.607651,
  asc_laa_psdn                    = -4.062540,
  asc_mna_psdn                    = -4.832360,
  asc_mna_nanc                    = -4.579712,
  asc_sba_nanc                    = -2.896603,
  asc_sda_nanc                    = -3.952324,
  theta_part                      =  0.993484,
  theta_laa                       = 10.071425,
  theta_mna                       = 10.029563,
  theta_sba                       =  0.733464,
  asc_no_participation            =  0.000000)


apollo_fixed_c7 = c("asc_no_participation", "theta_mna",  "theta_laa", "theta_sba", "theta_part", 
                    "asc_laa_cmck", "asc_mna_cmck", "asc_laa_jmck", "asc_mna_jmck",  "asc_laa_msqd",
                    "asc_mra_msqd", "asc_sba_msqd", "asc_sfa_msqd", "asc_mna_msqd",
                    "asc_laa_psdn", "asc_mna_psdn",
                    "asc_mna_nanc", "asc_sba_nanc", "asc_sda_nanc",
                    "c_psdn", "B_d_d", "w_msqd", "B_unem_rate", "B_dist_to_cog", "B_wind_max_220_mh", "B_dist_port_to_catch_area_zero")



apollo_probabilities_c7 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  if(use_d_c == 0){
    B_d_c <- 0
  }
  
  
  ### Create Lambda
  
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_laa <- lambda_part * (1 / (1 + exp(-theta_laa)))
  lambda_sba <- lambda_part * (1 / (1 + exp(-theta_sba)))
  lambda_mna <- lambda_part * (1 / (1 + exp(-theta_mna)))
  
  
  
  ### Create list of probabilities P
  P = list()
  
  # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_avail_mckl * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck  + B_unem_rate * unem_rate_laa_cmck + B_d_d * d_d_laa_cmck + (B_d_c*use_d_c) * d_c_laa_cmck
  V[["mna_cmck"]]         = asc_mna_cmck + B_mean_avail_mckl * mean_avail_mna_cmck + B_mean_price * mean_price_mna_cmck + B_wind_max_220_mh * wind_max_220_mh_mna_cmck + B_dist_to_cog * dist_to_cog_mna_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_cmck + B_dummy_prev_days * dummy_prev_days_mna_cmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_cmck  + B_unem_rate * unem_rate_mna_cmck + B_d_d * d_d_mna_cmck + (B_d_c*use_d_c) * d_c_mna_cmck
  V[["laa_jmck"]]         = asc_laa_jmck + B_mean_avail_mckl * mean_avail_laa_jmck + B_mean_price * mean_price_laa_jmck + B_wind_max_220_mh * wind_max_220_mh_laa_jmck + B_dist_to_cog * dist_to_cog_laa_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_jmck + B_dummy_prev_days * dummy_prev_days_laa_jmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_jmck  + B_unem_rate * unem_rate_laa_jmck + B_d_d * d_d_laa_jmck + (B_d_c*use_d_c) * d_c_laa_jmck
  V[["mna_jmck"]]         = asc_mna_jmck + B_mean_avail_mckl * mean_avail_mna_jmck + B_mean_price * mean_price_mna_jmck + B_wind_max_220_mh * wind_max_220_mh_mna_jmck + B_dist_to_cog * dist_to_cog_mna_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_jmck + B_dummy_prev_days * dummy_prev_days_mna_jmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_jmck  + B_unem_rate * unem_rate_mna_jmck + B_d_d * d_d_mna_jmck + (B_d_c*use_d_c) * d_c_mna_jmck
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_avail_msqd * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd  + B_unem_rate * unem_rate_laa_msqd + B_d_d * d_d_laa_msqd + (B_d_c*use_d_c) * d_c_laa_msqd + w_msqd * weekend 
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_avail_msqd * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd  + B_unem_rate * unem_rate_mra_msqd + B_d_d * d_d_mra_msqd + (B_d_c*use_d_c) * d_c_mra_msqd + w_msqd * weekend 
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_avail_msqd * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd  + B_unem_rate * unem_rate_sba_msqd + B_d_d * d_d_sba_msqd + (B_d_c*use_d_c) * d_c_sba_msqd + w_msqd * weekend 
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_avail_msqd * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd  + B_unem_rate * unem_rate_sfa_msqd + B_d_d * d_d_sfa_msqd + (B_d_c*use_d_c) * d_c_sfa_msqd + w_msqd * weekend 
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_avail_msqd * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd  + B_unem_rate * unem_rate_mna_msqd + B_d_d * d_d_mna_msqd + (B_d_c*use_d_c) * d_c_mna_msqd + w_msqd * weekend 
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_avail_psdn * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn  + B_unem_rate * unem_rate_laa_psdn + B_d_d * d_d_laa_psdn + (B_d_c*use_d_c) * d_c_laa_psdn + c_psdn * psdnclosure                            
  V[["mna_psdn"]]         = asc_mna_psdn + B_mean_avail_psdn * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_dist_to_cog * dist_to_cog_mna_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn  + B_unem_rate * unem_rate_mna_psdn + B_d_d * d_d_mna_psdn + (B_d_c*use_d_c) * d_c_mna_psdn + c_psdn * psdnclosure
  V[["mna_nanc"]]         = asc_mna_nanc + B_mean_avail_nanc * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_dist_to_cog * dist_to_cog_mna_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc  + B_unem_rate * unem_rate_mna_nanc + B_d_d * d_d_mna_nanc + (B_d_c*use_d_c) * d_c_mna_nanc
  V[["sba_nanc"]]         = asc_sba_nanc + B_mean_avail_nanc * mean_avail_sba_nanc + B_mean_price * mean_price_sba_nanc + B_wind_max_220_mh * wind_max_220_mh_sba_nanc + B_dist_to_cog * dist_to_cog_sba_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_nanc + B_dummy_prev_days * dummy_prev_days_sba_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sba_nanc  + B_unem_rate * unem_rate_sba_nanc + B_d_d * d_d_sba_nanc + (B_d_c*use_d_c) * d_c_sba_nanc
  V[["sda_nanc"]]         = asc_sda_nanc + B_mean_avail_nanc * mean_avail_sda_nanc + B_mean_price * mean_price_sda_nanc + B_wind_max_220_mh * wind_max_220_mh_sda_nanc + B_dist_to_cog * dist_to_cog_sda_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sda_nanc + B_dummy_prev_days * dummy_prev_days_sda_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sda_nanc  + B_unem_rate * unem_rate_sda_nanc + B_d_d * d_d_sda_nanc + (B_d_c*use_d_c) * d_c_sda_nanc
  V[["no_participation"]] = asc_no_participation 
  

  ### Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      laa = lambda_laa, sba = lambda_sba, mna = lambda_mna)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part")
  nlStructure[["part"]] = c("laa", "mna", "sba", "mra_msqd", "sfa_msqd", "sda_nanc")
  nlStructure[["laa"]] = c("laa_cmck", "laa_jmck", "laa_msqd", "laa_psdn")
  nlStructure[["mna"]] = c("mna_cmck", "mna_jmck", "mna_msqd", "mna_psdn", "mna_nanc")   ### Define settings for NL model
  nlStructure[["sba"]] = c("sba_msqd", "sba_nanc")   ### Define settings for NL model
  
  
  avail= list(
    laa_cmck         = 1,
    mna_cmck         = 1,
    laa_jmck         = 1,
    mna_jmck         = 1,
    laa_msqd         = 1 - msqdclosure,
    mra_msqd         = 1 - msqdclosure,
    sba_msqd         = 1 - msqdclosure,
    sfa_msqd         = 1 - msqdclosure,
    mna_msqd         = 1 - msqdclosure,
    laa_psdn         = 1,
    mna_psdn         = 1,
    mna_nanc         = 1,
    sba_nanc         = 1,
    sda_nanc         = 1,
    no_participation = 1)
  
  
  nl_settings <- list(
    alternatives = c(
      laa_cmck = 1,
      mna_cmck = 2,
      laa_jmck = 3,
      mna_jmck = 4,
      laa_msqd = 5,
      mra_msqd = 6,
      sba_msqd = 7,
      sfa_msqd = 8,
      mna_msqd = 9,
      laa_psdn = 10,
      mna_psdn = 11,
      mna_nanc = 12,
      sba_nanc = 13,
      sda_nanc = 14,
      no_participation = 15),
    avail        = avail,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # --- OOS/prediction: NO panelProd (evita underflow a 0) ---
  if(functionality == "prediction"){
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  # --- Estimación normal ---
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
}


# ---- Run all clusters ----
use_d_c_by_spec <- c(daily=1, MA7=1, MA14=0, MA30=0, t1daily=1)  # tweak if needed

# res_c4 <- run_cluster("c4", paste0(google_dir,"Data/Anonymised data/part_model_c4.rds"),
#   alts=c("sfa_nanc", "laa_nanc", "laa_cmck", "laa_msqd", "laa_ytna", "mna_msqd", "sba_msqd", "laa_btna", "sfa_msqd", "mna_psdn", "sba_cmck", "mra_msqd", "laa_psdn", "mna_nanc", "no_participation"),
#   apollo_beta_base=apollo_beta_c4, apollo_fixed_base=apollo_fixed_c4,
#   apollo_prob_fun=apollo_probabilities_c4,
#   case_vars=c("weekend","psdnclosure","btnaclosure"),
#   use_d_c_by_spec=use_d_c_by_spec)
# saveRDS(res_c4, "res_c4.rds")
# saveRDS(apollo_beta_c4, "apollo_beta_c4.rds")
# saveRDS(apollo_fixed_c4, "apollo_fixed_c4.rds")
# saveRDS(apollo_probabilities_c4, "apollo_probabilities_c4.rds")
res_c4 <- readRDS("res_c4.rds")

# res_c5 <- run_cluster("c5", paste0(google_dir,"Data/Anonymised data/part_model_c5.rds"),
#   alts=c("mna_msqd", "sba_msqd", "mra_msqd", "laa_msqd", "npa_msqd", "sfa_msqd", "cba_msqd", "laa_psdn", "clo_psdn", "cwa_psdn", "clw_psdn", "sba_cmck", "laa_cmck", "laa_nanc", "cwa_albc", "cwa_dcrb", "clw_dcrb", "no_participation"),
#   apollo_beta_base=apollo_beta_c5, apollo_fixed_base=apollo_fixed_c5,
#   apollo_prob_fun=apollo_probabilities_c5,
#   case_vars=c("weekend","psdnclosure","msqdclosure"),
#   use_d_c_by_spec=use_d_c_by_spec)
# saveRDS(res_c5, "res_c5.rds")
# saveRDS(apollo_beta_c5, "apollo_beta_c5.rds")
# saveRDS(apollo_fixed_c5, "apollo_fixed_c5.rds")
# saveRDS(apollo_probabilities_c5, "apollo_probabilities_c5.rds")
res_c5 <- readRDS("res_c5.rds")

# res_c6 <- run_cluster("c6", paste0(google_dir,"Data/Anonymised data/part_model_c6.rds"),
#   alts=c("cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn", "clo_nanc", "clw_nanc", "cwa_nanc", "clo_cmck","cwa_dcrb", "nps_sock", "no_participation"),
#   apollo_beta_base=apollo_beta_c6, apollo_fixed_base=apollo_fixed_c6,
#   apollo_prob_fun=apollo_probabilities_c6,
#   case_vars=c("weekend","psdnclosure","msqdclosure"),
#   use_d_c_by_spec=use_d_c_by_spec)
# saveRDS(res_c6, "res_c6.rds")
# saveRDS(apollo_beta_c6, "apollo_beta_c6.rds")
# saveRDS(apollo_fixed_c6, "apollo_fixed_c6.rds")
# saveRDS(apollo_probabilities_c6, "apollo_probabilities_c6.rds")
res_c6 <- readRDS("res_c6.rds")

# res_c7 <- run_cluster("c7", paste0(google_dir,"Data/Anonymised data/part_model_c7.rds"),
#   alts=c("laa_cmck", "mna_cmck", "laa_jmck", "mna_jmck", "laa_msqd", "mra_msqd", "sba_msqd", "sfa_msqd", "mna_msqd", "laa_psdn", "mna_psdn", "mna_nanc", "sba_nanc", "sda_nanc", "no_participation"),
#   apollo_beta_base=apollo_beta_c7, apollo_fixed_base=apollo_fixed_c7,
#   apollo_prob_fun=apollo_probabilities_c7,
#   case_vars=c("weekend","psdnclosure","msqdclosure"),
#   use_d_c_by_spec=use_d_c_by_spec)
# saveRDS(res_c7, "res_c7.rds")
# saveRDS(apollo_beta_c7, "apollo_beta_c7.rds")
# saveRDS(apollo_fixed_c7, "apollo_fixed_c7.rds")
# saveRDS(apollo_probabilities_c7, "apollo_probabilities_c7.rds")
res_c7 <- readRDS("res_c7.rds")

comp_all <- bind_rows(res_c4$comp, res_c5$comp, res_c6$comp, res_c7$comp) %>% arrange(cluster, AIC)
# print(comp_all)
# write.csv(comp_all, file=file.path("R","output","LL_AIC_BIC_allclusters.csv"), row.names=FALSE)
# writexl::write_xlsx(comp_all, path=file.path("R","output","LL_AIC_BIC_allclusters.xlsx"))



#### OUT OF SAMPLE METRIC ####
apollo_probabilities_null_c4 <- readRDS("apollo_probabilities_null_c4.rds")
apollo_probabilities_null_c5 <- readRDS("apollo_probabilities_null_c5.rds")
apollo_probabilities_null_c6 <- readRDS("apollo_probabilities_null_c6.rds")
apollo_probabilities_null_c7 <- readRDS("apollo_probabilities_null_c7.rds")
apollo_beta_null_c4          <- readRDS("apollo_beta_null_c4.rds")
apollo_beta_null_c5          <- readRDS("apollo_beta_null_c5.rds")
apollo_beta_null_c6          <- readRDS("apollo_beta_null_c6.rds")
apollo_beta_null_c7          <- readRDS("apollo_beta_null_c7.rds")
apollo_fixed_null_c4         <- readRDS("apollo_fixed_null_c4.rds")
apollo_fixed_null_c5         <- readRDS("apollo_fixed_null_c5.rds")
apollo_fixed_null_c6         <- readRDS("apollo_fixed_null_c6.rds")
apollo_fixed_null_c7         <- readRDS("apollo_fixed_null_c7.rds")


apollo_fixed_c4 <- c("asc_no_participation")
apollo_fixed_c5 <- c("asc_no_participation")
apollo_fixed_c6 <- c("asc_no_participation")
apollo_fixed_c7 <- c("asc_no_participation")


# #### OUT OF SAMPLE METRIC (FULL vs NULL-ASC trained on TRAIN) ####
# - Estimates FULL model on TRAIN, evaluates LL on TEST
# - Estimates NULL model (ASC-only, same NL structure) on TRAIN, evaluates LL on TEST
# - Computes McFadden-style OOS pseudo-R2: 1 - LL_test_full / LL_test_null
# - Uses functionality="prediction" (no panelProd) if your prob-funs are coded that way;
#   falls back to "validate" if prediction fails (symmetrically for full and null).

oos_compare_cluster <- function(res_cluster,
                                apollo_beta_base, apollo_fixed_base, apollo_prob_fun,
                                apollo_beta_null, apollo_fixed_null, apollo_prob_fun_null,
                                cut_date = as.Date("2017-01-01"),
                                specs = c("daily","MA7","MA14","MA30","t1daily"),
                                nCores = 16,
                                cl = "cX",
                                date_var = "set_date",
                                indiv_id = NULL,   # e.g., "fished_vessel_anon" for per-individual LL
                                eps = 1e-300,
                                verbose = TRUE){
  
  # ---------- helpers ----------
  safe_date <- function(x){
    if(inherits(x, "Date")) return(x)
    as.Date(x)
  }
  
  extract_probvec <- function(P_obj){
    # returns numeric vector of probs of chosen alternative
    if(is.null(P_obj)) stop("P_obj is NULL")
    
    if(!is.null(P_obj$model)){
      m <- P_obj$model
      
      # A) list with chosen
      if(is.list(m) && !is.null(names(m)) && "chosen" %in% names(m)){
        return(as.numeric(m$chosen))
      }
      
      # B) numeric vector
      if(is.numeric(m) && is.atomic(m)){
        return(as.numeric(m))
      }
      
      # C) list -> unlist
      if(is.list(m)){
        v <- unlist(m, use.names = FALSE)
        return(as.numeric(v))
      }
    }
    
    # fallback
    v <- unlist(P_obj, use.names = FALSE)
    as.numeric(v)
  }
  
  drop_parameter <- function(beta, fixed, par){
    if(par %in% names(beta)) beta <- beta[names(beta) != par]
    if(par %in% fixed) fixed <- setdiff(fixed, par)
    list(beta = beta, fixed = fixed)
  }
  
  run_probs <- function(prob_fun, beta_hat, inputs, what = c("FULL","NULL")){
    what <- match.arg(what)
    P <- try(prob_fun(beta_hat, inputs, functionality = "prediction"), silent = TRUE)
    if(inherits(P, "try-error")){
      if(verbose) cat("  ->", what, "prediction falló, usando validate.\n")
      P <- prob_fun(beta_hat, inputs, functionality = "validate")
    }
    P
  }
  
  # ---------- main ----------
  out <- list()
  
  for(sp in specs){
    
    if(verbose){
      cat("\n==============================\n")
      cat("Running OOS for cluster:", cl, "\n")
      cat("Running OOS for spec:", sp, "\n")
      cat("==============================\n")
    }
    
    # Check spec exists
    if(is.null(res_cluster$models[[sp]])) {
      if(verbose) cat("  -> spec no existe en res_cluster$models\n")
      next
    }
    db <- res_cluster$models[[sp]]$database_wide
    if(is.null(db)) {
      if(verbose) cat("  -> database_wide es NULL\n")
      next
    }
    
    # Dates + split
    if(!(date_var %in% names(db))) stop(paste0("No existe columna fecha: ", date_var))
    db[[date_var]] <- safe_date(db[[date_var]])
    
    db_train <- db[ db[[date_var]] <  cut_date, , drop = FALSE]
    db_test  <- db[ db[[date_var]] >= cut_date, , drop = FALSE]
    
    if(nrow(db_train) == 0 || nrow(db_test) == 0){
      if(verbose) cat("  -> train o test vacío. Saltando.\n")
      next
    }
    
    # Controls
    apollo_control <- res_cluster$models[[sp]]$apollo_control
    apollo_control$modelName <- paste0(apollo_control$modelName, "_", sp, "_train")
    apollo_control$nCores <- nCores
    
    # Get use_d_c if present
    use_d_c_val <- 0
    if(!is.null(res_cluster$models[[sp]]$apollo_inputs$use_d_c)){
      use_d_c_val <- res_cluster$models[[sp]]$apollo_inputs$use_d_c
      use_d_c_val <- as.numeric(!is.na(use_d_c_val) && use_d_c_val > 0)
    }
    
    # ---------- FULL: estimate on TRAIN ----------
    assign("database", db_train, envir = .GlobalEnv)
    assign("apollo_control", apollo_control, envir = .GlobalEnv)
    
    apollo_beta_full  <- apollo_beta_base
    apollo_fixed_full <- apollo_fixed_base
    
    if(use_d_c_val == 0){
      tmp <- drop_parameter(apollo_beta_full, apollo_fixed_full, "B_d_c")
      apollo_beta_full  <- tmp$beta
      apollo_fixed_full <- tmp$fixed
    }
    
    assign("apollo_beta",  apollo_beta_full,  envir = .GlobalEnv)
    assign("apollo_fixed", apollo_fixed_full, envir = .GlobalEnv)
    
    apollo_inputs_full <- apollo_validateInputs()
    apollo_inputs_full$use_d_c <- use_d_c_val
    
    m_full_train <- apollo_estimate(apollo_beta_full, apollo_fixed_full,
                                    apollo_prob_fun, apollo_inputs_full,
                                    estimate_settings = list(
                                      estimationRoutine = "bfgs"
                                    ))
    beta_full_hat <- m_full_train$estimate
    
    # ---------- NULL: estimate on TRAIN ----------
    assign("database", db_train, envir = .GlobalEnv)
    assign("apollo_control", apollo_control, envir = .GlobalEnv)
    
    apollo_beta_n  <- apollo_beta_null
    apollo_fixed_n <- apollo_fixed_null
    
    if(use_d_c_val == 0){
      tmp <- drop_parameter(apollo_beta_n, apollo_fixed_n, "B_d_c")
      apollo_beta_n  <- tmp$beta
      apollo_fixed_n <- tmp$fixed
    }
    
    assign("apollo_beta",  apollo_beta_n,  envir = .GlobalEnv)
    assign("apollo_fixed", apollo_fixed_n, envir = .GlobalEnv)
    
    apollo_inputs_null <- apollo_validateInputs()
    apollo_inputs_null$use_d_c <- use_d_c_val
    
    m_null_train <- apollo_estimate(apollo_beta_n, apollo_fixed_n,
                                    apollo_prob_fun_null, apollo_inputs_null,
                                    estimate_settings = list(
                                      estimationRoutine = "bfgs"
                                    ))
    beta_null_hat <- m_null_train$estimate
    
    # ---------- Evaluate on TEST (FULL + NULL) ----------
    apollo_control_test <- apollo_control
    apollo_control_test$modelName <- paste0(apollo_control$modelName, "_", sp, "_test")
    
    assign("database", db_test, envir = .GlobalEnv)
    assign("apollo_control", apollo_control_test, envir = .GlobalEnv)
    
    apollo_inputs_test <- apollo_validateInputs()
    apollo_inputs_test$use_d_c <- use_d_c_val
    
    # FULL probs + LL
    P_test_full <- run_probs(apollo_prob_fun, beta_full_hat, apollo_inputs_test, what = "FULL")
    p_full <- pmax(extract_probvec(P_test_full), eps)
    LL_test_full <- sum(log(p_full))
    n_elem <- length(p_full)
    
    # NULL probs + LL
    P_test_null <- run_probs(apollo_prob_fun_null, beta_null_hat, apollo_inputs_test, what = "NULL")
    p_null <- pmax(extract_probvec(P_test_null), eps)
    LL_test_null <- sum(log(p_null))
    
    # OOS pseudo-R2 (McFadden-style)
    pseudoR2_oos <- 1 - (LL_test_full / LL_test_null)
    
    # Optional per-individual averaging (only if p is per-observation)
    LL_full_perIndiv <- NA_real_
    nIndiv_test <- NA_integer_
    
    if(!is.null(indiv_id) && (indiv_id %in% names(db_test)) && length(p_full) == nrow(db_test)){
      ids <- db_test[[indiv_id]]
      ll_by_obs <- log(pmax(p_full, eps))
      ll_by_ind <- tapply(ll_by_obs, ids, sum)
      LL_full_perIndiv <- mean(ll_by_ind)
      nIndiv_test <- length(ll_by_ind)
    }
    
    row <- data.frame(
      cluster = cl,
      spec = sp,
      nTrain = nrow(db_train),
      nTest  = nrow(db_test),
      nProb  = n_elem,
      LL_test_full = LL_test_full,
      LL_test_null = LL_test_null,
      pseudoR2_oos = pseudoR2_oos,
      LL_full_perElem = LL_test_full / n_elem,
      LL_full_perIndiv = LL_full_perIndiv,
      nIndiv_test = nIndiv_test
    )
    
    out[[sp]] <- row
  }
  
  # Bind + order
  out_df <- do.call(rbind, out)
  out_df <- out_df[order(out_df$LL_full_perElem, decreasing = TRUE), ]
  rownames(out_df) <- NULL
  out_df
}

oos_results <- list()

for(k in 4:7){
  cl <- paste0("c", k)
  
  res_cluster       <- get(paste0("res_", cl))
  apollo_beta_base  <- get(paste0("apollo_beta_", cl))
  apollo_fixed_base <- get(paste0("apollo_fixed_", cl))
  apollo_prob_fun   <- get(paste0("apollo_probabilities_", cl))
  
  apollo_beta_null  <- get(paste0("apollo_beta_null_", cl))
  apollo_fixed_null <- get(paste0("apollo_fixed_null_", cl))
  apollo_prob_fun_null <- get(paste0("apollo_probabilities_null_", cl))
  
  oos_results[[cl]] <- oos_compare_cluster(
    res_cluster = res_cluster,
    apollo_beta_base = apollo_beta_base,
    apollo_fixed_base = apollo_fixed_base,
    apollo_prob_fun = apollo_prob_fun,
    apollo_beta_null = apollo_beta_null,
    apollo_fixed_null = apollo_fixed_null,
    apollo_prob_fun_null = apollo_prob_fun_null,
    cut_date = as.Date("2017-01-01"),
    specs = c("daily","MA7","MA14","MA30","t1daily"),
    nCores = 16,
    cl = cl,
    date_var = "set_date",
    indiv_id = "fished_vessel_anon",
    verbose = TRUE
  )
}



## Tabla

library(dplyr)


oos_table <- bind_rows(
  oos_results$c4,
  oos_results$c5,
  oos_results$c6,
  oos_results$c7
)

writexl::write_xlsx(oos_table, path=file.path("R","output","OOS_results_sdm_variation.xlsx"))
saveRDS(oos_table, "oos_table.RDS")




