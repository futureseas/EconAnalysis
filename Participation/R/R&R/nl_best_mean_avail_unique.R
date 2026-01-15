# ==========================================================
# REQUIRED OBJECTS (must exist in your session)
# ==========================================================
# You already have these in your current pipeline/script:

gc()

library(apollo)
apollo_initialise()

setwd("D:/GitHub/EconAnalysis/Participation")
res_c4 <- readRDS("res_c4.rds") 
res_c5 <- readRDS("res_c5.rds") 
res_c6 <- readRDS("res_c6.rds") 
res_c7 <- readRDS("res_c7.rds") 


############################################################
# SIMPLE EXTRACT + RUN + COMPARE
# 1) Pick best SDM spec per cluster (from oos_table.RDS)
# 2) Extract database_wide per cluster/spec from res_c4..res_c7
# 3) Save databases to disk (RDS)
# 4) Re-estimate your manual model per cluster (you fill beta/fixed/prob)
# 5) Compare LL and k vs original model in res_c*
############################################################

library(dplyr)

# ----------------------------
# (A) Pick best SDM spec per cluster
# ----------------------------
oos_table <- readRDS("oos_table.RDS")

best_spec <- oos_table %>%
  group_by(cluster) %>%
  slice_max(order_by = pseudoR2_oos, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cluster, spec, pseudoR2_oos, LL_test_full) 

print(best_spec)

# ----------------------------
# (B) Collect res objects (must exist in memory)
# ----------------------------
res_list <- list(
  c4 = res_c4,
  c5 = res_c5,
  c6 = res_c6,
  c7 = res_c7
)

# ----------------------------
# (C) Extract databases and original models
# ----------------------------
extract_cluster_assets <- function(res_obj, spec){
  slot <- res_obj$models[[spec]]
  if(is.null(slot)) stop(paste0("Spec '", spec, "' not found in res_obj$models"))
  
  db <- slot$database_wide
  if(is.null(db)) stop(paste0("database_wide NULL for spec '", spec, "'"))
  
  # original estimated model (if stored)
  m0 <- slot$model
  if(is.null(m0)) warning(paste0("No slot$model for spec '", spec, "' (will skip compare)."))
  
  list(database_wide = db, model_original = m0)
}

assets <- list()

for(cl in names(res_list)){
  spec_cl <- best_spec %>% filter(cluster == cl) %>% pull(spec)
  if(length(spec_cl)==0) stop(paste("No best spec for", cl))
  
  cat("\n--- Extracting", cl, "spec:", spec_cl, "---\n")
  assets[[cl]] <- extract_cluster_assets(res_list[[cl]], spec_cl)
  assets[[cl]]$spec <- spec_cl
}

# ----------------------------
# (D) Save databases to disk (one file per cluster) + a combined list
# ----------------------------
dir.create(file.path("R","output","databases_bestSDM"), showWarnings = FALSE, recursive = TRUE)

for(cl in names(assets)){
  saveRDS(
    assets[[cl]]$database_wide,
    file = file.path("R","output","databases_bestSDM", paste0("database_", cl, "_", assets[[cl]]$spec, ".rds"))
  )
}

saveRDS(assets, file = file.path("R","output","assets_bestSDM_byCluster.rds"))

cat("\nSaved:\n")
for(cl in names(assets)){
  cat(" - ", file.path("R","output","databases_bestSDM", paste0("database_", cl, "_", assets[[cl]]$spec, ".rds")), "\n")
}
cat(" - ", file.path("R","output","assets_bestSDM_byCluster.rds"), "\n")



############################################################
# (E) TEMPLATE: re-estimate your manual model and compare
#     -> YOU fill these objects per cluster:
#        apollo_beta_MANUAL, apollo_fixed_MANUAL, apollo_probabilities_MANUAL
############################################################

run_manual_and_compare <- function(cluster_id, database_wide, model_original,
                                   apollo_beta_MANUAL, apollo_fixed_MANUAL, apollo_probabilities_MANUAL, constraints_MANUAL, indivID="fished_vessel_anon", nCores=16,
                                   outDir=file.path("R","output")){
  
  apollo_control <- list(
    modelName       = paste0("MANUAL_", cluster_id, "_bestSDM"),
    modelDescr      = "Manual re-estimation on extracted database_wide",
    indivID         = indivID,
    outputDirectory = outDir,
    panelData       = TRUE,
    nCores          = nCores,
    workInLogs      = TRUE
  )
  
  # set globals as Apollo expects
  assign("database", database_wide, envir=.GlobalEnv)
  assign("apollo_control", apollo_control, envir=.GlobalEnv)
  assign("apollo_beta", apollo_beta_MANUAL, envir=.GlobalEnv)
  assign("apollo_fixed", apollo_fixed_MANUAL, envir=.GlobalEnv)
  
  apollo_inputs <- apollo_validateInputs()
  
  m1 <- apollo_estimate(
    apollo_beta_MANUAL, apollo_fixed_MANUAL,
    apollo_probabilities_MANUAL, apollo_inputs, 
    estimate_settings=list(constraints= constraints_MANUAL,
                           estimationRoutine = "bfgs") #bfgs
  )
  
  apollo_saveOutput(m1, saveOutput_settings = list(printT1 = 1))
  
  # Compare to original (if exists)
  comp <- data.frame(
    cluster = cluster_id,
    spec    = NA_character_,
    model   = c("original", "manual"),
    LL      = c(if(!is.null(model_original)) model_original$maximum else NA_real_,
                m1$maximum),
    k       = c(if(!is.null(model_original)) length(model_original$estimate) else NA_real_,
                length(m1$estimate))
  )
  
  list(model_manual = m1, compare = comp)
}


# ----------------------------
# LR test
# ----------------------------

lr_test <- function(model_restricted, model_unrestricted){
  if(is.null(model_restricted) || is.null(model_unrestricted))
    stop("One of the models is NULL.")
  
  LLr <- model_restricted$maximum
  LLu <- model_unrestricted$maximum
  
  kr <- length(model_restricted$estimate)
  ku <- length(model_unrestricted$estimate)
  
  LR  <- 2 * (LLu - LLr)
  df  <- ku - kr
  p   <- pchisq(LR, df = df, lower.tail = FALSE)
  
  data.frame(
    LR = LR,
    df = df,
    p_value = p,
    LL_restricted = LLr,
    LL_unrestricted = LLu,
    k_restricted = kr,
    k_unrestricted = ku
  )
}


# RUN MODELS



# ===================== Cluster c4 =====================

db_c4 <- assets$c4$database_wide
m0_c4 <- assets$c4$model_original
apollo_beta_c4 <- c(
  asc_sfa_nanc                    = -5.490086,
  asc_laa_nanc                    = -5.663332,
  asc_laa_cmck                    = -5.262887,
  asc_laa_msqd                    = -3.423177,
  asc_laa_ytna                    = -6.436951,
  asc_mna_msqd                    = -3.828992,
  asc_sba_msqd                    = -3.379281,
  asc_laa_btna                    = -6.998314,
  asc_sfa_msqd                    = -3.816320,
  asc_mna_psdn                    = -5.568935,
  asc_sba_cmck                    = -5.537029,
  asc_mra_msqd                    = -4.500112,
  asc_laa_psdn                    = -5.608794,
  asc_mna_nanc                    = -4.829045,
  c_psdn                          = -1.333250,
  w_msqd                          = -3.628054,
  theta_part                      =  3.754031,
  theta_cmck                      = 11.999788,
  theta_msqd                      =  1.376861,
  theta_psdn                      =  0.340393,
  theta_nanc                      =  0.045038,
  theta_tuna                      = 12.005228,
  B_mean_avail                    =  1.900983,
  B_mean_price                    =  0.455444,
  B_wind_max_220_mh               = -0.049756,
  B_d_d                           = -2.261145,
  B_d_cd                          = -2.563811,
  B_d_c                           = -0.958290,
  B_dist_port_to_catch_area_zero  = -0.010961,
  B_dummy_prev_days               =  2.654242,
  B_dummy_prev_year_days          =  0.243357,
  B_unem_rate_part                =  0.205768,
  asc_no_participation            =  0.000000,
  B_dist_to_cog                   = -0.002618)


apollo_fixed_c4 <- c("asc_no_participation", "theta_cmck", "theta_tuna")

apollo_probabilities_c4 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  lambda_part <- 1/(1 + exp(-theta_part))
  lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
  lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
  lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))
  P <- list(); V <- list()
  V[["sfa_nanc"]] <- asc_sfa_nanc + B_mean_avail*mean_avail_sfa_nanc + B_mean_price*mean_price_sfa_nanc + B_wind_max_220_mh*wind_max_220_mh_sfa_nanc + B_d_d*d_d_sfa_nanc + B_d_cd*d_cd_sfa_nanc + B_d_c*d_c_sfa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days*dummy_prev_days_sfa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_sfa_nanc + B_unem_rate_part*unem_rate_sfa_nanc + B_dist_to_cog*dist_to_cog_sfa_nanc                                        
  V[["laa_nanc"]] <- asc_laa_nanc + B_mean_avail*mean_avail_laa_nanc + B_mean_price*mean_price_laa_nanc + B_wind_max_220_mh*wind_max_220_mh_laa_nanc + B_d_d*d_d_laa_nanc + B_d_cd*d_cd_laa_nanc + B_d_c*d_c_laa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days*dummy_prev_days_laa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_laa_nanc + B_unem_rate_part*unem_rate_laa_nanc + B_dist_to_cog*dist_to_cog_laa_nanc                                        
  V[["laa_cmck"]] <- asc_laa_cmck + B_mean_avail*mean_avail_laa_cmck + B_mean_price*mean_price_laa_cmck + B_wind_max_220_mh*wind_max_220_mh_laa_cmck + B_d_d*d_d_laa_cmck + B_d_cd*d_cd_laa_cmck + B_d_c*d_c_laa_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days*dummy_prev_days_laa_cmck + B_dummy_prev_year_days*dummy_prev_year_days_laa_cmck + B_unem_rate_part*unem_rate_laa_cmck + B_dist_to_cog*dist_to_cog_laa_cmck                                        
  V[["laa_msqd"]] <- asc_laa_msqd + B_mean_avail*mean_avail_laa_msqd + B_mean_price*mean_price_laa_msqd + B_wind_max_220_mh*wind_max_220_mh_laa_msqd + B_d_d*d_d_laa_msqd + B_d_cd*d_cd_laa_msqd + B_d_c*d_c_laa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days*dummy_prev_days_laa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_laa_msqd + B_unem_rate_part*unem_rate_laa_msqd + B_dist_to_cog*dist_to_cog_laa_msqd + w_msqd * weekend                        
  V[["laa_ytna"]] <- asc_laa_ytna + B_mean_avail*mean_avail_laa_ytna + B_mean_price*mean_price_laa_ytna + B_wind_max_220_mh*wind_max_220_mh_laa_ytna + B_d_d*d_d_laa_ytna + B_d_cd*d_cd_laa_ytna + B_d_c*d_c_laa_ytna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days*dummy_prev_days_laa_ytna + B_dummy_prev_year_days*dummy_prev_year_days_laa_ytna + B_unem_rate_part*unem_rate_laa_ytna + B_dist_to_cog*dist_to_cog_laa_ytna                                         
  V[["mna_msqd"]] <- asc_mna_msqd + B_mean_avail*mean_avail_mna_msqd + B_mean_price*mean_price_mna_msqd + B_wind_max_220_mh*wind_max_220_mh_mna_msqd + B_d_d*d_d_mna_msqd + B_d_cd*d_cd_mna_msqd + B_d_c*d_c_mna_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days*dummy_prev_days_mna_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mna_msqd + B_unem_rate_part*unem_rate_mna_msqd + B_dist_to_cog*dist_to_cog_mna_msqd + w_msqd * weekend                        
  V[["sba_msqd"]] <- asc_sba_msqd + B_mean_avail*mean_avail_sba_msqd + B_mean_price*mean_price_sba_msqd + B_wind_max_220_mh*wind_max_220_mh_sba_msqd + B_d_d*d_d_sba_msqd + B_d_cd*d_cd_sba_msqd + B_d_c*d_c_sba_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days*dummy_prev_days_sba_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sba_msqd + B_unem_rate_part*unem_rate_sba_msqd + B_dist_to_cog*dist_to_cog_sba_msqd + w_msqd * weekend                        
  V[["laa_btna"]] <- asc_laa_btna + B_mean_avail*mean_avail_laa_btna + B_mean_price*mean_price_laa_btna + B_wind_max_220_mh*wind_max_220_mh_laa_btna + B_d_d*d_d_laa_btna + B_d_cd*d_cd_laa_btna + B_d_c*d_c_laa_btna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days*dummy_prev_days_laa_btna + B_dummy_prev_year_days*dummy_prev_year_days_laa_btna + B_unem_rate_part*unem_rate_laa_btna + B_dist_to_cog*dist_to_cog_laa_btna                          
  V[["sfa_msqd"]] <- asc_sfa_msqd + B_mean_avail*mean_avail_sfa_msqd + B_mean_price*mean_price_sfa_msqd + B_wind_max_220_mh*wind_max_220_mh_sfa_msqd + B_d_d*d_d_sfa_msqd + B_d_cd*d_cd_sfa_msqd + B_d_c*d_c_sfa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days*dummy_prev_days_sfa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sfa_msqd + B_unem_rate_part*unem_rate_sfa_msqd + B_dist_to_cog*dist_to_cog_sfa_msqd + w_msqd * weekend                        
  V[["mna_psdn"]] <- asc_mna_psdn + B_mean_avail*mean_avail_mna_psdn + B_mean_price*mean_price_mna_psdn + B_wind_max_220_mh*wind_max_220_mh_mna_psdn + B_d_d*d_d_mna_psdn + B_d_cd*d_cd_mna_psdn + B_d_c*d_c_mna_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days*dummy_prev_days_mna_psdn + B_dummy_prev_year_days*dummy_prev_year_days_mna_psdn + B_unem_rate_part*unem_rate_mna_psdn + B_dist_to_cog*dist_to_cog_mna_psdn + c_psdn * psdnclosure 
  V[["sba_cmck"]] <- asc_sba_cmck + B_mean_avail*mean_avail_sba_cmck + B_mean_price*mean_price_sba_cmck + B_wind_max_220_mh*wind_max_220_mh_sba_cmck + B_d_d*d_d_sba_cmck + B_d_cd*d_cd_sba_cmck + B_d_c*d_c_sba_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days*dummy_prev_days_sba_cmck + B_dummy_prev_year_days*dummy_prev_year_days_sba_cmck + B_unem_rate_part*unem_rate_sba_cmck + B_dist_to_cog*dist_to_cog_sba_cmck                      
  V[["mra_msqd"]] <- asc_mra_msqd + B_mean_avail*mean_avail_mra_msqd + B_mean_price*mean_price_mra_msqd + B_wind_max_220_mh*wind_max_220_mh_mra_msqd + B_d_d*d_d_mra_msqd + B_d_cd*d_cd_mra_msqd + B_d_c*d_c_mra_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days*dummy_prev_days_mra_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mra_msqd + B_unem_rate_part*unem_rate_mra_msqd + B_dist_to_cog*dist_to_cog_mra_msqd + w_msqd * weekend                        
  V[["laa_psdn"]] <- asc_laa_psdn + B_mean_avail*mean_avail_laa_psdn + B_mean_price*mean_price_laa_psdn + B_wind_max_220_mh*wind_max_220_mh_laa_psdn + B_d_d*d_d_laa_psdn + B_d_cd*d_cd_laa_psdn + B_d_c*d_c_laa_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days*dummy_prev_days_laa_psdn + B_dummy_prev_year_days*dummy_prev_year_days_laa_psdn + B_unem_rate_part*unem_rate_laa_psdn + B_dist_to_cog*dist_to_cog_laa_psdn + c_psdn * psdnclosure 
  V[["mna_nanc"]] <- asc_mna_nanc + B_mean_avail*mean_avail_mna_nanc + B_mean_price*mean_price_mna_nanc + B_wind_max_220_mh*wind_max_220_mh_mna_nanc + B_d_d*d_d_mna_nanc + B_d_cd*d_cd_mna_nanc + B_d_c*d_c_mna_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days*dummy_prev_days_mna_nanc + B_dummy_prev_year_days*dummy_prev_year_days_mna_nanc + B_unem_rate_part*unem_rate_mna_nanc + B_dist_to_cog*dist_to_cog_mna_nanc                      
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

out_c4 <- run_manual_and_compare(
  cluster_id = "c4",
  database_wide = db_c4,
  model_original = m0_c4,
  apollo_beta_MANUAL = apollo_beta_c4,
  apollo_fixed_MANUAL = apollo_fixed_c4,
  apollo_probabilities_MANUAL = apollo_probabilities_c4,
  constraints_MANUAL = NULL
)


# ===================== Cluster c5 (from n_logit_c5.R) =====================

db_c5 <- assets$c5$database_wide
m0_c5 <- assets$c5$model_original
apollo_beta_c5 = c(
  B_mean_avail                   =    1.912821,
  B_wind_max_220_mh              =   -0.064521,
  B_dist_to_cog                  = -8.9812e-04,
  B_dist_port_to_catch_area_zero =   -0.006273,
  B_dummy_prev_days              =    2.917936,
  B_dummy_prev_year_days         =    0.370203,
  B_unem_rate                    =    0.408903,
  B_d_d                          =   -2.028443,
  B_d_c                          =   -0.923232,
  B_d_cd                         =   -9.893429,
  c_psdn                         =   -2.141648,
  c_dcrb                         =   -1.786777,
  B_mean_price_part              =    4.511170,
  B_mean_price_crab              =    0.109650,
  asc_mna_msqd                   =   -8.993064,
  asc_sba_msqd                   =   -8.827100,
  asc_mra_msqd                   =   -9.909028,
  asc_laa_msqd                   =   -9.325850,
  asc_npa_msqd                   =   -9.502723,
  asc_sfa_msqd                   =   -9.438585,
  asc_cba_msqd                   =   -9.256518,
  asc_laa_psdn                   =   -7.975989,
  asc_clo_psdn                   =   -6.032144,
  asc_cwa_psdn                   =   -4.546371,
  asc_clw_psdn                   =   -4.860164,
  asc_sba_cmck                   =   -7.795499,
  asc_laa_cmck                   =   -8.945853,
  asc_laa_nanc                   =   -7.709738,
  asc_cwa_albc                   =  -19.519344,
  asc_cwa_dcrb                   =   -6.280222,
  asc_clw_dcrb                   =   -5.877424,
  theta_part                     =    1.190100,
  theta_cmck                     =   10.763614,
  theta_msqd                     =   11.124655,
  theta_psdn                     =    0.176171,
  theta_part_crab                =   10.696168,
  w_msqd                         =   -3.970433,
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


  ### COnstruir lambda
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_part_crab <- 1 / (1 + exp(-theta_part_crab))
  lambda_cmck <- lambda_part * (1 / (1 + exp(-theta_cmck)))     
  lambda_msqd <- lambda_part * (1 / (1 + exp(-theta_msqd)))    
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_price_part * mean_price_3_mna_msqd + B_mean_avail * mean_avail_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_unem_rate * unem_rate_mna_msqd                                    + B_d_c * d_c_mna_msqd + B_d_d * d_d_mna_msqd + w_msqd * weekend                            
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_price_part * mean_price_3_sba_msqd + B_mean_avail * mean_avail_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_unem_rate * unem_rate_sba_msqd                                    + B_d_c * d_c_sba_msqd + B_d_d * d_d_sba_msqd + w_msqd * weekend                            
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_price_part * mean_price_3_mra_msqd + B_mean_avail * mean_avail_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_unem_rate * unem_rate_mra_msqd                                    + B_d_c * d_c_mra_msqd + B_d_d * d_d_mra_msqd + w_msqd * weekend                            
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_price_part * mean_price_3_laa_msqd + B_mean_avail * mean_avail_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_unem_rate * unem_rate_laa_msqd                                    + B_d_c * d_c_laa_msqd + B_d_d * d_d_laa_msqd + w_msqd * weekend                            
  V[["npa_msqd"]]         = asc_npa_msqd + B_mean_price_part * mean_price_3_npa_msqd + B_mean_avail * mean_avail_npa_msqd + B_wind_max_220_mh * wind_max_220_mh_npa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_npa_msqd + B_dummy_prev_days * dummy_prev_days_npa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_npa_msqd + B_dist_to_cog * dist_to_cog_npa_msqd + B_unem_rate * unem_rate_npa_msqd                                    + B_d_c * d_c_npa_msqd + B_d_d * d_d_npa_msqd     
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_price_part * mean_price_3_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_unem_rate * unem_rate_sfa_msqd                                    + B_d_c * d_c_sfa_msqd + B_d_d * d_d_sfa_msqd + w_msqd * weekend                            
  V[["cba_msqd"]]         = asc_cba_msqd + B_mean_price_part * mean_price_3_cba_msqd + B_mean_avail * mean_avail_cba_msqd + B_wind_max_220_mh * wind_max_220_mh_cba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_msqd + B_dummy_prev_days * dummy_prev_days_cba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_cba_msqd + B_dist_to_cog * dist_to_cog_cba_msqd + B_unem_rate * unem_rate_cba_msqd                                    + B_d_c * d_c_cba_msqd + B_d_d * d_d_cba_msqd                              
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_price_part * mean_price_3_laa_psdn + B_mean_avail * mean_avail_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_unem_rate * unem_rate_laa_psdn + c_psdn * psdnclosure             + B_d_c * d_c_laa_psdn + B_d_d * d_d_laa_psdn     
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn + B_mean_avail * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_unem_rate * unem_rate_clo_psdn + c_psdn * psdnclosure             + B_d_c * d_c_clo_psdn + B_d_d * d_d_clo_psdn                             
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn + B_mean_avail * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_unem_rate * unem_rate_cwa_psdn + c_psdn * psdnclosure             + B_d_c * d_c_cwa_psdn + B_d_d * d_d_cwa_psdn                            
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn + B_mean_avail * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_unem_rate * unem_rate_clw_psdn + c_psdn * psdnclosure             + B_d_c * d_c_clw_psdn + B_d_d * d_d_clw_psdn                            
  V[["sba_cmck"]]         = asc_sba_cmck + B_mean_price_part * mean_price_3_sba_cmck + B_mean_avail * mean_avail_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_dist_to_cog * dist_to_cog_sba_cmck + B_unem_rate * unem_rate_sba_cmck                                    + B_d_c * d_c_sba_cmck + B_d_d * d_d_sba_cmck                             
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_price_part * mean_price_3_laa_cmck + B_mean_avail * mean_avail_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_unem_rate * unem_rate_laa_cmck                                    + B_d_c * d_c_laa_cmck + B_d_d * d_d_laa_cmck                             
  V[["laa_nanc"]]         = asc_laa_nanc + B_mean_price_part * mean_price_3_laa_nanc + B_mean_avail * mean_avail_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_dist_to_cog * dist_to_cog_laa_nanc + B_unem_rate * unem_rate_laa_nanc                                    + B_d_c * d_c_laa_nanc + B_d_d * d_d_laa_nanc          
  V[["cwa_albc"]]         = asc_cwa_albc + B_mean_price_part * mean_price_3_cwa_albc + B_mean_avail * mean_avail_cwa_albc + B_wind_max_220_mh * wind_max_220_mh_cwa_albc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_albc + B_dummy_prev_days * dummy_prev_days_cwa_albc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_albc + B_dist_to_cog * dist_to_cog_cwa_albc + B_unem_rate * unem_rate_cwa_albc                                    + B_d_c * d_c_cwa_albc + B_d_d * d_d_cwa_albc                             
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_price_crab * mean_price_3_cwa_dcrb + B_mean_avail * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_unem_rate * unem_rate_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb + B_d_c * d_c_cwa_dcrb + B_d_d * d_d_cwa_dcrb +  B_d_cd * d_cd_cwa_dcrb                            
  V[["clw_dcrb"]]         = asc_clw_dcrb + B_mean_price_crab * mean_price_3_clw_dcrb + B_mean_avail * mean_avail_clw_dcrb + B_wind_max_220_mh * wind_max_220_mh_clw_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_dcrb + B_dummy_prev_days * dummy_prev_days_clw_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_clw_dcrb + B_dist_to_cog * dist_to_cog_clw_dcrb + B_unem_rate * unem_rate_clw_dcrb + c_dcrb * dcrbclosurewad_clw_dcrb + B_d_c * d_c_clw_dcrb + B_d_d * d_d_clw_dcrb +  B_d_cd * d_cd_clw_dcrb                            
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

out_c5 <- run_manual_and_compare(
  cluster_id = "c5",
  database_wide = db_c5,
  model_original = m0_c5,
  apollo_beta_MANUAL = apollo_beta_c5,
  apollo_fixed_MANUAL = apollo_fixed_c5,
  apollo_probabilities_MANUAL = apollo_probabilities_c5,
  constraints_MANUAL = NULL
)

# ===================== Cluster c6 (from n_logit_c6.R) =====================

db_c6 <- assets$c6$database_wide
m0_c6 <- assets$c6$model_original
apollo_beta_c6=c(
  B_mean_avail                    =  0.868979,
  B_wind_max_220_mh               = -0.050233,
  B_dist_to_cog                   = -0.004442,
  B_dist_port_to_catch_area_zero  = -0.026278,
  B_dummy_prev_days               =  2.443029,
  B_dummy_prev_year_days          =  0.305293,
  B_unem_rate                     =  0.245480,
  B_d_c                           =  0.000000,
  B_d_d                           = -1.281591,
  B_d_cd                          = -8.182224,
  c_dcrb                          = -0.034778,
  B_mean_price_part               =  3.295303,
  B_mean_price_othr               =  0.095945,
  asc_cba_psdn                    = -2.875853,
  asc_clo_psdn                    = -3.589806,
  asc_clw_psdn                    = -3.596577,
  asc_cwa_psdn                    = -3.394706,
  asc_clo_nanc                    = -4.723649,
  asc_clw_nanc                    = -7.308895,
  asc_cwa_nanc                    = -3.935689,
  asc_clo_cmck                    = -4.445714,
  asc_cwa_dcrb                    = -5.306234,
  asc_nps_sock                    = -4.507728,
  theta_part                      =  0.280808,
  theta_nanc                      = 13.345486,
  theta_psdn                      =  1.209376,
  asc_no_participation            =  0.000000)

apollo_fixed_c6 <- c("asc_no_participation", "B_d_c", "B_mean_price_othr",
                     "theta_part", "theta_nanc", "theta_psdn", "B_d_d", "B_d_cd", "c_dcrb",
                     "asc_cba_psdn", "asc_clo_psdn", "asc_clw_psdn", "asc_cwa_psdn",
                     "asc_clo_nanc", "asc_clw_nanc", "asc_cwa_nanc", "asc_clo_cmck",
                     "asc_cwa_dcrb", "asc_nps_sock", "B_unem_rate")
                   
apollo_probabilities_c6 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  use_d_c <- if(!is.null(apollo_inputs$use_d_c)) apollo_inputs$use_d_c else 0
  
  if(use_d_c == 0){
    B_d_c <- 0
  }
  
  
  ### Construir lambda
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_nanc <- lambda_part * (1 / (1 + exp(-theta_nanc)))
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))

  
  ### Create list of probabilities P
  P = list()

    # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["cba_psdn"]]         = asc_cba_psdn + B_mean_avail * mean_avail_cba_psdn + B_wind_max_220_mh * wind_max_220_mh_cba_psdn + B_dist_to_cog * dist_to_cog_cba_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_psdn + B_dummy_prev_days * dummy_prev_days_cba_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cba_psdn  + B_unem_rate * unem_rate_cba_psdn + B_d_c * d_c_cba_psdn + B_d_d * d_d_cba_psdn + B_mean_price_part * mean_price_3_cba_psdn
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_avail * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn  + B_unem_rate * unem_rate_clo_psdn + B_d_c * d_c_clo_psdn + B_d_d * d_d_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_avail * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn  + B_unem_rate * unem_rate_clw_psdn + B_d_c * d_c_clw_psdn + B_d_d * d_d_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn 
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_avail * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn  + B_unem_rate * unem_rate_cwa_psdn + B_d_c * d_c_cwa_psdn + B_d_d * d_d_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn
  V[["clo_nanc"]]         = asc_clo_nanc + B_mean_avail * mean_avail_clo_nanc + B_wind_max_220_mh * wind_max_220_mh_clo_nanc + B_dist_to_cog * dist_to_cog_clo_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_nanc + B_dummy_prev_days * dummy_prev_days_clo_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clo_nanc  + B_unem_rate * unem_rate_clo_nanc + B_d_c * d_c_clo_nanc + B_d_d * d_d_clo_nanc + B_mean_price_part * mean_price_3_clo_nanc
  V[["clw_nanc"]]         = asc_clw_nanc + B_mean_avail * mean_avail_clw_nanc + B_wind_max_220_mh * wind_max_220_mh_clw_nanc + B_dist_to_cog * dist_to_cog_clw_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_nanc + B_dummy_prev_days * dummy_prev_days_clw_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clw_nanc  + B_unem_rate * unem_rate_clw_nanc + B_d_c * d_c_clw_nanc + B_d_d * d_d_clw_nanc + B_mean_price_part * mean_price_3_clw_nanc
  V[["cwa_nanc"]]         = asc_cwa_nanc + B_mean_avail * mean_avail_cwa_nanc + B_wind_max_220_mh * wind_max_220_mh_cwa_nanc + B_dist_to_cog * dist_to_cog_cwa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_nanc + B_dummy_prev_days * dummy_prev_days_cwa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_nanc  + B_unem_rate * unem_rate_cwa_nanc + B_d_c * d_c_cwa_nanc + B_d_d * d_d_cwa_nanc + B_mean_price_part * mean_price_3_cwa_nanc
  V[["clo_cmck"]]         = asc_clo_cmck + B_mean_avail * mean_avail_clo_cmck + B_wind_max_220_mh * wind_max_220_mh_clo_cmck + B_dist_to_cog * dist_to_cog_clo_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_cmck + B_dummy_prev_days * dummy_prev_days_clo_cmck + B_dummy_prev_year_days * dummy_prev_year_days_clo_cmck  + B_unem_rate * unem_rate_clo_cmck + B_d_c * d_c_clo_cmck + B_d_d * d_d_clo_cmck + B_mean_price_part * mean_price_3_clo_cmck 
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_avail * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb  + B_unem_rate * unem_rate_cwa_dcrb + B_d_c * d_c_cwa_dcrb + B_d_d * d_d_cwa_dcrb + B_mean_price_othr * mean_price_3_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb
  V[["nps_sock"]]         = asc_nps_sock + B_mean_avail * mean_avail_nps_sock + B_wind_max_220_mh * wind_max_220_mh_nps_sock + B_dist_to_cog * dist_to_cog_nps_sock + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_nps_sock + B_dummy_prev_days * dummy_prev_days_nps_sock + B_dummy_prev_year_days * dummy_prev_year_days_nps_sock  + B_unem_rate * unem_rate_nps_sock + B_d_c * d_c_nps_sock + B_d_d * d_d_nps_sock + B_mean_price_othr * mean_price_3_nps_sock + B_d_cd * d_cd_nps_sock                        
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

out_c6 <- run_manual_and_compare(
  cluster_id = "c6",
  database_wide = db_c6,
  model_original = m0_c6,
  apollo_beta_MANUAL = apollo_beta_c6,
  apollo_fixed_MANUAL = apollo_fixed_c6,
  apollo_probabilities_MANUAL = apollo_probabilities_c6,
  constraints_MANUAL = NULL)

# ===================== Cluster c7 (from n_logit_c7.R) =====================
db_c7 <- assets$c7$database_wide
m0_c7 <- assets$c7$model_original
apollo_beta_c7 = c(
  B_mean_avail                    = -0.127274,
  B_mean_price                    =  1.252025,
  B_wind_max_220_mh               = -0.039084,
  B_dist_to_cog                   = -0.003062,
  B_dist_port_to_catch_area_zero  = -0.006806,
  B_dummy_prev_days               =  3.018402,
  B_dummy_prev_year_days          =  0.112117,
  c_psdn                          = -1.399252,
  B_d_d                           = -0.486334,
  B_unem_rate                     =  0.118870,
  w_msqd                          = -3.763700,
  asc_laa_cmck                    = -4.536602,
  asc_mna_cmck                    = -6.265150,
  asc_laa_jmck                    = -4.775370,
  asc_mna_jmck                    = -6.287331,
  asc_laa_msqd                    = -4.204071,
  asc_mra_msqd                    = -4.929667,
  asc_sba_msqd                    = -4.071670,
  asc_sfa_msqd                    = -4.186556,
  asc_mna_msqd                    = -3.881184,
  asc_laa_psdn                    = -4.273978,
  asc_mna_psdn                    = -5.045827,
  asc_mna_nanc                    = -5.330748,
  asc_sba_nanc                    = -4.106263,
  asc_sda_nanc                    = -4.991050,
  theta_part                      =  0.944038,
  theta_laa                       = 10.094492,
  theta_mna                       = 10.038215,
  theta_sba                       =  0.136293,
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
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck  + B_unem_rate * unem_rate_laa_cmck + B_d_d * d_d_laa_cmck
  V[["mna_cmck"]]         = asc_mna_cmck + B_mean_avail * mean_avail_mna_cmck + B_mean_price * mean_price_mna_cmck + B_wind_max_220_mh * wind_max_220_mh_mna_cmck + B_dist_to_cog * dist_to_cog_mna_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_cmck + B_dummy_prev_days * dummy_prev_days_mna_cmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_cmck  + B_unem_rate * unem_rate_mna_cmck + B_d_d * d_d_mna_cmck
  V[["laa_jmck"]]         = asc_laa_jmck + B_mean_avail * mean_avail_laa_jmck + B_mean_price * mean_price_laa_jmck + B_wind_max_220_mh * wind_max_220_mh_laa_jmck + B_dist_to_cog * dist_to_cog_laa_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_jmck + B_dummy_prev_days * dummy_prev_days_laa_jmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_jmck  + B_unem_rate * unem_rate_laa_jmck + B_d_d * d_d_laa_jmck
  V[["mna_jmck"]]         = asc_mna_jmck + B_mean_avail * mean_avail_mna_jmck + B_mean_price * mean_price_mna_jmck + B_wind_max_220_mh * wind_max_220_mh_mna_jmck + B_dist_to_cog * dist_to_cog_mna_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_jmck + B_dummy_prev_days * dummy_prev_days_mna_jmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_jmck  + B_unem_rate * unem_rate_mna_jmck + B_d_d * d_d_mna_jmck
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd  + B_unem_rate * unem_rate_laa_msqd + B_d_d * d_d_laa_msqd + w_msqd * weekend 
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd  + B_unem_rate * unem_rate_mra_msqd + B_d_d * d_d_mra_msqd + w_msqd * weekend 
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd  + B_unem_rate * unem_rate_sba_msqd + B_d_d * d_d_sba_msqd + w_msqd * weekend 
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd  + B_unem_rate * unem_rate_sfa_msqd + B_d_d * d_d_sfa_msqd + w_msqd * weekend 
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd  + B_unem_rate * unem_rate_mna_msqd + B_d_d * d_d_mna_msqd + w_msqd * weekend 
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn  + B_unem_rate * unem_rate_laa_psdn + B_d_d * d_d_laa_psdn + c_psdn * psdnclosure                            
  V[["mna_psdn"]]         = asc_mna_psdn + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_dist_to_cog * dist_to_cog_mna_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn  + B_unem_rate * unem_rate_mna_psdn + B_d_d * d_d_mna_psdn + c_psdn * psdnclosure
  V[["mna_nanc"]]         = asc_mna_nanc + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_dist_to_cog * dist_to_cog_mna_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc  + B_unem_rate * unem_rate_mna_nanc + B_d_d * d_d_mna_nanc
  V[["sba_nanc"]]         = asc_sba_nanc + B_mean_avail * mean_avail_sba_nanc + B_mean_price * mean_price_sba_nanc + B_wind_max_220_mh * wind_max_220_mh_sba_nanc + B_dist_to_cog * dist_to_cog_sba_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_nanc + B_dummy_prev_days * dummy_prev_days_sba_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sba_nanc  + B_unem_rate * unem_rate_sba_nanc + B_d_d * d_d_sba_nanc
  V[["sda_nanc"]]         = asc_sda_nanc + B_mean_avail * mean_avail_sda_nanc + B_mean_price * mean_price_sda_nanc + B_wind_max_220_mh * wind_max_220_mh_sda_nanc + B_dist_to_cog * dist_to_cog_sda_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sda_nanc + B_dummy_prev_days * dummy_prev_days_sda_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sda_nanc  + B_unem_rate * unem_rate_sda_nanc + B_d_d * d_d_sda_nanc
  V[["no_participation"]] = asc_no_participation 
  
  
  ### Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part, laa = lambda_laa, mna = lambda_mna, sba = lambda_sba)   ### Specify tree structure for NL model
  
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

out_c7 <- run_manual_and_compare(
  cluster_id = "c7",
  database_wide = db_c7,
  model_original = m0_c7,
  apollo_beta_MANUAL = apollo_beta_c7,
  apollo_fixed_MANUAL = apollo_fixed_c7,
  apollo_probabilities_MANUAL = apollo_probabilities_c7,
  constraints_MANUAL = NULL
)


# MODELS

summary(out_c4$model_manual)
summary(out_c5$model_manual)
summary(out_c6$model_manual)
summary(out_c7$model_manual)

# LR test
lr_test(model_restricted = out_c4$model_manual, model_unrestricted = m0_c4)
lr_test(model_restricted = out_c5$model_manual, model_unrestricted = m0_c5)
lr_test(model_restricted = out_c6$model_manual, model_unrestricted = m0_c6)
lr_test(model_restricted = out_c7$model_manual, model_unrestricted = m0_c7)


# ### ALL CLUSTERS: bind rows #####
# comp_all <- bind_rows(out_c4$compare, out_c5$compare, out_c6$compare, out_c7$compare)
# write.csv(comp_all, file=file.path("R","output","compare_manual_vs_original_bestSDM.csv"), row.names=FALSE)
