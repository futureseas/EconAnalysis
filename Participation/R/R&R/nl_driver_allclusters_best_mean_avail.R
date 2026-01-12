# ==========================================================
# REQUIRED OBJECTS (must exist in your session)
# ==========================================================
# You already have these in your current pipeline/script:

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
                                   apollo_beta_MANUAL, apollo_fixed_MANUAL, apollo_probabilities_MANUAL, indivID="fished_vessel_anon", nCores=16,
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
    estimate_settings = list(
      estimationRoutine = "bfgs", # Cambiamos BGW por BFGS
      hessianRoutine = "numDeriv",
      scaling = FALSE,
      maxIterations = 500
    )
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
  asc_nanc=-2.75, 
  asc_cmck=-2.75, 
  asc_msqd=-2.75, 
  asc_ytna=-2.75, 
  asc_btna=-2.75, 
  asc_psdn=-2.75, 
  c_psdn=-0.4, 
  w_msqd=-4,
  theta_part=0, theta_cmck=12, theta_msqd=0, theta_psdn=0, theta_nanc=0, theta_tuna=12,
  B_mean_avail_nanc=1.15, B_mean_avail_cmck=1.15, B_mean_avail_msqd=1.15, B_mean_avail_ytna=1.15, B_mean_avail_btna=1.15, B_mean_avail_psdn=1.15,
  B_mean_price=0.3, 
  B_wind_max_220_mh=-0.05, 
  B_d_d=-1, 
  B_d_cd=-1, 
  B_d_c=0,
  B_dist_port_to_catch_area_zero=-0.005, B_dummy_prev_days=1.2, B_dummy_prev_year_days=0.15, B_unem_rate_part=0.2,
  asc_no_participation=0, B_dist_to_cog=-0.001)

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
  V[["sfa_nanc"]] <- asc_nanc + B_mean_avail_nanc*mean_avail_sfa_nanc + B_mean_price*mean_price_sfa_nanc + B_wind_max_220_mh*wind_max_220_mh_sfa_nanc + B_d_d*d_d_sfa_nanc + B_d_cd*d_cd_sfa_nanc + B_d_c*d_c_sfa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_nanc + B_dummy_prev_days*dummy_prev_days_sfa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_sfa_nanc + B_unem_rate_part*unem_rate_sfa_nanc + B_dist_to_cog*dist_to_cog_sfa_nanc                                        
  V[["laa_nanc"]] <- asc_nanc + B_mean_avail_nanc*mean_avail_laa_nanc + B_mean_price*mean_price_laa_nanc + B_wind_max_220_mh*wind_max_220_mh_laa_nanc + B_d_d*d_d_laa_nanc + B_d_cd*d_cd_laa_nanc + B_d_c*d_c_laa_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days*dummy_prev_days_laa_nanc + B_dummy_prev_year_days*dummy_prev_year_days_laa_nanc + B_unem_rate_part*unem_rate_laa_nanc + B_dist_to_cog*dist_to_cog_laa_nanc                                        
  V[["laa_cmck"]] <- asc_cmck + B_mean_avail_cmck*mean_avail_laa_cmck + B_mean_price*mean_price_laa_cmck + B_wind_max_220_mh*wind_max_220_mh_laa_cmck + B_d_d*d_d_laa_cmck + B_d_cd*d_cd_laa_cmck + B_d_c*d_c_laa_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days*dummy_prev_days_laa_cmck + B_dummy_prev_year_days*dummy_prev_year_days_laa_cmck + B_unem_rate_part*unem_rate_laa_cmck + B_dist_to_cog*dist_to_cog_laa_cmck                                        
  V[["laa_msqd"]] <- asc_msqd + B_mean_avail_msqd*mean_avail_laa_msqd + B_mean_price*mean_price_laa_msqd + B_wind_max_220_mh*wind_max_220_mh_laa_msqd + B_d_d*d_d_laa_msqd + B_d_cd*d_cd_laa_msqd + B_d_c*d_c_laa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days*dummy_prev_days_laa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_laa_msqd + B_unem_rate_part*unem_rate_laa_msqd + B_dist_to_cog*dist_to_cog_laa_msqd + w_msqd * weekend                        
  V[["laa_ytna"]] <- asc_ytna + B_mean_avail_ytna*mean_avail_laa_ytna + B_mean_price*mean_price_laa_ytna + B_wind_max_220_mh*wind_max_220_mh_laa_ytna + B_d_d*d_d_laa_ytna + B_d_cd*d_cd_laa_ytna + B_d_c*d_c_laa_ytna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_ytna + B_dummy_prev_days*dummy_prev_days_laa_ytna + B_dummy_prev_year_days*dummy_prev_year_days_laa_ytna + B_unem_rate_part*unem_rate_laa_ytna + B_dist_to_cog*dist_to_cog_laa_ytna                                         
  V[["mna_msqd"]] <- asc_msqd + B_mean_avail_msqd*mean_avail_mna_msqd + B_mean_price*mean_price_mna_msqd + B_wind_max_220_mh*wind_max_220_mh_mna_msqd + B_d_d*d_d_mna_msqd + B_d_cd*d_cd_mna_msqd + B_d_c*d_c_mna_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days*dummy_prev_days_mna_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mna_msqd + B_unem_rate_part*unem_rate_mna_msqd + B_dist_to_cog*dist_to_cog_mna_msqd + w_msqd * weekend                        
  V[["sba_msqd"]] <- asc_msqd + B_mean_avail_msqd*mean_avail_sba_msqd + B_mean_price*mean_price_sba_msqd + B_wind_max_220_mh*wind_max_220_mh_sba_msqd + B_d_d*d_d_sba_msqd + B_d_cd*d_cd_sba_msqd + B_d_c*d_c_sba_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days*dummy_prev_days_sba_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sba_msqd + B_unem_rate_part*unem_rate_sba_msqd + B_dist_to_cog*dist_to_cog_sba_msqd + w_msqd * weekend                        
  V[["laa_btna"]] <- asc_btna + B_mean_avail_btna*mean_avail_laa_btna + B_mean_price*mean_price_laa_btna + B_wind_max_220_mh*wind_max_220_mh_laa_btna + B_d_d*d_d_laa_btna + B_d_cd*d_cd_laa_btna + B_d_c*d_c_laa_btna + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_btna + B_dummy_prev_days*dummy_prev_days_laa_btna + B_dummy_prev_year_days*dummy_prev_year_days_laa_btna + B_unem_rate_part*unem_rate_laa_btna + B_dist_to_cog*dist_to_cog_laa_btna                          
  V[["sfa_msqd"]] <- asc_msqd + B_mean_avail_msqd*mean_avail_sfa_msqd + B_mean_price*mean_price_sfa_msqd + B_wind_max_220_mh*wind_max_220_mh_sfa_msqd + B_d_d*d_d_sfa_msqd + B_d_cd*d_cd_sfa_msqd + B_d_c*d_c_sfa_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days*dummy_prev_days_sfa_msqd + B_dummy_prev_year_days*dummy_prev_year_days_sfa_msqd + B_unem_rate_part*unem_rate_sfa_msqd + B_dist_to_cog*dist_to_cog_sfa_msqd + w_msqd * weekend                        
  V[["mna_psdn"]] <- asc_psdn + B_mean_avail_psdn*mean_avail_mna_psdn + B_mean_price*mean_price_mna_psdn + B_wind_max_220_mh*wind_max_220_mh_mna_psdn + B_d_d*d_d_mna_psdn + B_d_cd*d_cd_mna_psdn + B_d_c*d_c_mna_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days*dummy_prev_days_mna_psdn + B_dummy_prev_year_days*dummy_prev_year_days_mna_psdn + B_unem_rate_part*unem_rate_mna_psdn + B_dist_to_cog*dist_to_cog_mna_psdn + c_psdn * psdnclosure 
  V[["sba_cmck"]] <- asc_cmck + B_mean_avail_cmck*mean_avail_sba_cmck + B_mean_price*mean_price_sba_cmck + B_wind_max_220_mh*wind_max_220_mh_sba_cmck + B_d_d*d_d_sba_cmck + B_d_cd*d_cd_sba_cmck + B_d_c*d_c_sba_cmck + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days*dummy_prev_days_sba_cmck + B_dummy_prev_year_days*dummy_prev_year_days_sba_cmck + B_unem_rate_part*unem_rate_sba_cmck + B_dist_to_cog*dist_to_cog_sba_cmck                      
  V[["mra_msqd"]] <- asc_msqd + B_mean_avail_msqd*mean_avail_mra_msqd + B_mean_price*mean_price_mra_msqd + B_wind_max_220_mh*wind_max_220_mh_mra_msqd + B_d_d*d_d_mra_msqd + B_d_cd*d_cd_mra_msqd + B_d_c*d_c_mra_msqd + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days*dummy_prev_days_mra_msqd + B_dummy_prev_year_days*dummy_prev_year_days_mra_msqd + B_unem_rate_part*unem_rate_mra_msqd + B_dist_to_cog*dist_to_cog_mra_msqd + w_msqd * weekend                        
  V[["laa_psdn"]] <- asc_psdn + B_mean_avail_psdn*mean_avail_laa_psdn + B_mean_price*mean_price_laa_psdn + B_wind_max_220_mh*wind_max_220_mh_laa_psdn + B_d_d*d_d_laa_psdn + B_d_cd*d_cd_laa_psdn + B_d_c*d_c_laa_psdn + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days*dummy_prev_days_laa_psdn + B_dummy_prev_year_days*dummy_prev_year_days_laa_psdn + B_unem_rate_part*unem_rate_laa_psdn + B_dist_to_cog*dist_to_cog_laa_psdn + c_psdn * psdnclosure 
  V[["mna_nanc"]] <- asc_nanc + B_mean_avail_nanc*mean_avail_mna_nanc + B_mean_price*mean_price_mna_nanc + B_wind_max_220_mh*wind_max_220_mh_mna_nanc + B_d_d*d_d_mna_nanc + B_d_cd*d_cd_mna_nanc + B_d_c*d_c_mna_nanc + B_dist_port_to_catch_area_zero*dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days*dummy_prev_days_mna_nanc + B_dummy_prev_year_days*dummy_prev_year_days_mna_nanc + B_unem_rate_part*unem_rate_mna_nanc + B_dist_to_cog*dist_to_cog_mna_nanc                      
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
  apollo_probabilities_MANUAL = apollo_probabilities_c4
)

print(out_c4$compare)
summary(out_c4$model_manual)


# ===================== Cluster c5 (from n_logit_c5.R) =====================

db_c5 <- assets$c5$database_wide
m0_c5 <- assets$c5$model_original
apollo_beta_c5 = c(
              B_mean_avail_msqd              = 2.3,
              B_mean_avail_psdn              = 2.3,
              B_mean_avail_cmck              = 2.3,
              B_mean_avail_nanc              = 2.3,
              B_mean_avail_albc              = 2.3,
              B_mean_avail_dcrb              = 2.3,
              B_wind_max_220_mh              = -0.07,
              B_dist_to_cog                  = -0.0008,
              B_dist_port_to_catch_area_zero = -0.0045,
              B_dummy_prev_days              = 2.5,
              B_dummy_prev_year_days         = 0.35,
              B_unem_rate                    = 0.4,
              B_d_d                          = -1.75,
              c_psdn                         = -2,
              c_dcrb                         = -2,
              B_mean_price_part              = 3.75,
              B_mean_price_crab              = 0.1,
              asc_msqd              =  -9.547246,
              asc_psdn              =  -6.513243,
              asc_cmck              =  -6.372688,
              asc_nanc              = -9.688408,
              asc_albc              = -14.400378,
              asc_dcrb              =  -7.460698,
              theta_part                     = 0.5,
              theta_cmck                     = 10,
              theta_msqd                     = 2.362863,
              theta_psdn                     = 0.006157,
              theta_part_crab                = 10,
              w_msqd                         = -2,
              asc_no_participation           = 0)

apollo_fixed_c5 = c("asc_no_participation", "theta_cmck", "theta_psdn", "theta_part_crab", "asc_albc", "asc_nanc", "asc_cmck", "asc_dcrb", "asc_psdn")

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
  V[["mna_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_mna_msqd + B_mean_avail_msqd * mean_avail_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_unem_rate * unem_rate_mna_msqd                                    + B_d_d * d_d_mna_msqd + w_msqd * weekend                            
  V[["sba_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_sba_msqd + B_mean_avail_msqd * mean_avail_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_unem_rate * unem_rate_sba_msqd                                    + B_d_d * d_d_sba_msqd + w_msqd * weekend                            
  V[["mra_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_mra_msqd + B_mean_avail_msqd * mean_avail_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_unem_rate * unem_rate_mra_msqd                                    + B_d_d * d_d_mra_msqd + w_msqd * weekend                            
  V[["laa_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_laa_msqd + B_mean_avail_msqd * mean_avail_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_unem_rate * unem_rate_laa_msqd                                    + B_d_d * d_d_laa_msqd + w_msqd * weekend                            
  V[["npa_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_npa_msqd + B_mean_avail_msqd * mean_avail_npa_msqd + B_wind_max_220_mh * wind_max_220_mh_npa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_npa_msqd + B_dummy_prev_days * dummy_prev_days_npa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_npa_msqd + B_dist_to_cog * dist_to_cog_npa_msqd + B_unem_rate * unem_rate_npa_msqd                                    + B_d_d * d_d_npa_msqd     
  V[["sfa_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_sfa_msqd + B_mean_avail_msqd * mean_avail_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_unem_rate * unem_rate_sfa_msqd                                    + B_d_d * d_d_sfa_msqd + w_msqd * weekend                            
  V[["cba_msqd"]]         = asc_msqd + B_mean_price_part * mean_price_3_cba_msqd + B_mean_avail_msqd * mean_avail_cba_msqd + B_wind_max_220_mh * wind_max_220_mh_cba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_msqd + B_dummy_prev_days * dummy_prev_days_cba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_cba_msqd + B_dist_to_cog * dist_to_cog_cba_msqd + B_unem_rate * unem_rate_cba_msqd                                    + B_d_d * d_d_cba_msqd                              
  V[["laa_psdn"]]         = asc_psdn + B_mean_price_part * mean_price_3_laa_psdn + B_mean_avail_psdn * mean_avail_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_unem_rate * unem_rate_laa_psdn + c_psdn * psdnclosure             + B_d_d * d_d_laa_psdn     
  V[["clo_psdn"]]         = asc_psdn + B_mean_price_part * mean_price_3_clo_psdn + B_mean_avail_psdn * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_unem_rate * unem_rate_clo_psdn + c_psdn * psdnclosure             + B_d_d * d_d_clo_psdn                             
  V[["cwa_psdn"]]         = asc_psdn + B_mean_price_part * mean_price_3_cwa_psdn + B_mean_avail_psdn * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_unem_rate * unem_rate_cwa_psdn + c_psdn * psdnclosure             + B_d_d * d_d_cwa_psdn                            
  V[["clw_psdn"]]         = asc_psdn + B_mean_price_part * mean_price_3_clw_psdn + B_mean_avail_psdn * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_unem_rate * unem_rate_clw_psdn + c_psdn * psdnclosure             + B_d_d * d_d_clw_psdn                            
  V[["sba_cmck"]]         = asc_cmck + B_mean_price_part * mean_price_3_sba_cmck + B_mean_avail_cmck * mean_avail_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_dist_to_cog * dist_to_cog_sba_cmck + B_unem_rate * unem_rate_sba_cmck                                    + B_d_d * d_d_sba_cmck                             
  V[["laa_cmck"]]         = asc_cmck + B_mean_price_part * mean_price_3_laa_cmck + B_mean_avail_cmck * mean_avail_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_unem_rate * unem_rate_laa_cmck                                    + B_d_d * d_d_laa_cmck                             
  V[["laa_nanc"]]         = asc_nanc + B_mean_price_part * mean_price_3_laa_nanc + B_mean_avail_nanc * mean_avail_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_dist_to_cog * dist_to_cog_laa_nanc + B_unem_rate * unem_rate_laa_nanc                                    + B_d_d * d_d_laa_nanc          
  V[["cwa_albc"]]         = asc_albc + B_mean_price_part * mean_price_3_cwa_albc + B_mean_avail_albc * mean_avail_cwa_albc + B_wind_max_220_mh * wind_max_220_mh_cwa_albc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_albc + B_dummy_prev_days * dummy_prev_days_cwa_albc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_albc + B_dist_to_cog * dist_to_cog_cwa_albc + B_unem_rate * unem_rate_cwa_albc                                    + B_d_d * d_d_cwa_albc                             
  V[["cwa_dcrb"]]         = asc_dcrb + B_mean_price_crab * mean_price_3_cwa_dcrb + B_mean_avail_dcrb * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_unem_rate * unem_rate_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb + B_d_d * d_d_cwa_dcrb                             
  V[["clw_dcrb"]]         = asc_dcrb + B_mean_price_crab * mean_price_3_clw_dcrb + B_mean_avail_dcrb * mean_avail_clw_dcrb + B_wind_max_220_mh * wind_max_220_mh_clw_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_dcrb + B_dummy_prev_days * dummy_prev_days_clw_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_clw_dcrb + B_dist_to_cog * dist_to_cog_clw_dcrb + B_unem_rate * unem_rate_clw_dcrb + c_dcrb * dcrbclosurewad_clw_dcrb + B_d_d * d_d_clw_dcrb                             
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
    laa_nanc = 1 - weekend,
    cwa_albc = 1,
    cwa_dcrb = 1 - d_cd_cwa_dcrb,
    clw_dcrb = 1 - d_cd_clw_dcrb,
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
  apollo_probabilities_MANUAL = apollo_probabilities_c5
)

print(out_c5$compare)
summary(out_c5$model_manual)


# ===================== Cluster c6 (from n_logit_c6.R) =====================

db_c6 <- assets$c6$database_wide
db_c6 <- db_c6[db_c6$choice != 10, ]
db_c6 <- db_c6[db_c6$choice != 8, ]

m0_c6 <- assets$c6$model_original
apollo_beta_c6=c(
              B_mean_avail_psdn              = 1,
              B_mean_avail_nanc              = 1,
              B_mean_avail_dcrb              = 7.78,
              B_wind_max_220_mh              = -0.05,
              B_dist_to_cog                  = -0.005,
              B_dist_port_to_catch_area_zero = -0.02,
              B_dummy_prev_days              = 2,
              B_dummy_prev_year_days         = 0.3,
              B_unem_rate                    = 0.17,
              B_d_d                          = -0.24,
              c_dcrb                         = -0.25,
              B_mean_price_part              = 0,
              asc_psdn                   = -2.87,
              asc_nanc                   = -2.56,
              asc_dcrb                   = -6.93,
              theta_part                    =  -0.120167,
              asc_no_participation           = 0
             )

apollo_fixed_c6 <- c("asc_no_participation", "c_dcrb", "B_d_d", "theta_part", "asc_dcrb", "asc_nanc", "asc_psdn")



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

  
  
  ### Create list of probabilities P
  P = list()
  
  # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["cba_psdn"]]         = asc_psdn + B_mean_avail_psdn * mean_avail_cba_psdn + B_wind_max_220_mh * wind_max_220_mh_cba_psdn + B_dist_to_cog * dist_to_cog_cba_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_psdn + B_dummy_prev_days * dummy_prev_days_cba_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cba_psdn  + B_unem_rate * unem_rate_cba_psdn  + B_d_d * d_d_cba_psdn + B_mean_price_part * mean_price_3_cba_psdn
  V[["clo_psdn"]]         = asc_psdn + B_mean_avail_psdn * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn  + B_unem_rate * unem_rate_clo_psdn  + B_d_d * d_d_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn
  V[["clw_psdn"]]         = asc_psdn + B_mean_avail_psdn * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn  + B_unem_rate * unem_rate_clw_psdn  + B_d_d * d_d_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn 
  V[["cwa_psdn"]]         = asc_psdn + B_mean_avail_psdn * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn  + B_unem_rate * unem_rate_cwa_psdn  + B_d_d * d_d_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn
  V[["clo_nanc"]]         = asc_nanc + B_mean_avail_nanc * mean_avail_clo_nanc + B_wind_max_220_mh * wind_max_220_mh_clo_nanc + B_dist_to_cog * dist_to_cog_clo_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_nanc + B_dummy_prev_days * dummy_prev_days_clo_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clo_nanc  + B_unem_rate * unem_rate_clo_nanc  + B_d_d * d_d_clo_nanc + B_mean_price_part * mean_price_3_clo_nanc
  V[["clw_nanc"]]         = asc_nanc + B_mean_avail_nanc * mean_avail_clw_nanc + B_wind_max_220_mh * wind_max_220_mh_clw_nanc + B_dist_to_cog * dist_to_cog_clw_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_nanc + B_dummy_prev_days * dummy_prev_days_clw_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clw_nanc  + B_unem_rate * unem_rate_clw_nanc  + B_d_d * d_d_clw_nanc + B_mean_price_part * mean_price_3_clw_nanc
  V[["cwa_nanc"]]         = asc_nanc + B_mean_avail_nanc * mean_avail_cwa_nanc + B_wind_max_220_mh * wind_max_220_mh_cwa_nanc + B_dist_to_cog * dist_to_cog_cwa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_nanc + B_dummy_prev_days * dummy_prev_days_cwa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_nanc  + B_unem_rate * unem_rate_cwa_nanc  + B_d_d * d_d_cwa_nanc + B_mean_price_part * mean_price_3_cwa_nanc
  V[["cwa_dcrb"]]         = asc_dcrb + B_mean_avail_dcrb * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb  + B_unem_rate * unem_rate_cwa_dcrb  + B_d_d * d_d_cwa_dcrb + B_mean_price_part * mean_price_3_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb
  V[["no_participation"]] = asc_no_participation 
  
  
  ## Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part", "cwa_dcrb")
  nlStructure[["part"]] = c("clo_nanc", "clw_nanc", "cwa_nanc", "cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn")

  avail <- list(
    cba_psdn         = 1 - psdnclosure,
    clo_psdn         = 1 - psdnclosure,
    clw_psdn         = (1 - psdnclosure) * (1 - waclosure_clw_psdn),
    cwa_psdn         = (1 - psdnclosure) * (1 - waclosure_cwa_psdn),
    clo_nanc         = 1,
    clw_nanc         = 1,
    cwa_nanc         = 1,
    cwa_dcrb         = 1,
    no_participation = 1)
  
  nl_settings <- list(
    alternatives = c(cba_psdn =1,
                     clo_psdn =2,
                     clw_psdn =3,
                     cwa_psdn =4,
                     clo_nanc =5,
                     clw_nanc =6,
                     cwa_nanc =7,
                     cwa_dcrb =9,
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
  apollo_probabilities_MANUAL = apollo_probabilities_c6
)

print(out_c6$compare)
summary(out_c6$model_manual)


# ===================== Cluster c7 (from n_logit_c7.R) =====================
db_c7 <- assets$c7$database_wide
m0_c7 <- assets$c7$model_original
apollo_beta_c7 = c(
  B_mean_avail_jmck                = -5.015679, 
  B_mean_avail_msqd                =  2.422980, 
  B_mean_avail_psdn                = -1.226216, 
  B_mean_avail_nanc                =  0.174758, 
  B_mean_price                     =  0.834244, 
  B_wind_max_220_mh                = -0.042389, 
  B_dist_to_cog                    = -0.002051, 
  B_dist_port_to_catch_area_zero   = -0.009998, 
  B_dummy_prev_days                =  2.944770, 
  B_dummy_prev_year_days           =  0.174922, 
  c_psdn                           = -1.167252, 
  B_d_d                            = -0.782539, 
  B_unem_rate_part                 =  0.124537, 
  w_msqd                           = -3.831277, 
  asc_laa                          = -3.849474, 
  asc_mna                          = -4.458859, 
  asc_mra                          = -4.879744, 
  asc_sba                          = -3.832180, 
  asc_sfa                          = -4.742179, 
  asc_sda                          = -4.522930, 
  theta_part                       =  0.761065, 
  theta_laa                        = 10, 
  theta_mna                        = 10, 
  theta_sba                        =  0.892268, 
  asc_no_participation             =  0.000000 
  )


apollo_fixed_c7 = c("asc_no_participation")

apollo_fixed_c7 = c(
  "asc_no_participation", "theta_mna",  "theta_laa", "theta_sba", "theta_part",
  "c_psdn", "B_d_d", "w_msqd",
  "B_unem_rate_part", "B_dist_to_cog", "B_wind_max_220_mh", "B_dist_port_to_catch_area_zero",
  "asc_laa",
  "asc_mna",
  "asc_mra",
  "asc_sba",
  "asc_sfa",
  "asc_sda"
)


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
  V[["laa_cmck"]]         = asc_laa + B_mean_avail_mckl * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck  + B_unem_rate_part * unem_rate_laa_cmck + B_d_d * d_d_laa_cmck
  V[["mna_cmck"]]         = asc_mna + B_mean_avail_mckl * mean_avail_mna_cmck + B_mean_price * mean_price_mna_cmck + B_wind_max_220_mh * wind_max_220_mh_mna_cmck + B_dist_to_cog * dist_to_cog_mna_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_cmck + B_dummy_prev_days * dummy_prev_days_mna_cmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_cmck  + B_unem_rate_part * unem_rate_mna_cmck + B_d_d * d_d_mna_cmck
  V[["laa_jmck"]]         = asc_laa + B_mean_avail_mckl * mean_avail_laa_jmck + B_mean_price * mean_price_laa_jmck + B_wind_max_220_mh * wind_max_220_mh_laa_jmck + B_dist_to_cog * dist_to_cog_laa_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_jmck + B_dummy_prev_days * dummy_prev_days_laa_jmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_jmck  + B_unem_rate_part * unem_rate_laa_jmck + B_d_d * d_d_laa_jmck
  V[["mna_jmck"]]         = asc_mna + B_mean_avail_mckl * mean_avail_mna_jmck + B_mean_price * mean_price_mna_jmck + B_wind_max_220_mh * wind_max_220_mh_mna_jmck + B_dist_to_cog * dist_to_cog_mna_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_jmck + B_dummy_prev_days * dummy_prev_days_mna_jmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_jmck  + B_unem_rate_part * unem_rate_mna_jmck + B_d_d * d_d_mna_jmck
  V[["laa_msqd"]]         = asc_laa + B_mean_avail_msqd * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd  + B_unem_rate_part * unem_rate_laa_msqd + B_d_d * d_d_laa_msqd + w_msqd * weekend 
  V[["mra_msqd"]]         = asc_mra + B_mean_avail_msqd * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd  + B_unem_rate_part * unem_rate_mra_msqd + B_d_d * d_d_mra_msqd + w_msqd * weekend 
  V[["sba_msqd"]]         = asc_sba + B_mean_avail_msqd * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd  + B_unem_rate_part * unem_rate_sba_msqd + B_d_d * d_d_sba_msqd + w_msqd * weekend 
  V[["sfa_msqd"]]         = asc_sfa + B_mean_avail_msqd * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd  + B_unem_rate_part * unem_rate_sfa_msqd + B_d_d * d_d_sfa_msqd + w_msqd * weekend 
  V[["mna_msqd"]]         = asc_mna + B_mean_avail_msqd * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd  + B_unem_rate_part * unem_rate_mna_msqd + B_d_d * d_d_mna_msqd + w_msqd * weekend 
  V[["laa_psdn"]]         = asc_laa + B_mean_avail_psdn * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn  + B_unem_rate_part * unem_rate_laa_psdn + B_d_d * d_d_laa_psdn + c_psdn * psdnclosure                            
  V[["mna_psdn"]]         = asc_mna + B_mean_avail_psdn * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_dist_to_cog * dist_to_cog_mna_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn  + B_unem_rate_part * unem_rate_mna_psdn + B_d_d * d_d_mna_psdn + c_psdn * psdnclosure
  V[["mna_nanc"]]         = asc_mna + B_mean_avail_nanc * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_dist_to_cog * dist_to_cog_mna_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc  + B_unem_rate_part * unem_rate_mna_nanc + B_d_d * d_d_mna_nanc
  V[["sba_nanc"]]         = asc_sba + B_mean_avail_nanc * mean_avail_sba_nanc + B_mean_price * mean_price_sba_nanc + B_wind_max_220_mh * wind_max_220_mh_sba_nanc + B_dist_to_cog * dist_to_cog_sba_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_nanc + B_dummy_prev_days * dummy_prev_days_sba_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sba_nanc  + B_unem_rate_part * unem_rate_sba_nanc + B_d_d * d_d_sba_nanc
  V[["sda_nanc"]]         = asc_sda + B_mean_avail_nanc * mean_avail_sda_nanc + B_mean_price * mean_price_sda_nanc + B_wind_max_220_mh * wind_max_220_mh_sda_nanc + B_dist_to_cog * dist_to_cog_sda_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sda_nanc + B_dummy_prev_days * dummy_prev_days_sda_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sda_nanc  + B_unem_rate_part * unem_rate_sda_nanc + B_d_d * d_d_sda_nanc
  V[["no_participation"]] = asc_no_participation 
  
  
  ### Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part, laa = lambda_laa, mna = lambda_mna, sba = lambda_sba)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()

  nlStructure = list()
  # nlStructure[["root"]] = c("no_participation", "part")
  # nlStructure[["part"]] = c("laa_cmck", "mna_cmck", "laa_jmck", "mna_jmck", 
  #                           "laa_msqd", "mra_msqd", "sba_msqd", "sfa_msqd", 
  #                           "mna_msqd", "laa_psdn", "mna_psdn", "mna_nanc", 
  #                           "sba_nanc", "sda_nanc")
  
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
  apollo_probabilities_MANUAL = apollo_probabilities_c7
)

summary(out_c7$model_manual)


# ### ALL CLUSTERS: bind rows #####
# comp_all <- bind_rows(out_c4$compare, out_c5$compare, out_c6$compare, out_c7$compare)
# write.csv(comp_all, file=file.path("R","output","compare_manual_vs_original_bestSDM.csv"), row.names=FALSE)
