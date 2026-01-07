############################################################
# DRIVER: Run all clusters using BEST SDM spec (mean_avail)
#         + baseline vs species × mean_avail (species-level)
# Author: you (with ChatGPT help)
############################################################

# --- Packages ---
library(apollo)
library(dplyr)
library(stringr)

# ==========================================================
# 0) REQUIRED OBJECTS (must exist in your session)
# ==========================================================
# You already have these in your current pipeline/script:
res_c4 <- readRDS("res_c4.rds") 
res_c5 <- readRDS("res_c5.rds") 
res_c6 <- readRDS("res_c6.rds") 
res_c7 <- readRDS("res_c7.rds") 
apollo_beta_c4 <- readRDS("apollo_beta_c4.rds")
apollo_fixed_c4 <- readRDS("apollo_fixed_c4.rds")
apollo_probabilities_c4 <- readRDS("apollo_probabilities_c4.rds")
apollo_beta_c5 <- readRDS("apollo_beta_c5.rds")
apollo_fixed_c5 <- readRDS("apollo_fixed_c5.rds")
apollo_probabilities_c5 <- readRDS("apollo_probabilities_c5.rds")
apollo_beta_c6 <- readRDS("apollo_beta_c6.rds")
apollo_fixed_c6 <- readRDS( "apollo_fixed_c6.rds")
apollo_probabilities_c6 <- readRDS("apollo_probabilities_c6.rds")
apollo_beta_c7 <- readRDS("apollo_beta_c7.rds")
apollo_fixed_c7 <- readRDS( "apollo_fixed_c7.rds")
apollo_probabilities_c7 <- readRDS("apollo_probabilities_c7.rds")


# ==========================================================
# 1) Pick BEST SDM spec per cluster (from oos_table)
# ==========================================================
oos_table <- readRDS("oos_table.RDS")

best_spec <- oos_table %>%
  group_by(cluster) %>%
  slice_max(order_by = pseudoR2_oos, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cluster, spec, pseudoR2_oos, LL_test)

print(best_spec)

# ==========================================================
# 2) Species × mean_avail helpers (generic)
# ==========================================================
# Assumption: your wide database has columns like:
#   mean_avail_<altName>
# where altName looks like:
#   laa_msqd, psdn_cmck, etc.
# Species token is taken as the LAST underscore chunk of altName, e.g. "msqd" from "laa_msqd"
# If your alt naming differs, change extract_species_from_alt().

extract_species_from_alt <- function(alt_name){
  # "laa_msqd" -> "msqd"
  # "cwa_albc" -> "albc"
  pieces <- str_split(alt_name, "_", simplify = TRUE)
  pieces[, ncol(pieces)]
}

make_alt_species_map_from_db <- function(db, mean_avail_prefix = "mean_avail_"){
  ma_cols <- names(db)[startsWith(names(db), mean_avail_prefix)]
  if(length(ma_cols)==0) stop("No mean_avail_* columns found in database_wide.")
  
  alt_names <- sub(paste0("^", mean_avail_prefix), "", ma_cols)
  species   <- vapply(alt_names, extract_species_from_alt, character(1))
  
  data.frame(
    mean_avail_col = ma_cols,
    alt_name       = alt_names,
    species        = species,
    stringsAsFactors = FALSE
  )
}

# This function ADDS species-specific mean_avail terms into your utilities V
# You MUST call it INSIDE your apollo_probabilities_* AFTER building V, before apollo_nl().
#
# It uses:
# - apollo_inputs$use_species_avail (0/1)
# - apollo_inputs$alt_species_map (data frame produced above)
#
# It expects coefficients named: B_avail_<SPECIES> (e.g., B_avail_msqd)
add_species_avail_to_V <- function(V, db, apollo_beta, apollo_inputs){
  if(is.null(apollo_inputs$use_species_avail) || apollo_inputs$use_species_avail==0) return(V)
  
  m <- apollo_inputs$alt_species_map
  if(is.null(m) || nrow(m)==0) stop("apollo_inputs$alt_species_map missing/empty.")
  
  # add for each alternative j:
  #   V[[j]] <- V[[j]] + B_avail_species(j) * mean_avail_j
  for(i in seq_len(nrow(m))){
    alt   <- m$alt_name[i]
    sp    <- m$species[i]
    col   <- m$mean_avail_col[i]
    bname <- paste0("B_avail_", sp)
    
    if(!bname %in% names(apollo_beta)){
      stop(paste0("Missing coefficient in apollo_beta: ", bname,
                  " (needed for species×mean_avail)."))
    }
    if(!alt %in% names(V)){
      # If your V uses different names than alt_name extracted from mean_avail columns,
      # you need to adapt this mapping.
      stop(paste0("Alternative '", alt, "' not found in V names. ",
                  "Check your mean_avail_* column naming vs V alternative names."))
    }
    
    V[[alt]] <- V[[alt]] + apollo_beta[[bname]] * db[[col]]
  }
  V
}

# Build the needed betas for species-specific availability
# It creates coefficients for each species present in this cluster/spec’s wide DB:
#   B_avail_msqd, B_avail_psdn, ...
#
# IMPORTANT:
# - We usually FIX the original common mean_avail coefficient to 0 (if you have one),
#   to avoid duplication. If your common coefficient name differs, change common_name.
add_species_avail_betas <- function(beta, fixed, db, common_name = "B_mean_avail",
                                    mean_avail_prefix = "mean_avail_"){
  map <- make_alt_species_map_from_db(db, mean_avail_prefix = mean_avail_prefix)
  species_set <- sort(unique(map$species))
  
  # Add species-specific coefficients if missing
  for(sp in species_set){
    nm <- paste0("B_avail_", sp)
    if(!nm %in% names(beta)) beta[[nm]] <- 0
  }
  
  # Optional: remove/zero the common mean_avail coefficient to avoid collinearity
  if(common_name %in% names(beta)){
    # safest: FIX it at 0 (adds to apollo_fixed)
    if(!common_name %in% fixed) fixed <- c(fixed, common_name)
    beta[[common_name]] <- 0
  }
  
  list(beta = beta, fixed = fixed, map = map)
}

# ==========================================================
# 3) Fit helpers + LR test
# ==========================================================
lr_test <- function(m_restricted, m_unrestricted){
  LLr <- m_restricted$maximum
  LLu <- m_unrestricted$maximum
  k_r <- length(m_restricted$estimate)
  k_u <- length(m_unrestricted$estimate)
  LR  <- 2*(LLu - LLr)
  df  <- k_u - k_r
  p   <- pchisq(LR, df=df, lower.tail=FALSE)
  data.frame(LR=LR, df=df, p_value=p, LLr=LLr, LLu=LLu, k_r=k_r, k_u=k_u)
}

fit_apollo <- function(modelName, database_wide, apollo_beta, apollo_fixed, apollo_prob_fun,
                       indivID="fished_vessel_anon", nCores=14, outDir=file.path("R","output"),
                       extra_inputs=list()){
  apollo_control <- list(
    modelName       = modelName,
    modelDescr      = "NL participation model (best SDM spec)",
    indivID         = indivID,
    outputDirectory = outDir,
    panelData       = TRUE,
    nCores          = nCores,
    workInLogs      = TRUE
  )
  
  assign("database", database_wide, envir=.GlobalEnv)
  assign("apollo_control", apollo_control, envir=.GlobalEnv)
  assign("apollo_beta", apollo_beta, envir=.GlobalEnv)
  assign("apollo_fixed", apollo_fixed, envir=.GlobalEnv)
  
  apollo_inputs <- apollo_validateInputs()
  
  # inject flags/maps accessible inside apollo_probabilities_* via apollo_inputs
  for(nm in names(extra_inputs)) apollo_inputs[[nm]] <- extra_inputs[[nm]]
  
  m <- apollo_estimate(
    apollo_beta, apollo_fixed,
    apollo_prob_fun, apollo_inputs,
    estimate_settings = list(
      hessianRoutine = "numDeriv",
      scaleBeta      = TRUE,
      maxIterations  = 200
    )
  )
  apollo_saveOutput(m, saveOutput_settings = list(printT1 = 1))
  return(list(model=m, inputs=apollo_inputs, control=apollo_control))
}

# ==========================================================
# 4) IMPORTANT: minimal change you MUST do inside each apollo_probabilities_cX
# ==========================================================
# In EACH cluster-specific apollo_probabilities_* function:
#
#  (a) Build V as you already do
#  (b) ADD THIS LINE BEFORE apollo_nl(...):
#
#      V <- add_species_avail_to_V(V, database, apollo_beta, apollo_inputs)
#
# That’s it.
#
# Example insertion (pseudo):
#
# apollo_probabilities_c5 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
#   apollo_attach(apollo_beta, apollo_inputs)
#   on.exit(apollo_detach(apollo_beta, apollo_inputs))
#   P <- list()
#   V <- list()
#   ... your utility definitions ...
#   V <- add_species_avail_to_V(V, database, apollo_beta, apollo_inputs)
#   P[["model"]] <- apollo_nl(...)
#   P <- apollo_panelProd(P, apollo_inputs, functionality)
#   P <- apollo_prepareProb(P, apollo_inputs, functionality)
#   return(P)
# }
#
# ==========================================================
# 5) Cluster objects + run all
# ==========================================================
cluster_objs <- list(
  c4 = list(res=res_c4, beta=apollo_beta_c4, fixed=apollo_fixed_c4, prob=apollo_probabilities_c4),
  c5 = list(res=res_c5, beta=apollo_beta_c5, fixed=apollo_fixed_c5, prob=apollo_probabilities_c5),
  c6 = list(res=res_c6, beta=apollo_beta_c6, fixed=apollo_fixed_c6, prob=apollo_probabilities_c6),
  c7 = list(res=res_c7, beta=apollo_beta_c7, fixed=apollo_fixed_c7, prob=apollo_probabilities_c7)
)

results <- list()
tests   <- list()

for(cl in names(cluster_objs)){
  
  spec_cl <- best_spec %>% filter(cluster==cl) %>% pull(spec)
  if(length(spec_cl)==0) stop(paste("No best spec for", cl))
  
  db_best <- cluster_objs[[cl]]$res$models[[spec_cl]]$database_wide
  if(is.null(db_best)) stop(paste("database_wide NULL:", cl, spec_cl))
  
  cat("\n====================================================\n")
  cat("Cluster:", cl, " | Best SDM spec:", spec_cl, "\n")
  cat("====================================================\n")
  
  results[[cl]] <- list()
  
  # ---- (1) Baseline (your current model as-is) ----
  fit0 <- fit_apollo(
    modelName = paste0("NL_", cl, "_bestSDM_", spec_cl, "_baseline"),
    database_wide = db_best,
    apollo_beta   = cluster_objs[[cl]]$beta,
    apollo_fixed  = cluster_objs[[cl]]$fixed,
    apollo_prob_fun = cluster_objs[[cl]]$prob,
    nCores = 18,
    extra_inputs = list(
      use_species_avail = 0
    )
  )
  results[[cl]][["baseline"]] <- fit0
  
  # ---- (2) Species × mean_avail ----
  sp_setup <- add_species_avail_betas(
    beta  = cluster_objs[[cl]]$beta,
    fixed = cluster_objs[[cl]]$fixed,
    db    = db_best,
    common_name = "B_mean_avail",     # CHANGE if your common availability coef has different name
    mean_avail_prefix = "mean_avail_" # CHANGE if your columns differ
  )
  
  fit1 <- fit_apollo(
    modelName = paste0("NL_", cl, "_bestSDM_", spec_cl, "_speciesXavail"),
    database_wide = db_best,
    apollo_beta   = sp_setup$beta,
    apollo_fixed  = sp_setup$fixed,
    apollo_prob_fun = cluster_objs[[cl]]$prob,
    nCores = 18,
    extra_inputs = list(
      use_species_avail = 1,
      alt_species_map   = sp_setup$map
    )
  )
  results[[cl]][["speciesXavail"]] <- fit1
  
  # ---- LR test (baseline nested in speciesXavail) ----
  tests[[cl]] <- lr_test(fit0$model, fit1$model)
  tests[[cl]]$cluster <- cl
  tests[[cl]]$spec    <- spec_cl
  
  print(tests[[cl]])
}

# Bind tests into a table
tests_table <- bind_rows(tests) %>%
  select(cluster, spec, LR, df, p_value, LLr, LLu, k_r, k_u)

print(tests_table)

write.csv(tests_table,
          file = file.path("R","output","LR_speciesXavail_bestSDM_byCluster.csv"),
          row.names = FALSE)

saveRDS(results, file = file.path("R","output","fits_bestSDM_baseline_vs_speciesXavail.rds"))
