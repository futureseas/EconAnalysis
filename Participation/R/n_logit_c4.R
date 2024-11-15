# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()


### Set core controls
apollo_control = list(
  modelName       = "NL_three_levels_constraints_on_lambdas",
  modelDescr      = "Participation, location and target species decisions",
  indivID         = "fished_vessel_anon", 
  outputDirectory = "C:/GitHub/EconAnalysis/Participation/R/output",
  panelData       = FALSE,
  nCores          = 4
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Read database 
# library(haven)
# data <- read_dta("G:/Mi Unidad/Data/Anonymised data/part_model_c4.dta")
# saveRDS(data, "G:/Mi Unidad/Data/Anonymised data/part_model_c4.rds")

library(tidyr)
long_data = readRDS("G:/Mi Unidad/Data/Anonymised data/part_model_c4.rds") %>%
  dplyr::select("fished_haul_anon", "fished_vessel_anon",  "selection", "fished", "fished_vessel_anon", "mean_avail", 
                "mean_price", "wind_max_220_mh", "dist_to_cog", "dist_port_to_catch_area_zero", 
                "psdnclosured", "btnaclosured", "dummy_prev_days", "dummy_prev_year_days", 
                "unem_rate", "d_c", "d_d", "d_p", "d_cd", "d_pc", "d_pd", "d_pcd")

long_data$selection <- tolower(gsub("-", "_", long_data$selection))

database <- long_data %>%
  pivot_wider(
    names_from = selection,                  # Unique values for alternatives
    values_from = c(fished, mean_avail, mean_price, wind_max_220_mh, dist_to_cog, dist_port_to_catch_area_zero, 
                    psdnclosured, btnaclosured, dummy_prev_days, dummy_prev_year_days, 
                    unem_rate, d_c, d_d, d_p, d_cd, d_pc, d_pd, d_pcd))

database$choice <- ifelse(database$fished_sfa_nanc == 1, 1,                              
                   ifelse(database$fished_laa_nanc == 1, 2,
                   ifelse(database$fished_laa_cmck == 1, 3,                              
                   ifelse(database$fished_laa_msqd == 1, 4,
                   ifelse(database$fished_laa_ytna == 1, 5,
                   ifelse(database$fished_mna_msqd == 1, 6,                              
                   ifelse(database$fished_sba_msqd == 1, 7,
                   ifelse(database$fished_laa_btna == 1, 8,                              
                   ifelse(database$fished_sfa_msqd == 1, 9,
                   ifelse(database$fished_mna_psdn == 1, 10,                              
                   ifelse(database$fished_sba_cmck == 1, 11,
                   ifelse(database$fished_mra_msqd == 1, 12,                              
                   ifelse(database$fished_laa_psdn == 1, 13,
                   ifelse(database$fished_mna_nanc == 1, 14,
                   ifelse(database$fished_no_participation == 1, 15, NA)
                   ))))))))))))))                      

# # Add a numeric alternative column to your dataset
database$choice <- as.integer(database$choice)
database <- database[order(database$fished_vessel_anon, database$fished_haul_anon), ]





# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(asc_sfa_nanc         = -3,
              asc_laa_nanc         = -3,
              asc_laa_cmck         = -3,
              asc_laa_msqd         = -3,
              asc_laa_ytna         = -3,
              asc_mna_msqd         = -3,
              asc_sba_msqd         = -3,
              asc_laa_btna         = -3,
              asc_sfa_msqd         = -3,
              asc_mna_psdn         = -3,
              asc_sba_cmck         = -3,
              asc_mra_msqd         = -3,
              asc_laa_psdn         = -3,
              asc_mna_nanc         = -3,
              asc_no_participation = 0,
              B_mean_avail         = 1.1,
              B_mean_price         = 0.3,
              lambda_part          = 1, 
              lambda_cmck          = 0.5,
              lambda_msqd          = 0.5,
              lambda_psdn          = 0.5,
              lambda_nanc          = 0.5,
              lambda_tuna          = 0.5)

# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation")


### Read in starting values for at least some parameters from existing model output file
# apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"C:/GitHub/EconAnalysis/Participation/R/output/MNL_SP_covariates",overwriteFixed=FALSE)


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
# 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["sfa_nanc"]]         = asc_sfa_nanc + B_mean_avail * mean_avail_sfa_nanc + B_mean_price * mean_price_sfa_nanc        
  V[["laa_nanc"]]         = asc_laa_nanc + B_mean_avail * mean_avail_laa_nanc + B_mean_price * mean_price_laa_nanc        
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck        
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd        
  V[["laa_ytna"]]         = asc_laa_ytna + B_mean_avail * mean_avail_laa_ytna + B_mean_price * mean_price_laa_ytna        
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd        
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd        
  V[["laa_btna"]]         = asc_laa_btna + B_mean_avail * mean_avail_laa_btna + B_mean_price * mean_price_laa_btna        
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd        
  V[["mna_psdn"]]         = asc_mna_psdn + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn        
  V[["sba_cmck"]]         = asc_sba_cmck + B_mean_avail * mean_avail_sba_cmck + B_mean_price * mean_price_sba_cmck        
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd        
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn        
  V[["mna_nanc"]]         = asc_mna_nanc + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc        
  V[["no_participation"]] = asc_no_participation
  
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

  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
      sfa_nanc = 1, laa_nanc = 2, laa_cmck = 3, laa_msqd = 4, laa_ytna = 5, mna_msqd = 6, sba_msqd = 7, laa_btna = 8, 
      sfa_msqd = 9, mna_psdn = 10, sba_cmck = 11, mra_msqd = 12, laa_psdn = 13, mna_nanc = 14, no_participation = 15),
    avail = list(
      sfa_nanc = 1, laa_nanc = 1, laa_cmck = 1, laa_msqd = 1, laa_ytna = 1, mna_msqd = 1, sba_msqd = 1, laa_btna = 1,                      
      sfa_msqd = 1, mna_psdn = 1, sba_cmck = 1, mra_msqd = 1, laa_psdn = 1, mna_nanc = 1, no_participation = 1),
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # ### Take product across observation for same individual
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model,modelOutput_settings = list(printT1=1))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model,saveOutput_settings = list(printT1=1))

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST TWO-LEVEL NL MODEL                         ----
# ----------------------------------------------------------------- #

apollo_lrTest("NL_two_levels", model)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()