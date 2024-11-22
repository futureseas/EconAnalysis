# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
PC = "home" ### Where I am working?




if (PC == "home") {
  google_dir <- "H:/My Drive/"
  n_cores <- 4
} else if (PC == "work") {
  google_dir <- "G:/Mi unidad/"
  n_cores <- 14
}


### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()
setwd("C:/GitHub/EconAnalysis/Participation/R")


### Set core controls
apollo_control = list(
  modelName       = "NL_participation_model_c6",
  modelDescr      = "Participation, location and target species decisions",
  indivID         = "fished_vessel_anon", 
  outputDirectory = "output",
  panelData       = FALSE,
  nCores          = n_cores,
  workInLogs      = FALSE
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


## Read database
library(tidyr)
library(dplyr)
# library(readstata13)
# data <- read.dta13(paste0(google_dir, "Data/Anonymised data/part_model_c6.dta"))
# saveRDS(data, paste0(google_dir, "Data/Anonymised data/part_model_c6.rds"))

long_data <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c6.rds")) %>%
  dplyr::select("fished_haul_anon", "fished_vessel_anon", "selection", "fished",
                "fished_vessel_anon", "mean_avail", "mean_price_3", 
                "wind_max_220_mh", "dist_to_cog", "dist_port_to_catch_area_zero",
                "psdnclosure", "msqdclosure", "waclosure", "dcrbclosurewad", 
                "dummy_prev_days", "dummy_prev_year_days", "unem_rate", 
                "d_d", "d_cd", "weekend") 
  
long_data$selection <- tolower(gsub("-", "_", long_data$selection))
long_data %>% dplyr::group_by(selection) %>%
  dplyr::summarise(n_fished = sum(fished))



database <- long_data %>%
  pivot_wider(
    names_from = selection, # Unique values for alternatives
    values_from = c(fished, mean_avail, mean_price_3, wind_max_220_mh, dist_to_cog, 
                    dist_port_to_catch_area_zero, dummy_prev_days, dummy_prev_year_days, 
                    dcrbclosurewad, waclosure, unem_rate, d_d, d_cd)) %>%
  dplyr::mutate(unem_rate = (unem_rate_clo_psdn + unem_rate_clw_nanc) / 2) %>%
  dplyr::mutate(unem_rate_or = unem_rate_clo_psdn, unem_rate_wa = unem_rate_clw_nanc)

database$choice <- ifelse(database$fished_cba_psdn == 1, 1,
                   ifelse(database$fished_clo_psdn == 1, 2,        
                   ifelse(database$fished_clw_psdn == 1, 3,        
                   ifelse(database$fished_cwa_psdn == 1, 4,
                   ifelse(database$fished_clo_nanc == 1, 5,
                   ifelse(database$fished_clw_nanc == 1, 6,
                   ifelse(database$fished_cwa_nanc == 1, 7,
                   ifelse(database$fished_clo_cmck == 1, 8,
                   ifelse(database$fished_cwa_dcrb == 1, 9,
                   ifelse(database$fished_nps_sock == 1, 10,
                   ifelse(database$fished_no_participation == 1, 11, NA)
                   ))))))))))

unique(database$choice)

# ### Check variables 
df_selected <- database %>%
  dplyr::select(starts_with("d_cd_"))
  summary(df_selected)
  
  # d_cd_nps_sock

df_selected <- database %>%
  dplyr::select(starts_with("d_d_"))
  summary(df_selected)

  # all alternative
  
# df_selected <- database %>%
#   dplyr::select(starts_with("unem_rate_"))
#   summary(df_selected)


### Add a numeric alternative column to your dataset
database$choice <- as.integer(database$choice)
database <- database[order(database$fished_vessel_anon, database$fished_haul_anon), ]





# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(B_mean_avail                   = 2.3,
              B_wind_max_220_mh              = -0.07,
              B_dist_to_cog                  = -0.0008,
              B_dist_port_to_catch_area_zero = -0.0045,
              B_dummy_prev_days              = 2.5,
              B_dummy_prev_year_days         = 0.35,
              B_unem_rate                    = 0.4,
              B_d_d                          = -1.75,
              B_d_cd                         = -4,
              c_wa                           = -6, 
			  c_psdn_part                    = -2,
			  c_psdn_crab                    = -2,
			  c_psdn_slmn                    = -2,
              B_mean_price_part              = 3.75,
              B_mean_price_crab              = 0.1,
              B_mean_price_slmn              = 0.1,
			  c_dcrb                         = -2,
              asc_cba_psdn                   = -8,
              asc_clo_psdn                   = -8,
              asc_clw_psdn                   = -8,
              asc_cwa_psdn                   = -8,
              asc_clo_nanc                   = -8,
              asc_clw_nanc                   = -8,
              asc_cwa_nanc                   = -8,
              asc_clo_cmck                   = -8,
              asc_cwa_dcrb                   = -8,
              asc_nps_sock                   = -8,
              lambda_part                    = 0.6, 
              lambda_cmck                    = 0.6,
              lambda_msqd                    = 0.6,
              lambda_psdn                    = 0.6,
              lambda_part_crab               = 1,
              w_nanc                         = -2,
              w_msqd                         = -2,
              w_psdn                         = -2, 
              w_albc                         = -2,
              w_cmck                         = -2,
              w_dcrb                         = -2,
              c_msqd                         = -2,

              asc_no_participation           = 0,
              w_nopart                       = 0,
              B_mean_price_no_part           = 0)


# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation", "w_nopart", "B_mean_price_no_part")


# # ### Read in starting values for at least some parameters from existing model output file
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"NL_participation_model_c5",overwriteFixed=FALSE)


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
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_price_part * mean_price_3_mna_msqd + B_mean_avail * mean_avail_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_mna_msqd + B_d_cd * d_cd_mna_msqd + w_msqd * weekend                            
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_price_part * mean_price_3_sba_msqd + B_mean_avail * mean_avail_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_sba_msqd + B_d_cd * d_cd_sba_msqd + w_msqd * weekend                            
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_price_part * mean_price_3_mra_msqd + B_mean_avail * mean_avail_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_mra_msqd + B_d_cd * d_cd_mra_msqd + w_msqd * weekend                            
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_price_part * mean_price_3_laa_msqd + B_mean_avail * mean_avail_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_laa_msqd + B_d_cd * d_cd_laa_msqd + w_msqd * weekend                            
  V[["npa_msqd"]]         = asc_npa_msqd + B_mean_price_part * mean_price_3_npa_msqd + B_mean_avail * mean_avail_npa_msqd + B_wind_max_220_mh * wind_max_220_mh_npa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_npa_msqd + B_dummy_prev_days * dummy_prev_days_npa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_npa_msqd + B_dist_to_cog * dist_to_cog_npa_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_npa_msqd + B_d_cd * d_cd_npa_msqd + w_msqd * weekend    
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_price_part * mean_price_3_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_sfa_msqd + B_d_cd * d_cd_sfa_msqd + w_msqd * weekend                            
  V[["cba_msqd"]]         = asc_cba_msqd + B_mean_price_part * mean_price_3_cba_msqd + B_mean_avail * mean_avail_cba_msqd + B_wind_max_220_mh * wind_max_220_mh_cba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_msqd + B_dummy_prev_days * dummy_prev_days_cba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_cba_msqd + B_dist_to_cog * dist_to_cog_cba_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_cba_msqd + B_d_cd * d_cd_cba_msqd + w_msqd * weekend                             
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_price_part * mean_price_3_laa_psdn + B_mean_avail * mean_avail_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_unem_rate * unem_rate_ca + c_psdn * psdnclosure             + B_d_d * d_d_laa_psdn + B_d_cd * d_cd_laa_psdn + w_psdn * weekend     
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_price_part * mean_price_3_clo_psdn + B_mean_avail * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_unem_rate * unem_rate_or + c_psdn * psdnclosure             + B_d_d * d_d_clo_psdn + B_d_cd * d_cd_clo_psdn + w_psdn * weekend                             
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_price_part * mean_price_3_cwa_psdn + B_mean_avail * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_unem_rate * unem_rate_wa + c_psdn * psdnclosure             + B_d_d * d_d_cwa_psdn + B_d_cd * d_cd_cwa_psdn + w_psdn * weekend + c_wa * waclosure_cwa_psdn                             
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_price_part * mean_price_3_clw_psdn + B_mean_avail * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_unem_rate * unem_rate_wa + c_psdn * psdnclosure             + B_d_d * d_d_clw_psdn + B_d_cd * d_cd_clw_psdn + w_psdn * weekend + c_wa * waclosure_clw_psdn                             
  V[["sba_cmck"]]         = asc_sba_cmck + B_mean_price_part * mean_price_3_sba_cmck + B_mean_avail * mean_avail_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_dummy_prev_days * dummy_prev_days_sba_cmck + B_dummy_prev_year_days * dummy_prev_year_days_sba_cmck + B_dist_to_cog * dist_to_cog_sba_cmck + B_unem_rate * unem_rate_ca                                    + B_d_d * d_d_sba_cmck + B_d_cd * d_cd_sba_cmck + w_cmck * weekend                             
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_price_part * mean_price_3_laa_cmck + B_mean_avail * mean_avail_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_unem_rate * unem_rate_ca                                    + B_d_d * d_d_laa_cmck + B_d_cd * d_cd_laa_cmck + w_cmck * weekend                             
  V[["laa_nanc"]]         = asc_laa_nanc + B_mean_price_part * mean_price_3_laa_nanc + B_mean_avail * mean_avail_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_dummy_prev_days * dummy_prev_days_laa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_laa_nanc + B_dist_to_cog * dist_to_cog_laa_nanc + B_unem_rate * unem_rate_ca                                    + B_d_d * d_d_laa_nanc + B_d_cd * d_cd_laa_nanc + w_nanc * weekend                             
  V[["cwa_albc"]]         = asc_cwa_albc + B_mean_price_part * mean_price_3_cwa_albc + B_mean_avail * mean_avail_cwa_albc + B_wind_max_220_mh * wind_max_220_mh_cwa_albc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_albc + B_dummy_prev_days * dummy_prev_days_cwa_albc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_albc + B_dist_to_cog * dist_to_cog_cwa_albc + B_unem_rate * unem_rate_wa                                    + B_d_d * d_d_cwa_albc + B_d_cd * d_cd_cwa_albc + w_albc * weekend                             
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_price_crab * mean_price_3_cwa_dcrb + B_mean_avail * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_unem_rate * unem_rate_wa + c_dcrb * dcrbclosurewad_cwa_dcrb + B_d_d * d_d_cwa_dcrb + B_d_cd * d_cd_cwa_dcrb + w_dcrb * weekend                             
  V[["clw_dcrb"]]         = asc_clw_dcrb + B_mean_price_crab * mean_price_3_clw_dcrb + B_mean_avail * mean_avail_clw_dcrb + B_wind_max_220_mh * wind_max_220_mh_clw_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_dcrb + B_dummy_prev_days * dummy_prev_days_clw_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_clw_dcrb + B_dist_to_cog * dist_to_cog_clw_dcrb + B_unem_rate * unem_rate_wa + c_dcrb * dcrbclosurewad_clw_dcrb + B_d_d * d_d_clw_dcrb + B_d_cd * d_cd_clw_dcrb + w_dcrb * weekend                             
  V[["no_participation"]] = asc_no_participation + w_nopart * weekend + B_mean_price_no_part * mean_price_3_no_participation
  

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
 
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
	  mna_msqd = 1, sba_msqd = 2, mra_msqd = 3, laa_msqd = 4, npa_msqd = 5, sfa_msqd = 6,  
	  cba_msqd = 7, laa_psdn = 8, clo_psdn = 9, cwa_psdn = 10, clw_psdn = 11, sba_cmck = 12,
	  laa_cmck = 13, laa_nanc = 14, cwa_albc = 15, cwa_dcrb = 16, clw_dcrb = 17, no_participation = 18),
    avail        = 1,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(constraints=c("lambda_part < 1 + 1e-10",
                                                             "lambda_part_crab < 1 + 1e-10",
                                                             "lambda_part - lambda_cmck > -1e-10",
                                                             "lambda_part - lambda_msqd > -1e-10",
                                                             "lambda_part - lambda_psdn > -1e-10",
                                                             "lambda_cmck > 0",
                                                             "lambda_msqd > 0",
                                                             "lambda_psdn > 0",
                                                             "lambda_part_crab > 0")))

# model = apollo_estimate(apollo_beta, apollo_fixed,
#                         apollo_probabilities, apollo_inputs,
#                         estimate_settings=list(constraints=c("lambda_part < 1 + 1e-10",
#                                                              "lambda_cmck < 1 + 1e-10",
#                                                              "lambda_msqd < 1 + 1e-10",
#                                                              "lambda_psdn < 1 + 1e-10",
#                                                              "lambda_part_crab < 1 + 1e-10",
#                                                              "lambda_cmck > 0",
#                                                              "lambda_msqd > 0",
#                                                              "lambda_psdn > 0",
#                                                              "lambda_part_crab > 0",
#                                                              "lambda_part > 0")))

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


# ################################################################# #
##### Create Table                                               ####
# ################################################################# #

summary(model)








