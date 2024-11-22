# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
PC = "work" ### Where I am working?




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
  modelName       = "NL_participation_model_c7",
  modelDescr      = "Participation, location and target species decisions",
  indivID         = "fished_vessel_anon", 
  outputDirectory = "output",
  panelData       = TRUE,
  nCores          = n_cores,
  workInLogs      = TRUE
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


## Read database
library(tidyr)
library(dplyr)
# library(readstata13)
# data <- read.dta13(paste0(google_dir, "Data/Anonymised data/part_model_c7.dta"))
# saveRDS(data, paste0(google_dir, "Data/Anonymised data/part_model_c7.rds"))

long_data <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c7.rds")) %>%
  dplyr::select("fished_haul_anon", "fished_vessel_anon", "selection", "fished",
                "fished_vessel_anon", "mean_avail", "mean_price", 
                "wind_max_220_mh", "dist_to_cog", "dist_port_to_catch_area_zero",
                "psdnclosure", "msqdclosure", "waclosure", "dcrbclosurewad", 
                "dummy_prev_days", "dummy_prev_year_days", "unem_rate", 
                "d_d", "weekend") 

long_data$selection <- tolower(gsub("-", "_", long_data$selection))
long_data %>% dplyr::group_by(selection) %>%
  dplyr::summarise(n_fished = sum(fished))

database <- long_data %>%
  pivot_wider(
    names_from = selection, # Unique values for alternatives
    values_from = c(fished, mean_avail, mean_price, wind_max_220_mh, dist_to_cog, 
                    dist_port_to_catch_area_zero, dummy_prev_days, dummy_prev_year_days, 
                    dcrbclosurewad, waclosure, unem_rate, d_d)) %>%
  dplyr::mutate(unem_rate = unem_rate_laa_psdn)

database$choice <- 
 ifelse(database$fished_laa_cmck == 1, 1,
 ifelse(database$fished_mna_cmck == 1, 2,
 ifelse(database$fished_laa_jmck == 1, 3,
 ifelse(database$fished_mna_jmck == 1, 4,
 ifelse(database$fished_laa_msqd == 1, 5,
 ifelse(database$fished_mra_msqd == 1, 6,
 ifelse(database$fished_sba_msqd == 1, 7,
 ifelse(database$fished_sfa_msqd == 1, 8,
 ifelse(database$fished_mna_msqd == 1, 9,
 ifelse(database$fished_laa_psdn == 1, 10,
 ifelse(database$fished_mna_psdn == 1, 11,
 ifelse(database$fished_mna_nanc == 1, 12,
 ifelse(database$fished_sba_nanc == 1, 13,
 ifelse(database$fished_sda_nanc == 1, 14,
 ifelse(database$fished_no_participation == 1, 15, NA)))))))))))))))

unique(database$choice)


### Add a numeric alternative column to your dataset
database$choice <- as.integer(database$choice)
database <- database[order(database$fished_vessel_anon, database$fished_haul_anon), ]


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(B_mean_avail                   = 1,
              B_mean_price                   = 1.5,
              B_wind_max_220_mh              = -0.05,
              B_dist_to_cog                  = -0.0025,
              B_dist_port_to_catch_area_zero = -0.01,
              B_dummy_prev_days              = 2.8,
              B_dummy_prev_year_days         = 0.15,
              c_msqd                         = -5,
              c_psdn                         = -1.5,
              B_d_d                          = -1,
              B_unem_rate_part               = 0.15,
              B_unem_rate_nopart             = 0,
              w_cmck                         = -1, 
              w_jmck                         = -1,
              w_msqd                         = -1,
              w_psdn                         = -1,
              w_nanc                         = -1,
              asc_laa_cmck                   = -5,
              asc_mna_cmck                   = -5,
              asc_laa_jmck                   = -5,
              asc_mna_jmck                   = -5,
              asc_laa_msqd                   = -5,
              asc_mra_msqd                   = -5,
              asc_sba_msqd                   = -5,
              asc_sfa_msqd                   = -5,
              asc_mna_msqd                   = -5,
              asc_laa_psdn                   = -5,
              asc_mna_psdn                   = -5,
              asc_mna_nanc                   = -5,
              asc_sba_nanc                   = -5,
              asc_sda_nanc                   = -5,
              lambda_part                    = 0.5, 
              lambda_laa                     = 0.5,
              lambda_sba                     = 0.5,
              lambda_mna                     = 0.5,
              asc_no_participation           = 0,
              w_nopart                       = 0)




# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation", "w_nopart", "B_unem_rate_nopart")


# # # ### Read in starting values for at least some parameters from existing model output file
# apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"NL_participation_model_c7",overwriteFixed=FALSE)


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
  V[["laa_cmck"]]         = asc_laa_cmck + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_dist_to_cog * dist_to_cog_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_dummy_prev_days * dummy_prev_days_laa_cmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_cmck  + B_unem_rate_part * unem_rate + B_d_d * d_d_laa_cmck + w_cmck * weekend
  V[["mna_cmck"]]         = asc_mna_cmck + B_mean_avail * mean_avail_mna_cmck + B_mean_price * mean_price_mna_cmck + B_wind_max_220_mh * wind_max_220_mh_mna_cmck + B_dist_to_cog * dist_to_cog_mna_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_cmck + B_dummy_prev_days * dummy_prev_days_mna_cmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_cmck  + B_unem_rate_part * unem_rate + B_d_d * d_d_mna_cmck + w_cmck * weekend
  V[["laa_jmck"]]         = asc_laa_jmck + B_mean_avail * mean_avail_laa_jmck + B_mean_price * mean_price_laa_jmck + B_wind_max_220_mh * wind_max_220_mh_laa_jmck + B_dist_to_cog * dist_to_cog_laa_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_jmck + B_dummy_prev_days * dummy_prev_days_laa_jmck + B_dummy_prev_year_days * dummy_prev_year_days_laa_jmck  + B_unem_rate_part * unem_rate + B_d_d * d_d_laa_jmck + w_jmck * weekend
  V[["mna_jmck"]]         = asc_mna_jmck + B_mean_avail * mean_avail_mna_jmck + B_mean_price * mean_price_mna_jmck + B_wind_max_220_mh * wind_max_220_mh_mna_jmck + B_dist_to_cog * dist_to_cog_mna_jmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_jmck + B_dummy_prev_days * dummy_prev_days_mna_jmck + B_dummy_prev_year_days * dummy_prev_year_days_mna_jmck  + B_unem_rate_part * unem_rate + B_d_d * d_d_mna_jmck + w_jmck * weekend
  V[["laa_msqd"]]         = asc_laa_msqd + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_dist_to_cog * dist_to_cog_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_dummy_prev_days * dummy_prev_days_laa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_laa_msqd  + B_unem_rate_part * unem_rate + B_d_d * d_d_laa_msqd + w_msqd * weekend + c_msqd * msqdclosure
  V[["mra_msqd"]]         = asc_mra_msqd + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_dist_to_cog * dist_to_cog_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_dummy_prev_days * dummy_prev_days_mra_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mra_msqd  + B_unem_rate_part * unem_rate + B_d_d * d_d_mra_msqd + w_msqd * weekend + c_msqd * msqdclosure
  V[["sba_msqd"]]         = asc_sba_msqd + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_dist_to_cog * dist_to_cog_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_dummy_prev_days * dummy_prev_days_sba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sba_msqd  + B_unem_rate_part * unem_rate + B_d_d * d_d_sba_msqd + w_msqd * weekend + c_msqd * msqdclosure
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd  + B_unem_rate_part * unem_rate + B_d_d * d_d_sfa_msqd + w_msqd * weekend + c_msqd * msqdclosure
  V[["mna_msqd"]]         = asc_mna_msqd + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_dist_to_cog * dist_to_cog_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_dummy_prev_days * dummy_prev_days_mna_msqd + B_dummy_prev_year_days * dummy_prev_year_days_mna_msqd  + B_unem_rate_part * unem_rate + B_d_d * d_d_mna_msqd + w_msqd * weekend + c_msqd * msqdclosure
  V[["laa_psdn"]]         = asc_laa_psdn + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_dist_to_cog * dist_to_cog_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_dummy_prev_days * dummy_prev_days_laa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_laa_psdn  + B_unem_rate_part * unem_rate + B_d_d * d_d_laa_psdn + w_psdn * weekend + c_psdn * psdnclosure                            
  V[["mna_psdn"]]         = asc_mna_psdn + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_dist_to_cog * dist_to_cog_mna_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_dummy_prev_days * dummy_prev_days_mna_psdn + B_dummy_prev_year_days * dummy_prev_year_days_mna_psdn  + B_unem_rate_part * unem_rate + B_d_d * d_d_mna_psdn + w_psdn * weekend + c_psdn * psdnclosure
  V[["mna_nanc"]]         = asc_mna_nanc + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_dist_to_cog * dist_to_cog_mna_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_dummy_prev_days * dummy_prev_days_mna_nanc + B_dummy_prev_year_days * dummy_prev_year_days_mna_nanc  + B_unem_rate_part * unem_rate + B_d_d * d_d_mna_nanc + w_nanc * weekend
  V[["sba_nanc"]]         = asc_sba_nanc + B_mean_avail * mean_avail_sba_nanc + B_mean_price * mean_price_sba_nanc + B_wind_max_220_mh * wind_max_220_mh_sba_nanc + B_dist_to_cog * dist_to_cog_sba_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_nanc + B_dummy_prev_days * dummy_prev_days_sba_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sba_nanc  + B_unem_rate_part * unem_rate + B_d_d * d_d_sba_nanc + w_nanc * weekend
  V[["sda_nanc"]]         = asc_sda_nanc + B_mean_avail * mean_avail_sda_nanc + B_mean_price * mean_price_sda_nanc + B_wind_max_220_mh * wind_max_220_mh_sda_nanc + B_dist_to_cog * dist_to_cog_sda_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sda_nanc + B_dummy_prev_days * dummy_prev_days_sda_nanc + B_dummy_prev_year_days * dummy_prev_year_days_sda_nanc  + B_unem_rate_part * unem_rate + B_d_d * d_d_sda_nanc + w_nanc * weekend
  V[["no_participation"]] = asc_no_participation + w_nopart * weekend + B_unem_rate_nopart * unem_rate
  
  
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
                                                             "lambda_part - lambda_laa > -1e-10",
                                                             "lambda_part - lambda_sba > -1e-10",
                                                             "lambda_part - lambda_mna > -1e-10",
                                                             "lambda_laa > 0",
                                                             "lambda_mna > 0",
                                                             "lambda_sba > 0")))


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








