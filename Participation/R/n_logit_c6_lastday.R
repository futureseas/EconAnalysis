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
  modelName       = "NL_participation_model_c6_last_day",
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
# data <- read.dta13(paste0(google_dir, "Data/Anonymised data/part_model_c6.dta"))
# saveRDS(data, paste0(google_dir, "Data/Anonymised data/part_model_c6.rds"))

long_data <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c6.rds")) %>%
  dplyr::select("fished_haul_anon", "fished_vessel_anon", "selection", "fished",
                "fished_vessel_anon", "mean_avail", "mean_price_3", 
                "wind_max_220_mh", "dist_to_cog", "dist_port_to_catch_area_zero",
                "psdnclosure", "msqdclosure", "waclosure", "dcrbclosurewad", 
                "dummy_last_day", "unem_rate", 
                "d_d", "d_cd", "weekend") 

long_data$selection <- tolower(gsub("-", "_", long_data$selection))
long_data %>% dplyr::group_by(selection) %>%
  dplyr::summarise(n_fished = sum(fished))

database <- long_data %>%
  pivot_wider(
    names_from = selection, # Unique values for alternatives
    values_from = c(fished, mean_avail, mean_price_3, wind_max_220_mh, dist_to_cog, 
                    dist_port_to_catch_area_zero, dummy_last_day, 
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
                   ifelse(database$fished_no_participation == 1, 11, NA)))))))))))

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

apollo_beta=c(B_mean_avail                   = 2.8,
              B_wind_max_220_mh              = -0.05,
              B_dist_to_cog                  = -0.005,
              B_dist_port_to_catch_area_zero = -0.02,
              B_dummy_last_day               = 2,
              B_unem_rate                   = 0.4,
              B_d_d                          = -1.15,
              c_wa                           = -2, 
              c_dcrb                         = -2,
              c_psdn                         = -2,
              B_mean_price_part              = 3,
              B_mean_price_crab              = 3,
              B_mean_price_slmn              = 3,
              w_psdn                         = -1, 
              w_nanc                         = -1,
              w_dcrb                         = -1,
              w_slmn                         = -1,
              w_cmck                         = -1,
              asc_cba_psdn                   = -5,
              asc_clo_psdn                   = -5,
              asc_clw_psdn                   = -5,
              asc_cwa_psdn                   = -5,
              asc_clo_nanc                   = -5,
              asc_clw_nanc                   = -5,
              asc_cwa_nanc                   = -5,
              asc_clo_cmck                   = -5,
              asc_cwa_dcrb                   = -5,
              asc_nps_sock                   = -5,
              lambda_part                    = 0.5,
              lambda_nanc                    = 0.5,
              lambda_psdn                    = 0.5,
              asc_no_participation           = 0,
              w_nopart                       = 0,
              B_mean_price_no_part           = 0,
              B_d_cd                         = -4,
              c_no_participation             = 0)


# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_no_participation", "w_nopart", "B_mean_price_no_part", "c_no_participation")


# # # ### Read in starting values for at least some parameters from existing model output file
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"NL_participation_model_c6_v2",overwriteFixed=FALSE)


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
  V[["cba_psdn"]]         = asc_cba_psdn + B_mean_avail * mean_avail_cba_psdn + B_wind_max_220_mh * wind_max_220_mh_cba_psdn + B_dist_to_cog * dist_to_cog_cba_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_psdn + B_dummy_last_day * dummy_last_day_cba_psdn + B_unem_rate * unem_rate_or + B_d_d * d_d_cba_psdn                                     + c_psdn * psdnclosure + B_mean_price_part * mean_price_3_cba_psdn + w_psdn * weekend
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_avail * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_last_day * dummy_last_day_clo_psdn + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_psdn                                     + c_psdn * psdnclosure + B_mean_price_part * mean_price_3_clo_psdn + w_psdn * weekend
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_avail * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_last_day * dummy_last_day_clw_psdn + B_unem_rate * unem_rate_wa + B_d_d * d_d_clw_psdn  + c_wa * waclosure_clw_psdn        + c_psdn * psdnclosure + B_mean_price_part * mean_price_3_clw_psdn + w_psdn * weekend
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_avail * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_last_day * dummy_last_day_cwa_psdn + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_psdn  + c_wa * waclosure_cwa_psdn        + c_psdn * psdnclosure + B_mean_price_part * mean_price_3_cwa_psdn + w_psdn * weekend
  V[["clo_nanc"]]         = asc_clo_nanc + B_mean_avail * mean_avail_clo_nanc + B_wind_max_220_mh * wind_max_220_mh_clo_nanc + B_dist_to_cog * dist_to_cog_clo_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_nanc + B_dummy_last_day * dummy_last_day_clo_nanc + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_nanc                                                            + B_mean_price_part * mean_price_3_clo_nanc + w_nanc * weekend
  V[["clw_nanc"]]         = asc_clw_nanc + B_mean_avail * mean_avail_clw_nanc + B_wind_max_220_mh * wind_max_220_mh_clw_nanc + B_dist_to_cog * dist_to_cog_clw_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_nanc + B_dummy_last_day * dummy_last_day_clw_nanc + B_unem_rate * unem_rate_wa + B_d_d * d_d_clw_nanc                                                            + B_mean_price_part * mean_price_3_clw_nanc + w_nanc * weekend
  V[["cwa_nanc"]]         = asc_cwa_nanc + B_mean_avail * mean_avail_cwa_nanc + B_wind_max_220_mh * wind_max_220_mh_cwa_nanc + B_dist_to_cog * dist_to_cog_cwa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_nanc + B_dummy_last_day * dummy_last_day_cwa_nanc + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_nanc                                                            + B_mean_price_part * mean_price_3_cwa_nanc + w_nanc * weekend
  V[["clo_cmck"]]         = asc_clo_cmck + B_mean_avail * mean_avail_clo_cmck + B_wind_max_220_mh * wind_max_220_mh_clo_cmck + B_dist_to_cog * dist_to_cog_clo_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_cmck + B_dummy_last_day * dummy_last_day_clo_cmck + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_cmck                                                            + B_mean_price_part * mean_price_3_clo_cmck + w_cmck * weekend
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_avail * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_last_day * dummy_last_day_cwa_dcrb + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_dcrb  + c_dcrb * dcrbclosurewad_cwa_dcrb                        + B_mean_price_crab * mean_price_3_cwa_dcrb + w_dcrb * weekend + c_dcrb * dcrbclosurewad_cwa_dcrb
  V[["nps_sock"]]         = asc_nps_sock + B_mean_avail * mean_avail_nps_sock + B_wind_max_220_mh * wind_max_220_mh_nps_sock + B_dist_to_cog * dist_to_cog_nps_sock + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_nps_sock + B_dummy_last_day * dummy_last_day_nps_sock + B_unem_rate * unem_rate_wa + B_d_d * d_d_nps_sock                                                            + B_mean_price_slmn * mean_price_3_nps_sock + w_slmn * weekend + B_d_cd * d_cd_nps_sock                           
  V[["no_participation"]] = asc_no_participation + w_nopart * weekend + B_mean_price_no_part * mean_price_3_no_participation + c_no_participation * psdnclosure
  
  
  ## Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      nanc = lambda_nanc, psdn = lambda_psdn)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part", "nps_sock", "cwa_dcrb")
  nlStructure[["part"]] = c("nanc", "psdn", "clo_cmck" )
  nlStructure[["nanc"]] = c("clo_nanc", "clw_nanc", "cwa_nanc")
  nlStructure[["psdn"]] = c("cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn")   ### Define settings for NL model
  
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
    avail        = 1,
    choiceVar    = choice,
    utilities    = V
    ,
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
                        apollo_probabilities, apollo_inputs
                        ,
                        estimate_settings=list(constraints=c("lambda_part < 1 + 1e-10",
                                                             "lambda_part - lambda_nanc > -1e-10",
                                                             "lambda_part - lambda_psdn > -1e-10",
                                                             "lambda_nanc > 0",
                                                             "lambda_psdn > 0"))
)


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

# model2 <- readRDS("C:/GitHub/EconAnalysis/Participation/R/output/NL_participation_model_c6_model.rds")
# apollo_lrTest(model, model2)






