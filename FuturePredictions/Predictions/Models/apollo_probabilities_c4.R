
apollo_fixed <- c("asc_no_participation", "B_unem_rate_nopart", "w_nopart")

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="prediction"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["sfa_nanc"]]         = asc_sfa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_sfa_nanc + B_mean_price * mean_price_sfa_nanc + B_wind_max_220_mh * wind_max_220_mh_sfa_nanc + B_d_d * d_d_sfa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_nanc                           
  V[["laa_nanc"]]         = asc_laa_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_laa_nanc + B_mean_price * mean_price_laa_nanc + B_wind_max_220_mh * wind_max_220_mh_laa_nanc + B_d_d * d_d_laa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_nanc                           
  V[["laa_cmck"]]         = asc_laa_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_laa_cmck + B_mean_price * mean_price_laa_cmck + B_wind_max_220_mh * wind_max_220_mh_laa_cmck + B_d_d * d_d_laa_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_cmck                           
  V[["laa_msqd"]]         = asc_laa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_laa_msqd + B_mean_price * mean_price_laa_msqd + B_wind_max_220_mh * wind_max_220_mh_laa_msqd + B_d_d * d_d_laa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_msqd                           
  V[["laa_ytna"]]         = asc_laa_ytna + w_ytna * weekend                        + B_mean_avail * mean_avail_laa_ytna + B_mean_price * mean_price_laa_ytna + B_wind_max_220_mh * wind_max_220_mh_laa_ytna + B_d_d * d_d_laa_ytna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_ytna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_ytna + B_d_cd * d_cd_laa_ytna  
  V[["mna_msqd"]]         = asc_mna_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mna_msqd + B_mean_price * mean_price_mna_msqd + B_wind_max_220_mh * wind_max_220_mh_mna_msqd + B_d_d * d_d_mna_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_msqd                           
  V[["sba_msqd"]]         = asc_sba_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sba_msqd + B_mean_price * mean_price_sba_msqd + B_wind_max_220_mh * wind_max_220_mh_sba_msqd + B_d_d * d_d_sba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_msqd                            
  V[["laa_btna"]]         = asc_laa_btna + w_btna * weekend + c_btna * btnaclosure + B_mean_avail * mean_avail_laa_btna + B_mean_price * mean_price_laa_btna + B_wind_max_220_mh * wind_max_220_mh_laa_btna + B_d_d * d_d_laa_btna + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_btna + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_btna + B_d_cd * d_cd_laa_btna   
  V[["sfa_msqd"]]         = asc_sfa_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_sfa_msqd + B_mean_price * mean_price_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_d_d * d_d_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sfa_msqd                            
  V[["mna_psdn"]]         = asc_mna_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_mna_psdn + B_mean_price * mean_price_mna_psdn + B_wind_max_220_mh * wind_max_220_mh_mna_psdn + B_d_d * d_d_mna_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_psdn                            
  V[["sba_cmck"]]         = asc_sba_cmck + w_cmck * weekend                        + B_mean_avail * mean_avail_sba_cmck + B_mean_price * mean_price_sba_cmck + B_wind_max_220_mh * wind_max_220_mh_sba_cmck + B_d_d * d_d_sba_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sba_cmck + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_sba_cmck                            
  V[["mra_msqd"]]         = asc_mra_msqd + w_msqd * weekend                        + B_mean_avail * mean_avail_mra_msqd + B_mean_price * mean_price_mra_msqd + B_wind_max_220_mh * wind_max_220_mh_mra_msqd + B_d_d * d_d_mra_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mra_msqd + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mra_msqd                            
  V[["laa_psdn"]]         = asc_laa_psdn + w_psdn * weekend + c_psdn * psdnclosure + B_mean_avail * mean_avail_laa_psdn + B_mean_price * mean_price_laa_psdn + B_wind_max_220_mh * wind_max_220_mh_laa_psdn + B_d_d * d_d_laa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_laa_psdn + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_laa_psdn                            
  V[["mna_nanc"]]         = asc_mna_nanc + w_nanc * weekend                        + B_mean_avail * mean_avail_mna_nanc + B_mean_price * mean_price_mna_nanc + B_wind_max_220_mh * wind_max_220_mh_mna_nanc + B_d_d * d_d_mna_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_mna_nanc + B_unem_rate_part * unem_rate + B_dist_to_cog * dist_to_cog_mna_nanc                            
  V[["no_participation"]] = asc_no_participation + w_nopart * weekend + B_unem_rate_nopart * unem_rate
  
  
  ### Specify nests for NL model
  nlNests      = list(root=1, part_cps = lambda_part_cps,
                      cmck = lambda_cmck, msqd = lambda_msqd, psdn = lambda_psdn, 
                      nanc = lambda_nanc, part_tuna = lambda_part_tuna)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part_cps", "part_tuna")
  nlStructure[["part_cps"]] = c("cmck", "msqd", "psdn", "nanc")
  nlStructure[["cmck"]] = c("laa_cmck", "sba_cmck")
  nlStructure[["msqd"]] = c("laa_msqd", "mna_msqd", "mra_msqd", "sba_msqd", "sfa_msqd")
  nlStructure[["psdn"]] = c("laa_psdn", "mna_psdn")
  nlStructure[["nanc"]] = c("laa_nanc", "mna_nanc", "sfa_nanc")
  nlStructure[["part_tuna"]] = c("laa_ytna", "laa_btna")
  
  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
      sfa_nanc = 1, laa_nanc = 2, laa_cmck = 3, laa_msqd = 4, laa_ytna = 5, mna_msqd = 6, sba_msqd = 7, laa_btna = 8, 
      sfa_msqd = 9, mna_psdn = 10, sba_cmck = 11, mra_msqd = 12, laa_psdn = 13, mna_nanc = 14, no_participation = 15),
    avail        = 1,
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
