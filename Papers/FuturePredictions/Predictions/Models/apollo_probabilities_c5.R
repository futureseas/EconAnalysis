apollo_fixed = c("asc_no_participation", "w_nopart", "B_mean_price_no_part")

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="prediction"){
  
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
  V[["npa_msqd"]]         = asc_npa_msqd + B_mean_price_part * mean_price_3_npa_msqd + B_mean_avail * mean_avail_npa_msqd + B_wind_max_220_mh * wind_max_220_mh_npa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_npa_msqd + B_dummy_prev_days * dummy_prev_days_npa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_npa_msqd + B_dist_to_cog * dist_to_cog_npa_msqd + B_unem_rate * unem_rate_or + c_msqd * msqdclosure             + B_d_d * d_d_npa_msqd + B_d_cd * d_cd_npa_msqd + w_msqd * weekend    
  V[["sfa_msqd"]]         = asc_sfa_msqd + B_mean_price_part * mean_price_3_sfa_msqd + B_mean_avail * mean_avail_sfa_msqd + B_wind_max_220_mh * wind_max_220_mh_sfa_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_sfa_msqd + B_dummy_prev_days * dummy_prev_days_sfa_msqd + B_dummy_prev_year_days * dummy_prev_year_days_sfa_msqd + B_dist_to_cog * dist_to_cog_sfa_msqd + B_unem_rate * unem_rate_ca + c_msqd * msqdclosure             + B_d_d * d_d_sfa_msqd + B_d_cd * d_cd_sfa_msqd + w_msqd * weekend                            
  V[["cba_msqd"]]         = asc_cba_msqd + B_mean_price_part * mean_price_3_cba_msqd + B_mean_avail * mean_avail_cba_msqd + B_wind_max_220_mh * wind_max_220_mh_cba_msqd + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_msqd + B_dummy_prev_days * dummy_prev_days_cba_msqd + B_dummy_prev_year_days * dummy_prev_year_days_cba_msqd + B_dist_to_cog * dist_to_cog_cba_msqd + B_unem_rate * unem_rate_or + c_msqd * msqdclosure             + B_d_d * d_d_cba_msqd + B_d_cd * d_cd_cba_msqd + w_msqd * weekend                             
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





