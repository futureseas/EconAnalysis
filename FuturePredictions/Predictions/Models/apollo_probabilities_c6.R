apollo_fixed = c("asc_no_participation", "w_nopart", "B_mean_price_no_part", "c_no_participation")

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="prediction"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["cba_psdn"]]         = asc_cba_psdn + B_mean_avail * mean_avail_cba_psdn + B_wind_max_220_mh * wind_max_220_mh_cba_psdn + B_dist_to_cog * dist_to_cog_cba_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cba_psdn + B_dummy_prev_days * dummy_prev_days_cba_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cba_psdn  + B_unem_rate * unem_rate_or + B_d_d * d_d_cba_psdn  + B_mean_price_part * mean_price_3_cba_psdn + w_psdn * weekend + B_d_cd * d_cd_cba_psdn                             + c_psdn * psdnclosure 
  V[["clo_psdn"]]         = asc_clo_psdn + B_mean_avail * mean_avail_clo_psdn + B_wind_max_220_mh * wind_max_220_mh_clo_psdn + B_dist_to_cog * dist_to_cog_clo_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_psdn + B_dummy_prev_days * dummy_prev_days_clo_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clo_psdn  + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_psdn  + B_mean_price_part * mean_price_3_clo_psdn + w_psdn * weekend + B_d_cd * d_cd_clo_psdn                             + c_psdn * psdnclosure 
  V[["clw_psdn"]]         = asc_clw_psdn + B_mean_avail * mean_avail_clw_psdn + B_wind_max_220_mh * wind_max_220_mh_clw_psdn + B_dist_to_cog * dist_to_cog_clw_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_psdn + B_dummy_prev_days * dummy_prev_days_clw_psdn + B_dummy_prev_year_days * dummy_prev_year_days_clw_psdn  + B_unem_rate * unem_rate_wa + B_d_d * d_d_clw_psdn  + B_mean_price_part * mean_price_3_clw_psdn + w_psdn * weekend + B_d_cd * d_cd_clw_psdn + c_wa * waclosure_clw_psdn + c_psdn * psdnclosure 
  V[["cwa_psdn"]]         = asc_cwa_psdn + B_mean_avail * mean_avail_cwa_psdn + B_wind_max_220_mh * wind_max_220_mh_cwa_psdn + B_dist_to_cog * dist_to_cog_cwa_psdn + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_psdn + B_dummy_prev_days * dummy_prev_days_cwa_psdn + B_dummy_prev_year_days * dummy_prev_year_days_cwa_psdn  + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_psdn  + B_mean_price_part * mean_price_3_cwa_psdn + w_psdn * weekend + B_d_cd * d_cd_cwa_psdn + c_wa * waclosure_cwa_psdn + c_psdn * psdnclosure 
  V[["clo_nanc"]]         = asc_clo_nanc + B_mean_avail * mean_avail_clo_nanc + B_wind_max_220_mh * wind_max_220_mh_clo_nanc + B_dist_to_cog * dist_to_cog_clo_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_nanc + B_dummy_prev_days * dummy_prev_days_clo_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clo_nanc  + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_nanc  + B_mean_price_part * mean_price_3_clo_nanc + w_nanc * weekend + B_d_cd * d_cd_clo_nanc                                                           
  V[["clw_nanc"]]         = asc_clw_nanc + B_mean_avail * mean_avail_clw_nanc + B_wind_max_220_mh * wind_max_220_mh_clw_nanc + B_dist_to_cog * dist_to_cog_clw_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clw_nanc + B_dummy_prev_days * dummy_prev_days_clw_nanc + B_dummy_prev_year_days * dummy_prev_year_days_clw_nanc  + B_unem_rate * unem_rate_wa + B_d_d * d_d_clw_nanc  + B_mean_price_part * mean_price_3_clw_nanc + w_nanc * weekend + B_d_cd * d_cd_clw_nanc                                                           
  V[["cwa_nanc"]]         = asc_cwa_nanc + B_mean_avail * mean_avail_cwa_nanc + B_wind_max_220_mh * wind_max_220_mh_cwa_nanc + B_dist_to_cog * dist_to_cog_cwa_nanc + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_nanc + B_dummy_prev_days * dummy_prev_days_cwa_nanc + B_dummy_prev_year_days * dummy_prev_year_days_cwa_nanc  + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_nanc  + B_mean_price_part * mean_price_3_cwa_nanc + w_nanc * weekend + B_d_cd * d_cd_cwa_nanc                                                           
  V[["clo_cmck"]]         = asc_clo_cmck + B_mean_avail * mean_avail_clo_cmck + B_wind_max_220_mh * wind_max_220_mh_clo_cmck + B_dist_to_cog * dist_to_cog_clo_cmck + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_clo_cmck + B_dummy_prev_days * dummy_prev_days_clo_cmck + B_dummy_prev_year_days * dummy_prev_year_days_clo_cmck  + B_unem_rate * unem_rate_or + B_d_d * d_d_clo_cmck  + B_mean_price_part * mean_price_3_clo_cmck + w_cmck * weekend + B_d_cd * d_cd_clo_cmck                                                           
  V[["cwa_dcrb"]]         = asc_cwa_dcrb + B_mean_avail * mean_avail_cwa_dcrb + B_wind_max_220_mh * wind_max_220_mh_cwa_dcrb + B_dist_to_cog * dist_to_cog_cwa_dcrb + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_cwa_dcrb + B_dummy_prev_days * dummy_prev_days_cwa_dcrb + B_dummy_prev_year_days * dummy_prev_year_days_cwa_dcrb  + B_unem_rate * unem_rate_wa + B_d_d * d_d_cwa_dcrb  + B_mean_price_crab * mean_price_3_cwa_dcrb + w_dcrb * weekend + B_d_cd * d_cd_cwa_dcrb + c_dcrb * dcrbclosurewad_cwa_dcrb
  V[["nps_sock"]]         = asc_nps_sock + B_mean_avail * mean_avail_nps_sock + B_wind_max_220_mh * wind_max_220_mh_nps_sock + B_dist_to_cog * dist_to_cog_nps_sock + B_dist_port_to_catch_area_zero * dist_port_to_catch_area_zero_nps_sock + B_dummy_prev_days * dummy_prev_days_nps_sock + B_dummy_prev_year_days * dummy_prev_year_days_nps_sock  + B_unem_rate * unem_rate_wa + B_d_d * d_d_nps_sock  + B_mean_price_slmn * mean_price_3_nps_sock + w_slmn * weekend + B_d_cd * d_cd_nps_sock                                                                                      
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