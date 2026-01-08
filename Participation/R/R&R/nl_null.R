# ===================== Cluster c4 =====================

apollo_beta_null_c4 <- c(
  asc_sfa_nanc=-2.75, asc_laa_nanc=-2.75, asc_laa_cmck=-2.75, asc_laa_msqd=-2.75, asc_laa_ytna=-2.75, asc_mna_msqd=-2.75, asc_sba_msqd=-2.75, 
  asc_laa_btna=-2.75, asc_sfa_msqd=-2.75, asc_mna_psdn=-2.75, asc_sba_cmck=-2.75, asc_mra_msqd=-2.75, asc_laa_psdn=-2.75, asc_mna_nanc=-2.75,
  theta_part=0, theta_cmck=12, theta_msqd=0, theta_psdn=0, theta_nanc=0, theta_tuna=12,
  asc_no_participation=0)

apollo_fixed_null_c4 <- c("asc_no_participation", "theta_cmck","theta_tuna")

apollo_probabilities_null_c4 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  lambda_part <- 1/(1 + exp(-theta_part))
  lambda_cmck <- lambda_part * (1/(1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1/(1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1/(1 + exp(-theta_psdn)))
  lambda_nanc <- lambda_part * (1/(1 + exp(-theta_nanc)))
  lambda_tuna <- lambda_part * (1/(1 + exp(-theta_tuna)))
  P <- list(); V <- list()
  V[["sfa_nanc"]] <- asc_sfa_nanc 
  V[["laa_nanc"]] <- asc_laa_nanc 
  V[["laa_cmck"]] <- asc_laa_cmck 
  V[["laa_msqd"]] <- asc_laa_msqd 
  V[["laa_ytna"]] <- asc_laa_ytna 
  V[["mna_msqd"]] <- asc_mna_msqd 
  V[["sba_msqd"]] <- asc_sba_msqd 
  V[["laa_btna"]] <- asc_laa_btna 
  V[["sfa_msqd"]] <- asc_sfa_msqd 
  V[["mna_psdn"]] <- asc_mna_psdn 
  V[["sba_cmck"]] <- asc_sba_cmck 
  V[["mra_msqd"]] <- asc_mra_msqd 
  V[["laa_psdn"]] <- asc_laa_psdn 
  V[["mna_nanc"]] <- asc_mna_nanc 
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


# ===================== Cluster c5 (from n_logit_c5.R) =====================


apollo_beta_null_c5 =c(asc_mna_msqd                   = -8,
              asc_sba_msqd                   = -8,
              asc_mra_msqd                   = -8,
              asc_laa_msqd                   = -8,
              asc_npa_msqd                   = -8,
              asc_sfa_msqd                   = -8,
              asc_cba_msqd                   = -8,
              asc_laa_psdn                   = -8,
              asc_clo_psdn                   = -8,
              asc_cwa_psdn                   = -8,
              asc_clw_psdn                   = -8,
              asc_sba_cmck                   = -8,
              asc_laa_cmck                   = -8,
              asc_laa_nanc                   = -8,
              asc_cwa_albc                   = -8,
              asc_cwa_dcrb                   = -8,
              asc_clw_dcrb                   = -8,
              theta_part                     = 0,
              theta_cmck                     = 10,
              theta_msqd                     = 10,
              theta_psdn                     = 0,
              theta_part_crab                = 10,
              asc_no_participation           = 0 )

apollo_fixed_null_c5 = c("asc_no_participation", "theta_msqd", "theta_cmck", "theta_part_crab")


apollo_probabilities_null_c5 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_cmck <- lambda_part * (1 / (1 + exp(-theta_cmck)))
  lambda_msqd <- lambda_part * (1 / (1 + exp(-theta_msqd)))
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))
  lambda_part_crab <- 1 / (1 + exp(-theta_part_crab))
  
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["mna_msqd"]]         = asc_mna_msqd                           
  V[["sba_msqd"]]         = asc_sba_msqd                           
  V[["mra_msqd"]]         = asc_mra_msqd                           
  V[["laa_msqd"]]         = asc_laa_msqd                           
  V[["npa_msqd"]]         = asc_npa_msqd   
  V[["sfa_msqd"]]         = asc_sfa_msqd                           
  V[["cba_msqd"]]         = asc_cba_msqd                            
  V[["laa_psdn"]]         = asc_laa_psdn    
  V[["clo_psdn"]]         = asc_clo_psdn                            
  V[["cwa_psdn"]]         = asc_cwa_psdn                           
  V[["clw_psdn"]]         = asc_clw_psdn                           
  V[["sba_cmck"]]         = asc_sba_cmck                            
  V[["laa_cmck"]]         = asc_laa_cmck                            
  V[["laa_nanc"]]         = asc_laa_nanc         
  V[["cwa_albc"]]         = asc_cwa_albc                            
  V[["cwa_dcrb"]]         = asc_cwa_dcrb                            
  V[["clw_dcrb"]]         = asc_clw_dcrb                            
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


# ===================== Cluster c6 (from n_logit_c6.R) =====================

apollo_beta_null_c6 = c(asc_cba_psdn                   = -5,
              asc_clo_psdn                   = -5,
              asc_clw_psdn                   = -5,
              asc_cwa_psdn                   = -5,
              asc_clo_nanc                   = -5,
              asc_clw_nanc                   = -5,
              asc_cwa_nanc                   = -5,
              asc_clo_cmck                   = -5,
              asc_cwa_dcrb                   = -5,
              asc_nps_sock                   = -24,
              theta_part                    = 0.5,
              theta_nanc                    = 10,
              theta_psdn                    = 0.5,
              asc_no_participation           = 0)

apollo_fixed_null_c6 = c("asc_no_participation", "theta_nanc")

apollo_probabilities_null_c6 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  lambda_part <- 1 / (1 + exp(-theta_part))
  lambda_nanc <- lambda_part * (1 / (1 + exp(-theta_nanc)))
  lambda_psdn <- lambda_part * (1 / (1 + exp(-theta_psdn)))
  
  
  ### Create list of probabilities P
  P = list()
  
  # 
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["cba_psdn"]]         = asc_cba_psdn 
  V[["clo_psdn"]]         = asc_clo_psdn 
  V[["clw_psdn"]]         = asc_clw_psdn 
  V[["cwa_psdn"]]         = asc_cwa_psdn 
  V[["clo_nanc"]]         = asc_clo_nanc 
  V[["clw_nanc"]]         = asc_clw_nanc 
  V[["cwa_nanc"]]         = asc_cwa_nanc 
  V[["clo_cmck"]]         = asc_clo_cmck 
  V[["cwa_dcrb"]]         = asc_cwa_dcrb 
  V[["nps_sock"]]         = asc_nps_sock 
  V[["no_participation"]] = asc_no_participation
  
  
  ## Specify nests for NL model
  nlNests      = list(root=1, part = lambda_part,
                      nanc = lambda_nanc, psdn = lambda_psdn)   ### Specify tree structure for NL model
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]] = c("no_participation", "part", "nps_sock", "cwa_dcrb")
  nlStructure[["part"]] = c("nanc", "psdn", "clo_cmck" )
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
    nps_sock         = 1 - d_cd_nps_sock, 
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

# ===================== Cluster c7 (from n_logit_c7.R) =====================

apollo_beta_null_c7 = c(
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
              theta_part                    = 0.5, 
              theta_laa                     = 10,
              theta_sba                     = 0.5,
              theta_mna                     = 10,
              asc_no_participation           = 0)

apollo_fixed_null_c7 = c("asc_no_participation", "theta_laa", "theta_mna")

apollo_probabilities_null_c7 = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

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
  V[["laa_cmck"]]         = asc_laa_cmck
  V[["mna_cmck"]]         = asc_mna_cmck
  V[["laa_jmck"]]         = asc_laa_jmck
  V[["mna_jmck"]]         = asc_mna_jmck
  V[["laa_msqd"]]         = asc_laa_msqd
  V[["mra_msqd"]]         = asc_mra_msqd
  V[["sba_msqd"]]         = asc_sba_msqd
  V[["sfa_msqd"]]         = asc_sfa_msqd
  V[["mna_msqd"]]         = asc_mna_msqd
  V[["laa_psdn"]]         = asc_laa_psdn                      
  V[["mna_psdn"]]         = asc_mna_psdn
  V[["mna_nanc"]]         = asc_mna_nanc
  V[["sba_nanc"]]         = asc_sba_nanc
  V[["sda_nanc"]]         = asc_sda_nanc
  V[["no_participation"]] = asc_no_participation
  
  
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

saveRDS(apollo_probabilities_null_c4, "apollo_probabilities_null_c4.rds")
saveRDS(apollo_probabilities_null_c5, "apollo_probabilities_null_c5.rds")
saveRDS(apollo_probabilities_null_c6, "apollo_probabilities_null_c6.rds")
saveRDS(apollo_probabilities_null_c7, "apollo_probabilities_null_c7.rds")
saveRDS(apollo_beta_null_c4, "apollo_beta_null_c4.rds")
saveRDS(apollo_beta_null_c5, "apollo_beta_null_c5.rds")
saveRDS(apollo_beta_null_c6, "apollo_beta_null_c6.rds")
saveRDS(apollo_beta_null_c7, "apollo_beta_null_c7.rds")
saveRDS(apollo_fixed_null_c4, "apollo_fixed_null_c4.rds")
saveRDS(apollo_fixed_null_c5, "apollo_fixed_null_c5.rds")
saveRDS(apollo_fixed_null_c6, "apollo_fixed_null_c6.rds")
saveRDS(apollo_fixed_null_c7, "apollo_fixed_null_c7.rds")
