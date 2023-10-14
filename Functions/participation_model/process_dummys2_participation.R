#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td1 The tow dates
#' @param dat1 The Data
#' @param SDM.PSDN SDM data for sardine
#' @param SDM.MSQD SDM data for squid
#' @param SDM.NANC SDM data for anchovy
#' @param SDM.PHRG
#' @param SDM.JMCK 
#' @param SDM.CMCK 
#' @param CPUE.index
#' @param model_price1 estimated model for prices
#' @param fuel.prices1 fuel data by port
#' @param fuel.prices.state1 fuel data by port
#' @param min_year_est1 minimum year used in estimation
#' @param species.list.number1
#' @export

process_dummys2 <- function(xx, td1 = td, dat1 = dat, 
                            SDM.PSDN = psdn.sdm,
                            SDM.MSQD = msqd.sdm,
                            SDM.NANC = nanc.sdm,
                            SDM.PHRG = phrg.sdm,
                            SDM.JMCK = jmck.sdm,
                            SDM.CMCK = cmck.sdm,
                            CPUE.index = CPUE_index,
                            model_price1 = mod_estimate,
                            fuel.prices1 = fuel.prices,
                            fuel.prices.state1 = fuel.prices.state,
                            min_year_est1 = min_year_est,
                            species.list.number1 = species.list.number){

  # ### Delete
  # xx <- 150  # 6
  # td1 <- td
  # dat1 <- dat
  # SDM.PSDN <- psdn.sdm
  # SDM.MSQD <- msqd.sdm
  # SDM.NANC <- nanc.sdm
  # SDM.PHRG <- phrg.sdm
  # SDM.JMCK <- jmck.sdm
  # SDM.CMCK <- cmck.sdm
  # CPUE.index <- CPUE_index
  # model_price1 <- mod_estimate
  # fuel.prices1 <- fuel.prices
  # fuel.prices.state1 <- fuel.prices.state
  # min_year_est1 = min_year_est
  # species.list.number1 = species.list.number
  # ###

  temp_dat <- td1[xx, ]

  dat1$set_date <- as_date(dat1$set_date)
  fltz <- temp_dat$fleet_name
  sel  <- temp_dat$selection
  species  <- temp_dat$PACFIN_SPECIES_CODE
  port  <- temp_dat$PORT_AREA_CODE
  state  <- temp_dat$AGENCY_CODE


  ########################################################################################################
  ### DO VESSEL HAVE FISH THE SPECIES AT THE PORT IN CONSIDERATION IN THE LAST N_DAYS (AND LAST YEAR)? ###
  ########################################################################################################
  
  #Did vessel fish in past n_days at port for species X? 
  dum30 <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              selection == sel,
                                              selection != "No-Participation",
                                              group_all %in% fltz)
  dum30 <- dum30 %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30_val <- nrow(dum30)
  
  #-----------------------------------------------------------------------------------------------
  #Did cluster fish in past n_days at port for species X? 
  dum30_c <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter,
                                              selection == sel,
                                              selection != "No-Participation",
                                              group_all %in% fltz)
  dum30_c <- dum30_c %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30_c_val <- nrow(dum30_c)
  
  #-----------------------------------------------------------------------------------------------
  # Vessel fish in the past n_days of last year at port for species X?
  dum30y <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul, 
                                               set_date %within% temp_dat$prev_year_days_inter,
                                               VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                               selection == sel,
                                               selection != "No-Participation",
                                               group_all %in% fltz)

  dum30y <- dum30y %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30y_val <- nrow(dum30y)
  
  
  #-----------------------------------------------------------------------------------------------
  # Same decision that day before? 
  dum1 <- dat1 %>% ungroup %>% dplyr::filter(set_date %in% temp_dat$prev_day_date,
                                               VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                               selection == sel)
  
  dum1 <- dum1 %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum1_val <- nrow(dum1)
  
  
  #-----------------------------------------------------------------------------------------------
    #Did vessel fish in past n_days at port ? 
  dum30port <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              PORT_AREA_CODE == port,
                                              selection != "No-Participation",
                                              group_all %in% fltz)
  
  dum30port <- dum30port %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30port_val <- nrow(dum30port)
  
  
  ########################
  ## Calculate revenues ##
  ########################
  
  # ## Obtain vessel length and horsepower
  # vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
  #   dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
  # vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
  #   dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
  # if (is.na(vessel.hp$Vessel.horsepower)) {
  #   vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
  #     unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
  # }
  
  
  # << SDM LAST 30 DAYS >>
  
  if (sel != "No-Participation") {
    if (species == "PSDN") {
      # ## Obtain SDM previous day for the species/port combination
      # if (temp_dat$set_date >= as.Date("2017-04-20") & temp_dat$set_date <= as.Date("2017-04-30")) {
      #   sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
      #   sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
      #   sdm <- rbind(sdm1, sdm2)
      # } else if (temp_dat$set_date >= as.Date("2010-12-27") & temp_dat$set_date <= as.Date("2010-12-31")) {
      #   sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
      #   sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
      #   sdm <- rbind(sdm1, sdm2)
      # } else {
      #   sdm <- SDM.PSDN %>% dplyr::filter(set_date %in% temp_dat$set_date, PORT_AREA_CODE %in% port)
      # }
     
      # Average availability in the past n_days at port
      avail30y <- SDM.PSDN %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$PSDN_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "MSQD") {
      
      # Average availability in the past n_days at port
      avail30y <- SDM.MSQD %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$MSQD_SDM_90, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "NANC") {
      
      # Average availability in the past n_days at port
      avail30y <- SDM.NANC %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$NANC_SDM_20, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "JMCK") {
      
      # Average availability in the past n_days at port
      avail30y <- SDM.JMCK %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$JMCK_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "CMCK") {
      
      # Average availability in the past n_days at port
      avail30y <- SDM.CMCK %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$CMCK_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "PHRG") {
      
      # Average availability in the past n_days at port
      avail30y <- SDM.PHRG %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$PHRG_SDM_20, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else {
      # Average availability in the past n_days at port
      avail30y <- CPUE.index %>% ungroup %>%
        dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port, Species_Dominant %in% species)
      avail30y <- mean(avail30y$CPUE_index, na.rm = TRUE)
      dCPUE <- 1
      dCPUE_90 <- 0
      if (is.na(avail30y)) {
        avail30y <- CPUE.index %>% ungroup %>%
          dplyr::filter(set_date %within% temp_dat$days90_inter, PORT_AREA_CODE %in% port, Species_Dominant %in% species)
        avail30y <- mean(avail30y$CPUE_index, na.rm = TRUE)
        dCPUE_90 <- 1
      }
    }
    
    #Calculate availability
    mean_avail <- avail30y

    # Create database to predict prices
    est_price <- temp_dat
    est_price$trend <- (year(est_price$set_date) - min_year_est1) + 1 
    est_price$set_month_1 <- ifelse(month(est_price$set_date) == 1, 1, 0) 
    est_price$set_month_2 <- ifelse(month(est_price$set_date) == 2, 1, 0) 
    est_price$set_month_3 <- ifelse(month(est_price$set_date) == 3, 1, 0) 
    est_price$set_month_4 <- ifelse(month(est_price$set_date) == 4, 1, 0) 
    est_price$set_month_5 <- ifelse(month(est_price$set_date) == 5, 1, 0) 
    est_price$set_month_6 <- ifelse(month(est_price$set_date) == 6, 1, 0) 
    est_price$set_month_7 <- ifelse(month(est_price$set_date) == 7, 1, 0) 
    est_price$set_month_8 <- ifelse(month(est_price$set_date) == 8, 1, 0) 
    est_price$set_month_9 <- ifelse(month(est_price$set_date) == 9, 1, 0) 
    est_price$set_month_10 <- ifelse(month(est_price$set_date) == 10, 1, 0) 
    est_price$set_month_11 <- ifelse(month(est_price$set_date) == 11, 1, 0) 
    est_price$PORT_AREA_CODE <- port
    
    # Predict landings and prices
    id_price <- species.list.number1 %>% dplyr::filter(species.list == species)
    id_price1 <- id_price$id_number
    if (length(id_price1) == 0) {
      price30 <- dat1 %>% ungroup %>%
        dplyr::filter(set_date %within% temp_dat$days30_inter, selection == sel) 
      mean_price <- mean(price30$Price_mtons, na.rm = TRUE)
      dPrice30_species <- 0
      dPrice90_species <- 0
      dPrice30 <- 1
      ### If to restrictive, then use species
      if (is.na(mean_price)) {
        price30 <- dat1 %>% ungroup %>%
          dplyr::filter(set_date %within% temp_dat$days30_inter, Species_Dominant == species)
        mean_price <- mean(price30$Price_mtons, na.rm = TRUE)
        dPrice30_species <- 1
        dPrice90_species <- 0
      }
      if (is.na(mean_price)) {
        price30 <- dat1 %>% ungroup %>%
          dplyr::filter(set_date %within% temp_dat$days90_inter, Species_Dominant == species)
        mean_price <- mean(price30$Price_mtons, na.rm = TRUE)
        dPrice30_species <- 0
        dPrice90_species <- 1
      }
    } else {
      price <- as.data.frame(predict(model_price1[[id_price1]], est_price, interval = "prediction"))
      price$fit <- exp(price$fit)
      mean_price <- price$fit
      dPrice30 <- 0
      dPrice30_species <- 0
      dPrice90_species <- 0
    }
    
  } else {
    mean_avail <- 0
    mean_price <- 0
    dCPUE <- 0
    dCPUE_90 <- 0
    dPrice30 <- 0
    dPrice30_species <- 0
    dPrice90_species <- 0
}
  
  # Include new information in temp_dat
  temp_dat$mean_avail <- mean_avail
  temp_dat$mean_price <- mean_price
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_days_port <- dum30port_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_last_day <- dum1_val
  temp_dat$dummy_clust_prev_days <- dum30_c_val
  temp_dat$dCPUE <- dCPUE
  temp_dat$dCPUE90 <- dCPUE_90
  temp_dat$dPrice30 <- dPrice30
  temp_dat$dPrice30_s <- dPrice30_species
  temp_dat$dPrice90_s <- dPrice90_species
  
  
  
  ###################
  ## Calculate Cost #
  ###################
  
  if (sel != "No-Participation") {
    diesel.price <- fuel.prices1 %>% ungroup %>% 
      dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE == port)
    mean_diesel_price <- mean(diesel.price$diesel.price.AFI, na.rm = TRUE)
    dDieselState_price <- 0
    if (is.na(mean_diesel_price)) {
      diesel.price <- fuel.prices.state1 %>% ungroup %>% 
        dplyr::filter(LANDING_MONTH == lubridate::month(temp_dat$set_date),
                      LANDING_YEAR == lubridate::year(temp_dat$set_date), 
                      AGENCY_CODE == state)
      mean_diesel_price <- mean(diesel.price$diesel.price.state.AFI, na.rm = TRUE)
      dDieselState_price <- 1
    }
    expected.distance <- dat1 %>% ungroup %>% 
      dplyr::filter(set_date %within% temp_dat$days90_inter, selection == sel)
    exp_dist <- mean(expected.distance$dist, na.rm = TRUE)
  } else {
    exp_dist <- 0
    mean_diesel_price <- 0
    dDieselState_price <- 0
  }
  
  temp_dat$diesel_price <- mean_diesel_price
  temp_dat$dist_port_to_catch_area <- exp_dist
  temp_dat$dDieselState <- dDieselState_price
  
  ## Return data for row choice
  return(temp_dat)
}
  


