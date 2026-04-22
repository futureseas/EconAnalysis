#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td1 The tow dates
#' @param dat1 The Data
#' @param SDM.ALBC SDM data for albacore
#' @param SDM.PSDN SDM data for sardine
#' @param SDM.MSQD SDM data for squid
#' @param SDM.NANC SDM data for anchovy
#' @param SDM.PHRG
#' @param SDM.JMCK 
#' @param SDM.CMCK 
#' @param CPUE.index
#' @param model_price1 estimated model for prices
#' @param model_catch1 estimated model for catches
#' @param fuel.prices1 fuel data by port
#' @param fuel.prices.state1 fuel data by port
#' @param min_year_est1 minimum year used in estimation
#' @param species.list.number1
#' @param species.list.number.sdm1
#' @export

process_dummys2 <- function(xx, td1 = td, dat1 = dat, 
                            SDM.ALBC = albc.sdm,
                            SDM.PSDN = psdn.sdm,
                            SDM.MSQD = msqd.sdm,
                            SDM.NANC = nanc.sdm,
                            SDM.PHRG = phrg.sdm,
                            SDM.JMCK = jmck.sdm,
                            SDM.CMCK = cmck.sdm,
                            CPUE.index = CPUE_index,
                            model_price1 = mod_estimate,
                            model_catch1 = mod_estimate_sdm,
                            fuel.prices1 = fuel.prices,
                            fuel.prices.state1 = fuel.prices.state,
                            min_year_est1 = min_year_est,
                            species.list.number1 = species.list.number,
                            species.list.number.sdm1 = species.list.number.sdm){

  # ### Delete
  # xx <- 12  # 6
  # td1 <- td %>% arrange(fished_haul)
  # dat1 <- dat
  # SDM.PSDN <- psdn.sdm
  # SDM.ALBC <- albc.sdm
  # SDM.MSQD <- msqd.sdm
  # SDM.NANC <- nanc.sdm
  # SDM.PHRG <- phrg.sdm
  # SDM.JMCK <- jmck.sdm
  # SDM.CMCK <- cmck.sdm
  # CPUE.index <- CPUE_index
  # model_price1 <- mod_estimate
  # model_catch1 = mod_estimate_sdm
  # fuel.prices1 <- fuel.prices
  # fuel.prices.state1 <- fuel.prices.state
  # min_year_est1 = min_year_est
  # species.list.number1 = species.list.number
  # species.list.number.sdm1 = species.list.number.sdm
  # ###

  # start.time <- Sys.time()
  
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
  dum30 <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              selection == sel,
                                              selection != "No-Participation")
  dum30 <- dum30 %>% distinct(trip_id, .keep_all = T)
  dum30_val <- nrow(dum30)
  
  #-----------------------------------------------------------------------------------------------
  #Did cluster fish in past n_days at port for species X? 
  dum30_c <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                                selection == sel,
                                                selection != "No-Participation",
                                                group_all %in% fltz)
  dum30_c <- dum30_c %>% distinct(trip_id, .keep_all = T)
  dum30_c_val <- nrow(dum30_c)

  #-----------------------------------------------------------------------------------------------
  # Vessel fish in the past n_days of last year at port for species X?
  dum30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$prev_year_days_inter,
                                               VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                               selection == sel,
                                               selection != "No-Participation")
  dum30y <- dum30y %>% distinct(trip_id, .keep_all = T)
  dum30y_val <- nrow(dum30y)
  

  #-----------------------------------------------------------------------------------------------
  # Same decision that day before? 
  dum1 <- dat1 %>% ungroup %>% dplyr::filter(set_date %in% temp_dat$prev_day_date,
                                             VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                             selection == sel)
  dum1 <- dum1 %>% distinct(trip_id, .keep_all = T)
  dum1_val <- nrow(dum1)

  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past n_days at port ? 
  dum30port <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                                  VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                                  PORT_AREA_CODE == port,
                                                  selection != "No-Participation")
  
  dum30port <- dum30port %>% distinct(trip_id, .keep_all = T)
  dum30port_val <- nrow(dum30port)
  

  ########################
  ## Calculate revenues ##
  ########################
  
  if (sel != "No-Participation") {
    if (species == "ALBC") {
      # Average availability in the past n_days at port
      catch30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter,
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y <- mean(catch30y$Landings_mtons, na.rm = TRUE)
      catch30y2 <- catch30y

      # Average availability in the past n_days at port
      avail30y <- SDM.ALBC %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$albc_SDM_90, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
    } else if (species == "PSDN") {
      # Predict catch using SDM
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>%
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
        day.SDM <- SDM.PSDN %>% dplyr::select(c(set_date, PSDN_SDM_60, PORT_AREA_CODE)) %>%
          dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port) %>% unique()
      est_catch <- temp_dat
      est_catch$max_days_sea <- 1 
      est_catch$Vessel.length <- mean(vessel.length$Vessel.length, na.rm = TRUE)
      if (length(day.SDM) != 0) {
        est_catch$PSDN_SDM_60 <- mean(day.SDM$PSDN_SDM_60, na.rm = TRUE)
      } else {
        est_catch$PSDN_SDM_60 <- 0
      }
      est_catch$group_all <- fltz
      id_catch <- species.list.number.sdm1 %>% dplyr::filter(species.list_sdm == species)
      id_catch1 <- id_catch$id_number
      catch <- as.data.frame(predict(model_catch1[[id_catch1]], est_catch, interval = "prediction"))
      catch$fit <- exp(catch$fit)
      catch30y <- catch$fit
      
      # Average availability in the past n_days at port
      catch30y2 <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y2 <- mean(catch30y2$Landings_mtons, na.rm = TRUE)
     
      # Average availability in the past n_days at port
      avail30y <- SDM.PSDN %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                         PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$PSDN_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "MSQD") {
      
      # Predict catch using SDM
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>%
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
        day.SDM <- SDM.MSQD %>% dplyr::select(c(set_date, MSQD_SDM_90, PORT_AREA_CODE)) %>%
          dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port) %>% unique()
      est_catch <- temp_dat
      est_catch$max_days_sea <- 1 
      est_catch$Vessel.length <- mean(vessel.length$Vessel.length, na.rm = TRUE)
      if (length(day.SDM) != 0) {
        est_catch$MSQD_SDM_90 <- mean(day.SDM$MSQD_SDM_90, na.rm = TRUE)
      } else {
        est_catch$MSQD_SDM_90 <- 0
      }
      est_catch$group_all <- fltz
      id_catch <- species.list.number.sdm1 %>% dplyr::filter(species.list_sdm == species)
      id_catch1 <- id_catch$id_number
      catch <- as.data.frame(predict(model_catch1[[id_catch1]], est_catch, interval = "prediction"))
      catch$fit <- exp(catch$fit)
      catch30y <- catch$fit
      
      # Average availability in the past n_days at port
      catch30y2 <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y2 <- mean(catch30y2$Landings_mtons, na.rm = TRUE)

      # Average availability in the past n_days at port
      avail30y <- SDM.MSQD %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                         PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$MSQD_SDM_90, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "NANC") {
      
      # Predict catch using SDM
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>%
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      day.SDM <- SDM.NANC %>% dplyr::select(c(set_date, NANC_SDM_60, PORT_AREA_CODE)) %>%
        dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port) %>% unique()
      est_catch <- temp_dat
      est_catch$max_days_sea <- 1 
      est_catch$Vessel.length <- mean(vessel.length$Vessel.length, na.rm = TRUE)
      if (length(day.SDM) != 0) {
        est_catch$NANC_SDM_60 <- mean(day.SDM$NANC_SDM_60, na.rm = TRUE)
      } else {
        est_catch$NANC_SDM_60 <- 0
      }
      est_catch$group_all <- fltz 
      est_catch <- est_catch %>% mutate(group_all = ifelse(group_all == 5, 6, group_all))
      id_catch <- species.list.number.sdm1 %>% dplyr::filter(species.list_sdm == species)
      id_catch1 <- id_catch$id_number
      catch <- as.data.frame(predict(model_catch1[[id_catch1]], est_catch, interval = "prediction"))
      catch$fit <- exp(catch$fit)
      catch30y <- catch$fit
      
      # Average availability in the past n_days at port
      catch30y2 <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y2 <- mean(catch30y2$Landings_mtons, na.rm = TRUE)
      
      
      # Average availability in the past n_days at port
      avail30y <- SDM.NANC %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                         PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$NANC_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0
      
    } else if (species == "JMCK") {

      # Average availability in the past n_days at port
      catch30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter,
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y <- mean(catch30y$Landings_mtons, na.rm = TRUE)
      catch30y2 <- catch30y

      # Average availability in the past n_days at port
      avail30y <- SDM.JMCK %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$JMCK_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0

    } else if (species == "CMCK") {

      # Average availability in the past n_days at port
      catch30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter,
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y <- mean(catch30y$Landings_mtons, na.rm = TRUE)
      catch30y2 <- catch30y

      # Average availability in the past n_days at port
      avail30y <- SDM.CMCK %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                         PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$CMCK_SDM_60, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0

    } else if (species == "PHRG") {

      # Average availability in the past n_days at port
      catch30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter,
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y <- mean(catch30y$Landings_mtons, na.rm = TRUE)
      catch30y2 <- catch30y

      # Average availability in the past n_days at port
      avail30y <- SDM.PHRG %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                         PORT_AREA_CODE %in% port)
      avail30y <- mean(avail30y$PHRG_SDM_20, na.rm = TRUE)
      dCPUE <- 0
      dCPUE_90 <- 0

    } else {
      # Average availability in the past n_days at port
      catch30y <- dat1 %>% ungroup %>% dplyr::filter(set_date %within% temp_dat$days30_inter, 
                                                     PORT_AREA_CODE %in% port,
                                                     Species_Dominant %in% species)
      catch30y <- mean(catch30y$Landings_mtons, na.rm = TRUE)
      catch30y2 <- catch30y
      
      # Average availability in the past n_days at port
      avail30y <- CPUE.index %>% ungroup %>%
        dplyr::filter(set_date %within% temp_dat$days30_inter, 
                      PORT_AREA_CODE %in% port, 
                      Species_Dominant %in% species)
      avail30y <- mean(avail30y$CPUE_index, na.rm = TRUE)
      dCPUE <- 1
      dCPUE_90 <- 0
      if (is.na(avail30y)) {
        avail30y <- CPUE.index %>% ungroup %>%
          dplyr::filter(set_date %within% temp_dat$days90_inter, 
                        PORT_AREA_CODE %in% port, 
                        Species_Dominant %in% species)
        avail30y <- mean(avail30y$CPUE_index, na.rm = TRUE)
        dCPUE_90 <- 1
      }
    }
    
    #Calculate availability
    mean_avail <- avail30y
    mean_catch2 <- catch30y2 
    mean_catch  <- catch30y
    
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
    
    price30v2 <- dat1 %>% ungroup %>%
      dplyr::filter(set_date %within% temp_dat$days30_inter, selection == sel) 
      mean_price2 <- mean(price30v2$Price_mtons, na.rm = TRUE)
      dPrice30_species2 <- 0
      dPrice90_species2 <- 0

    ### If to restrictive, then use species
    if (is.na(mean_price2)) {
      price30v2 <- dat1 %>% ungroup %>%
        dplyr::filter(set_date %within% temp_dat$days30_inter, Species_Dominant == species)
        mean_price2 <- mean(price30v2$Price_mtons, na.rm = TRUE)
        dPrice30_species2 <- 1
        dPrice90_species2 <- 0
    }
    if (is.na(mean_price2)) {
      price30v2 <- dat1 %>% ungroup %>%
        dplyr::filter(set_date %within% temp_dat$days90_inter, Species_Dominant == species)
        mean_price2 <- mean(price30v2$Price_mtons, na.rm = TRUE)
        dPrice30_species2 <- 0
        dPrice90_species2 <- 1
    }

  } else {
    mean_avail <- 0
    mean_price <- 0
    mean_price2 <- 0
    mean_catch2 <- 0
    mean_catch <- 0
    dCPUE <- 0
    dCPUE_90 <- 0
    dPrice30 <- 0
    dPrice30_species <- 0
    dPrice90_species <- 0
    dPrice30_species2 <- 0
    dPrice90_species2 <- 0
}
  
  # Include new information in temp_dat
  temp_dat$mean_avail <- mean_avail
  temp_dat$mean_price <- mean_price
  temp_dat$mean_price2 <- mean_price2
  temp_dat$mean_catch2 <- mean_catch2
  temp_dat$mean_catch  <- mean_catch
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
  temp_dat$dPrice30_s2 <- dPrice30_species2
  temp_dat$dPrice90_s2 <- dPrice90_species2
  
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
  
  # end.time <- Sys.time()
  # time.taken <- round(end.time - start.time,2)
  # time.taken
  
  # return data 
 return(temp_dat)
}
  


