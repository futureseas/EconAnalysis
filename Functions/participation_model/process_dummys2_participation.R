#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td1 The tow dates
#' @param dat1 The Data
#' @param qPSDN1
#' @param qMSQD1 
#' @param qNANC1
#' @param qPHRG1
#' @param SDM.PSDN
#' @param SDM.MSQD
#' @param SDM.NANC
#' @param SDM.PHRG
#' @export

process_dummys2 <- function(xx, td1 = td, dat1 = dat, 
                            qPSDN1 = qPSDN, SDM.PSDN = psdn.sdm,
                            qMSQD1 = qMSQD, SDM.MSQD = msqd.sdm,
                            qNANC1 = qNANC, SDM.NANC = nanc.sdm,
                            qPHRG1 = qPHRG, SDM.PHRG = phrg.sdm){
  

  # ### Delete
  # xx <- 14239
  # td1 <- td
  # dat1 <- dat
  # qPSDN1 <- qPSDN
  # SDM.PSDN <- psdn.sdm
  # qMSQD1 <- qMSQD
  # SDM.MSQD <- msqd.sdm
  # qNANC1 <- qNANC
  # SDM.NANC <- nanc.sdm
  # qPHRG1 <- qPHRG
  # SDM.PHRG <- phrg.sdm
  # ###
  
  temp_dat <- td1[xx, ]

  dat1$set_date <- as_date(dat1$set_date)
  fltz <- temp_dat$fleet_name
  sel  <- temp_dat$selection
  species  <- temp_dat$species
  port  <- temp_dat$port
  

  ########################################################################################################
  ### DO VESSEL HAVE FISH THE SPECIES AT THE PORT IN CONSIDERATION IN THE LAST N_DAYS (AND LAST YEAR)? ###
  ########################################################################################################
  
  #Did vessel fish in past 30 days at port for species X? 
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
  # Vessel fish in the past 30 days of last year at port for species X?
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
  
  
  
  ##############################################################################################
  ## Calculate revenues (obtained by all the fleet within XX days, excluding the actual haul) ##
  ## Include SDM for species available...                                                      ##
  ##############################################################################################
  
  
  if (sel != "No-Participation") {
    if (species == "PSDN") {
      
      ## Obtain vessel length and horsepower
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      if (is.na(vessel.hp$Vessel.horsepower)) {
        vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
          unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
      }

      ## Obtain SDM previous day for the species/port combination
      if (temp_dat$prev_day_date >= as.Date("2017-04-20") & temp_dat$prev_day_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$prev_day_date >= as.Date("2010-12-27") & temp_dat$prev_day_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.PSDN %>% dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port)
      }
      
      ## Obtain average price at species/port combination last 30 days (not including current day)
      price <- dat1 %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                      PORT_AREA_CODE %in% port,
                                      Species_Dominant %in% species)

      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      # dum_rev$Closure <- 0
      dum_rev_SDM$lag_PSDN_SDM_60 <- mean(sdm$PSDN_SDM_60)
      dum_rev_SDM$set_year <- year(dum_rev_SDM$set_date)
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Price_mton <- mean(price$Price_mtons)
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      
      
      # Predict landings
      prediction <- as.data.frame(predict(qPSDN1, dum_rev_SDM, interval = "prediction"))
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- prediction$fit * dum_rev_SDM$Price_mton
      
      #---------------------------------------------------------------------------------
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                    set_date %within% temp_dat$days_inter,
                                                    group_all %in% fltz, ## Check if this is too restrictive...
                                                    selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
      

    } else if (species == "MSQD") {
      
      ## Obtain vessel length and horsepower
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      if (is.na(vessel.hp$Vessel.horsepower)) {
        vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
          unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
      }
      
      ## Obtain SDM previous day for the species/port combination (USE WITHIN IF SDM NOT AVAILABLE...)
      if (temp_dat$prev_day_date >= as.Date("2017-04-20") & temp_dat$prev_day_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.MSQD %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.MSQD %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$prev_day_date >= as.Date("2010-12-27") & temp_dat$prev_day_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.MSQD %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.MSQD %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.MSQD %>% dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port)
      }
      
      ## Obtain average price at species/port combination last 30 days (not including current day)
      price <- dat1 %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                      PORT_AREA_CODE %in% port,
                                      Species_Dominant %in% species)
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      # dum_rev$Closure <- 0
      dum_rev_SDM$lag_MSQD_SDM_90 <- mean(sdm$MSQD_SDM_90)
      dum_rev_SDM$set_year <- year(dum_rev_SDM$set_date)
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Price_mton <- mean(price$Price_mtons) 
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      
      # Predict landings
      prediction <- as.data.frame(predict(qMSQD1, dum_rev_SDM, interval = "prediction"))
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- prediction$fit * dum_rev_SDM$Price_mton
      
      
      #---------------------------------------------------------------------------------
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                    set_date %within% temp_dat$days_inter,
                                                    group_all %in% fltz, ## Check if this is too restrictive...
                                                    selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
      
      
    } else if (species == "NANC") {
      
      ## Obtain vessel length and horsepower
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      if (is.na(vessel.hp$Vessel.horsepower)) {
        vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
          unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
      }
      
      ## Obtain SDM previous day for the species/port combination
      if (temp_dat$prev_day_date >= as.Date("2017-04-20") & temp_dat$prev_day_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.NANC %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.NANC %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$prev_day_date >= as.Date("2010-12-27") & temp_dat$prev_day_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.NANC %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.NANC %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.NANC %>% dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port)
      }
      
      ## Obtain average price at species/port combination last 30 days (not including current day)
      price <- dat1 %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                      PORT_AREA_CODE %in% port,
                                      Species_Dominant %in% species)
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$lag_NANC_SDM_20 <- mean(sdm$NANC_SDM_20)
      dum_rev_SDM$set_year <- year(dum_rev_SDM$set_date)
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Price_mton <- mean(price$Price_mtons) 
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      
      # Predict landings
      prediction <- as.data.frame(predict(qNANC1, dum_rev_SDM, interval = "prediction"))
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- prediction$fit * dum_rev_SDM$Price_mton
      
      #---------------------------------------------------------------------------------
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                    set_date %within% temp_dat$days_inter,
                                                    group_all %in% fltz, ## Check if this is too restrictive...
                                                    selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
      
      
    } else if (species == "PHRG") {
      
      ## Obtain vessel length and horsepower
      vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
        dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
      if (is.na(vessel.hp$Vessel.horsepower)) {
        vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
          unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
      }
      
      ## Obtain SDM previous day for the species/port combination
      if (temp_dat$prev_day_date >= as.Date("2017-04-20") & temp_dat$prev_day_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.PHRG %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PHRG %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$prev_day_date >= as.Date("2010-12-27") & temp_dat$prev_day_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.PHRG %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PHRG %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.PHRG %>% dplyr::filter(set_date %in% temp_dat$prev_day_date, PORT_AREA_CODE %in% port)
      }
      
      ## Obtain average price at species/port combination last 30 days (not including current day)
      price <- dat1 %>% dplyr::filter(set_date %within% temp_dat$days_inter,
                                      PORT_AREA_CODE %in% port,
                                      Species_Dominant %in% species)
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$lag_PHRG_SDM_220 <- mean(sdm$PHRG_SDM_220)
      dum_rev_SDM$set_year <- year(dum_rev_SDM$set_date)
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Price_mton <- mean(price$Price_mtons) 
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      
      # Predict landings
      prediction <- as.data.frame(predict(qPHRG1, dum_rev_SDM, interval = "prediction"))
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- prediction$fit * dum_rev_SDM$Price_mton
      
      #---------------------------------------------------------------------------------
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                    set_date %within% temp_dat$days_inter,
                                                    group_all %in% fltz, ## Check if this is too restrictive...
                                                    selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
      
      
    } else {
      
      dum_rev_SDM <- NULL
      dum_rev_SDM$Revenue <- NA
      
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                  set_date %within% temp_dat$days_inter,
                                                  group_all %in% fltz, ## Check if this is too restrictive...
                                                  selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
    
    }
    
    #Calculate revenue
    mean_rev_SDM <- mean(dum_rev_SDM$Revenue, na.rm = TRUE)
    mean_rev_SDM <- replace(mean_rev_SDM, is.na(mean_rev_SDM), 0)
    mean_rev <- mean(dum_rev$Revenue, na.rm = TRUE)
    mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
    
    
  } else {
    mean_rev_SDM <- 0
    mean_rev <- 0
  }
  
  # Include new information in temp_dat
  temp_dat$mean_rev_SDM <- mean_rev_SDM
  temp_dat$mean_rev <- mean_rev
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_last_day <- dum1_val

  print(xx)
  return(temp_dat)
}
  


