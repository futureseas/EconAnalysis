#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td1 The tow dates
#' @param dat1 The Data
#' @param qPSDN1 landing model for sardine using SDMs
#' @param qMSQD1 landing model for squid using SDMs
#' @param qNANC1 landing model for anchovy using SDMs
#' @param SDM.PSDN SDM data for sardine
#' @param SDM.MSQD SDM data for squid
#' @param SDM.NANC SDM data for anchovy
#' @param model_landings1 estimated model for landings
#' @param model_price1 estimated model for prices
#' @param fuel.prices1 fuel data by port
#' @param min_year_est1 minimum year used in estimation
#' @export

process_dummys2 <- function(xx, td1 = td, dat1 = dat, 
                            qPSDN1 = qPSDN, SDM.PSDN = psdn.sdm,
                            qMSQD1 = qMSQD, SDM.MSQD = msqd.sdm,
                            qNANC1 = qNANC, SDM.NANC = nanc.sdm,
                            model_price1 = model_price,
                            model_landings1 = model_landings,
                            fuel.prices1 = fuel.prices,
                            min_year_est1 = min_year_est){

# ### Delete
# xx <- 14245 # 59
# td1 <- td
# dat1 <- dat
# qPSDN1 <- qPSDN
# SDM.PSDN <- psdn.sdm
# qMSQD1 <- qMSQD
# SDM.MSQD <- msqd.sdm
# qNANC1 <- qNANC
# SDM.NANC <- nanc.sdm
# model_price1 <- model_price
# model_landings1 <- model_landings
# fuel.prices1 <- fuel.prices
# min_year_est1 = min_year_est
# ###

  temp_dat <- td1[xx, ]

  dat1$set_date <- as_date(dat1$set_date)
  fltz <- temp_dat$fleet_name
  sel  <- temp_dat$selection
  species  <- temp_dat$species
  port  <- temp_dat$ports


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
  
  
  
  ########################
  ## Calculate revenues ##
  ########################
  
  ## Obtain vessel length and horsepower
  vessel.length <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.length)) %>% 
    dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
  vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower)) %>% 
    dplyr::filter(VESSEL_NUM %in% temp_dat$fished_VESSEL_NUM) %>% unique()
  if (is.na(vessel.hp$Vessel.horsepower)) {
    vessel.hp <- dat1 %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% 
      unique() %>% dplyr::filter(Vessel.length == mean(vessel.length$Vessel.length))
  }
  
  
  if (sel != "No-Participation") {
    if (species == "PSDN") {
      
      ## Obtain SDM previous day for the species/port combination
      if (temp_dat$set_date >= as.Date("2017-04-20") & temp_dat$set_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$set_date >= as.Date("2010-12-27") & temp_dat$set_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.PSDN %>% dplyr::filter(set_date %in% temp_dat$set_date, PORT_AREA_CODE %in% port)
      }
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$PSDN_SDM_60 <- mean(sdm$PSDN_SDM_60)
      dum_rev_SDM$trend <- (year(dum_rev_SDM$set_date) - min_year_est1) + 1 
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      dum_rev_SDM$Species_Dominant <- species
      dum_rev_SDM$PORT_AREA_CODE <- port
      
      # Predict landings and prices
      price <- as.data.frame(predict(model_price1, dum_rev_SDM, interval = "prediction"))
      price$fit <- exp(price$fit)
      landings <- as.data.frame(predict(qPSDN1, dum_rev_SDM, interval = "prediction"))
      landings$fit <- exp(landings$fit)
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- landings$fit * price$fit
      

    } else if (species == "MSQD") {
      
      ## Obtain SDM previous day for the species/port combination (USE WITHIN IF SDM NOT AVAILABLE...)
      if (temp_dat$set_date >= as.Date("2017-04-20") & temp_dat$set_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.MSQD %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.MSQD %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else if (temp_dat$set_date >= as.Date("2010-12-27") & temp_dat$set_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.MSQD %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.MSQD %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm <- rbind(sdm1, sdm2)
      } else {
        sdm <- SDM.MSQD %>% dplyr::filter(set_date %in% temp_dat$set_date, PORT_AREA_CODE %in% port)
      }
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$MSQD_SDM_90 <- mean(sdm$MSQD_SDM_90)
      dum_rev_SDM$trend <- (year(dum_rev_SDM$set_date) - min_year_est1) + 1 
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      dum_rev_SDM$Species_Dominant <- species
      dum_rev_SDM$PORT_AREA_CODE <- port
      
      # Predict landings and prices
      price <- as.data.frame(predict(model_price1, dum_rev_SDM, interval = "prediction"))
      price$fit <- exp(price$fit)
      landings <- as.data.frame(predict(qMSQD1, dum_rev_SDM, interval = "prediction"))
      landings$fit <- exp(landings$fit)

      # Calculate expected revenues
      dum_rev_SDM$Revenue <- landings$fit * price$fit
      
    } else if (species == "NANC") {
      
      ## Obtain SDM previous day for the species/port combination
      if (temp_dat$set_date >= as.Date("2017-04-20") & temp_dat$set_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.NANC %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.NANC %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm_nanc <- rbind(sdm1, sdm2)
      } else if (temp_dat$set_date >= as.Date("2010-12-27") & temp_dat$set_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.NANC %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.NANC %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm_nanc <- rbind(sdm1, sdm2)
      } else {
        sdm_nanc <- SDM.NANC %>% dplyr::filter(set_date %in% temp_dat$set_date, PORT_AREA_CODE %in% port)
      }
      
      if (temp_dat$set_date >= as.Date("2017-04-20") & temp_dat$set_date <= as.Date("2017-04-30")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-04-19", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2017-05-01", PORT_AREA_CODE %in% port)
        sdm_psdn <- rbind(sdm1, sdm2)
      } else if (temp_dat$set_date >= as.Date("2010-12-27") & temp_dat$set_date <= as.Date("2010-12-31")) {
        sdm1 <- SDM.PSDN %>% dplyr::filter(set_date == "2010-12-26", PORT_AREA_CODE %in% port)
        sdm2 <- SDM.PSDN %>% dplyr::filter(set_date == "2011-01-01", PORT_AREA_CODE %in% port)
        sdm_psdn <- rbind(sdm1, sdm2)
      } else {
        sdm_psdn <- SDM.PSDN %>% dplyr::filter(set_date %in% temp_dat$set_date, PORT_AREA_CODE %in% port)
      }
      
      # Create database to predict landings and prices
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$PSDN.Open <- ifelse(dum_rev_SDM$set_date >= "2008-05-29" & dum_rev_SDM$set_date < "2008-07-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2008-08-08" & dum_rev_SDM$set_date < "2008-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2008-09-23" & dum_rev_SDM$set_date < "2009-01-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2009-02-20" & dum_rev_SDM$set_date < "2009-07-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2009-07-18" & dum_rev_SDM$set_date < "2009-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2009-09-23" & dum_rev_SDM$set_date < "2010-01-01", 0,  
                               ifelse(dum_rev_SDM$set_date >= "2010-06-12" & dum_rev_SDM$set_date < "2010-07-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2010-07-22" & dum_rev_SDM$set_date < "2010-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2010-09-24" & dum_rev_SDM$set_date < "2011-01-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2011-03-05" & dum_rev_SDM$set_date < "2011-07-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2011-07-12" & dum_rev_SDM$set_date < "2011-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2011-09-21" & dum_rev_SDM$set_date < "2012-01-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2012-08-23" & dum_rev_SDM$set_date < "2012-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2013-08-22" & dum_rev_SDM$set_date < "2013-09-01", 0, 
                               ifelse(dum_rev_SDM$set_date >= "2015-04-28", 0, 1)))))))))))))))
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$NANC_SDM_20 <- mean(sdm_nanc$NANC_SDM_20)
      dum_rev_SDM$PSDN_SDM_60 <- mean(sdm_psdn$PSDN_SDM_60)
      dum_rev_SDM$trend <- (year(dum_rev_SDM$set_date) - min_year_est1) + 1 
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      dum_rev_SDM$Species_Dominant <- species
      dum_rev_SDM$PORT_AREA_CODE <- port
      
      # Predict landings and prices
      price <- as.data.frame(predict(model_price1, dum_rev_SDM, interval = "prediction"))
      price$fit <- exp(price$fit)
      landings <- as.data.frame(predict(qNANC1, dum_rev_SDM, interval = "prediction"))
      landings$fit <- exp(landings$fit)
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- landings$fit * price$fit
      
    } else {
      
      # Create database to predict landings
      dum_rev_SDM <- temp_dat
      dum_rev_SDM$VESSEL_NUM <- dum_rev_SDM$fished_VESSEL_NUM
      dum_rev_SDM$trend <- (year(dum_rev_SDM$set_date) - min_year_est1) + 1 
      dum_rev_SDM$set_month  <- month(dum_rev_SDM$set_date)
      dum_rev_SDM$Vessel.length <- mean(vessel.length$Vessel.length) 
      dum_rev_SDM$Vessel.horsepower <- mean(vessel.hp$Vessel.horsepower, na.rm = TRUE)
      dum_rev_SDM$Species_Dominant <- species
      dum_rev_SDM$PORT_AREA_CODE <- port
      
      # Predict landings and prices
      price <- as.data.frame(predict(model_price1, dum_rev_SDM, interval = "prediction"))
      price$fit <- exp(price$fit)
      landings <- as.data.frame(predict(model_landings1, dum_rev_SDM, interval = "prediction"))
      landings$fit <- exp(landings$fit)
      
      # Calculate expected revenues
      dum_rev_SDM$Revenue <- landings$fit * price$fit
    }
    
    #Calculate revenue
    mean_rev <- mean(dum_rev_SDM$Revenue, na.rm = TRUE)
    mean_rev <- replace(mean_rev, is.na(mean_rev), 0)

  } else {
    mean_rev <- 0
  }
  
  # Include new information in temp_dat
  temp_dat$mean_rev <- mean_rev
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_last_day <- dum1_val
  
  
  
  ###################
  ## Calculate Cost #
  ###################
  
  if (sel != "No-Participation") {
    diesel.price <- fuel.prices1 %>% ungroup %>% 
      dplyr::filter(set_date %within% temp_dat$days30_inter, PORT_AREA_CODE == port)
    mean_diesel_price <- mean(diesel.price$diesel.price.AFI, na.rm = TRUE)
    
    expected.distance <- dat1 %>% ungroup %>% 
      dplyr::filter(set_date %within% temp_dat$days90_inter, 
                    selection == sel)
    exp_dist <- mean(expected.distance$dist, na.rm = TRUE)
    
    cost_port_to_catch_area <- mean_diesel_price * exp_dist
    cost_port_to_cog <- mean_diesel_price * temp_dat$dist_to_cog
    travel_cost <- cost_port_to_catch_area + cost_port_to_cog
    
    
  } else {
    cost_port_to_catch_area <- 0
    cost_port_to_cog <- 0
    travel_cost <- 0
  }
  
  temp_dat$cost_port_to_catch_area <- cost_port_to_catch_area
  temp_dat$cost_port_to_cog <- cost_port_to_cog
  temp_dat$travel_cost <- travel_cost
  
  ## Return data for row choice
  return(temp_dat)
}
  


