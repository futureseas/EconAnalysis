#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td2 The tow dates
#' @param dat1 The Data
#' @param hab_dist Distance used to filter for habits and revenues
#' @export

process_dummys2 <- function(xx, td2 = td1, dat1 = dat, hab_dist = 5){
  
  #---------------------------------------------------------------
  ## Load functions
  source("C:\\GitHub\\EconAnalysis\\Functions\\deg2rad.R")
  source("C:\\GitHub\\EconAnalysis\\Functions\\gcd_slc.R")
  #---------------------------------------------------------------
  
  temp_dat <- td2[xx, ]

  fltz <- strsplit(temp_dat$fleet_name, "_")[[1]]
  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past 30 days? FQ: In the same depth_bin???
  dum30 <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$trip_id,
                                              set_date %within% temp_dat$days_inter,
                                              drvid == temp_dat$fished_drvid,
                                              fleet_name %in% fltz)
  dum30 <- dum30 %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances (exclude distance far from actual fished area)
  dum30$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30$set_long), lat2 = deg2rad(dum30$set_lat))
  dum30 <- dum30 %>% filter(dist <= hab_dist)
  
  #Add dummy coefficient
  dum30_val <- nrow(dum30)

  #-----------------------------------------------------------------------------------------------
  # Vessel fish in the past 30 days of last year?
  dum30y <- dat1 %>% ungroup %>% dplyr::filter(haul_id != temp_dat$haul_id, 
                                               set_date %within% temp_dat$prev_year_days_inter,
                                               drvid == temp_dat$fished_drvid,
                                               fleet_name %in% fltz)
  # depth_bin == temp_dat$depth_bin, 
  
  dum30y <- dum30y %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances
  dum30y$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30y$set_long), lat2 = deg2rad(dum30y$set_lat))
  dum30y <- dum30y %>% filter(dist <= hab_dist)
  
  #Add dummy coefficient
  dum30y_val <- nrow(dum30y)

  
#-----------------------------------------------------------------------------------------------
  ## Calculate revenues (obtained by all the fleet within 30 days, excluding the actual haul)
  ##                    (or... the revenues within a finer radius (habit_dist) and from the whole fleet, rather than individual vessel)

  dum_rev <- dat1 %>% ungroup %>% dplyr::filter(haul_id != temp_dat$haul_id,
                                                set_date %within% temp_dat$days_inter,
                                                fleet_name %in% fltz)
  dum_rev <- dum_rev %>% distinct(haul_id, .keep_all = T)

  #Calculate distance
  dum_rev$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
                          long2 = deg2rad(dum_rev$set_long), lat2 = deg2rad(dum_rev$set_lat))
  dum_rev <- dum_rev %>% filter(dist <= hab_dist)

  #Calculate revenue
  mean_rev <- mean(dum_rev$psdn.rev.catch)
  mean_rev <- replace(mean_rev, is.na(mean_rev), 0)

  # Include new information in temp_dat
  temp_dat$mean_rev <- mean_rev
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val

  return(temp_dat)
}
  


