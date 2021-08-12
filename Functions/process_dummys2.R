#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td2 The tow dates
#' @param dat1 The Data
#' @param hab_dist Distance used to filter for habits and revenues
#' @param net_cost Way of netting out costs for revenue calclations
#' @export

process_dummys2 <- function(xx, td2 = td1, dat1 = dat, hab_dist = 5, n_cost){
  temp_dat <- td2[xx, ]

  fltz <- strsplit(temp_dat$fleet_name, "_")[[1]]
  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past 30 days?
  dum30 <- dat1 %>% ungroup %>% filter(haul_id != temp_dat$haul_id, 
    set_date %within% temp_dat$days_inter,
    depth_bin == temp_dat$depth_bin, drvid == temp_dat$fished_drvid, 
    fleet_name %in% fltz)
  dum30 <- dum30 %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances
  dum30$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30$set_long), deg2rad(dum30$set_lat))
  dum30 <- dum30 %>% filter(dist <= hab_dist)
  
  #Add dummy coefficient
  dum30_val <- nrow(dum30)

  #-----------------------------------------------------------------------------------------------
  # Vessel fish in the past 30 days of last year?
  dum30y <- dat1 %>% ungroup %>% filter(haul_id != temp_dat$haul_id, set_date %within% temp_dat$prev_year_days_inter,
    depth_bin == temp_dat$depth_bin, drvid == temp_dat$fished_drvid, 
    fleet_name %in% fltz)
  dum30y <- dum30y %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances
  dum30y$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30y$set_long), deg2rad(dum30y$set_lat))
  dum30y <- dum30y %>% filter(dist <= hab_dist)
  
  #Add dummy coefficient
  dum30y_val <- nrow(dum30y)
  # if(nrow(dum30y) > 0) dum30y_val <- 1
  
  #-----------------------------------------------------------------------------------------------
  #Calculate the revenues within a finer radius and from the whole fleet, rather than individual vessel
# browser()  

  dum_rev <- dat1 %>% ungroup %>% filter(haul_id != temp_dat$haul_id, set_date %within% temp_dat$days_inter,
    depth_bin == temp_dat$depth_bin, fleet_name %in% fltz)
  dum_rev <- dum_rev %>% distinct(haul_id, .keep_all = T)

  #Calculate distance
  dum_rev$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum_rev$set_long), lat2 = deg2rad(dum_rev$set_lat))
  dum_rev <- dum_rev %>% filter(dist <= hab_dist)

  #Calculate revenue in a faster way with fewer if statements
  dum_rev_val <- nrow(dum_rev)

#####Here control whether you use value of all species or just target and groundfish species
  #Right now including all species "tgow_rev"
  # dum_rev[is.na(dum_rev$weak_quota_value), 'weak_quota_value'] <- 0
# browser()  
  mean_rev <- mean(dum_rev$tgow_rev)
  mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
  
  dum_rev_dollars <- mean_rev

#Deprecated features
  # mean_weak <- mean(dum_rev$weak_quota_value, na.rm = T)
  # mean_weak <- replace(mean_weak, is.na(mean_weak), 0)
  
  # dum_rev$quota_cost <- dum_rev$avg_quota_price * dum_rev$apounds
  # mean_qc <- mean(dum_rev$quota_cost, na.rm = T)
  # mean_qc <- replace(mean_qc, is.na(mean_qc), 0)

  # #Calculate different values based on arguments
  # if(n_cost == "trev"){
  #   dum_rev_dollars <- mean_rev
  # }

  # if(n_cost == 'cons'){
  #   dum_rev_dollars <- mean_rev - mean_weak
  # }

  # #quota costs for all species included
  # if(n_cost == "qcos"){
  #   dum_rev_dollars <- mean_rev - mean_qc
  # }
  
  # dum_rev_dollars <- mean_rev - mean_weak
  
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_miss <- dum_rev_val
  temp_dat$miss_rev <- dum_rev_dollars
  return(temp_dat)
  
}
  


