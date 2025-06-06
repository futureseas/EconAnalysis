#' Format RUM data base on resampled tows

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param ndays Number of previous days data to use in revenue expectations
#' @param focus_year Year to focus on for the models
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor
#' @param habit_distance Distance of spatiotemporal filter
#' @param return_hauls Option to return the hauls before processing dummys, defaults to FALSE
#' @param exp_rev Type of expected revenue (from sdm or catch)
#'
#' @export

sampled_rums <- function(data_in = filt_clusts, the_port = "ASTORIA / WARRENTON",
  min_year = 2010, max_year = 2012, ndays = 60, focus_year = 2012, nhauls_sampled = 50, 
  seed = 300, ncores, rev_scale, habit_distance, 
  return_hauls = FALSE, exp_rev = "sdm"){

#Start by sampling 50 tows within the same fleet
#Figure out how close the different clusters are
  
  
  #---------------------------------------------------------------
  ##Filter the data

  dat <- data_in %>% dplyr::filter(set_year >= min_year, set_year <= max_year,
    fleet_name %in% the_port)

  #---------------------------------------------------------------
  # #Modify the weak stock catches 
  # dat$weak_quota_value <- dat$weak_quota_value * risk_coefficient

  #---------------------------------------------------------------
  #Calculate net revenues for each haul
  dat <- dat %>% group_by(haul_id) %>%
    mutate(haul_net_revenue.catch = sum(psdn.rev.catch, na.rm = T)) %>%
    mutate(haul_net_revenue.sdm = sum(psdn.rev.sdm, na.rm = T))

  #---------------------------------------------------------------
  #Create data set, for each tow
  dist_hauls <- dat %>% 
    distinct(haul_id, .keep_all = T) %>% 
    dplyr::select(haul_id, set_month, drvid, trip_id, set_day, set_year, 
      set_long, set_lat, haul_num, depth_bin, up_long, up_lat,
      haul_net_revenue.sdm, haul_net_revenue.catch) %>% 
    as.data.frame

  dist_hauls_catch_shares <- dist_hauls %>% dplyr::filter(set_year >= min_year)

  #For each tow in the focus year, sample other tows
  #Hauls in focus year
  hauls <- dist_hauls %>% dplyr::filter(set_year == focus_year) %>% arrange(trip_id, haul_num)
  hauls$prev_haul_num <- hauls$haul_num - 1

  #Data frame of previous haul locations
  prev_hauls <- hauls %>% dplyr::select(trip_id, haul_num, up_long, up_lat)

  #add in zero haul_num values
  zero_hauls <- prev_hauls %>% distinct(trip_id)
  zero_hauls$haul_num <- 0

  port_locs <- dat %>% ungroup %>% distinct(trip_id, d_port_long, d_port_lat)
  zero_hauls <- zero_hauls %>% left_join(port_locs, by = "trip_id")
  zero_hauls <- plyr::rename(zero_hauls, c("d_port_long" = "up_long",
    "d_port_lat" = 'up_lat'))

  #Add into previous hauls data frame
  prev_hauls <- rbind(prev_hauls, zero_hauls) %>% arrange(trip_id, haul_num)
  names(prev_hauls)[2:4] <- c('prev_haul_num', "prev_up_long", 'prev_up_lat')

  #Add this into the hauls data frame
  hauls <- hauls %>% left_join(prev_hauls, by = c('trip_id', 'prev_haul_num'))

  #Calculate depth bin proportions
  dbp <- dist_hauls_catch_shares %>% dplyr::filter(depth_bin != 69) %>%
    group_by(depth_bin) %>% summarize(nvals = length(unique(haul_id))) %>%
    mutate(tot_nvals = sum(nvals), prop = nvals / tot_nvals)

  dbp <- as.data.frame(dbp)

  #Add number of values to sample
  dbp$n_samp <- dbp$prop * nhauls_sampled

  #Round the values to integers
  dbp$n_samp <- round(dbp$n_samp)

  #Top off the highest value
  max_dbp <- which(dbp$prop == max(dbp$prop))
  dbp[max_dbp, 'n_samp'] <- dbp[max_dbp, 'n_samp'] + (nhauls_sampled - sum(round(dbp$n_samp)))

  #-----------------------------------------------------------------------------
  #Sample Hauls
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls))

  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  source("C:\\GitHub\\EconAnalysis\\Functions\\sample_hauls.R")

  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls),
    .export = c("sample_hauls"),
    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% {
      sample_hauls(xx = ii, hauls1 = hauls,
        dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled,
        depth_bin_proportions = dbp, the_seed = seedz[ii])
    }
  print("Done sampling hauls")
  sampled_hauls <- plyr::ldply(sampled_hauls)
  
  #Add in the vessel that's doing the fishing
  fd <- sampled_hauls %>% dplyr::filter(fished == TRUE) %>% distinct(fished_haul, drvid)
  fd <- plyr::rename(fd, c("drvid" = 'fished_drvid'))
  sampled_hauls <- sampled_hauls %>% left_join(fd, by = "fished_haul")

  #Obtain previous day, year and day/year date
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)

  if(return_hauls == TRUE) {
    stopCluster(cl)
    return(sampled_hauls)
  }
  
  #-----------------------------------------------------------------------------
  ### Calculate interval between previews day and year
  
  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>%
    dplyr::select(haul_id, drvid, set_date, prev_days_date, prev_year_set_date, prev_year_days_date,
                  set_lat, set_long, up_lat, up_long, depth_bin, fished_drvid, haul_net_revenue.sdm, haul_net_revenue.catch)

  #calculate intervals
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)

  #add in the fleet name
  paste_port <- paste(the_port, collapse = "_")
  tow_dates$fleet_name <- paste_port
  td1 <- tow_dates


  #-----------------------------------------------------------------------------
  ### Calculate revenues (LATER!) from each period and process dummy variables

  dummys2 <- foreach::foreach(ii = 1:nrow(td1),
    .packages = c("dplyr", 'lubridate')) %dopar% {
      source("C:\\GitHub\\EconAnalysis\\Functions\\process_dummys2.R")
      process_dummys2(xx = ii, td2 = td1, dat1 = dat, hab_dist = habit_distance)
    }
  print("Done calculating dummys and revenues")
  stopCluster(cl)
  
  td1 <- ldply(dummys2)

  #-----------------------------------------------------------------------------
  ##Create additional dummys
  
  #Convert set_date to character string to merge with the sampled_hauls
  tow_dates$set_date_chr <- as.character(tow_dates$set_date)
  td1$set_date_chr <- as.character(td1$set_date)
  sampled_hauls$set_date_chr <- as.character(sampled_hauls$set_date)

  #Add in dummy variable for first tow
  first_hauls <- sampled_hauls %>% dplyr::filter(fished == TRUE, haul_num == 1) %>% dplyr::select(fished_haul)
  sampled_hauls$dummy_first <- 0
  sampled_hauls[which(sampled_hauls$fished_haul %in% first_hauls$fished_haul), 'dummy_first'] <- 1
  sampled_hauls$dummy_not_first <- sampled_hauls$dummy_first

  sampled_hauls[which(sampled_hauls$dummy_not_first == 1), "dummy_not_first"] <- 2
  sampled_hauls[which(sampled_hauls$dummy_not_first == 0), "dummy_not_first"] <- 1
  sampled_hauls[which(sampled_hauls$dummy_not_first == 2), "dummy_not_first"] <- 0

  td1[which(td1$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
  td1[which(td1$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1

  td1[which(td1$mean_rev != 0), 'dummy_miss'] <- 0
  td1[which(td1$mean_rev == 0), 'dummy_miss'] <- 1

  td1$mean_rev_adj <- td1$mean_rev / rev_scale

  sampled_hauls <- cbind(sampled_hauls,
    td1[, c('dummy_prev_days', 'dummy_prev_year_days', "dummy_miss", 'mean_rev', 'mean_rev_adj')] )

  #-----------------------------------------------------------------------------
  #Format as mlogit.data
  rdo <- sampled_hauls %>% dplyr::select(haul_id, haul_num, distance, fished, fished_haul,
    dummy_prev_days, dummy_prev_year_days, dummy_first, dummy_not_first, set_lat, set_long, haul_net_revenue.sdm, haul_net_revenue.catch,
    dummy_miss, mean_rev, mean_rev_adj)

  rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(haul_id)) %>% as.data.frame

  #-----------------------------------------------------------------------------
  ##Fit mlogit models returning the coefficients, the models, and the data going into the model

  #Filter out tows with missing values for distance
  rdo <- rdo %>% dplyr::filter(is.na(distance) == FALSE)
  remove_all_missing <- rdo %>% group_by(fished_haul) %>%
    summarize(keep = sum(fished)) %>% 
    dplyr::filter(keep != 1) %>% 
    as.data.frame
  rdo <- rdo %>% dplyr::filter(fished_haul %in% remove_all_missing$fished_haul == F)
  
  #Fit the model for everything at once
  the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'alt_tow', chid.var = 'fished_haul')
  
  if(exp_rev == "sdm"){
    mf <- mFormula(fished ~  distance * dummy_first + distance * dummy_not_first + dummy_prev_days + dummy_prev_year_days + haul_net_revenue.sdm 
                   - 1 - dist - distance - dummy_first - dummy_not_first)
    res <- mlogit(mf, the_tows)
    
    #List coefficients and rename to align with jeem paper
    coefs <- coef(res)
    coefs <- plyr::rename(coefs, c('dummy_prev_days' = 'dum30',
                                   "dummy_prev_year_days" = "dum30y", "distance:dummy_first" = 'dist1',
                                   "distance:dummy_not_first" = 'distN',
                                   'haul_net_revenue.sdm' = 'rev'))
    coefs <- data.frame(coefs = round(coefs[c('distN', 'dist1', 'dum30', 'dum30y', 'rev')],
      digits = 5))
    
    ps <- summary(res)$CoefTable[, 4]

    ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30',
                             "dummy_prev_year_days" = "dum30y", "distance:dummy_first" = 'dist1',
                             "distance:dummy_not_first" = 'distN', 
                             'haul_net_revenue.sdm' = 'rev'))
    
    ps <- ps[c('dist1', 'distN', 'dum30', 'dum30y', 'rev')]
  }  
  
  if(exp_rev == "catch"){
    mf <- mFormula(fished ~ mean_rev_adj + distance * dummy_first + distance * dummy_not_first 
                   + dummy_prev_days + dummy_prev_year_days + dummy_miss 
                   - 1 - distance - dummy_first - dummy_not_first)
    res <- mlogit(mf, the_tows)
    
    #List coefficients and rename to align with jeem paper
    coefs <- coef(res)

    coefs <- plyr::rename(coefs, c('dummy_prev_days' = 'dum30', "dummy_prev_year_days" = "dum30y",
                                   "mean_rev_adj" = "rev",
                                   "distance:dummy_first" = 'dist1', "distance:dummy_not_first" = 'distN',
                                   "dummy_miss" = "dmiss"))
    coefs <- data.frame(coefs = round(coefs[c('dist1', 'distN', 'rev', 'dmiss', 'dum30', 'dum30y')],
      digits = 5))

    ps <- summary(res)$CoefTable[, 4]

    ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30', "dummy_prev_year_days" = "dum30y", 
                             "mean_rev_adj" = "rev",
                             "distance:dummy_first" = 'dist1', "distance:dummy_not_first" = 'distN',
                             "dummy_miss" = "dmiss"))
    ps <- ps[c('dist1', 'distN', 'rev', 'dmiss', 'dum30', 'dum30y')]
  }
  
  print("Done with estimating discrete choice model")

  #Add significance values
  coefs$p_values <- round(ps, digits = 5)
  coefs$significance <- " "
  coefs[which(coefs$p_values <= .10), 'significance'] <- "."
  coefs[which(coefs$p_values <= .05), 'significance'] <- "*"
  coefs[which(coefs$p_values <= .01), 'significance'] <- "**"
  coefs[which(coefs$p_values <= .001), 'significance'] <- "***"
  
  #Generate and format the predictions
  source("C:\\GitHub\\EconAnalysis\\Functions\\pred_metrics.R")
  preds <- pred_metrics(choices = rdo, mod = res)
  print("Done calculating predictive metrics")
  preds <- data.frame(score1 = preds[1], score2 = preds[2], score3 = preds[3], score4 = preds[4])
  preds$min_year <- min_year
  preds$focus_year <- focus_year
  preds$nhauls_sampled <- nhauls_sampled
  preds$seed <- preds$seed
  preds$rev_scale <- rev_scale
  preds$habit_distance <- habit_distance
  preds$ndays <- ndays

  if(length(the_port) > 1) the_port <- paste(the_port, collapse = " and ")
   preds$port <- the_port

  ## Output from the model
  outs <- list(coefs = coefs, mod = res, preds = preds, choices = sampled_hauls, data = rdo)  
  return(outs)
}
