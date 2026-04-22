#---------------------------------------------------------------------------------
setwd("C://Users//peter.kuriyama//SynologyDrive//Research//noaa//future_seas//econ_ch4_example//ch4")

#---------------------------------------------------------------------------------
devtools::install_github("peterkuriyama/ch4")
library(ch4)


#Working example
#Start of obs_dat, work off this script
#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)
library(lubridate)
library(tidyr)
library(mlogit)
library(parallel)

#---------------------------------------------------------------------------------
# load('output//tows_clust_0621.Rdata')
# save(test_dat, file = 'output/test_dat.Rdata')

load(file = 'output/test_dat.Rdata')

samps <- sampled_rums(data_in = test_dat, the_port = "port",
                      min_year = 2012, max_year = 2012, risk_coefficient = 1,
                      ndays = 30, focus_year = 2012, nhauls_sampled = 10, seed = 10, 
                      ncores = 1, rev_scale = 100,
                      model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)

#---------------------------------------------------------------------------------
#Start of big function
#---------------------------------------------------------------------------------
#' Format RUM data base on resampled tows

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
##' @param trip_dists Distances covered by each trip
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param risk_coefficient Coefficient to adjust the quota prices up or down, feeding into net revenue calculations
#' @param ndays Number of previous days data to use in revenue expectations
#' @param focus_year Year to focus on for the models
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor
#' @param net_cost Type of netting of costs;
#' @param habit_distance Distance of spatiotemporal filter
#' @param return_hauls Option to return the hauls before processing dummys, defaults to FALSE
#' 
#' @export

sampled_rums <- function(data_in = filt_clusts, the_port = "ASTORIA / WARRENTON",
                         min_year = 2010, max_year = 2012, risk_coefficient = 1,
                         ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 300, ncores, rev_scale,
                         model_type = 'no_bycatch', net_cost, habit_distance, return_hauls = FALSE){
  #Start by sampling 50 tows within the same fleet  
  #Figure out how close the different clusters are
  #---------------------------------------------------------------
  ##Filter the data
browser()
  dat <- data_in %>% filter(set_year >= min_year, set_year <= max_year, 
                            fleet_name %in% the_port)
  
  #---------------------------------------------------------------
  #Modify the weak stock catches to see if 
  # browser()  
  dat$weak_quota_value <- dat$weak_quota_value * risk_coefficient
  
  #---------------------------------------------------------------
  #Calculate net revenues for each haul
  
  #add placeholder for haul_net_revenue
  dat$net_revenue <- 999
  # browser()
  
  # #net revenues based on constraining species netted out
  # if(net_cost == 'cons'){
  dat <- dat %>% group_by(haul_id) %>% 
    mutate(haul_net_revenue = sum(net_revenue, na.rm = T))  
  # }
  
  # #Use quota costs for all species
  # if(net_cost == "qcos"){
  #   dat %>% group_by(haul_id) %>%
  #     mutate(haul_net_revenue = sum())
  #   #Recalculate 
  # }
  
  # #Total Revenues, no subtraction for constraining species
  # if(net_cost == "trev"){
  #   #add in 1
  # }
  
  #---------------------------------------------------------------
  #Create data set, for each tow
  dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
                                                                    drvid, trip_id, set_day, set_year, haul_net_revenue, set_long, set_lat, 
                                                                    haul_num, avg_long, avg_lat, avg_depth, depth_bin, unq, up_long, up_lat, 
                                                                    weak_quota_value, tg_rev, tgo_rev, tgow_rev) %>% as.data.frame  
  
  dist_hauls_catch_shares <- dist_hauls %>% filter(set_year >= min_year)
  
  #For each tow in the focus year, sample other tows
  #Hauls in focus year
  hauls <- dist_hauls %>% filter(set_year == focus_year) %>% arrange(trip_id, haul_num)
  hauls$prev_haul_num <- hauls$haul_num - 1
  
  #Data frame of previous haul locations
  # prev_hauls <- hauls %>% select(trip_id, haul_num, avg_long, avg_lat)
  prev_hauls <- hauls %>% select(trip_id, haul_num, up_long, up_lat)
  
  #add in zero haul_num values
  zero_hauls <- prev_hauls %>% distinct(trip_id)  
  zero_hauls$haul_num <- 0
  
  port_locs <- dat %>% ungroup %>% distinct(trip_id, d_port_long, d_port_lat)
  zero_hauls <- zero_hauls %>% left_join(port_locs, by = "trip_id")
  zero_hauls <- plyr::rename(zero_hauls, c("d_port_long" = "up_long", 
                                           "d_port_lat" = 'up_lat'))
  
  # zero_hauls$avg_long <- unique(dat$d_port_long)
  # zero_hauls$avg_lat <- unique(dat$d_port_lat)
  
  #Add into previous hauls data frame
  prev_hauls <- rbind(prev_hauls, zero_hauls) %>% arrange(trip_id, haul_num)
  names(prev_hauls)[2:4] <- c('prev_haul_num', "prev_up_long", 'prev_up_lat')
  
  #Add this into the hauls data frame
  hauls <- hauls %>% left_join(prev_hauls, by = c('trip_id', 'prev_haul_num'))
  
  #Calculate depth bin proportions
  # library(dplyr)
  
  dbp <- dist_hauls_catch_shares %>% filter(depth_bin != 69) %>%
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
  
  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls), 
                                    .export = c("sample_hauls"), 
                                    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% 
    sample_hauls(xx = ii, hauls1 = hauls, 
                 dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled,
                 depth_bin_proportions = dbp, the_seed = seedz[ii])
  
  print("Done sampling hauls")  
  sampled_hauls <- plyr::ldply(sampled_hauls)
  
  #-----------------------------------------------------------------------------
  #Calculate revenues from each period
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)
  
  #Add in the vessel that's doing the fishing
  fd <- sampled_hauls %>% filter(fished == TRUE) %>% distinct(fished_haul, drvid)
  fd <- plyr::rename(fd, c("drvid" = 'fished_drvid'))
  # names(fd)[1] <- 'fished_drvid'
  
  sampled_hauls <- sampled_hauls %>% left_join(fd, by = "fished_haul")
  
  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>% 
    select(haul_id, drvid, unq_clust, set_date, prev_days_date, prev_year_set_date, prev_year_days_date,
           avg_long, avg_lat, set_lat, set_long, up_lat, up_long, avg_depth, depth_bin, unq, fished_drvid,
           tgow_rev)
  
  #Look at the unique dates and clusters only
  # td1 <- tow_dates %>% distinct(unq_clust, set_date)
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)
  
  #add in the fleet name
  # paste(the_port, collapse = "_")
  paste_port <- paste(the_port, collapse = "_")
  tow_dates$fleet_name <- paste_port
  
  # td1 <- tow_dates %>% distinct(unq_clust, set_date, .keep_all = T)
  td1 <- tow_dates
  
  #-----------------------------------------------------------------------------  
  #Process dummy variables
  # pd <- process_dummys2(xx = 2, td2 = td1, dat1 = dat)    
  # dummys2 <- foreach::foreach(ii = 1:10000, 
  #     .packages = c("dplyr", 'lubridate', 'ch4')) %dopar% 
  #       process_dummys2(xx = ii, td2 = td1, dat1 = dat, hab_dist = habit_distance, n_cost = net_cost)
  
  
  # yy <- process_dummys2(xx = 2, td2 = td1, dat1 = dat, hab_dist = habit_distance, n_cost = net_cost)
  if(return_hauls == TRUE) {
    stopCluster(cl)
    return(sampled_hauls)
  }
  
  dummys2 <- foreach::foreach(ii = 1:nrow(td1), 
                              .packages = c("dplyr", 'lubridate', 'ch4')) %dopar% 
    process_dummys2(xx = ii, td2 = td1, dat1 = dat, hab_dist = habit_distance, n_cost = net_cost)
  stopCluster(cl)
  
  print("Done calculating dummys and revenues")    
  
  td1 <- ldply(dummys2)
  
  #Check to see that this value is right 
  #converte set_date to character string to merge with the sampled_hauls
  tow_dates$set_date_chr <- as.character(tow_dates$set_date)
  td1$set_date_chr <- as.character(td1$set_date)
  
  #Add in charcater set_date for sampled_hauls
  sampled_hauls$set_date_chr <- as.character(sampled_hauls$set_date)
  # td1 <- td1 %>% select(unq_clust, set_date_chr, dummy_prev_days, dummy_prev_year_days, 
  #   dummy_miss, miss_rev) 
  # sampled_hauls <- sampled_hauls %>% left_join(td1, by = c("unq_clust", "set_date_chr"))
  
  #Add in dummy variable for first tow
  first_hauls <- sampled_hauls %>% filter(fished == TRUE, haul_num == 1) %>% select(fished_haul)
  sampled_hauls$dummy_first <- 0
  sampled_hauls[which(sampled_hauls$fished_haul %in% first_hauls$fished_haul), 'dummy_first'] <- 1
  sampled_hauls$dummy_not_first <- sampled_hauls$dummy_first
  
  sampled_hauls[which(sampled_hauls$dummy_not_first == 1), "dummy_not_first"] <- 2
  sampled_hauls[which(sampled_hauls$dummy_not_first == 0), "dummy_not_first"] <- 1
  sampled_hauls[which(sampled_hauls$dummy_not_first == 2), "dummy_not_first"] <- 0
  
  
  td1[which(td1$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
  td1[which(td1$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1
  
  td1[which(td1$miss_rev != 0), 'dummy_miss'] <- 0
  td1[which(td1$miss_rev == 0), 'dummy_miss'] <- 1
  
  td1$miss_rev_adj <- td1$miss_rev / rev_scale
  
  #Add dummy values into sampled_hauls
  sum(sampled_hauls$haul_id == td1$haul_id)
  
  sampled_hauls <- cbind(sampled_hauls, 
                         td1[, c('dummy_prev_days', 'dummy_prev_year_days', "dummy_miss", 'miss_rev', 'miss_rev_adj')] )
  
  #-----------------------------------------------------------------------------
  #Format as mlogit.data
  rdo <- sampled_hauls %>% select(haul_id, unq_clust, haul_num, distance, fished, fished_haul, 
                                  dummy_prev_days, dummy_prev_year_days, dummy_miss, miss_rev, miss_rev_adj,
                                  dummy_first, dummy_not_first)
  
  rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(haul_id)) %>% as.data.frame 
  
  #-----------------------------------------------------------------------------
  #Fit mlogit models returning the coefficients, the models, and the data going into the 
  #Filter out tows with missing values for distance
  rdo <- rdo %>% filter(is.na(distance) == FALSE)
  remove_all_missing <- rdo %>% group_by(fished_haul) %>% 
    summarize(keep = sum(fished)) %>% filter(keep != 1) %>% as.data.frame
  rdo <- rdo %>% filter(fished_haul %in% remove_all_missing$fished_haul == F)
  
  #Fit the model for everything at once  
  the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'alt_tow',
                          chid.var = 'fished_haul')
  
  if(model_type == "no_bycatch"){
    mf <- mFormula(fished ~ miss_rev_adj * dummy_first + 
                     distance * dummy_first + miss_rev_adj * dummy_not_first +
                     distance * dummy_not_first - distance - miss_rev_adj - 1 - 
                     dummy_first - dummy_not_first + dummy_prev_days + dummy_prev_year_days + dummy_miss)  
  }
  
  if(model_type == 'dummy_bycatch'){
    browser()
  }
  
  res <- mlogit(mf, the_tows)
  
  #List coefficients and rename to align with jeem paper
  coefs <- coef(res)
  
  if(model_type == 'no_bycatch'){
    coefs <- plyr::rename(coefs, c('dummy_prev_days' = 'dum30', 
                                   "dummy_prev_year_days" = "dum30y", "miss_rev_adj:dummy_first" = "rev1",
                                   "dummy_first:distance" = 'dist1', "miss_rev_adj:dummy_not_first" = "rev",
                                   "distance:dummy_not_first" = 'dist', "dummy_miss" = "dmiss"))
    coefs <- data.frame(coefs = round(coefs[c('dist', 'dist1', 'rev', 'rev1', 'dmiss', 'dum30', 'dum30y')],
                                      digits = 5))
    
    ps <- summary(res)$CoefTable[, 4]
    
    ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30', 
                             "dummy_prev_year_days" = "dum30y", "miss_rev_adj:dummy_first" = "rev1",
                             "dummy_first:distance" = 'dist1', "miss_rev_adj:dummy_not_first" = "rev",
                             "distance:dummy_not_first" = 'dist', "dummy_miss" = "dmiss"))
    
    ps <- ps[c('dist', 'dist1', 'rev', 'rev1', 'dmiss','dum30', 'dum30y')]
  }  
  
  #Add significance values
  coefs$p_values <- round(ps, digits = 5)
  coefs$significance <- " "
  coefs[which(coefs$p_values <= .10), 'significance'] <- "."
  coefs[which(coefs$p_values <= .05), 'significance'] <- "*"
  coefs[which(coefs$p_values <= .01), 'significance'] <- "**"
  coefs[which(coefs$p_values <= .001), 'significance'] <- "***"
  
  #Generate and format the predictions  
  preds <- pred_metrics(choices = sampled_hauls, mod = res)
  preds <- data.frame(score1 = preds[1], score2 = preds[2], score3 = preds[3],
                      score4 = preds[4])
  preds$min_year <- min_year
  preds$focus_year <- focus_year
  preds$nhauls_sampled <- nhauls_sampled
  preds$seed <- preds$seed
  preds$risk_coefficient <- risk_coefficient
  preds$rev_scale <- rev_scale
  preds$habit_distance <- habit_distance
  preds$ndays <- ndays
  preds$net_cost <- net_cost
  
  if(length(the_port) > 1) the_port <- paste(the_port, collapse = " and ")
  preds$port <- the_port
  outs <- list(coefs = coefs, mod = res, preds = preds, choices = sampled_hauls)
  return(outs)
  
}



#Scrap Code
#Turn NA prices into zeroes
#   dat[which(is.na(dat$avg_quota_price)), 'avg_quota_price'] <- 0

#   #Adjust the net prices based on risk coefficient
#   dat$rc <- risk_coefficient

#   # print("only weak stock species adjusted for risk")  
#   dat$net_price <- dat$exval_pound
#   dat$net_price <- (dat$exval_pound - dat$rc * dat$avg_quota_price)

#   # weak_inds <- which(dat$type == 'weaks')
#   # dat[weak_inds, 'net_price'] <- dat$exval_pound[weak_inds] - dat$rc[weak_inds] * 
#   #   dat$avg_quota_price[weak_inds]

# ####Change net_price for groundfish and other species to 0
#   # dat[which(dat$type == 'groundfish'), 'net_price'] <- 0
#   dat[which(dat$type == 'other'), 'net_price'] <- 0
# ####  

# ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30', 
#   "dummy_prev_year_days" = "dum30y", "prev_days_rev:dummy_first" = "rev1",
#   "dummy_first:distance" = 'dist1', "prev_days_rev:dummy_not_first" = "rev",
#   "distance:dummy_not_first" = 'dist'))

#Make sure that missing values have dummy variable value of 1 for prev_days
# sampled_hauls[which(sampled_hauls$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
# sampled_hauls[which(sampled_hauls$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1
# sampled_hauls[which(sampled_hauls$miss_rev != 0), 'dummy_miss'] <- 0
# sampled_hauls[which(sampled_hauls$miss_rev == 0), 'dummy_miss'] <- 1
# sampled_hauls$miss_rev_adj <- sampled_hauls$miss_rev / rev_scale    

# browser()
# dummys22 <- ldply(dummys2)
#Compare the dummy calculations
# dummys <- foreach::foreach(ii = 1:nrow(td1), 
#   .packages = c("dplyr", 'lubridate', 'ch4')) %dopar% 
#     process_dummys(xx = ii, td2 = td1, dat1 = dat)
# dummys1 <- ldply(dummys)

#Change values to be 0 and 1 for dummy variables
# tow_dates <- cbind(tow_dates, dummys1)
# td1 <- cbind(td1, dummys1)

# sample_hauls(xx = 1, hauls1 = hauls, 
#   dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled,
#   depth_bin_proportions = dbp)

