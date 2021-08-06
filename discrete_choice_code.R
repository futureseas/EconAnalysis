################################################################# 
## Sampling choice set based on Peter's code for Hicks's paper ##
#################################################################


  #Sample Hauls  
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls)) # changes hauls for trips?
  
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