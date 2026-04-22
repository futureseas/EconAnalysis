#' Prediction Metrics

#' Function that calculates metrics based on the tow with the highest modeled probability

#' @param choices Choice set
#' @param mod Model results

#' @export

pred_metrics <- function(choices, mod){

  #---------------------------------------------------------------
  ## Load functions
  source("C:\\GitHub\\EconAnalysis\\Functions\\deg2rad.R")
  source("C:\\GitHub\\EconAnalysis\\Functions\\gcd_slc.R")
  #---------------------------------------------------------------
  

  #Add probabilities into the predictions
  fits <- fitted(mod, outcome = FALSE)
  mfits <- melt(fits, byrow = T)
  names(mfits) <- c("rrow", "ccolumn", 'value')
  mfits <- mfits %>% arrange(rrow, ccolumn)
  preds <- as.data.frame(choices)
  preds$probs <- mfits$value
  

  ## -- Correct prediction using max probability value -- ##
  # Filter out the fished tows
  pred_tows <- preds %>% group_by(fished_haul) %>% filter(probs == max(probs)) %>% as.data.frame
  correct_prediction <- sum(pred_tows$fished) / nrow(pred_tows) #score 1
  

  ## -- Calculate distance between the two (fished and prediction) -- #
  fished_tows <- preds %>% filter(fished == TRUE)
  pred_tows <- rbind(fished_tows, pred_tows) %>% arrange(fished_haul)
  tow_dists <- pred_tows %>% group_by(fished_haul) %>% 
    summarize(dist = gcd_slc(long1 = set_long[1], lat1 = set_lat[1], long2 = set_long[2], lat2 = set_lat[2])) %>%
    as.data.frame 
  tow_dists[which(is.na(tow_dists$dist)), 'dist'] <- 0
  tow_dists$correct <- 0
  tow_dists[tow_dists$dist <= 5, "correct"] <- 1
  correct_area <- sum(tow_dists$correct) / nrow(tow_dists) #score 2
  average_distance <- mean(tow_dists$dist) #score 3
  
  # -- Calculate the distance between the fished points and the others --#
  fished_locs <- preds %>% filter(fished == TRUE) %>% select(fished_haul, set_long, set_lat)
  fished_locs <- plyr::rename(fished_locs, c("set_long" = "fished_long", "set_lat" = "fished_lat"))
  preds <- preds %>% left_join(fished_locs, by = "fished_haul")
  preds$distance_from_fished <- gcd_slc(long1 = preds$set_long, lat1 = preds$set_lat,
    long2 = preds$fished_long, lat2 = preds$fished_lat)
  preds[is.na(preds$distance_from_fished), "distance_from_fished"] <- 0
  prob_in_area <- preds %>% filter(distance_from_fished <= 5) %>% group_by(fished_haul) %>%
    summarize(sum_prob = sum(probs)) 
  prob_mass <- mean(prob_in_area$sum_prob) #Score 4
  
  outs <- c(correct_prediction, correct_area, average_distance, prob_mass)
  return(outs)
}
