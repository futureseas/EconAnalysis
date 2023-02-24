#' Prediction Metrics

#' Function that calculates metrics based on the tow with the highest modeled probability

#' @param choices Choice set
#' @param mod Model results

#' @export

pred_metrics <- function(choices, mod){


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
  

  outs <- c(correct_prediction)
  return(outs)
}
