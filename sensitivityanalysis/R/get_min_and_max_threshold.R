# get the minimum and maximum threshold based on a criterion
#
#
#
#' @title Get the minimum and maximum threshold based on a criterion
#' @description Based on the results of \emph{sensitivity_analysis}. Derives the minimum and maximum
#' threshold meeting on of the following criteria: a) maximum difference of one column from a value or b)
#' minimum value of a column.
#' @param thresholds vector of all possible thresholds
#' @param values vector of values (column of the sensitivity data.frame) which should meet the criterion
#' @param max_diff maximum difference from a value in percent (scenario a)
#' @param value value from which the column value can derogate to a certain degree (scenario a)
#' @param min_value the value tha values vector should not fall below (scenario b)


get_min_and_max_threshold <- function(thresholds, values, max_diff, value, min_value){
  if (missing(min_value)){
    #maximum difference to a value
    min_threshold <- thresholds[min(which((values-value)^2 <= (max_diff*value)^2))]
    max_threshold <- thresholds[max(which((values-value)^2 <= (max_diff*value)^2))]
  } else{
    #minimum value
    min_threshold <- thresholds[min(which(values >= min_value))]
    max_threshold <- thresholds[max(which(values >= min_value))]
  }

  #fit to accuracy
  return(c(min_threshold, max_threshold))
}
