# Do a sensitivity analysis
#
#
#' @title Sensitivity analysis of thresholding a band ratio
#' @aliases sensitivity_analysis
#' @description Calculates the glacier area for a range of thresholds. If validation data is available,
#' it also caculates accuracy measures (overall accuracy, false positive ratio and true positive ratio).
#' The results for the FPR and TPR can then be used to calculate the \emph{\code{\link[=area_under_curve]{Area under The Curve}}}
#' (AOC).
#'
#' @param raster raster of the band ratio
#' @param val_raster if available: raster with the ground truth as validation data
#' @param ratio_name name of the ratio (how \emph{raster} is calculated)
#' @param threshold_min minimum threshold. Lower end of the threshold range.
#' @param threshold_max maximum threshold. Upper end of the threshold range.
#' @param steps number of steps within the range of thresholds
#'
#'

sensitivity_analysis <- function(raster, val_raster, ratio_name, threshold_min, threshold_max, threshold, steps){

  #create empty variables to store results
  if (missing(val_raster)){
    #if now validation data is available, only the glacier_area can be calculated
    df <- data.frame(threshold = numeric(), glacier_area = numeric(), index = numeric())
  }else{
    #if validation data is available, accuracy measures can be calculated as well
    df <- data.frame(threshold = numeric(), glacier_area = numeric(),
                      overall_accuracy = numeric(), TPR = numeric(), FPR = numeric(), index = numeric())
  }

  #intervall of each step is calculated from the threshold range and the number of steps
  step = (threshold_max-threshold_min)/steps
  #calculate the parameters for each threshold within the range
  for (t in seq(threshold_min, threshold_max, step)){

    bin_map <- get_binary_map(raster, t, ratio_name)
    area <- calc_glacier_area(bin_map)

    if (missing(val_raster)){
      #bind result to data.frame
      df <- rbind(df, c(t, area))
    } else{
      #if validation data is available, also calculate accuracy measures
      pred_data <- getValues(bin_map)
      ref_data <- getValues(val_raster)
      #build confusion matrix to derive accuracy measures
      confusion_matrix <- confusionMatrix(data=pred_data, reference=ref_data)
      acc <- confusion_matrix$overall[1]
      tpr <- confusion_matrix$byClass[1] #sensitivity
      fpr <- 1 - confusion_matrix$byClass[2] #1-specificity
      #bind results to matrix
      df <- rbind(df, c(t, area, acc, tpr, fpr))
    }
  }

  #name the columns
  if(missing(val_raster)){
    names(df) <- c("threshold", "glacier_area")
  }else{
    names(df) <- c("threshold", "glacier_area", "overall_accuracy", "TPR", "FPR")
  }

  #column with an index: threshold which is closest to optimal threshold: index = 0
  #negative for thresholds < optimal threshold, positive for thresholds > optimal threshold

  index_min <-  -((which.min(abs(df$threshold - threshold)))-1)
  index_max <- length(df$threshold) -  which.min(abs(df$threshold - threshold))
  indices <- seq(index_min, index_max, 1)
  df$index <- indices

  #order in ascending order of threshold
  df <-  df[order(df$threshold),]
  return(df)
}
