# function for caculating the area under the curve
#
#' @title Area Under The Curve
#' @description calculates the area under the curve and plots the result. Based on the results of
#' the \emph{\code{\link[=sensitivity_analysis]{sensitivity analysis}}}.
#' @param fpr vector with values of the false positive rate for all possible thresholds (or a range of thresholds)
#' @param tpr vector with values of the true positive rate for all possible thresholds (or a range of thresholds)

#' @param plot.ROC logical. If true, the ROC is plotted
#' @examples   auc <- area_under_curve(fpr = sensitivity_analysis[,5], tpr = sensitivity_analysis[,4],  plot.ROC = TRUE)


area_under_curve <- function(fpr, tpr, plot.ROC){
  aoc <- 0.0
  #set up a data.frame containing fpr and tpr values with increasing fpr
  x <- data.frame(FPR = fpr, TPR = tpr)
  x <- arrange(x, x[,1])
  #sum up the area under the all the pairs of points
  #intervall: (0,0) to (1,1)
  for (i in c(1: (nrow(x)+1))){
    if (i == 1){
      aoc <-  aoc + (0.5*x[i,1]*x[i,2])
    }else if (i > 1 & i <= nrow(x)){
      aoc <-  aoc + (0.5*(x[i,2]+x[i-1,2])*(x[i,1]-x[i-1,1]))
    } else if (i == nrow(x)+1){
      aoc <- aoc +(0.5*(1+x[i-1,2])*(1-x[i-1,1]))
    }
  }
  if (plot.ROC == TRUE){
    #if requested, plot ROC
    plot(x[,1], x[,2], main = "ROC", xlab = "FPR", ylab = "TPR", type = "l")
    abline(a = 0, b = 1)
  }
  return(aoc)
}
