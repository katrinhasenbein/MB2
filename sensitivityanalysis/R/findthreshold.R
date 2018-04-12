# find the as best considered threshold based on the histogram of the band ratio
#
#' @title Find threshold for a band ratio
#' @description {Finds the as best considered threshold based on the histogram of the raster.
#' The histogram is considered to be bimodal representing the two classes.
#' The best threshold is considered to be at position of the local minimum between the two humps.}
#' @param raster raster (ideally with a bimodel distribution of values)
#' @param plot.hist logical. If true, histogram with threshold indicated in darkred is plotted. For visual check of the validity of the threshold.

find_threshold <- function(raster, plot.hist){
  #save points of density plot in a data.frame
  density_line <- density(raster, plot = FALSE)
  density_df = data.frame(x = density_line[1], y = density_line[2])

  #add two columns to the data.frame giving the previous and next value of the density line
  density_df$y_next <- shift.vec(density_df$y, -1)
  density_df$y_prev <- shift.vec(density_df$y, 1)

  #find the local minima of the density line (where the previous and next values are greater than the actual value)
  threshold <-  density_df[density_df$y < density_df$y_prev & density_df$y < density_df$y_next,]
  #take the first threshold containing a value as the threshold
  threshold <-  threshold[!is.na(threshold$x),]
  threshold <- threshold[1,1]

  if (plot.hist == TRUE){
    #if requested, plot the density line with the threshold indicated as a vertical line
    lines(density(raster))
    abline(v = threshold, col = "darkred")
  }

  if(is.na(threshold)){
    #if no threshold could be found, print error message
    print("No threshold could be found. Take a look at the histogram of your raster
          to make sure that it has a bimodal distribution.")
  }

  return(threshold)
}
