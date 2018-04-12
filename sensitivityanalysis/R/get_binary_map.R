# function to derive a binary map from a raster containing the values of a band ratio
#
#
#' @title Derive a binary map
#' @description Get a binary map (glacier/no glacier) from a raster containing the values of a band ratio. Based on a given threshold.
#' @param ratio raster of the band ratio
#' @param threshold threshold that should be applied on the raster
#' @param ratio_name name of the ratio.  Possible values: "red/swir", "nir/swir", "tir/(nir/swir)" and "ndsi".
#' @examples binary_map <- get_binary_map(ratio = ndsi_ratio, threshold = 0.4, ratio_name = "ndsi")

get_binary_map <- function(ratio, threshold, ratio_name){

  if(ratio_name == "tir/(nir/swir)"){
    #for ratio "tir/(nir/swir)" all pixels with the value < threshold are classified as glacier
    binary_map <- calc(ratio, fun = function(x){ifelse (x < threshold, 1, 0)})
  } else{
    #for all other ratios pixels > threshold are classified as glacier
    binary_map <- calc(ratio, fun = function(x){ifelse (x > threshold, 1, 0)})
  }
  return (binary_map)
}
