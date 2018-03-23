# calculates the band ratio of a raster
#
#
#
#' @title Calcuate a band ratio
#' @description Caculates band ratios of a raster which are known to be sensitive to glacierized areas.
#' @param raster raster of which the band ratio should be calculated of
#' @param ratio name of the band ratio. Possible values: "red/swir", "nir/swir", "tir/(nir/swir)" and "ndsi".
#' @param bnbr_green number of the green channel (obligatory for "ndsi" ratio)
#' @param bnbr_red number of the green channel (obligatory for "red/swir" ratio)
#' @param bnbr_nir number of the green channel (obligatory for "nir/swir" and "tir/(nir/swir)" ratios)
#' @param bnbr_swir number of the green channel (obligatory for all ratios)
#' @param bnbr_tir number of the green channel (obligatory for "tir/(nir/swir)" ratio)
#' @examples band_ratio <- calc_ratio(raster = landsat_images, ratio = "red/swir", bnbr_red = 4, bnbr_swir = 6)


calc_ratio <- function(raster, ratio,bnbr_green, bnbr_red, bnbr_nir, bnbr_swir, bnbr_tir){
  ratio_raster <- raster
  if (ratio == "red/swir"){
    ratio_raster <- calc(img, fun = function(x){(x[[bnbr_red]]/x[[bnbr_swir]])})
  } else if (ratio == "nir/swir"){
    ratio_raster <- calc(img, fun = function(x){(x[[bnbr_nir]]/x[[bnbr_swir]])})
  } else if (ratio == "tir/(nir/swir)"){
    ratio_raster <- calc(img, fun = function(x){x[[bnbr_tir]]/(x[[bnbr_nir]]/x[[bnbr_swir]])})
  } else if (ratio == "ndsi"){
    ratio_raster <- calc(img, fun = function(x){(x[[bnbr_green]]-x[[bnbr_swir]])/(x[[bnbr_green]]+x[[bnbr_swir]])})
  } else{
    #if ratio is none of the known ratios, print error message.
    print("No ratio calculation. Please give a known ratio.")
  }
  return(ratio_raster)
}
