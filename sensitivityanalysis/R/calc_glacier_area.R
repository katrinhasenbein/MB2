# function to calculate the glacierized area (glacier indicated with 1)
#
#' @title Calculate the glacierized area
#' @description Calculates the glacierized area (in km²) from a map where the glacier is indicated by the value 1
#' @param x raster (binary map) with glacierized pixels containing the value 1
#' @param res_x resolution of the raster in x direction in km. By default 0.03 (resolution of Landsat images).
#' @param res_y resolution of the raster in y direction in km. By default 0.03 (resolution of Landsat images).
#' @examples glacier_area <- calc_glacier_area(binary_map_ndsi)

calc_glacier_area <- function(x, res_x, res_y){
  if (missing(res_x) & missing(res_y)){
    res_x <- 0.03
    res_y <- 0.03
  }
  vals <- getValues(x)
  glacier_cells <- sum(vals ==1, na.rm = TRUE)
  glacier_area <-  sum(vals ==1, na.rm = TRUE) * res_x * res_y
  return(glacier_area)
}
