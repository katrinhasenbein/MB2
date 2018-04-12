# function to load and install packages
#
#' @title Load and install packages
#' @description Loads all packages and installs them in case they are not installed yet.
#' @param pkglist vector with all needed packages
#' @examples packages <- c("ggplot2", "raster")
#' @examples loadandinstall(packages)

loadandinstall <- function(pkglist) {
  for (i in pkglist){
    #check if package is already installed, if not install it
    if (!is.element(i, installed.packages()[,1])){
      install.packages(i)
    }
    #load all packages in list
    library(i, character.only = TRUE)
  }
}
