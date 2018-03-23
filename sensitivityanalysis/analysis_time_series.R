setwd("C:/Users/katri/Documents/Uni/MB2 Introduction to Programming and Geostatistics/semester_project")
library(sensitivityanalysis)

#load all needed packages
pkgs <- c("RStoolbox", "rgdal", "raster", "caret", "dplyr")
loadandinstall(pkgs)

#load data
#shapefile: ROI
outline <- readOGR(".", "columbia_icefield")



ratios <-  c("tir/(nir/swir)", "red/swir", "nir/swir", "ndsi")


steps = 1000

#list parameters of all Landsat images
list_images <- list.files("Landsat_images/")
images <-  data.frame(name = list_images,
                      year = as.numeric(substr(list_images, 18, 21)),
                      landsat = as.numeric(substr(list_images, 4, 4)))
images <-  images[order(images$year),]


#########################################################################################################
###DO SENSITIVITY ANALYSIS WITH TIME SERIES OF LANDSAT IMAGES

#build variables to store results
#list of length 4 (four ratios) to store the data.frames derived by the sensitivity analysis
results2 <- vector("list", length(ratios))
names(results2) <- ratios
for (j in (1:length(results2))){
  results2[[j]] <- data.frame(row_number = seq(1, steps+1))
}
#column names of the results variable (threshold, area, difference to optimal area, index)
col_names <- c("row_number")
for (k in (1:nrow(images))){
  col1 <- paste0(images[k,2], "_thd")
  col2 <- paste0(images[k,2], "_area")
  col3 <- paste0(images[k,2], "_diff_area")
  col4 <- paste0(images[k,2], "_ind")
  col_names <- c(col_names, col1, col2, col3, col4)
}



#list of length 4 (four ratios) to store the minimum and maximum thresholds for each year
#(plus the standard deviation and the "optimal" glacier area)
threshold_ranges <- vector("list", length(ratios))
names(threshold_ranges) <- ratios
for (j in (1:length(threshold_ranges))){
  threshold_ranges[[j]] <-  data.frame(year = images$year, threshold = NA, std_ratio = NA, glacier_area = NA,
                               min_threshold = NA, max_threshold = NA)
}





#for loop through all landat images and ratios
for (i in 1:nrow(images)){
  #read in and crop image
  path_to_MTL <- paste("Landsat_images/", images[i,1],"/", images[i,1], "_MTL.txt", sep = "")
  meta <- readMeta(path_to_MTL)
  img <- stackMeta(meta)
  #crop raster to ROI
  img <- crop(img, outline)

  #define band numbers according to Landsat
  if ((images$landsat[i] ==5) | (images$landsat[i] == 7)){
    green <- 2
    red <- 3
    nir <- 4
    swir <- 5
    tir <- 6
  }else if(images$landsat[i] == 8){
    green <- 3
    red <- 4
    nir <- 5
    swir <- 6
    tir <- 10
  }


  for (r in ratios){

      ratio <-   calc_ratio(img, ratio = r,bnbr_green = green, bnbr_red = red,
                            bnbr_nir = nir, bnbr_swir = swir, bnbr_tir = tir)

      #set threshold
      if (r == "ndsi") {
        threshold <- 0.4
      } else {
        threshold <- find_threshold(ratio, plot.hist = TRUE)
      }

      #get binary map and glacier area from "optimal" threshold
      map <- get_binary_map(ratio, threshold, ratio_name = r)
      opt_area <- calc_glacier_area(map)

      #get range of thresholds
      threshold_min <- minValue(ratio)
      #ratio may be infinite (division by 0), exclude these cases when defining the upper limit of the threshold range
      threshold_max <- max(ratio[is.finite(ratio)])

      #do sensitivity analysis (only area calculation as there is no validation data for every year)
      sensitivity <- sensitivity_analysis(raster = ratio, ratio_name = r, threshold_min = threshold_min,
                                          threshold_max = threshold_max, steps = steps)


      #difference of calculated area to area derived from "optimal" threshold
      sensitivity$diff_area <- sensitivity$glacier_area - opt_area


      min_max_thresholds <- get_min_and_max_threshold(thresholds = sensitivity$threshold, values = sensitivity$glacier_area, max_diff = 0.05, value = opt_area)


      #store results in variables
      results2[[which(names(results2) == r)]] <- cbind(results2[[which(names(results2) == r)]], sensitivity)
      threshold_ranges[[which(names(threshold_ranges) == r)]][threshold_ranges[[which(names(threshold_ranges) == r)]]$year == images$year[i], 2:6] <- c(threshold, sd(ratio[is.finite(ratio)]), opt_area, min_max_thresholds)
  }

}

#give results variable their column names
for (j in (1:length(results2))){
  names(results2[[j]]) <- col_names
}


######################################################################################################
### PLOT THE RESULTS


#plot glacier area for all methods (exclude outliers)
color_assignment <- c( A = "blue", B = "green", C="red", D= "black")

ggplot() +
  geom_line(data = threshold_ranges[[1]], aes(x = year, y = glacier_area, colour = "A"))+
  geom_line(data = threshold_ranges[[2]], aes(x = year, y = glacier_area, colour = "B"))+
  geom_line(data = threshold_ranges[[3]], aes(x = year, y = glacier_area, colour = "C"))+
  geom_line(data = threshold_ranges[[4]], aes(x = year, y = glacier_area, colour = "D"))+
  scale_color_manual(values = color_assignment, labels = names(threshold_ranges),name = "Band ratio")

#nir/swir ratio shows some outliers (--> thresholding is not working properly) --> exclude unrealistic values
ggplot() +
  geom_line(data = threshold_ranges[[1]][threshold_ranges[[1]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "A"))+
  geom_line(data = threshold_ranges[[2]][threshold_ranges[[2]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "B"))+
  geom_line(data = threshold_ranges[[3]][threshold_ranges[[3]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "C"))+
  geom_line(data = threshold_ranges[[4]][threshold_ranges[[4]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "D"))+
  scale_color_manual(values = color_assignment, labels = names(threshold_ranges),name = "Band ratio")


for (j in 1: length(threshold_ranges)){
  threshold_ranges[[j]]$min_threshold_std <- (threshold_ranges[[j]]$min_threshold - threshold_ranges[[j]]$threshold)/threshold_ranges[[j]]$std_ratio
  threshold_ranges[[j]]$max_threshold_std <- (threshold_ranges[[j]]$max_threshold - threshold_ranges[[j]]$threshold)/threshold_ranges[[j]]$std_ratio
}


