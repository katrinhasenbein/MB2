#########################################################################################################
### Script for a sensitivity analysis of thresholding different band ratios
### Author: Katrin Hasenbein
### March 2018
########################################################################################################

########################################################################################################
### Background information
###
### Study area: Columbia Icefield, Canada
### For the identification of glacierized area, it is a common method to threshold a band ratio.
### The most popular band ratios are: red/swir, nir/swir, tir/(nir(swir)), ndsi
### This script analyses the sensitvity of the thresholds of these ratios in the context of the glacier area
### and the accuracy.
###
### As validation data the glacier outlines from the The Randolph Glacier Inventory 5.0 are used
### (glacier outline of 2004).
### See https://www.glims.org/RGI/rgi50_dl.html for more information.
#########################################################################################################

#########################################################################################################
### preparations

#set working directory
setwd("C:/Users/katri/Documents/Uni/MB2 Introduction to Programming and Geostatistics/semester_project")
#load package which was built for the sensitivity analysis
library(sensitivityanalysis)
#load (and install) all needed packages
pkgs <- c("RStoolbox", "rgdal", "raster", "caret", "dplyr", "fmsb", "reshape2")
loadandinstall(pkgs)


#load data
#landsat data from the year 2004
meta <- readMeta("Landsat_images/LT05_L1TP_044024_20040813_20161130_01_T1/LT05_L1TP_044024_20040813_20161130_01_T1_MTL.txt")
img <- stackMeta(meta)
#shapefile of the Region of Interest
outline <- readOGR(".", "columbia_icefield")
#validation data
#note: the validation shapefile is nor delivered correctly (holes are not indicated as such)
#shapefile had to be manually prepocessed in QGIS and was resampled and cropped to the ROI in the course of
#the prepocessing.
valdata <- raster("valdata.tif")
valdata[is.na(valdata)] <- 0
#calculate true glacier area
true_glacier_area <- calc_glacier_area(valdata)

#crop landsat image to ROI
img <- crop(img, outline)

#list parameters of all Landsat images (time series)
list_images <- list.files("Landsat_images/")
images <-  data.frame(name = list_images,
                      year = as.numeric(substr(list_images, 18, 21)),
                      landsat = as.numeric(substr(list_images, 4, 4)))
images <-  images[order(images$year),]


#########################################################################################################
###set parameter (number of steps within the threshold ranges)
steps <- 1000


#######################################################################################################
#### DO SENSITIVITY ANALYSIS
####


#vector of the ratios
ratios <-  c("tir/(nir/swir)", "red/swir", "nir/swir", "ndsi")

#build empty variables to store the results
results <- vector("list", 4)
names(results) <- ratios
model_parameters <- data.frame(ratio = ratios, std_ratio = NA,
                               opt_threshold_hist = NA, opt_area_hist = NA, opt_acc_hist = NA,
                               opt_threshold_AUC = NA, opt_area_AUC = NA, opt_acc_AUC = NA,
                               opt_threshold_accuracy = NA, opt_area_accuracy = NA, opt_acc_accuracy = NA,
                               opt_threshold_area = NA, opt_area_area = NA, opt_acc_area = NA,
                               AUC = NA, stringsAsFactors=FALSE)




for (i in ratios){
  ratio <-   calc_ratio(img, ratio = i,bnbr_green = 2, bnbr_red = 3, bnbr_nir = 4, bnbr_swir = 5, bnbr_tir = 6)
  # if (i == "ndsi") {
  #   #for the ndsi a fixed threshold of 0.4 is used
  #   threshold <- 0.4
  # } else {
    threshold <- find_threshold(ratio, plot.hist = TRUE)
  # }

  #standard deviation of the ratio values
  std_ratio <- sd(getValues(ratio))

  #get binary map, glacier area and accuracy of map based on "optimal" threshold
  map <- get_binary_map(ratio, threshold, ratio_name = i)
  opt_area <- calc_glacier_area(map)

  pred_data <- getValues(map)
  ref_data <- getValues(valdata)
  confusion_matrix <- confusionMatrix(data=pred_data, reference=ref_data)
  opt_accuracy <- confusion_matrix$overall[1]

  #get the range of all possible thresholds
  threshold_min <- minValue(ratio)
  threshold_max <- maxValue(ratio)

  #do sensitivity analysis
  sensitivity <- sensitivity_analysis(raster = ratio, val_raster = valdata, ratio_name = i,
                                      threshold_min = threshold_min,threshold_max = threshold_max,
                                      threshold = threshold, steps = steps)

  #calculate area under the curve
  auc <- area_under_curve(fpr = sensitivity[,5], tpr = sensitivity[,4],  plot.ROC = TRUE)




  #other criteria to define "best" threshold
  #point which is closest to (0,1) in ROC
  sensitivity$diff_to_ROC_optimum <- sqrt((sensitivity$FPR)^2 + (1-sensitivity$TPR)^2)
  ind_best_auc <- which(sensitivity$diff_to_ROC_optimum == min(sensitivity$diff_to_ROC_optimum))
  best_auc <-  sensitivity[ind_best_auc, c(1:3)]
  #area closest to true area
  ind_best_area <- which.min(abs(sensitivity$glacier_area - true_glacier_area))
  best_area <- sensitivity[ind_best_area, c(1:3) ]
  #threshold with highest accuracy
  ind_best_accuracy <-  which.max(sensitivity$overall_accuracy)
  best_accuracy <- sensitivity[ind_best_accuracy, c(1:3)]




  #save results in variables
  results[[which(ratios == i)]] <- sensitivity
  # data.frame with "best" thresholds, glacier area and accuracy in context of thresholding the histogram (hist),
  # best point in ROC (auc), best accuracy (accuracy) and best area estimate (area)
  model_parameters[model_parameters$ratio == i, 2:ncol(model_parameters)] <- c(std_ratio, threshold, opt_area, opt_accuracy,
                                                                               best_auc, best_accuracy, best_area,
                                                                               auc)

}


##################################################################################################################
### PLOT RESULTS FROM SENSITIVITY ANALYSIS
###


color_assignment <- c( A = "blue", B = "green", C="red", D= "black")

#plot accuracy of different methods over range of thresholds
ggplot()+
  geom_line(data = results[[1]], aes(x = index, y = overall_accuracy, col = "A"))+
  geom_line(data = results[[2]], aes(x = index, y = overall_accuracy, col = "B"))+
  geom_line(data = results[[3]], aes(x = index, y = overall_accuracy, col = "C"))+
  geom_line(data = results[[4]], aes(x = index, y = overall_accuracy, col = "D"))+
  geom_vline(xintercept=0) +
  scale_color_manual(values = color_assignment,
                     labels = names(results), name = "Band ratio")

#closer look at range of thresholds with a accuracy > 0.95
ggplot()+
  geom_line(data = results[[1]][results[[1]]$overall_accuracy > 0.95,], aes(x = index, y = overall_accuracy, col = "A"))+
  geom_line(data = results[[2]][results[[2]]$overall_accuracy > 0.95,], aes(x = index, y = overall_accuracy, col = "B"))+
  geom_line(data = results[[3]][results[[3]]$overall_accuracy > 0.95,], aes(x = index, y = overall_accuracy, col = "C"))+
  geom_line(data = results[[4]][results[[4]]$overall_accuracy > 0.95,], aes(x = index, y = overall_accuracy, col = "D"))+
  geom_vline(xintercept=0) +
  scale_color_manual(values = color_assignment,
                     labels = names(results), name = "Band ratio")+
  ggtitle("Accuracy of different band ratios over the range of thresholds")


#plot change of glacier area over range of thresholds
ggplot()+
  geom_line(data = results[[1]], aes(x = index, y = glacier_area, col = "A"))+
  geom_line(data = results[[2]], aes(x = index, y = glacier_area, col = "B"))+
  geom_line(data = results[[3]], aes(x = index, y = glacier_area, col = "C"))+
  geom_line(data = results[[4]], aes(x = index, y = glacier_area, col = "D"))+
  geom_vline(xintercept=0) +
  geom_hline(yintercept = true_glacier_area)+
  scale_color_manual(values = color_assignment, labels = names(results), name = "Band ratio")

fill_assignment = c("Fill" = "orange")

#closer look
ggplot()+
  geom_line(data = results[[1]][results[[1]]$glacier_area > 0.9*true_glacier_area &results[[1]]$glacier_area < 1.1*true_glacier_area,], aes(x = index, y = glacier_area, col = "A"))+
  geom_line(data = results[[2]][results[[2]]$glacier_area > 0.9*true_glacier_area &results[[2]]$glacier_area < 1.1*true_glacier_area,], aes(x = index, y = glacier_area, col = "B"))+
  geom_line(data = results[[3]][results[[3]]$glacier_area > 0.9*true_glacier_area &results[[3]]$glacier_area < 1.1*true_glacier_area,], aes(x = index, y = glacier_area, col = "C"))+
  geom_line(data = results[[4]][results[[4]]$glacier_area > 0.9*true_glacier_area &results[[4]]$glacier_area < 1.1*true_glacier_area,], aes(x = index, y = glacier_area, col = "D"))+
  #geom_vline(xintercept=0) +
  geom_hline(yintercept = true_glacier_area)+
  #geom_hline(yintercept = 0.95*true_glacier_area, colour = "gray")+
  #geom_hline(yintercept = 1.05*true_glacier_area, colour = "gray")+
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=0.95*true_glacier_area, ymax=1.05*true_glacier_area, fill = "Fill"), alpha = .3)+
  scale_color_manual(values = color_assignment, labels = names(results), name = "Band ratio")+
  scale_fill_manual(values = fill_assignment, labels = c("±5% of true area"), name = "")
  ggtitle("Glacier area from different band ratios over the range of thresholds")


#AUC of different ratios
ggplot()+
  geom_line(data = results[[1]], aes(x = FPR, y = TPR, col = "A"))+
  geom_line(data = results[[2]], aes(x = FPR, y = TPR, col = "B"))+
  geom_line(data = results[[3]], aes(x = FPR, y = TPR, col = "C"))+
  geom_line(data = results[[4]], aes(x = FPR, y = TPR, col = "D"))+
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) + scale_y_continuous(limits= c(0,1), expand = c(0,0))+
  scale_color_manual(values = color_assignment, labels = names(results), name = "Band ratio")

model_parameters[, c(1,15)]

##################################################################################################################
### Quantify sensitivity of thresholds

##+/-5% of true glacier area and more than 95% accuracy
for (i in c(1:4)){
  min_max_thresholds_area <- get_min_and_max_threshold(thresholds = results[[i]]$threshold, values = results[[i]]$glacier_area, max_diff = 0.05, value = true_glacier_area)

  model_parameters$min_threshold_095area[model_parameters$ratio == names(results)[i]] <- min_max_thresholds_area[1]
  model_parameters$max_threshold_105area[model_parameters$ratio == names(results)[i]] <- min_max_thresholds_area[2]


  min_max_thresholds_accuracy <- get_min_and_max_threshold(thresholds = results[[i]]$threshold, values = results[[i]]$overall_accuracy, min_value = 0.95)

  model_parameters$min_threshold_095accuracy[model_parameters$ratio == names(results)[i]] <- min_max_thresholds_accuracy[1]
  model_parameters$max_threshold_095accuracy[model_parameters$ratio == names(results)[i]] <- min_max_thresholds_accuracy[2]

}

#range of possible thresholds as a factor of the standard deviation of the ratio values
model_parameters$min_threshold_095area_sd <- (model_parameters$min_threshold_095area-model_parameters$opt_threshold_hist)/model_parameters$std_ratio
model_parameters$max_threshold_105area_sd <- (model_parameters$max_threshold_105area-model_parameters$opt_threshold_hist)/model_parameters$std_ratio

model_parameters$min_threshold_095accuracy_sd <- (model_parameters$min_threshold_095accuracy-model_parameters$opt_threshold_hist)/model_parameters$std_ratio
model_parameters$max_threshold_095accuracy_sd <- (model_parameters$max_threshold_095accuracy-model_parameters$opt_threshold_hist)/model_parameters$std_ratio

model_parameters$total_range_sd_accuracy <- model_parameters$max_threshold_095accuracy_sd - model_parameters$min_threshold_095accuracy_sd
model_parameters$total_range_sd_area <- model_parameters$max_threshold_105area_sd - model_parameters$min_threshold_095area_sd

#print results
model_parameters[, c(1,20:25)]

test <- model_parameters[,c(1, 20:23)]
#names(test) <- c("ratio", "area", "area", "accuracy", "accuracy")
test <- melt(test, id.vars = c("ratio"))
test$variable <-  c(rep("area", 8), rep("accuracy", 8))
test$group <- paste(test$ratio, test$variable, sep = "_")
test_area <- test[test$variable == "area",]
test_accuracy <- test[test$variable == "accuracy",]

ggplot(data = test, aes(x = group, y = value))+
  stat_summary(fun.ymin="min",fun.ymax="max",geom="errorbar")+
  geom_hline(yintercept = 0)

ggplot(data = test_area, aes(x = ratio, y = value))+
  stat_summary(fun.ymin="min",fun.ymax="max",geom="errorbar")+
  geom_hline(yintercept = 0) +ggtitle("area")

ggplot(data = test_accuracy, aes(x = ratio, y = value))+
  stat_summary(fun.ymin="min",fun.ymax="max",geom="errorbar")+
  geom_hline(yintercept = 0) +ggtitle("accuracy")


#plot the different "best" thresholds (due to different criteria) and their accuracies
#data.frame has to be reshaped
compare_thresholds <- model_parameters[,c(1, 3, 5, 6, 8, 9, 11, 12, 14)]
compare_threshold_long <- melt(compare_thresholds,
                               id.vars = "ratio")
compare_threshold_long$measure <- rep(c(rep("threshold", 4), rep("accuracy", 4)),4)
compare_threshold_long$criteria <- rep(c("histogram","AUC", "accuracy", "area"), each = 8)
compare_threshold_long <- compare_threshold_long[, c(1,3:5)]

compare_threshold_wide <- dcast(compare_threshold_long, ratio + criteria ~ measure, value.var = "value" )

ggplot()+
  geom_point(data = compare_threshold_wide, aes(x = threshold, y = accuracy, col = ratio, shape = criteria))

##################################################################################################################
### PLOT THE OVERALL RESULTS

#radar chart with categories: accuracy with hist_threshold, best accuracy, AUC, robustness (total range of thresholds accuracy + area)
radarchart <-  model_parameters[, c(5,11, 15, 24, 25)]
#radarchart_data <- rbind(rep(1,5), c(0.95,0.95,0.95,0,0), radarchart)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4, 0.3, 0.7, 0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2), rgb(0.4,0.3,0.7,0.2) )

#manipulating radarchart
#caution: min and max value for each axes are calculated by the min and max value of each column
#differences may be little, but seem big in the chart (eg. best accuracy differs among all methods less than 0.2%)
radarchart( radarchart, axistype=0 , maxmin = FALSE,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            #custom labels
            vlcex=0.8, vlabels = c("accuracy map", "best accuracy", "AUC", "range of thresholds \n- accuracy", "range of thresholds \n- area"),
            title = "Performance of the different methods in different categories"
)
legend("bottomleft",legend = model_parameters[,1], bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1, pt.cex=3)

##radarchart ranking
#instead of true values take the ranks of the different methods in the different categories
#no information about how much better/worse the method is, but the danger of misinterpretation is less
radarchart_rank <- radarchart
for (i in 1:ncol(radarchart_rank)){
  radarchart_rank[,i] <- rank(radarchart_rank[,i])
}
radarchart_rank <- rbind(rep(4,5), rep(0,5), radarchart_rank)
radarchart( radarchart_rank , axistype=0 , maxmin = TRUE,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            #custom labels
            vlcex=0.8, vlabels = c("accuracy map", "best accuracy", "AUC", "range of thresholds \n- accuracy", "range of thresholds \n- area"),
            title = "Ranking of the different methods in different categories"
)
legend("bottomleft",legend = model_parameters[,1], bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1, pt.cex=3)



#########################################################################################################
#########################################################################################################
#########################################################################################################
################### PART II: TIME SERIES ANALYSIS #######################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

# Question: Is the range of appropriate thresholds stable over a time-series of Landsat images?
# Data basis: 30 year time series (1986-2017)
# There is no validation data available for every year.
# The sensitivity analysis can only be performed on basis of the calculated area and its deviations.



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
    # if (r == "ndsi") {
    #   threshold <- 0.4
    # } else {
      threshold <- find_threshold(ratio, plot.hist = FALSE)
    # }

    #get binary map and glacier area from "optimal" threshold
    map <- get_binary_map(ratio, threshold, ratio_name = r)
    opt_area <- calc_glacier_area(map)

    #get range of thresholds
    threshold_min <- minValue(ratio)
    #ratio may be infinite (division by 0), exclude these cases when defining the upper limit of the threshold range
    threshold_max <- max(ratio[is.finite(ratio)])

    #do sensitivity analysis (only area calculation as there is no validation data for every year)
    sensitivity <- sensitivity_analysis(raster = ratio, ratio_name = r, threshold_min = threshold_min,
                                        threshold_max = threshold_max, threshold = threshold, steps = steps)


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

#give the range of thresholds as a factor of the standard deviation
for (j in 1: length(threshold_ranges)){
  threshold_ranges[[j]]$min_threshold_std <- (threshold_ranges[[j]]$min_threshold - threshold_ranges[[j]]$threshold)/threshold_ranges[[j]]$std_ratio
  threshold_ranges[[j]]$max_threshold_std <- (threshold_ranges[[j]]$max_threshold - threshold_ranges[[j]]$threshold)/threshold_ranges[[j]]$std_ratio
  threshold_ranges[[j]]$total_threshold_range_std <- threshold_ranges[[j]]$max_threshold_std - threshold_ranges[[j]]$min_threshold_std
}



######################################################################################################
### PLOT THE RESULTS


#plot glacier area for all methods (exclude outliers)

ggplot() +
  geom_line(data = threshold_ranges[[1]], aes(x = year, y = glacier_area, colour = "A"))+
  geom_line(data = threshold_ranges[[2]], aes(x = year, y = glacier_area, colour = "B"))+
  geom_line(data = threshold_ranges[[3]], aes(x = year, y = glacier_area, colour = "C"))+
  geom_line(data = threshold_ranges[[4]], aes(x = year, y = glacier_area, colour = "D"))+
  scale_color_manual(values = color_assignment, labels = names(threshold_ranges),name = "Band ratio")

#nir/swir ratio shows some outliers (--> thresholding is not working properly) --> exclude unrealistic value
ggplot() +
  geom_line(data = threshold_ranges[[1]][threshold_ranges[[1]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "A"))+
  geom_line(data = threshold_ranges[[2]][threshold_ranges[[2]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "B"))+
  geom_line(data = threshold_ranges[[3]][threshold_ranges[[3]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "C"))+
  geom_line(data = threshold_ranges[[4]][threshold_ranges[[4]]$glacier_area < 500,], aes(x = year, y = glacier_area, colour = "D"))+
  scale_color_manual(values = color_assignment, labels = names(threshold_ranges),name = "Band ratio")

#exclude erroneous data points from nir/swir ratio
threshold_ranges <- threshold_ranges
for (i in 1: length(threshold_ranges)){
  threshold_ranges[[i]] <- threshold_ranges[[i]][threshold_ranges[[i]]$glacier_area < 500,]
}



#reshape data.frame in order to get boxplot
sd_ranges <- data.frame(year = numeric(), ratio = numeric(), sd_range = numeric())
for (i in 1 : length(threshold_ranges)){
  df <- data.frame(year = threshold_ranges[[i]]$year, ratio = names(threshold_ranges)[i],
                   sd_range = threshold_ranges[[i]]$total_threshold_range_std)
  sd_ranges <- rbind(sd_ranges, df)
}
names(sd_ranges) <- c("year", "ratio", "sd_range")


ggplot(data = sd_ranges, aes(y = sd_range, x = ratio))+
  geom_boxplot()+ #outlier.size
  geom_jitter(width = 0.1)


