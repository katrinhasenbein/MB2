5+6 # R as a calculator
5*5^4-88*15
0/0 #NaN: Not a Number
1/0 #Inf: Infinity
result1 <- 5+6 # assigning results in an object
result2 = 5+6 #direction not clearly defined

prec_avg <-  c(56, 46, 50, 53, 69, 83, 80, 63, 55, 60, 63)
plot(prec_avg)
plot(prec_avg, pch = 19, cex = 2, col = "#00ff0060")
lines(lowess(prec_avg, f = .2))

install.packages("raster") #install the package
library(raster) #load it

germany <- getData("GADM", country = "DEU", level =2) #get country borders
plot(germany)
prec <- getData("worldclim", var = "prec", res=.5, lon=10, lat=51) #get precipitation data
plot(prec)

prec_ger1 <- crop(prec, germany) #crop precipitation to extent of germany
spplot(prec_ger1)
prec_ger2 <- mask(prec_ger1, germany) #mask precipitation to shape of germany
spplot (prec_ger2)
prec_avg <- cellStats(prec_ger2, stat = "mean")

install.packages("ggplot2")
library(ggplot2)
x11()
x <- data.frame(x=1, y=1, label= "ggplot2 introduction \n@ EAGLE")
ggplot(data=x, aes(x=x, y=y)) + geom_text(aes(label=label), size =15)

head(mpg)
ggplot(mpg, aes(x=displ, y =hwy))+geom_point()
ggplot(mpg, aes(displ, cty, colour=class))+geom_point()
ggplot(mpg, aes(displ, hwy))+geom_point()+facet_wrap(~class)+geom_smooth()
ggplot(mpg, aes(class, displ))+geom_boxplot(alpha=.5)+geom_point(aes(color=hwy), alpha=.7, size=1.5,position=position_jitter(width=.25, height=0))

myPlot <- ggplot(mpg, aes(x=displ, y=hwy))+geom_point()
myPlot + geom_smooth()

ggplot()+geom_point(data=mpg, aes(x=displ, y=hwy)) # definitions in ggplot() are global, in the individual geoms are not
