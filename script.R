difftime(("2017-12-24"), Sys.Date(), units="days")

install.packages("raster")
library(raster)
germany <- getData("GADM", country = "DEU", level =2)
plot(germany)
prec <- getData("worldclim", var = "prec", res=.5, lon=10, lat=51)
plot(prec)
prec_ger1 <- crop(prec, germany)
spplot(prec_ger1)
prec_ger2 <- mask(prec_ger1, germany)
spplot (prec_ger2)
prec_avg <- cellStats(prec_ger2, stat = "mean")
prec_avg

#Canada
canada <- getData("GADM", country ="CAN", level =2)
plot(canada)
prec <- getData("worldclim", var="prec", res=5)
plot(prec)
prec_can1 <-  crop(prec, canada)
spplot(prec_can1)
prec_can2 <- mask(prec_can1, canada, inverse = TRUE)
spplot(prec_can2)
prec_avg_can <- cellStats(prec_can2, stat="mean")
prec_avg_can
mean(prec_avg_can)
mean(prec_avg)
