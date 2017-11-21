#generate a 2x3 matrix
m1 <- matrix(c(4,7,3,8,9,2),nrow=2)
m1

#with defined rows and columns and how to fill it
m2 <- matrix(c(2,4,3,1,5,7), nrow = 2, ncol=3,byrow = FALSE)
m2


#create a vector with 80 entries based on normally distributed data
numbers_1 <- rnorm(80, mean = 0, sd = 1)
mat_1 <- matrix(numbers_1, nrow = 20, ncol = 4)
mat_1


#convert to data frame
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1", "var2", "var3", "var4")
df_1


df <- data.frame(plot="location_name_1", measure1 = runif(100)*1000, measure2 = round(runif(100)*1000),
                 value = rnorm(100,2,1), ID=rep(LETTERS,100))
#plot =,...: column names
#runif(): equal distribution with number of observations
#rep(): repeat x times
#round(): rounding of numbers
#LETTERS: in-build constants

df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2=round(runif(50)*10),
                   value=rnorm(50), ID=rep(LETTERS,50))
df <- rbind(df,df_2) # combine data row-wise (cbind: column-wise)

plot(df$measure1, df$measure2)
plot(df$value, df$measure1)
##information on plots: https://www.r-graph-gallery.com/all-graphs/
install.packages("rgl")
library(rgl)
plot3d(df$value, df$measure1, df$measure2)

df[df$value>3.0,] #show all rows with value > 3
df[df$value>3.2 | df$measure1>50,] #| OR, & AND
df$newcol <- df$measure1*df$measure2 #new column
length(df$measure1) #length of column

df[grep("a",df$ID, ignore.case = TRUE),] #query data using a keyword (e.g. "a" for ID column), ignore.case = TRUE = not case sensitive

x1 <- rbinom(10, size = 1, prob = 0.5) #random list of "woman" and "man": binomial values
x2 <-  factor(x1, labels = c("man", "woman"))
summary(x2)
levels(x2)
as.character(x2)
#recode
library(car)
recode(x2, "'woman'='woman'; 'man' = 'guy'")
#or
ifelse(x2=="man", 'guy', 'woman')

##Create raster
library(raster)
library(sp) #spatial points
r1 <- raster(nrow =10,ncols=10)
r1[] <- rnorm(100)
plot(r1)

poi1 <-  cbind(c(rnorm(10)), c(rnorm(10)))#create ten random coordinate pairs
poi1
poi1.sp <- SpatialPoints(poi1) #convert the list of coordinates to a spatial object
plot(poi1.sp)
df <- data.frame(attr1=c("a","b","z","d","e","q","w","r","z","y"),
                         attr2=c(101:110)) #creating values
poi1.spdf <- SpatialPointsDataFrame(poi1.sp,df) #adding values to spatial point data set
plot(poi1.spdf)

install.packages("RStoolbox")
library(RStoolbox)
lsat
lsat[] #provides underlying data
plot(lsat[[1]]) #indexing raster layer: plot first band of lsat, =lsat$B1_dn
x <- lsat[[2:3]] #save second and third band in new object
plot(lsat[[2:3]])

lsat[[2]][1,1]
lsat[1,1]

x <- lsat[1:10,] # values of rows one to ten
#all values
x <- lsat[]
x <- getValues(lsat)
#based on logical query
x <- lsat[ lsat == 10 ]

raster_data@data #point to the data of a raster data set
raster_data@data$... #points to the data in layer x

#load/create example data
install.packages("move")
library(move)
data(lsat)
data(leroy)
env <- raster(leroy, vals = rnorm(100))

#based on vector geometry, e.g. move objects or polygons
x <- extract(env, leroy) #extracts values of raster cells where points of leroy are
x
#set values
lsat[] <- rnorm(ncell(lsat))
lsat[lsat <0] <- NA

env[] <- 0 #all cells have value 0
env[leroy] <- 1 #all cells where point from leroy have value 1

#create empty raster
r1 <- raster(nrows = 100, ncols = 100)
r1
r1 <- df$measure1[1:1000] #populate empty raster with column measure 1 values
plot(r1)
r2 <- raster(nrows = 100, ncols = 100)
r2[] <- df$measure2[1:1000]
r12 <-  stack(r1, r2) #stack two raster together
r12
plot(r12)
plot(r12[[1]]) # plot only first raster
r12$new <- r12[[1]]*r12[[2]]^2 #create new raster layer
r12
plot(r12)
#convert raster values back to data frame
df12 <-  r12[]
head(df12)

##crop data to smaller rectangular extent
plot(band_3)
ext <-  drawExtent() #draw an extent on the monitor (NW corner - SE corner)
band_3_crop <-  crop(band_3, extent) # ext is an obkect of class extent
ex * 2 #grow and shrink extents by multiplying (e.g. grows ex in all four directions)


