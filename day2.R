difftime(("2017-12-24"), Sys.Date(), units="days") #days until Christmas

install.packages("raster")
library(raster)
#Canada
canada <- getData("GADM", country ="CAN", level =2)
plot(canada)
prec <- getData("worldclim", var="prec", res=5)
plot(prec)
prec_can1 <-  crop(prec, canada)
spplot(prec_can1)
prec_can2 <- mask(prec_can1, canada, inverse = TRUE) #everything except Canada
spplot(prec_can2)
prec_avg_can <- cellStats(prec_can2, stat="mean")

##Get data into R
setwd("E:/MB2")
mydata <- read.table("bio_data_forest.csv", header = TRUE)
mydata_2 <- read.csv("bio_data_forest.csv", sep="", header = TRUE)
#first overview
head(mydata)
summary(mydata)
plot(mydata)
#Data Export
write.table(mydata, file= "mydata.txt")


##Indexing
X <-  matrix(c(4,7,3,8,9,2), nrow=2) #2x3 matrix
X
X[2,2]
X[,2]

number_1 <- rnorm(80, mean=0,sd=1) #normal distribution with mean and sd
mat_1 <- matrix(number_1, nrow=20,ncol=4)
mat_1
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1", "var2", "var3", "var4")
head(df_1)

df_2 <-  data.frame()
rownames(X) <-  c("a", "b")

X <- seq(1,100, 2.5) #vector
X>20 #TRUE/FALSE for each element
(X<= 10)|(X>=30) #TRUE/FALSE for each element
X[X<10|X>30] #all values which meet condition
X[length(X)] #last value
X[length(X)-1]
X[-2] #extract all but one position
#extract/omit list of positions
idx <- c(1,4,6)
X[idx]
X[-idx]

#changing values
X2 <- numeric(length(X))
X2[X<=30] <- 1
X2[(X>30) & (X < 70)] <- 2
X2[X>70] <- 3
#alternative approach
library(car)
X2 <- recode(X, "0:30=1; 30:70=2; else=3") #intervals: 0 < x <= 30

#statistics
summary(X2)
sum(X2)
cumsum(X2)
#data modification
sort(X, decreasing = TRUE)
sample(X, 10) #sample 10 values out of X
rev(X) #revert order


test <-  data.frame(A=c(1,2,3), B=c("a","b","c")) #data frame with two columns
test
test$A #equivalent to test[,1] or test[,"A"]
test$A[2] 
test$B
test[2,2]

df <- data.frame(plot="location_name_1", measure1 = runif(100)*1000, measure2 = round(runif(100)*1000),
                 value = rnorm(100,2,1), ID=rep(LETTERS,100))
#runif(): equal distribution with number of observations
#rep(): repeat x times
#round(): rounding of numbers
#LETTERS: in-build constants

df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2=round(runif(50)*10),
                   value=rnorm(50), ID=rep(LETTERS,50))
df <- rbind(df,df_2) # combine data row-wise (cbind: column-wise)
#first look at data
summary(df)
str(df)
mode(df) #storage mode of object
head(df)

df[66:115, c("plot", "measure1", "measure2")] # line 556-570 for three columns

##basic analysis
max(X)
range(X)
which.min(X) #which element is minimum (not value)
which.max(X)
diff(X) # difference between elements (first value to second value, second to third...)


###HOMEWORK
setwd("")
mydata <- read.csv("E:/Backup 27.12.2016/Dokumente/Uni/Master/MB2 Introduction to Programming and Geostatistics/2017-11-07/homework_example_data.csv", 
                   sep=";", header = TRUE, stringsAsFactors = TRUE)
summary(mydata)
min(mydata[,2]) #youngest age
mydata[mydata[,2]==min(mydata[,2]),1] #name of youngest student
ages <- mydata[,2]
sort(ages, decreasing = FALSE) #prints vector in order
order(ages, decreasing = FALSE) #prints new index of elements
quantile(ages, probs = 0.3) #probs = define quantiles

ages <- c(23, 27, 16, 23, 25, 21, 56)
cut(ages, breaks = c( 18, 30)) #cuts vector in intervals

##how to explore new commands
#---- cut() tool ----
#Generating 10 random numbers
c <- rnorm(10)
#the new created numbers are now ordered to the defined area -2:1
#the new area has three groups
table(cut(c, breaks = -2:1))
# now you can see how often a number of the data c appears in which group


wuerzburg <-read.table("E:/Backup 27.12.2016/Dokumente/Uni/BA/6. Semester/BA/Messdaten/wuerzburg.txt", header =TRUE)
