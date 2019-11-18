### Advanced Graphics in  R - Fall 2012
### 
###  Created by Matt Pocernich, Will Barnett and Mark Fitzgerald
###  Neptune and Co., Inc.  www.neptuneinc.org
### Questions or comments may be sent to Matt Pocernich mpocernich@neptuneinc.org

### The code in this script use file references which assumes that the current directory is 
### set to AdvancedR.Fall2012.  
### Check working directory

getwd()
### if directory is not AdvancedR.Fall2012 - change!

library(reshape2)
library(plyr)
library(maptools)

#### Part A Reshape

streamDat <- read.table("Data/Graphics/streamData.reformat.csv", sep = ",", header = TRUE)

## a small example
streamSmall <- streamDat[c(1,2,13,14), -c(2:6) ]
streamSmall_tall <- melt(streamSmall, id.vars = c("stream"), variable.name="analyte" )
streamSmall_tall

#### Example with full dataset

streamDat_tall <- melt(streamDat, id.vars = c("stream", "season", "date"), variable.name="analyte" )
head(streamDat_tall)

### reshape to bring data back into wide format

streamWide<- dcast(data = streamDat_tall, stream +season + date ~ analyte)
head(streamWide)

###  Note what happens when cells don't represent unique values
streamWide<- dcast(data = streamDat_tall, stream +season  ~ analyte)
head(streamWide)

streamWide<- dcast(data = streamDat_tall, stream +season  ~ analyte, margins = TRUE)

### specifying fun.aggregate can be used as to accomplish the same items as aggregate

streamWide<- dcast(data = streamDat_tall, stream   ~ analyte, fun.aggregate = max)

##############################
### Part B Split Apply Combine
##############################

streamDat <- read.table("Data/Graphics/streamData.reformat.csv", sep = ",", header = TRUE)

### calculate mean for each variable.

args(apply)
apply(streamDat[, -c(1,2,3)], 2, mean )  ## apply only works on numbers
apply(streamDat[, -c(1,2,3)], 2, mean, na.rm = TRUE )

### apply works on arrays

temp <- array(1:24, dim = c( 2,3,4) )

dim(temp)

apply(temp, MARGIN = 1, sum)  ## summ across rows
apply(temp, MARGIN = 3, sum)  ## summ across depth

apply(temp, MARGIN = c(1,2), sum)  ## summ across front

### apply used with compiled function like mean is efficient

temp <- matrix(rnorm(1e7), nrow = 1e6) ## a million rows by 10 columns

apply(temp,2,mean)


### we want to caluate the mean for each variable for each site and season

### loops are okay

OUT <- NULL ## object to store output
for(site in unique(streamDat$stream)){
  for(seas in unique(streamDat$season)){
    sub <- subset(streamDat, stream == site & season == seas)  ## split
    out.sub <- apply(sub[, -c(1,2,3)], 2, mean, na.rm = TRUE ) ## aggregate
    out.sub2 <- data.frame( stream = site, season = seas, t( out.sub) )  ## t transposes out.sub
    OUT <- rbind(OUT, out.sub2)  ## combine
  }
}

head(OUT)

####### aggregate with
### formula type expression
head(
aggregate( . ~ stream + season, data = streamDat[, -3], mean) ##-3, drops date column.
)
head(
  aggregate( streamDat[, -c(1,2,3)], by = list(stream = streamDat$stream, season = streamDat$seas), mean, na.rm = TRUE)
)

### plyr package

args(ddply)

### we need a function that takes a dataframe as input and output.
f <- function(x){
  apply(x[, -c(1,2,3)], 2, mean, na.rm = TRUE )
}

##
ddply(.data = streamDat, .variables = c("stream", "season"), .fun = f )

## a silly example, because it does the same thing as aggregate
## suppose we wanted to calculate the ratio of NO3/NH4  and the max flow?

f <- function(x){ 
  data.frame(ratio = x$NO3/x$NH4, max.do = max(x$Q))
  }

head(
ddply(.data = streamDat, .variables = c("stream", "season"), .fun = f )
)

#### Part C Programming Tools and Aids
#### browser, debugging

#### Using browser for debugging

### Let us see how ecdf works
### create an alternative version and place browser()  command
ecdf.alt <-  function (x) {
  browser() ### when this function is run, code will stop here.
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

ls()
environment()

ecdf.alt(x)
ls()
n
environment()
### hit return several times
Q  ## ends us out


#### a word/ warning about environments
rm(y)  ## make sure these objects don't exist

f <- function(x){x + y}
  
f(3)  ## Returns and error because y is unknown.

y <- 3
f(3) ### works because function uses value for y in the global enviroment.


#### how long does a procedure take?

system.time(
  median( rnorm(1e7) )
)

system.time(
  mean( rnorm(1e7) )
)

### Part D S4 Classes
### the following data is from the Spatial Module
### Check spatial data type of the shapefile:

## Read the shapefile in as an 'sp' object using 'maptools':
readShp <- readShapeSpatial("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations")
str(readShp) # Default projection is "NA" in readShapeSpatial()

slotNames(readShp)

temp <- slot(readShp, "data" ) ## readShp@data also works but is not favored.

coordinates(readShp)  ## returns matrix



