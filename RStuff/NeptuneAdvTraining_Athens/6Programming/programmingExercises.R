#### Advanced R Training
#### R Programming Session
#### Solutions are provided below.  

### reshape2

### 1.  airquality is a dataset included in base R.  The data was collected in 1973
     ## a. Melt the data into a tall format with Month and Day  as id.variables
     ## b. Use dcast command to return the data to a wide format
     ## c. Use dcast command to calculate the maximum value for each variable for each month.  Remove NA values 
     ##    from calculations.

### 2. apply command
     ## a. Use apply command to calculate averaged for each analyte in the airquality data dataframe


### 3. aggregate command

     ## a. Read in wqData and create columns with month and year values

     ## b. reshape data to tall format with Site, Year and Month as id variables.  
     ## c. Use aggregate to calcuate the average value by by Site and Analyte
     ## d. Use aggregate to calcuate the average value by by Site, Analyte,  by Year

### 4. plyr package,  ddply function

     ## a.  create a function which returns the ratio of Calcium (Ca) to Magnesium (Mg).
     ## b.  Use the ddply function with the functin created in the previous step to summarize data by Site
     ## c.  Repeat for Site and Month

### 5. Using browser to explore a function
     ## a. dump the function hist.default to a local file 
     ## b. rename the function hist.alt 
     ## c. inserte the browser command at the beginning of the function and source or send the file to the console
     ## d. Generate random data and step through the code in the function.

##################### Solutions

### reshape2

### 1.  airquality is a dataset included in base R.  The data was collected in 1973
## a. Melt the data into a tall format with Month and Day  as id.variables
library(reshape2)
temp <- melt( data = airquality, id.vars = c("Month", "Day"))

## b. Use dcast command to return the data to a wide format
tempWide <- dcast(temp, Month+ Day ~ variable)

## c. Use dcast command to calculate the maximum value for each variable for each month.  Remove NA values 
##    from calculations.

dcast(temp, Month  ~ variable, fun.aggregate=max, na.rm = TRUE)


### 2. apply command
## a. Use apply command to calculate averaged for each analyte in the airquality data dataframe

apply(airquality[, 1:4], 2, mean, na.rm = TRUE)

### 3. aggregate command

## a. Read in wqData and create columns with month and year values

wqDat <- read.table("Data/Graphics/WQ.csv", sep = ",", header = TRUE, na.strings = "-999999") ### assumes you are in AdvancedR.Fall2012
temp <- strptime(wqDat$Date, format = "%Y-%m-%d")
wqDat$Year <- format(temp, "%Y") ## note - this returns a character string, not a number
wqDat$Month <- format(temp, "%m") ## note - this returns a character string, not a number

## b. reshape data to tall format with Site, Year and Month as id variables.  

temp<- melt(wqDat[, -c(1,3)], id.var = c("Site", "Year", "Month")  , variable.name= "Analyte") ##Why, wqDat[, -c(1,3)]? 
##If Date and Dataset columns are not excluded, they will be treated as analytes. 

## c. Use aggregate to calcuate the average value by by Site and Analyte

out <- aggregate(value~ Analyte + Site, data = temp, mean, na.rm = TRUE)
head(out)

## d. Use aggregate to calcuate the average value by by Site, Analyte,  by Year

head( aggregate(value~ Analyte + Site + Year, data = temp, mean, na.rm = TRUE) )

### 4. plyr package,  ddply function

## a.  create a function which returns the median ratio of Calcium (Ca) to Magnesium (Mg).

f <- function(x){return(median( x$Ca/ x$Mg , na.rm = TRUE) )  }
## b.  Use the ddply function with the functin created in the previous step to summarize data by Site
library(plyr)

ddply( .data = wqDat, .fun=f , .variables=c("Site"), .drop = FALSE )

## c.  Repeat for Site and Month

ddply( .data = wqDat, .fun=f , .variables=c("Site", "Month"), .drop = FALSE )


### 5. Using browser to explore a function

## a. dump the function hist.default to a local file
dump("hist.default", file = "Programming/alt.hist.R")

## b. rename the function hist.alt by changing there first line to 
##   hist.alt <-
## c. insert the browser command at the beginning of the function and source or send the file to the console
## a file named "alt.hist.solution.R" has been included with the Programming directory
source("Programming/alt.hist.solution.R")

## d. Generate random data and step through the code in the function.

x <- rnorm(20)
hist.alt(x)

### step through by typing n
### quit by entering Q to return to regular environment

Q

