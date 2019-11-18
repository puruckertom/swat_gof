#### Advanced R Training, Fall 2012
#### Correlation Exercises



###### Exercises ######

## 1: Read in the PNA Index data, make it a time series object, and make a 
## time series plot.


## 2: Look at the ACF and PACF plots, using both acf() and acf2(). Do you
## notice any differences? Do you prefer one or the other?


## 3: We haven't talked about this, but another way to check whether there 
## is / isn't correlation is the 'runs test'. You have to convert the 
## series to a 2-variable factor first, with the factors coded according to whether
## each value is above / below a threshold (usually the mean). Look at the 
## runs.test() function in the 'tseries' package and try it. 
## (Hint: the ENSO Index is centered on 0 already...)


## 4: Since there are no covariates associated with the ENSO Index data, try fitting
## a pure auto-regressive moving average (ARMA) model to the series. Look at the 
## ACF and PACF again, and try a few processes. Try such models as an AR(1), AR(2),
## MA(1,1), and MA(2,2) and use the AIC criterion to find which is best. 
## (Hint: ?arima, ?sarima, ?arma)
## Simulate a time series from the best fitting model and see if the structure
## looks similar to the original.


## 5: Read in the NOAA SEAMAP Dissolved Oxygen data. Make the data frame a SpatialPointsDataFrame object
## and make a spatial plot and a semivariogram. The relevant column for the oxygen results is 'OXMGL'.
## It might make more sense to split the data into 2 sets and then make a semivariogram.
## (Hint: ?spplot, look at the 'identify' argument to figure out how to know where to split them.)










###### Solutions ######


## 1: Read in the PNA Index data, make it a time series object, and make a 
## time series plot.

## 1.
pna <- read.table("Data/Correlation/PNA_Monthly.txt", header=TRUE)
pnaTS <- ts(data = pna$PNAIndex, start = c(1950, 1), frequency = 12)
plot(pnaTS, main = "PNA Index Time Series, 1950 - 2012", ylab = "PNA Index")


## 2: Look at the ACF and PACF plots, using both acf() and acf2(). Do you
## notice any differences? Do you prefer one or the other?

## 2.
acf(pnaTS) # in 'stats' package, included in R
pacf(pnaTS) # ditto
## The acf() and pacf() functions are shown on different scales. The acf() function shows the 
## 0'th lag, which is the correlation between each value and itself. Of course it's 1.0! It 
## can be misleading...
library(astsa)
acf2(pnaTS) # in 'astsa' package, which accompanies a Time Series book by Stoffer.
# Displays ACF and PACF in same window, on same scale. Doesn't show the 0'th lag.


## 3: We haven't talked about this, but another way to check whether there 
## is / isn't correlation is the 'runs test'. This is a non-parametric version
## of correlation.  You have to convert the series to a 2-variable factor
## first, with the factors coded according to whether each value is
## above / below a threshold (usually the mean). Look at the 
## runs.test() function in the 'tseries' package and try it. 
## (Hint: the ENSO Index is centered on 0 already...)

## 3.
library(tseries)
?runs.test
pnaRuns <- as.factor(ifelse(pna$PNAIndex >= 0, 1, 0))
runs.test(pnaRuns)
## low p-value indicates evidence against the null hypothesis.
## The default alternative is 'two-sides', meaning there could me 
## more or fewer runs than you woudld expect by chance.


## 4: Since there are no covariates associated with the ENSO Index data, try fitting
## a pure auto-regressive moving average (ARMA) model to the series. Look at the 
## ACF and PACF again, and try a few processes. Try such models as an AR(1), AR(2),
## MA(1,1), and MA(2,2) and use the AIC criterion to find which is best. 
## (Hint: ?arima, ?sarima, ?arma)
## Simulate a time series from the best fitting model and see if the structure
## looks similar to the original.

## 4. 
# The arima() function gives some dubious results (Mean/Intercept is wrong, Ljung-Box bounds
# are wrong) The arma() function is a wrapper for arima(). I like sarima(), which correct the 
# errors in arima().
pnaAR1 <- sarima(pnaTS, p = 1, d = 0, q = 0)
pnaAR2 <- sarima(pnaTS, p = 2, d = 0, q = 0)
pnaMA11 <- sarima(pnaTS, p = 1, d = 0, q = 1)
pnaMA22 <- sarima(pnaTS, p = 2, d = 0, q = 2)
modelResults <- data.frame("Model" = c("AR1", "AR2", "MA11", "MA22"), 
           "AIC" = c(pnaAR1$fit$aic,
                     pnaAR2$fit$aic,
                     pnaMA11$fit$aic,
                     pnaMA22$fit$aic)
           )
modelResults

newSeries <- arima.sim(pnaAR2,n=200)
plot(newSeries)


## 5: Read in the NOAA SEAMAP Dissolved Oxygen data. Make the data frame a SpatialPointsDataFrame object
## and make a spatial plot and a semivariogram. The relevant column for the oxygen results is 'OXMGL'.
## It might make more sense to split the data into 2 sets and then make a semivariogram.
## (Hint: ?spplot, look at the 'identify' argument to figure out how to know where to split them.)

## 5.
library(sp)
library(gstat)
oxg12 <- read.csv("Data/Correlation/Oxygen2012.csv", header = TRUE)
head(oxg12)
oxy <- oxg12[,c("LATITUDE", "LONGITUDE", "OXMGL")]
names(oxy) <- tolower(names(oxy))
coordinates(oxy) = ~ longitude + latitude
spplot(obj = oxy, zcol = "oxmgl", identify = TRUE) # 184'th row index seems to be the barrier.
oxyWest <- oxy[1:184, ] # Can subset a SpatialPointsDataFrame just like a data frame.
oxyEast <- oxy[185:nrow(oxy), ]
oxyWestVG <- variogram(oxmgl ~ 1, oxyWest)
plot(oxyWestVG,  main = "Semivariogram for DO, NW Gulf",
     pch = 16, col = "red", cex = 1.5)
oxyEastVG <- variogram(oxmgl ~ 1, oxyEast)
plot(oxyEastVG,  main = "Semivariogram for DO, NE Gulf",
     pch = 16, col = "red", cex = 1.5)
