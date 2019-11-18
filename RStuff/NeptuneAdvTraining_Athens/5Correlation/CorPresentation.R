#### Advanced R Training, Fall 2012
#### Dealing with Correlation.

##Set working directory to be in the folder that contains
##the subfolder Data

## Libraries.
library(MASS)
library(astsa)
library(sp)
library(gstat)
library(tseries)
library(nlme)

###### Part 1: Why does correlation matter? ######

## Parameters for simulation.
pVals <- numeric(0) # Empty storage vector for p-values.
numIter <- 1000 
sampSize <- 50
covPar <- 0.0
stdDev <- 3

## Loop for simulation.
for(k in 1:numIter){
  ## So you know it's doing something...
  if( !(k%%10) ){
    print(k) 
  }
  sigMat <- matrix(rep(covPar, sampSize^2), ncol = sampSize) # Covariance matrix
  diag(sigMat) <- rep(stdDev^2, sampSize) # Set diagonals to be stdDev
  tmpDat <- mvrnorm(n = 1, mu = rep(100, sampSize), Sigma = sigMat)  # Generate samples.
  ## Combine p-values from previous iterations:
  pVals <- c(pVals, t.test(x = tmpDat, alternative = "two.sided", mu=100)$p.value)
}

## Histogram of the p-values.
hist(pVals, col = 8, cex.lab = 1.5, cex.axis = 1.25,
     xlab = "P-values", main = "P-values from independent data")
## This should be roughly 0.05
mean(pVals <= 0.05)

## Do it again, with a covariance parameter of 0.1
## Parameters for simulation.
pVals <- numeric(0) # Empty storage vector for p-values.
numIter <- 1000 
sampSize <- 50
covPar <- 0.2
stdDev <- 3

## Loop for simulations.
for(k in 1:numIter){
  ## So you know it's doing something...
  if( !(k%%10) ){
    print(k)
  }
  sigMat <- matrix(rep(covPar, sampSize^2), ncol = sampSize) # Covariance matrix
  diag(sigMat) <- rep(stdDev^2, sampSize) # Set diagonals to be stdDev.
  tmpDat <- mvrnorm(n = 1, mu = rep(100, sampSize), Sigma = sigMat)  # Generate samples.
  ## Combine p-values from previous iterations:
  t.test(x = tmpDat, alternative = "two.sided", mu=100)$p.value
  pVals<-c(pVals, t.test(x = tmpDat, alternative = "two.sided", mu=100)$p.value)
}

## Histogram of the p-values.
par(mfrow=c(1,1))
hist(pVals, col = 8, cex.lab = 1.5, cex.axis = 1.25,
     xlab = "P-values", main = "P-values from correlated data")
## This should be roughly 0.05 if the samples are independent.
mean(pVals <= 0.05)

###### End of Part 1 ######





###### Part 2: Temporal Correlation ######
## Load workspace: 
ensoDat <- read.table("Data/Correlation/ENSO_Monthly.txt", header = TRUE,
                      stringsAsFactors = FALSE)
head(ensoDat)

## Make ENSO data a time series object and plot it.
ensoTS <- ts(ensoDat$ANOM, start = c(1950, 1), frequency = 12) # ?ts for details
plot(ensoTS, main = "ENSO Index, 1950 - 2012", ylab = "ENSO Index Value")
## Is there correlation?
acf2(ensoTS) # acf2() is a function in 'astsa' package. Different from acf() in 'stats'

## Example of autocorrelation that can be explained with a variable.
x <- seq(1,10, 0.2)
y <- sin(x) + rnorm(length(x), 0, 0.1)
plot(y ~ x, pch = 18, main = "y = cos(x) + error")
acf2(y, max.lag = 30)

###### End of Part 2 ######





###### Part 3: Spatial Correlation ######
## Yukon fire data
yukonDat <- read.table("Data/Correlation/yuk242_block.txt", sep = " ",
                       header = TRUE)
## Put together a SpatialPixelsDataFrame for the fire data
yukonFire <- yukonDat
coordinates(yukonFire) <- ~ x + y # Makes a SpatialPointsDataFrame
gridded(yukonFire) = TRUE # Promotes to SpatialPixelsDataFrame
## Randomize the 'nbr' values to show uncorrelated data
yukonRandom <- yukonDat
yukonRandom[ , "nbr"] <- yukonDat[ sample(1:nrow(yukonDat), replace = FALSE),
                                              "nbr"]
coordinates(yukonRandom) <- ~ x + y
gridded(yukonRandom) = TRUE
## Plot the two
image(yukonFire["nbr"])
image(yukonRandom["nbr"])
## Variogram for fire data
vgNbr <- variogram(nbr ~ 1, yukonFire)
plot(vgNbr,  main = "Semivariogram for NBR Index, Yukon",
     pch = 16, col = "red", cex = 1.5)

###### End of Part 3 ######





###### Part 4: Modeling Ft. Pulaski Time Series ######
## Read in the Fort Pulaski meteorological data and do a TS regression.
ftPulDat <- read.table("Data/Correlation/SeptMeteorData.txt", comment.char = "#",
                    header=TRUE, sep=" ")
plot(ftPulDat$WindSp, type = "l", xlab = "Hour of Day, Sept. 1-30, 2012", ylab = "Wind Speed (Knots)",
     main = " Wind Speed at Fort Pulaski, Sept. 2012", col = "red")

## Check for correlation
acf2(ftPulDat$WindSp) # Definitely some correlation there.
## Check for stationarity.
adf.test(ftPulDat$WindSp) # Augmented Dickey-Fuller Test => stationary series.

## Do any of the other variables matter?
pairs(ftPulDat[,4:ncol(ftPulDat)]) 
plot(WindSp ~ BaroPr, ftPulDat)
plot(WindSp ~ WindGt, ftPulDat) # BaroPr and WindGt seem related, but how different is WindGt?
plot(WindSp ~ WindDr, ftPulDat)

## Fit three models, with variables BaroPr, BaroPr^2, and WindDr.

## Model 1:
fit1 <- lm(WindSp ~ BaroPr + WindDr, data = ftPulDat) # significant covariates
summary(fit1)
plot(fit1) # They look decent, the Q-Q plot isn't great.
#plot(WindSp ~ BaroPr, ftPulDat, xlab = "Barometric Pressure", 
#     ylab = "Wind Speed", main = "Linear Fit")
plot(ftPulDat$WindSp, type="l", main = "Actual and Fitted Series, Model 1")
lines(fit1$fitted, col = "red")
## Wind speed and Barometric pressure seem to be related in a parabolic way.

## Model 2:
ftPulDat$BaroPr2 <- (ftPulDat$BaroPr)^2 # Make a barometer-pressure-squared variable
fit2 <- lm(WindSp ~ BaroPr + BaroPr2 + WindDr, data = ftPulDat) # both covariates are significant
summary(fit2)
plot(fit2) # Resid. vs. fitted plot looks much less curved, Q-Q plot is better.
plot(ftPulDat$BaroPr ~ ftPulDat$BaroPr2) # The covariates are VERY correlated.
fit2 <- lm(WindSp ~ poly(BaroPr, 2) + WindDr, x = TRUE, data = ftPulDat) # Re-fit the model.
cor(fit2$x) # Look at the correlation between the newly-centered variables.
AIC(fit1)
AIC(fit2) # Lower AIC value by far.
plot(ftPulDat$WindSp, type="l", main = "Actual and Fitted Series, Model 2")
lines(fit2$fitted, col = "red")

## Still correlation?
fit2Cor <- acf2(fit2$residuals) # ACF/PACF of the residuals
# Still some autocorrelation at the first lag. Try an AR(1) error structure.
fitAR <- gls(WindSp ~ poly(BaroPr, 2) + WindDr, data = ftPulDat,
             correlation = corARMA(p = 1, q = 0))
plot(ftPulDat$WindSp, type="l")
lines(fitAR$fitted, col = "red")
plot(fitAR$residuals, type="l", ylab = "")
acf2(fitAR$residuals)

## Important bit here: look at p-values for the 'WindDr' coefficient in both models
summary(fit2)
summary(fitAR) # The 'WindDr' variable is no longer significant when you account
               # for temporal correlation.




## Extra stuff:
getinfo.shape("Data/Correlation/2012Contours.shp")
contOxy <- readShapePoly("Data/Correlation/2012Contours.shp")
