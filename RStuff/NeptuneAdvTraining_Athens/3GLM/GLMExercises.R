####
## Exercises for GLM module
####

## 1. Logistic Regression
##    Best management practices were implemented in a county, in
##    an attempt to reduce phosphate loading to a stream.  The
##    county then monitored the stream to examine the effect.
##    The county attempted to achieve 5 samples per week, but
##    sometimes achieved fewer.  Reported (in this data set)
##    for each sample was simply a Yes/No if the phosphate loadings
##    exceeded a regulatory threshold.
## 1a. Read in the StreamExceedance data and plot the propotion of
##     exceedances as a function of week.  Is there a pattern?

stream <- read.csv("Data/GLM/StreamExceedance.csv")
head(stream)
stream$Prop <- stream$NumExceedances / stream$NumSamples
with( stream, plot( Week, Prop ) )

## 1b. Perform a least squares regression (lm) for the proportion of
##     exceedances as a function of week.  Add the least squares
##     regression line to the plot.  The county is hoping to see a
##     negative trend.  Based on least squares, is there one?

linmod <- lm( Prop ~ Week, data=stream )
abline( coefficients(linmod) )
summary( linmod )
anova( linmod )

## 1c. Now perform the more appropriate model: a logistic regression for
##     probability of exceedance as a function of week.  Note: the data
##     is stored as binomial, for purposes of the glm call.  What do
##     you think about trend now?

stream$NumNonExceed <- stream$NumSamples - stream$NumExceedances
logmod <- glm( cbind( NumExceedances, NumNonExceed ) ~ Week,
              data=stream, family=binomial )
summary( logmod )
anova( logmod, test="Chisq" )

## 1d. Compute fitted values from the logistic model for adding to the
##     plot.  Add approximate 95% confidence interval lines.

newdat <- data.frame( Week=seq(-5,150,len=201) )
predlog <- predict( logmod, newdata=newdat, type="response", se.fit=TRUE )
lines( newdat$Week, predlog$fit, col=2 )
lines( newdat$Week, predlog$fit + 2 * predlog$se.fit, col=2, lty=2 )
lines( newdat$Week, predlog$fit - 2 * predlog$se.fit, col=2, lty=2 )

## 1e. Summer activity tends to be more problematic for phosphates in
##     this county.  In order to account for this seasonal effect, the
##     stream temperature might be used in the logistic regression.
##     Add temperature into the model and then assess trend.
##     Plot the new fitted values in the plot.

logmodtemp <- glm( cbind( NumExceedances, NumNonExceed ) ~ Temp + Week,
                  data=stream, family=binomial )
summary( logmodtemp )
anova( logmodtemp, test="Chisq" )
lines( stream$Week, fitted(logmodtemp), col=4 )

## 1f. Look at diagnostic plots for the logistic models.  Any features
##     of concern?
plot( logmod )
plot( logmodtemp )

## 1g. Read in the alternate version of this data set that is stored
##     with different row for each sample (i.e. Bernoulli).  Look at
##     the data in this form.  Or for extra credit, construct the
##     Bernoulli data from the binomial data.

## Extra credit version:
nWeek=rep(stream$Week,stream$NumSamples)
nTemp=rep(stream$Temp,stream$NumSamples)
nExceed=rep(rep(c(TRUE,FALSE),nrow(stream)),
  as.vector(t(stream[,c("NumExceedances","NumNonExceed")])))
streamalt <- data.frame( Week=nWeek, Temp=nTemp, Exceed=nExceed )

## Or read from file:
streamalt <- read.csv("Data/GLM/StreamExceedanceAlt.csv")

with( streamalt, plot( Week, Exceed ) )

## 1h. Repeat the least squares regression from 1a.  Does it match?
##     (It shouldn't generally.)  Repeat the logistic regressions
##     of 1c and 1e and confirm that they do match.  Which features
##     don't match?
linmodalt <- lm( Exceed ~ Week, data=streamalt )
summary(linmodalt)
logmodalt <- glm( Exceed ~ Week, data=streamalt, family=binomial )
summary(logmodalt)
logmodtempalt <- glm( Exceed ~ Temp + Week, data=streamalt, family=binomial )
summary(logmodtempalt)



## 2. Poisson regression
##    A clean-up effort is to be undertaken at a former military site.
##    The current goal is to characterize the site in terms of the
##    likely rates of unexploded ordnance (UXO).  Thirty-one plots
##    of land have been sampled and characterized for munitions of
##    explosive concern (MEC).
## 2a. One factor that is almost certain to be a significant predictor
##     of MEC is the distance from the historical target area.  Read in
##     the data file UXOCounts.  Plot counts by distance, and perform
##     a Poisson regression of counts by distance.

uxo <- read.csv("Data/GLM/UXOCounts.csv")
uxo
with( uxo, plot( DistFromTarg, Count ) )
pmod <- glm( Count ~ DistFromTarg, data=uxo, family=poisson )
summary( pmod )
anova( pmod, test="Chisq" )

## 2b. Add a regression fit to the plot, with approximate 95% conf. bounds

newdat <- data.frame( DistFromTarg=seq(0,1400,length=201) )
ppred <- predict( pmod, newdata=newdat, type="response", se.fit=TRUE )
lines( newdat$DistFromTarg, ppred$fit, col=2 )
lines( newdat$DistFromTarg, ppred$fit + 2*ppred$se.fit, col=2, lty=2 )
lines( newdat$DistFromTarg, ppred$fit - 2*ppred$se.fit, col=2, lty=2 )

## 2c. Compare the Poisson regression to a least squares fit

lmod <- lm( Count ~ DistFromTarg, data=uxo )
abline( coefficients(lmod), col=3 )

## 2d. There is a conjecture that vegetated areas are likely to have
##     hidden otherwise exposed MEC leading to higher rates in vegetated
##     areas.  Examine this hypothesis.

pmod2 <- glm( Count ~ DistFromTarg + Vegetation, data=uxo, family=poisson )
summary( pmod2 )
anova( pmod2, test="Chisq" )

## 2e. There is another conjecture that MEC is less likely to have survived
##     on more highly sloped ground.  Examine this hypothesis.

## Depending on what you decided in 2d about vegetation ...
pmod3 <- glm( Count ~ DistFromTarg + Vegetation + Slope,
             data=uxo, family=poisson )
summary( pmod3 )
anova( pmod3, test="Chisq" )
## or
pmod4 <- glm( Count ~ DistFromTarg + Slope,
             data=uxo, family=poisson )
summary( pmod4 )
anova( pmod4, test="Chisq" )

## 2f. Which model appears best, according to AIC?
AIC( pmod, pmod2, pmod3, pmod4 )

## 2g. What are the units of the model fit?  It would be highly
##     preferable to convert to MEC/acre, and acreage has been
##     ignored so far.  Repeat the model fits above, but account
##     for the fact that four of the land plots were 4 times bigger
##     than the others.  How are the results affected?

pmodacre <- glm( Count ~ DistFromTarg, offset=log(Acres),
                 data=uxo, family=poisson )
pmod2acre <- glm( Count ~ DistFromTarg + Vegetation, offset=log(Acres),
                 data=uxo, family=poisson )
pmod3acre <- glm( Count ~ DistFromTarg + Vegetation + Slope, offset=log(Acres),
                 data=uxo, family=poisson )
pmod4acre <- glm( Count ~ DistFromTarg + Slope, offset=log(Acres),
                 data=uxo, family=poisson )

## 2h. Compute the AIC weights for the models fit in 2g.
AICmat <- AIC( pmodacre, pmod2acre, pmod3acre, pmod4acre )
AICs <- AICmat[,"AIC"]
names(AICs) <- rownames(AICmat)
omegas <- AICs - min( AICs )
AICweights <- exp(-omegas) / sum(exp(-omegas))
round(AICweights,3)

