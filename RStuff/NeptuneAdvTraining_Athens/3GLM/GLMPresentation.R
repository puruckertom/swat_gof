
## Example of Fitting a Linear Model to Binomial Data

norg    <- c(10,15,18,12,14,22, 8, 3)
numsurv <- c( 2, 4, 6, 4, 5,10, 3, 2)
prop <- numsurv / norg
dose <- 1:8
plot( dose, prop, xlab="Dose", ylab="Proportion or Organisms Surviving",
     pch=16, cex=1.5, cex.lab=1.5, cex.axis=1.5 )
lmmod <- lm( prop ~ dose )
abline( lmmod$coef, lwd=2, col=2 )

par(mfrow=c(2,2),mar=c(3.1,3.1,3.1,1.1),cex=1.25,cex.axis=1.25,
    lwd=2,cex.main=1.25,cex.sub=1.25,pch=16)
plot(lmmod)
dev.off()

## Different look at this data
numdied <- norg - numsurv
survstatus <- c( rep( 0, sum(numdied) ), rep( 1, sum(numsurv) ) )
newdose <- rep( c(dose,dose), c(numdied, numsurv) )
newlm <- lm( survstatus ~ newdose )
plot( dose, rep( 1, length(dose) ), pch=16, cex=numsurv*0.5,
     ylim=c(-0.1,1.1), xlab="Dose", ylab="Survived=1, Died=0",
     cex.axis=1.5, cex.lab=1.5 )
abline( lmmod$coef, lwd=2, col=2 )
points( dose, rep( 0, length(dose) ), pch=16, cex=numdied*0.5 )
text(4.5,0.6,"Point Size Proportional\nto Number",cex=2)
abline( newlm$coef, lwd=2, col=3 )

par(mfrow=c(2,2),mar=c(3.1,3.1,3.1,1.1),cex=1.25,cex.axis=1.25,
    lwd=2,cex.main=1.25,cex.sub=1.25,pch=16)
plot(newlm)
dev.off()

## Other problems with least squares here
norg2    <- c(10,15,18,12,14,22, 8, 3, 10, 13, 29, 7, 16, 2, 19, 14 )
numsurv2 <- c( 2, 4, 6, 4, 5,10, 3, 2,  7,  9, 21, 6, 15, 2, 19, 14 )
prop2 <- numsurv2 / norg2
dose2 <- 1:16
plot( dose2, prop2, xlab="Dose", ylab="Proportion or Organisms Surviving",
     pch=16, cex=1.5, cex.lab=1.5, cex.axis=1.5, xlim=c(-3,20), ylim=c(0,1.3) )
lmmod2 <- lm( prop2 ~ dose2 )
abline( lmmod2$coef, lwd=2, col=2 )
abline( h=c(0,1), lwd=2, lty=2, col='gray80' )



####
## Logistic Regression
####

## Construct above data as a sequence of 0s and 1s
numdied2 <- norg2 - numsurv2
survstatus2 <- c( rep( 0, sum(numdied2) ), rep( 1, sum(numsurv2) ) )
newdose2 <- rep( c(dose2,dose2), c(numdied2, numsurv2) )

## Fit logit model to 0s and 1s
logitmod2 <- glm( survstatus2 ~ newdose2, family=binomial )
summary(logitmod2)

## Fit logit model to same data, stored the orginial way
logitmod2orig <- glm(formula = cbind(numsurv2, numdied2) ~ dose2,
                     family = binomial)


## To fit a fitted value for dose = 5 "manually"
theta5 <- logitmod2$coef[1] + logitmod2$coef[2] * 5
mu5 <- exp( theta5 ) / ( 1 + exp( theta5 ) )

ddose <- seq( -5, 20, len=201 )
## New data needs to be in a data.frame with the same name as
##   that used in the model fit
newdat <- data.frame( newdose2=ddose ) 
predprobresp = predict( logitmod2, newdata=newdat,
  type="response", se.fit=TRUE )
## Plot fitted values in response space
plot( dose2, prop2, xlab="Dose", ylab="Proportion or Organisms Surviving",
     pch=16, cex=1.5, cex.lab=1.5, cex.axis=1.5, xlim=c(-3,20), ylim=c(0,1.3) )
lmmod2 <- lm( prop2 ~ dose2 )
abline( lmmod2$coef, lwd=2, col=2 )
abline( h=c(0,1), lwd=2, lty=2, col='gray80' )
lines( ddose, predprobresp$fit, lwd=2, col=4 )
lines( ddose, predprobresp$fit + 2*predprobresp$se.fit, lwd=2, col=4, lty=2 )
lines( ddose, predprobresp$fit - 2*predprobresp$se.fit, lwd=2, col=4, lty=2 )

## Plot fitted values in link space
predproblink = predict( logitmod2, newdata=newdat,
  type="link", se.fit=TRUE )
plot( ddose, predproblink$fit, type='l', xlab="Dose",
     ylab="Log-Odds of Organisms Surviving", lwd=2,
     cex.lab=1.5, cex.axis=1.5 )
lines( ddose, predproblink$fit + 2*predproblink$se.fit, lwd=2, lty=2 )
lines( ddose, predproblink$fit - 2*predproblink$se.fit, lwd=2, lty=2 )

## Diagnostic plots for two versions of the same model
par(mfrow=c(1,2), pch=16, cex=1.5, cex.lab=1.5, cex.axis=1.5,
    cex.main=1.5, cex.sub=1.5, lwd=2 )

plot( logitmod2, which=1, main="Bernoulli" )
plot( logitmod2orig, which=1, main="Binomial" )

plot( logitmod2, which=2, main="Bernoulli" )
plot( logitmod2orig, which=2, main="Binomial" )

plot( logitmod2, which=3, main="Bernoulli" )
plot( logitmod2orig, which=3, main="Binomial" )

plot( logitmod2, which=5, main="Bernoulli" )
plot( logitmod2orig, which=5, main="Binomial" )

dev.off()


####
## Poisson Regression
####

## Some data
fd <- data.frame(
  Plot  = LETTERS[1:14],
  pH    = c(6.5,7.2,8.3,7.6,6.1,7.9,7.7,8.2,7.4,7.1,6.3,8.0,7.1,6.9),
  Treatment = factor(rep(c("Yes","No"),c(6,8))),
  Acres = c(1.3,4.2,2.1,3.2,3.2,1.4,1.8,1.8,2.5,3.2,2.2,4.6,1.2,1.5),
  Count = c(  8, 13,  7, 16, 16,  4,  0,  5,  7,  12, 4,  7,  5,  3))

## Plot data
with(fd, plot(pH,Count,col=Treatment,xlab="pH",ylab="# Diseased Plants",
              pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5))
legend("topright",c("Untreated","Treated"),pch=16,cex=1.5,col=1:2)

## Fit a Poisson Model
pmod <- glm( Count ~ pH + Treatment, data=fd,
            family=poisson )
summary(pmod)
anova(pmod,test="Chisq")
fitted( pmod )

newdata <- expand.grid( pH=seq(5.8,8.8,len=101),
                       Treatment=factor(c("Yes","No")) )
predpmod <- predict( pmod, newdata=newdata, type="response", se.fit=TRUE )

with(fd, plot(pH,Count,col=Treatment,xlab="pH",ylab="# Diseased Plants",
              pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5))
legend("topright",c("Untreated","Treated"),pch=16,cex=1.5,col=1:2)
subYes <- which(newdata$Treatment=="Yes")
lines( newdata$pH[subYes], predpmod$fit[subYes], lwd=2, col=2 )
lines( newdata$pH[-subYes], predpmod$fit[-subYes], lwd=2, col=1 )

## Incorporate Acres into analysis
with(fd, plot(pH,Count,col=Treatment,xlab="pH",ylab="# Diseased Plants",
              pch=16,cex=Acres,cex.lab=1.5,cex.axis=1.5))
legend("topright",c("Untreated","Treated","Acres=4","Acres=1"),
       pch=16,cex=1.5,pt.cex=c(1.5,1.5,4,1),col=c(1,2,1,1))

pmod2 <- glm( Count ~ pH + Treatment, data=fd,
             offset=log(Acres), family=poisson )
summary(pmod2)
anova(pmod2,test="Chisq")
fitted( pmod )


####
## AIC Weights
####

x <- seq(0.4,20,0.4)
y <- c(3,4,1,6,7,4,8,12,15,10,17,5,4,23,6,39,8,6,15,
       73,13,20,26,34,37,15,3,31,14,16,54,38,39,29,
       122,38,68,18,103,3,38,25,28,105,35,66,91,5,16,62)

normmod  <- glm( y ~ x, family=gaussian(link="identity") )
lnormmod <- glm( y ~ x, family=gaussian(link="log") )
gammod   <- glm( y ~ x, family=Gamma )
poismod  <- glm( y ~ x, family=poisson )
AICs <- c( AIC(normmod), AIC(lnormmod), AIC(gammod), AIC(poismod) )
names(AICs) = c("Normal","Lognormal","Gamma","Poisson")
omegas <- AICs - min(AICs)
AICweights <- exp(-omegas) / sum(exp(-omegas))

newdat <- data.frame(x=seq(-3,24,len=201))
plot(x,y,xlab="Dose",ylab="Response",pch=16,cex=1.5,
     cex.lab=1.5,cex.axis=1.5)
lines(newdat$x,predict(normmod,newdata=newdat,type="response"),
       lty=2, lwd=2,col=1)
lines(newdat$x,predict(lnormmod,newdata=newdat,type="response"),
       lty=2, lwd=2,col=2)
lines(newdat$x,predict(gammod,newdata=newdat,type="response"),
       lty=2, lwd=2,col=3)
lines(newdat$x,predict(poismod,newdata=newdat,type="response"),
       lty=2, lwd=2,col=4)
legend("topleft",c("Normal","Lognormal","Gamma","Poisson"),
       lwd=2,lty=2,cex=1.5,col=1:4)
