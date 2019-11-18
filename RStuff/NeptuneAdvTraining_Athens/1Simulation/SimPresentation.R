
###############################################
## Part A - Probability Distributions
###############################################

## Set some default plotting parameters - not repeated
##   later in code, so only applies if graphing windows
##   are not closed in the interim
par( lwd=2, cex=1.25, pch=16, cex.axis=1.25, cex.lab=1.25, cex.main=1.25 )

## Graph the density function of a normal distribution
##   with mean 8 and standard deviation 2.5
curve( dnorm( x, mean=8, sd=2.5 ), from=0, to=16 )

## Probability density function of a normal distribution
##   with mean 8 and standard deviation 2.5, evaluated at 44
dnorm( 44, mean=8, sd=2.5 )
## repeated but compute log-density - more numerically stable
dnorm( 44, mean=8, sd=2.5, log=TRUE )


## Graph the CDF of a normal distribution
##   with mean 8 and standard deviation 2.5
curve( pnorm( x, mean=8, sd=2.5 ), from=0, to=16 )

## The CDF for a normal distribution with mean 8 and standard
##   deviation 2.5, evaluated at 44
pnorm( 44, mean=8, sd=2.5 )  # numerical underflow makes the answer == 1
## To compute the upper tail instead [Prob(X>=44)]
pnorm( 44, mean=8, sd=2.5, lower.tail=FALSE )
## To compute in log-space for numerical stability
pnorm( 44, mean=8, sd=2.5, lower.tail=FALSE, log.p=TRUE )


## Graph the inverse CDF of a normal distribution
##   with mean 8 and standard deviation 2.5
curve( qnorm( x, mean=8, sd=2.5 ), from=0, to=1 )

## The inverse CDF for a normal distribution with mean 8
##   and standard deviation 2.5, evaluated at the 10th percentile
qnorm( 0.1, mean=8, sd=2.5 )
## Now at the 99.9999th percentile
qnorm( 0.999999, mean=8, sd=2.5 )
## If underflow in the first argument is an issue, could
##   instead use the upper tail
qnorm( 0.000001, mean=8, sd=2.5, lower.tail=FALSE )

## Generate 1000 random variables from the normal distribution
##   with mean 8 and standard deviation 2.5 (and graph the
##   resulting sample as a histogram)
hist( rnorm( 1000, mean=8, sd=2.5 ) )


## Repeat density, CDF, inverse CDF, and simulation for a
##   Weibull distribution with shape=1.5 and scale=12
curve( dweibull( x, shape=1.5, scale=12), from=0, to=40 )
curve( pweibull( x, shape=1.5, scale=12), from=0, to=40 )
curve( qweibull( x, shape=1.5, scale=12), from=0, to=1 )
hist( rweibull( 1000, shape=1.5, scale=12 ) )


## Discrete samples

## Randomly select 8 letters from the first 5 letters of the
##   alphabet, chosen with probabilities inversely proportional 
##   to position
sample( letters[1:5], 8, prob=1/(1:5), replace=TRUE )
## Check that sample probabilities look right
barplot( table( sample( letters[1:5], 1000, prob=1/(1:5), replace=TRUE ) ) )

## Randomly re-order the alphabet
sample( letters, 26, replace=FALSE )

## Set up a full factorial experiment with 3 experimental factors
Exper = expand.grid( Temp=c(60, 75), Flow=c(100,200,300),
  Species=factor(c("A","B")) )
## Randomize the order of runs for the experiment
Exper[ sample( nrow(Exper) ), ]



###############################################
## Part B - Random Number Generation
###############################################

## Store the current random number seed and then generate
##   5 random normal variates
tmp <- .Random.seed
rnorm( 5 )
## If you generate 5 more, the result is different
rnorm( 5 )
## Restore the random number seed to its previous state
##   and generate again - to get the same results as the first time
.Random.seed <- tmp
rnorm( 5 )

## Set the random number seed the easy way
set.seed( 123 )
rnorm( 5 )
## The previous code should have produced precisely these values
##   (up to display rounding):
##   [1] -0.56047565 -0.23017749  1.55870831  0.07050839  0.12928774

## Probability integral transform


###############################################
## Part C - Multivariate Simulation
###############################################

## Generate from the 2-D multivariate normal distribution
##   with means 10 & 50 standard deviations 2 & 5, and
##   correlation 0.7
mu <- c(10, 50)
Sigma <- matrix( c(2^2, 2*5*0.7, 2*5*0.7, 5^2), ncol=2 )
xy <- MASS::mvrnorm( 100, mu, Sigma )
plot( xy, xlab="x", ylab="y" )

## Generate (X,Y) pairs with a regression relationship
x <- rnorm( 100, 5, 2 )
y <- 3.2 + 5 * x + rnorm( 100, 0, 1.5 )
plot( x, y )

## Generate (X,Y,Z) triples with a more complicated stochastic
##   relationship
x <- rlnorm( 100, 1, 0.5 )
y <- rlnorm( 100, 2, 0.3 )
z <- rgamma( 100, 0.5*x, scale=x/y )
pairs( cbind( x, y, z ) )

## Inducing correlation example
## Generate mv normals with 0 mean, unit variance, and correlation 0.7
mu <- c(0,0)
Sigma <- matrix( c(1,0.7,0.7,1), ncol=2 )
x <- MASS::mvrnorm( 100, mu, Sigma )
u <- pnorm( x )
nx <- qgamma( u[,1], shape=0.5, rate=0.3 )
ny <- qlnorm( u[,2], meanlog=1, sdlog=0.75 )
plot( nx, ny )
cor( nx, ny )


###############################################
## Part D - Monte Carlo Integration
###############################################

## Compute the Monte Carlo estimate of E[e^X] for X~normal(0,1)
##   using just 4 random variables
set.seed( 1 )
tmp <- rnorm( 4 )
mean( exp( tmp ) )
## Repeat with a different random number seed
##   Look at the results of tmp
set.seed( 72 )
tmp <- rnorm( 4 )
mean( exp( tmp ) )
## Repeat with a different random number seed
##   Look at the results of tmp
set.seed( 150 )
tmp <- rnorm( 4 )
mean( exp( tmp ) )
## Show the sampling distribution of such an estimate
## Repeat the experiment in a loop, storing the results each tim
mnex <- numeric( 10000 )
for(i in 1:10000){
  mnex[i] <- mean( exp( rnorm( 4 ) ) )
}
hist( mnex, xlab=expression("Estimate of E["*e^X*"]"),main="")

## Alternative Riemann-integral-like estimate
tmp <- qnorm( c(0.2, 0.4, 0.6, 0.8) )
mean( exp( tmp ) )


## Quasi-random numbers
q <- gsl::qrng_alloc( dim=1, type="sobol" )
## Get the first 10 numbers in the sequence
gsl::qrng_get( q, 10 )
## Get the next 10 numbers in the sequence
gsl::qrng_get( q, 10 )


## Latin hypercube sampling
tmp <- lhs::randomLHS( 10, 3 )
pairs( tmp )
