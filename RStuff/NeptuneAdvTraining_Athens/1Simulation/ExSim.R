
## 1. Explore the features of the gamma distribution

## 1a. Graph the probability density function for the gamma distribution
##     with a range of shape values, keeping the rate parameter constant
curve( dgamma( x, 0.5, 1 ), from=0, to=10, ylab="pdf" )
curve( dgamma( x, 1, 1 ), from=0, to=10, col=2, add=TRUE )
curve( dgamma( x, 2, 1 ), from=0, to=10, col=3, add=TRUE )
curve( dgamma( x, 4, 1 ), from=0, to=10, col=4, add=TRUE )
curve( dgamma( x, 8, 1 ), from=0, to=10, col=5, add=TRUE )

## 1b. Graph the probability density function for the gamma distribution
##     with a range of rate values, keeping the shape parameter constant
curve( dgamma( x, 2, 8 ), from=0, to=10, ylab="pdf" )
curve( dgamma( x, 2, 4 ), from=0, to=10, col=2, add=TRUE )
curve( dgamma( x, 2, 2 ), from=0, to=10, col=3, add=TRUE )
curve( dgamma( x, 2, 1 ), from=0, to=10, col=4, add=TRUE )
curve( dgamma( x, 2, 0.5 ), from=0, to=10, col=5, add=TRUE )

## 1c. Graph the probability density function for the gamma distribution
##     with a range of shape values, keeping the mean of the distribution
##     constant (mean = shape*rate)
curve( dgamma( x, 0.5, 0.5/4 ), from=0, to=10, ylab="pdf" )
curve( dgamma( x, 1, 1/4 ), from=0, to=10, col=2, add=TRUE )
curve( dgamma( x, 2, 2/4 ), from=0, to=10, col=3, add=TRUE )
curve( dgamma( x, 4, 4/4 ), from=0, to=10, col=4, add=TRUE )
curve( dgamma( x, 8, 8/4 ), from=0, to=10, col=5, add=TRUE )

## 1d. Graph the cumulative distribution function for the gamma
##     distribution with a range of shape values, keeping the rate
##     parameter constant
curve( pgamma( x, 0.5, 1 ), from=0, to=10, ylab="cdf" )
curve( pgamma( x, 1, 1 ), from=0, to=10, col=2, add=TRUE )
curve( pgamma( x, 2, 1 ), from=0, to=10, col=3, add=TRUE )
curve( pgamma( x, 4, 1 ), from=0, to=10, col=4, add=TRUE )
curve( pgamma( x, 8, 1 ), from=0, to=10, col=5, add=TRUE )

## 1e. Compute percentiles of a gamma distribution with shape 2
##     and rate 1: 5th, 25th, 50th, 75th, and 95th percentiles
qgamma( c(0.05, 0.25, 0.50, 0.75, 0.95), 2, 1 )

## 1f. Simulate 20 random numbers from a gamma distribution
##     with shape 0.5 and rate 0.5.  Compute the histogram
##     of the simulated values.  How does the histogram compare
##     to the density function?  Repeat for 100 random numbers,
##     1000 random numbers, 10,000 random numbers.
hist( rgamma( 20, 0.5, 0.5 ), freq=FALSE )
curve( dgamma( x, 0.5, 0.5 ), add=TRUE, col=2 )
hist( rgamma( 100, 0.5, 0.5 ), freq=FALSE )
curve( dgamma( x, 0.5, 0.5 ), add=TRUE, col=2 )
hist( rgamma( 1000, 0.5, 0.5 ), freq=FALSE )
curve( dgamma( x, 0.5, 0.5 ), add=TRUE, col=2 )
hist( rgamma( 10000, 0.5, 0.5 ), freq=FALSE )
curve( dgamma( x, 0.5, 0.5 ), add=TRUE, col=2 )



## 2. Explore the features of the beta distribution
## 2a. Look at the pdf of the beta distribution
##     for several sets of parameter values (shape1, shape2):
##     (0.5,0.5); (0.5,2); (1,1); (5,2); (4,1); (5,10)
curve( dbeta( x, 0.5, 0.5 ), from=0, to=1, ylab="pdf", ylim=c(0,3.5) )
curve( dbeta( x, 0.5, 2 ), from=0, to=1, col=2, add=TRUE )
curve( dbeta( x, 1, 1 ), from=0, to=1, col=3, add=TRUE )
curve( dbeta( x, 5, 2 ), from=0, to=1, col=4, add=TRUE )
curve( dbeta( x, 4, 1 ), from=0, to=1, col=5, add=TRUE )
curve( dbeta( x, 5, 10 ), from=0, to=1, col=6, add=TRUE )

## 2b. Estimate the mean, standard deviation, and skewness
##     of a beta distribution based on a random sample
##     from a beta distribution
x <- rbeta( 1000, 4, 8 )
mean(x)
sd(x)
skewness <- function(z){
  thirdcentralmoment <- mean( ( z - mean(z) )^3 )
  secondcentralmoment <- mean( ( z - mean(z) )^2 )
  skewness <- thirdcentralmoment / secondcentralmoment^1.5
  return( skewness )
}
skewness(x)


## 3. Explore the sample function
## 3a. Randomly choose 5 distinct values from the integers from 1 to 20
sample( 1:20, 5, replace=FALSE )
## 3b. Randomly choose 30 values from the integers from 1 to 20
sample( 1:20, 30, replace=TRUE )
## Bonus - bootstrap 
## 3c. Construct a bootstrap estimate for the standard error of
##     the sample skewness of a beta distribution with shape parameters
##     4 and 8, based on 30 samples
## 3c1. Generate a sample from the distribution
x <- rbeta( 30, 4, 8 )
## 3c2. Construct many resamples of the data
## This code will generate 1000 new re-samples of the data
##   each of sample size 30
xresample <- matrix( sample( x, 1000 * 30, replace=TRUE ), ncol=30 )
## 3c3. Compute the skewness for each resample
xresampleskew <- apply( xresample, 1, skewness )
## 3c4. Look at the bootstrap distribution of the skewness
hist( xresampleskew )
## 3c5. Compute the bootstrap standard error of the skewness
sd( xresampleskew )
## Note: the boot package provides a one-line command for constructing
##   bootstrap estimates


## 4. Test out setting random number seeds
## 4a. Set the random number seed the same as your neighbor
##     and generate 5 cauchy random variables.
##     Check that the 5 number match your neighbor
set.seed(1234)
rcauchy( 5 )
## 4b. Repeat a and show that you get the exact same results
set.seed(1234)
rcauchy( 5 )
## 4c. Generate 5 more and show that they're different if you
##     don't reset the random number seed
rcauchy( 5 )
## 4d. Set the random number seed differently from your neighbor
##     and compare a histogram of 50 cauchy random variables
## You:
set.seed(1234)
hist( rcauchy( 50 ) )
## Your neighbor:
set.seed(12345)
hist( rcauchy( 50 ) )
## Note: The cauchy distribution has extremely long tails and thus
##   produces occasional values that are very high or very low


## 5. Probability Integral Transform and Multivariate
## 5a. Generate a (sub-standard) random sample from the normal
##     distribution by generating uniform random variates and
##     using the probability integral transform
u <- runif( 1000 )
x <- qnorm( u, 0, 1 )
hist( x )
## 5b. Generate a (sub-standard) random sample from the
##     uniform distribution by generating normal random
##     variates and tranforming to uniform
x <- rnorm( 1000, 0, 1 )
u <- pnorm( x, 0, 1 )
## 5c. Generate a set of multivariate normal random variates
##     with dimenstion 3, that has correlations of:
#      x.y: 0.6, x.z: 0.9, y.z: 0.2
means <- c( 0, 0, 0 )
cormat <- matrix( c(  1, 0.6, 0.9,
                    0.6,   1, 0.2,
                    0.9, 0.2,   1 ), ncol=3 )
xyz <- MASS::mvrnorm( 1000, means, cormat )
colnames( xyz ) <- c("x","y","z")
pairs( xyz )
cor( xyz )
## 5d. Generate a set of correlated uniform random variates
##     by transforming the correlated normal variates.
##     What is the correlation now?
uvw <- pnorm( xyz )
colnames( uvw ) <- c("u","v","w")
pairs( uvw )
cor( uvw )
## 5e. Generate a set of correlated exponential random variates
##     by transforming the correlated uniforms.
##     What is the correlation now?
abc <- data.frame( a=qgamma( uvw[,1], 1, 0.5 ),
                   b=qgamma( uvw[,2], 2, 0.1 ),
                   c=qgamma( uvw[,3], 0.5, 2 ) )
pairs( abc )
cor( abc )
## 5f. Compute the derived quantity a / ( b + c ) for the
##     gamma random variates.  Look at the distribution, and
##     calculate summary statistics
d <- abc$a / ( abc$b + abc$c )
hist( d )
summary( d )
sd( d )
## 5g. Repeat 5f, but with uncorrelated random variates.
##     How do the results differ?
nabc <- data.frame( a=rgamma( 1000, 1, 0.5 ),
                    b=rgamma( 1000, 2, 0.1 ),
                    c=rgamma( 1000, 0.5, 2 ) )
e <- nabc$a / ( nabc$b + nabc$c )
brks <- seq( 0, max(d,e)+0.1, 0.05 )
par( mfrow=c(2,1) )
hist( d, breaks=brks )
hist( e, breaks=brks )
summary( e )
sd( e )
## Reset graphing window:
par( mfrow=c(1,1) )

## 6. Monte Carlo integration
## 6a. Compute a Monte Carlo estimate of the mean value of
##     exp(x)/(x+1), based on 100 random variates from the Student's t
##     distribution with 2 degrees of freedom
##     Correct answer = 15.552
x <- rgamma( 100, 6, 6 )
mean( exp(x)/x^4 )
## 6b. Increase the sample size to 1000 and re-compute
x <- c( x, rgamma( 900, 6, 6 ) )
mean( exp(x)/x^4 )
## 6c. Increase the sample size to 10,000 and re-compute
x <- c( x, rgamma( 9000, 6, 6 ) )
mean( exp(x)/x^4 )
## 6d. Increase the sample size to 100,000 and re-compute
x <- c( x, rgamma( 90000, 6, 6 ) )
mean( exp(x)/x^4 )
## 6e. Repeat 6a-6d, using a quasi-random sequence
library(gsl)
q <- qrng_alloc( dim=1, type="sobol" )
u <- qrng_get( q, 100 )
y <- qgamma( u, 6, 6 )
mean( exp(y)/y^4 )
u <- c( u, qrng_get( q, 900 ) )
y <- qgamma( u, 6, 6 )
mean( exp(y)/y^4 )
u <- c( u, qrng_get( q, 9000 ) )
y <- qgamma( u, 6, 6 )
mean( exp(y)/y^4 )
u <- c( u, qrng_get( q, 90000 ) )
y <- qgamma( u, 6, 6 )
mean( exp(y)/y^4 )
## 6f. Which result looks better?
##     The expected value of this function depends very heavily
##     on the tails of the distribution.
##     With the pseudo-random sequence, you might have gotten
##     closer to the answer quickly, but likely the answer did
##     not stabilize quickly as sample size was increased
##     With the quasi-random sequence, the small sample size
##     answers are relatively poor, because the tails of the
##     distribution are not "sampled" yet.  But as the sample
##     size is increased, the estimate generally improves

