if (x<3) print(x<3) else print("x>=3")

x <- 4
if (x<3) {
	print("x<3")
	z <- 1
} else {
	print("x>=3")
	z <- 2
}

x <- 1:1000000
y <- x^2
n <- length(x)
z <- rep(0, n)
for ( i in 1:n) {
	z[i] <- x[i]^2
}

n <- 0 
sum.so.far <- 0
while ( sum.so.far <= 1000 ) {
	n <- n+1
	sum.so.far <- sum.so.far + n
}
print(c(n, sum.so.far))
sum(1:45)


my.stat <- function(x)
{
	m <- mean(x)
	s <- sd(x)
	res <- list(x=x, m=m, s=s)
	par(mfrow=c(1,2))
	boxplot(x, main="Boxplot")
	hist(x, prob=T, col="lightgray", main="Histrogram", xlab="data")
	z <- seq(from=-0, to=6, by=0.01)
	lines(z, dnorm(z, mean=3, sd=1), col=2, lwd=3, lty=2)
	return(res)
}

X <- rnorm(1000, mean=3, sd=1)
my.analysis <- my.stat(x=X)
my.analysis$m
my.analysis$s
