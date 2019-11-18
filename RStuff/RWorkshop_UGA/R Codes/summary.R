library(Matching)
data(lalonde)
attach(lalonde)

boxplot(re78~treat)

re78.treat <- re78[treat==1]
re78.contr <- re78[treat==0]

mean(re78); sd(re78)
mean(re78.treat, trim=.01); sd(re78.treat)
mean(re78.contr, trim=.01); sd(re78.contr)

# plot(density(re78.contr), xlim=c(0, max(re78)), xlab="re78", main="", lwd=3)
# lines(density(re78.treat), lwd=3, col="red")
# legend(locator(1), legend=c("Control", "Treatment"), col=c(1, 2), lty=c(1,1))

summary.stats <- function(y)
{
	x <- na.omit(y)       # Omit missing values
	m <- mean(x)
	s <- sd(x)
	z <- (x-m)/s          # Standardization
	skew <- mean(z^3)     # Skewness
	kurt <- mean(z^4)     # Kurtosis
	mini <- min(x)        # Minimum
	maxi <- max(x)        # Maximum
	q <- quantile(x, probs=c(.25, .50, .74))
	res <- list(average=m, stdev=s, skewness=skew, kurtosis=kurt, 
	            q1=q[1], q2=q[2], q3=q[3], minimum=mini, maximum=maxi)
	
	return(res)
}

summary.stats(re78.treat)
summary.stats(re78.contr)


par(mfrow=c(3,2))
plot(re78~age)   # plot(age, re78)
plot(re78~educ)
boxplot(re78~black, col="lightblue", xlab="black", ylab="re78")
boxplot(re78~hisp, col="lightgray", xlab="hisp", ylab="re78")
boxplot(re78~married, col="darkred", xlab="married", ylab="re78")
boxplot(re78~nodegr, col="cyan", xlab="nodegr", ylab="re78")

table(treat, black)
table(treat, hisp)
table(treat, married)
table(treat, nodegr)
par(mfrow=c(1,2))
boxplot(age~treat, names=c("Control", "Treatment"), ylab="age")
boxplot(educ~treat, names=c("Control", "Treatment"), ylab="educ")
  
