setwd("/Users/katieprice/Dropbox/ktp/R_workshop/")
###################
#IMPORT/EXPORT DATA
###################
#Read data
x <- scan() #manually enter 1..n as called

#From a file: read.table(), use with .txt "sep=\t" for tab-delimited
x <- read.table(file="tabletxt", header=T, sep=" ")

#From a file: read.csv(), use with .csv
x <- read.csv(file="table.csv", header=T)

x <- read.table(file.choose(), header=T) #opens finder, if you don't want to write filepath

x <- read.table(file="/Users/katieprice/Dropbox/ktp/R_workshop/ads.txt", header=T, sep=" ")
x #check whether dataframe properly loaded
fix(x) #opens data editor, doesn't work in R studio

library("MatchIt")
data() #find datasets built into currently loaded packages
data("lalonde") #load MatchIt's lalonde dataset
lalonde #see it

#Export data
#to console:
x <- scan() #manually enter
print(x) #print vector to screen
#to a file
x <- seq(from=0, to=1, by=0.1)
write.table(x, file="output.txt") #exports x as textfile to working directory, don't use "write", use "write.table"
write.csv(x, file="output.csv")

#foreign package for import/export of other common file types, doesn't always work right, safer to exchange txt or csv
install.packages("foreign")
library("foreign")

##########
#GRAPHICS
##########

## Beer Preference example

beer <- c(3, 4, 1, 1, 3, 4, 3, 3, 1, 3, 2, 1, 2, 1, 2, 3, 2, 3, 1, 1, 1, 1, 4, 3, 1)
# (1) Domestic can, (2) Domestic bottle, (3) Microbrew and (4) Import

barplot(table(beer), col=c("lightblue", "mistyrose", "lightcyan","cornsilk"))

barplot(table(beer)/length(beer), col=c("lightblue", "mistyrose", "lightcyan","cornsilk"), 
names.arg=c("Domestic can", "Domestic bottle", "Microbrew",  "Import"), ylab="Relative frequency", main="Beer Preference Survey")

beer.counts <- table(beer) # store the table result
pie(beer.counts) # first pie -- kind of dull
names(beer.counts) <- c("Domestic\n can","Domestic\n bottle", "Microbrew","Import") # give names
pie(beer.counts) # prints out names

## Stem-and-leaf

scores <- c(2, 3, 16, 23, 14, 12, 4, 13, 2, 0, 0, 0, 6, 28, 31, 14, 4, 8, 2, 5)
stem(scores)

## histogram

x <- rnorm(1000)
hist(x, xlab="data") #default is frequency
hist(x, probability=T, xlab="data") #probability=T means that height corresponds to relative frequency
z <- seq(from=-3, to=3, by=0.01)
lines(z, dnorm(z), col=2)
lines(z, dnorm(z, mean=mean(x), sd=sd(x)), col=3)
? hist

## Boxplot

growth <- c(75,72,73,61,67,64,62,63) # the size of flies
sugar <- c("C","C","C","F","F","F","S","S") 
fly <- list(growth=growth, sugar=sugar)
boxplot(fly$growth)
boxplot(growth~sugar, xlab="Sugar Type", ylab="Growth", main="Growth against sugar types", data=fly)
jpeg(file="flygrowth.jpg", width=480, height=360)
boxplot(growth~sugar, xlab="Sugar Type", ylab="Growth", main="Growth against sugar types", data=fly)
dev.off()

## Scatterplot

plot(cars$speed, cars$dist) # the speed of cars and the distances taken to stop
attach(cars) #allows use of names
plot(speed, dist, col="blue", pch="+", ylab="Distance taken to stop", xlab="Speed", ylim=c(-20, 140))
lm(dist~speed)
abline(-17.579, 3.932, col="red")
title(main="Scatterplot with best fit line", font.main=4)

## Scatterplot matrix

attach(iris)
iris
iris[,2]  #[,#] pulls out that column, e.g. [,2] second column
iris[,1:4] #extracts first four columns
pairs(iris[,1:3])
pairs(iris[,1:4])
pairs(iris[Species=="virginica", 1:4]) #extracts Virginica (by species column)

## 2D Histogram

library(hexbin)
plot(hexbin(iris[,3], iris[,4]), xlab="Petal Length", ylab="Petal Width")

############
#PROGRAMMING
############
library("MatchIt")
x<-5
if (x<3) print("x<3") else print ("x>4")
if (x<3) {print("x<3");z<-"M"} else {print("x>3");Z<-"F"}

#Iteration, loop
x<-1:10
n<-length(x)
y<-rep(0,n)
for(i in 1:n) {y[i]<-x[i]^2
							 }
z<-x^2
print(cbind(y,z))

x<-1:1000000
y<-x
for (i in 1:1000000) y[i] <- x[i]^2
z<-x[i]^2 #not looping through values will speed runtime, avoid loops if possible

# but sometimes loops are necessary
# e.g. while loop
n <- 0 
sum.so.far <- 0
while ( sum.so.far <= 1000 ) {
	n <- n+1
	sum.so.far <- sum.so.far + n
}
print(c(n, sum.so.far))
sum(1:45)

#Apply a function to every row/column
#apply()
A<-matrix(1:20, 4, 5)
apply(A,1,sum) #1 means to every row
apply(A,2,sum) #2 means to every column
apply(A,1,mean)

#Write new function
my.stat <- function(x) #my.stat = name of function, can be anything
{
	m <- mean(x)
	s <- sd(x)
	res <- list(x=x, m=m, s=s)
	par(mfrow=c(1,2))
	boxplot(x, main="Boxplot")
	hist(x, prob=T, col="lightgray", main="Histrogram", xlab="data")
	z <- seq(from=-0, to=6, by=0.01)
	lines(z, dnorm(z, mean=3, sd=1), col=2, lwd=3, lty=2) #lwd=line width, lty= line type (here is dashed)
	return(res)
}

X <- rnorm(1000, mean=3, sd=1)
my.analysis <- my.stat(x=X)

#################
#SUMMARIZING DATA
#################
library("MatchIt")
data()
data(lalonde)
attach(lalonde)

lalonde

boxplot(re78~treat)

re78.treat <- re78[treat==1]
re78.contr <- re78[treat==0]

mean(re78); sd(re78)
mean(re78.treat, trim=.01); sd(re78.treat)
mean(re78.contr, trim=.01); sd(re78.contr)
length(re78)
dim(lalonde) #returns 614 10, 614=n, 10=#variables
length(re78[treat==1]) #different ways to look at the number of treatment samples
sum(treat)
length(re78.treat)
 

plot(density(re78.contr), xlim=c(0, max(re78)), xlab="re78", main="", lwd=3)
lines(density(re78.treat), lwd=3, col="red")
#legend(locator(1), legend=c("Control", "Treatment"), col=c(1, 2), lty=c(1,1))

summary(lalonde)
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


par(mfrow=c(3,2)) #par sets up plot, 3 rows, 2 columns
plot(re78~age)   # plot(age, re78)
plot(re78~educ)
boxplot(re78~black, col="lightblue", xlab="black", ylab="re78")
boxplot(re78~hisp, col="lightgray", xlab="hispan", ylab="re78")
boxplot(re78~married, col="darkred", xlab="married", ylab="re78")
boxplot(re78~nodegr, col="cyan", xlab="nodegree", ylab="re78")

table(treat, black)
table(treat, hispan)
table(treat, married)
table(treat, nodegree)
par(mfrow=c(1,2)) #sets up plot, 1 row, 2 columns
boxplot(age~treat, names=c("Control", "Treatment"), ylab="age")
boxplot(educ~treat, names=c("Control", "Treatment"), ylab="educ")
  
#One sample t-test
#daily energy intake in kJ for 11 women
daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
#To investigate whether women's intake deviates from rec 7725 kJ
mean(daily.intake)
sd(daily.intake)
boxplot(daily.intake);abline(h=7725, col=2, lty=2) #need only intercept h for this type of reference line
t.test(daily.intake, mu=7725, alternative="less")
t.test(daily.intake, mu=7725, alternative="greater")
t.test(daily.intake, mu=7725, alternative="two.sided")

#One-sample Wilcoxon Signed-Rank
wilcox.test(daily.intake,mu=7725)

#Two-sample Tests
group <- c(rep("M",4), rep("F",6)) #assigns male to first four y-vals, female to last 6
y <- c(4.0, 3.8, 3.5, 3.1, 3.3, 3.5, 4.0, 4.2, 3.8, 3.7)
boxplot(y~group) #plot boxplot of GPA with a box for each gender

#Two-sample t-test
t.test(y~group)
wilcox.test(y~group)

#paired t-test
install.packages("ISwR")
library(ISwR)
attach(intake)
t.test(pre, post, paired=T)
wilcox.test(pre, post, paired=T)
#or
diff <- post-pre
t.test(diff, mu=0)
wilcox.test(diff, mu=0)

###########
# NORMALITY
###########
#to test for other distributions, transform then test normality, no packaged way to test other distributions

shapiro.test(daily.intake) #pval>0.05 = normal
qqnorm(daily.intake) #qq plot
 
#######
# ANOVA
#######

school<-c(3,2,2,2,1,1,3,3,1,3)
school<-factor(school, labels=c("A","B","C")) #assumes A is assigned to lowest number
y <- c(4.0, 3.8, 3.5, 3.1, 3.3, 3.5, 4.0, 4.2, 3.8, 3.7)
boxplot(y~school)

res<-lm(y~school) #linear model grades by school
anova(res)
summary(res) #gives full result of lm
pairwise.t.test(y,school,p.adj="bonferroni")
res<-aov(y~school) #alternative to creating res as lm then doing anova
TukeyHSD(res) #have to use aov method
kruskal.test(y~school)

#two-way anova
#heart rate after admin of enalaprilate
library(ISwR)
attach(heart.rate)
heart.rate
anova(lm(hr~subj+time))
#nonparametric equiv
friedman.test(hr~time|subj)

###########
#REGRESSION
###########

SAT <- c(388, 354, 361, 329, 331, 364, 399, 421, 398, 383)
GEN <- c("F", "F", "F", "F", "M", "M", "M", "F", "M", "M")
GPA <- c(4.0, 3.8, 3.5, 3.1, 3.3, 3.5, 4.0, 4.2, 3.8, 3.7)

cor(SAT, GPA, method="pearson") # kendall, spearman
cor(SAT, GPA, method="kendall")
cor(SAT, GPA, method="spearman")

par(mfrow=c(1,2))
plot(SAT, GPA)
boxplot(GPA~GEN, xlab="GENDER", ylab="GPA")

res <- lsfit(SAT, GPA) #simple linear regression, only continuous data
ls.print(res)

res.no <- lsfit(SAT, GPA, intercept=F)
ls.print(res.no)

t.test(GPA~GEN)

res.lm <- lm(GPA~GEN+SAT-1)
summary(res.lm)

#Stepwise, default is backward
data(state)
statedata <- data.frame(state.x77,row.names=state.abb, check.names=T)
g <- lm(Life.Exp~.,data=statedata)
summary(g)
step(g)
? step

#Categorical data
M <- as.table(rbind(c(25, 12), c(11,14)))
M
dimnames(M) <- list(education=c("High School","College"), income=c("Low","High"))
M
res <- chisq.test(M)
res$expected
res

##########
#EXCERCISE
##########
#[1] Find the built-in dataset named 'cars' and load it on your workspace.
data()
data(cars)
attach(cars)

#[2] Print the dataset on your console.
cars

#[3] Excute some exploratory data analyses on the dataset...
summary(cars)
plot(cars) #works only with data frames
plot(speed,dist) 
lm(dist~speed)
abline(-17.579, 3.932, col="red")
title(main="Scatterplot with best fit line", font.main=4)
boxplot(speed, dist)
boxplot(cars)
#same as boxplot (cars$speed, cars$dist, xlab="speed", ylab="dist")

#[4] Define a new binary variable for speed in the following way. 
   # speed < 15  --->  speed.b = low
   # speed >= 15 --->  speed.b = high

speed.b <- speed #so you don't write over raw speed
speed.b[speed < 15] <- "low"
speed.b
speed.b[speed >= 15] <- "high"
speed.b

#[5] Define a new variable (factor) for speed in the following way. 
          #speed < 15  --->  speed.l = low
    #15 <= speed < 20  --->  speed.l = mid
   # 20 <= speed       --->  speed.l = high

speed.l <- speed
speed.l[speed<15] <- "low"
speed.l[(speed>=15)&(speed<20)] <- "mid"
speed.l[speed>=20] <- "high"
speed.l


#[6] Define a new variable (factor) for dist in the following way. 
         # dist < 20  --->  dist.l = low
   # 20 <= dist < 40  --->  dist.l = mid
   # 40 <= dist < 60  --->  dist.l = high
   # 60 <= dist       --->  dist.l = xhigh

dist.l <- dist
dist.l[dist<20] <- "low"
dist.l[(dist>=20)&(dist<40)] <- "mid"
dist.l[(dist>=40)&(dist<60)] <- "high"
dist.l[dist>=60] <- "xhigh"
dist.l


#[7] t-test: speed.b vs. dist
t.test(dist~speed.b, alternative="two.sided")


#[8] One-way ANOVA: speed.l vs. dist 


#[9] Contingency table: speed.1 vs dist.l

#[10] Regression speed vs. distance