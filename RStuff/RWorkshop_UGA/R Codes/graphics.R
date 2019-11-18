## Beer Preference example

beer <- c(3, 4, 1, 1, 3, 4, 3, 3, 1, 3, 2, 1, 2, 1, 2, 3, 2, 3, 1, 1, 1, 1, 4, 3, 1)
# (1) Domestic can, (2) Domestic bottle, (3) Microbrew and (4) Import

barplot(table(beer), col=c("lightblue", "mistyrose", "lightcyan","cornsilk"))

barplot(table(beer)/length(beer), col=c("lightblue", "mistyrose", "lightcyan","cornsilk"), names.arg=c("Domestic can", "Domestic bottle", "Microbrew",  "Import"), ylab="Relative frequency", main="Beer Preference Survey")

beer.counts <- table(beer) # store the table result
pie(beer.counts) # first pie -- kind of dull
names(beer.counts) <- c("Domestic\n can","Domestic\n bottle", "Microbrew","Import") # give names
pie(beer.counts) # prints out names

## Stem-and-leaf

scores <- c(2, 3, 16, 23, 14, 12, 4, 13, 2, 0, 0, 0, 6, 28, 31, 14, 4, 8, 2, 5)
stem(scores)

## histogram

x <- rnorm(1000)
hist(x, xlab="data")
hist(x, probability=T, xlab="data")
z <- seq(from=-3, to=3, by=0.01)
lines(z, dnorm(z), col=2)

## Boxplot

growth <- c(75,72,73,61,67,64,62,63) # the size of flies
sugar <- c("C","C","C","F","F","F","S","S") 
fly <- list(growth=growth, sugar=sugar)
boxplot(fly$growth)
boxplot(growth~sugar, xlab="Sugar Type", ylab="Growth", main="Growth against sugar types", data=fly)

## Scatterplot

plot(cars$speed, cars$dist) # the speed of cars and the distances taken to stop
attach(cars)
plot(speed, dist, col="blue", pch="+", ylab="Distance taken to stop", xlab="Speed", ylim=c(-20, 140))
lm(dist~speed)
abline(-17.579, 3.932, col="red")
title(main="Scatterplot with best fit line", font.main=4)

## Scatterplot matrix

attach(iris)
pairs(iris[,1:4])
pairs(iris[Species=="virginica", 1:4])

## 2D Histogram

library(hexbin)
plot(hexbin(iris[,3], iris[,4]), xlab="Petal Length", ylab="Petal Width")



