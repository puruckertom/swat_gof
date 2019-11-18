a <- 3
b <- 2
x <- (a+b)/2
print(x)     

x <- "Hi!"
x

pnorm(-1.96, mean=0, sd=1)

## Classical example: Student's sleep data
attach(sleep)
plot(extra ~ group)
t.test(extra[group == 1],
       extra[group == 2], paired = T)
## Misleading test
t.test(extra ~ group, data = sleep)


## Matchinf example with LaLonde data
library(MatchIt)
library(Zelig)
data(lalonde)

exact.match <- matchit(treat~educ+black+hisp+nodegr+married+re74+re75, data=lalonde, method="exact")
summary(exact.match)

exact.out <- zelig(re78~treat+educ+black+hisp+nodegr+married+re74+re75, data=match.data(exact.match), model="ls")
summary(exact.out)

