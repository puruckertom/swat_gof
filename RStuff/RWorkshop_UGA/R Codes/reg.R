SAT <- c(388, 354, 361, 329, 331, 364, 399, 421, 398, 383)
GEN <- c("F", "F", "F", "F", "M", "M", "M", "F", "M", "M")
GPA <- c(4.0, 3.8, 3.5, 3.1, 3.3, 3.5, 4.0, 4.2, 3.8, 3.7)

cor(SAT, GPA, method="pearson") # kendall, spearman



par(mfrow=c(1,2))
plot(SAT, GPA)
boxplot(GPA~GEN, xlab="GENDER", ylab="GPA")

res <- lsfit(SAT, GPA)
ls.print(res)

res.no <- lsfit(SAT, GPA, intercept=F)
ls.print(res.no)

res.lm <- lm(GPA~GEN+SAT-1)
summary(res.lm)
