data <- read.csv("01-2.csv")
data.c <- data[data$Track==0,]
data.t <- data[data$Track==2,]
data.02 <- rbind(data.c, data.t)
rm(data, data.c, data.t)
names(data.02)

