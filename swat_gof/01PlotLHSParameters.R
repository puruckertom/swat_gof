lhs.parameters <- read.table(paste(lhs_dir,"goal.sf2",sep=""),header=TRUE)
summary(lhs.parameters)
flow.observed <- read.table(paste(lhs_dir,"observed.txt",sep=""),header=FALSE)
summary(flow.observed)
rough.flow.modeled <- read.delim(paste(lhs_dir,"q_1.out",sep=""),header=TRUE)
dim(rough.flow.modeled)

# examine mixing of swatcup latin hypercube sampling for specified parameters
#Nsim <- 1127

ncols <- length(dimnames(lhs.parameters)[[2]])
lhs.filename <- paste(lhs_dir,"lhs_ts.pdf",sep="")
pdf(file=lhs.filename,width=7.5, height=10.5, bg="white")
par(mfrow=c(5,3))
for(i in 1:ncols){
  hist(lhs.parameters[,i],main=paste("Histogram of",colnames(lhs.parameters)[i]))
  x <- lhs.parameters[,i]
  x1 <- x[-Nsim]
  x2 <- x[-1]
  plot(x1,x2)
  acf(x)
}
dev.off()

lhs.filename <- paste(lhs_dir,"lhs_parameters.csv",sep="")
write.csv(lhs.parameters,file=lhs.filename)
