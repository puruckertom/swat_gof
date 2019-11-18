sf_dir <- "m://athdrive/R/SpectralFlow/"

#  the flow values are the "FLOW_OUTcms" column in the "rch" table of the Access database

obsflow <- read.csv(paste(sf_dir,"2002_2010_julian.csv",sep=""),header=TRUE)
mpeflow <- read.csv(paste(sf_dir,"mpe_uncalibrated2.txt",sep=""),header=TRUE)
ncdcflow <- read.csv(paste(sf_dir,"ncdc_uncalibrated.txt",sep=""),header=TRUE)

# jan 1, 2002 to 325 days deep into 2010
dim(obsflow)
#[1] 3247    3
summary(obsflow)
dimnames(obsflow)[[2]]
obsflow <- obsflow[1:2922,]

# jan 1, 2002 to Dec 31, 2006
dim(mpeflow)
#[1] 2922   46
summary(mpeflow)
dimnames(mpeflow)[[2]]
# jan 1, 2002 to Dec 31, 2009
dim(ncdcflow)
#[1] 2922   46
summary(ncdcflow)
dimnames(ncdcflow)[[2]]

timecounter <- 1:2922

spectralflow <- as.data.frame(cbind(timecounter,obsflow$year,mpeflow$YEAR,ncdcflow$YEAR,obsflow$day,mpeflow$MON,ncdcflow$MON,obsflow$flow,mpeflow$FLOW_OUTcms,ncdcflow$FLOW_OUTcms))
class(spectralflow)
dimnames(spectralflow)[[2]] <- c("timecounter","year","year2","year3","day","day2","day3","obsflow","mpeflow","ncdcflow")
spectralflow$year2 <- NULL
spectralflow$year3 <- NULL
spectralflow$day2 <- NULL
spectralflow$day3 <- NULL
dimnames(spectralflow)[[2]]

#drop leap year Dec 31s
dim(spectralflow[spectralflow$day!=366,])
spectralflow <- spectralflow[spectralflow$day!=366,]
spectralflow$timecounter <- 1:2920
dimnames(spectralflow)[[1]] <- 1:2920
class(spectralflow)
dimnames(spectralflow)[[2]]

#create time series objects
2920/365

spectralflow.matrix <- as.matrix(cbind(spectralflow$obsflow,spectralflow$mpeflow,spectralflow$ncdcflow))
spectralflow.ts <- ts(spectralflow.matrix, start=c(2002, 1), frequency=365)
plot(spectralflow.ts,plot.type="multiple",col=4,main="NC subbasin Time Series")

plot(spectralflow.ts,lag(spectralflow.ts,1),plot.type="multiple",col=4,main="NC subbasin Time Series")

par(mfrow=c(3,1))
plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],1),col=1)
plot(spectralflow.ts[,2],lag(spectralflow.ts[,2],1),col=2)
plot(spectralflow.ts[,3],lag(spectralflow.ts[,3],1),col=3)

par(mfrow=c(3,1))
plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],2),col=1)
plot(spectralflow.ts[,2],lag(spectralflow.ts[,2],2),col=2)
plot(spectralflow.ts[,3],lag(spectralflow.ts[,3],2),col=3)

par(mfrow=c(3,1))
plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],3),col=1)
plot(spectralflow.ts[,2],lag(spectralflow.ts[,2],3),col=2)
plot(spectralflow.ts[,3],lag(spectralflow.ts[,3],3),col=3))