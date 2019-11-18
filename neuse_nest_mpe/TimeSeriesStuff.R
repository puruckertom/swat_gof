library(tseries)
#library(punitroots)
library(CADFtest)
library(xts)

#Met data used here is from HCN

if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/ScalePaper/NeuseNestMPE/")
  output_dir <- path.expand("~/Dropbox/ktp/ScalePaper/NeuseNestMPE/")
}

#windows
if(.Platform$OS.type=="windows"){
	root_dir <- path.expand("d://Dropbox/ktp/ScalePaper/NeuseNestMPE/")
	output_dir <- path.expand("d://Dropbox/ktp/ScalePaper/NeuseNestMPE/")
}

Q.MC.dly <- read.csv(file=paste(root_dir,"DailyExplore/QMCdly97_12.csv",sep=""),header=T) 
Q.LR.dly <- read.csv(file=paste(root_dir,"DailyExplore/QLRdly97_12.csv",sep=""),header=T) 
Q.NC.dly <- read.csv(file=paste(root_dir,"DailyExplore/QNCdly97_12.csv",sep=""),header=T)
Q.FB.dly <- read.csv(file=paste(root_dir,"DailyExplore/QFBdly97_12.csv",sep=""),header=T) 
Precip.CH.dly <- read.csv(file=paste(root_dir,"DailyExplore/PrecipCHdly97_12.csv",sep=""),header=T)
Precip.SF.dly <- read.csv(file=paste(root_dir,"DailyExplore/PrecipSFdly97_12.csv",sep=""),header=T) 
Tmean.CH.dly <- read.csv(file=paste(root_dir,"DailyExplore/TmeanCHdly97_12.csv",sep=""),header=T)
Tmean.SF.dly <- read.csv(file=paste(root_dir,"DailyExplore/TmeanSFdly97_12.csv",sep=""),header=T)

QMC.S1h <- read.csv(file=paste(root_dir,"Storms/S1MChQ.csv",sep=""),header=T)
QMC.S2h <- read.csv(file=paste(root_dir,"Storms/S2MChQ.csv",sep=""),header=T)
QMC.S3h <- read.csv(file=paste(root_dir,"Storms/S3MChQ.csv",sep=""),header=T)
QMC.S4h <- read.csv(file=paste(root_dir,"Storms/S4MChQ.csv",sep=""),header=T)
QLR.S1h <- read.csv(file=paste(root_dir,"Storms/S1LRhQ.csv",sep=""),header=T)
QLR.S2h <- read.csv(file=paste(root_dir,"Storms/S2LRhQ.csv",sep=""),header=T)
QLR.S3h <- read.csv(file=paste(root_dir,"Storms/S3LRhQ.csv",sep=""),header=T)
QLR.S4h <- read.csv(file=paste(root_dir,"Storms/S4LRhQ.csv",sep=""),header=T)
QNC.S1h <- read.csv(file=paste(root_dir,"Storms/S1NChQ.csv",sep=""),header=T)
QNC.S2h <- read.csv(file=paste(root_dir,"Storms/S2NChQ.csv",sep=""),header=T)
QNC.S3h <- read.csv(file=paste(root_dir,"Storms/S3NChQ.csv",sep=""),header=T)
QNC.S4h <- read.csv(file=paste(root_dir,"Storms/S4NChQ.csv",sep=""),header=T)
QFB.S1h <- read.csv(file=paste(root_dir,"Storms/S1FBhQ.csv",sep=""),header=T)
QFB.S2h <- read.csv(file=paste(root_dir,"Storms/S2FBhQ.csv",sep=""),header=T)
QFB.S3h <- read.csv(file=paste(root_dir,"Storms/S3FBhQ.csv",sep=""),header=T)
QFB.S4h <- read.csv(file=paste(root_dir,"Storms/S4FBhQ.csv",sep=""),header=T)

PrecipMC.S1h <- read.csv(file=paste(root_dir,"Storms/S1_MC.csv",sep=""),header=T)
PrecipMC.S2h <- read.csv(file=paste(root_dir,"Storms/S2_MC.csv",sep=""),header=T)
PrecipMC.S3h <- read.csv(file=paste(root_dir,"Storms/S3_MC.csv",sep=""),header=T)
PrecipMC.S4h <- read.csv(file=paste(root_dir,"Storms/S4_MC.csv",sep=""),header=T)
PrecipLR.S1h <- read.csv(file=paste(root_dir,"Storms/S1_LR.csv",sep=""),header=T)
PrecipLR.S2h <- read.csv(file=paste(root_dir,"Storms/S2_LR.csv",sep=""),header=T)
PrecipLR.S3h <- read.csv(file=paste(root_dir,"Storms/S3_LR.csv",sep=""),header=T)
PrecipLR.S4h <- read.csv(file=paste(root_dir,"Storms/S4_LR.csv",sep=""),header=T)
PrecipNC.S1h <- read.csv(file=paste(root_dir,"Storms/S1_NC.csv",sep=""),header=T)
PrecipNC.S2h <- read.csv(file=paste(root_dir,"Storms/S2_NC.csv",sep=""),header=T)
PrecipNC.S3h <- read.csv(file=paste(root_dir,"Storms/S3_NC.csv",sep=""),header=T)
PrecipNC.S4h <- read.csv(file=paste(root_dir,"Storms/S4_NC.csv",sep=""),header=T)
PrecipFB.S1h <- read.csv(file=paste(root_dir,"Storms/S1_FB.csv",sep=""),header=T)
PrecipFB.S2h <- read.csv(file=paste(root_dir,"Storms/S2_FB.csv",sep=""),header=T)
PrecipFB.S3h <- read.csv(file=paste(root_dir,"Storms/S3_FB.csv",sep=""),header=T)
PrecipFB.S4h <- read.csv(file=paste(root_dir,"Storms/S4_FB.csv",sep=""),header=T)

#1cfs = 0.0283168cms
Q.MC.dly[,2] <- (Q.MC.dly[,2]*0.028317)
Q.LR.dly[,2] <- (Q.LR.dly[,2]*0.028317)
Q.NC.dly[,2] <- (Q.NC.dly[,2]*0.028317)
Q.FB.dly[,2] <- (Q.FB.dly[,2]*0.028317)

Q.MC.dly <- read.zoo(Q.MC.dly, format="%m/%d/%y")
Q.MC.dly <- as.xts(Q.MC.dly)
Q.LR.dly <- read.zoo(Q.LR.dly, format="%m/%d/%y")
Q.LR.dly <- as.xts(Q.LR.dly)
Q.NC.dly <- read.zoo(Q.NC.dly, format="%m/%d/%y")
Q.NC.dly <- as.xts(Q.NC.dly)
Q.FB.dly <- read.zoo(Q.FB.dly, format="%m/%d/%y")
Q.FB.dly <- as.xts(Q.FB.dly)
Precip.CH.dly <- read.zoo(Precip.CH.dly, format="%m/%d/%y")
Precip.CH.dly <- as.xts(Precip.CH.dly)
Precip.SF.dly <- read.zoo(Precip.SF.dly, format="%m/%d/%y")
Precip.SF.dly <- as.xts(Precip.SF.dly)
Tmean.CH.dly <- read.zoo(Tmean.CH.dly, format="%m/%d/%y")
Tmean.CH.dly <- as.xts(Tmean.CH.dly)
Tmean.SF.dly <- read.zoo(Tmean.SF.dly, format="%m/%d/%y")
Tmean.SF.dly <- as.xts(Tmean.SF.dly)

startdate <- as.Date("1996-10-01")
enddate <- as.Date("2012-09-30")
Ndays <- as.numeric(enddate-startdate + 1)
days <- as.Date(seq(as.Date(startdate):as.Date(enddate)))
days.text <- gsub("-","",as.Date(days, origin="1970-01-01"))
#days.text

par(mfrow=c(2,2))
plot(Q.FB.dly)
plot(Q.NC.dly)
plot(Q.LR.dly)
plot(Q.MC.dly)

par(mfrow=c(2,2))
plot(Precip.CH.dly)
plot(Precip.SF.dly)
plot(Tmean.CH.dly)
plot(Tmean.SF.dly)

dFB <- diff(Q.FB.dly)
dNC <- diff(Q.NC.dly)
dLR <- diff(Q.LR.dly)
dMC <- diff(Q.MC.dly)
dCHp <- diff(Precip.CH.dly)
dSFp <- diff(Precip.SF.dly)
dCHt <- diff(Tmean.CH.dly)
dSFt <- diff(Tmean.SF.dly)

#within series differences
par(mfrow=c(2,4))
plot(dFB)
plot(dNC)
plot(dLR)
plot(dMC)
plot(dCHp)
plot(dSFp)
plot(dCHt)
plot(dSFt)

lag.plot(Q.FB.dly, 9, do.lines=F)
lag.plot(Q.NC.dly, 9, do.lines=F)
lag.plot(Q.LR.dly, 9, do.lines=F)
lag.plot(Q.MC.dly, 9, do.lines=F)
lag.plot(Precip.CH.dly, 9, do.lines=F)
lag.plot(Precip.SF.dly, 9, do.lines=F)
lag.plot(Tmean.CH.dly, 9, do.lines=F)
lag.plot(Tmean.SF.dly, 9, do.lines=F)

par(mfrow=c(2,4))
acf(Q.FB.dly, 20)
acf(Q.NC.dly, 20)
acf(Q.LR.dly, 20)
acf(Q.MC.dly, 20)
acf(Precip.CH.dly, 20) 
acf(Precip.SF.dly, 20)
acf(Tmean.CH.dly, 20)
acf(Tmean.SF.dly, 20)

par(mfrow=c(2,4))
pacf(Q.FB.dly, 20)
pacf(Q.NC.dly, 20)
pacf(Q.LR.dly, 20)
pacf(Q.MC.dly, 20)
pacf(Precip.CH.dly, 20) 
pacf(Precip.SF.dly, 20)
pacf(Tmean.CH.dly, 20)
pacf(Tmean.SF.dly, 20)

QMC.S1h <- read.csv(file=paste(root_dir,"Storms/S1MChQ.csv",sep=""))
#QMC.S1h <- QMC.S1h[4]
QMC.S1h <- QMC.S1h[1:336,4]
QMC.S1h <- as.ts(QMC.S1h, start=1, end=336, frequency=24)

QLR.S1h <- read.csv(file=paste(root_dir,"Storms/S1LRhQ.csv",sep=""),header=T)
QLR.S1h <- QLR.S1h[1:337,4]
QLR.S1h <- as.ts(QLR.S1h, start=1, end=337, frequency=24)

QNC.S1h <- read.csv(file=paste(root_dir,"Storms/S1NChQ.csv",sep=""),header=T)
QNC.S1h <- QNC.S1h[1:337,4]
QNC.S1h <- as.ts(QNC.S1h, start=1, end=337, frequency=24)

QFB.S1h <- read.csv(file=paste(root_dir,"Storms/S1FBhQ.csv",sep=""),header=T)
QFB.S1h <- QFB.S1h[1:310,4]
QFB.S1h <- as.ts(QFB.S1h, start=1, end=310, frequency=24)

par(mfrow=c(2,2))
pacf(QMC.S1h)
pacf(QLR.S1h)
pacf(QNC.S1h)
pacf(QFB.S1h)

dQMC.S1h <- diff(QMC.S1h)
par(mfrow=c(2,2))
plot(QMC.S1h)
plot(dQMC.S1h)
NAs <- which(QMC.S1h==NA)
NAs
acf(QMC.S1h)


pacf(dQMC.S1h)
QMCS1 <- QMC.S1h[,4]
colnames(QMC.S1h) <- c("hour","Q")
dQMCS1 <- diff(QMCS1)
dQMCS1[1:10]
plot(dQMCS1)
acf(dQMCS1)
QMC.S1h$hour <- paste(QMC.S1h$hour,":00",sep="")
QMC.S1h[,1] <- paste(QMC.S1h$date,QMC.S1h$hour,sep=" ")
QMC.S1h <- QMC.S1h[,-2]
QMC.S1h[1:70,]
QMC.S1h[,1] <- as.POSIXct(QMC.S1h[,1],format="%Y%m%d %h:%M")

v <- QMC.S1h$x 
z <- zoo(v, as.chron(d, format = "%Y%m%d") + h / 2400) 