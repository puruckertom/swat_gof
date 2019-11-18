#sf_dir <- "c://temp/"
sf_dir <- "l://Public/purucker/SpectralFlow/"

library(hydroGOF)
#hydroGOF
#http://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

library(hydroTSM)
#hydroTSM
#http://cran.r-project.org/web/packages/hydroTSM/hydroTSM.pdf

# version

#  the flow values are the "FLOW_OUTcms" column in the "rch" table of the Access database
library(RODBC)
access.root <- "NCDC_18_uncalib"
dotmdb <- ".mdb"
swat.connection.name <- paste(sf_dir,access.root,dotmdb,sep="")
swat.channel <- odbcConnectAccess(connection.name)
ncdcflow <- sqlQuery(swat.channel , paste("select * from rch"))
dim(ncdcflow)
summary(ncdcflow)
colnames(ncdcflow)



obsflow <- read.csv(paste(sf_dir,"2002_2010_julian.csv",sep=""),header=TRUE)
dim(obsflow)
mpeflow <- read.csv(paste(sf_dir,"mpe605.csv",sep=""),header=TRUE)
colnames(mpeflow)
dim(mpeflow)
#ncdcflow <- read.csv(paste(sf_dir,"ncdc602.csv",sep=""),header=TRUE)
#dim(ncdcflow)

mpeflow[,2:3]

# jan 1, 2002 to 325 days deep into 2010
dim(obsflow)
#[1] 3247    3
summary(obsflow)
dimnames(obsflow)[[2]]
obsflow <- obsflow[1:2922,]
#what time series did Katie feed SWAT?  which day did she drop?

# jan 1, 2002 to Dec 31, 2006
dim(mpeflow)
#[1] 2922   46
summary(mpeflow)
#dimnames(mpeflow)[[2]]
# jan 1, 2002 to Dec 31, 2009
dim(ncdcflow)
#[1] 2922   46
summary(ncdcflow)
dimnames(ncdcflow)[[2]]

timecounter <- 1:2922

spectralflow <- as.data.frame(cbind(timecounter,obsflow$year,mpeflow$YEAR,ncdcflow$YEAR,obsflow$day,mpeflow$MON,ncdcflow$MON,obsflow$flow,mpeflow$FLOW_OUTcms,ncdcflow$FLOW_OUTcms))
class(spectralflow)
dim(spectralflow)
dimnames(spectralflow)[[2]] <- c("timecounter","year","year2","year3","day","day2","day3","obsflow","mpeflow","ncdcflow")
spectralflow$year2 <- NULL
spectralflow$year3 <- NULL
spectralflow$day2 <- NULL
spectralflow$day3 <- NULL
dimnames(spectralflow)[[2]]

#drop leap year Dec 31s
#dim(spectralflow[spectralflow$day!=366,])
#spectralflow <- spectralflow[spectralflow$day!=366,]
#spectralflow$timecounter <- 1:2920
#dimnames(spectralflow)[[1]] <- 1:2920
#class(spectralflow)
#dimnames(spectralflow)[[2]]

#create time series objects
#2920/365

obsflow.ts <- ts(spectralflow$obsflow,frequency=365,start=c(2002,1))
mpeflow.ts <- ts(spectralflow$mpeflow,frequency=365,start=c(2002,1))
ncdcflow.ts <- ts(spectralflow$ncdcflow,frequency=365,start=c(2002,1))

x.Dates <- as.Date("2002-01-01") + 1:2922 - 1
obsflow.zoo <- as.zoo(zoo(spectralflow$obsflow,frequency=365))
class(obsflow.zoo)
obsflow.log.zoo <- as.zoo(zoo(log10(abs(jitter(spectralflow$obsflow))),x.Dates))
mpeflow.zoo <- as.zoo(zoo(spectralflow$mpeflow,frequency=365))
mpeflow.log.zoo <- as.zoo(zoo(log10(spectralflow$mpeflow),frequency=365))
ncdcflow.zoo <- as.zoo(zoo(spectralflow$ncdcflow,frequency=365))
ncdcflow.log.zoo <- as.zoo(zoo(log10(spectralflow$ncdcflow),frequency=365))

# plot the time series
ts.filename <- paste(sf_dir,"ts_",access.root,".pdf",sep="")
pdf(file=ts.filename,width=10.5, height=5.5, bg="white")
  plot(log10(obsflow.ts),type="l",ylab="log10(Flow(cms))",xlab="Time")
  lines(log10(mpeflow.ts),col="blue")
  lines(log10(ncdcflow.ts),col="red")
  title(paste("Mountain Creek (",expression(km^2),")",sep=""))
  legend(2009,-2.5,c("Observed","MPE","NCDC"),lty=c(1,1,1),col=c("black","blue","red"),bty="n")
dev.off()

gof(sim=mpeflow.log.zoo,obs=obsflow.log.zoo)
gof(sim=ncdcflow.log.zoo,obs=obsflow.log.zoo)

# graphical goodness of fit
summary(log10(mpeflow.zoo))
summary(log10(obsflow.zoo))
summary(log10(abs(jitter(obsflow.zoo))))
log10(obsflow.zoo)
summary(obsflow.zoo)

#par(mfrow=c(1,1))
ggof1.filename <- paste(sf_dir,"ggof1_zoo_",access.root,".pdf",sep="")
pdf(file=ggof1.filename,width=8.5, height=10.5, bg="white")
  ggof(sim=mpeflow.log.zoo,obs=obsflow.log.zoo, ftype="dm", FUN=mean)
dev.off()

ggof2.filename <- paste(sf_dir,"ggof2_zoo_",access.root,".pdf",sep="")
pdf(file=ggof2.filename,width=8.5, height=10.5, bg="white")
  ggof(sim=ncdcflow.log.zoo,obs=obsflow.log.zoo, ftype="dm", FUN=mean)
dev.off()


# plot the pdfs
par(mfrow=c(1,1))
pdfs.filename <- paste(sf_dir,"hist_pdfs_",access.root,".pdf",sep="")
pdf(file=pdfs.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(3,1))
  hist(log10(obsflow.ts),xlim=c(-4,1.6),col="darkgrey",breaks=20,xlab="log10(Flow(cms))",main="Mountain Creek Observed")
  mpe.gof <- gof(sim=mpeflow.ts,obs=obsflow.ts)
  gof.mat <- matrix(data = NA, nrow = 18, ncol = 1)
  gof.mat[,1] <- paste(rownames(mpe.gof), "=",mpe.gof)
  mpe.sub.gof.mat <- gof.mat[c(2,9,17),1]
  mpe.eff <- 1 - sum((obsflow.ts - mpeflow.ts)^2) / sum((obsflow.ts-mean(obsflow.ts))^2)
  hist(log10(mpeflow.ts),xlim=c(-4,1.6),col="blue",breaks=20,xlab="log10(Flow(cms))",main=paste("MPE N-S efficiency =",round(mpe.eff,digits=2)))
  legend(0.5,600,mpe.sub.gof.mat)
  ncdc.gof <- gof(sim=ncdcflow.ts,obs=obsflow.ts)
  gof.mat <- matrix(data = NA, nrow = 18, ncol = 1)
  gof.mat[,1] <- paste(rownames(ncdc.gof), "=",ncdc.gof)
  ncdc.sub.gof.mat <- gof.mat[c(2,9,17),1]
  ncdc.eff <- 1 - sum((obsflow.ts - ncdcflow.ts)^2) / sum((obsflow.ts-mean(obsflow.ts))^2)
  hist(log10(ncdcflow.ts),xlim=c(-4,1.6),col="red",breaks=20,xlab="log10(Flow(cms))",main=paste("NCDC N-S efficiency =",round(ncdc.eff,digits=2)))
  legend(0.5,600,ncdc.sub.gof.mat)
dev.off()

# power spectrum - fast fourier transforms
Nsamps <- length(ncdcflow.ts)
k = (2*pi*(1:((Nsamps/2) - 1)))/Nsamps
obs.Pk = abs(fft(obsflow.ts[1:(Nsamps/2) - 1], inverse = TRUE))^2
mpe.Pk = abs(fft(mpeflow.ts[1:(Nsamps/2) - 1], inverse = TRUE))^2
ncdc.Pk = abs(fft(ncdcflow.ts[1:(Nsamps/2) - 1], inverse = TRUE))^2
par(mfrow=c(3,1))
plot(k, obs.Pk, "l", log="xy")
plot(k, mpe.Pk, "l", log="xy",col="blue")
plot(k, ncdc.Pk, "l", log="xy",col="red")

# periodograms
per.filename <- paste(sf_dir,"periodograms_",access.root,".pdf",sep="")
pdf(file=per.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(2,1))
  spec.pgram(mpeflow.ts,ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(mpeflow.ts,col="blue",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,col="red",ylim=c(1e-7,1e-1),add=TRUE)
dev.off()

# periodograms shortened to a month
per.filename <- paste(sf_dir,"periodograms_month_",access.root,".pdf",sep="")
pdf(file=per.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(2,1))
  spec.pgram(mpeflow.ts,xlim=c(0,30),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,30),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(mpeflow.ts,xlim=c(0,30),col="blue",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,30),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,30),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,30),col="red",ylim=c(1e-7,1e-1),add=TRUE)
dev.off()

# periodograms shortened to a week
per.filename <- paste(sf_dir,"periodograms_week_",access.root,".pdf",sep="")
pdf(file=per.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(2,1))
  spec.pgram(mpeflow.ts,xlim=c(0,7),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,7),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(mpeflow.ts,xlim=c(0,7),col="blue",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,7),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,7),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,7),col="red",ylim=c(1e-7,1e-1),add=TRUE)
dev.off()

# looks like each day is in units of 1/8 what is displayed on the x-axis
# periodograms shortened to a week
per.filename <- paste(sf_dir,"periodograms_shorterweek_",access.root,".pdf",sep="")
pdf(file=per.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(2,1))
  spec.pgram(mpeflow.ts,xlim=c(0,1),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,1),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(mpeflow.ts,xlim=c(0,1),col="blue",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,1),ylim=c(1e-7,1e-1))
  spec.pgram(obsflow.ts,xlim=c(0,1),col="darkgray",ylim=c(1e-7,1e-1),add=TRUE)
  spec.pgram(ncdcflow.ts,xlim=c(0,1),col="red",ylim=c(1e-7,1e-1),add=TRUE)
dev.off()

spectralflow.matrix <- as.matrix(cbind(spectralflow$obsflow,spectralflow$mpeflow,spectralflow$ncdcflow))
spectralflow.ts <- ts(spectralflow.matrix, start=c(2002, 1), frequency=365)
plot(spectralflow.ts,plot.type="multiple",col=4,main="NC subbasin Time Series")

mpediff <- spectralflow$mpeflow-spectralflow$obsflow
ncdcdiff <- spectralflow$ncdcflow-spectralflow$obsflow
plot(log(mpediff^2),col="blue",type="l")
lines(log(ncdcdiff^2),col="red")

mpe.squared <- mpediff^2
ncdc.squared <- ncdcdiff^2

length(mpe.squared)
length(ncdc.squared)
#plot(spectralflow.ts,lag(spectralflow.ts,1),plot.type="multiple",col=4,main="NC subbasin Time Series")

phase.filename <- paste(sf_dir,"phase_portrait_",access.root,".pdf",sep="")
pdf(file=phase.filename,width=8.5, height=10.5, bg="white")
  par(mfrow=c(3,1))
  plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],1),xlim=c(0,22), xaxs="i", ylim=c(0,15), yaxs="i",pch=19, las =1, 
  xlab="USGS observed streamflow",ylab="", col=1)
  plot(spectralflow.ts[,2],lag(spectralflow.ts[,1],2),xlim=c(0,22), xaxs="i", ylim=c(0,15), yaxs="i",pch=19, las =1, 
  xlab="MPE simulated streamflow",ylab="", col=1)
  plot(spectralflow.ts[,3],lag(spectralflow.ts[,1],3),xlim=c(0,22), xaxs="i", ylim=c(0,15), yaxs="i",pch=19, las =1, 
  xlab="NCDC simulated streamflow",ylab="", col=1)
dev.off()

par(mfrow=c(3,1))
plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],2),col=1)
plot(spectralflow.ts[,2],lag(spectralflow.ts[,2],2),col=2)
plot(spectralflow.ts[,3],lag(spectralflow.ts[,3],2),col=3)

par(mfrow=c(3,1))
plot(spectralflow.ts[,1],lag(spectralflow.ts[,1],3),col=1)
plot(spectralflow.ts[,2],lag(spectralflow.ts[,2],3),col=2)
plot(spectralflow.ts[,3],lag(spectralflow.ts[,3],3),col=3)