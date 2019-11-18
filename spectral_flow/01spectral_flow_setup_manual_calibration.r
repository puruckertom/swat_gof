library(hydroGOF)
#hydroGOF
#http://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

library(hydroTSM)
#hydroTSM
#http://cran.r-project.org/web/packages/hydroTSM/hydroTSM.pdf

#  the flow values are the "FLOW_OUTcms" column in the "rch" table of the Access database
library(RODBC)

#dotmdb may not be necessary
dotmdb <- ".mdb"

# open access connection
swat.connection.name <- paste(sf_dir,access.root,dotmdb,sep="")
swat.channel <- odbcConnectAccess(swat.connection.name)
modeledflow <- sqlQuery(swat.channel , paste("SELECT * FROM rch WHERE sub=",watershed,sep=""))
odbcCloseAll()

#modeled
print("modeled")
dim(modeledflow)
summary(modeledflow)
colnames(modeledflow)

#observedflow
print("observed")
dim(obsflow)
summary(obsflow)
colnames(obsflow)

#times may disagree
timesteps <- dim(modeledflow)[[1]]
timecounter <- 1:timesteps
#truncate obsflow
obsflow <- obsflow[1:timesteps,]
startdate <- "2002-01-01"

compareflow <- as.data.frame(cbind(timecounter,obsflow$year,modeledflow$YEAR,obsflow$day,modeledflow$MON,obsflow$flow,modeledflow$FLOW_OUTcms))
class(compareflow)
dim(compareflow)
dimnames(compareflow)[[2]] <- c("timecounter","obsyear","modeledyear","obsday","modeledmonth","obsflow","modeledflow")
compareflow

compareflow$modeledyear <- NULL
compareflow$modeledmonth <- NULL

dimnames(compareflow)[[2]] <- c("timecounter","year","day","obsflow","modeledflow")

#drop leap year Dec 31s
#dim(spectralflow[spectralflow$day!=366,])
#spectralflow <- spectralflow[spectralflow$day!=366,]
#spectralflow$timecounter <- 1:2920
#dimnames(spectralflow)[[1]] <- 1:2920
#class(spectralflow)
#dimnames(spectralflow)[[2]]

#create time series objects
#2920/365

obsflow.ts <- ts(compareflow$obsflow,frequency=365,start=c(2002,1))
modeledflow.ts <- ts(compareflow$modeledflow,frequency=365,start=c(2002,1))


x.Dates <- as.Date(startdate) + 1:timesteps - 1
obsflow.zoo <- as.zoo(zoo(compareflow$obsflow,x.Dates))
class(obsflow.zoo)
obsflow.log.zoo <- as.zoo(zoo(log10(abs(jitter(compareflow$obsflow))),x.Dates))
modeledflow.zoo <- as.zoo(zoo(compareflow$modeledflow,x.Dates))
modeledflow.log.zoo <- as.zoo(zoo(log10(abs(jitter(compareflow$modeledflow))),x.Dates))

summary(modeledflow.log.zoo)

# plot the time series - hydrograph
ts.filename <- paste(output_dir,counter,"hydrograph_103",access.root,".pdf",sep="")
pdf(file=ts.filename,width=10.5, height=5.5, bg="white")
  par(mfrow=c(1,1))
  plot(log10(obsflow.ts),type="l",ylab="log10(Flow(cms))",xlab="Time")
  lines(log10(modeledflow.ts),col="blue")
  title(paste("Hydrograph- ",somethingdescriptive," (",access.root,")",sep=""))
  legend(x="bottomright",c("Observed","Modeled"),lty=c(1,1),col=c("black","blue"),bty="n")
dev.off()

gof(sim=modeledflow.log.zoo,obs=obsflow.log.zoo)

# graphical goodness of fit
summary(log10(modeledflow.zoo))
summary(log10(obsflow.zoo))
summary(log10(abs(jitter(obsflow.zoo))))
log10(obsflow.zoo)
summary(obsflow.zoo)

#log graphical goodness of fit
ggof.log.filename <- paste(output_dir,counter,"ggof_log_zoo_",access.root,".pdf",sep="")
pdf(file=ggof.log.filename,width=10.5, height=8.5, bg="white")
  ggof(sim=modeledflow.log.zoo,obs=obsflow.log.zoo, ftype="dm", FUN=mean)
dev.off()

#graphical goodness of fit
ggof.filename <- paste(output_dir,counter,"ggof_zoo_",access.root,".pdf",sep="")
pdf(file=ggof.filename,width=10.5, height=8.5, bg="white")
  ggof(sim=modeledflow.zoo,obs=obsflow.zoo, ftype="dm", FUN=mean)
dev.off()

# plot the pdfs
pdfs.filename <- paste(output_dir,counter,"hist_pdfs_",access.root,".pdf",sep="")
pdf(file=pdfs.filename,width=10.5, height=8.5, bg="white")
  par(mfrow=c(1,2))
  hist(log10(obsflow.ts),xlim=c(-4,1.6),col="darkgrey",breaks=20,xlab="log10(Flow(cms))",main=paste("Observed: Mean=",round(mean(obsflow.ts),digits=2),"; Stand Dev=",round(sd(obsflow.ts),digits=2)))
  lines(density(log10(obsflow.ts),width=1,n=500))
  modeled.gof <- gof(sim=modeledflow.ts,obs=obsflow.ts)
  gof.mat <- matrix(data = NA, nrow = 18, ncol = 1)
  gof.mat[,1] <- paste(rownames(modeled.gof), "=",modeled.gof)
  modeled.sub.gof.mat <- gof.mat[c(1,2,4,17,18,9,10,13),1]
  modeled.eff <- 1 - sum((obsflow.ts - mpeflow.ts)^2) / sum((obsflow.ts-mean(obsflow.ts))^2)
  hist(log10(modeledflow.ts),xlim=c(-4,1.6),col="blue",breaks=20,xlab="log10(Flow(cms))",main=paste("Modeled: Mean=",round(mean(modeledflow.ts),digits=2),"; Stand Dev=",round(sd(modeledflow.ts),digits=2)))
  legend(x="topleft",modeled.sub.gof.mat)
dev.off()

title.text <- paste(somethingdescriptive," (",access.root,")",sep="")

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
per.filename <- paste(output_dir,counter,"periodograms_",access.root,".pdf",sep="")
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
per.filename <- paste(output_dir,counter,"periodograms_month_",access.root,".pdf",sep="")
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
per.filename <- paste(output_dir,counter,"periodograms_week_",access.root,".pdf",sep="")
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
per.filename <- paste(output_dir,counter,"periodograms_shorterweek_",access.root,".pdf",sep="")
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

phase.filename <- paste(output_dir,counter,"phase_portrait_",access.root,".pdf",sep="")
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