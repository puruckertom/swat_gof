library(hydroGOF)
library(hydroTSM)
library(RODBC)

obs_dir <- "c://dropbox/ktp/SpectralFlow/"
ncdc_dir <- "c://dropbox/ktp/SpectralFlow/best_runs_22711/mc_ncdc_valid/"
mpe_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_mpe_valid/"
output_dir <- "c://Dropbox/ktp/SpectralFlow/gwrc_figure/"

obsflow <- read.csv(paste(obs_dir,"2002_2010_julian.csv",sep=""),header=TRUE)
access.root <- "SWATOutput"
watershed = 4
dotmdb <- ".mdb"

# open access connection
ncdc.connection.name <- paste(ncdc_dir,access.root,dotmdb,sep="")
mpe.connection.name <- paste(mpe_dir,access.root,dotmdb,sep="")
ncdc.channel <- odbcConnectAccess(ncdc.connection.name)
mpe.channel <- odbcConnectAccess(mpe.connection.name)
ncdc.modeledflow <- sqlQuery(ncdc.channel , paste("SELECT * FROM rch WHERE sub=",watershed,sep=""))
mpe.modeledflow <- sqlQuery(mpe.channel , paste("SELECT * FROM rch WHERE sub=",watershed,sep=""))
odbcCloseAll()

timesteps <- dim(ncdc.modeledflow)[[1]]
timesteps2 <- dim(mpe.modeledflow)[[1]]
timecounter <- 1:timesteps
#truncate obsflow
obsflow <- obsflow[1:timesteps,]
startdate <- "2002-01-01"

ncdc.compareflow <- as.data.frame(cbind(timecounter,obsflow$year,ncdc.modeledflow$YEAR,obsflow$day,ncdc.modeledflow$MON,obsflow$flow,ncdc.modeledflow$FLOW_OUTcms))
mpe.compareflow <- as.data.frame(cbind(timecounter,obsflow$year,mpe.modeledflow$YEAR,obsflow$day,mpe.modeledflow$MON,obsflow$flow,mpe.modeledflow$FLOW_OUTcms))
#class(compareflow)
#dim(compareflow)
dimnames(ncdc.compareflow)[[2]] <- c("timecounter","obsyear","modeledyear","obsday","modeledmonth","obsflow","modeledflow")
dimnames(mpe.compareflow)[[2]] <- c("timecounter","obsyear","modeledyear","obsday","modeledmonth","obsflow","modeledflow")

ncdc.compareflow$modeledyear <- NULL
ncdc.compareflow$modeledmonth <- NULL
mpe.compareflow$modeledyear <- NULL
mpe.compareflow$modeledmonth <- NULL

dimnames(ncdc.compareflow)[[2]] <- c("timecounter","year","day","obsflow","modeledflow")
dimnames(mpe.compareflow)[[2]] <- c("timecounter","year","day","obsflow","modeledflow")

obsflow.ts <- ts(abs(jitter(ncdc.compareflow$obsflow)),frequency=365,start=c(2002,1))
ncdc.modeledflow.ts <- ts(abs(jitter(ncdc.compareflow$modeledflow)),frequency=365,start=c(2002,1))
mpe.modeledflow.ts <- ts(abs(jitter(mpe.compareflow$modeledflow)),frequency=365,start=c(2002,1))

pacfobs.filename <- paste(output_dir,"pacf_obs_",access.root,".pdf",sep="")
pdf(file=pacfobs.filename,width=10.5, height=4, bg="white")
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
  par(omi=c(0.1,0.1,0.01,0.1))
  pacf(mpe.modeledflow.ts,main="",ylim=c(-0.04,0.25),cex=1.3,cex.lab=1.3)
  text(0.05,0.24,"MPE",cex=2.1)
  pacf(obsflow.ts,ylab="",main="",ylim=c(-0.04,0.25),cex=1.3)
  text(0.05,0.24,"Observed",cex=2.1)
  pacf(ncdc.modeledflow.ts,ylab="",main="",ylim=c(-0.04,0.25),cex=1.3)
  text(0.05,0.24,"NCDC",cex=2.1)
dev.off()

stl.obs.filename <- paste(output_dir,counter,"stl_obs_",access.root,".pdf",sep="")
pdf(file=stl.obs.filename,width=10.5, height=8.5, bg="white")
  #layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 4, 3, byrow = TRUE))
  op <- par(mar = c(0, 4, 0, 3), oma = c(5, 0, 4, 0), mfcol = c(4, 3))
  #par(mfrow=c(4,3),omi=c(0.1,0.1,0.01,0.1))
  plot(stl(mpe.modeledflow.ts,"periodic"))
  plot(stl(obsflow.ts,"periodic"))
  plot(stl(ncdc.modeledflow.ts,"periodic"))
dev.off()
