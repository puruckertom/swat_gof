#Validations only, 5/95 only?
sessionInfo()

library(zoo)
library(lattice)
library(hydroGOF)

if(.Platform$OS.type=="windows"){source_dir <- "D:/Dropbox/ktp/swat_gof/"}
if(.Platform$OS.type=="unix"){source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")}
if(.Platform$OS.type=="windows"){output_dir <- "D:/Dropbox/ktp/PPTCompPaper/Figures/"}
if(.Platform$OS.type=="unix"){output_dir <- path.expand("~/Dropbox/ktp/PPTCompPaper/Figures/")}

#assign dates
startdate <- "2008-01-01"
enddate <- "2010-12-31"
Ndays <- (as.numeric(as.Date(enddate))-as.numeric(as.Date(startdate)))+1
Nmonths <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "month"))
boolDays=TRUE

#set x.Dates as monthly or daily
if(boolDays==TRUE){
  x.Dates <- as.Date(startdate) + 1:Ndays -1
}else{
  x.Dates <- seq(as.Date(startdate), by="month", length.out=Nmonths)
}

#############################################################
#Katie change these as necessary for different sets of runs
Npasses = 8 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
#graphical output and tables will be generated in this directory
sim.dir[1] <- "mcCL_NCDCV/"
sim.dir[2] <- "mcCL_MPEV/"
sim.dir[3] <- "lrCL_NCDCV/"
sim.dir[4] <- "lrCL_MPEV/"
sim.dir[5] <- "ncCL_NCDCV/"
sim.dir[6] <- "ncCL_MPEV/"
sim.dir[7] <- "fbCL_NCDCV/"
sim.dir[8] <- "fbCL_MPEV/"
###############################################################

sim.label <- vector(mode="character",length=Npasses)
sim.label[1] <- "mcNCDCV"
sim.label[2] <- "mcMPEV"
sim.label[3] <- "lrNCDCV"
sim.label[4] <- "lrMPEV"
sim.label[5] <- "ncNCDCV"
sim.label[6] <- "ncMPEV"
sim.label[7] <- "fbNCDCV"
sim.label[8] <- "fbMPEV"

#graphics directory
graphics.dir <- output_dir

#check to see that they all exist
for(i in 1:Npasses){
  print(file.exists(paste(source_dir,sim.dir[i],sep="")))
}

##############################################
#R figure for observations versus fdc percentiles
#assign dates
#calib.startdate <- "2002-01-01"
#calib.enddate <- "2007-12-31"
#calib.Ndays <- (as.numeric(as.Date(calib.enddate))-as.numeric(as.Date(calib.startdate)))+1
#calib.Dates <- as.Date(calib.startdate) + 1:Ndays -1

valid.startdate <- "2008-01-01"
valid.enddate <- "2010-12-31"
valid.Ndays <- (as.numeric(as.Date(valid.enddate))-as.numeric(as.Date(valid.startdate)))+1
valid.Dates <- as.Date(valid.startdate) + 1:Ndays -1

#mountain creek observed flow
valid.mc.flow.observed <- read.table(paste(source_dir,"mcCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.mc.flow.observed <- as.vector(t(valid.mc.flow.observed[,3]))
summary(valid.mc.flow.observed)
#little river observed flow
valid.lr.flow.observed <- read.table(paste(source_dir,"lrCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.lr.flow.observed <- as.vector(t(valid.lr.flow.observed[,3]))
summary(valid.lr.flow.observed)
#neuse clayton observed flow
valid.nc.flow.observed <- read.table(paste(source_dir,"ncCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.nc.flow.observed <- as.vector(t(valid.nc.flow.observed[,3]))
summary(valid.nc.flow.observed)
#fort barnwell observed flow
valid.fb.flow.observed <- read.table(paste(source_dir,"fbCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.fb.flow.observed <- as.vector(t(valid.fb.flow.observed[,3]))
summary(valid.fb.flow.observed)

#observed percentiles
valid.lr.obs.95 <- quantile(valid.lr.flow.observed,0.95)
valid.lr.obs.50 <- quantile(valid.lr.flow.observed,0.5)
valid.lr.obs.5 <- quantile(valid.lr.flow.observed,0.05)
valid.mc.obs.95 <- quantile(valid.mc.flow.observed,0.95)
valid.mc.obs.50 <- quantile(valid.mc.flow.observed,0.5)
valid.mc.obs.5 <- quantile(valid.mc.flow.observed,0.05)
valid.nc.obs.95 <- quantile(valid.nc.flow.observed,0.95)
valid.nc.obs.50 <- quantile(valid.nc.flow.observed,0.5)
valid.nc.obs.5 <- quantile(valid.nc.flow.observed,0.05)
valid.fb.obs.95 <- quantile(valid.fb.flow.observed,0.95)
valid.fb.obs.50 <- quantile(valid.fb.flow.observed,0.5)
valid.fb.obs.5 <- quantile(valid.fb.flow.observed,0.05)

####################
#simulated flows and percentiles
sim.5th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
sim.50th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
sim.95th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)

#check to see that they exist
for(i in 1:Npasses){
  print(file.exists(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep="")))
}


for(i in 1:8){
  print(file.exists(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep="")))
  print(dim(read.csv(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep=""),header=TRUE)))
  sim.temp.percentiles <- read.csv(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep=""),header=TRUE)
  dim(sim.temp.percentiles)
  for(j in 2:2002){
    sim.5th.percentiles[j-1,i] <- quantile(sim.temp.percentiles[,j],0.05)
    sim.50th.percentiles[j-1,i] <- quantile(sim.temp.percentiles[,j],0.5)
    sim.95th.percentiles[j-1,i] <- quantile(sim.temp.percentiles[,j],0.95)
  }
}
colnames(sim.5th.percentiles) <- sim.label
dim(sim.5th.percentiles)
summary(sim.5th.percentiles)
colnames(sim.50th.percentiles) <- sim.label
dim(sim.50th.percentiles)
summary(sim.50th.percentiles)
colnames(sim.95th.percentiles) <- sim.label
dim(sim.95th.percentiles)
summary(sim.95th.percentiles)


pdf(paste(output_dir,"flowpercentiles.pdf",sep=""),height=6.25,width=5,colormodel="cmyk")
  par(mfrow=c(3,4),mai=c(0.1,0.15,0.1,0.1),mgp=c(2.25,1,0),oma=c(2,2,2,1))
#1 5th percentile MC
  boxplot(cbind(sim.5th.percentiles[,1],sim.5th.percentiles[,2]), ylim=c(0.00,0.004), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.5,ymax.5),
    title("MC", cex.main=1)
    abline(h=valid.mc.obs.5, lwd=1,lend=1,lty=3)
    axis(2,at=c(0.000, 0.001, 0.002, 0.003, 0.004),labels=c(0.000, 0.001, 0.002, 0.003, 0.004),cex.axis=0.85, padj=1)
    mtext("5th percentile flow (cms)", side=2, padj=-3, cex=0.6)
#2 5th percentile LR
  boxplot(cbind(sim.5th.percentiles[,3],sim.5th.percentiles[,4]), ylim=c(0.00,0.04), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.5,ymax.5),
    title("LR", cex.main=1)
    abline(h=valid.lr.obs.5,lwd=1,lend=1,lty=3)
    axis(2,at=c(0.00, 0.01, 0.02, 0.03, 0.04),labels=c(0.00, 0.01, 0.02, 0.03, 0.04),cex.axis=0.85, padj=1)
#3 5th percentile NC
  boxplot(cbind(sim.5th.percentiles[,5],sim.5th.percentiles[,6]),ylim=c(0,8), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.5,ymax.5),
    title("NC", cex.main=1)
    abline(h=valid.nc.obs.5,lwd=1,lend=1,lty=3)
    axis(2,at=c(0, 2, 4, 6, 8),labels=c(0, 2, 4, 6, 8),cex.axis=0.85, padj=1)
#4 5th percentile FB
  boxplot(cbind(sim.5th.percentiles[,7],sim.5th.percentiles[,8]),ylim=c(0,25), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.5,ymax.5),
    title("FB",cex.main=1)
    abline(h=valid.fb.obs.5, lwd=1,lend=1,lty=3)
    axis(2,at=c(0, 5, 10, 15, 20, 25),labels=c(0, 5, 10, 15, 20, 25),cex.axis=0.85, padj=1)
#5 50th percentiles MC
	boxplot(cbind(sim.50th.percentiles[,1],sim.50th.percentiles[,2]), ylim=c(0,0.08), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
		abline(h=valid.mc.obs.50, lwd=1,lend=1,lty=3)
		axis(2,at=c(0,0.02,0.04,0.06,0.08),labels=c(0,0.02,0.04,0.06,0.08),cex.axis=0.85,padj=1)
		mtext("50th percentile flow (cms)", side=2, padj=-3, cex=0.6) 
		#axis(side=1,at=c(1,2),labels=c("GAUGE","RADAR"), cex.axis=0.9, padj=-1)
#6 50th percentiles LR
	boxplot(cbind(sim.50th.percentiles[,3],sim.50th.percentiles[,4]), ylim=c(0,0.8), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
		abline(h=valid.lr.obs.50, ,lwd=1,lend=1,lty=3)
		axis(2,at=c(0,0.2,0.4,0.6,0.8),labels=c(0,0.2,0.4,0.6,0.8),cex.axis=0.85,padj=1)
		#axis(side=1,at=c(1,2),labels=c("GAUGE","RADAR"), cex.axis=0.9, padj=-1)
#7 50th percentiles NC
	boxplot(cbind(sim.50th.percentiles[,5],sim.50th.percentiles[,6]), ylim=c(0,30), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
		abline(h=valid.nc.obs.50,lwd=1,lend=1,lty=3)
		axis(2,at=c(0,10,20,30),labels=c(0,10,20,30),cex.axis=0.85,padj=1)
		#axis(side=1,at=c(1,2),labels=c("GAUGE","RADAR"), cex.axis=0.9, padj=-1)
#8 50th percentiles FB
	boxplot(cbind(sim.50th.percentiles[,7],sim.50th.percentiles[,8]), ylim=c(0,100), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
		abline(h=valid.fb.obs.50, lwd=1,lend=1,lty=3)
		axis(2,at=c(0,25,50,75,100),labels=c(0,25,50,75,100),cex.axis=0.85,padj=1)
		#axis(side=1,at=c(1,2),labels=c("GAUGE","RADAR"), cex.axis=0.9, padj=-1)
#9 95th percentiles MC
 boxplot(cbind(sim.95th.percentiles[,1],sim.95th.percentiles[,2]), ylim=c(0,1.5), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
   abline(h=valid.mc.obs.95, lwd=1,lend=1,lty=3)
   axis(2,at=c(0,0.5,1,1.5),labels=c(0,0.5,1,1.5),cex.axis=0.85,padj=1)
   mtext("95th percentile flow (cms)", side=2, padj=-3, cex=0.6) 
   axis(side=1,at=c(1,2),labels=c("G","R"), cex.axis=0.9, padj=-1)
 #10 95th percentiles LR
  boxplot(cbind(sim.95th.percentiles[,3],sim.95th.percentiles[,4]), ylim=c(0,15), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
    abline(h=valid.lr.obs.95, ,lwd=1,lend=1,lty=3)
    axis(2,at=c(0,5,10,15),labels=c(0,5,10,15),cex.axis=0.85,padj=1)
    axis(side=1,at=c(1,2),labels=c("G","R"), cex.axis=0.9, padj=-1)
 #11 95th percentiles NC
 boxplot(cbind(sim.95th.percentiles[,5],sim.95th.percentiles[,6]), ylim=c(0,150), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
    abline(h=valid.nc.obs.95,lwd=1,lend=1,lty=3)
    axis(2,at=c(0,50,100,150),labels=c(0,50,100,150),cex.axis=0.85,padj=1)
    axis(side=1,at=c(1,2),labels=c("G","R"), cex.axis=0.9, padj=-1)
 #12 95th percentiles FB
boxplot(cbind(sim.95th.percentiles[,7],sim.95th.percentiles[,8]), ylim=c(0,450), ylab=NA, yaxt="n", xaxt="n",col=c("orangered3","darkcyan"), boxwex=0.6, range=0) #ylim=c(ymin.95,ymax.95),
    abline(h=valid.fb.obs.95, lwd=1,lend=1,lty=3)
    axis(2,at=c(0,150,300,450),labels=c(0,150,300,450),cex.axis=0.85,padj=1)
    axis(side=1,at=c(1,2),labels=c("G","R"), cex.axis=0.9, padj=-1)
dev.off()

