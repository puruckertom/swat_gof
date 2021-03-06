sessionInfo()

library(zoo)
library(lattice)
library(hydroGOF)

if(.Platform$OS.type=="windows"){source_dir <- "c:/dropbox/"}
if(.Platform$OS.type=="unix"){source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")}

#assign dates
startdate <- "2002-01-01"
enddate <- "2007-12-31"
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
Npasses = 16 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
#graphical output and tables will be generated in this directory
sim.dir[1] <- "mcCL_NCDC/mcNCDCd3/" 
sim.dir[2] <- "mcCL_NCDCV/"
sim.dir[3] <- "mcCL_MPE/mcMPEd2/"
sim.dir[4] <- "mcCL_MPEV/"
sim.dir[5] <- "lrCL_NCDC/lrNCDC_redo_d4/"
sim.dir[6] <- "lrCL_NCDCV/"
sim.dir[7] <- "lrCL_MPE/lrMPEd2/"
sim.dir[8] <- "lrCL_MPEV/"
sim.dir[9] <- "ncCL_NCDC/ncNCDCd3/" 
sim.dir[10] <- "ncCL_NCDCV/"
sim.dir[11] <- "ncCL_MPE/ncMPEd5/"
sim.dir[12] <- "ncCL_MPEV/"
sim.dir[13] <- "fbCL_NCDC/fbNCDCd4/"
sim.dir[14] <- "fbCL_NCDCV/"
sim.dir[15] <- "fbCL_MPE/fbMPEd5/"
sim.dir[16] <- "fbCL_MPEV/"
###############################################################

sim.label <- vector(mode="character",length=Npasses)
sim.label[1] <- "mcNCDC" 
sim.label[2] <- "mcNCDCV"
sim.label[3] <- "mcMPE"
sim.label[4] <- "mcMPEV"
sim.label[5] <- "lrNCDC"
sim.label[6] <- "lrNCDCV"
sim.label[7] <- "lrMPE"
sim.label[8] <- "lrMPEV"
sim.label[9] <- "ncNCDC" 
sim.label[10] <- "ncNCDCV"
sim.label[11] <- "ncMPE"
sim.label[12] <- "ncMPEV"
sim.label[13] <- "fbNCDC"
sim.label[14] <- "fbNCDCV"
sim.label[15] <- "fbMPE"
sim.label[16] <- "fbMPEV"

#graphics directory
graphics.dir <- source_dir

#check to see that they all exist
for(i in 1:Npasses){
  pr int(file.exists(paste(source_dir,sim.dir[i],sep="")))
}

##############################################
#R figure for observations versus fdc percentiles
#assign dates
calib.startdate <- "2002-01-01"
calib.enddate <- "2007-12-31"
calib.Ndays <- (as.numeric(as.Date(calib.enddate))-as.numeric(as.Date(calib.startdate)))+1
calib.Dates <- as.Date(calib.startdate) + 1:Ndays -1

valid.startdate <- "2008-01-01"
valid.enddate <- "2010-12-31"
valid.Ndays <- (as.numeric(as.Date(valid.enddate))-as.numeric(as.Date(valid.startdate)))+1
valid.Dates <- as.Date(valid.startdate) + 1:Ndays -1

#little river observed flow
calib.lr.flow.observed <- read.table(paste(source_dir,"lrCL_MPE/observed.txt",sep=""),header=FALSE)
calib.lr.flow.observed <- as.vector(t(calib.lr.flow.observed[,3]))
summary(calib.lr.flow.observed)
valid.lr.flow.observed <- read.table(paste(source_dir,"lrCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.lr.flow.observed <- as.vector(t(valid.lr.flow.observed[,3]))
summary(valid.lr.flow.observed)
#mountain creek observed flow
calib.mc.flow.observed <- read.table(paste(source_dir,"mcCL_MPE/observed.txt",sep=""),header=FALSE)
calib.mc.flow.observed <- as.vector(t(calib.mc.flow.observed[,3]))
summary(calib.mc.flow.observed)
valid.mc.flow.observed <- read.table(paste(source_dir,"mcCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.mc.flow.observed <- as.vector(t(valid.mc.flow.observed[,3]))
summary(valid.mc.flow.observed)
#neuse clayton observed flow
calib.nc.flow.observed <- read.table(paste(source_dir,"ncCL_MPE/ncMPEd1/observed.txt",sep=""),header=FALSE)
calib.nc.flow.observed <- as.vector(t(calib.nc.flow.observed[,3]))
summary(calib.nc.flow.observed)
valid.nc.flow.observed <- read.table(paste(source_dir,"ncCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.nc.flow.observed <- as.vector(t(valid.nc.flow.observed[,3]))
summary(valid.nc.flow.observed)
#fort barnwell observed flow
calib.fb.flow.observed <- read.table(paste(source_dir,"fbCL_MPE/fbMPEd1/observed.txt",sep=""),header=FALSE)
calib.fb.flow.observed <- as.vector(t(calib.fb.flow.observed[,3]))
summary(calib.fb.flow.observed)
valid.fb.flow.observed <- read.table(paste(source_dir,"fbCL_MPEV/observed.txt",sep=""),header=FALSE)
valid.fb.flow.observed <- as.vector(t(valid.fb.flow.observed[,3]))
summary(valid.fb.flow.observed)

#observed percentiles
calib.lr.obs.95 <- quantile(calib.lr.flow.observed,0.95)
calib.lr.obs.50 <- quantile(calib.lr.flow.observed,0.5)
calib.lr.obs.5 <- quantile(calib.lr.flow.observed,0.05)
calib.mc.obs.95 <- quantile(calib.mc.flow.observed,0.95)
calib.mc.obs.50 <- quantile(calib.mc.flow.observed,0.5)
calib.mc.obs.5 <- quantile(calib.mc.flow.observed,0.05)
calib.nc.obs.95 <- quantile(calib.nc.flow.observed,0.95)
calib.nc.obs.50 <- quantile(calib.nc.flow.observed,0.5)
calib.nc.obs.5 <- quantile(calib.nc.flow.observed,0.05)
calib.fb.obs.95 <- quantile(calib.fb.flow.observed,0.95)
calib.fb.obs.50 <- quantile(calib.fb.flow.observed,0.5)
calib.fb.obs.5 <- quantile(calib.fb.flow.observed,0.05)
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
sim.5th.percentiles <- matrix(data=NA,nrow=2001,ncol=16)
sim.50th.percentiles <- matrix(data=NA,nrow=2001,ncol=16)
sim.95th.percentiles <- matrix(data=NA,nrow=2001,ncol=16)

#check to see that they exist
for(i in 1:Npasses){
  print(file.exists(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep="")))
}


for(i in 1:16){
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

#Mountain Creek
pdf(paste(source_dir,"figure_precip_percentiles_mc.pdf",sep=""),height=8.5,width=3)
  par(mfrow=c(3,2),mai=c(0.8976/10,0.05,0.7216/4,0.05),mgp=c(2.25,1,0),oma=c(0,3,1.8,0))
  ## Mountain Creek
  ymax.95 <- max(sim.95th.percentiles[,1:4],calib.mc.obs.95,valid.mc.obs.95)
  ymin.95 <- min(sim.95th.percentiles[,1:4],calib.mc.obs.95,valid.mc.obs.95)
  ymax.50 <- max(sim.50th.percentiles[,1:4],calib.mc.obs.50,valid.mc.obs.50)
  ymin.50 <- min(sim.50th.percentiles[,1:4],calib.mc.obs.50,valid.mc.obs.50)
  ymax.5 <- max(sim.5th.percentiles[,1:4],calib.mc.obs.5,valid.mc.obs.5)
  ymin.5 <- min(sim.5th.percentiles[,1:4],calib.mc.obs.5,valid.mc.obs.5)
  #95th percentiles
  boxplot(cbind(sim.95th.percentiles[,1],sim.95th.percentiles[,2]),ylim=c(ymin.95,ymax.95),xaxt="n",col=c("gold","steelblue3"))
    title("NCDC")
    lines(c(0.5,1.5),c(calib.mc.obs.95,calib.mc.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.95,valid.mc.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.95+ymin.95)/2,labels="95th Percentiles",outer=TRUE,padj=-1.2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.95th.percentiles[,3],sim.95th.percentiles[,4]),ylim=c(ymin.95,ymax.95),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    title("MPE")
    lines(c(0.5,1.5),c(calib.mc.obs.95,calib.mc.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.95,valid.mc.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  #50th percentiles
  boxplot(cbind(sim.50th.percentiles[,1],sim.50th.percentiles[,2]),ylim=c(ymin.50,ymax.50),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.mc.obs.50,calib.mc.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.50,valid.mc.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.50+ymin.50)/2,labels="50th Percentiles",outer=TRUE,padj=-1.2)
  boxplot(cbind(sim.50th.percentiles[,3],sim.50th.percentiles[,4]),ylim=c(ymin.50,ymax.50),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.mc.obs.50,calib.mc.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.50,valid.mc.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
  # 5th percentiles
  par(mai=c(0.8976/3,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.5th.percentiles[,1],sim.5th.percentiles[,2]),ylim=c(ymin.5,ymax.5),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.mc.obs.5,calib.mc.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.5,valid.mc.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.5+ymin.5)/2,labels="5th Percentiles",outer=TRUE,padj=-1.2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
  boxplot(cbind(sim.5th.percentiles[,3],sim.5th.percentiles[,4]),ylim=c(ymin.5,ymax.5),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.mc.obs.5,calib.mc.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.mc.obs.5,valid.mc.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.mc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.mc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
dev.off()

#Little River
pdf(paste(source_dir,"figure_precip_percentiles_lr.pdf",sep=""),height=8.5,width=3)
  par(mfrow=c(3,2),mai=c(0.8976/10,0.05,0.7216/4,0.05),mgp=c(2.25,1,0),oma=c(0,3,1.8,0))
  ## Little River
  ymax.95 <- max(sim.95th.percentiles[,5:8],calib.lr.obs.95,valid.lr.obs.95)
  ymin.95 <- min(sim.95th.percentiles[,5:8],calib.lr.obs.95,valid.lr.obs.95)
  ymax.50 <- max(sim.50th.percentiles[,5:8],calib.lr.obs.50,valid.lr.obs.50)
  ymin.50 <- min(sim.50th.percentiles[,5:8],calib.lr.obs.50,valid.lr.obs.50)
  ymax.5 <- max(sim.5th.percentiles[,5:8],calib.lr.obs.5,valid.lr.obs.5)
  ymin.5 <- min(sim.5th.percentiles[,5:8],calib.lr.obs.5,valid.lr.obs.5)
  #95th percentiles
  boxplot(cbind(sim.95th.percentiles[,5],sim.95th.percentiles[,6]),ylim=c(ymin.95,ymax.95),xaxt="n",col=c("gold","steelblue3"))
    title("NCDC")
    lines(c(0.5,1.5),c(calib.lr.obs.95,calib.lr.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.95,valid.lr.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.95+ymin.95)/2,labels="95th Percentiles",outer=TRUE,padj=-1.2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.95th.percentiles[,7],sim.95th.percentiles[,8]),ylim=c(ymin.95,ymax.95),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    title("MPE")
    lines(c(0.5,1.5),c(calib.lr.obs.95,calib.lr.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.95,valid.lr.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  #50th percentiles
  boxplot(cbind(sim.50th.percentiles[,5],sim.50th.percentiles[,6]),ylim=c(ymin.50,ymax.50),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.lr.obs.50,calib.lr.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.50,valid.lr.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.50+ymin.50)/2,labels="50th Percentiles",outer=TRUE,padj=-1.2)
  boxplot(cbind(sim.50th.percentiles[,7],sim.50th.percentiles[,8]),ylim=c(ymin.50,ymax.50),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.lr.obs.50,calib.lr.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.50,valid.lr.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.50,col="red",pch=1,cex=1.8,lwd=2)
  # 5th percentiles
  par(mai=c(0.8976/3,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.5th.percentiles[,5],sim.5th.percentiles[,6]),ylim=c(ymin.5,ymax.5),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.lr.obs.5,calib.lr.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.5,valid.lr.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.5+ymin.5)/2,labels="5th Percentiles",outer=TRUE,padj=-1.2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
  boxplot(cbind(sim.5th.percentiles[,7],sim.5th.percentiles[,8]),ylim=c(ymin.5,ymax.5),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.lr.obs.5,calib.lr.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.lr.obs.5,valid.lr.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.lr.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.lr.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
dev.off()

#Neuse Clayton
pdf(paste(source_dir,"figure_precip_percentiles_nc.pdf",sep=""),height=8.5,width=3)
  par(mfrow=c(3,2),mai=c(0.8976/10,0.05,0.7216/4,0.05),mgp=c(2.25,1,0),oma=c(0,3,1.8,0))
  ## Neuse Clayton
  ymax.95 <- max(sim.95th.percentiles[,9:12],calib.nc.obs.95,valid.nc.obs.95)
  ymin.95 <- min(sim.95th.percentiles[,9:12],calib.nc.obs.95,valid.nc.obs.95)
  ymax.50 <- max(sim.50th.percentiles[,9:12],calib.nc.obs.50,valid.nc.obs.50)
  ymin.50 <- min(sim.50th.percentiles[,9:12],calib.nc.obs.50,valid.nc.obs.50)
  ymax.5 <- max(sim.5th.percentiles[,9:12],calib.nc.obs.5,valid.nc.obs.5)
  ymin.5 <- min(sim.5th.percentiles[,9:12],calib.nc.obs.5,valid.nc.obs.5)
  #95th percentiles
  boxplot(cbind(sim.95th.percentiles[,9],sim.95th.percentiles[,10]),ylim=c(ymin.95,ymax.95),xaxt="n",col=c("gold","steelblue3"))
    title("NCDC")
    lines(c(0.5,1.5),c(calib.nc.obs.95,calib.nc.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.95,valid.nc.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.95+ymin.95)/2,labels="95th Percentiles",outer=TRUE,padj=-1.2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.95th.percentiles[,11],sim.95th.percentiles[,12]),ylim=c(ymin.95,ymax.95),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    title("MPE")
    lines(c(0.5,1.5),c(calib.nc.obs.95,calib.nc.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.95,valid.nc.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  #50th percentiles
  boxplot(cbind(sim.50th.percentiles[,9],sim.50th.percentiles[,10]),ylim=c(ymin.50,ymax.50),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.nc.obs.50,calib.nc.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.50,valid.nc.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.50+ymin.50)/2,labels="50th Percentiles",outer=TRUE,padj=-1.2)
  boxplot(cbind(sim.50th.percentiles[,11],sim.50th.percentiles[,12]),ylim=c(ymin.50,ymax.50),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.nc.obs.50,calib.nc.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.50,valid.nc.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.50,col="red",pch=1,cex=1.8,lwd=2)
  # 5th percentiles
  par(mai=c(0.8976/3,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.5th.percentiles[,9],sim.5th.percentiles[,10]),ylim=c(ymin.5,ymax.5),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.nc.obs.5,calib.nc.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.5,valid.nc.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.5+ymin.5)/2,labels="5th Percentiles",outer=TRUE,padj=-1.2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
  boxplot(cbind(sim.5th.percentiles[,11],sim.5th.percentiles[,12]),ylim=c(ymin.5,ymax.5),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.nc.obs.5,calib.nc.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.nc.obs.5,valid.nc.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.nc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.nc.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
dev.off()

#Fort Barnwell
pdf(paste(source_dir,"figure_precip_percentiles_fb.pdf",sep=""),height=8.5,width=3)
  par(mfrow=c(3,2),mai=c(0.8976/10,0.05,0.7216/4,0.05),mgp=c(2.25,1,0),oma=c(0,3,1.8,0))
  ## Fort Barnwell
  ymax.95 <- max(sim.95th.percentiles[,13:16],calib.fb.obs.95,valid.fb.obs.95)
  ymin.95 <- min(sim.95th.percentiles[,13:16],calib.fb.obs.95,valid.fb.obs.95)
  ymax.50 <- max(sim.50th.percentiles[,13:16],calib.fb.obs.50,valid.fb.obs.50)
  ymin.50 <- min(sim.50th.percentiles[,13:16],calib.fb.obs.50,valid.fb.obs.50)
  ymax.5 <- max(sim.5th.percentiles[,13:16],calib.fb.obs.5,valid.fb.obs.5)
  ymin.5 <- min(sim.5th.percentiles[,13:16],calib.fb.obs.5,valid.fb.obs.5)
  #95th percentiles
  boxplot(cbind(sim.95th.percentiles[,13],sim.95th.percentiles[,14]),ylim=c(ymin.95,ymax.95),xaxt="n",col=c("gold","steelblue3"))
    title("NCDC")
    lines(c(0.5,1.5),c(calib.fb.obs.95,calib.fb.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.95,valid.fb.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.95+ymin.95)/2,labels="95th Percentiles",outer=TRUE,padj=-1.2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.95th.percentiles[,15],sim.95th.percentiles[,16]),ylim=c(ymin.95,ymax.95),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    title("MPE")
    lines(c(0.5,1.5),c(calib.fb.obs.95,calib.fb.obs.95),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.95,valid.fb.obs.95),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.95,col="red",pch=1,cex=1.8,lwd=2)
    #title(main="Little River: Simulated Flow Percentile Distributions",outer=TRUE,cex=1.6)
    par(mai=c(0.8976/10,0.05,0.7216/4,0.05))
  #50th percentiles
  boxplot(cbind(sim.50th.percentiles[,13],sim.50th.percentiles[,14]),ylim=c(ymin.50,ymax.50),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.fb.obs.50,calib.fb.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.50,valid.fb.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.50+ymin.50)/2,labels="50th Percentiles",outer=TRUE,padj=-1.2)
  boxplot(cbind(sim.50th.percentiles[,15],sim.50th.percentiles[,16]),ylim=c(ymin.50,ymax.50),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.fb.obs.50,calib.fb.obs.50),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.50,valid.fb.obs.50),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.50,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.50,col="red",pch=1,cex=1.8,lwd=2)
  # 5th percentiles
  par(mai=c(0.8976/3,0.05,0.7216/4,0.05))
  boxplot(cbind(sim.5th.percentiles[,13],sim.5th.percentiles[,14]),ylim=c(ymin.5,ymax.5),xaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.fb.obs.5,calib.fb.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.5,valid.fb.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(2,at=(ymax.5+ymin.5)/2,labels="5th Percentiles",outer=TRUE,padj=-1.2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
  boxplot(cbind(sim.5th.percentiles[,15],sim.5th.percentiles[,16]),ylim=c(ymin.5,ymax.5),xaxt="n",yaxt="n",col=c("gold","steelblue3"))
    lines(c(0.5,1.5),c(calib.fb.obs.5,calib.fb.obs.5),col="red",lwd=3,lend=2)
    lines(c(1.5,2.5),c(valid.fb.obs.5,valid.fb.obs.5),col="red",lwd=3,lend=1)
    points(1,calib.fb.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    points(2,valid.fb.obs.5,col="red",pch=1,cex=1.8,lwd=2)
    axis(side=1,at=c(1,2),labels=c("C","V"))
dev.off()
