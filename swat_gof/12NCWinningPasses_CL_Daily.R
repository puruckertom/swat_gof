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
Npasses = 8 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
#graphical output and tables will be generated in this directory
sim.dir[1] <- "ncCL_NCDC/ncNCDCd3/" 
sim.dir[2] <- "ncCL_NCDC/ncNCDCd3/"
sim.dir[3] <- "ncCL_MPE/ncMPEd5/"
sim.dir[4] <- "ncCL_MPE/ncMPEd5/"
sim.dir[5] <- "ncCL_NCDCV/" 
sim.dir[6] <- "ncCL_NCDCV/"
sim.dir[7] <- "ncCL_MPEV/"
sim.dir[8] <- "ncCL_MPEV/"
###############################################################

#graphics directory
graphics.dir <- source_dir

#check to see that they all exist
for(i in 1:Npasses){
  pr int(file.exists(paste(source_dir,sim.dir[i],sep="")))
}

#load the real flow data
flow.observed <- read.table(paste(source_dir,sim.dir[1],"observed.txt",sep=""),header=FALSE)
summary(flow.observed)
dim(flow.observed)
flow.observed <- as.vector(t(flow.observed[,3]))
Nt <- length(flow.observed)
Nsims <- 2001
if(boolDays==TRUE){
  flow.observed <- flow.observed[1:Ndays]
}else{
  flow.observed <- flow.observed[1:Nmonths]
}

#load the validation flow data
#assign dates
startdate.valid <- "2008-01-01"
enddate.valid <- "2010-12-31"
Ndays.valid <- (as.numeric(as.Date(enddate.valid))-as.numeric(as.Date(startdate.valid)))+1
Nmonths.valid <- length(seq(from=as.Date(startdate.valid), to=as.Date(enddate.valid), by = "month"))
boolDays=TRUE

#set x.Dates as monthly or daily
if(boolDays==TRUE){
  x.Dates.valid <- as.Date(startdate.valid) + 1:Ndays.valid -1
}else{
  x.Dates.valid <- seq(as.Date(startdate.valid), by="month", length.out=Nmonths.valid)
}

#load the real flow data for the validation time period
flow.observed.valid <- read.table(paste(source_dir,sim.dir[5],"observed.txt",sep=""),header=FALSE)
summary(flow.observed)
dim(flow.observed.valid)
flow.observed.valid <- as.vector(t(flow.observed.valid[,3]))
length(flow.observed.valid)
Nt.valid <- length(flow.observed.valid)
Nsims <- 2001
if(boolDays==TRUE){
  flow.observed.valid <- flow.observed.valid[1:Ndays.valid]
}else{
  flow.observed.valid <- flow.observed.valid[1:Nmonths.valid]
}

#load the simulated data sets
#check to see that they all exist
for(i in 1:Npasses){
  pr int(file.exists(paste(source_dir,sim.dir[i],"flow_modeled_daily.csv",sep="")))
}

#load data for calibration period for 4 metrics
if(boolDays==TRUE){
  flow.sims <- array(data=NA, dim=c(Ndays,Nsims,4))
  system.time(
    for(i in 1:4){
      flow.sims.temp <- t(read.csv(paste(source_dir,sim.dir[i],"flow_modeled_daily.csv",sep=""),header=TRUE,row.names=1))
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Ndays,])
      flow.sims[,,i] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}else{
  flow.sims <- array(data=NA, dim=c(Nmonths,Nsims,4))
  system.time(
    for(i in 1:4){
      flow.sims.temp <- t(read.csv(paste(source_dir,sim.dir[i],"flow_modeled_daily.csv",sep=""),header=TRUE,row.names=1))
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Nmonths,])
      flow.sims[,,i] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}

#load data for validation period for 4 metrics
if(boolDays==TRUE){
  flow.sims.valid <- array(data=NA, dim=c(Ndays.valid,Nsims,4))
  system.time(
    for(i in 5:8){
      flow.sims.temp <- t(read.csv(paste(source_dir,sim.dir[i],"flow_modeled_daily.csv",sep=""),header=TRUE,row.names=1))
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Ndays.valid,])
      flow.sims.valid[,,i-4] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}else{
  system.time(
    for(i in 5:8){
      flow.sims.temp <- t(read.csv(paste(source_dir,sim.dir[i],"flow_modeled_daily.csv",sep=""),header=TRUE,row.names=1))
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Nmonths.valid,])
      flow.sims.valid[,,i-4] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}

#calculate unweighted quantiles for the simulated data for each pass
quantiles.values <- c(0.001,0.023,0.159,0.5,0.841,0.977,0.999)
Nquantiles <- length(quantiles.values)

#get quantiles for calibration period
if(boolDays==TRUE){
  quantiles.sims <- array(data=NA, dim=c(Ndays,Nquantiles,4))
  system.time(
    for(i in 1:4){
      for(j in 1:Ndays){
        quantiles.sims[j,,i] <- quantile(flow.sims[j,,i],probs=quantiles.values)
      }
    }
  )
}else{
  quantiles.sims <- array(data=NA, dim=c(Nmonths,Nquantiles,4))
  system.time(
    for(i in 1:4){
      for(j in 1:Nmonths){
        quantiles.sims[j,,i] <- quantile(flow.sims[j,,i],probs=quantiles.values)
      }
    }
  )
}
dim(quantiles.sims)
#summary(quantiles.sims[,,3])

#get quantiles for validation period
if(boolDays==TRUE){
  quantiles.sims.valid <- array(data=NA, dim=c(Ndays.valid,Nquantiles,4))
  system.time(
    for(i in 1:4){
      for(j in 1:Ndays.valid){
        quantiles.sims.valid[j,,i] <- quantile(flow.sims.valid[j,,i],probs=quantiles.values)
      }
    }
  )
}else{
  quantiles.sims.valid <- array(data=NA, dim=c(Nmonths.valid,Nquantiles,4))
  system.time(
    for(i in 1:4){
      for(j in 1:Nmonths.valid){
        quantiles.sims.valid[j,,i] <- quantile(flow.sims.valid[j,,i],probs=quantiles.values)
      }
    }
  )
}
dim(quantiles.sims.valid)
#summary(quantiles.sims.valid[,,3])

#convert sims and flow estimates to time series for calibration
length(x.Dates)
colnames(x.Dates)
#dim=c(Ndays,Nquantiles,Npasses))
quantiles.sims <- quantiles.sims[-(length(x.Dates)+1:Nt),1:Nquantiles,1:4,drop=FALSE]
dim(quantiles.sims)
quantile.names <- paste("X",quantiles.values,sep="")
colnames(quantiles.sims) <- quantile.names
colnames(quantiles.sims)
dim(quantiles.sims)

#convert sims and flow estimates to time series for validation
length(x.Dates.valid)
colnames(x.Dates.valid)
#dim=c(Ndays,Nquantiles,Npasses))
quantiles.sims.valid <- quantiles.sims.valid[-(length(x.Dates.valid)+1:Nt),1:Nquantiles,1:4,drop=FALSE]
dim(quantiles.sims.valid)
quantile.names <- paste("X",quantiles.values,sep="")
colnames(quantiles.sims.valid) <- quantile.names
colnames(quantiles.sims.valid)
dim(quantiles.sims.valid)

#zoo objects
the.zero <- min(flow.observed[which(flow.observed>0)],quantiles.sims[which(quantiles.sims>0)])
the.max <- max(flow.observed,quantiles.sims)
the.zero.valid <- min(flow.observed.valid[which(flow.observed.valid>0)],quantiles.sims.valid[which(quantiles.sims.valid>0)])
the.max.valid <- max(flow.observed.valid,quantiles.sims.valid)
#observed
flow.observed.zoo <- zoo(flow.observed,x.Dates)
flow.observed.zeroes <- flow.observed
flow.observed.zeroes[which(flow.observed==0)] <- the.zero
flow.observed.zeroes.zoo <- zoo(flow.observed.zeroes,x.Dates)
min(flow.observed.zeroes.zoo)
flow.observed.zoo.valid <- zoo(flow.observed.valid,x.Dates.valid)
flow.observed.zeroes.valid <- flow.observed.valid
flow.observed.zeroes.valid[which(flow.observed.valid==0)] <- the.zero.valid
flow.observed.zeroes.zoo.valid <- zoo(flow.observed.zeroes.valid,x.Dates.valid)
min(flow.observed.zeroes.zoo.valid)
#sims
flow.sims.zoo <- zoo(flow.sims,x.Dates)
flow.sims.zeroes <- flow.sims
flow.sims.zeroes[which(flow.sims==0)] <- the.zero
flow.sims.zeroes.zoo <- zoo(flow.sims.zeroes,x.Dates)
min(flow.sims.zeroes.zoo)
flow.sims.zoo.valid <- zoo(flow.sims.valid,x.Dates.valid)
flow.sims.zeroes.valid <- flow.sims.valid
flow.sims.zeroes.valid[which(flow.sims.valid==0)] <- the.zero.valid
flow.sims.zeroes.zoo.valid <- zoo(flow.sims.zeroes.valid,x.Dates.valid)
min(flow.sims.zeroes.zoo.valid)
#simulated quantiles
quantiles.sims.zoo <- zoo(quantiles.sims,x.Dates)
quantiles.sims.zeroes <- quantiles.sims
quantiles.sims.zeroes[which(quantiles.sims==0)] <- the.zero
quantiles.sims.zeroes.zoo <- zoo(quantiles.sims.zeroes,x.Dates)
min(quantiles.sims.zeroes.zoo)
quantiles.sims.zoo.valid <- zoo(quantiles.sims.valid,x.Dates.valid)
quantiles.sims.zeroes.valid <- quantiles.sims.valid
quantiles.sims.zeroes.valid[which(quantiles.sims.valid==0)] <- the.zero.valid
quantiles.sims.zeroes.zoo.valid <- zoo(quantiles.sims.zeroes.valid,x.Dates.valid)
min(quantiles.sims.zeroes.zoo.valid)

#plot the quantiles versus the flow data - logged
pass.tspolygons.filename <- paste(graphics.dir,"nc_winning_passes.pdf",sep="")
npass.label <- vector(mode="character",length=4)
npass.label[1] <- "CL: NCDC"
npass.label[2] <- "CL: NCDC"
npass.label[3] <- "CL: MPE"
npass.label[4] <- "CL: MPE"
npass.label[5] <- "CL: NCDC"
npass.label[6] <- "CL: NCDC"
npass.label[7] <- "CL: MPE"
npass.label[8] <- "CL: MPE"
pdf(file=pass.tspolygons.filename,width=10.5, height=5.5, bg="white")
  par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
  layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
  if(boolDays==TRUE){
    for(i in 1:4){
      if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
      if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main="",sub=npass.label[i])
      if(i==1){title(main="Calibration \n CL (Pass=2)",cex=1.6)}
      if(i==2){title(main="NSE (Pass=2)",cex=1.6)}
      if(i==3){title(main="MNS (Pass=5)",cex=1.6)}
      if(i==4){title(main="RSD (Pass=1)",cex=1.6)}
      if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
      labels.dates <- c("2002","2003","2004","2005","2006","2007","2008")
      labels.locs <- seq(x.Dates[1],x.Dates[Ndays],by=365)
      axis(side=1,at=as.numeric(labels.locs),labels=c("","","","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
      if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
      plot(quantiles.sims.zeroes.zoo.valid[,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="",xlab="",main="")
      if(i==1){title(main="Validation \n CL",cex=1.6)}
      if(i==2){title(main="NSE",cex=1.6)}
      if(i==3){title(main="MNS",cex=1.6)}
      if(i==4){title(main="RSD",cex=1.6)}      
      labels.dates.valid <- c("2008","2009","2010","2011")
      labels.locs.valid <- seq(x.Dates.valid[1],x.Dates.valid[Ndays.valid],by=365)
      axis(side=1,at=as.numeric(labels.locs.valid),labels=c("","","",""))
      axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,1,i],rev(quantiles.sims.zeroes.valid[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,2,i],rev(quantiles.sims.zeroes.valid[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,3,i],rev(quantiles.sims.zeroes.valid[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo.valid[,4,i],col="black")
    }
  }else{
    for(i in 1:4){
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label[i])
      labels.dates <- c(x.Dates[1],x.Dates[13],x.Dates[25],x.Dates[37],x.Dates[49],x.Dates[61])
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label[i])
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
    }    
  }
dev.off()


#plot the quantiles versus the flow data - not logged
pass.tspolygons.filename <- paste(graphics.dir,"nc_notlogged_winning_passes.pdf",sep="")
pdf(file=pass.tspolygons.filename,width=5.5, height=10.5, bg="white")
  par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
  layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
  if(boolDays==TRUE){
    for(i in 1:4){
      if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
      if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),col="white",ylab="Flow",xlab="",main="",sub=npass.label[i])
      if(i==1){title(main="Calibration \n CL (Pass=2)",cex=1.6)}
      if(i==2){title(main="NSE (Pass=2)",cex=1.6)}
      if(i==3){title(main="MNS (Pass=5)",cex=1.6)}
      if(i==4){title(main="RSD (Pass=1)",cex=1.6)}
      if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
      labels.dates <- c("2002","2003","2004","2005","2006","2007","2008")
      labels.locs <- seq(x.Dates[1],x.Dates[Ndays],by=365)
      axis(side=1,at=as.numeric(labels.locs),labels=c("","","","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
      if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
      plot(quantiles.sims.zeroes.zoo.valid[,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),col="white",ylab="",xlab="",main="")
      if(i==1){title(main="Validation \n CL",cex=1.6)}
      if(i==2){title(main="NSE",cex=1.6)}
      if(i==3){title(main="MNS",cex=1.6)}
      if(i==4){title(main="RSD",cex=1.6)}      
      labels.dates.valid <- c("2008","2009","2010","2011")
      labels.locs.valid <- seq(x.Dates.valid[1],x.Dates.valid[Ndays.valid],by=365)
      axis(side=1,at=as.numeric(labels.locs.valid),labels=c("","","",""))
      axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,1,i],rev(quantiles.sims.zeroes.valid[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,2,i],rev(quantiles.sims.zeroes.valid[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates.valid[1:Ndays.valid],x.Dates.valid[Ndays.valid:1]),c(quantiles.sims.zeroes.valid[,3,i],rev(quantiles.sims.zeroes.valid[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo.valid[,4,i],col="black")
    }
  }else{
    for(i in 1:4){
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),col="white",ylab="Flow",xlab="",main=npass.label[i])
      labels.dates <- c(x.Dates[1],x.Dates[13],x.Dates[25],x.Dates[37],x.Dates[49],x.Dates[61])
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),col="white",ylab="Flow",xlab="",main=npass.label[i])
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
    }    
  }
dev.off()


#################
###############
### subset
# set beginning and ending dates for calibration and validation time series
start.calib <- 1462
end.calib <- 1826
x.Dates[start.calib]
x.Dates[end.calib]
start.valid <- 1
end.valid <- 366
x.Dates.valid[start.valid]
x.Dates.valid[end.valid]
tick.space <- 30

the.zero.log <- 0.1
the.max.here <- max(flow.observed.zeroes.zoo[start.calib:end.calib,],quantiles.sims.zeroes.zoo[start.calib:end.calib,])


#plot the quantiles versus the flow data - logged
pass.tspolygons.filename <- paste(graphics.dir,"nc_logged_2006_red.pdf",sep="")
pdf(file=pass.tspolygons.filename,width=10.5, height=5.5, bg="white")
  par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
  layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
  if(boolDays==TRUE){
    for(i in 1:4){
      if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
      if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
      plot(quantiles.sims.zeroes.zoo[start.calib:end.calib,4,i],xaxt="n",ylim=c(the.zero.log,the.max.here),log="y",col="white",ylab="Flow",xlab="",main="")#,sub=npass.label[i])
      if(i==1){title(main="Calibration \n CL (Pass=2)",cex=1.6)}
      if(i==2){title(main="NSE (Pass=2)",cex=1.6)}
      if(i==3){title(main="MNS (Pass=5)",cex=1.6)}
      if(i==4){title(main="RSD (Pass=1)",cex=1.6)}
      if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
      labels.dates <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
      labels.locs <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
      axis(side=1,at=as.numeric(labels.locs),labels=c(rep("",length(labels.locs))))
      if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,1,i],rev(quantiles.sims.zeroes[start.calib:end.calib,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,2,i],rev(quantiles.sims.zeroes[start.calib:end.calib,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,3,i],rev(quantiles.sims.zeroes[start.calib:end.calib,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[start.calib:end.calib,4,i],col="black")
      if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
      if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
      plot(quantiles.sims.zeroes.zoo.valid[start.valid:end.valid,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="",xlab="",main="")
      if(i==1){title(main="Validation \n CL",cex=1.6)}
      if(i==2){title(main="NSE",cex=1.6)}
      if(i==3){title(main="MNS",cex=1.6)}
      if(i==4){title(main="RSD",cex=1.6)}      
      labels.dates.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
      labels.locs.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
      axis(side=1,at=as.numeric(labels.locs.valid),labels=rep("",length(labels.locs.valid)))
      axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,1,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,2,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,3,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo.valid[start.valid:end.valid,4,i],col="black")
    }
  }    
dev.off()

pass.tspolygons.filename <- paste(graphics.dir,"nc_logged_2006_blue.pdf",sep="")
pdf(file=pass.tspolygons.filename,width=10.5, height=5.5, bg="white")
par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
if(boolDays==TRUE){
	for(i in 1:4){
		if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
		if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
		if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
		if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
		plot(quantiles.sims.zeroes.zoo[start.calib:end.calib,4,i],xaxt="n",ylim=c(the.zero.log,the.max.here),log="y",col="white",ylab="Flow",xlab="",main="")#,sub=npass.label[i])
		if(i==1){title(main="Calibration \n CL (Pass=2)",cex=1.6)}
		if(i==2){title(main="NSE (Pass=2)",cex=1.6)}
		if(i==3){title(main="MNS (Pass=5)",cex=1.6)}
		if(i==4){title(main="RSD (Pass=1)",cex=1.6)}
		if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
		labels.dates <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
		labels.locs <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
		axis(side=1,at=as.numeric(labels.locs),labels=c(rep("",length(labels.locs))))
		if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,1,i],rev(quantiles.sims.zeroes[start.calib:end.calib,7,i])),col="lightblue",border=NA)
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,2,i],rev(quantiles.sims.zeroes[start.calib:end.calib,6,i])),col="dodgerblue",border=NA)
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,3,i],rev(quantiles.sims.zeroes[start.calib:end.calib,5,i])),col="dodgerblue3",border=NA)
		#lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
		lines(flow.observed.zeroes.zoo[start.calib:end.calib,4,i],col="black")
		if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
		if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
		if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
		if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
		plot(quantiles.sims.zeroes.zoo.valid[start.valid:end.valid,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="",xlab="",main="")
		if(i==1){title(main="Validation \n CL",cex=1.6)}
		if(i==2){title(main="NSE",cex=1.6)}
		if(i==3){title(main="MNS",cex=1.6)}
		if(i==4){title(main="RSD",cex=1.6)}      
		labels.dates.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
		labels.locs.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
		axis(side=1,at=as.numeric(labels.locs.valid),labels=rep("",length(labels.locs.valid)))
		axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
		if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,1,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,7,i])),col="lightblue",border=NA)
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,2,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,6,i])),col="dodgerblue",border=NA)
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,3,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,5,i])),col="dodgerblue3",border=NA)
		#lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
		lines(flow.observed.zeroes.zoo.valid[start.valid:end.valid,4,i],col="black")
	}
}    
dev.off()

#plot the quantiles versus the flow data - not logged
pass.tspolygons.filename <- paste(graphics.dir,"nc_notlogged_2006_red.pdf",sep="")
pdf(file=pass.tspolygons.filename,width=10.5, height=5.5, bg="white")
  par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
  layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
  if(boolDays==TRUE){
    for(i in 1:4){
      if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
      if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
      if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
      plot(quantiles.sims.zeroes.zoo[start.calib:end.calib,4,i],xaxt="n",ylim=c(0,the.max.here),col="white",ylab="Flow",xlab="",main="")#,sub=npass.label[i])
      if(i==1){title(main="Gauge",cex=1.6)}
      if(i==2){title(main="Gauge",cex=1.6)}
      if(i==3){title(main="Radar",cex=1.6)}
      if(i==4){title(main="Radar",cex=1.6)}
      if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
      labels.dates <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
      labels.locs <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
      axis(side=1,at=as.numeric(labels.locs),labels=c(rep("",length(labels.locs))))
      if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,1,i],rev(quantiles.sims.zeroes[start.calib:end.calib,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,2,i],rev(quantiles.sims.zeroes[start.calib:end.calib,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,3,i],rev(quantiles.sims.zeroes[start.calib:end.calib,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[start.calib:end.calib,4,i],col="black")
      if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
      if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
      if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
      plot(quantiles.sims.zeroes.zoo.valid[start.valid:end.valid,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),col="white",ylab="",xlab="",main="")
      if(i==1){title(main="Validation \n CL",cex=1.6)}
      if(i==2){title(main="NSE",cex=1.6)}
      if(i==3){title(main="MNS",cex=1.6)}
      if(i==4){title(main="RSD",cex=1.6)}      
      labels.dates.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
      labels.locs.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
      axis(side=1,at=as.numeric(labels.locs.valid),labels=rep("",length(labels.locs.valid)))
      axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
      if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,1,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,2,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,3,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo.valid[start.valid:end.valid,4,i],col="black")
    }
  }    
dev.off()

pass.tspolygons.filename <- paste(graphics.dir,"nc_notlogged_2006_blue.pdf",sep="")
pdf(file=pass.tspolygons.filename,width=10.5, height=5.5, bg="white")
par(mgp=c(2.25,1,0),oma=c(0,0,1.7,0))
layout(matrix(1:8, 4, 2, byrow = TRUE),widths=c(.64,.36))
if(boolDays==TRUE){
	for(i in 1:4){
		if(i==1){par(mai=c(0.8976/6,0.7216/1.6,0.7216/2,0.3696/8))}
		if(i==2){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
		if(i==3){par(mai=c(0.8976/6,0.7216/1.6,0.7216/4,0.3696/8))}
		if(i==4){par(mai=c(0.8976/3,0.7216/1.6,0.7216/4,0.3696/8))}      
		plot(quantiles.sims.zeroes.zoo[start.calib:end.calib,4,i],xaxt="n",ylim=c(0,the.max.here),col="white",ylab="Flow",xlab="",main="")#,sub=npass.label[i])
		if(i==1){title(main="Gauge",cex=1.6)}
		if(i==2){title(main="Gauge",cex=1.6)}
		if(i==3){title(main="Radar",cex=1.6)}
		if(i==4){title(main="Radar",cex=1.6)}
		if(i==1){title(main="           Neuse at Clayton: Winning Passes",outer=TRUE,cex=1.8)}
		labels.dates <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
		labels.locs <- seq(x.Dates[start.calib],x.Dates[end.calib],by=tick.space)
		axis(side=1,at=as.numeric(labels.locs),labels=c(rep("",length(labels.locs))))
		if(i==4){axis(side=1,at=as.numeric(labels.locs),labels=labels.dates)}
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,1,i],rev(quantiles.sims.zeroes[start.calib:end.calib,7,i])),col="lightblue",border=NA)
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,2,i],rev(quantiles.sims.zeroes[start.calib:end.calib,6,i])),col="dodgerblue",border=NA)
		polygon(c(x.Dates[start.calib:end.calib],x.Dates[rev(start.calib:end.calib)]),c(quantiles.sims.zeroes[start.calib:end.calib,3,i],rev(quantiles.sims.zeroes[start.calib:end.calib,5,i])),col="dodgerblue3",border=NA)
		#lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
		lines(flow.observed.zeroes.zoo[start.calib:end.calib,4,i],col="black")
		if(i==1){par(mai=c(0.8976/6,0.7216/8,0.7216/2,0.3696/6))}
		if(i==2){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
		if(i==3){par(mai=c(0.8976/6,0.7216/8,0.7216/4,0.3696/6))}
		if(i==4){par(mai=c(0.8976/3,0.7216/8,0.7216/4,0.3696/6))}
		plot(quantiles.sims.zeroes.zoo.valid[start.valid:end.valid,4,i],xaxt="n",yaxt="n",ylim=c(the.zero,the.max),col="white",ylab="",xlab="",main="")
		if(i==1){title(main="Validation \n CL",cex=1.6)}
		if(i==2){title(main="NSE",cex=1.6)}
		if(i==3){title(main="MNS",cex=1.6)}
		if(i==4){title(main="RSD",cex=1.6)}      
		labels.dates.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
		labels.locs.valid <- seq(x.Dates.valid[start.valid],x.Dates.valid[end.valid],by=tick.space)
		axis(side=1,at=as.numeric(labels.locs.valid),labels=rep("",length(labels.locs.valid)))
		axis(side=2,at=c(1e-6,1e-4,1e-2,1e+0,1e+2),labels=c("","","","",""))
		if(i==4){axis(side=1,at=as.numeric(labels.locs.valid),labels=labels.dates.valid)}
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,1,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,7,i])),col="lightblue",border=NA)
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,2,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,6,i])),col="dodgerblue",border=NA)
		polygon(c(x.Dates.valid[start.valid:end.valid],x.Dates.valid[rev(start.valid:end.valid)]),c(quantiles.sims.zeroes.valid[start.valid:end.valid,3,i],rev(quantiles.sims.zeroes.valid[start.valid:end.valid,5,i])),col="dodgerblue3",border=NA)
		#lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
		lines(flow.observed.zeroes.zoo.valid[start.valid:end.valid,4,i],col="black")
	}
}    
dev.off()


