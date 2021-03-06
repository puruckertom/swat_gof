sessionInfo()

library(zoo)
library(lattice)
library(hydroGOF)
#library(abind)
#library(vioplot)

if(.Platform$OS.type=="windows"){source_dir <- "c:/dropbox/"}
if(.Platform$OS.type=="unix"){source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")}

#source_dir <- "//Users/puruckertom/Dropbox/ktp/swat_gof/"
#source_dir <- "//Users/katieprice/Dropbox/ktp/swat_gof/"
#source_dir <- "//Users/mattpurucker/Dropbox/ktp/swat_gof/"
location <- "fbMPEV"

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
Npasses = 1 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
sim.dir[1] <- "fbCL_MPEV/" #graphical output and tables will be generated in this directory
#sim.dir[2] <- "fbCL_MPE/fbMPEd2/"
#sim.dir[3] <- "fbCL_MPE/fbMPEd3/"
#sim.dir[4] <- "fbCL_MPE/fbMPEd4/"
#sim.dir[5] <- "fbCL_MPE/fbMPEd5/"
#sim.dir[6] <- "mcCL/mcMPEd6/"
###############################################################

#graphics directory
graphics.dir <- paste(source_dir,sim.dir[1],sep="")

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

#load the simulated data sets
#check to see that they all exist
for(i in 1:Npasses){
  pr int(file.exists(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep="")))
}

if(boolDays==TRUE){
  flow.sims <- array(data=NA, dim=c(Ndays,Nsims,Npasses))
  system.time(
    for(i in 1:Npasses){
      flow.sims.temp <- read.csv(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep=""),header=TRUE,row.names=1)
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Ndays,])
      flow.sims[,,i] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}else{
  flow.sims <- array(data=NA, dim=c(Nmonths,Nsims,Npasses))
  system.time(
    for(i in 1:Npasses){
      flow.sims.temp <- read.csv(paste(source_dir,sim.dir[i],"flow_modeled.csv",sep=""),header=TRUE,row.names=1)
      dim(flow.sims.temp)
      flow.sims.mat <- as.matrix(flow.sims.temp[1:Nmonths,])
      flow.sims[,,i] <- flow.sims.mat
      rm(flow.sims.mat)
      rm(flow.sims.temp)
    }
  )
}

summary(flow.sims)

#calculate unweighted quantiles for the simulated data for each pass
quantiles.values <- c(0.001,0.023,0.159,0.5,0.841,0.977,0.999)
Nquantiles <- length(quantiles.values)

if(boolDays==TRUE){
  quantiles.sims <- array(data=NA, dim=c(Ndays,Nquantiles,Npasses))
  system.time(
    for(i in 1:Npasses){
      for(j in 1:Ndays){
        quantiles.sims[j,,i] <- quantile(flow.sims[j,,i],probs=quantiles.values)
      }
    }
  )
}else{
  quantiles.sims <- array(data=NA, dim=c(Nmonths,Nquantiles,Npasses))
  system.time(
    for(i in 1:Npasses){
      for(j in 1:Nmonths){
        quantiles.sims[j,,i] <- quantile(flow.sims[j,,i],probs=quantiles.values)
      }
    }
  )
}

dim(quantiles.sims)
#summary(quantiles.sims[,,3])

#convert sims and flow estimates to time series
length(x.Dates)
colnames(x.Dates)
#dim=c(Ndays,Nquantiles,Npasses))
quantiles.sims <- quantiles.sims[-(length(x.Dates)+1:Nt),1:Nquantiles,1:Npasses,drop=FALSE]
dim(quantiles.sims)
quantile.names <- paste("X",quantiles.values,sep="")
colnames(quantiles.sims) <- quantile.names
colnames(quantiles.sims)
dim(quantiles.sims)

#zoo objects
the.zero <- min(flow.observed[which(flow.observed>0)],quantiles.sims[which(quantiles.sims>0)])
the.max <- max(flow.observed,quantiles.sims)
#observed
flow.observed.zoo <- zoo(flow.observed,x.Dates)
flow.observed.zeroes <- flow.observed
flow.observed.zeroes[which(flow.observed==0)] <- the.zero
flow.observed.zeroes.zoo <- zoo(flow.observed.zeroes,x.Dates)
min(flow.observed.zeroes.zoo)
#sims
flow.sims.zoo <- zoo(flow.sims,x.Dates)
flow.sims.zeroes <- flow.sims
flow.sims.zeroes[which(flow.sims==0)] <- the.zero
flow.sims.zeroes.zoo <- zoo(flow.sims.zeroes,x.Dates)
min(flow.sims.zeroes.zoo)
#simulated quantiles
quantiles.sims.zoo <- zoo(quantiles.sims,x.Dates)
quantiles.sims.zeroes <- quantiles.sims
quantiles.sims.zeroes[which(quantiles.sims==0)] <- the.zero
quantiles.sims.zeroes.zoo <- zoo(quantiles.sims.zeroes,x.Dates)
min(quantiles.sims.zeroes.zoo)

##pass statistic
#pass.zscores <- array(data=NA, dim=c(Npasses,Ndays))
#for(i in 1:Npasses){ #Npasses
#  for(j in 1:Ndays){ #Ndays
#    sim.mean <- mean(quantiles.sims[j,,i])
#    sim.sd <- sd(quantiles.sims[j,,i])
#    flow.value <- flow.observed[j]
#    flow.zscore <- (flow.value-sim.mean)/sim.sd
#    pnorm(flow.zscore)
#    pass.zscores[i,j] <- abs(flow.zscore)
#  } 
#}  
#sum(pass.zscores[1,])
#sum(pass.zscores[2,])
#sum(pass.zscores[3,])

#plot the quantiles versus the flow data
pass.tspolygons.filename <- paste(graphics.dir,"pass_ts_polygons_",location,".pdf",sep="")
pdf(file=pass.tspolygons.filename,width=10.5, height=7.5, bg="white")
  par(mfrow=c(2,1))
  if(boolDays==TRUE){
    for(i in 1:Npasses){
      npass.label <- paste(location,": Pass = ",i)
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label)
      labels.dates <- seq(x.Dates[1],x.Dates[Ndays],by=365)
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label)
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Ndays],x.Dates[Ndays:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
    }
  }else{
    for(i in 1:Npasses){
      npass.label <- paste(location,": Pass = ",i)
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label)
      labels.dates <- c(x.Dates[1],x.Dates[13],x.Dates[25],x.Dates[37],x.Dates[49],x.Dates[61])
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="dodgerblue",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="dodgerblue3",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
      plot(quantiles.sims.zeroes.zoo[,4,i],xaxt="n",ylim=c(the.zero,the.max),log="y",col="white",ylab="Flow",xlab="",main=npass.label)
      axis(side=1,at=as.numeric(labels.dates),labels=labels.dates)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,1,i],rev(quantiles.sims.zeroes[,7,i])),col="lightgoldenrod1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,2,i],rev(quantiles.sims.zeroes[,6,i])),col="orange1",border=NA)
      polygon(c(x.Dates[1:Nmonths],x.Dates[Nmonths:1]),c(quantiles.sims.zeroes[,3,i],rev(quantiles.sims.zeroes[,5,i])),col="red2",border=NA)
      #lines(quantiles.sims.zeroes.zoo[,4,i],col="white")
      lines(flow.observed.zeroes.zoo[,4,i],col="black")
    }    
  }
dev.off()

#top3 gofs
pass.top3 <- array(data=NA, dim=c(Npasses,4))
for(i in 1:Npasses){
  pass.top3.gofs <- read.csv(paste(source_dir,sim.dir[i],"top3_gofs.csv",sep=""),header=TRUE)
  dim(pass.top3.gofs)
  colnames(pass.top3.gofs)
  sum.mnseff <- sum(pass.top3.gofs[,2])
  pass.top3[i,1] <- sum.mnseff
  sum.ns2 <- sum(pass.top3.gofs[,3])
  pass.top3[i,2] <- sum.ns2
  sum.rsd <- sum(pass.top3.gofs[,4])
  pass.top3[i,3] <- sum.rsd
  sum.top3 <- sum(pass.top3.gofs[,5])
  pass.top3[i,4] <- sum.top3
}

#sim_gofs.csv
pass.simgofs <- array(data=NA, dim=c(Npasses,4))
raw.simgofs <- array(data=NA, dim=c(4,Nsims,Npasses))
for(i in 1:Npasses){
  pass.simgofs.all <- read.csv(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep=""),header=TRUE)
  dim(pass.simgofs.all)
  class(pass.simgofs.all)
  pass.simgofs.all <- as.matrix(pass.simgofs.all)
  #colnames(pass.simgofs.all)
  #mnseff
  raw.mnseff <- pass.simgofs.all[10,]
  raw.mnseff[which(raw.mnseff<0)]=0
  raw.simgofs[1,,i] <- raw.mnseff
  sum.mnseff <- sum(raw.mnseff)
  pass.simgofs[i,1] <- sum.mnseff
  #nseff
  raw.nseff <- pass.simgofs.all[9,]
  raw.nseff[which(raw.nseff<0)]=0  
  raw.simgofs[2,,i] <- raw.nseff
  sum.ns2 <- sum(raw.nseff)
  pass.simgofs[i,2] <- sum.ns2
  #rsd
  raw.rsd <- pass.simgofs.all[8,]
  raw.rsd <- abs(1-raw.rsd)
  raw.rsd[which(raw.rsd>1)]=1
  raw.rsd <- 1-raw.rsd
  raw.simgofs[3,,i] <- raw.rsd
  sum.rsd <- sum(raw.rsd)
  pass.simgofs[i,3] <- sum.rsd
  #top3
  raw.top3 <- (raw.mnseff+raw.nseff+raw.rsd)/3
  raw.simgofs[4,,i] <- raw.top3
  sum.top3 <- (sum.mnseff+sum.ns2+sum.rsd)/3
  pass.simgofs[i,4] <- sum.top3
}
pass.simgofs <- pass.simgofs/Nsims
rownames(pass.simgofs) <- paste(rep("pass",Npasses),1:Npasses,sep="")
colnames(pass.simgofs) <- c("modNS","NS","rSD","Top3")
pass.simgofs
summary(raw.mnseff)
summary(raw.nseff)
summary(raw.rsd)
summary(raw.top3)

#find max for top3 and hydrograph it
pass.best.filename <- paste(graphics.dir,"pass_best_hydrographs_",location,".pdf",sep="")
which.max <- vector(mode="numeric", length=Npasses)
pdf(file=pass.best.filename,width=10.5, height=7.5, bg="white")
for(i in 1:Npasses){
  which.max[i] <- which(raw.simgofs[4,,i]==max(raw.simgofs[4,,i]))
  best.run.zeroes.zoo <- zoo(flow.sims.zeroes[,min(which(raw.simgofs[4,,i]==max(raw.simgofs[4,,i]))),i],x.Dates)
  ggof(best.run.zeroes.zoo,flow.observed.zeroes.zoo,log="y")
  rm(best.run.zeroes.zoo)
}
dev.off()

pass.plot.filename <- paste(graphics.dir,"pass_plot_",location,".pdf",sep="")
pdf(file=pass.plot.filename,width=10.5, height=7.5, bg="white")
  par(mfrow=c(1,1))
  plot(1:Npasses,pass.simgofs[,1],type="b",ylim=c(0,1),col="red",xaxt="n",xlab="Simulation Sets",ylab="Score")
  axis(1, at=1:Npasses, labels=rownames(pass.simgofs))
  lines(1:Npasses,pass.simgofs[,2],type="b",col="blue")
  lines(1:Npasses,pass.simgofs[,3],type="b",col="green")
  lines(1:Npasses,pass.simgofs[,4],type="b",col="darkgrey")
  leg.txt <- c("Modified NSE","NSE","rSD","Top3")
  legend("topleft",legend=leg.txt,col=c("red","blue","green","darkgrey"),lty=1,pch=21)
dev.off()

pass.boxplots.filename <- paste(graphics.dir,"pass_boxplots_",location,".pdf",sep="")
pdf(file=pass.boxplots.filename,width=7.5, height=10.5, bg="white")
  par(mfrow=c(2,2))
  boxplot(raw.simgofs[1,,],col="red",ylim=c(0,1),notch=TRUE,main="Modified NS Efficiency", range = 0)
  boxplot(raw.simgofs[2,,],col="blue",ylim=c(0,1),notch=TRUE,main="NS Efficiency", range = 0)
  boxplot(raw.simgofs[3,,],col="green",ylim=c(0,1),notch=TRUE,main="rSD", range = 0)
  boxplot(raw.simgofs[4,,],col="darkgrey",ylim=c(0,1),notch=TRUE,main="Top 3", range = 0)
dev.off()

#create sums of raw simgofs
sum.pass.simgofs <- array(data=NA, dim=c(Npasses,4))
for(i in 1:Npasses){
  for(j in 1:4){
    sum.pass.simgofs[i,j] <- sum(raw.simgofs[j,,i])
  }
}
colnames(sum.pass.simgofs) <- c("modNS","NS","rSD","Top3")
rownames(sum.pass.simgofs) <- paste(rep("pass",Npasses),1:Npasses,sep="")
#this is the pass statistic that we are using- Top3 max for the pass
sum.pass.simgofs

#not done yet here
pass.sum.plots.filename <- paste(graphics.dir,"pass_sum_plots_",location,".pdf",sep="")
pdf(file=pass.sum.plots.filename,width=7.5, height=10.5, bg="white")
  par(mfrow=c(2,2))
  plot(1:Npasses,sum.pass.simgofs[,1],type="b",ylim=c(0,1),col="red",xaxt="n",xlab="Simulation Sets",ylab="Score")
  axis(1, at=1:Npasses, labels=rownames(sum.pass.simgofs))
  lines(1:Npasses,sum.pass.simgofs[,2],type="b",col="blue")
  lines(1:Npasses,sum.pass.simgofs[,3],type="b",col="green")
  lines(1:Npasses,sum.pass.simgofs[,4],type="b",col="darkgrey")
  leg.txt <- c("Modified NSE","NSE","rSD","Top3")
  legend("topleft",legend=leg.txt,col=c("red","blue","green","darkgrey"),lty=1,pch=21)
dev.off()
