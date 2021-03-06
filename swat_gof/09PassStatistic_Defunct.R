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
location <- "mc_MNSEff"

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
Npasses = 5 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
sim.dir[1] <- "mcCL/mcMPEd1/" #graphical output and tables will be generated in this directory
sim.dir[2] <- "mcCL/mcMPEd2/"
sim.dir[3] <- "mcCL/mcMPEd3/"
sim.dir[4] <- "mcCL/mcMPEd4/"
sim.dir[5] <- "mcCL/mcMPEd5/"
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

source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("flowViz")
library(flowViz)
openVignette()

#install.packages("flowViz")
#multiple pass picture with lattice and densityplot() and histogram()
dim(sum.pass.simgofs)
dim(raw.simgofs)
## composite likelihood index
evo.cl.1 <- as.vector(raw.simgofs[1,,1])
evo.cl.2 <- as.vector(raw.simgofs[1,,2])
evo.cl.3 <- as.vector(raw.simgofs[1,,3])
evo.cl.4 <- as.vector(raw.simgofs[1,,4])
evo.cl.5 <- as.vector(raw.simgofs[1,,5])
pass.evo.cl <- stack(list(Pass5=evo.cl.5,Pass4=evo.cl.4,Pass3=evo.cl.3,Pass2=evo.cl.2,Pass1=evo.cl.1))
dim(pass.evo.cl)
colnames(pass.evo.cl)
summary(pass.evo.cl)
class(pass.evo.cl$values)
class(pass.evo.cl$ind)
#densityplot(~ values | ind, data=pass.evo.cl, layout=c(1,5))
pass.sum.plots.filename <- paste(graphics.dir,"pass_evolution_cl.pdf",sep="")
histogram.cl <- histogram(~ values | ind, data=pass.evo.cl, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Composite Likelihood Index")
pdf(file=pass.sum.plots.filename,width=5, height=8, bg="white")
  histogram.cl
dev.off()

## nash-sutcliffe
evo.nse.1 <- as.vector(raw.simgofs[2,,1])
evo.nse.2 <- as.vector(raw.simgofs[2,,2])
evo.nse.3 <- as.vector(raw.simgofs[2,,3])
evo.nse.4 <- as.vector(raw.simgofs[2,,4])
evo.nse.5 <- as.vector(raw.simgofs[2,,5])
pass.evo.nse <- stack(list(Pass5=evo.nse.5,Pass4=evo.nse.4,Pass3=evo.nse.3,Pass2=evo.nse.2,Pass1=evo.nse.1))
dim(pass.evo.nse)
colnames(pass.evo.nse)
summary(pass.evo.nse)
class(pass.evo.nse$values)
class(pass.evo.nse$ind)
#densityplot(~ values | ind, data=pass.evo.cl, layout=c(1,5))
pass.sum.plots.filename <- paste(graphics.dir,"pass_evolution_nse.pdf",sep="")
histogram.nse <- histogram(~ values | ind, data=pass.evo.nse, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Nash-Sutcliffe Efficiency")
pdf(file=pass.sum.plots.filename,width=5, height=8, bg="white")
  histogram.nse
dev.off()

## modified nash-sutcliffe
evo.mns.1 <- as.vector(raw.simgofs[3,,1])
evo.mns.2 <- as.vector(raw.simgofs[3,,2])
evo.mns.3 <- as.vector(raw.simgofs[3,,3])
evo.mns.4 <- as.vector(raw.simgofs[3,,4])
evo.mns.5 <- as.vector(raw.simgofs[3,,5])
pass.evo.mns <- stack(list(Pass5=evo.mns.5,Pass4=evo.mns.4,Pass3=evo.mns.3,Pass2=evo.mns.2,Pass1=evo.mns.1))
dim(pass.evo.mns)
colnames(pass.evo.mns)
summary(pass.evo.mns)
class(pass.evo.mns$values)
class(pass.evo.mns$ind)
#densityplot(~ values | ind, data=pass.evo.cl, layout=c(1,5))
pass.sum.plots.filename <- paste(graphics.dir,"pass_evolution_mns.pdf",sep="")
histogram.mns <- histogram(~ values | ind, data=pass.evo.mns, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Modified Nash-Sutcliffe")
pdf(file=pass.sum.plots.filename,width=5, height=8, bg="white")
  histogram.mns
dev.off()

## ratio of standard deviation
evo.rsd.1 <- as.vector(raw.simgofs[4,,1])
evo.rsd.2 <- as.vector(raw.simgofs[4,,2])
evo.rsd.3 <- as.vector(raw.simgofs[4,,3])
evo.rsd.4 <- as.vector(raw.simgofs[4,,4])
evo.rsd.5 <- as.vector(raw.simgofs[4,,5])
pass.evo.rsd <- stack(list(Pass5=evo.rsd.5,Pass4=evo.rsd.4,Pass3=evo.rsd.3,Pass2=evo.rsd.2,Pass1=evo.rsd.1))
dim(pass.evo.rsd)
colnames(pass.evo.rsd)
summary(pass.evo.rsd)
class(pass.evo.rsd$values)
class(pass.evo.rsd$ind)
#densityplot(~ values | ind, data=pass.evo.cl, layout=c(1,5))
pass.sum.plots.filename <- paste(graphics.dir,"pass_evolution_rsd.pdf",sep="")
histogram.rsd <- histogram(~ values | ind, data=pass.evo.rsd, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Standard Deviation Ratio")
pdf(file=pass.sum.plots.filename,width=5, height=8, bg="white")
  par(mfrow=c(1,1))
  print(histogram.rsd)
dev.off()

pass.sum.plots.filename <- paste(graphics.dir,"pass_evolution_all.pdf",sep="")
histogram.cl <- histogram(~ values | ind, data=pass.evo.cl, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Composite Likelihood Index",xlab="",cex.main=0.5)
histogram.nse <- histogram(~ values | ind, data=pass.evo.nse, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Nash-Sutcliffe Efficiency",ylab="",xlab="",cex.main=0.6)
histogram.mns <- histogram(~ values | ind, data=pass.evo.mns, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Modified Nash-Sutcliffe",ylab="",xlab="",cex.main=0.7)
histogram.rsd <- histogram(~ values | ind, data=pass.evo.rsd, layout=c(1,5),xlim=c(0,1),type="percent",col="chartreuse4",main="Standard Deviation Ratio",ylab="",xlab="",cex.main=0.8)
pdf(file=pass.sum.plots.filename,width=12, height=5, bg="white")
  print(histogram.cl,more=TRUE,position=c(0,0,0.28,1))
  print(histogram.nse,more=TRUE,position=c(0.24,0,0.52,1))
  print(histogram.mns,more=TRUE,position=c(0.48,0,0.76,1))
  print(histogram.rsd,position=c(0.72,0,1,1))
dev.off()

#t1 <- raw.simgofs[4,,1]
#t2 <- raw.simgofs[4,,2]
#t3 <- raw.simgofs[4,,3]
#vioplot(t1,t2,t3,col="darkgrey",ylim=c(0,1))
#m1 <- raw.simgofs[1,,1]
#m2 <- raw.simgofs[1,,2]
#m3 <- raw.simgofs[1,,3]
#vioplot(m1,m2,m3,col="red",ylim=c(0,1))
#n1 <- raw.simgofs[2,,1]
#n2 <- raw.simgofs[2,,2]
#n3 <- raw.simgofs[2,,3]
#vioplot(n1,n2,n3,col="blue",ylim=c(0,1))

##then calculate the penalties by comparing the real data to the simulated quantiles
#for(i in 1:Npasses){
#  #load simulated dataset
#  quantile(sim.dataset,c(0.001,0.023.,0.159,0.841,0.977,0.999))
#  gof_set[i] = abs(P68out - .32) + abs(P95out - .05) + abs(P997out - .003)
#}

#import the gof data for each pass
  
#plot boxplots of gofs for each pass

#what happens to the correlation structure over multiple passes?