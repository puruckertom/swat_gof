############################################
install.packages("hydroTSM")
library(hydroTSM) #fdc is flow duration curve
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#manual install (Windows only) is required for IHA, these 3 packages also need to be installed before successfully loading:
install.packages("RColorBrewer")
install.packages("caTools")
install.packages("plyr")
#install.packages("TNC") doesn't exist
# install not working, have to download from https://r-forge.r-project.org/R/ with Mac package installer
library(IHA)
library(lattice)
############################################

##########################################
#setup directory structure
if(.Platform$OS.type=="windows"){source_dir <- "c:/dropbox/"}
if(.Platform$OS.type=="unix"){source_dir <- path.expand("~/Dropbox/")}
#if(Sys.info()[7]=="puruckertom"){    
#}

source_dir <- paste(source_dir,"ktp/swat_gof/",sep="")

### get calibration results
dir.best.mc.cl <- paste(source_dir,"mcCL_MPE/mcMPEd2/",sep="")
dir.best.lr.cl <- paste(source_dir,"lrCL_MPE/lrMPEd2/",sep="")
dir.best.mc.ns1 <- paste(source_dir,"mcNS1_MPE/mcMPEd3/",sep="")
dir.best.lr.ns1 <- paste(source_dir,"lrNS1_MPE/lrMPEd2/",sep="")
dir.best.mc.mns <- paste(source_dir,"mcMNSeff_MPE/mcMPEd5/",sep="")
dir.best.lr.mns <- paste(source_dir,"lrMNSeff_MPE/lrMPEd5/",sep="")
dir.best.mc.rsd <- paste(source_dir,"mcRSD_MPE/mcMPEd1/",sep="")
dir.best.lr.rsd <- paste(source_dir,"lrRSD_MPE/lrMPEd1/",sep="")

best.dirs <- vector(mode="character",length=8)
best.dirs[1] <- dir.best.mc.cl
best.dirs[2] <- dir.best.lr.cl
best.dirs[3] <- dir.best.mc.ns1
best.dirs[4] <- dir.best.lr.ns1
best.dirs[5] <- dir.best.mc.mns
best.dirs[6] <- dir.best.lr.mns
best.dirs[7] <- dir.best.mc.rsd
best.dirs[8] <- dir.best.lr.rsd

w <- 2001*8
calib.gof <- matrix(data=NA,nrow=0,ncol=25)
for(i in 1:8){
  print(file.exists(paste(best.dirs[i],"sim_gofs.csv",sep="")))
  print(dim(t(read.csv(paste(best.dirs[i],"sim_gofs.csv",sep=""),header=TRUE))))
  istart <- (i-1)*2001+1
  istop <- i*2001
  print(paste(istart,":",istop))
  calib.temp <- t(read.csv(paste(best.dirs[i],"sim_gofs.csv",sep="")))
  calib.gof <- rbind(calib.gof,calib.temp)
}
dim(calib.gof)
summary(calib.gof)
colnames(calib.gof)

### get validation results
#val_dir <- paste(source_dir,"Validations/",sep="")
dir.val.mc.cl <- paste(source_dir,"mcCL_MPEV/",sep="")
dir.val.lr.cl <- paste(source_dir,"lrCL_MPEV/",sep="")
dir.val.mc.ns1 <- paste(source_dir,"mcNS1_MPEV/",sep="")
dir.val.lr.ns1 <- paste(source_dir,"lrNS1_MPEV/",sep="")
dir.val.mc.mns <- paste(source_dir,"mcMNSEff_MPEV/",sep="")
dir.val.lr.mns <- paste(source_dir,"lrMNSEff_MPEV/",sep="")
dir.val.mc.rsd <- paste(source_dir,"mcRSD_MPEV/",sep="")
dir.val.lr.rsd <- paste(source_dir,"lrRSD_MPEV/",sep="")

val.dirs <- vector(mode="character",length=8)
val.dirs[1] <- dir.val.mc.cl
val.dirs[2] <- dir.val.lr.cl
val.dirs[3] <- dir.val.mc.ns1
val.dirs[4] <- dir.val.lr.ns1
val.dirs[5] <- dir.val.mc.mns
val.dirs[6] <- dir.val.lr.mns
val.dirs[7] <- dir.val.mc.rsd
val.dirs[8] <- dir.val.lr.rsd

w <- 2001*8
valid.gof <- matrix(data=NA,nrow=0,ncol=25)
for(i in 1:8){
  print(file.exists(paste(val.dirs[i],"sim_gofs.csv",sep="")))
  print(dim(t(read.csv(paste(val.dirs[i],"sim_gofs.csv",sep="")))))
  istart <- (i-1)*2001+1
  istop <- i*2001
  print(paste(istart,":",istop))
  valid.temp <- t(read.csv(paste(val.dirs[i],"sim_gofs.csv",sep="")))
  valid.gof <- rbind(valid.gof,valid.temp)
}


dim(valid.gof)
summary(valid.gof)
colnames(valid.gof)

#add fields
basin <- c(rep("Mountain Creek",2001),rep("Little River",2001))
basin <- rep(basin,4)
length(basin) #2001*8
method <- c(rep("CL",4002),rep("NSE",4002),rep("MNS",4002),rep("RSD",4002))
length(method)

domain <- rep("C",16008)
calib.gof <- as.data.frame(calib.gof,stringsasFactors=FALSE)
calib.gof <- cbind(calib.gof,basin,method,domain)
dim(calib.gof)
colnames(calib.gof)[26:28]
class(calib.gof$NSeff)
class(calib.gof$mNSeff)
class(calib.gof$rSD) 
class(calib.gof$basin) <- "factor"
class(calib.gof$method) <- "factor"
class(calib.gof$domain) <- "factor"

domain <- rep("V",16008)
valid.gof <- as.data.frame(valid.gof,stringsasFactors=FALSE)
valid.gof <- cbind(valid.gof,basin,method,domain)
dim(valid.gof)
colnames(valid.gof)[26:28]
class(valid.gof$NSeff)
class(valid.gof$mNSeff)
class(valid.gof$rSD)
class(valid.gof$basin) <- "factor"
class(valid.gof$method) <- "factor"
class(valid.gof$domain) <- "factor"

combined.gof <- rbind(calib.gof,valid.gof)
dim(combined.gof)
colnames(combined.gof)
summary(combined.gof)

#set.seed(1)
#z <- data.frame(x = rt(100, 1), g = rep(letters[1:4], each = 25))
#bwplot(x ~ g, z,
#         prepanel = function(x, y) {
#           bp <- boxplot(split(y, x), plot = FALSE)
#           ylim <- range(bp$stats)
#           list(ylim = ylim)
#         })

#convert
combined.gof$NSeff[which(combined.gof$NSeff<0)]=0
combined.gof$mNSeff[which(combined.gof$mNSeff<0)]=0
combined.gof$rSD <- abs(1-combined.gof$rSD)
combined.gof$rSD[which(combined.gof$rSD>1)]=1
combined.gof$rSD <- 1-combined.gof$rSD
combined.gof$cl <- (combined.gof$NSeff+combined.gof$mNSeff+combined.gof$rSD)/3
       
which.cl <- which(combined.gof$method=="CL")
which.ns <- which(combined.gof$method=="NSE")
which.mns <- which(combined.gof$method=="MNS")
which.rsd <- which(combined.gof$method=="RSD")
combined.cl <- combined.gof[which.cl,]
combined.ns1 <- combined.gof[which.ns,]
combined.mns <- combined.gof[which.mns,]
combined.rsd <- combined.gof[which.rsd,]

print.cl <-  bwplot(cl ~ domain | basin, data=combined.cl, main = "Composite Likelihood",ylim=c(0,1),ylab="")
print.ns <-  bwplot(NSeff ~ domain | basin, data=combined.ns1, main = "NSeff",ylim=c(0,1),ylab="")
print.mns <-  bwplot(mNSeff ~ domain | basin, data=combined.gof, main="mNSeff",ylim=c(0,1),ylab="")
print.rsd <-  bwplot(rSD ~ domain | basin, data=combined.gof, main="rSD",ylim=c(0,1),ylab="")

pdf(paste(source_dir,"figure_boxplotcompare.pdf",sep=""),height=6,width=8,)
  print(print.cl, split=c(1,1,2,2), more=TRUE)
  print(print.ns, split=c(2,1,2,2), more=TRUE)
  print(print.mns, split=c(1,2,2,2), more=TRUE)
  print(print.rsd, split=c(2,2,2,2), more=FALSE)
dev.off()

combined.cl$Metric <- combined.cl$cl
dim(combined.cl)
combined.ns1$Metric <- combined.ns1$NSeff
dim(combined.ns1)
combined.mns$Metric <- combined.mns$mNSeff
dim(combined.mns)
combined.rsd$Metric <- combined.rsd$rSD
dim(combined.rsd)

combined.met <- rbind(combined.rsd,combined.mns, combined.ns1, combined.cl)
combined.met <- rbind(combined.cl,combined.mns, combined.ns1, combined.rsd)
factor(combined.met$method)
#show.settings() #lattice settings
trellis.par.get("box.rectangle")
trellis.par.set (box.rectangle = modifyList (trellis.par.get ("box.rectangle"), list (col = "black", lty=1,index.cond=c(2,1))))
trellis.par.get("box.rectangle")
trellis.par.get("box.umbrella")
trellis.par.set (box.umbrella = modifyList (trellis.par.get ("box.umbrella"), list (col = "black",lty=1))) 
trellis.par.get("box.umbrella")
bwtest <- bwplot(Metric ~ domain | basin + method, data=combined.met, layout=c(4,2),fill=c("gold","steelblue3"))
plot(bwtest)

bwtest$index.cond[[2]] <- c(2,4,1,3)
pdf(paste(source_dir,"figure_boxplotcompare_better.pdf",sep=""),height=5,width=7.5)
  trellis.par.get("box.rectangle")
  trellis.par.set (box.rectangle = modifyList (trellis.par.get ("box.rectangle"), list (col = "black", lty=1)))
  trellis.par.get("box.rectangle")
  trellis.par.get("box.umbrella")
  trellis.par.set (box.umbrella = modifyList (trellis.par.get ("box.umbrella"), list (col = "black",lty=1))) 
  trellis.par.get("box.umbrella")
  bwtest <- bwplot(Metric ~ domain | basin + method, data=combined.met, pch="|",ylim=c(0,1),layout=c(4,2),fill=c("gold","steelblue3"))
  bwtest$index.cond[[1]] <- c(2,1)
  bwtest$index.cond[[2]] <- c(2,4,1,3)  
  plot(bwtest)
dev.off()

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

calib.lr.flow.observed <- read.table(paste(source_dir,"lrCL/observed.txt",sep=""),header=FALSE)
calib.lr.flow.observed <- as.vector(t(calib.lr.flow.observed[,3]))
summary(calib.lr.flow.observed)
valid.lr.flow.observed <- read.table(paste(source_dir,"lrMPEclV/observed.txt",sep=""),header=FALSE)
valid.lr.flow.observed <- as.vector(t(valid.lr.flow.observed[,3]))
summary(valid.lr.flow.observed)
calib.mc.flow.observed <- read.table(paste(source_dir,"mcCL/observed.txt",sep=""),header=FALSE)
calib.mc.flow.observed <- as.vector(t(calib.mc.flow.observed[,3]))
summary(calib.mc.flow.observed)
valid.mc.flow.observed <- read.table(paste(source_dir,"mcMPEclV/observed.txt",sep=""),header=FALSE)
valid.mc.flow.observed <- as.vector(t(valid.mc.flow.observed[,3]))
summary(valid.mc.flow.observed)

fdc.values <- read.csv(paste(source_dir,"exceed_prob_indiv.csv",sep=""),header=TRUE)
dim(fdc.values)
colnames(fdc.values)
#factors
rep("Little River",calib.Ndays)
rep("Little River",valid.Ndays)

#simulated flows and percentiles
calib.5th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
calib.50th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
calib.95th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
valid.5th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
valid.50th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
valid.95th.percentiles <- matrix(data=NA,nrow=2001,ncol=8)
for(i in 1:8){
  print(file.exists(paste(best.dirs[i],"flow_modeled.csv",sep="")))
  print(dim(read.csv(paste(best.dirs[i],"flow_modeled.csv",sep=""),header=TRUE)))
  calib.temp.percentiles <- read.csv(paste(best.dirs[i],"flow_modeled.csv",sep=""),header=TRUE)
  valid.temp.percentiles <- read.csv(paste(val.dirs[i],"flow_modeled.csv",sep=""),header=TRUE)
  dim(calib.temp.percentiles)
  for(j in 2:2002){
    calib.5th.percentiles[j-1,i] <- quantile(calib.temp.percentiles[,j],0.05)
    calib.50th.percentiles[j-1,i] <- quantile(calib.temp.percentiles[,j],0.5)
    calib.95th.percentiles[j-1,i] <- quantile(calib.temp.percentiles[,j],0.95)
    valid.5th.percentiles[j-1,i] <- quantile(valid.temp.percentiles[,j],0.05)
    valid.50th.percentiles[j-1,i] <- quantile(valid.temp.percentiles[,j],0.5)
    valid.95th.percentiles[j-1,i] <- quantile(valid.temp.percentiles[,j],0.95)
  }
}

#observed percentiles
calib.lr.obs.95 <- quantile(calib.lr.flow.observed,0.95)
calib.lr.obs.50 <- quantile(calib.lr.flow.observed,0.5)
calib.lr.obs.5 <- quantile(calib.lr.flow.observed,0.05)
calib.mc.obs.95 <- quantile(calib.mc.flow.observed,0.95)
calib.mc.obs.50 <- quantile(calib.mc.flow.observed,0.5)
calib.mc.obs.5 <- quantile(calib.mc.flow.observed,0.05)
valid.lr.obs.95 <- quantile(valid.lr.flow.observed,0.95)
valid.lr.obs.50 <- quantile(valid.lr.flow.observed,0.5)
valid.lr.obs.5 <- quantile(valid.lr.flow.observed,0.05)
valid.mc.obs.95 <- quantile(valid.mc.flow.observed,0.95)
valid.mc.obs.50 <- quantile(valid.mc.flow.observed,0.5)
valid.mc.obs.5 <- quantile(valid.mc.flow.observed,0.05)

source(paste(source_dir,"11MCFlowPercentiles.R",sep=""))

source(paste(source_dir,"11LRFlowPercentiles.R",sep=""))

