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

#graphics directory
graphics.dir <- source_dir

#check to see that they all exist
for(i in 1:Npasses){
  pr int(file.exists(paste(source_dir,sim.dir[i],sep="")))
}

#construct data frame with composite likelihood index values
calib.gof <- matrix(data=NA,nrow=0,ncol=25)
if(boolConvertToMonthly==FALSE){
  for(i in 1:16){
    print(file.exists(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep="")))
    print(dim(t(read.csv(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep=""),header=TRUE))))
    istart <- (i-1)*2001+1
    istop <- i*2001
    print(paste(istart,":",istop))
    calib.temp <- t(read.csv(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep="")))
    calib.gof <- rbind(calib.gof,calib.temp)
  }
}else{
  print(file.exists(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep="")))
  print(dim(t(read.csv(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep=""),header=TRUE))))
  istart <- (i-1)*2001+1
  istop <- i*2001
  print(paste(istart,":",istop))
  calib.temp <- t(read.csv(paste(source_dir,sim.dir[i],"sim_gofs.csv",sep="")))
  calib.gof <- rbind(calib.gof,calib.temp)
}  

dim(calib.gof)
summary(calib.gof)
colnames(calib.gof)
rownames(calib.gof)

#add fields
basin <- c(rep("MC",8004),rep("LR",8004),rep("NC",8004),rep("FB",8004))
#basin <- rep(basin,4)
length(basin) #2001*8
method <- c(rep("NCDC",4002),rep("MPE",4002))
method <- rep(method,4)
length(method)
domain <- c(rep("C",2001),rep("V",2001))
domain <- rep(domain,8)
length(domain)

rownames(calib.gof)
rownames(domain)
rownames(basin)
rownames(method)
dim(calib.gof)
calib.gof <- as.data.frame(calib.gof,stringsasFactors=FALSE)
rownames(calib.gof)
calib.gof <- cbind(calib.gof,basin,method,domain)
colnames(calib.gof)
summary(calib.gof)

#convert
calib.gof$NSeff[which(calib.gof$NSeff<0)]=0
calib.gof$mNSeff[which(calib.gof$mNSeff<0)]=0
calib.gof$rSD <- abs(1-calib.gof$rSD)
calib.gof$rSD[which(calib.gof$rSD>1)]=1
calib.gof$rSD <- 1-calib.gof$rSD
calib.gof$cl <- (calib.gof$NSeff+calib.gof$mNSeff+calib.gof$rSD)/3
dim(calib.gof)

if(boolConvertToMonthly==FALSE){
  pdf(paste(source_dir,"figure_precipcomparison.pdf",sep=""),height=10,width=6)
}else{
  pdf(paste(source_dir,"figure_precipcomparison_monthly.pdf",sep=""),height=10,width=6)
}
  trellis.par.get("box.rectangle")
  trellis.par.set (box.rectangle = modifyList (trellis.par.get ("box.rectangle"), list (col = "black", lty=1)))
  trellis.par.get("box.rectangle")
  trellis.par.get("box.umbrella")
  trellis.par.set (box.umbrella = modifyList (trellis.par.get ("box.umbrella"), list (col = "black",lty=1))) 
  trellis.par.get("box.umbrella")
  bwtest <- bwplot(cl ~ domain | method + basin, data=calib.gof, pch="|",layout=c(4,2),
                   fill=c("gold","steelblue3"),ylim=c(0,0.8),ylab="Composite Likelihood")
  bwtest$index.cond[[1]] <- c(2,1)
  bwtest$index.cond[[2]] <- c(4,1,3,2)  
  plot(bwtest)
dev.off()
