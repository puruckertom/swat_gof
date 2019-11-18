library(zoo)
library(lattice)

# get observed daily flow data
#Little River
ncdc.dir <- paste(source_dir,"LR_ncdc2/",sep="")
mpe.dir <- paste(source_dir,"LR_mpe2/",sep="")
prefix="lr"
text.title="Little River"
#Mountain Creek
ncdc.dir <- paste(source_dir,"MC_ncdc2/",sep="")
mpe.dir <- paste(source_dir,"MC_mpe2/",sep="")
prefix="mc"
text.title="Mountain Creek"
#Neuse/Clayton
ncdc.dir <- paste(source_dir,"NC_ncdc/",sep="")
mpe.dir <- paste(source_dir,"NC_mpe/",sep="")
prefix="nc"
text.title="Neuse"

###
flow.observed <- read.table(paste(ncdc.dir,"observed.txt",sep=""),header=FALSE)
summary(flow.observed)

mpe.quantiles <- read.csv(paste(mpe.dir,"quantiles_unweighted_out.csv",sep=""),header=TRUE)
summary(mpe.quantiles)
mpe.quantiles.zoo <- as.zoo(zoo(mpe.quantiles,x.Dates))

ncdc.quantiles <- read.csv(paste(ncdc.dir,"quantiles_unweighted_out.csv",sep=""),header=TRUE)
summary(ncdc.quantiles)
ncdc.quantiles.zoo <- as.zoo(zoo(ncdc.quantiles,x.Dates))

colnames(ncdc.quantiles.zoo)

#standard deviations without observed
#one standard deviation
plot.me <- cbind(mpe.quantiles$X0.16,mpe.quantiles$X0.5,mpe.quantiles$X0.84,ncdc.quantiles$X0.16,ncdc.quantiles$X0.5,ncdc.quantiles$X0.84)
text.percentiles <- list(c("0.16","0.5","0.84","0.16","0.5","0.84"))
suffix = "1sd"
#two standard deviations
plot.me <- cbind(mpe.quantiles$X0.05,mpe.quantiles$X0.5,mpe.quantiles$X0.95,ncdc.quantiles$X0.05,ncdc.quantiles$X0.5,ncdc.quantiles$X0.95)
text.percentiles <- list(c("0.05","0.5","0.95","0.05","0.5","0.95"))
suffix = "2sd"
#three standard deviations
plot.me <- cbind(mpe.quantiles$X0.003,mpe.quantiles$X0.5,mpe.quantiles$X0.997,ncdc.quantiles$X0.003,ncdc.quantiles$X0.5,ncdc.quantiles$X0.997)
text.percentiles <- list(c("0.003","0.5","0.997","0.003","0.5","0.997"))
suffix = "3sd"

####
plot.me.ts <- ts(plot.me, frequency = 365, start = c(2002, 1))
min.y = min(plot.me)
max.y = max(plot.me)
color.list = c("firebrick2","firebrick4","firebrick2","deepskyblue2","deepskyblue4","deepskyblue2")
key.custom <- list(text=text.percentiles,lines=list(col=color.list),space="right")
png.filename <- paste(root_dir,"gwrc2011/extra_images/",prefix,"_quantiles_noobs_",suffix,".png",sep="")
png(file=png.filename,width=9, height=7, units="in", bg="white", res=300)
 xyplot(plot.me.ts,ylim=c(min.y,max.y),log="y",col=color.list,
  scales=list(y=list(log=10)),key=key.custom,strip = TRUE,layout=c(1,2),
  ylab="Flow (cms)",screens = c(rep(paste("MPE:",text.title), 3),rep(paste("NCDC:",text.title),3)),superpose=TRUE)
dev.off()

#standard deviations with observed
flow.observed.v <- flow.observed[,3]
flow.observed.v[which(flow.observed.v==0)]=NA
#one standard deviation
plot.me <- cbind(mpe.quantiles$X0.16,mpe.quantiles$X0.5,mpe.quantiles$X0.84,flow.observed.v,ncdc.quantiles$X0.16,ncdc.quantiles$X0.5,ncdc.quantiles$X0.84,flow.observed.v)
text.percentiles <- list(c("0.16","0.5","0.84","Observed","0.16","0.5","0.84","Observed"))
suffix="1sd"
#two standard deviations
plot.me <- cbind(mpe.quantiles$X0.05,mpe.quantiles$X0.5,mpe.quantiles$X0.95,flow.observed.v,ncdc.quantiles$X0.05,ncdc.quantiles$X0.5,ncdc.quantiles$X0.95,flow.observed.v)
text.percentiles <- list(c("0.05","0.5","0.95","Observed","0.05","0.5","0.95","Observed"))
suffix="2sd"
#three standard deviations
plot.me <- cbind(mpe.quantiles$X0.003,mpe.quantiles$X0.5,mpe.quantiles$X0.997,flow.observed.v,ncdc.quantiles$X0.003,ncdc.quantiles$X0.5,ncdc.quantiles$X0.997,flow.observed.v)
text.percentiles <- list(c("0.003","0.5","0.997","Observed","0.003","0.5","0.997","Observed"))
suffix="3sd"
#
plot.me.ts <- ts(plot.me, frequency = 365, start = c(2002, 1))
colnames(plot.me.ts) <- c("a","b","c","d","e","f","g","h")
min.y = min(plot.me)
max.y = max(plot.me)
color.list = c("firebrick2","firebrick4","firebrick2","gray25","deepskyblue2","deepskyblue4","deepskyblue2","gray25")
key.custom <- list(text=text.percentiles,lines=list(col=color.list),space="right")
png.filename <- paste(root_dir,"gwrc2011/extra_images/",prefix,"_quantiles_wobs_",suffix,".png",sep="")
png(file=png.filename,width=9, height=7, units="in",bg="white",res=300)
  xyplot(plot.me.ts,ylim=c(min.y,max.y),log="y",col=color.list,
    scales=list(y=list(log=10)),key=key.custom,strip = TRUE,layout=c(1,2),
    ylab="Flow (cms)",screens = c(rep(paste("MPE:",text.title), 4),rep(paste("NCDC:",text.title),4)),superpose=TRUE)
dev.off()

# compute ppu type goodness of fit for different sd ranges
flow.observed.ppu <- flow.observed[,3]
ppu.ncdc.ranges <- array(data=FALSE, dim=c(Ndays,3))
ppu.mpe.ranges <- array(data=FALSE, dim=c(Ndays,3))
for(i in 1:Ndays){
  if(flow.observed.ppu[i]<= ncdc.quantiles$X0.84[i] && flow.observed.ppu[i] >= ncdc.quantiles$X0.16){
    ppu.ncdc.ranges[i,1] = TRUE
  }
  if(flow.observed.ppu[i]<= ncdc.quantiles$X0.95[i] && flow.observed.ppu[i] >= ncdc.quantiles$X0.05){
    ppu.ncdc.ranges[i,2] = TRUE
  }
  if(flow.observed.ppu[i]<= ncdc.quantiles$X0.997[i] && flow.observed.ppu[i] >= ncdc.quantiles$X0.003){
    ppu.ncdc.ranges[i,3] = TRUE
  }
  if(flow.observed.ppu[i]<= mpe.quantiles$X0.84[i] && flow.observed.ppu[i] >= mpe.quantiles$X0.16){
    ppu.mpe.ranges[i,1] = TRUE
  }
  if(flow.observed.ppu[i]<= mpe.quantiles$X0.95[i] && flow.observed.ppu[i] >= mpe.quantiles$X0.05){
    ppu.mpe.ranges[i,2] = TRUE
  }
  if(flow.observed.ppu[i]<= mpe.quantiles$X0.997[i] && flow.observed.ppu[i] >= mpe.quantiles$X0.003){
    ppu.mpe.ranges[i,3] = TRUE
  }
}
ncdc.ppu.summary <- array(data=NA, dim=c(1,3))
colnames(ncdc.ppu.summary) <- c("1sd","2sd","3sd")
ncdc.ppu.summary[1,1] <- sum(ppu.ncdc.ranges[,1])/Ndays
ncdc.ppu.summary[1,2] <- sum(ppu.ncdc.ranges[,2])/Ndays
ncdc.ppu.summary[1,3] <- sum(ppu.ncdc.ranges[,3])/Ndays
file.ncdc.ppu <- paste(ncdc.dir,prefix,".ppu.summary.csv",sep="")
write.csv(ncdc.ppu.summary,file=file.ncdc.ppu)

mpe.ppu.summary <- array(data=NA, dim=c(1,3))
colnames(mpe.ppu.summary) <- c("1sd","2sd","3sd")
mpe.ppu.summary[1,1] <-sum(ppu.mpe.ranges[,1])/Ndays
mpe.ppu.summary[1,2] <-sum(ppu.mpe.ranges[,2])/Ndays
mpe.ppu.summary[1,3] <-sum(ppu.mpe.ranges[,3])/Ndays
file.mpe.ppu <-paste(mpe.dir,prefix,".ppu.summary.csv",sep="")
write.csv(mpe.ppu.summary,file=file.mpe.ppu)

mpe.median.gof <- gof(mpe.quantiles$X0.5,flow.observed.ppu)
ncdc.median.gof <- gof(ncdc.quantiles$X0.5,flow.observed.ppu)
median.gof<- cbind(ncdc.median.gof,mpe.median.gof)
colnames(median.gof) <- c("ncdc","mpe")
median.gof
file.ncdc.median.gof <-paste(ncdc.dir,prefix,".median.gof.csv",sep="")
write.csv(median.gof,file=file.ncdc.median.gof)
file.mpe.median.gof <-paste(mpe.dir,prefix,".median.gof.csv",sep="")
write.csv(median.gof,file=file.mpe.median.gof)

#xyplot(plot.me.ts,layout=c(4,2),superpose=TRUE)
#############################################################
min.y = min(c(mpe.quantiles$X0.003,mpe.quantiles$X0.5,mpe.quantiles$X0.997,ncdc.quantiles$X0.003,ncdc.quantiles$X0.5,ncdc.quantiles$X0.997))
max.y = max(c(mpe.quantiles$X0.003,mpe.quantiles$X0.5,mpe.quantiles$X0.997,ncdc.quantiles$X0.003,ncdc.quantiles$X0.5,ncdc.quantiles$X0.997))
par(mfrow=c(2,1))
plot(mpe.quantiles.zoo$X0.5,col="firebrick4",log="y",ylim=c(min.y,max.y))
lines(mpe.quantiles.zoo$X0.16,col="firebrick2")
lines(mpe.quantiles.zoo$X0.84,col="firebrick2")
plot(ncdc.quantiles.zoo$X0.5,col="deepskyblue4",log="y",ylim=c(min.y,max.y))
lines(ncdc.quantiles.zoo$X0.16,col="deepskyblue2")
lines(ncdc.quantiles.zoo$X0.84,col="deepskyblue2")
#lines(obsflow.zoo,col="gray27")

#two standard deviations without observed
plot(mpe.quantiles.zoo$X0.5,col="firebrick4",log="y",ylim=c(min.y,max.y))
lines(mpe.quantiles.zoo$X0.05,col="firebrick2")
lines(mpe.quantiles.zoo$X0.95,col="firebrick2")
lines(ncdc.quantiles.zoo$X0.50,col="deepskyblue4")
lines(ncdc.quantiles.zoo$X0.05,col="deepskyblue2")
lines(ncdc.quantiles.zoo$X0.95,col="deepskyblue2")
#lines(obsflow.zoo,col="gray27")

# length(obsflow.zoo)
# sim.flow.quantiles.zoo <- as.zoo(zoo(sim.flow.quantiles,x.Dates))

# sim.flow.quantiles.ts <- ts((sim.flow.quantiles),x.Dates)
# class(sim.flow.quantiles.ts)
# warnings()
# 
# sim.flow.quantiles.zoo[,7]
# 
# 
# 
# colnames(sim.flow.quantiles.zoo)
# min.y = min(c(sim.flow.quantiles[,7],sim.flow.quantiles[,3],sim.flow.quantiles[,11]))
# max.y = max(c(sim.flow.quantiles[,7],sim.flow.quantiles[,3],sim.flow.quantiles[,11]))
# plot(sim.flow.quantiles.zoo[,7],col="firebrick4",log="y",ylim=c(min.y,max.y))
# #lines(sim.flow.quantiles.zoo[,5],col="firebrick3")
# #lines(sim.flow.quantiles.zoo[,9],col="firebrick3")
# lines(sim.flow.quantiles.zoo[,3],col="firebrick2")
# lines(sim.flow.quantiles.zoo[,11],col="firebrick2")
# #lines(sim.flow.quantiles.zoo[,1],col="firebrick1")
# #lines(sim.flow.quantiles.zoo[,13],col="firebrick1")
# lines(obsflow.zoo,col="gray27")
# 
# 
# 
# plot(sim.log.flow.quantiles.zoo[,6],ylim=c(min(sim.log.flow.quantiles.zoo),max(sim.log.flow.quantiles.zoo)))
# lines(sim.log.flow.quantiles.zoo[,4],col="red")
# lines(sim.log.flow.quantiles.zoo[,8],col="red")
# lines(sim.log.flow.quantiles.zoo[,2],col="blue")
# lines(sim.log.flow.quantiles.zoo[,10],col="blue")
# lines(sim.log.flow.quantiles.zoo[,2],col="darkgreen")
# lines(sim.log.flow.quantiles.zoo[,10],col="darkgreen")
# 
# sim.log.flow.quantiles.zoo[[1]]
# plot(sim.log.flow.quantiles.zoo[,7],ylim=c(min(sim.log.flow.quantiles.zoo),max(sim.log.flow.quantiles.zoo)),col="black")
# polygon(c(index(sim.log.flow.quantiles.zoo), rev(index(sim.log.flow.quantiles.zoo))), c(sim.log.flow.quantiles.zoo[,13], rev(sim.log.flow.quantiles.zoo[,1])),col = "grey30", border = NA)
