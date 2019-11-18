mpe.ncdc.comp <- read.csv(paste(source_dir,'precip_comp/s_p_matrix2.csv',sep=""))
head(mpe.ncdc.comp)
dim(mpe.ncdc.comp)
class(mpe.ncdc.comp)
#View(mpe.ncdc.comp)
summary(mpe.ncdc.comp)

mpe.ncdc.comp <- as.matrix(mpe.ncdc.comp)
hist(mpe.ncdc.comp)

mc.basins <- c(3)
lr.basins <- c(3)
nc.basins <- c(1,2,3,4,6,7,8,9,13,14,15,16,18,19,20,22,23)

comp.title <- vector(mode="character",length=28)
comp.title[1] <- "NC/FB"
comp.title[2] <-"NC/FB"
comp.title[3] <-"MC/LR/NC/FB"
comp.title[4] <-"NC/FB"
comp.title[5] <-"FB"
comp.title[6] <-"NC/FB"
comp.title[7] <-"NC/FB"
comp.title[8] <-"NC/FB"
comp.title[9] <-"NC/FB"
comp.title[10] <-"FB"
comp.title[11] <-"FB"
comp.title[12] <-"FB"
comp.title[13] <-"NC/FB"
comp.title[14] <-"NC/FB"
comp.title[15] <-"NC/FB"
comp.title[16] <-"NC/FB"
comp.title[17] <-"FB"
comp.title[18] <-"NC/FB"
comp.title[19] <-"NC/FB"
comp.title[20] <-"NC/FB"
comp.title[21] <-"FB"
comp.title[22] <-"NC/FB"
comp.title[23] <-"NC/FB"
comp.title[24] <-"FB"
comp.title[25] <-"FB"
comp.title[26] <-"FB"
comp.title[27] <-"FB"
comp.title[28] <-"FB"


pdf(paste(source_dir,"compare_mpe_ncdc.pdf",sep=""),height=6,width=6)
  par(mfrow=c(2,2))
  for(i in 1:28){
    holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
    holding.points[holding.points==0]<-NA
    max.log.hp <- max(log(holding.points),na.rm=TRUE)
    print(paste(i,max.log.hp))
    log.hp <- as.data.frame(na.omit(log(holding.points)))
    colnames(log.hp) <- c("x","y")
    plot(log.hp,xlab="s",ylab="p",main=comp.title[i],
         xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
    lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
    lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
  }
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess.pdf",sep=""),height=6,width=6)
  par(mfrow=c(1,1))
  plot(7,7,xlab="s",ylab="p",main="MC/LR/NC/FB",
       xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
  for(i in 1:28){
    holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
    holding.points[holding.points==0]<-NA
    max.log.hp <- max(log(holding.points),na.rm=TRUE)
    log.hp <- as.data.frame(na.omit(log(holding.points)))
    colnames(log.hp) <- c("x","y")

    lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
    lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
  }
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess_fb.pdf",sep=""),height=6,width=6)
par(mfrow=c(1,1))
plot(7,7,xlab="s",ylab="p",main="Fort Barnwell",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in 1:28){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
}
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess_nc.pdf",sep=""),height=6,width=6)
par(mfrow=c(1,1))
plot(7,7,xlab="s",ylab="p",main="Neuse/Clayton",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in nc.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
}
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess_lr.pdf",sep=""),height=6,width=6)
par(mfrow=c(1,1))
plot(7,7,xlab="s",ylab="p",main="Little River",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in lr.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
}
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess_mc.pdf",sep=""),height=6,width=6)
par(mfrow=c(1,1))
plot(7,7,xlab="s",ylab="p",main="Mountain Creek",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in mc.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=3)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=3)
}
dev.off()

pdf(paste(source_dir,"compare_mpe_ncdc_lowess_alltogether.pdf",sep=""),height=6,width=6)
par(mfrow=c(2,2))
#mc
plot(7,7,xlab="",ylab="log(MPE) mm",main="Mountain Creek",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in mc.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=1,lty=2)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=2)
}

#lr
plot(7,7,xlab="",ylab="",main="Little River",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in lr.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=1, lty=2)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=2)
}

#nc
plot(7,7,xlab="log(NCDC) mm",ylab="log(MPE) mm",main="Neuse/Clayton",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in nc.basins){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=1,lty=2)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=2)
}

#fb
plot(7,7,xlab="log(NCDC) mm",ylab="",main="Fort Barnwell",
     xlim=c(0,max.log.hp),ylim=c(0,max.log.hp),pty="s")
for(i in 1:28){
  holding.points <- cbind(mpe.ncdc.comp[,i],mpe.ncdc.comp[,i+28])
  holding.points[holding.points==0]<-NA
  max.log.hp <- max(log(holding.points),na.rm=TRUE)
  log.hp <- as.data.frame(na.omit(log(holding.points)))
  colnames(log.hp) <- c("x","y")
  
  lines(seq(0,6,1),seq(0,6,1),col="blue",lwd=1,lty=2)
  lines(lowess(log.hp[,2]~log.hp[,1]),col="red",lwd=2)
}

dev.off()