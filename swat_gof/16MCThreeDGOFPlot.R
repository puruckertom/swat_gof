if(.Platform$OS.type=="unix"){source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")}
library(scatterplot3d)

threed.data.1 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd1/top3_gofs.csv",sep=""),header=TRUE)
threed.data.2 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd2/top3_gofs.csv",sep=""),header=TRUE)
threed.data.3 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd3/top3_gofs.csv",sep=""),header=TRUE)
threed.data.4 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd4/top3_gofs.csv",sep=""),header=TRUE)
threed.data.5 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd5/top3_gofs.csv",sep=""),header=TRUE)

threed.range.1 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd1/estimates_top3_quantiles_top3.csv",sep=""),header=TRUE)
threed.range.2 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd2/estimates_top3_quantiles_top3.csv",sep=""),header=TRUE)
threed.range.3 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd3/estimates_top3_quantiles_top3.csv",sep=""),header=TRUE)
threed.range.4 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd4/estimates_top3_quantiles_top3.csv",sep=""),header=TRUE)
threed.range.5 <- read.csv(paste(source_dir,"mcCL_MPE/mcMPEd5/estimates_top3_quantiles_top3.csv",sep=""),header=TRUE)

threed.data.1[,7]
#View(threed.range.1)
lower.1 <- threed.range.1[2,6]
upper.1 <- threed.range.1[2,10]
inplay.1 <- which(threed.data.1[,7]>=lower.1 & threed.data.1[,7]<=upper.1)
lower.2 <- threed.range.2[2,6]
upper.2 <- threed.range.2[2,10]
inplay.2 <- which(threed.data.2[,7]>=lower.2 & threed.data.2[,7]<=upper.2)
lower.3 <- threed.range.3[2,6]
upper.3 <- threed.range.3[2,10]
inplay.3 <- which(threed.data.3[,7]>=lower.3 & threed.data.3[,7]<=upper.3)
lower.4 <- threed.range.4[2,6]
upper.4 <- threed.range.4[2,10]
inplay.4 <- which(threed.data.4[,7]>=lower.4 & threed.data.4[,7]<=upper.4)
length(inplay.4)
lower.5 <- threed.range.5[2,6]
upper.5 <- threed.range.5[2,10]
inplay.5 <- which(threed.data.5[,7]>=lower.5 & threed.data.5[,7]<=upper.5)
length(inplay.5)

threed.thin = 1
#View(threed.data)
colnames(threed.data)
plot.this.1 <- cbind(threed.data.1$ns1,threed.data.1$MNSeff,threed.data.1$rSD)
plot.this.thinned.1 <- plot.this.1[seq(1,2001,threed.thin),]
plot.this.2 <- cbind(threed.data.2$ns1,threed.data.2$MNSeff,threed.data.2$rSD)
plot.this.thinned.2 <- plot.this.2[seq(1,2001,threed.thin),]
plot.this.3 <- cbind(threed.data.3$ns1,threed.data.3$MNSeff,threed.data.3$rSD)
plot.this.thinned.3 <- plot.this.3[seq(1,2001,threed.thin),]
plot.this.4 <- cbind(threed.data.4$ns1,threed.data.4$MNSeff,threed.data.4$rSD)
plot.this.thinned.4 <- plot.this.4[seq(1,2001,threed.thin),]
plot.this.5 <- cbind(threed.data.5$ns1,threed.data.5$MNSeff,threed.data.5$rSD)
plot.this.thinned.5 <- plot.this.5[seq(1,2001,threed.thin),]

summary(plot.this.1)

threedplot.angle = 55
threedscale.z = 4
threed.mar <- c(3, 2, 2, 0) + 0.1
pdf.filename <- paste(source_dir,"mcCL_MPE/3dpass_evolution.pdf",sep="")


pdf(file=pdf.filename,width=3, height=9,bg="white")
  par(mfrow=c(5,1),mgp=c(1,0,0))
  sp3d.1 <- scatterplot3d(plot.this.thinned.1, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=4, 
                pch=16, main="Pass 1    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.1)){
    if(i %in% inplay.1){
      sp3d.1$points3d(t(as.matrix(plot.this.thinned.1[i,])),col="blue", type="p", pch=16,cex=0.7)
    }else{
      sp3d.1$points3d(t(as.matrix(plot.this.thinned.1[i,])),col="red", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.2 <- scatterplot3d(plot.this.thinned.2, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 2    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.2)){
    if(i %in% inplay.2){
      sp3d.2$points3d(t(as.matrix(plot.this.thinned.2[i,])),col="blue", type="p", pch=16,cex=0.7)
    }else{
      sp3d.2$points3d(t(as.matrix(plot.this.thinned.2[i,])),col="red", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.3 <- scatterplot3d(plot.this.thinned.3, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 3    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.3)){
    if(i %in% inplay.3){
      sp3d.3$points3d(t(as.matrix(plot.this.thinned.3[i,])),col="blue", type="p", pch=16,cex=0.7)
    }else{
      sp3d.3$points3d(t(as.matrix(plot.this.thinned.3[i,])),col="red", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.4 <- scatterplot3d(plot.this.thinned.4, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 4    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.4)){
    if(i %in% inplay.4){
      sp3d.4$points3d(t(as.matrix(plot.this.thinned.4[i,])),col="blue", type="p", pch=16,cex=0.7)
    }else{
      sp3d.4$points3d(t(as.matrix(plot.this.thinned.4[i,])),col="red", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.5 <- scatterplot3d(plot.this.thinned.5, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 5    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                cex.lab=0.9,zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.5)){
    if(i %in% inplay.5){
      sp3d.5$points3d(t(as.matrix(plot.this.thinned.5[i,])),col="blue", type="p", pch=16,cex=0.7)
    }else{
      sp3d.5$points3d(t(as.matrix(plot.this.thinned.5[i,])),col="red", type="p", pch=16,cex=0.7)    
    }
  }
  #par(oma=c(1.5,0,0,0)) # 1.5 line2 at the bottom
  #shortname <- pdf.filename # or maybe a filename
  #mtext(paste(shortname, " ",format(Sys.time(), "%Y-%m-%d %H:%M")),cex=0.4, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()


pdf.filename <- paste(source_dir,"mcCL_MPE/3dpass_evolution_greyscale.pdf",sep="")
pdf(file=pdf.filename,width=3, height=9,bg="white")
  par(mfrow=c(5,1),mgp=c(1,0,0))
  sp3d.1 <- scatterplot3d(plot.this.thinned.1, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 1    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.1)){
    if(i %in% inplay.1){
      sp3d.1$points3d(t(as.matrix(plot.this.thinned.1[i,])),col="gray21", type="p", pch=16,cex=0.7)
    }else{
      sp3d.1$points3d(t(as.matrix(plot.this.thinned.1[i,])),col="gray71", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.2 <- scatterplot3d(plot.this.thinned.2, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 2    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.2)){
    if(i %in% inplay.2){
      sp3d.2$points3d(t(as.matrix(plot.this.thinned.2[i,])),col="gray21", type="p", pch=16,cex=0.7)
    }else{
      sp3d.2$points3d(t(as.matrix(plot.this.thinned.2[i,])),col="gray71", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.3 <- scatterplot3d(plot.this.thinned.3, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 3    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.3)){
    if(i %in% inplay.3){
      sp3d.3$points3d(t(as.matrix(plot.this.thinned.3[i,])),col="gray21", type="p", pch=16,cex=0.7)
    }else{
      sp3d.3$points3d(t(as.matrix(plot.this.thinned.3[i,])),col="gray71", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.4 <- scatterplot3d(plot.this.thinned.4, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 4    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.4)){
    if(i %in% inplay.4){
      sp3d.4$points3d(t(as.matrix(plot.this.thinned.4[i,])),col="gray21", type="p", pch=16,cex=0.7)
    }else{
      sp3d.4$points3d(t(as.matrix(plot.this.thinned.4[i,])),col="gray71", type="p", pch=16,cex=0.7)    
    }
  }
  sp3d.5 <- scatterplot3d(plot.this.thinned.5, type="h",highlight.3d=FALSE, angle=threedplot.angle, scale.z=threedscale.z, 
                pch=16, main="Pass 5    ",xlab="",ylab="",mar= threed.mar,col.axis="gray31",col.grid="lightblue",
                cex.lab=0.9,zlab="",label.tick.marks = FALSE,color="cornsilk4")
                mtext("NSE", SOUTH<-1, line=0.3, adj=0.33,cex=0.8)
                mtext("RSD",  WEST <-2, at=3, line=-0.1, cex=0.8, adj=.43)
                mtext("MNS", EAST<-4, at=2, line=-3.5, cex=0.8)
  for(i in 1:nrow(plot.this.thinned.5)){
    if(i %in% inplay.5){
      sp3d.5$points3d(t(as.matrix(plot.this.thinned.5[i,])),col="gray21", type="p", pch=16,cex=0.7)
    }else{
      sp3d.5$points3d(t(as.matrix(plot.this.thinned.5[i,])),col="gray71", type="p", pch=16,cex=0.7)    
    }
  }
  #par(oma=c(1.5,0,0,0)) # 1.5 line2 at the bottom
  #shortname <- pdf.filename # or maybe a filename
  #mtext(paste(shortname, " ",format(Sys.time(), "%Y-%m-%d %H:%M")),cex=0.4, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()
