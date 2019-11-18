
#############
# set directories with output that are to be compared
source_dir <- "//Users/katieprice/Dropbox/ktp/swat_gof/"
location <- "MountainCreek"
firstID <- "statsgoNCDCpass1d"
first_target_dir <- "statsgo_1/mcNCDCd/" # will be fire red
secondID <- "ssurgoNCDCpass1d"
second_target_dir <- "ssurgo_1/mcNCDCd/" # will be sky blue
#############

write_dir <- paste(source_dir,"gof_compare/",sep="")
first_dir <- paste(source_dir,first_target_dir,sep="")
second_dir <- paste(source_dir,second_target_dir,sep="")

first_read <- paste(first_dir,"sim_gofs.csv",sep="")
second_read <- paste(second_dir,"sim_gofs.csv",sep="")

first_gofs <- as.matrix(read.table(first_read,sep=",",row.names=1,header=TRUE))
second_gofs <- as.matrix(read.table(second_read,sep=",",row.names=1,header=TRUE))

rownames(first_gofs)
rownames(second_gofs)
gof.names <- rownames(first_gofs)

# print out gof distributions
ngofs <- length(rownames(first_gofs))
gof.filename <- paste(write_dir,location,"_",firstID,"_",secondID,"_gof_all.pdf",sep="")
pdf(file=gof.filename,width=7.5, height=10.5, bg="white")
par(mfrow=c(3,3))
for(i in 1:ngofs){
  min.x <- min(first_gofs[i,],second_gofs[i,])
  max.x <- max(first_gofs[i,],second_gofs[i,])
  hist1 <- hist(first_gofs[i,],plot=FALSE,main=gof.names[i],freq=FALSE,xlab="",breaks=seq(min.x, max.x, length=40), include.lowest=TRUE,col="indianred3")
  hist2 <- hist(second_gofs[i,],plot=FALSE,main=gof.names[i],freq=FALSE,xlab="",breaks=seq(min.x, max.x, length=40), include.lowest=TRUE,col="#0000ff22",add=TRUE)
  max.y <- max(hist1$density,hist2$density)
  if(i==2 | i==11 | i==20){
    main.text <- gof.names[i]
    mtext(paste(location,firstID,"(red) v.",secondID,"(blue)"),side=1,line=3,at=0.9)
  }
  else{
    main.text=gof.names[i]
  }
  hist(first_gofs[i,],main=main.text,freq=FALSE,xlab="",breaks=seq(min.x, max.x, length=40),ylim=c(0,max.y),include.lowest=TRUE,col=rgb(1,0,0,0.5))
  hist(second_gofs[i,],main=main.text,freq=FALSE,xlab="",breaks=seq(min.x, max.x, length=40),ylim=c(0,max.y),include.lowest=TRUE,col=rgb(0,0,1,0.5),add=TRUE)
}
dev.off()


# compare flow to simulated flows- histogram
flow.observed <- read.table(paste(first_dir,"observed.txt",sep=""),header=FALSE)
flow.observed <- as.vector(t(flow.observed[,3]))
hist(log(flow.observed),col="indianred3")
flow.modeled <- read.csv(paste(first_dir,"flow_modeled.csv",sep=""),row.names=1)
hist(log(flow.modeled[,1]),col="#0000ff22",add=TRUE)
#

