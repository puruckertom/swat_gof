library(sp)
library(RNetCDF)
library(maptools) 
library(lattice)


if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
	output_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
}

#windows
if(.Platform$OS.type=="windows"){
	root_dir <- path.expand("d://Dropbox/ktp/Manitowoc/")
	output_dir <- path.expand("d://Dropbox/ktp/Manitowoc/")
}

MPE.root <- paste(root_dir,"FiveTestDays/",sep="")

llCRS <- CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs")
llPolar <- CRS("+proj=longlat +ellps=")
#llStereo <- CRS("+proj=stereographic")
#llHRAP <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs")
llNCEP_Sphere <- CRS("+proj=stere +lat_0=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")
#gpclibPermit()

# daily simulation length
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-01-05")

Ndays <- as.numeric(enddate-startdate + 1)
MPE.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
# remove "-" and convert to text
days.text <- gsub("-","",as.Date(MPE.days, origin="1970-01-01"))
#daily.ncdf.filename <- paste("ST4_",days.text,sep="") 

hours <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
Nhours <- as.numeric(length(hours))

hourly.ncdf.filename <- list(data=NA, nrow=Ndays*Nhours)
llCounter = 1
for(i in 1:Ndays){
  for(j in 1:Nhours){
    hourly.ncdf.filename[llCounter] <- paste(MPE.root,"ST4_",days.text[i],hours[j],"_01h.nc",sep="")
    llCounter=llCounter+1
  }
}

#subs <- c("SB1","SB2","SB3","SB4","SB5","SB6","SB7","SB8","SB9","SB10","SB11","SB12","SB13","SB14","SB15","SB16","SB17","SB18","SB19")
#getinfo.shape(paste(root_dir,"SB1",sep=""))

#name <- list(data=NA, nrows=19)
#for(i in 1:19){
  #name[i] <- paste(subs[i],".poly")
#}

SB1.poly <- readShapePoly(paste(root_dir,"SB1",sep=""),proj4string=llCRS)
bbox(SB1.poly)
plot(SB1.poly)

#getinfo.shape(paste(root_dir,"conus_pts",sep=""))
TheDairyState <- readShapePoly(paste(root_dir,"WisconsinWGS84",sep=""), proj4string=llCRS)
Man.MPE <- readShapePoints(paste(root_dir,"ManitowocMPECentroids",sep=""), proj4string=llCRS)

plot(TheDairyState, col="firebrick4")
plot(Man.MPE,add=TRUE, col="gold2")
plot(SB1.poly, add=TRUE, col="darkseagreen4")

class(hourly.ncdf.filename[1])
file.exists(as.character(hourly.ncdf.filename[1]))

#N = 55*30
#long.v <- vector(mode = "numeric", length = N)
#lat.v <- vector(mode = "numeric", length = N)
#daily.v <- vector(mode = "numeric", length = N)


#SW Corner = min lat and min long of this box:
#data.ncdf = open.nc(as.character(hourly.ncdf.filename[1]))
#startingpoint <- c(1,1)
#startingpoint.d <- c(1,1,1)
#boxsize <- c(20,20)
#boxsize.d <- c(20,20,1)

#NE Corner = max lat and max long of this box:
#data.ncdf = open.nc(as.character(hourly.ncdf.filename[1]))
#startingpoint <- c(1101,861)
#startingpoint.d <- c(1101,861,1)
#boxsize <- c(20,20)
#boxsize.d <- c(20,20,1)

#SE Corner = min lat and max long of this box:
data.ncdf = open.nc(as.character(hourly.ncdf.filename[1]))
startingpoint <- c(1101,1)
startingpoint.d <- c(1101,1,1)
boxsize <- c(20,20)
boxsize.d <- c(20,20,1)

#NW Corner = min long and max lat of this box:
data.ncdf = open.nc(as.character(hourly.ncdf.filename[1]))
startingpoint <- c(1,861)
startingpoint.d <- c(1,861,1)
boxsize <- c(20,20)
boxsize.d <- c(20,20,1)

#the whole thing
data.ncdf = open.nc(as.character(hourly.ncdf.filename[1]))
data.ncdf2 = open.nc(as.character(hourly.ncdf.filename[2]))
startingpoint <- c(1,1)
startingpoint.d <- c(1,1,1)
boxsize <- c(1121,881)
boxsize.d <- c(1121,881,1)

#SB2

long = var.get.nc(data.ncdf, "longitude",start=startingpoint,count=boxsize)
lat = var.get.nc(data.ncdf, "latitude",start=startingpoint,count=boxsize)
hourly = var.get.nc(data.ncdf, "APCP_SFC",start=startingpoint.d,count=boxsize.d)

#If you want to work with R, you can use ncdf package. You will be able to extract your data thanks to the
#get.var.ncdffunction. You can plot it thanks to the sp package and spplotfunction or use the rglpackage
#(or else scatterplot).

getClass("SpatialPoints")
class(long)
Nrows <- dim(hourly)[[1]]
Nrows
Ncols <- dim(hourly)[[2]]
Ncols
N = Nrows*Ncols
long.v <- vector(mode = "numeric", length = N)
lat.v <- vector(mode = "numeric", length = N)
hourly.v <- vector(mode = "numeric", length = N)

for(i in 1:Nrows){
  for(j in 1:Ncols){
    k = (i-1)*Ncols + j
    #print(paste(k,i,j,sep=" "))
    long.v[k] = long[i,j]
    lat.v[k] = lat[i,j]
    hourly.v[k] = hourly[i,j]
  }
}

test.df <- data.frame(cbind(long.v,lat.v, hourly.v))
test.sdf <- test.df
test.mat <- cbind(long.v,lat.v)
summary(test.mat)
test.sp <- SpatialPoints(test.mat,proj4string=llNCEP_Sphere)
test.sp2 <- SpatialPoints(test.mat,proj4string=llCRS)
test.bbcoords <- bbox(test.sp)
test.bbcoords
test.bbcoords2 <- bbox(test.sp2)
test.bbcoords2

bbox(TheDairyState)

#View(TheDairyState)
#View(test.mat)

dim(data.ncdf)

plot(TheDairyState, col="firebrick4")
#plot(test.bbcoords, col="black")
plot(test.sp, col="blue")
plot(test.sp2, col="red", add=TRUE)
plot(test.bbcoords2, add=TRUE, col="red")


conus.df <- data.frame(cbind(long.v,lat.v,daily.v))
conus.sdf <- conus.df
conus.mat <- cbind(long.v,lat.v)
conus.sp <- SpatialPoints(conus.mat, proj4string=llStereo)
spplot(test.sp)

lonlat.nc <- sapply(slot(nc.poly, "polygons"),function(x) lapply(slot(x,"Polygons"), function(y) slot(y,"coords")))
lonlat.nc <- as.matrix(lonlat.nc[[1]])
lon.sp <- coordinates(conus.sp)[,1]
lat.sp <- coordinates(conus.sp)[,2]
lon.nc <- as.vector(lonlat.nc[,1])
lat.nc <- as.vector(lonlat.nc[,2])
  
lonlat.neuse <- sapply(slot(neuse.5kbuffer.poly, "polygons"),function(x) lapply(slot(x,"Polygons"), function(y) slot(y,"coords")))
lonlat.neuse <- as.matrix(lonlat.neuse[[1]])
lon.neuse <- as.vector(lonlat.neuse[,1])
lat.neuse <- as.vector(lonlat.neuse[,2])  #careful here- using lat again but original lat is only useful as lat.v
# here is the problem on March 17, 2011- Npoints is zero
Npoints <- length(which(point.in.polygon(lon.sp,lat.sp,lon.neuse,lat.neuse,mode.checked=FALSE)==1))


#plot(nc.poly, col="green")
#plot(neuse.poly,add=TRUE, col="pink")
#lines(bbox(neuse.poly,col="orange")

#names(neuse.poly)

bbox.x <- t(c(bbox(neuse.5kbuffer.poly)[1,][1],bbox(neuse.5kbuffer.poly)[1,][1],bbox(neuse.5kbuffer.poly)[1,][2],bbox(neuse.5kbuffer.poly)[1,][2]))
bbox.y <- t(c(bbox(neuse.5kbuffer.poly)[2,][1],bbox(neuse.5kbuffer.poly)[2,][2],bbox(neuse.5kbuffer.poly)[2,][2],bbox(neuse.5kbuffer.poly)[2,][1]))
#point.in.polygon(coordinates(conus.sp)[,1],  coordinates(conus.sp)[,2],t(bbox.x), t(bbox.y),mode.checked=FALSE)
#plot(conus.sp[which(point.in.polygon(coordinates(conus.sp)[,1],  coordinates(conus.sp)[,2],t(bbox.x), t(bbox.y),mode.checked=FALSE)==1)])

#plot(neuse.poly)
#plot(daily[which(point.in.polygon(coordinates(conus.sp)[,1],coordinates(conus.sp)[,2],t(bbox.x),t(bbox.y),mode.checked=FALSE)==1)],add=TRUE)
#plot(neuse.5kbuffer.poly, col="red")
#plot(neuse.nobuffer.poly, add=TRUE, col="white")
#points(conus.sp[which(point.in.polygon(coordinates(conus.sp)[,1],coordinates(conus.sp)[,2],t(bbox.x),t(bbox.y),mode.checked=FALSE)==1)])

    
missing.days.list <- vector("list", 1)

# open the daily ncdf files and extract the data
for(ii in 1:Ndays){
#for(i in 1:2){
#i=1
  print(as.Date(hourly.days[ii], origin="1970-01-01"))
  #for(i in 1:1){
  
  #data.ncdf = open.nc("E://stageiv_daily/200811161200_apcp.nc")
  #data.ncdf = open.nc("E://stageiv_daily/200811141200_apcp.nc")   # a wet day
  #data.ncdf = open.nc("E://stageiv_daily/200901071200_apcp.nc")   # a wet day

  #data.ncdf = open.nc("E://stageiv_daily/nws_precip_conus_20060905.nc")
  
  does.file.exist <- file.exists(daily.ncdf.filename[ii])
  if(does.file.exist==TRUE){
    data.ncdf = open.nc(daily.ncdf.filename[ii])
  }
  else{
    missing.days.list <- c(missing.days.list,daily.ncdf.filename[ii])
  }

  if(does.file.exist==TRUE){
    print(daily.ncdf.filename[ii])
    #data.ncdf = open.nc(daily.ncdf.filename[1121])
    #summary.ncdf(data.ncdf)
    
    #daily.subset = var.get.nc(data.ncdf, "APCP_SFC",start=c(5,5),count=c(5,5))

    #(1121,881)
    startingpoint <- c(950,440)
    startingpoint.d <- c(950,440,1)
    boxsize <- c(55,30)
    boxsize.d <- c(55,30,1)    
    #extract coordinate and data information
    long = var.get.nc(data.ncdf, "longitude",start=startingpoint,count=boxsize)
    lat = var.get.nc(data.ncdf, "latitude",start=startingpoint,count=boxsize)
    daily = var.get.nc(data.ncdf, "APCP_SFC",start=startingpoint.d,count=boxsize.d)
    #print(max(daily))
    #> dim(onehour)
    #[1] 1121  881
    #daily = var.get.nc(data.ncdf, "APCP_SFC",start=c(949,332),count=c(53,24))
    #image(daily,col=rainbow(100))
    #image(daily[949:1001,332:355],col=rainbow(100))
    
    getClass("SpatialPoints")
    class(long)
    Nrows <- dim(daily)[[1]]
    Ncols <- dim(daily)[[2]]
    N = Nrows*Ncols
    long.v <- vector(mode = "numeric", length = N)
    lat.v <- vector(mode = "numeric", length = N)
    daily.v <- vector(mode = "numeric", length = N)
    
    for(i in 1:Nrows){
      for(j in 1:Ncols){
        k = (i-1)*Ncols + j
        #print(paste(k,i,j,sep=" "))
        long.v[k] = long[i,j]
        lat.v[k] = lat[i,j]
        daily.v[k] = daily[i,j]
      }
    }
  
     #max(daily.v,na.rm=TRUE)  
    #  # conus plot
    #  conus.df <- data.frame(cbind(long.v,lat.v,daily.v))
    #  conus.sdf <- conus.df
    #  coordinates(conus.sdf) <- c("long.v","lat.v") 
    #  proj4string(conus.sdf) <- llCRS
    #  bbox(conus.sdf)
    #  class(conus.sdf)
    #  conus.poly.layout <- list("sp.polygons",conus.poly,col="red")
    #  north <- list("SpatialPolygonsRescale",layout.north.arrow(),offset=c(-77.5,36.2),scale=0.2)
    #  datetext <- list("sp.text",c(-96,46),as.Date(hourly.days[i], origin="1970-01-01"))
    #  scalebar <- list("SpatialPolygonsRescale",layout.scale.bar(),offset=c(-96,34),scale=0.5,fill=c("transparent","black"))
    #  conus.layout <- list(north,datetext,scalebar,conus.poly.layout)
    #  spplot(conus.sdf, fill=FALSE, col=rainbow(12), cex=0.5, cuts=c(0,10,20,30,40,50,60,70,80,90,100,250,500),sp.layout=conus.layout) 
    
    #    # nc plot
    #  long.in.nc <- long.v[which(point.in.polygon(lon.sp,lat.sp,lon.nc,lat.nc,mode.checked=FALSE)==1)]
    #  lat.in.nc <- lat.v[which(point.in.polygon(lon.sp,lat.sp,lon.nc,lat.nc,mode.checked=FALSE)==1)]
    #  daily.in.nc <- daily.v[which(point.in.polygon(lon.sp,lat.sp,lon.nc,lat.nc,mode.checked=FALSE)==1)]
    #  nc.df <- data.frame(cbind(long.in.nc,lat.in.nc,daily.in.nc))
    #  nc.sdf <- nc.df
    #  coordinates(nc.sdf) <- c("long.in.nc","lat.in.nc") 
    #  proj4string(nc.sdf) <- llCRS
    #  bbox(nc.sdf)
    #  class(nc.sdf)
    #  nc.poly.layout <- list("sp.polygons",nc.poly,col="red")
    #  north <- list("SpatialPolygonsRescale",layout.north.arrow(),offset=c(-77.5,36.2),scale=0.2)
    #  datetext <- list("sp.text",c(-96,46),as.Date(hourly.days[i], origin="1970-01-01"))
    #  scalebar <- list("SpatialPolygonsRescale",layout.scale.bar(),offset=c(-96,34),scale=0.5,fill=c("transparent","black"))
    #  nc.layout <- list(north,datetext,scalebar,nc.poly.layout)
    #  spplot(nc.sdf, fill=TRUE, col=rainbow(12), cex=1, cuts=c(0.254,2.54,6.25,12.5,18.75,25.4,38,50.8,63,76,102,250,500),sp.layout=nc.layout) 
    
    #length(long.v)
    #summary(long.v)
    #length(lat.v)
    #length(daily.v)
    #summary(daily.v)
      
    #plot(neuse.5kbuffer.poly, col="green")
    #plot(neuse.nobuffer.poly, add=TRUE, col="white")
    #points(conus.sp[which(point.in.polygon(lon.sp,lat.sp,lon,lat,mode.checked=FALSE)==1)])
      
    neuse.box.mat <- cbind(long.v,lat.v)
    neuse.box.sp <- SpatialPoints(neuse.box.mat, proj4string=llStereo)
    lon.box.sp <- coordinates(neuse.box.sp)[,1]
    lat.box.sp <- coordinates(neuse.box.sp)[,2]
     
    long.in.v <- long.v[which(point.in.polygon(lon.box.sp,lat.box.sp,lon.neuse,lat.neuse,mode.checked=FALSE)==1)]
    lat.in.v <- lat.v[which(point.in.polygon(lon.box.sp,lat.box.sp,lon.neuse,lat.neuse,mode.checked=FALSE)==1)]
    daily.in.v <- daily.v[which(point.in.polygon(lon.box.sp,lat.box.sp,lon.neuse,lat.neuse,mode.checked=FALSE)==1)]
  
    #length(daily.in.v)
    #print(daily.in.v)
  
    if(ii==1){     
      neuse.precip <- matrix(data = NA, nrow = Ndays, ncol = Npoints)
    }
    print(paste(ii,Npoints,length(long.in.v),length(lat.in.v),length(daily.in.v)))
    neuse.precip[ii,] <- daily.in.v
  }
  else{
    neuse.precip[ii,] <- NA
  }
  
  neuse.df <- data.frame(cbind(long.in.v,lat.in.v,daily.in.v))
  neuse.sdf <- neuse.df
  coordinates(neuse.sdf) <- c("long.in.v","lat.in.v") 
  proj4string(neuse.sdf) <- llCRS
  bbox(neuse.sdf)
  class(neuse.sdf)
  
  bb <- bbox(neuse.sdf)
  cs <- c(0.01,0.01)
  cc <- bb[,1] + (cs/2)
  cd <- ceiling(diff(t(bb))/cs)
  
  neuse.mat <- cbind(long.in.v,lat.in.v)
  neuse.sp <- SpatialPoints(neuse.mat, proj4string=llStereo)
  
  neuse.grid <- GridTopology(cellcentre.offset = cc, cellsize=cs, cells.dim=cd)
  p4s <- CRS(proj4string(neuse.sp))
  neuse.sg <- SpatialGrid(neuse.grid,proj4string=p4s)
  summary(neuse.sg)
  #neuse.sdf$daily.in.v <- runif(794)
  
  #spplot(spatialpointsdataframe,variable)
  #[1] "#FF0000FF" "#FF8000FF" "#FFFF00FF" "#80FF00FF" "#00FF00FF" "#00FF80FF"
  #[7] "#00FFFFFF" "#0080FFFF" "#0000FFFF" "#8000FFFF" "#FF00FFFF" "#FF0080FF"
  #precip.colors=c(rainbow(7),rainbow(8),rainbow(9),rainbow(4),rainbow(5),rainbow(6),rainbow(3),rainbow(2),rainbow(1),rainbow(12),rainbow(11),rainbow(10))
  #precip.colors=c("#00FFFFFF","#0080FFFF","#0000FFFF","#80FF00FF","#00FF00FF","#00FF80FF","#FFFF00FF","#FF8000FF","#FF0000FF","#FF0080FF","#FF00FFFF","#8000FFFF")
  
  #neuse.poly.layout <- list("sp.polygons",neuse.poly)
  #north <- list("SpatialPolygonsRescale",layout.north.arrow(),offset=c(-77.5,36.2),scale=0.2)
  #datetext <- list("sp.text",c(-78.3,36.3),as.Date(hourly.days[i], origin="1970-01-01"))
  #scalebar <- list("SpatialPolygonsRescale",layout.scale.bar(),offset=c(-79,35.1),scale=0.5,fill=c("transparent","black"))
  #neuse.layout <- list(neuse.poly.layout, north,datetext,scalebar)
  ##spplot(neuse.sdf, fill=TRUE, col=colorRampPalette(precip.colors), cuts=c(0,10,20,30,40,50,60,70,80,90,100,110,250)) 
  #spplot(neuse.sdf, fill=TRUE, col=rainbow(12), cuts=c(0,10,20,30,40,50,60,70,80,90,100,110,250),sp.layout=neuse.layout) 
  
  #plot(neuse.sp)
  #points(daily.in.v, col=heat.colors(12))
  #plot(neuse.sg)
  #plot(neuse.sdf)
  #lonlat <- sapply(slot(neuse.5kbuffer.poly, "polygons"),function(x) lapply(slot(x,"Polygons"), function(y) slot(y,"coords")))
  #lonlat <- as.matrix(lonlat[[1]])
  #lon.sdf <- coordinates(conus.sdf)[,1]
  #lat.sdf <- coordinates(conus.sdf)[,2]
  #lon <- as.vector(lonlat[,1])
  #lat <- as.vector(lonlat[,2])
  #conus.sdf[which(point.in.polygon(lon.sdf,lat.sdf,lon,lat,mode.checked=FALSE)==1)]
}

print(missing.days.list)


#first pass
#write.table(neuse.precip, file = paste(stageiv.root,"neuse_precip1.csv",sep=""), sep=",")
#second pass
write.table(neuse.precip, file = paste(stageiv.root,"neuse_precip2.csv",sep=""), sep=",")

# start again from Jan 1 2007 to latest day (must restart R due to ncdf library limitation)


                                  
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
##### data have been written to csv, create one large table in R to write dbfs

neuse.precip.1 <- read.csv(file = paste(stageiv.root,"neuse_precip1.csv",sep=""))
# dim(neuse.precip.1)
#[1] 1826  794
neuse.precip.2 <- read.csv(file = paste(stageiv.root,"neuse_precip2.csv",sep=""))
# dim(neuse.precip.2)
#[1] 1339  794
neuse.precip.all <- rbind(neuse.precip.1,neuse.precip.2)
# dim(neuse.precip.all)
#[1] 3165  794
# add a year of burn-in
Nrowsneuse = dim(neuse.precip.all)[1]
Ncolsneuse = dim(neuse.precip.all)[2]
Nrowsplusone = dim(neuse.precip.all)[1] + 365
Ncolsplusone = dim(neuse.precip.all)[2]
neuse.precip.plusone <- matrix(data = NA, nrow = Nrowsplusone, ncol = Ncolsplusone)
# direct assignment seems to screw up the matrix dimensions
for(i in 366:Nrowsplusone){
  for(j in 1:794){
    neuse.precip.plusone[i,j] <- neuse.precip.all[(i-365),j]
  }
}
for(i in 1:365){
  for(j in 1:794){
    neuse.precip.plusone[i,j] <- neuse.precip.all[i,j]
  }
}

#########################
##write to a bunch of dbfs
#startday <- as.numeric(as.Date("2002-01-01"))
# with burninyear
startday <- as.numeric(as.Date("2001-01-01"))
endday <- as.numeric(as.Date("2010-12-31"))
daily.days <- rep(startday:endday)
daily.days2 <- as.Date(daily.days,origin="1970-01-01")

#redefine Ndays
Ndays <- as.numeric(enddate-startdate + 1)

neuse.precip.all[is.na(neuse.precip.all)] <- -99.0
neuse.precip.plusone[is.na(neuse.precip.all)] <- -99.0

# generate daily precip for location i
for(i in 1:Ncolsplusone){
  xyu.daily <- array(data=NA, c(Ndays,2))
  swat.dataset.daily <- vector(mode = "numeric", length=Ndays)
  
  # three digit itext
  ifelse(i<=9,(itext=paste(0,0,0,0,i,sep="")),
    ifelse(i<=99,(itext=paste(0,0,0,i,sep="")),
      ifelse(i<=999,(itext=paste(0,0,i,sep="")),
        ifelse(i<=9999,(itext=paste(0,i,sep="")),
          ifelse(i>=10000,(itext=i))))))
    
  #swat.dataset.daily <- neuse.precip.all[,i]
  # with burnin year
  swat.dataset.daily <- neuse.precip.plusone[,i]
  xyu.daily <- data.frame(cbind(daily.days2,swat.dataset.daily))
  dimnames(xyu.daily)[[2]] <- c("DATE","PCP")
  class(xyu.daily[,1]) <- "Date"
  xyu.daily[,1] <- daily.days2 # doing it twice is dumb but it works
  write.dbf(xyu.daily, paste(stageiv.root,"neuse_dy_",itext,".dbf",sep=""),factor2char=TRUE)
}

#write coords
neuse.coords <- cbind(long.in.v, lat.in.v)
write.csv(neuse.coords, paste(stageiv.root,"neuse_coords.csv",sep=""))
