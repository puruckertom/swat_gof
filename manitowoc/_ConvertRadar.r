library(sp)
library(RNetCDF)
library(maptools) 
library(lattice)

llCRS <- CRS("+proj=longlat +ellps=WGS84")
llPolar <- CRS("+proj=longlat +ellps=")
llStereo <- CRS("+proj=stereographic")

# daily simulation length
#first pass (also change write csv filename)
#startdate <- as.Date("2002-01-01")
#enddate <- as.Date("2006-12-31")
#second pass (also change write csv filename)
startdate <- as.Date("2007-01-01")
enddate <- as.Date("2010-12-31")

stageiv.root <- "G://stageiv_daily/"

Ndays <- as.numeric(enddate-startdate + 1)

hourly.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
# remove "-" and convert to text
hourly.days.text <- gsub("-","",as.Date(hourly.days, origin="1970-01-01"))
daily.ncdf.filename <- paste(stageiv.root,hourly.days.text,"1200_apcp.nc",sep="")

getinfo.shape(paste(stageiv.root,"polygon_files/NFB_WGS84_Buffer",sep=""))
#neuse.500mbuffer.poly <- readShapePoly(paste(stageiv.root,"polygon_files/NFB_WGS84_Buffer",sep=""), proj4string=llCRS)
#neuse.nobuffer.poly <- readShapePoly(paste(stageiv.root,"polygon_files/Neuse_FtBarn_WGS84",sep=""), proj4string=llCRS)
neuse.5kbuffer.poly <- readShapePoly(paste(stageiv.root,"polygon_files/NFB_WGS84_5Kbuffer",sep=""), proj4string=llCRS)
#> bbox(neuse.poly)
#        min       max
#x -79.23385 -77.29679
#y  35.10421  36.40362

getinfo.shape(paste(stageiv.root,"polygon_files/CONUS_WGS84",sep=""))
conus.poly <- readShapePoly(paste(stageiv.root,"polygon_files/CONUS_WGS84",sep=""), proj4string=llCRS)
#> bbox(conus.poly)
#         min       max
#x -124.73279 -66.96927
#y   24.95638  49.37174

getinfo.shape(paste(stageiv.root,"polygon_files/NC_WGS84",sep=""))
nc.poly <- readShapePoly(paste(stageiv.root,"polygon_files/NC_WGS84",sep=""), proj4string=llCRS)

#image(conus.sdf$daily)

plot(conus.poly)
#plot(neuse.poly,add=TRUE,col="blue")
#points(conus.sp)

#plot(neuse.poly)
#points(conus.sp)

#plot(gridlines(conus.sp), axes=TRUE)
#points(conus.sp)
plot(conus.poly,add=TRUE, col="red")
#plot(nc.poly,add=TRUE, col="green")
#plot(neuse.poly,add=TRUE, col="pink")
#lines(bbox(neuse.poly,add=TRUE,col="orange")
#plot(neuse.poly)
points(conus.sp)
daily.ncdf.filename[1]

#N = 55*30
#long.v <- vector(mode = "numeric", length = N)
#lat.v <- vector(mode = "numeric", length = N)
#daily.v <- vector(mode = "numeric", length = N)

data.ncdf = open.nc(daily.ncdf.filename[1])

startingpoint <- c(950,440)
startingpoint.d <- c(950,440,1)
boxsize <- c(55,30)
boxsize.d <- c(55,30,1) 
    
long = var.get.nc(data.ncdf, "longitude",start=startingpoint,count=boxsize)
lat = var.get.nc(data.ncdf, "latitude",start=startingpoint,count=boxsize)
daily = var.get.nc(data.ncdf, "APCP_SFC",start=startingpoint.d,count=boxsize.d)

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
        
conus.df <- data.frame(cbind(long.v,lat.v,daily.v))
conus.sdf <- conus.df
#coordinates(conus.sdf) <- c("long.v","lat.v")
#bbox(conus.sdf)
#class(conus.sdf)



conus.mat <- cbind(long.v,lat.v)
conus.sp <- SpatialPoints(conus.mat, proj4string=llStereo)
#conus.sdf <- conus.df
#coordinates(conus.sdf) <- c("long.v","lat.v")
#bbox(conus.sdf)
#class(conus.sdf)

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
