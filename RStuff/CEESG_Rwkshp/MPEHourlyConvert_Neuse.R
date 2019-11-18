library(sp)
#library(foreign) #for dbfs
library(proj4) #for reprojection - not sure this is necessary
library(rgdal) #for coordinate projection of sp files and data export
library(ncdf)
library(RNetCDF)
library(rgeos) #for geometry ops
library(maptools)
#library(PBSmapping) #for GIS-like geoprocessing ops

### proj4 strings
HRAP <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs") #HRAP SR-ORG:4694
LL.WGS84 <- CRS("+proj=longlat +ellps=WGS84")
UTM83_18 <- CRS("+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
UTM83_17 <- CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
  output_dir <- path.expand("~/Dropbox/ktp/Manitowoc/TestOutput/")
}

#windows
if(.Platform$OS.type=="windows"){
  root_dir <- path.expand("d://Dropbox/ktp/ScalePaper/NeuseNestMPE/Shapefiles/")
  output_dir <- path.expand("d://Dropbox/ktp/ScalePaper/NeuseNestMPE/")
}

MPE.root <- path.expand("g://HourlyNetCDF/")

##On Tim:
#root_dir <- path.expand("e://Manitowoc/")
#output_dir <- path.expand("e://Manitowoc/output/")
#MPE.root <- path.expand("e://HourlyNetCDF/")

################################
#1. Project and buffer shapefile 
################################
##Read in watershed boundary shapefile(s) in original projection
##skip to bottom to read in HRAP
MC_unbuffered <- readShapePoly(paste(root_dir,"MtnCreek_UTM83",sep=""),proj4string=UTM83_17)
LR_unbuffered <- readShapePoly(paste(root_dir,"LittleRiv_UTM83",sep=""),proj4string=UTM83_17)
NC_unbuffered <- readShapePoly(paste(root_dir,"Neuse_Clayton_UTM83",sep=""),proj4string=UTM83_17)
FB_unbuffered <- readShapePoly(paste(root_dir,"Neuse_FtBarn_UTM83",sep=""),proj4string=UTM83_17)
plot(FB_unbuffered, col="darkolivegreen")
plot(NC_unbuffered, col="darkolivegreen3", add=T)
plot(LR_unbuffered, col="darkolivegreen1", add=T)
plot(MC_unbuffered, col="white", add=T)

##Reproject watershed boundary shapefile(s) to HRAP
MC.ub.HRAP <- spTransform(MC_unbuffered,HRAP)
LR.ub.HRAP <- spTransform(LR_unbuffered,HRAP)
NC.ub.HRAP <- spTransform(NC_unbuffered,HRAP)
FB.ub.HRAP <- spTransform(FB_unbuffered,HRAP)

##Buffer polygon 4km to catch outside centroids (HRAP linear unit = meters)
MC <- gBuffer(MC.ub.HRAP, byid=FALSE, id=NULL, width=4000)
  proj4string(MC) <- HRAP
LR <- gBuffer(LR.ub.HRAP, byid=FALSE, id=NULL, width=4000)
  proj4string(LR) <- HRAP
NC <- gBuffer(NC.ub.HRAP, byid=FALSE, id=NULL, width=4000)
  proj4string(NC) <- HRAP
FB <- gBuffer(FB.ub.HRAP, byid=FALSE, id=NULL, width=4000)
  proj4string(FB) <- HRAP
plot(FB, col="darkolivegreen")
plot(NC, col="darkolivegreen3", add=T)
plot(LR, col="darkolivegreen1", add=T)
plot(MC, col="white", add=T)

#Stuff below is need to coerce buffered ws to SPDF for export as .shp, isn't necessary for rain extraction
data <- data.frame(f=99.9, row.names="buffer")
MC.SPDF <- SpatialPolygonsDataFrame(MC,data)
LR.SPDF <- SpatialPolygonsDataFrame(LR,data)
NC.SPDF <- SpatialPolygonsDataFrame(NC,data)
FB.SPDF <- SpatialPolygonsDataFrame(FB,data)
writeOGR(MC.SPDF, paste(output_dir,"Shapefiles",sep=""), "MC_HRAP_Buffer", driver="ESRI Shapefile")
writeOGR(LR.SPDF, paste(output_dir,"Shapefiles",sep=""), "LR_HRAP_Buffer", driver="ESRI Shapefile")
writeOGR(NC.SPDF, paste(output_dir,"Shapefiles",sep=""), "NC_HRAP_Buffer", driver="ESRI Shapefile")
writeOGR(FB.SPDF, paste(output_dir,"Shapefiles",sep=""), "FB_HRAP_Buffer", driver="ESRI Shapefile")

##once the writeOGR is done, can just load projected and buffered shp:
MC <- readShapePoly(paste(root_dir,"MC_HRAP_Buffer",sep=""),proj4string=HRAP)
LR <- readShapePoly(paste(root_dir,"LR_HRAP_Buffer",sep=""),proj4string=HRAP)
NC <- readShapePoly(paste(root_dir,"NC_HRAP_Buffer",sep=""),proj4string=HRAP)
FB <- readShapePoly(paste(root_dir,"FB_HRAP_Buffer",sep=""),proj4string=HRAP)

################################
#2. set up coords in SPDF, ID radar pixels inside ws
################################
##Read in Lat/Lon layers of netcdf file, each is a sep grid layer
GetCoords <- open.ncdf(paste(MPE.root,"ST4_2009010100_01h.nc",sep="")) #this can be any one of the MPE nc files
print(GetCoords)

LatMatrix <- get.var.ncdf(GetCoords, "latitude") 
LonMatrix <- get.var.ncdf(GetCoords, "longitude")
Lat <- NULL
  for(i in 1:881){
      Lat <- c(Lat,LatMatrix[,i])
}
Lon <- NULL
  for(i in 1:881){
      Lon <- c(Lon,LonMatrix[,i])
}
                       
xy <- cbind(Lon,Lat)
dim(xy)
xy[1,]
Conus.WGS84 <- SpatialPoints(xy)
proj4string(Conus.WGS84) <- LL.WGS84
ConusPts <- spTransform(Conus.WGS84,HRAP)
summary(ConusPts)

plot(FB, col="darkolivegreen")
plot(NC, col="darkolivegreen3", add=T)
plot(LR, col="darkolivegreen1", add=T)
plot(MC, col="white", add=T)
plot(ConusPts,col="orange", add=T)

##_____________________________PICK UP HERE 6/6_____________________________##

NcIDy <- rep(1:881,each=1121)#NWS ID coord system in CONUS files
NcIDx <- rep(1:1121,881) 
ExtractID <- seq(1:987601)
ID <- as.data.frame(cbind(ExtractID,xy,NcIDx,NcIDy))
ID.SPDF <- SpatialPointsDataFrame(ConusPts,ID)

##To id pixels within watershed
WSCentroids <- over(Manitowoc,ConusPts,returnList=T) #'over' doesn't work with SPDF - just use SP
WSCentroids <- as.data.frame(WSCentroids)
##Write csv of centroids
#write.csv(WSCentroids, file=paste(root_dir,"TestOutput/ManitowocCentroids.csv",sep=""))

##with other information
WSIDs <- over(Manitowoc, ID.SPDF, returnList=T)
WSIDs.DF <- as.data.frame(WSIDs)
WSIDs.DF <- WSIDs.DF[,-1]
write.csv(WSIDs.DF,file=paste(output_dir,"CoordInfo.csv",sep=""))

###To write buffered Manitowoc points to shapefile
dim(WSIDs.DF)
WSxy <- ConusPts[Manitowoc,]
WSIDs.SPDF <- SpatialPointsDataFrame(WSxy,WSIDs.DF)
summary(WSIDs.SPDF)
writeOGR(WSIDs.SPDF, paste(root_dir,"TestOutput",sep=""),"ManitowocPtsR",driver="ESRI Shapefile")

###To write *all* CONUS points to shapefile:
ConusSPDF <- SpatialPointsDataFrame(ConusPts,ID) 
summary(ConusSPDF)
writeOGR(ConusSPDF, paste(root_dir,"ConusPts",sep=""), "ConusPtsR", driver="ESRI Shapefile")

################################
##3. Create SPDF with x#, y#, Lat, Long, columns for each timestep
################################
PixelsSB1 <- WSCentroids[,1]
Npixels <- length(PixelsSB1) #This is the number of pixels to extract data from, set length of SPDF to this
print(Npixels) 
PixelIDs <- as.vector(PixelsSB1)
PixelIDs

startdate <- as.Date("2008-01-01")
enddate <- as.Date("2012-12-31")
MPE.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
days.text <- gsub("-","",as.Date(MPE.days, origin="1970-01-01"))
hours <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
Ndays <- as.numeric(enddate-startdate + 1)
Nhours <- as.numeric(length(hours))
hours.text <- rep(days.text,each=24)
hours.text <- paste(hours.text,"_",hours,sep="")
hours.text

hourly.ncdf.filename <- list(data=NA, nrow=(Ndays*Nhours))
llCounter = 1
for(i in 1:Ndays){
  for(j in 1:Nhours){
      hourly.ncdf.filename[llCounter] <- paste(MPE.root,"ST4_",days.text[i],hours[j],"_01h.nc",sep="")
      llCounter=llCounter+1
  }
}
hourly.ncdf.filename


WSPrecip <- array(data=NA, dim=c((Ndays*Nhours), Npixels))
WSPrecip <- as.data.frame(WSPrecip)
rownames(WSPrecip) <- hours.text
colnames(WSPrecip) <- PixelIDs
WSPrecip

#################################
#4.Extract data
#################################

for (i in 1:(Ndays*Nhours)){
  does.file.exist <- file.exists(as.character(hourly.ncdf.filename[i]))
  if(does.file.exist==TRUE){
    TS <- open.ncdf(hourly.ncdf.filename[i])
    RainMatrix <- get.var.ncdf(TS,names(TS$var[4]))
    Rain <- NULL
      for (j in 1:881){
        Rain <- c(Rain,RainMatrix[,j])
      }
    Rain <- Rain[PixelIDs]
    #Rain
    WSPrecip[i,] <- Rain
    close.ncdf(TS)
    rm(TS)
    }
}
#WSPrecip

##Add a column of average over all ws pixels for each timestep
WSPrecip[,Npixels+1] <- rowMeans(WSPrecip, na.rm=T, dims=1)
colnames(WSPrecip)[Npixels+1] <- "Mean"
WSPrecip

##Huzzah!
write.csv(WSPrecip, file=paste(output_dir,"Manitowoc20080101_20121231.csv",sep=""))

##Stitch all 11 years together
Man1 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20020101_20030710.csv",sep=""),header=T)
Man2 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20030711_20031231.csv",sep=""),header=T)
Man3 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20040101_20070918.csv",sep=""),header=T)
Man4 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20070919_20071104.csv",sep=""),header=T)
Man5 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20071105_20071231.csv",sep=""),header=T)
Man6 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20080101_20120831.csv",sep=""),header=T)
#Man7 <- read.csv(file=paste(root_dir,"ConvertedChunks/Manitowoc20120901_20121231.csv",sep=""),header=T)
ManitowocPrecip <- rbind(Man1,Man2,Man3,Man4,Man5,Man6)
dim(ManitowocPrecip)
LengthShouldBe <- ((8*365)+(3*366)-(31+30+31+30))*24 #(-(31+30+31+30) is bc 2012 isn't complete yet, thru 8/31
LengthShouldBe #NAILED IT!
write.csv(ManitowocPrecip, file=paste(root_dir,"ConvertedChunks/ManitowocHourly20020101_20120831.csv",sep=""))

################################
##5. ID pixels in each sub-basin, write shapefile of centroids for each
################################

FullMatrix <- read.csv(file=paste(root_dir,"ConvertedChunks/ManitowocHourly20020101_20120831.csv",sep=""),header=T)
FullMatrix <- FullMatrix[,-1]

SB1.WGS84 <- readShapePoly(paste(root_dir,"SB1",sep=""),proj4string=LL.WGS84)
SB2.WGS84 <- readShapePoly(paste(root_dir,"SB2",sep=""),proj4string=LL.WGS84)
SB3.WGS84 <- readShapePoly(paste(root_dir,"SB3",sep=""),proj4string=LL.WGS84)
SB4.WGS84 <- readShapePoly(paste(root_dir,"SB4",sep=""),proj4string=LL.WGS84)
SB5.WGS84 <- readShapePoly(paste(root_dir,"SB5",sep=""),proj4string=LL.WGS84)
SB6.WGS84 <- readShapePoly(paste(root_dir,"SB6",sep=""),proj4string=LL.WGS84)
SB7.WGS84 <- readShapePoly(paste(root_dir,"SB7",sep=""),proj4string=LL.WGS84)
SB8.WGS84 <- readShapePoly(paste(root_dir,"SB8",sep=""),proj4string=LL.WGS84)
SB9.WGS84 <- readShapePoly(paste(root_dir,"SB9",sep=""),proj4string=LL.WGS84)
SB10.WGS84 <- readShapePoly(paste(root_dir,"SB10",sep=""),proj4string=LL.WGS84)
SB11.WGS84 <- readShapePoly(paste(root_dir,"SB11",sep=""),proj4string=LL.WGS84)
SB12.WGS84 <- readShapePoly(paste(root_dir,"SB12",sep=""),proj4string=LL.WGS84)
SB13.WGS84 <- readShapePoly(paste(root_dir,"SB13",sep=""),proj4string=LL.WGS84)
SB14.WGS84 <- readShapePoly(paste(root_dir,"SB14",sep=""),proj4string=LL.WGS84)
SB15.WGS84 <- readShapePoly(paste(root_dir,"SB15",sep=""),proj4string=LL.WGS84)
SB16.WGS84 <- readShapePoly(paste(root_dir,"SB16",sep=""),proj4string=LL.WGS84)
SB17.WGS84 <- readShapePoly(paste(root_dir,"SB17",sep=""),proj4string=LL.WGS84)
SB18.WGS84 <- readShapePoly(paste(root_dir,"SB18",sep=""),proj4string=LL.WGS84)
SB19.WGS84 <- readShapePoly(paste(root_dir,"SB19",sep=""),proj4string=LL.WGS84)

##reproject and buffer
for(i in 1:19){
  nam <- paste("SB",i,".WGS84",sep="")
  nam <- eval(parse(text=nam))
  temp.obj <- spTransform(nam,HRAP)
  temp.obj <- gBuffer(temp.obj, byid=FALSE, id=NULL, width=4000)
  assign(paste("SB",i,sep=""), temp.obj)
}

#plot(SB1)
#summary(SB1)

##return pixel lists by SB and write shapefiles of pixels
for(i in 1:19){
  nam <- paste("SB",i,sep="")
  nam <- eval(parse(text=nam))
  temp.obj <- over(nam,ConusPts,returnList=T)
  temp.obj <- as.data.frame(temp.obj)
  spatial.temp.obj <- SpatialPointsDataFrame(temp.obj)
  temp.obj <- as.vector(paste("X",temp.obj[,1],sep=""))
  assign(paste("SB",i,"pixels",sep=""), temp.obj)
  
}

all.startdate <- as.Date("2002-01-01")
all.enddate <- as.Date("2012-08-31")
all.MPE.days <- rep(as.Date(all.startdate, origin="1970-01-01"):as.Date(all.enddate, origin="1970-01-01"), each=1)
all.days.text <- gsub("-","",as.Date(all.MPE.days, origin="1970-01-01"))
hours <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
Ndays.all <- as.numeric(all.enddate-all.startdate + 1)
Nhours <- as.numeric(length(hours))
all.hours.text <- rep(all.days.text,each=24)
all.hours.text <- paste(all.hours.text,"_",hours,sep="")
summary(all.hours.text)

##extract rain and write csvs for all 19 subbasins
for(i in 1:19){
  nam <- paste("SB",i,"pixels",sep="")
  nam <- eval(parse(text=nam))
  Npixels <- length(nam)
  temp.obj <- FullMatrix[,nam]
  mean <- rowMeans(temp.obj, na.rm = T, dims = 1)
  temp.obj <- cbind(temp.obj, mean)
  rownames(temp.obj) <- all.hours.text
  write.csv(temp.obj, file=paste(output_dir,"SB",i,"MPEh.csv",sep=""))
  assign(paste("SB",i,"precip",sep=""), temp.obj)
}

##Write SB pixels to shapefiles
# from above: WSIDs.SPDF <- SpatialPointsDataFrame(WSxy,WSIDs)

#Eventually add bounding box option?

