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

#if(.Platform$OS.type=="unix"){
  #root_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
  #output_dir <- path.expand("~/Dropbox/ktp/Manitowoc/MilwaukeeAirport/")
#}

#windows
if(.Platform$OS.type=="windows"){
  root_dir <- path.expand("d://Dropbox/ktp/Manitowoc/")
  output_dir <- path.expand("d://Dropbox/ktp/Manitowoc/MilwaukeeAirport/")
}

MPE.root <- path.expand("g://HourlyNetCDF/")

MKE.coords <- as.matrix(cbind(-87.9044,42.955))
MKE.point <- SpatialPoints(MKE.coords, proj4string=LL.WGS84)
MKE.point <- spTransform(MKE.point, HRAP)

##Check that it plots properly
TheDairyState <- readShapePoly(paste(root_dir,"WisconsinWGS84",sep=""),proj4string=LL.WGS84)
TheDairyState <- spTransform(TheDairyState, HRAP)
plot(TheDairyState, col="firebrick") 
plot(MKE.point, pch=21, col="black", bg="aquamarine3", cex=3, add=T) 

##Buffer polygon 50km
MKE <- gBuffer(MKE.point, byid=FALSE, id=NULL, width=50000)
proj4string(MKE) <- HRAP
plot(MKE, add=T)

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

plot(MKE, col="goldenrod3",border="chocolate4", lwd=6, add=T)
plot(ConusPts,pch=21,col=NULL,bg="darkslategray4",add=T)
NcIDy <- rep(1:881,each=1121)#NWS ID coord system in CONUS files
NcIDx <- rep(1:1121,881) 
ExtractID <- seq(1:987601)
ID <- as.data.frame(cbind(ExtractID,xy,NcIDx,NcIDy)) #all conus pts (this is input for everything)
ID.SPDF <- SpatialPointsDataFrame(ConusPts,ID) #all conus pts (this is input for everything)

IDs.MKE <- over(MKE, ID.SPDF, returnList=T)
IDs.MKE <- as.data.frame(IDs.MKE)
IDs.MKE.SPDF <- SpatialPointsDataFrame(IDs.MKE[,2:3], proj4string=LL.WGS84)

Pixels <- IDs.MKE[,1]
Npixels <- length(Pixels) #This is the number of pixels to extract data from, set length of SPDF to this
Npixels
Pixels <- as.vector(Pixels)

startdate <- as.Date("2010-01-01")
enddate <- as.Date("2012-08-31")
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

MKEPrecip <- array(data=NA, dim=c((Ndays*Nhours), Npixels))
MKEPrecip <- as.data.frame(MKEPrecip)
rownames(MKEPrecip) <- hours.text
colnames(MKEPrecip) <- Pixels

#################################
#3.Extract data
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
    Rain <- Rain[Pixels]
    #Rain
    MKEPrecip[i,] <- Rain
    close.ncdf(TS)
    rm(TS)
  }
}
MKEPrecip[1:10,]

##Add a column of average over all ws pixels for each timestep
MKEPrecip[,Npixels+1] <- rowMeans(MKEPrecip, na.rm=T, dims=1)
colnames(MKEPrecip)[Npixels+1] <- "Mean"
MKEPrecip

##Huzzah!
write.csv(MKEPrecip, file=paste(output_dir,"MKE2010_20120831.csv",sep=""))

##Stitch all 11 years together
MKE01 <- read.csv(file=paste(output_dir,"MKE2002.csv",sep=""),header=T)
MKE02 <- read.csv(file=paste(output_dir,"MKE2003.csv",sep=""),header=T)
MKE03 <- read.csv(file=paste(output_dir,"MKE2004_2006.csv",sep=""),header=T)
MKE04 <- read.csv(file=paste(output_dir,"MKE2007_2009.csv",sep=""),header=T)
MKE05 <- read.csv(file=paste(output_dir,"MKE2010_20120831.csv",sep=""),header=T)

MKEPrecip <- rbind(MKE01,MKE02,MKE03,MKE04,MKE05)
dim(MKEPrecip)
LengthShouldBe <- ((8*365)+(3*366)-(31+30+31+30))*24 #(-(31+30+31+30) is bc 2012 isn't complete yet, thru 8/31
LengthShouldBe
#Nailed it!

write.csv(MKEPrecip, file=paste(output_dir,"MKE_precip.csv",sep=""))
write.csv(IDs.MKE, file=paste(output_dir,"MKE_coords.csv",sep=""))

#make shapefile of pixels
MKE.points <- ConusPts[MKE,]
MKE.SPDF <- SpatialPointsDataFrame(MKE.points,IDs.MKE)
proj4string(MKE.SPDF) <- HRAP
MKE.LL <- spTransform(MKE.SPDF,LL.WGS84)
writeOGR(MKE.LL, paste(output_dir, "Shapefiles", sep=""), "MKEPoints", driver="ESRI Shapefile")
