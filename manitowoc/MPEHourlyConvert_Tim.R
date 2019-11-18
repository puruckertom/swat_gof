library(sp)
#library(foreign) #for dbfs
library(proj4) #for reprojection - not sure this is necessary
library(rgdal) #for coordinate projection of sp files and data export
library(ncdf)
library(RNetCDF)
library(rgeos) #for geometry ops
library(maptools)

### proj4 strings
HRAP <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs") #HRAP SR-ORG:4694
LL.WGS84 <- CRS("+proj=longlat +ellps=WGS84")
UTM83_18 <- CRS("+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#if(.Platform$OS.type=="unix"){
  #root_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
  #output_dir <- path.expand("~/Dropbox/ktp/Manitowoc/TestOutput/")
#}

#windows
#if(.Platform$OS.type=="windows"){
  #root_dir <- path.expand("d://Dropbox/ktp/Manitowoc/")
  #output_dir <- path.expand("d://Dropbox/ktp/Manitowoc/TestOutput/")
#}

#On Tim:
root_dir <- path.expand("e://Manitowoc/")
output_dir <- path.expand("e://Manitowoc/output/")
MPE.root <- path.expand("e://HourlyNetCDF/")

################################
#1. Project and buffer shapefile 
################################
##Read in watershed boundary shapefile(s) in original projection
#Manitowoc.WGS84 <- readShapePoly(paste(root_dir,"KeewooksOrigFiles/Manitowoc_19Subbasins",sep=""),proj4string=UTM83_18)
#SB1.WGS84 <- readShapePoly(paste(root_dir,"SB1",sep=""),proj4string=CRS("+proj=longlat +ellps=WGS84"))
#summary(SB1.WGS84)
#LR <- readShapePoly(paste(root_dir,"ShapefileTest/LR_WGS84_Buffer",sep=""),proj4string=CRS("+proj=longlat +ellps=WGS84"))

##Reproject watershed boundary shapefile(s) to HRAP
#Manitowoc.HRAP <- spTransform(Manitowoc.WGS84,HRAP)
#summary(Manitowoc.HRAP)

##Buffer polygon 4km to catch outside centroids (HRAP linear unit = meters)
#Manitowoc <- gBuffer(Manitowoc.HRAP, byid=FALSE, id=NULL, width=4000)
#proj4string(Manitowoc) <- HRAP


##Stuff below is need to coerce buffered ws to SPDF for export as .shp, isn't necessary for rain extraction
#row.names(Manitowoc)
#data <- data.frame(f=99.9, row.names="buffer")
#ManitowocSPDF <- SpatialPolygonsDataFrame(Manitowoc,data)
#summary(ManitowocSPDF)
#writeOGR(ManitowocSPDF, paste(root_dir,"KeewooksOrigFiles",sep=""), "ManitowocHRAPBuffer", driver="ESRI Shapefile")

##once the writeOGR is done, can just load projected and buffered shp:
Manitowoc <- readShapePoly(paste(root_dir,"ManitowocHRAPBuffer",sep=""),proj4string=HRAP)
#SB1 <- readShapePoly(paste(root_dir,"SB1/sB1BufferR",sep=""),proj4string=CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))
#summary(SB1)

################################
#2. set up coords in SPDF, ID radar pixels inside ws
################################
##Read in Lat/Long layers of netcdf file, each is a sep grid layer
GetCoords <- open.ncdf(paste(MPE.root,"ST4_2002010100_01h.nc",sep="")) #this can be any one of the MPE nc files
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

#plot(SB1, col="orange")
#plot(LRHRAP, col="dodgerblue")
plot(Manitowoc, col="darkolivegreen2")
plot(ConusPts, add=T)

NcIDy <- rep(1:881,each=1121) #IDx and y are here to check against dbf list and make sure coord list is in right order (also counts as data for SPDF)
NcIDx <- rep(1:1121,881) 
ExtractID <- seq(1:987601)
ID <- as.data.frame(cbind(ExtractID,NcIDx,NcIDy))
#ID[7354,]

##To id pixels within watershed
WSCentroids <- over(Manitowoc,ConusPts,returnList=T) #'over' doesn't work with SPDF - just use SP
WSCentroids <- as.data.frame(WSCentroids)
#write.csv(WSCentroids, file=paste(root_dir,"TestOutput/sB1Centroids.csv",sep=""))

###To write centroids to shapefile:
#?xy
#?CentroidCoords <- xy[WSCentroids,]
#?CentroidsSPDF <- SpatialPointsDataFrame(WSCentroids,ID) 
#?writeOGR(CentroidsSPDF, paste(root_dir,"TestOutput",sep=""), "SB1Centroids", driver="ESRI Shapefile") #how do I do this without it making a new subdirectory?

###To write *all* CONUS points to shapefile:
#ConusSPDF <- SpatialPointsDataFrame(ConusPts,ID) 
#summary(ConusSPDF)
#writeOGR(ConusSPDF, paste(root_dir,"ConusPts",sep=""), "ConusPtsR", driver="ESRI Shapefile")

################################
#3. Create SPDF with x#, y#, Lat, Long, columns for each timestep
################################
PixelsSB1 <- WSCentroids[,1]
Npixels <- length(PixelsSB1) #This is the number of pixels to extract data from, set length of SPDF to this
print(Npixels) 
PixelIDs <- as.vector(PixelsSB1)
PixelIDs

startdate <- as.Date("2005-01-01")
enddate <- as.Date("2009-12-31")
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

WSPrecip <- array(data=NA, dim=c((Ndays*Nhours), Npixels))
WSPrecip <- as.data.frame(WSPrecip)
rownames(WSPrecip) <- hours.text
colnames(WSPrecip) <- PixelIDs
WSPrecip

#################################
#4.Extract data
#################################

for (i in 1:(Ndays*Nhours)){
  if(file.exists(hourly.ncdf.filename[i])==TRUE){
  TS <- open.ncdf(hourly.ncdf.filename[i])
  #print(TS)
  RainMatrix <- get.var.ncdf(TS,"APCP_SFC")
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
Means <- rowMeans(WSPrecip, na.rm = T, dims = 1)
WSPrecip[,Npixels+1] <- rowMeans(WSPrecip, na.rm=T, dims=1)
colnames(WSPrecip)[Npixels+1] <- "Mean"
WSPrecip

##Huzzah!
write.csv(WSPrecip, file=paste(output_dir,"/Manitowoc2005_2009.csv",sep=""))




#NEXT: find a way to dummy check the coordinates; compare with gauge rainfall totals
#EVENTUALLY: add a bounding box option

#print(T1) #Gets layer (variable) names from NetCDF info
##Reassure everything is still lining up right?
#Rain <- as.data.frame(cbind(ExtractID,Rain)) #the extract ID isn't at all necessary but feels safer
#Rain <- Rain[PixelIDs,]
