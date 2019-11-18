library(sp)
#library(foreign) #for dbfs
library(proj4) #for reprojection - not sure this is necessary
library(rgdal) #for coordinate projection of sp files and data export
library(ncdf)
library(RNetCDF)
library(rgeos) #for geometry ops
library(maptools)

### proj4 strings
#HRAP="+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs" #HRAP SR-ORG:4694
#LL/WGS84=CRS("+proj=longlat +ellps=WGS84")

if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/Manitowoc/")
  output_dir <- path.expand("~/Dropbox/ktp/Manitowoc/TestOutput/")
}

#windows
if(.Platform$OS.type=="windows"){
  root_dir <- path.expand("d://Dropbox/ktp/Manitowoc/")
  output_dir <- path.expand("d://Dropbox/ktp/Manitowoc/TestOutput/")
}

MPE.root <- paste(root_dir,"FiveTestDays/OldDailyFiles/",sep="")

################################
#1. Project and buffer shapefile (add alternate option for bounding box?)
################################
##Read in watershed boundary shapefile(s) in original projection
SB1.WGS84 <- readShapePoly(paste(root_dir,"SB1",sep=""),proj4string=CRS("+proj=longlat +ellps=WGS84"))
summary(SB1.WGS84)

##Reproject watershed boundary shapefile(s) to HRAP
SB1HRAP <- spTransform(SB1.WGS84,CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))
summary(SB1HRAP)
#plot(SB1HRAP)

##Buffer polygon 4km to catch outside centroids (HRAP linear unit = meters)
SB1 <- gBuffer(SB1HRAP, byid=FALSE, id=NULL, width=4000)
proj4string(SB1) <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs")

##Stuff below is need to coerce buffered ws to SPDF for export as .shp
#row.names(SB1)
#data <- data.frame(f=99.9, row.names="buffer")
#SB1 <- SpatialPolygonsDataFrame(SB1,data)
#summary(SB1)
#writeOGR(SB1, paste(root_dir,"SB1",sep=""), "SB1BufferR", driver="ESRI Shapefile")

#once the writeOGR is done, can just load projected shp:
SB1 <- readShapePoly(paste(root_dir,"SB1/sB1BufferR",sep=""),proj4string=CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))
summary(SB1)

################################
#2. set up coords in SPDF, ID coords inside ws
################################
GetCoords <- open.ncdf(paste(MPE.root,"200901011200_apcp.nc",sep="")) #this can be any one of the MPE nc files
##Read in Lat/Long layers of netcdf file, each is a sep grid layer
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
ConusWGS84 <- SpatialPoints(xy)
proj4string(ConusWGS84) <- CRS("+proj=longlat +ellps=WGS84")
ConusPts <- spTransform(ConusWGS84,CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))
#proj4string(ConusCoords) <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs")
summary(ConusPts)

plot(SB1, col="orange")
plot(ConusPts, add=T)

NcIDy <- rep(1:881,each=1121) #IDx and y are here to check against dbf list and make sure coord list is in right order (also counts as data for SPDF)
NcIDx <- rep(1:1121,881) 
ExtractID <- seq(1:987601)
ID <- as.data.frame(cbind(ExtractID,NcIDx,NcIDy))
#ID[7354,]

###To write CONUS points shapefile
ConusSPDF <- SpatialPointsDataFrame(ConusPts,ID) 
#summary(ConusSPDF)
#writeOGR(ConusSPDF, paste(root_dir,"ConusPts",sep=""), "ConusPtsR", driver="ESRI Shapefile")

##To id pixels within watershed
WSCentroids <- over(SB1,ConusPts,returnList=T) #'over' doesn't work right with SPDF? just use SP
WSCentroids <- as.data.frame(WSCentroids)
#write.csv(WSCentroids, file=paste(root_dir,"TestOutput/sB1Centroids.csv",sep=""))

################################
#3. Create SPDF with x#, y#, Lat, Long, columns for each timestep
################################
PixelListSB1 <- WSCentroids[,1]
Npixels <- length(PixelListSB1) #This is the number of pixels to extract data from, set length of SPDF to this
print(Npixels) 
PixelIDs <- as.vector(PixelListSB1)
#print(PixelIDs)

startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-01-05")
Ndays <- as.numeric(enddate-startdate + 1)
MPE.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
days.text <- gsub("-","",as.Date(MPE.days, origin="1970-01-01"))

daily.ncdf.filename <- list(data=NA, nrow=Ndays)
llCounter = 1
for(i in 1:Ndays){
  daily.ncdf.filename[llCounter] <- paste(MPE.root, days.text[i],"1200_apcp.nc",sep="")
  llCounter=llCounter+1
}

WSPrecip <- array(data=NA, dim=c(Ndays, Npixels))
WSPrecip <- as.data.frame(WSPrecip)
rownames(WSPrecip) <- days.text
colnames(WSPrecip) <- PixelIDs
WSPrecip

#################################
#4.Extract data
#################################

##open ncdf files pull out rain data
#TS1 <- open.ncdf(paste(MPE.root,"200901011200_apcp.nc",sep=""))
#RainMatrix <- get.var.ncdf(TS1,"APCP_SFC")
#Rain <- NULL
#for(i in 1:881){
#  Rain <- c(Rain,RainMatrix[,i])
#}
#Rain <- Rain[PixelIDs]
#WSPrecip[1,] <- Rain
#close.ncdf(TS1)
#m(TS1)

for (i in 1:Ndays){
  TS <- open.ncdf(daily.ncdf.filename[i])
  RainMatrix <- get.var.ncdf(TS,"APCP_SFC")
  Rain <- NULL
    for (j in 1:881){
      Rain <- c(Rain,RainMatrix[,j])
    }
  Rain <- Rain[PixelIDs]
  WSPrecip[i,] <- Rain
  close.ncdf(TS)
  rm(TS)
}
WSPrecip

#NEXT: find a way to dummy check the coordinates; try five days of hourly; compare with gauge rainfall totals

#print(T1) #Gets layer (variable) names from NetCDF info
##Reassure everything is still lining up right?
#Rain <- as.data.frame(cbind(ExtractID,Rain)) #the extract ID isn't at all necessary but feels safer
#Rain <- Rain[PixelIDs,]



########################################################
########################################################
###From NetCDF
#Day1test <- open.ncdf(daily.ncdf.filename[1])
#RainMatrix <- get.var.ncdf(Day1test, "APCP_SFC") #Read in rain layer of netcdf file, is still a grid
##build list in same order as coords
#Rain <- 
  #Rain <- NULL
#for(i in 1:881){
  #Rain <- c(Rain,RainMatrix[,i])
#}


startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-01-05")

Ndays <- as.numeric(enddate-startdate + 1)
MPE.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
# remove "-" and convert to text
days.text <- gsub("-","",as.Date(MPE.days, origin="1970-01-01"))

daily.ncdf.filename <- list(data=NA, nrow=Ndays)
llCounter = 1
for(i in 1:Ndays){
    daily.ncdf.filename[llCounter] <- paste(MPE.root, days.text[i],"1200_apcp.nc",sep="")
    llCounter=llCounter+1
}




#Build Lat-Lon list
MPE.root <- paste(root_dir,"FiveTestDays/OldDailyFiles/",sep="")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-01-05")

Ndays <- as.numeric(enddate-startdate + 1)
MPE.days <- rep(as.Date(startdate, origin="1970-01-01"):as.Date(enddate, origin="1970-01-01"), each=1)
# remove "-" and convert to text
days.text <- gsub("-","",as.Date(MPE.days, origin="1970-01-01"))
daily.ncdf.filename <- list(data=NA, nrow=Ndays)
llCounter = 1
for(i in 1:Ndays){
  daily.ncdf.filename[llCounter] <- paste(MPE.root,days.text[i],"1200_apcp.nc",sep="")
  llCounter=llCounter+1
}
daily.ncdf.filename
#Day1test <- open.ncdf(daily.ncdf.filename[1])
print(Day1test)
LatMatrix <- get.var.ncdf(Day1test, "latitude")
LonMatrix <- get.var.ncdf(Day1test, "longitude")

#rm(Lat)
#LatList <- list(data=NA, nrow=987601)
Lat <- NULL
for(i in 1:881){
  Lat <- c(Lat,LatMatrix[,i])
}

Lon <- NULL
for(i in 1:881){
  Lon <- c(Lon,LonMatrix[,i])
}

IDy <- rep(1:881,each=1121)
#IDy[1120:1124]
IDx <- rep(1:1121,881)
#IDx[1120:1124]

Coords <- cbind(IDx,IDy,Lon,Lat)
write.csv(Coords, paste(root_dir,"Coords.csv",sep=""))

###(following example on p39 of rgdal documentation)
##Assign spatial information 
LL <- Coords[,3:4]
#LL[1,]
LL <- SpatialPoints(LL)
#Block1 <- SpatialPointsDataFrame(LL + rain data....)
#proj4string(Day1SPDF) <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs") #HRAP SR-ORG:4694
#summary(Day1SPDF)

Rain <- Day1Test[,6]
Rain <- as.data.frame(Rain)













Day1SPDF <- SpatialPointsDataFrame(LL,Rain)
proj4string(Day1SPDF) <- CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs") #HRAP SR-ORG:4694
summary(Day1SPDF)

##Read in watershed boundary shapefile(s) in original projection
SB1.WGS84 <- readShapePoly(paste(root_dir,"SB1",sep=""),proj4string=CRS("+proj=longlat +ellps=WGS84"))
summary(SB1.WGS84)

##Reproject watershed boundary shapefile(s) to HRAP
SB1HRAP <- spTransform(SB1.WGS84,CRS("+proj=stere +lat_0=39.224079 +lon_0=-98.54180699999999 +k=1 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))
summary(SB1)
##Export reprojected data to test in Arc (from Spatial Cheatsheet I/O)
writeOGR(SB1, paste(root_dir,"SB1",sep=""), "SB1HRAP", driver="ESRI Shapefile") #holy crap it worked

###
##clip centroids by watershed boundary
SB1Clip <- over(SB1, Day1SPDF, returnList=T)
summary(SB1Clip) 
#NOPE



#hours <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
#Nhours <- as.numeric(length(hours))
#hourly.ncdf.filename <- list(data=NA, nrow=Ndays*Nhours)
#llCounter = 1
#for(i in 1:Ndays){
#for(j in 1:Nhours){
#hourly.ncdf.filename[llCounter] <- paste(MPE.root,"ST4_",days.text[i],hours[j],"_01h.nc",sep="")
#llCounter=llCounter+1
#}
#}