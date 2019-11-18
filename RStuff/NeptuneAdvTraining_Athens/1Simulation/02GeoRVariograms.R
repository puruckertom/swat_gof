if(.Platform$OS.type=="unix"){
	root_dir <- path.expand("~/Dropbox/WRF/")
	output_dir <- path.expand("~/Dropbox/WRF/geoRplay/")
}

#windows
if(.Platform$OS.type=="windows"){
	root_dir <- path.expand("d://Dropbox/WRF/")
	output_dir <- path.expand("d://Dropbox/WRF/geoRplay/")
}

library(geoR)


Nozero <- read.csv(paste(root_dir,"geoRplay/nozero_5years.csv",sep=""),header=TRUE)
Coords <- read.csv(paste(root_dir,"gauge_coords.csv",sep=""),header=TRUE)

AlbersNAD83 <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs 
")
Gauges_sp <- SpatialPoints(coords, proj4string=AlbersNAD83)

GaugesNonzeroSPDF <- SpatialPointsDataFrame(Gauges_sp, Nozero, proj4string=AlbersNAD83, match.ID=T)
summary(GaugesNonzeroSPDF)
dim(GaugesNonzeroSPDF)



Ndays = 1433
breaks <- seq(from=0, to=125000, l=8) #these choices based on "04-variogram.pdf", max separation distance of 250km, average separation distance = 16km, variogram valid to half distance gives 8 bins

pdf(paste(output_dir,"NonZeroVariography.pdf",sep=""), height=8, width=8)
for(i in 1:Ndays){
	par(mfrow=c(4,1))
	#variography and fit
	vario1 <- variog(as.geodata(NozeroSPDF[,i]),breaks=breaks,op="bin")
	vf1 <- variofit(vario1)
	plot(vario1, main=i)
	lines(vf1)
	#rainfall map for that day
	
	#pdf of rainfall intensity by pixel
	
	#print summary as text
	
	#save all the necessary parameters    
	#either as a list of lists or with a thought out table
	
}
dev.off()

#for (i in 1:Ndays){
#	vario[i] <-variog(as.geodata(NozeroSPDF[,i]),breaks=breaks,op="bin")
#}