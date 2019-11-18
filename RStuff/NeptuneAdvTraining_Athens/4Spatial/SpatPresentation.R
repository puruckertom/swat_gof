### Advanced Graphics in  R - Fall 2012
### 
###  Created by Matt Pocernich, Will Barnett and Mark Fitzgerald
###  Neptune and Co., Inc.  www.neptuneinc.org

### Questions or comments may be sent to Matt Pocernich mpocernich@neptuneinc.org


### The code in this script use file references which assumes that the current directory is 
### set to AdvancedR.Fall2012.  
### Check working directory
## getwd()


## Packages
library(shapefiles)
library(sp)
library(gstat)
library(maptools)
library(raster)
library(spsurvey)
library(lattice)

## Options
# rm(list = ls() ) ## removes any objects in workspace.  A nice practice
options(digits = 3)  ## causes only 3 digits to be displayed.  Default is 7


###### Part 1: Import/Export in R ######
## Using 'maptools' functions to read/write shape files.
## Check spatial data type of the shapefile:
infoShp <- getinfo.shape("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations.shp")




## Read the shapefile in as an 'sp' object using 'maptools':
readShp <- readShapeSpatial("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations")
str(readShp) # Default projection is "NA" in readShapeSpatial()

### note that a projection is note specified.
### to specify use

str(readShp)

readShp[1:10,] # The head() command doesn't show the coordinates, only the data.
sp2shape(readShp, shpfilename = "2008Stations1")

## Read the .dbf file using 'shapefiles':
readDbf <- read.dbf("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations.dbf")
str(readDbf) # A data.frame instead of an 'sp' object


### alternative ways to access data directly from shape object

dat <- readShp@data
dat2 <- slot(readShp, "data")

## Read the shapefile as a list using 'shapefiles':
readShp2 <- read.shp("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations.shp")
str(readShp2)
head(readShp2$shp)

## Read the shapefile as a list of lists using 'shapefiles'.
readShp3 <- read.shapefile("Data/Spatial/Gulf Hypoxia 2008-2011/2008-Hypoxia-Stations/2008SummerHypoxiaStations")
## files yourself and convert them into a 'sp' object manually.
## Exporting files to GIS format:
write.shp(shp = readShp2, out.name = "2008Stations.shp") # .shp file by itself
write.shapefile(readShp3, "2008Stations2", arcgis = TRUE) # all 3 files at once

#### data.frame to shapefile
### assume we begin with the dataframe dat and wish to make a shapefile for GIS User

spatialPointsTemp <- SpatialPoints(coords = dat2[, c("LONGITUDE", "LATITUDE")],
                         proj4string = CRS("+proj=longlat +ellps=WGS84"))

spatialPointsDF<- SpatialPointsDataFrame( spatialPointsTemp, dat2)

newShape <- writeSpatialShape(spatialPointsDF, "newShape")

#### 
## readShapeSpatial is a shortcut, so that you don't have to combine the .dbf, .shp and
## .shx files yourself and convert them into a 'sp' object manually. For manipulating them in R,
## readShapeSpatial() is the easiest.
### crude way to get bounding box

#### A quick plot using ggmap  
#### requires internet access

bbox(coordinates(readShp) )

library(ggmap)
map <- get_map(  location = c( left = -97.5, bottom = 25.9, 
                               right = -87.5, top = 30.1)  , 
       source = "osm", col = "bw")
ggmap(map)

ggmap(map) + geom_point(aes( x = LONGITUDE, y = LATITUDE, col = OXMGL),  data = dat2) + scale_size(guide = "none") +
             xlab("") + ylab("") + labs(title = "Dissolved Oxygen" )           
####



###### End of Part 1 ######




###### Part 2: Visualization and Basic Analyses ######
## Basic plot of the 2011 Northern Gulf DO values:
plot(readShp, pch = "*", col = "red", axes = TRUE)
plot(readShp, pch = "*", col = "red", axes = TRUE, main = "Type title here...") # 'main' doesn't work.
## Get shoreline boundaries for Northern Gulf
gulf <- readShapeSpatial("Data/Spatial/Shoreline/gom_medium_shoreline")
plot(gulf, axes = TRUE) # takes a long time to plot...
plot(readShp, pch = "*", col = "red", add = TRUE)

## Draw polygon around the points using drawPoly() in 'raster' package. Doesn't work well in RStudio.
###### Do in the base R application during presentation:
#polyDO2008 <- drawPoly(sp = TRUE) # Left-click to create segments, right-click to end.
#plot(polyDO2008, add = TRUE)
#class(polyDO2008)
# Find unique ID for each polygon:
#IDs <- sapply(slot(polyDO2008, "polygons"), function(x) slot(x, "ID"))
# Create dummy data frame for each polygon:
#df <- data.frame(rep(0, length(IDs)), row.names=IDs)
# Coerce into sp object:
#spdfDO2008 <- SpatialPolygonsDataFrame(polyDO2008, df)
#class(spdfDO2008)
# Write 
#writePolyShape(spdfDO2008, "PolyDO2008")
#getinfo.shape("PolyDO2008")


polyDO2008 <- readShapePoly("Data/Spatial/Polygons/PolyDO2008")


######


## Plot these using spplot() instead.
# Re-name 'readShp' to something meaningful.
do2008 <- readShp
names(do2008) <- tolower(names(do2008))
shore <- list("sp.lines", gulf)
poly2008 <- list("sp.polygons", polyDO2008)
# Basic spplot:
spplot(do2008, zcol = "oxmgl")
# More complicated spplot:
# Takes a bit to process:
spplot(do2008, zcol = "oxmgl", key.space = "right",
       sp.layout = list(shore, poly2008)) # Notice the x and y limits... 
# Change the legend cutpoints, add a title, the font size, the legend labels...
do08Sum <- summary(do2008@data[,"oxmgl"])
spplot(do2008, zcol = "oxmgl", key.space = "right", 
       sp.layout = list(shore, poly2008),
       col.regions = terrain.colors(7),
       main = "2008 Northern Gulf Hypoxia Measurements (mg/L)", 
       cex = 1.2, cuts = 0:7, 
       legendEntries = c("0 - 1", "1 - 2", "2 - 3", "3 - 4",
                         "4 - 5", "5 - 6", "6 - 7"))


## Variogram and basic covariance model:
# Make a variogram
vgOxg08 <- variogram(oxmgl ~ 1, do2008)
plot(vgOxg08, pch = 16, col = "red", cex = 1.5,
     main = "Semivariogram for Dissolved Oxygen in Northern Gulf, 2008")
# Fit a model to this variogram
fitOxg08 <- fit.variogram(vgOxg08, vgm(psill = 3, "Exp", range = 3,
                                      nugget = 0))
plot(vgOxg08, fitOxg08, 
     main = "Exponential Model Fit to Northern Gulf DO Data, 2008",
     pch = 16, col = "red", cex = 1.5)

## Can krige to 'new' spatial locations as well:
nLoc <- 1000
newLoc08 <- spsample(x = polyDO2008, n = nLoc, type = "regular")
gridded(newLoc08) <- TRUE
# Plot the new sample locations and the old ones:
plot(newLoc08, col = "red")
plot(polyDO2008, add = TRUE)
plot(do2008, pch = "*", col = "blue", add = TRUE)
# Krige using ordinary kriging:
doOk08 = krige(oxmgl ~ 1, do2008, newLoc08, model = fitOxg08)
# Make a spplot() with kriged values and the original points:
trellis.par.set(sp.theme())
pts <- list("sp.points", do2008, pch = 3, col = "grey")
spplot(obj = doOk08, zcol = "var1.pred", 
       sp.layout = list(shore, pts, poly2008),
       main = "Predicted Dissolved Oxygen Across Northern Gulf, 2008")
# Contoured spplot:
spplot(obj = doOk08, zcol = "var1.pred", 
       sp.layout = list(shore, pts, poly2008), contour = TRUE,
       main = "Predicted Dissolved Oxygen Across Northern Gulf, 2008")
# Predictions and standard errors:
doOk08$se <- sqrt(doOk08[["var1.var"]])
spplot(obj = doOk08, zcol = c("var1.pred", "se"),
       sp.layout = list(shore, poly2008), as.table = TRUE,
       names.attr = c("Ordinary Kriging Predictions", "Ordinary Kriging Standard Errors"),
       main = "Predicted Dissolved Oxygen Across Northern Gulf, 2008")
