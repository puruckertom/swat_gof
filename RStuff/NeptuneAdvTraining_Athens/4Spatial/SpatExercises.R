#### Advanced R Training, Fall 2012
#### Spatial Exercises
###  Created by Matt Pocernich, Will Barnett and Mark Fitzgerald
###  Neptune and Co., Inc.  www.neptuneinc.org

### Questions or comments may be sent to Matt Pocernich mpocernich@neptuneinc.org


### The code in this script use file references which assumes that the current directory is 
### set to AdvancedR.Fall2012.  
### Check working directory
## getwd()


###### Exercises ######

## 1: Read in the 2009 Hypoxia Station / DO shapefile using the different methods described
## in the presentation. That is, use read.shp / read.dbf, read.shapefile, and readShapeSpatial.


## 2. Make a plot of the 2009 DO data and draw a polygon around the points. Save the polygon as
## a shapefile.


## 3. Use sppplot() to plot the 2009 DO data, with layers for the 
## polygon you drew in Ex. 2 and the shoreline shapefile. Modify the 
## legend as you would like, and change the color scheme to something
## suitable for you. (Hint: ?terrain.colors for examples of other colors)


## 4. Make a variogram of the 2009 DO data and fit two covariance models to it.
## (Hint: vgm() with no arguments returns the available covariance models in gstat.)
## (?vgm  and show.vgms() help too.)


## 5. Sample new points within the polygon you made in Ex. 2 and use the new points to 
## find predicted (kriged) values across the northern gulf. Do this with both models you fit
## in Part 4. Compare the kriged results between the two models by plotting them 
## in spplot together.









###### Solutions ######

## 1. Read in the 2009 Hypoxia Station / DO shapefile using the different methods described
## in the presentation. That is, use read.shp / read.dbf, read.shapefile, and readShapeSpatial.

fn <- "Data/Spatial/Gulf Hypoxia 2008-2011/2009-Hypoxia-Stations/2009_hypoxia_stations"
getinfo.shape(fn)
shp09 <- read.shp(paste(fn,".shp", sep = ""))
dbf09 <- read.dbf(paste(fn,".dbf", sep = ""))
shx09 <- read.shx(paste(fn,".shx", sep = ""))
all09 <- read.shapefile(fn)
do2009 <- readShapeSpatial(fn)


## 2. Make a plot of the 2009 DO data and draw a polygon around the points. Save the polygon as
## a shapefile.

plot(do2009, col = "red", axes = TRUE)
## This will be done by users:
polyDO2009 <- drawPoly(sp = TRUE)
class(polyDO2009)
## Find unique ID for each polygon:
IDs <- sapply(slot(polyDO2009, "polygons"), function(x) slot(x, "ID"))
## Create dummy data frame for each polygon:
df <- data.frame(rep(0, length(IDs)), row.names=IDs)
## Coerce into sp object:
spdfDO2009 <- SpatialPolygonsDataFrame(polyDO2009, df)
class(spdfDO2009)
## Write the polygon to a file:
writePolyShape(spdfDO2009, "PolyDO2009")
getinfo.shape("PolyDO2009")
polyDO2009 <- readShapePoly("Data/Spatial/Polygons/PolyDO2009")


## 3. Use sppplot() to plot the 2009 DO data, with layers for the 
## polygon you drew in Ex. 2 and the shoreline shapefile. Modify the 
## legend as you would like, and change the color scheme to something
## suitable for you. (Hint: ?terrain.colors for examples of other colors)

## Read in the Gulf shoreline shapefile, if it's not already in 
gulf <- readShapeSpatial("Data/Spatial/Shoreline/gom_medium_shoreline")
## Make lists of the shoreline and 2009 polygon for the sp.layout argument in spplot()
shore <- list("sp.lines", gulf)
poly2009 <- list("sp.polygons", polyDO2009)
## Plot using spplot():
summary(do2009@data$OXMGL) # Look at the  summary of the 'data' slot, 'OXMGL' column
spplot(do2009, zcol = "OXMGL", key.space = "right", 
       sp.layout = list(shore, poly2009),
       main = "2009 Northern Gulf Hypoxia Measurements (mg/L)", 
       cuts = 0:8, col.regions = topo.colors(8),
       legendEntries = c("0-1 mg/L", "1-2 mg/L", "2-3 mg/L", "3-4 mg/L",
                         "4-5 mg/L", "5-6 mg/L", "6-7 mg/L", "7-8 mg/L")
       )


## 4. Make a variogram of the 2009 DO data and fit two covariance models to it.
## (Hint: vgm() with no arguments returns the available covariance models in gstat.)
## (?vgm  and show.vgms() help too.)
names(do2009) <- tolower(names(do2009))
vgOxg09 <- variogram(oxmgl ~ 1, do2009)
plot(vgOxg09, pch = 16, col = "red", cex = 1.5,
     main = "Semivariogram for Dissolved Oxygen in Northern Gulf, 2009")
## The plot above looks linear. Fit a linear, exponential, and spherical model.
Oxg09Lin <- fit.variogram(vgOxg09, vgm(psill = 2, "Lin", nugget = 0))
Oxg09Exp <- fit.variogram(vgOxg09, vgm(psill = 2, "Exp", range = 3, nugget = 0))
Oxg09Sph <- fit.variogram(vgOxg09, vgm(psill = 2, "Sph", range = 3, nugget = 0))
plot(gamma ~ dist, vgOxg09, ylim = c(0, 1.05 * max(vgOxg09$gamma)), col="red", 
     ylab = "semivariance", xlab = "distance", pch = 16, cex = 1.5)
lines(variogramLine(Oxg09Lin, 4), lty = 1, cex = 1.5)
lines(variogramLine(Oxg09Exp, 4), lty = 2, cex = 1.5)
lines(variogramLine(Oxg09Sph, 4), lty = 3, cex = 1.5)
legend("bottomright", legend = c("Linear", "Exponential", "Spherical"), lty = 1:3)


## 5. Sample new points within the polygon you made in Ex. 2 and use the new points to 
## find predicted (kriged) values across the northern gulf. Do this with all the models you fit
## in Part 4. Compare the kriged results between the two models by plotting them 
## in spplot together.
newLoc09 <- spsample(x = polyDO2009, n = 5000, type = "regular")
class(newLoc09)
gridded(newLoc09) <- TRUE
class(newLoc09)
kgLin09 = krige(oxmgl ~ 1, do2009, newLoc09, model = Oxg09Lin)
kgExp09 = krige(oxmgl ~ 1, do2009, newLoc09, model = Oxg09Exp)
kgSph09 = krige(oxmgl ~ 1, do2009, newLoc09, model = Oxg09Sph)
kg <- kgLin09 # Make a dummy SpatialPixelsDataFrame
kg[["Lin"]] <- kgLin09[["var1.pred"]] # Add Linear column to @data.
kg[["Exp"]] <- kgExp09[["var1.pred"]] # Add Exponential column to @data.
kg[["Sph"]] <- kgSph09[["var1.pred"]] # Add Spherical column to @data.

spplot(obj = kg, zcol = c("Lin", "Exp", "Sph"),
       sp.layout = list(shore, poly2009), as.table = TRUE,
       names.attr = c("Kriging, Linear", "Kriging, Exponential", "Kriging, Spherical"),
       main = "Predicted Dissolved Oxygen Across Northern Gulf, 2009")
