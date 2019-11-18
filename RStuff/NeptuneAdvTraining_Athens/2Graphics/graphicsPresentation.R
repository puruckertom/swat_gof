### Advanced Graphics in  R - Fall 2012
### 
###  Created by Matt Pocernich, Will Barnett and Mark Fitzgerald
###  Neptune and Co., Inc.  www.neptuneinc.org

### Questions or comments may be sent to Matt Pocernich mpocernich@neptuneinc.org


### The code in this script use file references which assumes that the current directory is 
### set to AdvancedR.Fall2012.  
### Check working directory

getwd()
### if directory is not AdvancedR.Fall2012 - change!

##### Part A Improving Base Plots

## Create a basic plot and save as png
## comments saving files have been commented out with ###XXX to avoid writing 
## to the directory.  

data(iris)
###XXX png("test1.png")

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")

###XXX dev.off()

### Adjustments for presentations

## 1.  Fix margins
## 2.  Thicker lines and points, rotate labels
## 3.  Larger letters

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")
 ### let us see how pars are set.

###  display default par options
par()
 
### modify options with par
par(mar = c(5,5,1,1), ### make more room in the margins
    cex = 1.5, ## enlarge text and points; pointsize*cex
     lwd = 2, ## default line thickness * lwd ; just borders - not tick marks
     las = 1) ## rotate axis labels

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")

dev.off()

### How does one enlarge just points - not text
par(mar = c(5,5,1,1))

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")

points(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
       pch = 16, cex = 2) 

dev.off()

### Make thicker lines including tick marks

###XXX png("test5.png", res = 300, units = "in", width = 5, height = 5)

par(mar = c(5,5,1,1), ### make more room in the margins
    cex = 1.5, ## enlarge text and points; pointsize*cex
     lwd = 4, ## default line thickness * lwd ; just borders - not tick marks
     las = 1) ## rotate axis labels

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length", axes = FALSE, pch = 16)
box()
axis(1, lwd = 5)
axis(2, lwd = 5)

dev.off()


## why does box take and argument from par, while axis doesn't
args(box)
args(axis)

## Note:  par settings can be saved as a list and reused
presPar <- par(mar = c(5,5,1,1), ### make more room in the margins
    cex = 1.5, ## enlarge text and points; pointsize*cex
     lwd = 4, ## default line thickness * lwd ; just borders - not tick marks
     las = 1) ## rotate axis labels

dev.off()
parOld <- par()
par("mar")
 
par(presPar) 
par("mar")

par(parOld)
dev.off()

##### Add notation to corner of a plot

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")

usr <- par("usr")  ## retrieves user coordinates of current plot, xleft, xright, ybottom, ytop

text( x = usr[1] + 0.1 * (usr[2]- usr[1]), ## 10% across x axis
      y = usr[4] - 0.1 * (usr[4]- usr[3]), ## 10% from top
      label = "Comment"
) 

### add notation in upper left corner using legend command 

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")
legend("topleft", legend = "",title = "Comment", bty = "n", text.font = 2)	


#### Part B: Graphic File Options
#### Issues of size and resolution

### First increase resolution - causes problems.
### Why?
###
png("test4PT10.RE160.png",  res = 160)

plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , 
	xlab = "Width", ylab = "Length")

dev.off()

### works looks okay on the screen

###XXX  pdf("test.pdf", pointsize = 10)
###XXX plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , pch = 16, las = 1, 
###XXX	xlab = "Variable 1", ylab = "Variable 2")
###XXX dev.off()

### exploring graphics options

options()
ps.options()

### options for pop up window.
windows.options()


######## font family names

PSFonts <- postscriptFonts()
length(PSFonts)
names(PSFonts)
PSFonts[[1]]

### Create a courier plot

###XXX
pdf("courier.pdf", family = "Courier")
 
plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , pch = 16, las = 1, xlab = "Variable 1", ylab = "Variable 2")

dev.off()

## Create a courier plot onscreen.
windows(family = "mono")
plot(1:10)
dev.off()

#### Journal Formating
#### examples of a journal requiremets

## 89 mm width, 8 point pdf, 300 ppi resolution, cmyk color

### pdf options
###XXX
pdf( "journal1.pdf",  width = 8.9/2.54, height = 8.9/2.54, pointsize = 8)
par(mar = c(4,4,1,1) )
plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , pch = 16, las = 1, 
	xlab = "Width", ylab = "Height")

dev.off()

## jpeg version

###XXX
jpeg( "journal1.jpeg",  width = 8.9, height = 8.9, pointsize = 8, unit = "cm", res = 300)

par(mar = c(4,4,1,1) )
plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , pch = 16, las = 1, 
	xlab = "Variable 1", ylab = "Variable 2")

dev.off()

### change color to cmyk 
###XXX 
pdf( "journal1.color.pdf",  
     width = 8.9/2.54, height = 8.9/2.54, 
     pointsize = 8, 
    colormodel = "cmyk")

par(mar = c(4,4,1,1) )
plot(iris$Sepal.Width, iris$Sepal.Length, col = as.factor(iris$Species) , pch = 16, las = 1, 
	xlab = "Variable 1", ylab = "Variable 2")

dev.off()

###############################################################
### Part D: ggplot 
###############################################################
### Note ggplot issues warning when records are not plotted because they are blank.  Generally, this is okay.
### ggsave is a wrapper/ convenience function which saves the previous plot.  It has been commented out.

library(ggplot2)

### check for default directory
streamDat <- read.table("Data/Graphics/streamData.reformat.csv", sep = ",", header = TRUE)

#### this is a simple example to show how efficient code for this package can be.
#### note this is a data object which by itself - does not return anything.

p <- ggplot( data = streamDat, aes( x = Q, y = DO) )

p + geom_point( aes(col = stream, shape = season),  size = 4) + geom_smooth()

#ggsave("ggexample.png")

#### stats layer - add a linear fit to each groiup

p + geom_point( aes(col = stream, shape = season),  size = 2) + 
     geom_smooth( aes( group = season), method = "lm", se = FALSE)

#ggsave("ggexample2.png")

### scale - change aspect of plot mapping data to colors
### assign specific characters


###
p + geom_point( aes(col = stream, shape = season),  size = 4) +  scale_shape_manual( name = "Seasons! ",values = c("1", "2", "3", "4" ) )  

## ggsave("ggexample3.png")


#### change to log scale
# The difference between transforming the scales and
# transforming the coordinate system is that scale
# transformation occurs BEFORE statistics, and coordinate
# transformation afterwards.  Coordinate transformation also
# changes the shape of geoms:


### change coordinate system in plots to log.

p + geom_point( aes(col = stream, shape = season),  size = 4) +
coord_trans(y = "log")

ggsave("ggexampleLogT.png")

#### Caution: xlim and ylim remove points prior to conducting stats


p + geom_point( aes(col = stream, shape = season),  size = 4) + geom_smooth( method  = "lm") +
xlim( 20,50) + ylim(20,100)

### alternatively use coord_cartesion if your desire is just to zoom in on a region - while having
### the statistics represent all the data in the dataframe.

p + geom_point( aes(col = stream, shape = season),  size = 4) + geom_smooth( method  = "lm") +
coord_cartesian(xlim = c( 20,50),  ylim= c(20,100) )

### faceting - make separate plots for each group.

p + geom_point( aes(col = stream),  size = 4) + facet_wrap(~season)


### two potentially useful options are controlling the number of rows used in the layout 
### and allowing the scales to change between plots

p + geom_point( aes(col = stream),  size = 4) + facet_wrap(~season, nrow = 4, scales = c("free"))

### tweaking attributes like fonts is commonly done using the option and theme commands

theme_get() ## shows how elements are currently set

opt1 <- theme(axis.text.x = element_text( color = "red" ))
opt3 <- theme(axis.text.x = element_blank() )  ## removes text or option

p + geom_point( aes(col = stream, shape = season),  size = 4) + opt1



