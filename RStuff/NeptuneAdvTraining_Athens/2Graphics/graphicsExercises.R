#### Advanced R Training
#### Graphics Session
#### Solutions are provided below.  

### Part 1 - Modifying basic plots

### 1. Read in water quality  data found in Data/Graphics/WQ.csv  and assign it object wqDat.  Note: in this file missing values are coded as -999999
wqDat. <- read.csv("~/Dropbox/ktp/RStuff/NeptuneAdvTraining_Athens/Data/Graphics/WQ.csv",sep=",",header=T)

### 2. Enhancing basic plots
### 	a.  Plot N_tot_Kj vs P_tot and color code by Site
### 	b.  Add legend
###		c.	Rotate axis text on y-axis to be perpendicular 
### 	d   Reduce font size for axis text to cex  = 0.8
###	    e.  Remove excess white space in upper and right margins
##      f.  Save as a png file with 8 point font, 300 ppi and a height and width of 3.5 inches
 	
### 3.  Making mulitple plots
###     a. Create a figure with separate plots for each site.  Hint par(mfrow = c(2,3)  ) will create a layout for 6 plots. 
###     b. Place labels A, B, ..E in the upper left corner of each plot.
 	 
plot(wqDat.$N_tot_Kj,wqDat.$P_tot) 	 
### Part 2 - ggplot

### 4.  Perform similar actions with ggplot
### 	a. create a ggplot data object
###		b. create a points plot using the geom_point function
###		c. code each site by color
###		d. create a linear fit with confidence itervals for each site data separately.
###		e. create a separate fit through each site group, suppress the confidence intervals.
###		f. Use facet wrap to create a separate plot for each site with a fitted line. 
###		g. Allow the axes to change between plots


### Solutions
### Part 1 - Modifying basic plots

### 1. Read in water quality  data found in Data/Graphics/WQ.csv  and assign it object wqDat.  Note: in this file missing values are coded as -999999
wqDat <- read.table("~/Dropbox/ktp/RStuff/NeptuneAdvTraining_Athens/Data/Graphics/WQ.csv", sep = ",", header = TRUE, na.strings = "-999999") ### assumes you are in AdvancedR.Fall2012


### 2. Enhancing basic plots
### 	a.  Plot N_tot_Kj vs P_tot and color code by Site
### 	b.  Add legend
###		c.	Rotate axis text on y-axis to be perpendicular 
### 	d   Reduce font size for axis text to cex  = 0.8
###	    e.  Remove excess white space in upper and right margins
par(mar = c(4,4,1,1) )
plot(wqDat$N_tot_Kj, wqDat$P_tot,  xlab = "N_total", ylab= "P_total",
     col = wqDat$Site, 
     cex.axis = 0.8,
     las = 1,
     pch = 16)


legend( "topleft", inset = 0.05, legend = levels(wqDat$Site), 
         col = 1:6, pch = 16 )


##      f.  Save as a png file with 8 point font, 300 ppi and a height and width of 3.5 inches
png( "test1.png", width = 3.5, height = 3.5, units = "in", pointsize = 8, res = 300)         

par(mar = c(4,4,1,1) )
plot(wqDat$N_tot_Kj, wqDat$P_tot,  xlab = "N_total", ylab= "P_total",
     col = wqDat$Site, 
     cex.axis = 0.8,
     las = 1,
     pch = 16)


legend( "topleft", inset = 0.05, legend = levels(wqDat$Site), 
         col = 1:6, pch = 16 )

dev.off()



 	
### 3.  Making mulitple plots
###     a. Create a figure with separate plots for each site.  Hint par(mfrow = c(2,3)  ) will create a layout for 6 plots. 
###     b. Place labels A, B, ..E in the upper left corner of each plot.
 
 SITES <- unique(wqDat$Site) ### Identify unique sites
LABELS <- LETTERS[1:6]  ### create labels

par(mfrow = c(3,2), mar = c(4,4,2,1 ) )

for(i in 1:6){
	
	sub <- subset(wqDat, Site == SITES[i] )
	plot(sub$N_tot_Kj, sub$P_tot,   xlab = "N_total", ylab= "P_total",
     main = paste("Site:", SITES[i]), 
     cex.axis = 0.8,
     las = 1,
     pch = 16)

usrCorners <- par('usr')
text( x = usrCorners[1] + 0.1 * (usrCorners[2]- usrCorners[1]), ## 10% across x axis
      y = usrCorners[4] - 0.1 * (usrCorners[4]- usrCorners[3]), ## 10% from top
      label = LETTERS[i], font = 2, cex = 2, col = 2
) 

	
}
	 
 	 
### Part 2 - ggplot

### 4.  Perform similar actions with ggplot
### 	a. create a ggplot data object
library(ggplot2)
p  <- ggplot(data  = wqDat, aes(x = N_tot_Kj, y = P_tot) )

###		b. create a points plot using the geom_point function
p + geom_point()

###		c. code each site by color
p + geom_point(aes(col = Site))

###		d. create a linear fit with confidence itervals for each site data separately.

p + geom_point(aes(col = Site)) + stat_smooth()
p + geom_point(aes(col = Site)) + stat_smooth(method = "lm")

###		e. create a separate fit through each site group, suppress the confidence intervals.
p + geom_point(aes(col = Site)) + stat_smooth(aes(group = Site, col = Site), method = "lm", se = FALSE)


###		f. Use facet wrap to create a separate plot for each site with a fitted line. 
p + geom_point(aes(col = Site)) + stat_smooth(method = "lm") + facet_wrap(~Site)

###		g. Allow the axes to change between plots

p + geom_point(aes(col = Site)) + stat_smooth(method = "lm") + facet_wrap(~Site, scale = "free")




