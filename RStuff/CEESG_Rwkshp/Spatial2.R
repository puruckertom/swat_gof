library(ncdf)
library(raster)
library(RNetCDF)

ncdf.root <- path.expand("~/Dropbox/ktp/RStuff/CEESG_Rwkshp/FiveTestDays/")


r1 <- raster(paste(ncdf.root,"ST4_2008060900_01h.nc",sep=""))
r2 <- raster(paste(ncdf.root,"ST4_2008060901_01h.nc",sep=""))


f <- list.files(path.expand("~/Dropbox/ktp/RStuff/CEESG_Rwkshp/FiveTestDays/"), 
                  pattern='nc$', full=T)
f= sort(f)
s <- stack(f)
time = substr(basename(f), 5,12)
dn = substr(basename(f), 13,14)
time[1]
dn[1]


format="%m/%d/%y"
date = as.Date(time, "%Y%M%D")

#brick.1 <- open.nc(paste(ncdf.root,"ST4_2008060900_01h.nc",sep=""))
hour.1 <- open.ncdf(paste(ncdf.root,"ST4_2008060900_01h.nc",sep=""))
hour.1
># Reading file with ncdf package to examine its strucuture.
  >> n <- open.ncdf ('~/Dropbox/web/soita.clay.nc')
>> n                  
>[1] "file ~/Dropbox/web/soita.clay.nc has 5 dimensions:"
>[1] "longitude   Size: 720"
>[1] "latitude   Size: 360"
>[1] "layer   Size: 6"
>[1] "time   Size: 1"
>[1] "lengthd   Size: 10"
>[1] "------------------------"
>[1] "file ~/Dropbox/web/soita.clay.nc has 1 variables:"
>[1] "double claypct[longitude,latitude,layer,time]  Longname:claypct Missval:1e+30"
>

r1 <- raster(hour.1,lvar=1)

#f <- "soita.clay.nc"
#b <- brick(f, lvar=4)
#NAvalue(b) <- 9e+20
#plot(b)
#OR#
#r1 <- raster(f, level=1)
#r2 <- raster(f, level=2)
#r3 <- raster(f, level=3)
#r4 <- raster(f, level=4)
#r5 <- raster(f, level=5)
#r6 <- raster(f, level=6)
#s <- stack(r1, r2, r3, r4, r5, r6)

#f <- "soita.clay.nc"
#b <- brick(f, lvar=4)
#NAvalue(b) <- 9e+20
#plot(b)