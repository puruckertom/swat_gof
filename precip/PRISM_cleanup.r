#memory.limit(size = 3000)
library(foreign)

#filepath <- path.expand("~/Dropbox/ktp/precip/PRISMdata/Katie_White/Interpolated_2001_2010_Katie/NC/")
filepath <- "D:/Dropbox/ktp/precip/PRISMdata/Katie_White/Interpolated_2001_2010_Katie/NC/"
station <- list.files(filepath)
station <- station[-which(station=="precip")] 
station <-station[-which(station=="temp")] 
dottxt <- ".txt"
dotdbf <- ".dbf"

for(i in 1:length(station)){ #length(station)
  readfile <- paste(filepath,station[i],sep="")
  cat(readfile, "\n")
  ppt <- read.csv(readfile, stringsAsFactors=FALSE,header=TRUE)
  stationname <-   gsub("_interpolate.csv","",station[i])
  tempdbf <- paste(filepath,"temp/",stationname,dotdbf,sep="")
  precipdbf <- paste(filepath,"precip/",stationname,dotdbf,sep="")
  dim(ppt)
  summary(ppt)
  colnames(ppt)

  #ppt$Tmax[ppt$Tmax==99999] <- NA
  #ppt$Tmin[ppt$Tmin==99999] <- NA
  #ppt$Prcp[ppt$Prcp==999.99] <- NA
  #split date up into day.month,year
  firstday <- ppt$DATE[1]
  lastday <- ppt$DATE[length(ppt$DATE)]
  
  #loop and add date fields
  for(j in 1:length(ppt$DATE)){
    ppt$month[j] <- strsplit(ppt$DATE[j],"/")[[1]][1]
    ppt$day[j] <- strsplit(ppt$DATE[j],"/")[[1]][2]
    ppt$year[j] <- strsplit(ppt$DATE[j],"/")[[1]][3]
  }
  #swap years and day
  firstday <- paste(ppt$year[1],"-",ppt$month[1],"-",ppt$day[1],sep="")
  lastday <- paste(ppt$year[length(ppt$year)],"-",ppt$month[length(ppt$month)],"-",ppt$day[length(ppt$day)],sep="")
  cat(lastday, "\n")
  startday <- as.numeric(as.Date(firstday))
  endday <- as.numeric(as.Date(lastday))

  for(k in 1:length(ppt$year)){
    ppt$dateformat[k] <- paste(ppt$year[k],"-",ppt$month[k],"-",ppt$day[k],sep="")
    ppt$daynumber[k] <- as.numeric(as.Date(ppt$dateformat[k]))
    if(k>1) ppt$dif <- ppt$daynumber[k]-ppt$daynumber[k-1]
  }
  ppt$dif[1] <- 1

  ppt2 <- startday:endday
  ppt2 <- as.matrix(ppt2)
  colnames(ppt2) <- "daynumber"
  ppt3 <- merge(ppt,ppt2,all.y=TRUE)
  daily.days <- rep(startday:endday)
  daily.days2 <- as.Date(daily.days,origin="1970-01-01")
  colnames(ppt3)
  ppt3$Prcp <- ppt3$PRCP_MM
  ppt3$Tmin <- ppt3$TMIN_C
  ppt3$Tmax <- ppt3$TMAX_C
  ppt3$Prcp[is.na(ppt3$Prcp)] <- -99.0
  ppt3$Tmin[is.na(ppt3$Tmin)] <- -99.0
  ppt3$Tmax[is.na(ppt3$Tmax)] <- -99.0

  xyu.daily <- data.frame(cbind(daily.days,ppt3$Prcp))
  dimnames(xyu.daily)[[2]] <- c("DATE","PCP")
  class(xyu.daily[,1]) <- "Date"
  xyu.daily[,1] <- daily.days2
  write.dbf(xyu.daily, precipdbf,factor2char=TRUE)

  t.daily <- data.frame(cbind(daily.days,ppt3$Tmax,ppt3$Tmin))
  dimnames(t.daily)[[2]] <- c("DATE","MAX", "MIN")
  class(t.daily[,1]) <- "Date"
  t.daily[,1] <- daily.days2
  write.dbf(t.daily, tempdbf,factor2char=TRUE)
}
