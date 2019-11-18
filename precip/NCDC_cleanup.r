memory.limit(size = 3000)
library(foreign)

filepath <- "L://Lab/GIS/static/MEERT/Neuse/NCDC/DataFiles/Raw/Daily/2001-2010/"
station <- c("310212","311535","311677","311820","311881","312500","312515","312993","313555","313638","313919","315123","316108","316122","316510","317069",
"317074","317079","317395","317400","317516","317994","318500","318706","319081","319100","319476","319923","441746","447925")
dottxt <- ".txt"
dotdbf <- ".dbf"

for(i in 1:length(station)){
  readfile <- paste(filepath,station[i],dottxt,sep="")
  cat(readfile, "\n")
  tempdbf <- paste(filepath,station[i],"t",dotdbf,sep="")
  precipdbf <- paste(filepath,station[i],"p",dotdbf,sep="")
  ppt <- read.csv(readfile, stringsAsFactors=FALSE)
  dim(ppt)
  summary(ppt)
  colnames(ppt)

  ppt$Tmax[ppt$Tmax==99999] <- NA
  ppt$Tmin[ppt$Tmin==99999] <- NA
  ppt$Prcp[ppt$Prcp==999.99] <- NA
  firstday <- paste(ppt$year[1],"-",ppt$month[1],"-",ppt$day[1],sep="")
  lastday <- paste(ppt$year[length(ppt$year)],"-",ppt$month[length(ppt$month)],"-",ppt$day[length(ppt$day)],sep="")
  cat(lastday, "\n")
  startday <- as.numeric(as.Date(firstday))
  endday <- as.numeric(as.Date(lastday))

  for(i in 1:length(ppt$year)){
    ppt$dateformat[i] <- paste(ppt$year[i],"-",ppt$month[i],"-",ppt$day[i],sep="")
    ppt$daynumber[i] <- as.numeric(as.Date(ppt$dateformat[i]))
    if(i>1) ppt$dif <- ppt$daynumber[i]-ppt$daynumber[i-1]
  }
  ppt$dif[1] <- 1

  ppt2 <- startday:endday
  ppt2 <- as.matrix(ppt2)
  colnames(ppt2) <- "daynumber"
  ppt3 <- merge(ppt,ppt2,all.y=TRUE)
  daily.days <- rep(startday:endday)
  daily.days2 <- as.Date(daily.days,origin="1970-01-01")
  ppt3$Prcp <- as.numeric(ppt3$Prcp)*25.4
  ppt3$Tmin <- (ppt3$Tmin-32)*5/9
  ppt3$Tmax <- (ppt3$Tmax-32)*5/9
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
