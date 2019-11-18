memory.limit(size = 3000)
library(foreign)

if(exists("tmax")){rm(tmax)}
if(exists("tmax1")){rm(tmax1)}
if(exists("tmax2")){rm(tmax2)}
if(exists("tmax3")){rm(tmax3)}
if(exists("tmax4")){rm(tmax4)}

if(exists("tmin")){rm(tmin)}
if(exists("tmin1")){rm(tmin1)}
if(exists("tmin2")){rm(tmin2)}
if(exists("tmin3")){rm(tmin3)}
if(exists("tmin4")){rm(tmin4)}

filepath <- "L://Public/purucker/Katie_data_fill/"
#filepath <- "C://Dropbox/RActive/Katie_data_fill/"

# complete list of NCDC stations
station <- c("310212","311285","311535","311677","311820","311881","312500","312515","312518","312993","313238","313555","313638","313919","314987","315123","316108","316122","316510","317069",
"317074","317079","317395","317400","317499","317516","317994","318430","318500","318706","319081","319100","319476","319923","441746","447925",
"CLA2","CLAY","DURH","KHNZ","KHRJ","KIGX","KJNX","KLHZ","KRDU","KRWI","KTDF","LAKE","NDUK","NRKM","OXFO","REED")
Nstations <- length(station)
#tempmax.cols <- 
#tempmax.stations <- 

dottxt <- ".txt"
dotdbf <- ".dbf"

#first figure out dimensions for big matrix
# which files to use, cronos, ncdc, or both
cronos <- vector(mode = "logical", length = Nstations)
CRONOS.readfile <- paste(filepath,"Price_daily.txt",sep="")
tmax0 <- read.csv(CRONOS.readfile, stringsAsFactors=FALSE)
tmax.stations <- unique(tmax0$station)
tmin0 <- read.csv(CRONOS.readfile, stringsAsFactors=FALSE)
tmin.stations <- unique(tmin0$station)

for(i in 1:Nstations){
  #CRONOS.readfile <- paste(filepath,"Price_Katie_",station[i],"_Daily",dottxt,sep="")  # replace with big file extracts
  NCDC.readfile <- paste(filepath,station[i],dottxt,sep="")
  cat(CRONOS.readfile, " = ",file.exists(CRONOS.readfile),"\n")
  cat(NCDC.readfile, " = ",file.exists(NCDC.readfile),"\n")
  #cronos[i] <- file.exists(CRONOS.readfile) #needs to be changed
  cronos[i] <- station[i] %in% tmax.stations
}

# the data range across all the data sets
minstartday = 1000000
maxendday = 0
for(i in 1:Nstations){
  if(cronos[i]==TRUE){
    # extract station from CRONOS.readfile    
    tmax1 <- tmax0[which(tmax0$station == station[i]),]
    tmin1 <- tmin0[which(tmin0$station == station[i]),]
    firstday <- tmax1$ob[1]
    Nrows <- length(tmax1$ob)
    lastday <- tmax1$ob[Nrows]
    startday <- as.numeric(as.Date(firstday,origin="1970-01-01"))
    minstartday = min(minstartday,startday)
    mindaytext = as.Date(minstartday,origin="1970-01-01")
    print(paste("start: cronos",station[i],"=",mindaytext))
    endday <- as.numeric(as.Date(lastday,origin="1970-01-01"))
    maxendday = max(maxendday,endday)
    maxdaytext = as.Date(maxendday,origin="1970-01-01")
    print(paste("end: cronos",station[i],"=",maxdaytext))
  }
  else{
    NCDC.readfile <- paste(filepath,station[i],dottxt,sep="")
    tmax1 <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
    tmin1 <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
    firstday <- paste(tmax1$year[1],"-",tmax1$month[1],"-",tmax1$day[1],sep="")
    lastday <- paste(tmax1$year[length(tmax1$year)],"-",tmax1$month[length(tmax1$month)],"-",tmax1$day[length(tmax1$day)],sep="")  
    startday <- as.numeric(as.Date(firstday,origin="1970-01-01"))
    minstartday = min(minstartday,startday)
    mindaytext = as.Date(minstartday,origin="1970-01-01")
    print(paste("start: ncdc",station[i],"=",mindaytext))
    endday <- as.numeric(as.Date(lastday,origin="1970-01-01"))
    maxendday = max(maxendday,endday)
    maxdaytext = as.Date(maxendday,origin="1970-01-01")
    print(paste("end: ncdc",station[i],"=",maxdaytext))    
  }
}

#minstartday should be Jan 1, 2001
#maxendday shoudl be Dec 31, 2010

daily.days <- rep(minstartday:maxendday)
daily.days2 <- as.Date(daily.days,origin="1970-01-01")
Ndays = maxendday-minstartday+1
# bigmatrix is 3591 (Ndays between s3 (precip and temp), 54 stations, 3 slices for variables
Precip.Temp.Data <- array(data=NA, dim=c(Ndays,Nstations+1,3)) 
tmax2 <- minstartday:maxendday
tmax2 <- as.matrix(tmax2)
colnames(tmax2) <- "daynumber"
tmin2 <- minstartday:maxendday
tmin2 <- as.matrix(tmin2)
colnames(tmin2) <- "daynumber"

# get tempmax data
for(i in 1:Nstations){
#for(i in 2:2){
  if(cronos[i]==TRUE){
    #CRONOS.readfile <- paste(filepath,"Price_Katie_",station[i],"_Daily",dottxt,sep="")
    #tmax <- read.csv(CRONOS.readfile, stringsAsFactors=FALSE)
    tmax <- tmax0[which(tmax0$station == station[i]),]
    tmax$tempmaxdata <- array(data=NA, dim=length(tmax[,1]))
    headers <- colnames(tmax)
    tempmax.header <- c("tempmax")
    if(is.finite(match(tempmax.header,headers))){
      tmax$tempmaxdata <- tmax[,which(headers %in% tempmax.header)]
      Nsplit <- length(unlist(strsplit(tmax[,2],"-")))/3
      tmax$year <- unlist(strsplit(tmax[,2],"-"))[seq(from=1,to=Nsplit*3-2,by=3)]
      tmax$month <- unlist(strsplit(tmax[,2],"-"))[seq(from=2,to=Nsplit*3-1,by=3)]
      tmax$day <- unlist(strsplit(tmax[,2],"-"))[seq(from=3,to=Nsplit*3,by=3)]
    }
    tmin <- tmin0[which(tmin0$station == station[i]),]
    tmin$tempmindata <- array(data=NA, dim=length(tmin[,1]))
    headers <- colnames(tmin)
    tempmin.header <- c("tempmin")
    if(is.finite(match(tempmin.header,headers))){
      tmin$tempmindata <- tmin[,which(headers %in% tempmin.header)]
      Nsplit <- length(unlist(strsplit(tmin[,2],"-")))/3
      tmin$year <- unlist(strsplit(tmin[,2],"-"))[seq(from=1,to=Nsplit*3-2,by=3)]
      tmin$month <- unlist(strsplit(tmin[,2],"-"))[seq(from=2,to=Nsplit*3-1,by=3)]
      tmin$day <- unlist(strsplit(tmin[,2],"-"))[seq(from=3,to=Nsplit*3,by=3)]
    }
  }
  else{
    NCDC.readfile <- paste(filepath,station[i],dottxt,sep="")
    #temp max
    if(file.exists(NCDC.readfile)){
      tmax <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
      tmax$tempmaxdata <- array(data=NA, dim=length(tmax[,1]))
      headers <- colnames(tmax)
      tempmax.header <- c("Tmax")
      if(is.finite(match(tempmax.header,headers))){
        tmax$tempmaxdata <- tmax[which(headers %in% tempmax.header)]
      }
        else{
          cat("FAIL- ",NCDC.readfile)
        }     
      }
      else{
        cat("FAIL- ",NCDC.readfile)
        tmax$tempmaxdata <- array(data=NA, dim=length(tmax[,1]))
    }
    # temp min
    if(file.exists(NCDC.readfile)){
      tmin <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
      tmin$tempmindata <- array(data=NA, dim=length(tmin[,1]))
      headers <- colnames(tmin)
      tempmin.header <- c("Tmin")
      if(is.finite(match(tempmin.header,headers))){
        tmin$tempmindata <- tmin[which(headers %in% tempmin.header)]
      }
        else{
          cat("FAIL- ",NCDC.readfile)
        }     
      }
      else{
        cat("FAIL- ",NCDC.readfile)
        tmin$tempmindata <- array(data=NA, dim=length(tmin[,1]))
    }
  }
  # insert NAs where appropriate
  tmax$tempmaxdata[tmax$tempmaxdata==999.99] <- NA
  #tmax$tempmaxdata[tmax$tempmaxdata=="T"] <- 0.0
  tmin$tempmindata[tmin$tempmindata==999.99] <- NA
  #tmax$tempmaxdata[tmax$tempmaxdata=="T"] <- 0.0

  # take tempmaxdata and populate data oblect with merge command
  if(!is.null(tmax$year)){
    for(j in 1:length(tmax$year)){
      tmax$dateformat[j] <- paste(tmax$year[j],"-",tmax$month[j],"-",tmax$day[j],sep="")
      tmax$daynumber[j] <- as.numeric(as.Date(tmax$dateformat[j]))
      if(j>1) tmax$dif <- tmax$daynumber[j]-tmax$daynumber[j-1]
    }
    tmax$dif[1] <- 1
    # tmax is not as long as it should be here
    
    tmax3 <- cbind(tmax$daynumber,tmax$tempmaxdata)
    colnames(tmax3) <- c("daynumber",station[i])
  }
  else{
    nadays <- minstartday:maxendday
    navals <- array(data=NA, dim=c(length(nadays),1))
    tmax3 <- cbind(nadays,navals)
    colnames(tmax3) <- c("daynumber",station[i])  
  }
  
  # take tempmindata and populate data object with merge command
  if(!is.null(tmin$year)){
    for(j in 1:length(tmin$year)){
      tmin$dateformat[j] <- paste(tmin$year[j],"-",tmin$month[j],"-",tmin$day[j],sep="")
      tmin$daynumber[j] <- as.numeric(as.Date(tmin$dateformat[j]))
      if(j>1) tmin$dif <- tmin$daynumber[j]-tmin$daynumber[j-1]
    }
    tmin$dif[1] <- 1
    # tmin is not as long as it should be here
    
    tmin3 <- cbind(tmin$daynumber,tmin$tempmindata)
    colnames(tmin3) <- c("daynumber",station[i])
  }
  else{
    nadays <- minstartday:maxendday
    navals <- array(data=NA, dim=c(length(nadays),1))
    tmin3 <- cbind(nadays,navals)
    colnames(tmin3) <- c("daynumber",station[i])  
  }
  
  #max
  print(length(tmax3[,1]))
  tmax2 <- merge(tmax3,tmax2,all=TRUE,by="daynumber")
  write.csv(tmax2,file=paste(filepath,"tempmaxdata_",i,".csv",sep=""))
  tmin2 <- merge(tmin3,tmin2,all=TRUE,by="daynumber")
  write.csv(tmin2,file=paste(filepath,"tempmindata_",i,".csv",sep=""))
}

colnames(tmax2)
#tmax2[1:10,12]
write.csv(tmax2,file=paste(filepath,"tempmaxdata_before.csv",sep=""))
write.csv(tmin2,file=paste(filepath,"tempmindata_before.csv",sep=""))
# priority ranking of NA replacements
tcols <- read.csv(paste(filepath,"tcols.csv",sep=""),header=FALSE,stringsAsFactors=FALSE)

# now substitute values where appropriate according to tcols
ncols = length(colnames((tmax2)))
for(i in 2:ncols){
  class(tmax2[,i])="numeric"
}
ncols = length(colnames((tmin2)))
for(i in 2:ncols){
  class(tmin2[,i])="numeric"
}
tmax4 <- tmax2
tmin4 <- tmin2
for(i in 1:33){
  #for(i in 11:11){
  #target columns to replace NAs in
  targetstation <- tcols[i,1]
  targetcol <- match(tcols[i,1],colnames(tmax4))
  #colnames(tmax4)[targetcol]
  #tmax2[1:10,targetcol]
  #
  first <- tcols[i,3]
  firstcol <- match(tcols[first,1],colnames(tmax4))
  #colnames(tmax4)[firstcol]
  first.na.max.count <- apply(as.matrix(sapply(tmax4[,targetcol],is.na)),2,sum)
  first.na.min.count <- apply(as.matrix(sapply(tmin4[,targetcol],is.na)),2,sum)
  if(first.na.max.count>0){
    tmax4[which(is.na(tmax4[,targetcol])),targetcol] <- tmax2[which(is.na(tmax4[,targetcol])),firstcol]
  }
  if(first.na.min.count>0){  
    tmin4[which(is.na(tmin4[,targetcol])),targetcol] <- tmin2[which(is.na(tmin4[,targetcol])),firstcol]
  }
  #length(tmax4[which(is.na(tmax4[,targetcol])),targetcol])
  #tmax2[1:10,firstcol]
  second <- tcols[i,4]
  secondcol <- match(tcols[second,1],colnames(tmax4))
  second.na.max.count <- apply(as.matrix(sapply(tmax4[,targetcol],is.na)),2,sum)  
  second.na.min.count <- apply(as.matrix(sapply(tmin4[,targetcol],is.na)),2,sum)  
  #colnames(tmax4)[secondcol]
  if(second.na.max.count>0){
    tmax4[which(is.na(tmax4[,targetcol])),targetcol] <- tmax2[which(is.na(tmax4[,targetcol])),secondcol]
  }
  if(second.na.min.count>0){
    tmin4[which(is.na(tmin4[,targetcol])),targetcol] <- tmin2[which(is.na(tmin4[,targetcol])),secondcol]
  }
  #length(tmax4[which(is.na(tmax4[,targetcol])),targetcol])
  #tmax2[1:10,secondcol]
  third <- tcols[i,5]
  thirdcol <- match(tcols[third,1],colnames(tmax4))
  third.na.max.count <- apply(as.matrix(sapply(tmax4[,targetcol],is.na)),2,sum)  
  third.na.min.count <- apply(as.matrix(sapply(tmin4[,targetcol],is.na)),2,sum)
  #colnames(tmax4)[thirdcol]
  if(third.na.max.count>0){
    tmax4[which(is.na(tmax4[,targetcol])),targetcol] <- tmax2[which(is.na(tmax4[,targetcol])),thirdcol]
  }
  if(third.na.min.count>0){  
    tmin4[which(is.na(tmin4[,targetcol])),targetcol] <- tmin2[which(is.na(tmin4[,targetcol])),thirdcol]
  }
  #length(tmax4[which(is.na(tmax4[,targetcol])),targetcol])
  #tmax2[1:10,thirdcol]
  #max
  fourth.na.max.count <- apply(as.matrix(sapply(tmax4[,targetcol],is.na)),2,sum)   
  if(fourth.na.max.count>0){
    tmax4[which(!is.na(tmax4[,targetcol])),targetcol] <- (tmax4[which(!is.na(tmax4[,targetcol])),targetcol]-32)*5/9
    tmax4[which(is.na(tmax4[,targetcol])),targetcol] <- -99.0
  }
  else{
    tmax4[,targetcol] <- (tmax4[,targetcol]-32)*5/9
  } 
  #min might have different NAs
  fourth.na.min.count <- apply(as.matrix(sapply(tmin4[,targetcol],is.na)),2,sum)   
  if(fourth.na.min.count>0){
    tmin4[which(!is.na(tmin4[,targetcol])),targetcol] <- (tmin4[which(!is.na(tmin4[,targetcol])),targetcol]-32)*5/9
    tmin4[which(is.na(tmin4[,targetcol])),targetcol] <- -99.0
  }
  else{
    tmin4[,targetcol] <- (tmin4[,targetcol]-32)*5/9
  } 

  
  # write out dbf files for SWAT 
  #length(daily.days) 
  #length(tmax4[,targetcol])
  #length(tmax2[,targetcol])
  xyu.daily <- data.frame(cbind(daily.days,tmin4[,targetcol],tmax4[,targetcol]))
  dimnames(xyu.daily)[[2]] <- c("DATE","MIN","MAX")
  class(xyu.daily[,1]) <- "Date"
  xyu.daily[,1] <- daily.days2
  tempdbf <- paste(filepath,targetstation,"t",dotdbf,sep="")
  write.dbf(xyu.daily,tempdbf,factor2char=TRUE)
}

write.csv(tmin4,file=paste(filepath,"tempmindata_after.csv",sep=""))
write.csv(tmax4,file=paste(filepath,"tempmaxdata_after.csv",sep=""))
#Precip.Temp.Data(,,1) <- tmax4                     