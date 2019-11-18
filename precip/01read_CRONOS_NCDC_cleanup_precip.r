memory.limit(size = 3000)
library(foreign)

if(exists("ppt")){rm(ppt)}
if(exists("ppt1")){rm(ppt1)}
if(exists("ppt2")){rm(ppt2)}
if(exists("ppt3")){rm(ppt3)}
if(exists("ppt4")){rm(ppt4)}


filepath <- "L://Public/purucker/Katie_data_fill/"
#filepath <- "C://Dropbox/RActive/Katie_data_fill/"

# complete list of NCDC stations
station <- c("310212","311285","311535","311677","311820","311881","312500","312515","312518","312993","313238","313555","313638","313919","314987","315123","316108","316122","316510","317069",
"317074","317079","317395","317400","317499","317516","317994","318430","318500","318706","319081","319100","319476","319923","441746","447925",
"CLA2","CLAY","DURH","KHNZ","KHRJ","KIGX","KJNX","KLHZ","KRDU","KRWI","KTDF","LAKE","NDUK","NRKM","OXFO","REED")
Nstations <- length(station)
#precip.cols <- 
#precip.stations <- 

dottxt <- ".txt"
dotdbf <- ".dbf"

#first figure out dimensions for big matrix
# which files to use, cronos, ncdc, or both
cronos <- vector(mode = "logical", length = Nstations)
CRONOS.readfile <- paste(filepath,"Price_daily.txt",sep="")
ppt0 <- read.csv(CRONOS.readfile, stringsAsFactors=FALSE)
ppt.stations <- unique(ppt0$station)

for(i in 1:Nstations){
  #CRONOS.readfile <- paste(filepath,"Price_Katie_",station[i],"_Daily",dottxt,sep="")  # replace with big file extracts
  NCDC.readfile <- paste(filepath,station[i],dottxt,sep="")
  cat(CRONOS.readfile, " = ",file.exists(CRONOS.readfile),"\n")
  cat(NCDC.readfile, " = ",file.exists(NCDC.readfile),"\n")
  #cronos[i] <- file.exists(CRONOS.readfile) #needs to be changed
  cronos[i] <- station[i] %in% ppt.stations
}

# the data range across all the data sets
minstartday = 1000000
maxendday = 0
for(i in 1:Nstations){
  if(cronos[i]==TRUE){
    # extract station from CRONOS.readfile    
    ppt1 <- ppt0[which(ppt0$station == station[i]),]
    firstday <- ppt1$ob[1]
    Nrows <- length(ppt1$ob)
    lastday <- ppt1$ob[Nrows]
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
    ppt1 <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
    firstday <- paste(ppt1$year[1],"-",ppt1$month[1],"-",ppt1$day[1],sep="")
    lastday <- paste(ppt1$year[length(ppt1$year)],"-",ppt1$month[length(ppt1$month)],"-",ppt1$day[length(ppt1$day)],sep="")  
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
ppt2 <- minstartday:maxendday
ppt2 <- as.matrix(ppt2)
colnames(ppt2) <- "daynumber"
# get precipitation data
for(i in 1:Nstations){
#for(i in 2:2){
  if(cronos[i]==TRUE){
    #CRONOS.readfile <- paste(filepath,"Price_Katie_",station[i],"_Daily",dottxt,sep="")
    #ppt <- read.csv(CRONOS.readfile, stringsAsFactors=FALSE)
    ppt <- ppt0[which(ppt0$station == station[i]),]
    ppt$precipdata <- array(data=NA, dim=length(ppt[,1]))
    headers <- colnames(ppt)
    precip.header <- c("precip")
    if(is.finite(match(precip.header,headers))){
      ppt$precipdata <- ppt[,which(headers %in% precip.header)]
      Nsplit <- length(unlist(strsplit(ppt[,2],"-")))/3
      ppt$year <- unlist(strsplit(ppt[,2],"-"))[seq(from=1,to=Nsplit*3-2,by=3)]
      ppt$month <- unlist(strsplit(ppt[,2],"-"))[seq(from=2,to=Nsplit*3-1,by=3)]
      ppt$day <- unlist(strsplit(ppt[,2],"-"))[seq(from=3,to=Nsplit*3,by=3)]
    }
  }
  else{
    NCDC.readfile <- paste(filepath,station[i],dottxt,sep="")
    if(file.exists(NCDC.readfile)){
      ppt <- read.csv(NCDC.readfile, stringsAsFactors=FALSE)
      ppt$precipdata <- array(data=NA, dim=length(ppt[,1]))
      headers <- colnames(ppt)
      precip.header <- c("Prcp")
      if(is.finite(match(precip.header,headers))){
        ppt$precipdata <- ppt[which(headers %in% precip.header)]
      }
        else{
          cat("FAIL- ",NCDC.readfile)
        }     
      }
      else{
        cat("FAIL- ",NCDC.readfile)
        ppt$precipdata <- array(data=NA, dim=length(ppt[,1]))
    }
  }
  # insert NAs where appropriate
  ppt$precipdata[ppt$precipdata==999.99] <- NA
  ppt$precipdata[ppt$precipdata=="T"] <- 0.0
  # take precipdata and populate data oblect with merge command
  if(!is.null(ppt$year)){
    for(j in 1:length(ppt$year)){
      ppt$dateformat[j] <- paste(ppt$year[j],"-",ppt$month[j],"-",ppt$day[j],sep="")
      ppt$daynumber[j] <- as.numeric(as.Date(ppt$dateformat[j]))
      if(j>1) ppt$dif <- ppt$daynumber[j]-ppt$daynumber[j-1]
    }
    ppt$dif[1] <- 1
    # ppt is not as long as it should be here
    
    ppt3 <- cbind(ppt$daynumber,ppt$precipdata)
    colnames(ppt3) <- c("daynumber",station[i])
  }
  else{
    nadays <- minstartday:maxendday
    navals <- array(data=NA, dim=c(length(nadays),1))
    ppt3 <- cbind(nadays,navals)
    colnames(ppt3) <- c("daynumber",station[i])  
  }
  print(length(ppt3[,1]))
  ppt2 <- merge(ppt3,ppt2,all=TRUE,by="daynumber")
  write.csv(ppt2,file=paste(filepath,"precipdata_",i,".csv",sep=""))
}
colnames(ppt2)
#ppt2[1:10,12]
write.csv(ppt2,file=paste(filepath,"precipdata_before.csv",sep=""))
# priority ranking of NA replacements
pcols <- read.csv(paste(filepath,"pcols.csv",sep=""),header=FALSE,stringsAsFactors=FALSE)

# now substitute values where appropriate according to pcols
ncols = length(colnames((ppt2)))
for(i in 2:ncols){
  class(ppt2[,i])="numeric"
}
ppt4 <- ppt2
for(i in 1:40){
  #for(i in 11:11){
  #target columns to replace NAs in
  targetstation <- pcols[i,1]
  targetcol <- match(pcols[i,1],colnames(ppt4))
  #colnames(ppt4)[targetcol]
  #ppt2[1:10,targetcol]
  #
  first <- pcols[i,3]
  firstcol <- match(pcols[first,1],colnames(ppt4))
  first.na.count <- apply(as.matrix(sapply(ppt4[,targetcol],is.na)),2,sum)
  #colnames(ppt4)[firstcol]
  if(first.na.count>0){
    ppt4[which(is.na(ppt4[,targetcol])),targetcol] <- ppt2[which(is.na(ppt4[,targetcol])),firstcol]
  }
  #length(ppt4[which(is.na(ppt4[,targetcol])),targetcol])
  #ppt2[1:10,firstcol]
  second <- pcols[i,4]
  secondcol <- match(pcols[second,1],colnames(ppt4))
  second.na.count <- apply(as.matrix(sapply(ppt4[,targetcol],is.na)),2,sum)
  #colnames(ppt4)[secondcol]
  if(second.na.count>0){
    ppt4[which(is.na(ppt4[,targetcol])),targetcol] <- ppt2[which(is.na(ppt4[,targetcol])),secondcol]
  }
  #length(ppt4[which(is.na(ppt4[,targetcol])),targetcol])
  #ppt2[1:10,secondcol]
  third <- pcols[i,5]
  thirdcol <- match(pcols[third,1],colnames(ppt4))
  third.na.count <- apply(as.matrix(sapply(ppt4[,targetcol],is.na)),2,sum)  
  #colnames(ppt4)[thirdcol]
  if(third.na.count>0){
    ppt4[which(is.na(ppt4[,targetcol])),targetcol] <- ppt2[which(is.na(ppt4[,targetcol])),thirdcol]
  }
  #length(ppt4[which(is.na(ppt4[,targetcol])),targetcol])
  #ppt2[1:10,thirdcol]
  fourth.na.count <- apply(as.matrix(sapply(ppt4[,targetcol],is.na)),2,sum)
  if(fourth.na.count>0){
    ppt4[which(!is.na(ppt4[,targetcol])),targetcol] <- ppt4[which(!is.na(as.numeric(ppt4[,targetcol]))),targetcol]*25.4
    ppt4[which(is.na(ppt4[,targetcol])),targetcol] <- -99.0
  }
  else{
    ppt4[,targetcol] <- as.numeric(ppt4[,targetcol])*25.4
  } 

  # write out dbf files for SWAT 
  #length(daily.days) 
  #length(ppt4[,targetcol])
  #length(ppt2[,targetcol])
  xyu.daily <- data.frame(cbind(daily.days,ppt4[,targetcol]))
  dimnames(xyu.daily)[[2]] <- c("DATE","PCP")
  class(xyu.daily[,1]) <- "Date"
  xyu.daily[,1] <- daily.days2
  precipdbf <- paste(filepath,targetstation,"p",dotdbf,sep="")
  write.dbf(xyu.daily,precipdbf,factor2char=TRUE)
}

write.csv(ppt4,file=paste(filepath,"precipdata_after.csv",sep=""))
#Precip.Temp.Data(,,1) <- ppt4                     
