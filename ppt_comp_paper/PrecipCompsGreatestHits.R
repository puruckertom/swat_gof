if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/PPTCompPaper/ppt/")
}
#windows
if(.Platform$OS.type=="windows"){
  root_dir <- path.expand("d://Dropbox/ktp/PPTCompPaper/ppt/")
}

library(zoo)
library(xts)

MCds.N <- read.csv(paste(root_dir,"MCNd.csv",sep=""), header=T)
MCds.M <- read.csv(paste(root_dir,"MCMd.csv",sep=""), header=T)
LRds.N <- read.csv(paste(root_dir,"LRNd.csv",sep=""), header=T)
LRds.M <- read.csv(paste(root_dir,"LRMd.csv",sep=""), header=T)
NCds.N <- read.csv(paste(root_dir,"dailylist.NCN.csv",sep=""), header=T)
NCds.M <- read.csv(paste(root_dir,"dailylist.NCM.csv",sep=""), header=T)
FBds.N <- read.csv(paste(root_dir,"dailylist.FBN.csv",sep=""), header=T)
FBds.M <- read.csv(paste(root_dir,"dailylist.FBM.csv",sep=""), header=T)
MCdf.N <- read.csv(paste(root_dir,"MCNd_fullws.csv",sep=""), header=T)
MCdf.M <- read.csv(paste(root_dir,"MCMd_fullws.csv",sep=""), header=T)
LRdf.N <- read.csv(paste(root_dir,"LRNd_fullws.csv",sep=""), header=T)
LRdf.M <- read.csv(paste(root_dir,"LRMd_fullws.csv",sep=""), header=T)
NCdf.N <- read.csv(paste(root_dir,"NCNd_fullws.csv",sep=""), header=T)
NCdf.M <- read.csv(paste(root_dir,"NCMd_fullws.csv",sep=""), header=T)
FBdf.N <- read.csv(paste(root_dir,"FBNd_fullws.csv",sep=""), header=T)
FBdf.M <- read.csv(paste(root_dir,"FBMd_fullws.csv",sep=""), header=T)

sum(FBds.N[,2])
sum(FBds.M[,2])
sum(FBdf.N[,2])
sum(FBdf.M[,2])
#all the above files have been checked, and the FB total P values for NCDC and MPE are fine for both df and ds

######Replace double zeroes with NAs
col1.zeroes <- which(MCds.N[,2]==0)
col2.zeroes <- which(MCds.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(MCds.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(MCds.M[,2])
MPE[maketheseNA] <- NA
MCds.NA <- cbind(NCDC,MPE)
colnames(MCds.NA) = c("N","M")

col1.zeroes <- which(LRds.N[,2]==0)
col2.zeroes <- which(LRds.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(LRds.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(LRds.M[,2])
MPE[maketheseNA] <- NA
LRds.NA <- cbind(NCDC,MPE)
colnames(LRds.NA) = c("N","M")

col1.zeroes <- which(NCds.N[,2]==0)
col2.zeroes <- which(NCds.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(NCds.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(NCds.M[,2])
MPE[maketheseNA] <- NA
NCds.NA <- cbind(NCDC,MPE)
colnames(NCds.NA) = c("N","M")

col1.zeroes <- which(FBds.N[,2]==0)
col2.zeroes <- which(FBds.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(FBds.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(FBds.M[,2])
MPE[maketheseNA] <- NA
FBds.NA <- cbind(NCDC,MPE)
colnames(FBds.NA) = c("N","M")

write.csv(MCds.NA, file=paste(root_dir,"MCds.NA.csv",sep=""))
write.csv(LRds.NA, file=paste(root_dir,"LRds.NA.csv",sep=""))
write.csv(NCds.NA, file=paste(root_dir,"NCds.NA.csv",sep=""))
write.csv(FBds.NA, file=paste(root_dir,"FBds.NA.csv",sep=""))

col1.zeroes <- which(MCdf.N[,2]==0)
col2.zeroes <- which(MCdf.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(MCdf.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(MCdf.M[,2])
MPE[maketheseNA] <- NA
MCdf.NA <- cbind(NCDC,MPE)
colnames(MCdf.NA) = c("N","M")

col1.zeroes <- which(LRdf.N[,2]==0)
col2.zeroes <- which(LRdf.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(LRdf.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(LRdf.M[,2])
MPE[maketheseNA] <- NA
LRdf.NA <- cbind(NCDC,MPE)
colnames(LRdf.NA) = c("N","M")

col1.zeroes <- which(NCdf.N[,2]==0)
col2.zeroes <- which(NCdf.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(NCdf.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(NCdf.M[,2])
MPE[maketheseNA] <- NA
NCdf.NA <- cbind(NCDC,MPE)
colnames(NCdf.NA) = c("N","M")

col1.zeroes <- which(FBdf.N[,2]==0)
col2.zeroes <- which(FBdf.M[,2]==0)
maketheseNA <- intersect(col1.zeroes,col2.zeroes)
NCDC <- as.vector(FBdf.N[,2])
NCDC[maketheseNA] <- NA
MPE <- as.vector(FBdf.M[,2])
MPE[maketheseNA] <- NA
FBdf.NA <- cbind(NCDC,MPE)
colnames(FBdf.NA) = c("N","M")

write.csv(MCdf.NA, file=paste(root_dir,"MCdf.NA.csv",sep=""))
write.csv(LRdf.NA, file=paste(root_dir,"LRdf.NA.csv",sep=""))
write.csv(NCdf.NA, file=paste(root_dir,"NCdf.NA.csv",sep=""))
write.csv(FBdf.NA, file=paste(root_dir,"FBdf.NA.csv",sep=""))

#####DAILY t's

MCds.NA <- read.csv(paste(root_dir,"MCds.NA.csv",sep=""), header=T)
LRds.NA <- read.csv(paste(root_dir,"LRds.NA.csv",sep=""), header=T)
NCds.NA <- read.csv(paste(root_dir,"NCds.NA.csv",sep=""), header=T)
FBds.NA <- read.csv(paste(root_dir,"FBds.NA.csv",sep=""), header=T)
MCdf.NA <- read.csv(paste(root_dir,"MCdf.NA.csv",sep=""), header=T)
LRdf.NA <- read.csv(paste(root_dir,"LRdf.NA.csv",sep=""), header=T)
NCdf.NA <- read.csv(paste(root_dir,"NCdf.NA.csv",sep=""), header=T)
FBdf.NA <- read.csv(paste(root_dir,"FBdf.NA.csv",sep=""), header=T)

t.test1 <- t.test(MCds.NA[,2], MCds.NA[,3], na.rm=T, paired=T)
t.test2 <- t.test(LRds.NA[,2], LRds.NA[,3], na.rm=T, paired=T)
t.test3 <- t.test(NCds.NA[,2], NCds.NA[,3], na.rm=T, paired=T)
t.test4 <- t.test(FBds.NA[,2], FBds.NA[,3], na.rm=T, paired=T)
t.test5 <- t.test(MCdf.NA[,2], MCdf.NA[,3], na.rm=T, paired=T)
t.test6 <- t.test(LRdf.NA[,2], LRdf.NA[,3], na.rm=T, paired=T)
t.test7 <- t.test(NCdf.NA[,2], NCdf.NA[,3], na.rm=T, paired=T)
t.test8 <- t.test(FBdf.NA[,2], FBdf.NA[,3], na.rm=T, paired=T)

ttestout <- capture.output(t.test1, t.test2, t.test3, t.test4, t.test5, t.test6, t.test7, t.test8)
cat(ttestout, file=paste(root_dir,"PairedTsDaily.txt"), sep="\n")

#unstack dailies into dataframe by subbasin
MCds.N <- read.csv(paste(root_dir,"MCNd.csv",sep=""), header=T)
MCds.M <- read.csv(paste(root_dir,"MCMd.csv",sep=""), header=T)
LRds.N <- read.csv(paste(root_dir,"LRNd.csv",sep=""), header=T)
LRds.M <- read.csv(paste(root_dir,"LRMd.csv",sep=""), header=T)
NCds.N <- read.csv(paste(root_dir,"dailylist.NCN.csv",sep=""), header=T)
NCds.M <- read.csv(paste(root_dir,"dailylist.NCM.csv",sep=""), header=T)
FBds.N <- read.csv(paste(root_dir,"dailylist.FBN.csv",sep=""), header=T)
FBds.M <- read.csv(paste(root_dir,"dailylist.FBM.csv",sep=""), header=T)

MCSubIDs <- sort(c(rep(1:4,3287)))
LRSubIDs <- sort(c(rep(1:18,3287)))
NCSubIDs <- sort(c(rep(1:249,3287)))
FBSubIDs <- sort(c(rep(1:890,3287)))

MCds.N <- cbind(MCSubIDs,MCds.N[,-1])
MCds.M <- cbind(MCSubIDs,MCds.M[,-1])
LRds.N <- cbind(LRSubIDs,LRds.N[,-1])
LRds.M <- cbind(LRSubIDs,LRds.M[,-1])
NCds.N <- cbind(NCSubIDs,NCds.N[,-1])
NCds.M <- cbind(NCSubIDs,NCds.M[,-1])
FBds.N <- cbind(FBSubIDs,FBds.N[,-1])
FBds.M <- cbind(FBSubIDs,FBds.M[,-1])

MCds.N.df <- unstack(MCds.N[,2],MCds.N[,2]~MCds.N[,1])
MCds.M.df <- unstack(MCds.M[,2],MCds.M[,2]~MCds.M[,1])
LRds.N.df <- unstack(LRds.N[,2],LRds.N[,2]~LRds.N[,1])
LRds.M.df <- unstack(LRds.M[,2],LRds.M[,2]~LRds.M[,1])
NCds.N.df <- unstack(NCds.N[,2],NCds.N[,2]~NCds.N[,1])
NCds.M.df <- unstack(NCds.M[,2],NCds.M[,2]~NCds.M[,1])
FBds.N.df <- unstack(FBds.N[,2],FBds.N[,2]~FBds.N[,1])
FBds.M.df <- unstack(FBds.M[,2],FBds.M[,2]~FBds.M[,1])

#Sum Monthlies and Annuals for subbasin (as zoo)
startdate <- "2002-01-01"
enddate <- "2010-12-31"
Ndays <- (as.numeric(as.Date(enddate))-as.numeric(as.Date(startdate)))+1
Nweeks <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "7 day"))+1
Nmonths <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "month"))
Nquarters <- (length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "year")))*4 #assumes full years of data
Nyears <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "year"))

x.Dates.daily <- as.Date(startdate) + 1:Ndays -1
x.Dates.weekly <- seq(as.Date(startdate), by="7 day", length.out=Nweeks)
x.Dates.monthly <- seq(as.Date(startdate), by="month", length.out=Nmonths)
x.Dates.quarterly <- seq(as.Date(startdate), by="3 month", length.out=Nquarters)
x.Dates.yearly <- seq(as.Date(startdate), by="year", length.out=Nyears)

MCds.N.zoo <- zoo(MCds.N.df,x.Dates.daily)
MCds.M.zoo <- zoo(MCds.M.df,x.Dates.daily)
LRds.N.zoo <- zoo(LRds.M.df,x.Dates.daily)
LRds.M.zoo <- zoo(LRds.M.df,x.Dates.daily)
NCds.N.zoo <- zoo(NCds.N.df,x.Dates.daily)
NCds.M.zoo <- zoo(NCds.M.df,x.Dates.daily)
FBds.N.zoo <- zoo(FBds.N.df,x.Dates.daily)
FBds.M.zoo <- zoo(FBds.M.df,x.Dates.daily)
MCdf.N.zoo <- zoo(MCdf.N[,2],x.Dates.daily)
MCdf.M.zoo <- zoo(MCdf.M[,2],x.Dates.daily)
LRdf.N.zoo <- zoo(LRdf.N[,2],x.Dates.daily)
LRdf.M.zoo <- zoo(LRdf.M[,2],x.Dates.daily)
NCdf.N.zoo <- zoo(NCdf.N[,2],x.Dates.daily)
NCdf.M.zoo <- zoo(NCdf.M[,2],x.Dates.daily)
FBdf.N.zoo <- zoo(FBdf.N[,2],x.Dates.daily)
FBdf.M.zoo <- zoo(FBdf.M[,2],x.Dates.daily)



#Monthly and Yearly only for Full Watershed (as xts)
MCdf.N <- xts(MCdf.N)
MCdf.M <- xts(MCdf.M)
LRdf.N <- xts(LRdf.N)
LRdf.M <- xts(LRdf.M)
NCdf.N <- xts(NCdf.N)
NCdf.M <- xts(NCdf.M)
FBdf.N <- xts(FBdf.N)
FBdf.M <- xts(FBdf.M)

MCdf.N.m <- apply.monthly(MCdf.N, sum)
MCdf.M.m <- apply.monthly(MCdf.M, sum)
LRdf.N.m <- apply.monthly(LRdf.N, sum)
LRdf.M.m <- apply.monthly(LRdf.M, sum)
NCdf.N.m <- apply.monthly(NCdf.N, sum)
NCdf.M.m <- apply.monthly(NCdf.M, sum)
FBdf.N.m <- apply.monthly(FBdf.N, sum)
FBdf.M.m <- apply.monthly(FBdf.M, sum)

MCdf.N.y <- apply.yearly(MCdf.N, sum)
MCdf.M.y <- apply.yearly(MCdf.M, sum)
LRdf.N.y <- apply.yearly(LRdf.N, sum)
LRdf.M.y <- apply.yearly(LRdf.M, sum)
NCdf.N.y <- apply.yearly(NCdf.N, sum)
NCdf.M.y <- apply.yearly(NCdf.M, sum)
FBdf.N.y <- apply.yearly(FBdf.N, sum)
FBdf.M.y <- apply.yearly(FBdf.M, sum)

MonthlyPrecip <- as.data.frame(cbind(MCdf.N.m, MCdf.M.m, LRdf.N.m, LRdf.M.m, NCdf.N.m, NCdf.M.m, FBdf.N.m, FBdf.M.m))
AnnualPrecip <- as.data.frame(cbind(MCdf.N.y, MCdf.M.y, LRdf.N.y, LRdf.M.y, NCdf.N.y, NCdf.M.y, FBdf.N.y, FBdf.M.y))
colnames(MonthlyPrecip) <- c("MCN","MCM","LRN","LRM","NCN","NCM","FBN","FBM")
colnames(AnnualPrecip) <- c("MCN","MCM","LRN","LRM","NCN","NCM","FBN","FBM")
rownames(AnnualPrecip) <- c("2002","2003","2004","2005","2006","2007","2008","2009","2010")
write.csv(MonthlyPrecip, row.names=T, file=paste(root_dir, "MonthlyPrecip.csv", sep=""))
write.csv(AnnualPrecip, row.names=T, file=paste(root_dir, "AnnualPrecip.csv", sep=""))


