### flow.modeled.*time should always be dimensioned as [Ntime,Nsim]
### flow.modeled.*time.zoo should always be dimensioned as [Ntime,Nsim]
### flow.modeled.*time.xts should always be dimensioned as [Ntime,Nsim]

# clean up rough flow to make a 2-d array
dim(rough.flow.modeled) #[1] 2740499       1 #65792871
working.flow.modeled <- as.vector(t(rough.flow.modeled))
class(working.flow.modeled)
length(working.flow.modeled)

# get rid of non-data elements- the simulation numbers
nrows <- length(working.flow.modeled)

## for daily processing
# not 1 because of the column header
del.seq <- seq(from=(Ndays+1), length.out=Nsim, by=(Ndays+1))
#working.flow.modeled[del.seq[1:30]]
#working.flow.modeled[del.seq[1499]]
#working.flow.modeled[del.seq[2]] # should be "   3 "
working.flow.modeled <-working.flow.modeled[-del.seq]
#length(working.flow.modeled)
#working.flow.modeled[length(working.flow.modeled)]
#split out numeric flow values from character vector
#below yields a regular expression that has to be unlisted- tripling the vector length
split.flow.modeled <- unlist(strsplit(working.flow.modeled, " ",fixed=TRUE))
#get the days
seq.days <- seq(from=1,by=3,length=Ndays)
day.list <- as.numeric(split.flow.modeled[seq.days])
# get the Nsim Ndays modeled results and stuff them into a 2-d matrix
flow.modeled.daily <- array(data=NA, dim=c(Nsim,Ndays))
dim(flow.modeled.daily) # 1500 1826 #2001 3286
for(i in 1:Nsim){
  beg.seq <- ((i-1)*Ndays*3)+3
  flow.seq <- seq(from=beg.seq,by=3,length=Ndays)
  temp.modeled <- as.numeric(split.flow.modeled[flow.seq])
  #summary(temp.modeled)
  flow.modeled.daily[i,] <- temp.modeled
}

#create a time series object for monthly
days2days <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "day")
days2weeks <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "7 day")
days2months <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "month")
days2quarters <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "3 month")
days2years <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "year")

length(days2days) #2191 = 6*365
length(days2weeks) #313 =~6*52
length(days2months) #72 = 6*12
length(days2quarters) #=6*4
length(days2years) #=6

dim(flow.modeled.daily) #Nsim Ndays
flow.modeled.daily.zoo <- zoo(t(flow.modeled.daily), order.by=days2days)
dim(flow.modeled.daily.zoo)
###as.xts assumes by default that the dates are stored in the rownames of the data.frame!!!
flow.modeled.daily.xts <- as.xts(flow.modeled.daily.zoo)
dim(flow.modeled.daily.xts)
#convert from daily to monthly- using zoo
flow.modeled.daily <- t(flow.modeled.daily)
flow.modeled.weekly <- array(data=NA, dim=c(Nweeks,Nsim))
flow.modeled.monthly <- array(data=NA, dim=c(Nmonths,Nsim))
flow.modeled.quarterly <- array(data=NA, dim=c(Nquarters,Nsim))
flow.modeled.yearly <- array(data=NA, dim=c(Nyears,Nsim))
for(i in 1:Nsim){
  #flow.modeled.monthly[,i] <- as.vector(aggregateMonthlySeries(flow.modeled.zoo[,i], FUNC=mean))
  flow.modeled.weekly[,i] <- as.vector(apply.weekly(flow.modeled.daily.xts[,i], FUN=mean))
  flow.modeled.monthly[,i] <- as.vector(aggregate(flow.modeled.daily.zoo[,i], by=as.yearmon, FUN=mean))
  flow.modeled.quarterly[,i] <- as.vector(aggregate(flow.modeled.daily.zoo[,i], by=as.yearqtr, FUN=mean))
  flow.modeled.yearly[,i] <- as.vector(apply.yearly(flow.modeled.daily.xts[,i], FUN=mean))
}
dim(flow.modeled.daily)
dim(flow.modeled.weekly)
dim(flow.modeled.monthly)
dim(flow.modeled.quarterly)
dim(flow.modeled.yearly)

summary(flow.modeled.daily)
summary(flow.modeled.weekly)
summary(flow.modeled.monthly)
summary(flow.modeled.quarterly)
summary(flow.modeled.yearly)

#flow.modeled.weekly <- as.matrix(t(flow.modeled.weekly))
#flow.modeled.monthly <- as.matrix(t(flow.modeled.monthly))
#flow.modeled.quarterly <- as.matrix(t(flow.modeled.quarterly))
#flow.modeled.yearly <- as.matrix(t(flow.modeled.yearly))

#dim(flow.modeled.daily)
#dim(flow.modeled.weekly)
#dim(flow.modeled.monthly)
#dim(flow.modeled.quarterly)
#dim(flow.modeled.yearly)

if(boolConvertToMonthly == TRUE){
  #boolDays now needs to be false for the rest of the code
  boolDays = FALSE
}

#write flow.modeled
writeflow.filename <- paste(lhs_dir,"flow_modeled_daily.csv",sep="")
write.csv(t(flow.modeled.daily),file=writeflow.filename,row.names=TRUE)
writeflow.filename <- paste(lhs_dir,"flow_modeled_weekly.csv",sep="")
write.csv(t(flow.modeled.weekly),file=writeflow.filename,row.names=TRUE)
writeflow.filename <- paste(lhs_dir,"flow_modeled_monthly.csv",sep="")
write.csv(t(flow.modeled.monthly),file=writeflow.filename,row.names=TRUE)
writeflow.filename <- paste(lhs_dir,"flow_modeled_quarterly.csv",sep="")
write.csv(t(flow.modeled.quarterly),file=writeflow.filename,row.names=TRUE)
writeflow.filename <- paste(lhs_dir,"flow_modeled_yearly.csv",sep="")
write.csv(t(flow.modeled.yearly),file=writeflow.filename,row.names=TRUE)

#assign dates
x.Dates.daily <- as.Date(startdate) + 1:Ndays -1
x.Dates.weekly <- seq(as.Date(startdate), by="7 day", length.out=Nweeks)
x.Dates.monthly <- seq(as.Date(startdate), by="month", length.out=Nmonths)
x.Dates.quarterly <- seq(as.Date(startdate), by="3 month", length.out=Nquarters)
x.Dates.yearly <- seq(as.Date(startdate), by="year", length.out=Nyears)

#dim(flow.observed)
flow.observed <- as.vector(t(flow.observed[,3]))
length(flow.observed)
obsflow.zoo.daily <- zoo(flow.observed,x.Dates.daily)
obsflow.xts.daily <- as.xts(obsflow.zoo.daily)
obsflow.zoo.weekly <- apply.weekly(obsflow.xts.daily,FUN=mean)
obsflow.zoo.monthly <- aggregate(obsflow.zoo.daily, by=as.yearmon, FUN=mean)
obsflow.zoo.quarterly <- aggregate(obsflow.zoo.daily, by=as.yearqtr, FUN=mean)
obsflow.zoo.yearly <- apply.yearly(obsflow.xts.daily,FUN=mean)
length(obsflow.zoo.daily)
length(obsflow.zoo.weekly)
length(obsflow.zoo.monthly)
length(obsflow.zoo.quarterly)
length(obsflow.zoo.yearly)

modeledflow.zoo.daily <- zoo(flow.modeled.daily,x.Dates.daily)
modeledflow.zoo.weekly <- zoo(flow.modeled.weekly,x.Dates.weekly)
modeledflow.zoo.monthly <- zoo(flow.modeled.monthly,x.Dates.monthly)
modeledflow.zoo.quarterly <- zoo(flow.modeled.quarterly,x.Dates.quarterly)
modeledflow.zoo.yearly <- zoo(flow.modeled.yearly,x.Dates.yearly)
dim(modeledflow.zoo.daily)
dim(modeledflow.zoo.weekly)
dim(modeledflow.zoo.monthly)
dim(modeledflow.zoo.quarterly)
dim(modeledflow.zoo.yearly)

# still always daily but not used

#dim(modeledflow.log.zoo)
#obsflow.log.zoo <- log10(abs(jitter(obsflow.zoo)))
#length(obsflow.log.zoo)


#if(boolDays==FALSE){x.Dates <- x.Dates.monthly}

gof.modeled.daily <- array(data=NA, dim=c(19,Nsim))
gof.modeled.weekly <- array(data=NA, dim=c(19,Nsim))
gof.modeled.monthly <- array(data=NA, dim=c(19,Nsim))
gof.modeled.quarterly <- array(data=NA, dim=c(19,Nsim))
gof.modeled.yearly <- array(data=NA, dim=c(19,Nsim))
#gof.log.modeled <- array(data=NA, dim=c(19,Nsim))
#
for(i in 1:Nsim){
  sim.gof.daily <- gof(sim=as.vector(modeledflow.zoo.daily[,i]),obs=as.vector(obsflow.zoo.daily))
  gof.modeled.daily[,i]<-sim.gof.daily
  sim.gof.weekly <- gof(sim=as.vector(modeledflow.zoo.weekly[,i]),obs=as.vector(obsflow.zoo.weekly))
  gof.modeled.weekly[,i]<-sim.gof.weekly
  sim.gof.monthly <- gof(sim=as.vector(modeledflow.zoo.monthly[,i]),obs=as.vector(obsflow.zoo.monthly))
  gof.modeled.monthly[,i]<-sim.gof.monthly
  sim.gof.quarterly <- gof(sim=as.vector(modeledflow.zoo.quarterly[,i]),obs=as.vector(obsflow.zoo.quarterly))
  gof.modeled.quarterly[,i]<-sim.gof.quarterly
  sim.gof.yearly <- gof(sim=as.vector(modeledflow.zoo.yearly[,i]),obs=as.vector(obsflow.zoo.yearly))
  gof.modeled.yearly[,i]<-sim.gof.yearly  
}
gof.names <- rownames(sim.gof.daily)  
gof.names

################################################
#temporal scaling is contained right here, change all 5 to daily, weekly, monthly, quarterly, or yearly
#1
timescale <- "yearly"
#2
modeledflow.zoo <- modeledflow.zoo.yearly
#2
obsflow.zoo <- obsflow.zoo.yearly
#4
flow.modeled <- flow.modeled.yearly
#5
gof.modeled <- gof.modeled.yearly
################################################

#######################################################
# calculate glue likelihoods using inverse error variance and exponentiated Nash-Sutcliffe
dim(flow.modeled) #1500 1826 #2001 3287
length(obsflow.zoo) #1826 #3287
t.gof.glue.plus8 <- array(data=NA, dim=c(8,Nsim))
for(i in 1:Nsim){ #Nsim
  inv.err.var1 <- (sum((flow.modeled[,i]-obsflow.zoo)^2)/Nsim)^(-1)
  inv.err.var2 <- (sum((flow.modeled[,i]-obsflow.zoo)^2)/Nsim)^(-2)
  inv.err.var3 <- (sum((flow.modeled[,i]-obsflow.zoo)^2)/Nsim)^(-3)
  t.gof.glue.plus8[1,i] <- inv.err.var1
  t.gof.glue.plus8[2,i] <- inv.err.var2
  t.gof.glue.plus8[3,i] <- inv.err.var3
  nash.sut.1 <- (1 - sum((flow.modeled[,i]-obsflow.zoo)^2) / sum((obsflow.zoo-mean(obsflow.zoo))^2))^1
  nash.sut.2 <- (1 - sum((flow.modeled[,i]-obsflow.zoo)^2) / sum((obsflow.zoo-mean(obsflow.zoo))^2))^2
  nash.sut.3 <- (1 - sum((flow.modeled[,i]-obsflow.zoo)^2) / sum((obsflow.zoo-mean(obsflow.zoo))^2))^3
  t.gof.glue.plus8[4,i] <- nash.sut.1
  t.gof.glue.plus8[5,i] <- nash.sut.2
  t.gof.glue.plus8[6,i] <- nash.sut.3 
  rnseff.check <- (1 - (sum((flow.modeled[,i]-obsflow.zoo)^2)/mean(obsflow.zoo)) / (sum((obsflow.zoo-mean(obsflow.zoo))^2)/mean(obsflow.zoo)))
  t.gof.glue.plus8[7,i] <- rnseff.check
  rd.check <- (1 - sum(((obsflow.zoo-flow.modeled[,i])/mean(obsflow.zoo))^2) / sum(((abs(flow.modeled[,i]-mean(obsflow.zoo))+abs(obsflow.zoo-mean(obsflow.zoo)))/mean(obsflow.zoo))^2))
  #rd.check2 = 1 - [ sum(((obs - sim) / obs )^2 ] / sum( ( (abs(sim - mean(obs) ) + abs(obs - mean(obs) ) ) / mean(obs) )^2 )
  t.gof.glue.plus8[8,i] <- rd.check
}
summary(t.gof.glue.plus8)

# "behavior threshold" of 0 to stop funky sign/exponent issues, negative values get very low likelihood
if(min(t.gof.glue.plus8[4,])<=0){
  if(max(t.gof.glue.plus8[4,])>0){
    min.ns1 = min(t.gof.glue.plus8[4,which(t.gof.glue.plus8[4,]>0)])
    t.gof.glue.plus8[4,which(t.gof.glue.plus8[4,]<=0)]=min.ns1
  }else{
    min.ns1=0
    t.gof.glue.plus8[4,which(t.gof.glue.plus8[4,]<=0)]=min.ns1
  }
}
if(min(t.gof.glue.plus8[5,])<=0){
  if(max(t.gof.glue.plus8[5,])>0){
    min.ns2 = min(t.gof.glue.plus8[5,which(t.gof.glue.plus8[,5]>0)])
    t.gof.glue.plus8[5,which(t.gof.glue.plus8[5]<=0)]=min.ns2
  }else{
    min.ns2=0
    t.gof.glue.plus8[5,which(t.gof.glue.plus8[5,]<=0)]=min.ns2
  }
}
if(min(t.gof.glue.plus8[6,])<=0){
  if(max(t.gof.glue.plus8[6,])>0){
    min.ns3 = min(t.gof.glue.plus8[6,which(t.gof.glue.plus8[6,]>0)])
    t.gof.glue.plus8[6,which(t.gof.glue.plus8[,6]<=0)]=min.ns3
  }else{
    min.ns3=0
    t.gof.glue.plus8[6,which(t.gof.glue.plus8[6,]<=0)]=min.ns3
  }    
}
summary(t.gof.glue.plus8)

# Nseff from hydrogof corresponds to ns1
# add the 6 new gofs to original table
dim(gof.modeled)
dim(t.gof.glue.plus8)
#dim(t(t.gof.glue.plus8))
gof.modeled <- rbind(gof.modeled,t.gof.glue.plus8[1:6,])
dim(gof.modeled)
gof.names[20:25] <- c("iev1","iev2","iev3","ns1","ns2","ns3")
rownames(gof.modeled) <- gof.names
# fix rNSeff and rd
gof.modeled[11,] <- t.gof.glue.plus8[7,]
gof.modeled[14,] <- t.gof.glue.plus8[8,]

# print out gof distributions
ngofs <- dim(gof.modeled)[[1]]
gof.filename <- paste(lhs_dir,"gof_all_",timescale,".pdf",sep="")
pdf(file=gof.filename,width=7.5, height=10.5, bg="white")
par(mfrow=c(3,3))
for(i in 1:ngofs){
  hist(gof.modeled[i,],main=gof.names[i],xlab="",col="darkolivegreen4")
}
dev.off()
  
#cross-plot of variables versus gofs (boxplot)
dim(gof.modeled) # 25 2001
dim(lhs.parameters) # 1500 14
t.gof.modeled <- t(gof.modeled) 
dim(t.gof.modeled) # 2001 25
lhs.names <- colnames(lhs.parameters)
colnames(t.gof.modeled) <- gof.names
nparameters <- length(lhs.names)
for(i in 1:nparameters){
 pargof.filename <- paste(lhs_dir,"lowess_gof_par_",timescale,"_",lhs.names[i],".pdf",sep="")
 pdf(file=pargof.filename,width=7.5, height=10.5, bg="white")
 par(mfrow=c(3,3))
 for(j in 1:ngofs){
    plot(lhs.parameters[,i],t.gof.modeled[,j],main=paste(lhs.names[i],"v.",gof.names[j]),xlab=lhs.names[i],ylab=gof.names[j])
    #loess.df <- data.frame(cbind(lhs.parameters[,i],t.gof.modeled[,j]))
    #loess.predict <- loess(loess.df[,2]~loess.df[,1],loess.df)
    #lines(lhs.parameters[,i],predict(loess.predict),col="red")
    lines(lowess(lhs.parameters[,i],t.gof.modeled[,j]),col="red",lwd=3)
 }
 dev.off()
}
summary(t.gof.modeled)

#cross-plot of gofs versus variables (boxplot)
# drop the % sign
gof.names[5]<- "NRMSE"
gof.names[6]<- "PBIAS"
for(i in 1:ngofs){
 metricgof.filename <- paste(lhs_dir,"metric_gof_par_",gof.names[i],".pdf",sep="")
 pdf(file=metricgof.filename,width=7.5, height=10.5, bg="white")
 par(mfrow=c(3,3))
 for(j in 1:nparameters){
    plot(lhs.parameters[,j],t.gof.modeled[,i],main=paste(lhs.names[j],"v.",gof.names[i]),xlab=lhs.names[j],ylab=gof.names[i])
    lines(lowess(lhs.parameters[,j],t.gof.modeled[,i]),col="red",lwd=3)
 }
 dev.off()
}

# calculate relative likelihoods for gofs
dim(t.gof.modeled)
t.gof.glue <- array(data=NA, dim=c(Nsim,ngofs))
dim(t.gof.glue)
for(i in 1:ngofs){
  if(sum(t.gof.modeled[,i])>0){
    t.gof.glue[,i] <- t.gof.modeled[,i]/sum(t.gof.modeled[,i])
    #t.gof.glue[,i] <- exp(t.gof.modeled[,i]-max(t.gof.modeled[,i]))
  }else{
    t.gof.glue[,i] <- 1/Nsim
  }
}
#fix mean absolute error
t.gof.glue[,2] <- (1/t.gof.modeled[,2])/sum(1/(t.gof.modeled[,2]))
#fix ratio of standard deviations
t.gof.glue[,8] <- abs(1-t.gof.modeled[,8])/sum(abs(1-t.gof.modeled[,8]))
#fix MNSeff
if(min(t.gof.glue[,10])<=0){
  min.MNSeff = min(t.gof.glue[which(t.gof.glue[,10]>0),10])
  t.gof.glue[which(t.gof.glue[,10]<=0),10]=min.MNSeff
}
t.gof.glue[,10] <- t.gof.glue[,10]/sum(t.gof.glue[,10])
summary(t.gof.glue)

### top 5
#calculate combo likelihoods
gof.names
#1) low flows -- MNSeff -- 10, cut at zero
#2) high flow -- ns1 -- 23
#3) error -- iev1 -- 20
#4) bias - MAE -- 2
#5) sd ratios -- rSD -- 8
top5 <- (t.gof.glue[,10]*t.gof.glue[,23]*t.gof.glue[,20]*t.gof.glue[,2]*t.gof.glue[,8])/sum(t.gof.glue[,10]*t.gof.glue[,22]*t.gof.glue[,19]*t.gof.glue[,2]*t.gof.glue[,8])
top5.gofs <- cbind(t.gof.glue[,10],t.gof.glue[,23],t.gof.glue[,20],t.gof.glue[,2],t.gof.glue[,8],top5)
colnames(c(t.gof.glue[,10],t.gof.glue[,23],t.gof.glue[,20],t.gof.glue[,2],t.gof.glue[,8]))
colnames(top5.gofs) <- c("MNSeff","ns1","iev1","MAE","rSD","top5")
top5.gofs.filename <- paste(lhs_dir,"top5_gofs_",timescale,".csv",sep="")
dim(top5.gofs)
dim(lhs.parameters)
top5.gofs <- cbind(top5.gofs,lhs.parameters)
write.csv(top5.gofs,file=top5.gofs.filename,row.names=TRUE)

### top 3
#calculate combo likelihoods
gof.names
#1) low flows -- MNSeff -- 10, cut at zero
#2) high flow -- ns2 -- NOT 23
#3) sd ratios -- rSD -- 8
top3 <- (t.gof.glue[,10]*t.gof.glue[,23]*t.gof.glue[,8])/sum(t.gof.glue[,10]*t.gof.glue[,23]*t.gof.glue[,8])
top3.gofs <- cbind(t.gof.glue[,10],t.gof.glue[,23],t.gof.glue[,8],top3)
colnames(c(t.gof.glue[,10],t.gof.glue[,23],t.gof.glue[,8]))
colnames(top3.gofs) <- c("MNSeff","ns1","rSD","top3")
top3.gofs.filename <- paste(lhs_dir,"top3_gofs_",timescale,".csv",sep="")
dim(top3.gofs)
dim(lhs.parameters)
top3.gofs <- cbind(top3.gofs,lhs.parameters)
write.csv(top3.gofs,file=top3.gofs.filename,row.names=TRUE)

# plot relative likelihoods all for top 5
dim(gof.modeled)
dim(t.gof.glue)
gof.glue <- t(t.gof.glue)
likelihood.filename <- paste(lhs_dir,"likelihoods_all_top5_",timescale,".pdf",sep="")
pdf(file=likelihood.filename,width=7.5, height=10.5, bg="white")
  par(mfrow=c(3,3))
  for(i in 1:ngofs){
    hist(gof.glue[i,],main=gof.names[i],xlab="",col="darkolivegreen4")
  }
  hist(top5,main="Top5 Likelihood",xlab="",col="darkblue")
dev.off()

# plot relative likelihoods all for top 3
dim(gof.modeled)
dim(t.gof.glue)
gof.glue <- t(t.gof.glue)
likelihood.filename <- paste(lhs_dir,"likelihoods_all_top3_",timescale,".pdf",sep="")
pdf(file=likelihood.filename,width=7.5, height=10.5, bg="white")
  par(mfrow=c(3,3))
  for(i in 1:ngofs){
    hist(gof.glue[i,],main=gof.names[i],xlab="",col="darkolivegreen4")
  }
  hist(top3,main="Top3 Likelihood",xlab="",col="darkblue")
dev.off()

#TOP5- estimate parameters weighted by likelihoods
t.gof.glue <- cbind(t.gof.glue,top5)
ngofs <- length(colnames(t.gof.glue))
quantiles.probs <- c(0.001,0.023,0.025,0.05,0.159,0.25,0.5,0.75,0.841,0.95,0.975,0.977,0.999)
nquantiles <- length(quantiles.probs)
quantile.parameters <- array(data=NA, dim=c(nparameters,nquantiles))
calib.parameters <- array(data=NA, dim=c(nparameters,nquantiles,ngofs))
for(i in 1:nparameters){
  quantile.parameters[i,] <- quantile(lhs.parameters[,i], probs=quantiles.probs)
  quantile.parameters
  for(j in 1:ngofs){
    calib.parameters[i,,j] <- wtd.quantile(lhs.parameters[,i],weights=t.gof.glue[,j], probs=quantiles.probs,normwt=TRUE)
    summary(lhs.parameters[,i])
    calib.parameters[i,,j]
  }
}
###top5- write out calibrated parameters to tables
dim(calib.parameters) #14 9 25
colnames(calib.parameters) <- quantiles.probs
rownames(calib.parameters) <- lhs.names
gof.names <- c(gof.names,"top5")
dimnames(calib.parameters)[[3]] <- gof.names
for(i in 1:ngofs){
  parameterdump.filename <- paste(lhs_dir,"estimates_",gof.names[i],"_quantiles_top5_",timescale,".csv",sep="")
  write.csv(calib.parameters[,,i],file=parameterdump.filename,row.names=TRUE)
}
###top5-write out quantiles to a table
colnames(quantile.parameters) <- quantiles.probs
rownames(quantile.parameters) <- lhs.names
parameterdump.filename <- paste(lhs_dir,"estimates_original_quantiles_top5_",timescale,".csv",sep="")
write.csv(quantile.parameters,file=parameterdump.filename,row.names=TRUE)
###top 5 winning parameters write
winning.parameters <- lhs.parameters[which(top5==max(top5)),]
winnerdump.filename <- paste(lhs_dir,"winning_parameters_top5_",timescale,".csv",sep="")
write.csv(winning.parameters,file=winnerdump.filename,row.names=TRUE)


#TOP3- estimate parameters weighted by likelihoods
t.gof.glue <- cbind(t.gof.glue,top3)
ngofs <- length(colnames(t.gof.glue))
nquantiles <- length(quantiles.probs)
quantile.parameters <- array(data=NA, dim=c(nparameters,nquantiles))
calib.parameters <- array(data=NA, dim=c(nparameters,nquantiles,ngofs))
for(i in 1:nparameters){
  quantile.parameters[i,] <- quantile(lhs.parameters[,i], probs=quantiles.probs)
  quantile.parameters
  for(j in 1:ngofs){
    calib.parameters[i,,j] <- wtd.quantile(lhs.parameters[,i],weights=t.gof.glue[,j], probs=quantiles.probs,normwt=TRUE)
    summary(lhs.parameters[,i])
    calib.parameters[i,,j]
  }
}
###top3- write out calibrated parameters to tables
dim(calib.parameters) #14 9 25
colnames(calib.parameters) <- quantiles.probs
rownames(calib.parameters) <- lhs.names
gof.names <- c(gof.names,"top3")
dimnames(calib.parameters)[[3]] <- gof.names
for(i in 1:ngofs){
  parameterdump.filename <- paste(lhs_dir,"estimates_",gof.names[i],"_quantiles_top3_",timescale,".csv",sep="")
  write.csv(calib.parameters[,,i],file=parameterdump.filename,row.names=TRUE)
}

###top3-write out quantiles to a table
colnames(quantile.parameters) <- quantiles.probs
rownames(quantile.parameters) <- lhs.names
parameterdump.filename <- paste(lhs_dir,"estimates_original_quantiles_top3_",timescale,".csv",sep="")
write.csv(quantile.parameters,file=parameterdump.filename,row.names=TRUE)
###top 3 winning parameters write
winning.parameters <- lhs.parameters[which(top3==max(top3)),]
winnerdump.filename <- paste(lhs_dir,"winning_parameters_top3_",timescale,".csv",sep="")
write.csv(winning.parameters,file=winnerdump.filename,row.names=TRUE)

summary(t(gof.modeled))
View(gof.modeled)

################################################################################################
# write gof results to csv
dim(gof.modeled.weekly)
colnames(gof.modeled.daily)
write.table(gof.modeled,file=paste(lhs_dir,"sim_gofs_plus6",timescale,".csv",sep=""),row.names=gof.names[1:25],sep=",")
write.table(gof.modeled.daily,file=paste(lhs_dir,"sim_gofs_daily.csv",sep=""),row.names=gof.names[1:19],sep=",")
write.table(gof.modeled.weekly,file=paste(lhs_dir,"sim_gofs_weekly.csv",sep=""),row.names=gof.names[1:19],sep=",")
write.table(gof.modeled.monthly,file=paste(lhs_dir,"sim_gofs_monthly.csv",sep=""),row.names=gof.names[1:19],sep=",")
write.table(gof.modeled.quarterly,file=paste(lhs_dir,"sim_gofs_quarterly.csv",sep=""),row.names=gof.names[1:19],sep=",")
write.table(gof.modeled.yearly,file=paste(lhs_dir,"sim_gofs_yearly.csv",sep=""),row.names=gof.names[1:19],sep=",")



