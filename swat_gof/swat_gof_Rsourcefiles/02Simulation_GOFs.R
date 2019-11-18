# clean up rough flow to make a 2-d array
dim(rough.flow.modeled) #[1] 2740499       1 #65792871
working.flow.modeled <- as.vector(t(rough.flow.modeled))
class(working.flow.modeled)
length(working.flow.modeled)

# get rid of non-data elements- the simulation numbers
nrows <- length(working.flow.modeled)

## for daily processing
if(boolDays==TRUE){
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
  flow.modeled <- array(data=NA, dim=c(Nsim,Ndays))
  dim(flow.modeled) # 1500 1826 #2001 3286
  for(i in 1:Nsim){
    beg.seq <- ((i-1)*Ndays*3)+3
    flow.seq <- seq(from=beg.seq,by=3,length=Ndays)
    temp.modeled <- as.numeric(split.flow.modeled[flow.seq])
    #summary(temp.modeled)
    flow.modeled[i,] <- temp.modeled
  }
  if(boolConvertToMonthly == FALSE){
    #nothing, good to go
  }else{
    #create a time series object
    days2months <- seq(from=as.Date(startdate), to=as.Date(enddate), by = "day")
    length(days2months)
    dim(flow.modeled)
    flow.modeled.tSeries <- timeSeries(t(flow.modeled), days2months)
    #convert from daily to monthly- from QRMlib - aggregateMonthlySeries
    flow.modeled.monthly <- array(data=NA, dim=c(Nmonths,Nsim))
    for(i in 1:Nsim){
      flow.modeled.monthly[,i] <- as.vector(aggregateMonthlySeries(flow.modeled.tSeries[,i], FUNC=mean))
    }
    flow.modeled.daily <- flow.modeled
    flow.modeled <- t(as.matrix(flow.modeled.monthly))
    #boolDays now needs to be false for the rest of the code
    boolDays == FALSE
  }
}

## for monthly processing
if(boolDays==FALSE && boolConvertToMonthly==FALSE){
  del.seq <- seq(from=(Nmonths+1), length.out=Nsim, by=(Nmonths+1))
  working.flow.modeled <-working.flow.modeled[-del.seq]
  # fix split.flow.modeled
  flow.modeled <- working.flow.modeled
  split.flow.modeled <- unlist(strsplit(working.flow.modeled, " ",fixed=TRUE))
  seq.months <- seq(from=1,by=3,length=Nmonths)
  month.list <- as.numeric(split.flow.modeled[seq.months])
  # get the Nsim Ndays modeled results and stuff them into a 2-d matrix
  flow.modeled <- array(data=NA, dim=c(Nsim,Nmonths))
  dim(flow.modeled) # 1500 1826 #2001 3286
  for(i in 1:Nsim){
    beg.seq <- ((i-1)*Nmonths*3)+3
    flow.seq <- seq(from=beg.seq,by=3,length=Nmonths)
    temp.modeled <- as.numeric(split.flow.modeled[flow.seq])
    #summary(temp.modeled)
    flow.modeled[i,] <- temp.modeled
  }
}

#write flow.modeled
writeflow.filename <- paste(lhs_dir,"flow_modeled.csv",sep="")
write.csv(t(flow.modeled),file=writeflow.filename,row.names=TRUE)

#assign dates
if(boolDays==TRUE){
  x.Dates <- as.Date(startdate) + 1:Ndays -1
}
if(boolDays==FALSE){
  x.Dates <- seq(as.Date(startdate), by="month", length.out=Nmonths)
}

#dim(flow.observed)
flow.observed <- as.vector(t(flow.observed[,3]))
obsflow.zoo <- as.zoo(zoo(flow.observed,x.Dates))
obsflow.log.zoo <- as.zoo(zoo(log10(abs(jitter(flow.observed))),x.Dates))
gof.modeled <- array(data=NA, dim=c(19,Nsim))
gof.log.modeled <- array(data=NA, dim=c(19,Nsim))
#
for(i in 1:Nsim){
  modeledflow.zoo <- as.zoo(zoo(flow.modeled[i,],x.Dates))
  modeledflow.log.zoo <- as.zoo(zoo(log10(abs(jitter(flow.modeled[i,]))),x.Dates))
  sim.gof <- gof(sim=modeledflow.zoo,obs=obsflow.zoo)
  sim.log.gof <- gof(sim=modeledflow.log.zoo,obs=obsflow.log.zoo)
  gof.modeled[,i]<-sim.gof  
  gof.log.modeled[,i]<-sim.log.gof
}
gof.names <- rownames(sim.gof)
gof.names

# calculate glue likelihoods using inverse error variance and exponentiated Nash-Sutcliffe
dim(flow.modeled) #1500 1826 #2001 3287
length(flow.observed) #1826 #3287
t.gof.glue.plus8 <- array(data=NA, dim=c(Nsim,8))
for(i in 1:Nsim){ #Nsim
  inv.err.var1 <- (sum((flow.modeled[i,]-flow.observed)^2)/Nsim)^(-1)
  inv.err.var2 <- (sum((flow.modeled[i,]-flow.observed)^2)/Nsim)^(-2)
  inv.err.var3 <- (sum((flow.modeled[i,]-flow.observed)^2)/Nsim)^(-3)
  t.gof.glue.plus8[i,1] <- inv.err.var1
  t.gof.glue.plus8[i,2] <- inv.err.var2
  t.gof.glue.plus8[i,3] <- inv.err.var3
  nash.sut.1 <- (1 - sum((flow.modeled[i,]-flow.observed)^2) / sum((flow.observed-mean(flow.observed))^2))^1
  nash.sut.2 <- (1 - sum((flow.modeled[i,]-flow.observed)^2) / sum((flow.observed-mean(flow.observed))^2))^2
  nash.sut.3 <- (1 - sum((flow.modeled[i,]-flow.observed)^2) / sum((flow.observed-mean(flow.observed))^2))^3
  t.gof.glue.plus8[i,4] <- nash.sut.1
  t.gof.glue.plus8[i,5] <- nash.sut.2
  t.gof.glue.plus8[i,6] <- nash.sut.3 
  rnseff.check <- (1 - (sum((flow.modeled[i,]-flow.observed)^2)/mean(flow.observed)) / (sum((flow.observed-mean(flow.observed))^2)/mean(flow.observed)))
  t.gof.glue.plus8[i,7] <- rnseff.check
  rd.check <- (1 - sum(((flow.observed-flow.modeled[i,])/mean(flow.observed))^2) / sum(((abs(flow.modeled[i,]-mean(flow.observed))+abs(flow.observed-mean(flow.observed)))/mean(flow.observed))^2))
  #rd.check2 = 1 - [ sum(((obs - sim) / obs )^2 ] / sum( ( (abs(sim - mean(obs) ) + abs(obs - mean(obs) ) ) / mean(obs) )^2 )
  t.gof.glue.plus8[i,8] <- rd.check
}
summary(t.gof.glue.plus8)

# "behavior threshold" of 0 to stop funky sign/exponent issues, negative values get very low likelihood
if(min(t.gof.glue.plus8[,4])<=0){
  if(max(t.gof.glue.plus8[,4])>0){
    min.ns1 = min(t.gof.glue.plus8[which(t.gof.glue.plus8[,4]>0),4])
    t.gof.glue.plus8[which(t.gof.glue.plus8[,4]<=0),4]=min.ns1
  }else{
    min.ns1=0
    t.gof.glue.plus8[which(t.gof.glue.plus8[,4]<=0),4]=min.ns1
  }
}
if(min(t.gof.glue.plus8[,5])<=0){
  if(max(t.gof.glue.plus8[,5])>0){
    min.ns2 = min(t.gof.glue.plus8[which(t.gof.glue.plus8[,5]>0),5])
    t.gof.glue.plus8[which(t.gof.glue.plus8[,5]<=0),5]=min.ns2
  }else{
    min.ns2=0
    t.gof.glue.plus8[which(t.gof.glue.plus8[,5]<=0),5]=min.ns2
  }
}
if(min(t.gof.glue.plus8[,6])<=0){
  if(max(t.gof.glue.plus8[,6])>0){
    min.ns3 = min(t.gof.glue.plus8[which(t.gof.glue.plus8[,6]>0),6])
    t.gof.glue.plus8[which(t.gof.glue.plus8[,6]<=0),6]=min.ns3
  }else{
    min.ns3=0
    t.gof.glue.plus8[which(t.gof.glue.plus8[,6]<=0),6]=min.ns3
  }    
}

# Nseff from hydrogof corresponds to ns1

# add the 6 new gofs to original table
dim(gof.modeled)
dim(t(t.gof.glue.plus8))
gof.modeled <- rbind(gof.modeled,t(t.gof.glue.plus8)[1:6,])
dim(gof.modeled)
gof.names[20:25] <- c("iev1","iev2","iev3","ns1","ns2","ns3")

# fix rNSeff and rd
gof.modeled[11,] <- t.gof.glue.plus8[,7]
gof.modeled[14,] <- t.gof.glue.plus8[,8]

# write gof results to csv
write.table(gof.modeled,file=paste(lhs_dir,"sim_gofs.csv",sep=""),row.names=gof.names,sep=",")
write.table(gof.log.modeled,file=paste(lhs_dir,"sim_log_gofs.csv",sep=""),row.names=gof.names[1:19],sep=",")

# print out gof distributions
ngofs <- dim(gof.modeled)[[1]]
gof.filename <- paste(lhs_dir,"gof_all.pdf",sep="")
pdf(file=gof.filename,width=7.5, height=10.5, bg="white")
par(mfrow=c(3,3))
for(i in 1:ngofs){
  hist(gof.modeled[i,],main=gof.names[i],xlab="",col="darkolivegreen4")
}
dev.off()
  
#cross-plot of variables versus gofs (boxplot)
dim(gof.modeled) # 24 1500
dim(lhs.parameters) # 1500 14
t.gof.modeled <- t(gof.modeled) 
dim(t.gof.modeled) # 1500 24 2001
lhs.names <- colnames(lhs.parameters)
colnames(t.gof.modeled) <- gof.names
nparameters <- length(lhs.names)
for(i in 1:nparameters){
 pargof.filename <- paste(lhs_dir,"lowess_gof_par_",lhs.names[i],".pdf",sep="")
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
top5.gofs.filename <- paste(lhs_dir,"top5_gofs.csv",sep="")
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
top3.gofs.filename <- paste(lhs_dir,"top3_gofs.csv",sep="")
dim(top3.gofs)
dim(lhs.parameters)
top3.gofs <- cbind(top3.gofs,lhs.parameters)
write.csv(top3.gofs,file=top3.gofs.filename,row.names=TRUE)

# plot relative likelihoods all for top 5
dim(gof.modeled)
dim(t.gof.glue)
gof.glue <- t(t.gof.glue)
likelihood.filename <- paste(lhs_dir,"likelihoods_all_top5.pdf",sep="")
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
likelihood.filename <- paste(lhs_dir,"likelihoods_all_top3.pdf",sep="")
pdf(file=likelihood.filename,width=7.5, height=10.5, bg="white")
par(mfrow=c(3,3))
for(i in 1:ngofs){
  hist(gof.glue[i,],main=gof.names[i],xlab="",col="darkolivegreen4")
}
hist(top3,main="Top3 Likelihood",xlab="",col="darkblue")
dev.off()


#1/1500
#shoot.high <- c(9,10,11,12,13,14,15,16,17,18)
#shoot.low <- c(1,2,3,4,5,7)
#shoot.zero <- c(6,8)
#for(i in shoot.high){
#  t.gof.glue[,i] <- exp(t.gof.modeled[,i]-max(t.gof.modeled[,i]))
#}
#for(i in shoot.high){
#  t.gof.glue[,i] <- exp(t.gof.modeled[,i]-max(t.gof.modeled[,i]))
#}
#for(i in shoot.high){
#  t.gof.glue[,i] <- exp(t.gof.modeled[,i]-max(t.gof.modeled[,i]))
#}

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
  parameterdump.filename <- paste(lhs_dir,"estimates_",gof.names[i],"_quantiles_top5.csv",sep="")
  write.csv(calib.parameters[,,i],file=parameterdump.filename,row.names=TRUE)
}
###top5-write out quantiles to a table
colnames(quantile.parameters) <- quantiles.probs
rownames(quantile.parameters) <- lhs.names
parameterdump.filename <- paste(lhs_dir,"estimates_original_quantiles_top5.csv",sep="")
write.csv(quantile.parameters,file=parameterdump.filename,row.names=TRUE)
###top 5 winning parameters write
winning.parameters <- lhs.parameters[which(top5==max(top5)),]
winnerdump.filename <- paste(lhs_dir,"winning_parameters_top5.csv",sep="")
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
  parameterdump.filename <- paste(lhs_dir,"estimates_",gof.names[i],"_quantiles_top3.csv",sep="")
  write.csv(calib.parameters[,,i],file=parameterdump.filename,row.names=TRUE)
}
###top3-write out quantiles to a table
colnames(quantile.parameters) <- quantiles.probs
rownames(quantile.parameters) <- lhs.names
parameterdump.filename <- paste(lhs_dir,"estimates_original_quantiles_top3.csv",sep="")
write.csv(quantile.parameters,file=parameterdump.filename,row.names=TRUE)
###top 3 winning parameters write
winning.parameters <- lhs.parameters[which(top3==max(top3)),]
winnerdump.filename <- paste(lhs_dir,"winning_parameters_top3.csv",sep="")
write.csv(winning.parameters,file=winnerdump.filename,row.names=TRUE)





