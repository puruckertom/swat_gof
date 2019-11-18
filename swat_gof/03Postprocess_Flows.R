dim(flow.modeled) #1500 1826
length(flow.observed)
length(top3)

if(boolDays==TRUE && boolConvertToMonthly==FALSE){
  sim.flow.quantiles.weighted <- array(data=NA, dim=c(Ndays,nquantiles))
  sim.flow.quantiles.unweighted <- array(data=NA, dim=c(Ndays,nquantiles))
  for(i in 1:Ndays){
   sim.flow.quantiles.weighted[i,] <- wtd.quantile(flow.modeled[,i],weights=top3, probs=quantiles.probs,normwt=TRUE) 
   sim.flow.quantiles.unweighted[i,] <- quantile(flow.modeled[,i], probs=quantiles.probs) 
  }
}

if(boolDays==TRUE && boolConvertToMonthly==TRUE){
  sim.flow.quantiles.weighted <- array(data=NA, dim=c(Nmonths,nquantiles))
  sim.flow.quantiles.unweighted <- array(data=NA, dim=c(Nmonths,nquantiles))
  for(i in 1:Nmonths){
   sim.flow.quantiles.weighted[i,] <- wtd.quantile(flow.modeled[,i],weights=top3, probs=quantiles.probs,normwt=TRUE) 
   sim.flow.quantiles.unweighted[i,] <- quantile(flow.modeled[,i], probs=quantiles.probs) 
  }
}

if(boolDays==FALSE){
  sim.flow.quantiles.weighted <- array(data=NA, dim=c(Nmonths,nquantiles))
  sim.flow.quantiles.unweighted <- array(data=NA, dim=c(Nmonths,nquantiles))
  for(i in 1:Nmonths){
   sim.flow.quantiles.weighted[i,] <- wtd.quantile(flow.modeled[,i],weights=top3, probs=quantiles.probs,normwt=TRUE) 
   sim.flow.quantiles.unweighted[i,] <- quantile(flow.modeled[,i], probs=quantiles.probs) 
  }
}

dim(sim.flow.quantiles.weighted)
colnames(sim.flow.quantiles.weighted) <- quantiles.probs
colnames(sim.flow.quantiles.unweighted) <- quantiles.probs
summary(sim.flow.quantiles.weighted)
summary(sim.flow.quantiles.unweighted)
quantiles.probs

#write weighted quantiles
writequantiles.filename <- paste(lhs_dir,"quantiles_weighted_out.csv",sep="")
write.csv(sim.flow.quantiles.weighted,file=writequantiles.filename,row.names=TRUE)

#write unweighted quantiles
writequantiles.filename <- paste(lhs_dir,"quantiles_unweighted_out.csv",sep="")
write.csv(sim.flow.quantiles.unweighted,file=writequantiles.filename,row.names=TRUE)

#write par_inf.sf2 file
par.sf2 <- file(paste(lhs_dir,"par_inf.sf2",sep=""), "w")  # open an output file connection
cat("Test_example_2005\n", file = par.sf2)
cat("\n", file = par.sf2)
cat("Number_of_Parameters=  12\n", file = par.sf2)
cat("Number_of_LH_sims=  2001\n", file = par.sf2)
cat("\n", file = par.sf2)
cat(paste("  r__CN2.mgt","\n",sep=""), file = par.sf2)
cat(paste("  v__ALPHA_BF.gw","\n",sep=""), file = par.sf2)
cat(paste("  v__GW_REVAP.gw","\n",sep=""), file = par.sf2)
cat(paste("  v__CH_N2.rte","\n",sep=""), file = par.sf2)
cat(paste("  v__CH_K2.rte","\n",sep=""), file = par.sf2)
cat(paste("  v__GWQMN.gw","\n",sep=""), file = par.sf2)
cat(paste("  r__SOL_AWC(1).sol","\n",sep=""), file = par.sf2)
cat(paste("  r__SOL_K(1).sol","\n",sep=""), file = par.sf2)
cat(paste("  r__SOL_Z().sol","\n",sep=""), file = par.sf2)
cat(paste("  v__CANMX.hru","\n",sep=""), file = par.sf2)
cat(paste("  v__ESCO.hru","\n",sep=""), file = par.sf2)
cat(paste("  v__SURLAG.bsn","\n",sep=""), file = par.sf2)
close(par.sf2)

