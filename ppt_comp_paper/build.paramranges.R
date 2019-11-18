###BUILD DATAFRAME OF CALIBRATED PARAMETER RANGES###

#setup directory structure
Sys.info()

#any mac
if(.Platform$OS.type=="unix"){
	root_dir <- path.expand("~/Dropbox/ktp/PPTCompPaper/")
	source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")
}

#windows
if(.Platform$OS.type=="windows"){
	root_dir <- path.expand("d://Dropbox/ktp/PPTCompPaper/paramranges")
	source_dir <- path.expand("d://Dropbox/ktp/swat_gof")
}

Nparams=12
Nvalues=16 #8 calibrations, min and max for each

### read in calibrated parameter ranges  (winning pass - 1)
### winning passes: 			MCN=3, MCM=2, LRN=4, LRM=2, NCN=3, NCM=5, FBN=4, FBM=5
### use output from: 		MCN=2, MCM=1, LRN=3, LRM=1, NCN=2, NCM=4, FBN=3, FBM=4 
### param range file is "estimates_top3_quantiles_top3.csv" in each subdirectory

MCN <- read.csv(paste(source_dir, "mcCL_NCDC/MCNCDCd2/estimates_top3_quantiles_top3.csv", sep=""), header=T)
MCM <- read.csv(paste(source_dir, "mcCL_MPE/MCMPEd1/estimates_top3_quantiles_top3.csv", sep=""), header=T)
LRN <- read.csv(paste(source_dir, "lrCL_NCDC/lrNCDC_redo_d3/estimates_top3_quantiles_top3.csv", sep=""), header=T)
LRM <- read.csv(paste(source_dir, "lrCL_MPE/LRMPEd1/estimates_top3_quantiles_top3.csv", sep=""), header=T)
NCN <- read.csv(paste(source_dir, "ncCL_NCDC/ncNCDCd2/estimates_top3_quantiles_top3.csv", sep=""), header=T)
NCM <- read.csv(paste(source_dir, "ncCL_MPE/ncMPEd4/estimates_top3_quantiles_top3.csv", sep=""), header=T)
FBN <- read.csv(paste(source_dir, "fbCL_NCDC/FBNCDCd3/estimates_top3_quantiles_top3.csv", sep=""), header=T)
FBM <- read.csv(paste(source_dir, "fbCL_MPE/FBMPEd4/estimates_top3_quantiles_top3.csv", sep=""), header=T)

Calib.Ranges <- array(data=NA, dim=c(Nparams,Nvalues))
Calib.Ranges <- as.data.frame(Calib.Ranges)
rownames(Calib.Ranges) <- c("CN2", "ALPHA_BF", "GW_REVAP", "CH_N2", "CH_K2", "GWQMN", "SOL_AWC", "SOL_K", "SOL_Z", "CANMX", "ESCO", "SURLAG") #?KP type in real param abbreviations
colnames(Calib.Ranges) <- c("MCN_min", "MCN_max", "MCM_min", "MCM_max", "LRN_min", "LRN_max", "LRM_min", "LRM_max", "NCN_min", "NCN_max", "NCM_min", "NCM_max", "FBN_min", "FBN_max", "FBM_min", "FBM_max")
dim(Calib.Ranges)

#2:13, 6,10
for(j in 2:13){
	Calib.Ranges[j-1,1] <- MCN[j,6]
	Calib.Ranges[j-1,2] <- MCN[j,10]
	Calib.Ranges[j-1,3] <- MCM[j,6]
	Calib.Ranges[j-1,4] <- MCM[j,10]
	Calib.Ranges[j-1,5] <- LRN[j,6]
	Calib.Ranges[j-1,6] <- LRN[j,10]
	Calib.Ranges[j-1,7] <- LRM[j,6]
	Calib.Ranges[j-1,8] <- LRM[j,10]
	Calib.Ranges[j-1,9] <- NCN[j,6]
	Calib.Ranges[j-1,10] <- NCN[j,10]
	Calib.Ranges[j-1,11] <- NCM[j,6]
	Calib.Ranges[j-1,12] <- NCM[j,10]
	Calib.Ranges[j-1,13] <- FBN[j,6]
	Calib.Ranges[j-1,14] <- FBN[j,10]
	Calib.Ranges[j-1,15] <- FBM[j,6]
	Calib.Ranges[j-1,16] <- FBM[j,10]
}

write.csv(Calib.Ranges, file=paste(root_dir,"paramranges.csv",sep=""))