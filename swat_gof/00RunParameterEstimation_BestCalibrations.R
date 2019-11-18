#############################################################
# lhs_dir should contain:
#          goal.sf2 -- LHS parameters, removed first few lines
#          observed.txt  -- observed time series, removed first few lines
#          q_1.out  -- modeled time series for each simulation, I think we also have to delete a line or 2 here
#############################################################

#session info
sessionInfo()

#goodness of fit via hydrogof
library(hydroGOF)
library(zoo)
library(Hmisc)
library(xts)
#library(QRMlib) # NA for 2.14 2-Mar-2012 deprecated and replaced by zoo functions
#library(fCalendar) # NA for 2.14 2-Mar-2012 deprecated and replaced by zoo functions

#setup directory structure
Sys.info()

#any mac
if(.Platform$OS.type=="unix"){
  root_dir <- path.expand("~/Dropbox/ktp/swat_gof/")
  source_dir <- path.expand("~/Dropbox/ktp/swat_gof/")
}

#root_dir <- "//AA.AD.EPA.GOV/ORD/ATH/USERS/kprice/Net MyDocuments/Dropbox/ktp/swat_gof/"
#root_dir <- "//Users/Purucker/Dropbox/"
#root_dir <- "c:///dropbox/"
#root_dir <- "C:///Documents and Settings/tpurucke/My Documents/Dropbox/"
#root_dir <- "D:///Dropbox/"
#root_dir <- "//Users/katieprice/Dropbox/ktp/swat_gof/"

#source_dir <- "//AA.AD.EPA.GOV/ORD/ATH/USERS/kprice/Net MyDocuments/Dropbox/ktp/swat_gof/"
#source_dir <- "//Users/Purucker/Dropbox/ktp/swat_gof/"
#source_dir <- "c:///dropbox/ktp/swat_gof/"
#source_dir <- "C:///Documents and Settings/tpurucke/My Documents/Dropbox/ktp/swat_gof/"
#source_dir <- "D:///Dropbox/ktp/swat_gof/"
#source_dir <- "//Users/katieprice/Dropbox/ktp/swat_gof/"


pdf_dir <- paste(source_dir,"gof_pdfs/",sep="")

#all the best directories
Npasses = 8 # this is how many we really will calculate the pass statistic for
sim.dir <- vector(mode="character",length=Npasses) # there need to be Npasses directories specified below
#graphical output and tables will be generated in this directory
sim.dir[1] <- "mcCL_NCDC/mcNCDCd3/" 
sim.dir[2] <- "mcCL_MPE/mcMPEd2/"
sim.dir[3] <- "lrCL_NCDC/lrNCDC_redo_d4/"
sim.dir[4] <- "lrCL_MPE/lrMPEd2/"
sim.dir[5] <- "ncCL_NCDC/ncNCDCd3/" 
sim.dir[6] <- "ncCL_MPE/ncMPEd5/"
sim.dir[7] <- "fbCL_NCDC/fbNCDCd4/"
sim.dir[8] <- "fbCL_MPE/fbMPEd5/"

for(j in 1:Npasses){
  # point to directory and specify whether months or days
  lhs_dir <- paste(source_dir,sim.dir[j],sep="")
  file.exists(lhs_dir)
  boolDays <- TRUE # TRUE is daily; FALSE is monthly
  boolConvertToMonthly <- TRUE #convert daily calibrated data to monthly for gofs
  # daily sims calibrated daily: boolDays = TRUE; boolConvertToMonthly = FALSE
  # daily sims calibrated monthly: boolDays = TRUE; boolConvertToMonthly = TRUE; boolDays is reset to FALSE in script
  # monthly sims calibrated monthly; boolDays = FALSE; boolConvertToMonthly = FALSE
  # boolDays = FALSE; boolConvertToMonthly = TRUE; is not handled and will fail somewhere
  
  startdate <- "2002-01-01"
  enddate <- "2007-12-31"
  Ndays <- (as.numeric(as.Date(enddate))-as.numeric(as.Date(startdate)))+1
  Nweeks <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "7 day"))+1
  if(Nweeks==158){Nweeks=Nweeks-1}
  Nmonths <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "month"))
  Nquarters <- (length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "year")))*4 #assumes full years of data
  Nyears <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "year"))
  
  Nsim <- 2001
  
  source(paste(source_dir,"01PlotLHSParameters.R",sep=""))
  print(j)
  source(paste(source_dir,"02Simulation_GOFs.R",sep=""))
}

source(paste(source_dir,"03Postprocess_Flows.R",sep=""))