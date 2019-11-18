#############################################################
# lhs_dir should contain:
#          goal.sf2 -- LHS parameters, removed first few lines
#          observed.txt  -- observed time series, removed first few lines
#          q_1.out  -- modeled time series for each simulation, I think we also have to delete a line or 2 here
#############################################################

#goodness of fit via hydrogof
library(hydroGOF)
library(zoo)
library(Hmisc)
library(QRMlib)
library(fCalendar)


#root_dir <- "//AA.AD.EPA.GOV/ORD/ATH/USERS/kprice/Net MyDocuments/Dropbox/ktp/swat_gof/"
#root_dir <- "//Users/Purucker/Dropbox/"
#root_dir <- "c:///dropbox/"
#root_dir <- "C:///Documents and Settings/tpurucke/My Documents/Dropbox/"
#root_dir <- "D:///Dropbox/"
root_dir <- "//Users/katieprice/Dropbox/"

#source_dir <- "//AA.AD.EPA.GOV/ORD/ATH/USERS/kprice/Net MyDocuments/Dropbox/ktp/swat_gof/"
#source_dir <- "//Users/Purucker/Dropbox/ktp/swat_gof/"
#source_dir <- "c:///dropbox/ktp/swat_gof/"
#source_dir <- "C:///Documents and Settings/tpurucke/My Documents/Dropbox/ktp/swat_gof/"
#source_dir <- "D:///Dropbox/ktp/swat_gof/"
source_dir <- "//Users/katieprice/Dropbox/Validations/"


pdf_dir <- paste(source_dir,"gof_pdfs/",sep="")

# point to directory and specify whether months or days
lhs_dir <- paste(source_dir,"mcNCDCns1V/",sep="")
boolDays <- TRUE # TRUE is daily; FALSE is monthly
boolConvertToMonthly <- FALSE #convert daily calibrated data to monthly for gofs
# daily sims calibrated daily: boolDays = TRUE; boolConvertToMonthly = FALSE
# daily sims calibrated monthly: boolDays = TRUE; boolConvertToMonthly = TRUE; boolDays is reset to FALSE in script
# monthly sims calibrated monthly; boolDays = FALSE; boolConvertToMonthly = FALSE
# boolDays = FALSE; boolConvertToMonthly = TRUE; is not handled and will fail somewhere

startdate <- "2002-01-01"
enddate <- "2007-12-31"
Ndays <- (as.numeric(as.Date(enddate))-as.numeric(as.Date(startdate)))+1
Nmonths <- length(seq(from=as.Date(startdate), to=as.Date(enddate), by = "month"))

Nsim <- 2001

source(paste(source_dir,"01PlotLHSParameters.R",sep=""))

source(paste(source_dir,"02Simulation_GOFs.R",sep=""))

source(paste(source_dir,"03Postprocess_Flows.R",sep=""))