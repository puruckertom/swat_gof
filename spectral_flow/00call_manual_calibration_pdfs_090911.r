# directory containing swat output access file
#sf_dir <- "c://temp/"
#obs_dir <- "l://Public/purucker/SpectralFlow/"
obs_dir <- "d://dropbox/ktp/SpectralFlow/"
#sf_dir <- "l://Public/purucker/from_Kt/ET_Comp/mc_ncdc_d_101/TablesOut/"
#sf_dir <- "l://Public/purucker/SpectralFlow/"

#sf_dir <- "c://dropbox/ktp/SpectralFlow/best_runs_22711/mc_ncdc_valid/"             # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_ncdc_calib/"            # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_mpe_valid/"             # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_mpe_calib/"             # watershed = 4

#1
sf_dir <- "d://Arcswat_10_Statsgo/fb/Scenarios/Hx_2008_2010/TablesOut/"
output_dir <- sf_dir

#sf_dir2
#obs_dir <- "L://Lab/GIS/static/MEERT/Neuse/USGS StreamflowData/Mountain_Creek"
#output_dir <- "l://Public/purucker/SpectralFlow/"
#output_dir <- "c://Dropbox/ktp/SpectralFlow/"


#set version/counter number
counter = "100"

# Something descriptive for titles
somethingdescriptive <- "fb_2008_2010"

#access file name without .mdb extension
access.root <- "SWATOutput"

#location of observed data, set watershed number to extract from Access file
#mc
#watershed = 4
#obsflow <- read.csv(paste(obs_dir,"mc_2002_2010_julian.csv",sep=""),header=TRUE)

#FB
watershed = 649
obsflow <- read.csv(paste(obs_dir,"fb_2002_2010_julian.csv",sep=""),header=TRUE)

startdate <- "2002-01-01"
source(paste(obs_dir,"01spectral_flow_setup_manual_calibration_090911.r",sep=""))


# call manual calibration r file
source(paste(obs_dir,"01spectral_flow_setup_manual_calibration_090911.r",sep=""))
