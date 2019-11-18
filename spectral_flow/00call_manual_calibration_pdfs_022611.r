# directory containing swat output access file
#sf_dir <- "c://temp/"
#obs_dir <- "l://Public/purucker/SpectralFlow/"
obs_dir <- "c://dropbox/ktp/SpectralFlow/"
#sf_dir <- "l://Public/purucker/from_Kt/ET_Comp/mc_ncdc_d_101/TablesOut/"
#sf_dir <- "l://Public/purucker/SpectralFlow/"

sf_dir <- "c://dropbox/ktp/SpectralFlow/best_runs_22711/mc_ncdc_valid/"             # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_ncdc_calib/"            # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_mpe_valid/"             # watershed = 4
#sf_dir <- "c://Dropbox/ktp/SpectralFlow/best_runs_22711/mc_mpe_calib/"             # watershed = 4

#sf_dir2
#obs_dir <- "L://Lab/GIS/static/MEERT/Neuse/USGS StreamflowData/Mountain_Creek"
#output_dir <- "l://Public/purucker/SpectralFlow/"
#output_dir <- "c://Dropbox/ktp/SpectralFlow/"
output_dir <- sf_dir

#set version/counter number
counter = "100"

# Something descriptive for titles
somethingdescriptive <- "default"

#location of observed data
#obsflow <- read.csv(paste(obs_dir,"lr_2002_2010_julian.csv",sep=""),header=TRUE)
obsflow <- read.csv(paste(obs_dir,"2002_2010_julian.csv",sep=""),header=TRUE)

#access file name without .mdb extension
access.root <- "SWATOutput"

# set watershed number to extract from Access file
watershed = 4
#watershed2=

# call manual calibration r file
source(paste(obs_dir,"01spectral_flow_setup_manual_calibration_022611.r",sep=""))
