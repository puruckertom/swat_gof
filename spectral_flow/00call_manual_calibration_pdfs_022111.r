# directory containing swat output access file
#sf_dir <- "c://temp/"
obs_dir <- "l://Public/purucker/SpectralFlow/"
#sf_dir <- "l://Public/purucker/from_Kt/ET_Comp/mc_ncdc_d_101/TablesOut/"
sf_dir <- "d://SWAT_Projects_corrupted/LittleRiver/LR_4/Scenarios/lr_mpe_104/TablesOut/"
#sf_dir2
#obs_dir <- "L://Lab/GIS/static/MEERT/Neuse/USGS StreamflowData/Mountain_Creek"
output_dir <- "d://SWAT_Projects_corrupted/LittleRiver/LR_4/Scenarios/"

#set version/counter number
counter = "104"

# Something descriptive for titles
somethingdescriptive <- "LR104"

#location of observed data
obsflow <- read.csv(paste(obs_dir,"lr_2002_2010_julian.csv",sep=""),header=TRUE)

#access file name without .mdb extension
access.root <- "SWATOutput"

# set watershed number to extract from Access file
watershed = 41
#watershed2=

# call manual calibration r file
source(paste(obs_dir,"01spectral_flow_setup_manual_calibration_022411.r",sep=""))
