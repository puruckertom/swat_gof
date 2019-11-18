### Four things need to be specified for each run
# (1) the number of files to be collated
# (2) the location of each q1 directory
# (3) the name of each q1 input file
# (4) the location of where to put the collated q1 file


# lappend function
lappend <- function(lst, obj) {
    lst[[length(lst)+1]] <- obj
    return(lst)
}

#(1) the number of files to be collated
N_q1s_collate <- 32 
q1.dirs <- vector("list", N_q1s_collate)

# (2) the location of each q1 directory
q1.dirs[1] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[2] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[3] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[4] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[5] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[6] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[7] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[8] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[9] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[10] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[11] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[12] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[13] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[14] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[15] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[16] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[17] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[18] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[19] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[20] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[21] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[22] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[23] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[24] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[25] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[26] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[27] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[28] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[29] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[30] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[31] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
q1.dirs[32] <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/"
# up to N_q1s_collate...

q1 <- vector("list", N_q1s_collate)
# (3) the filename for each q.out
q1[1] <- "0301q_1.out"
q1[2] <- "0302q_1.out"
q1[3] <- "0303q_1.out"
q1[4] <- "0304q_1.out"
q1[5] <- "0305q_1.out"
q1[6] <- "0306q_1.out"
q1[7] <- "0307q_1.out"
q1[8] <- "0308q_1.out"
q1[9] <- "0309q_1.out"
q1[10] <- "0310q_1.out"
q1[11] <- "0311q_1.out"
q1[12] <- "0312q_1.out"
q1[13] <- "0313q_1.out"
q1[14] <- "0314q_1.out"
q1[15] <- "0315q_1.out"
q1[16] <- "0316q_1.out"
q1[17] <- "0501q_1.out"
q1[18] <- "0502q_1.out"
q1[19] <- "0503q_1.out"
q1[20] <- "0504q_1.out"
q1[21] <- "0505q_1.out"
q1[22] <- "0506q_1.out"
q1[23] <- "0507q_1.out"
q1[24] <- "0508q_1.out"
q1[25] <- "0509q_1.out"
q1[26] <- "0510q_1.out"
q1[27] <- "0511q_1.out"
q1[28] <- "0512q_1.out"
q1[29] <- "0513q_1.out"
q1[30] <- "0514q_1.out"
q1[31] <- "0515q_1.out"
q1[32] <- "0516q_1.out"

# up to N_q1s_collate...

# check to see if files exist
q1.filename <- vector("list", N_q1s_collate)
for(i in 1:N_q1s_collate){
  q1.filename[i] <- paste(q1.dirs[i],q1[i],sep="")
  print(paste(q1.filename[i], "exists? = ", file.exists(as.character(q1.filename[i]))))
}

# this will complain if q1.collate does not exist (the first time), it is OK
#readLines(as.character(q1.filename[1]))
rm(q1.collate,new.q1)
q1.collate <- vector("list", length=0)
new.q1 <- vector("list", length=0)

for(i in 1:N_q1s_collate){
  # read.table or scan, not sure which is more efficient
  if(file.exists(as.character(q1.filename[i]))){
    new.q1 <- readLines(as.character(q1.filename[i])) 
    q1.collate <- lappend(q1.collate,new.q1) 
    print(paste("run",i,":",length(unlist(q1.collate))))
    rm(new.q1)
  }else
  print(paste("FAIL- could not locate q1 file",q1.filename[i]))
}

length(q1.collate)
#q1.collate[1,]

# (4) the location (directory = q1.dir.out) of where to put the collated q1 file and specify th efilename
q1.dir.out <- "d://dropbox/ktp/swat_gof/fbCL_MPEV/" #will overwrite existing file without asking
q1.name.out <- "q_1.out"
q1.out.filename <- paste(q1.dir.out,q1.name.out,sep="")
#write it somewhere
write(unlist(q1.collate),q1.out.filename)
