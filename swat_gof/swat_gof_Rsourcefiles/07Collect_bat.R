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
N_q1s_collate <- 8 
q1.dirs <- vector("list", N_q1s_collate)

# (2) the location of each q1 directory
q1.dirs[1] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[2] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[3] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[4] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[5] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[6] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[7] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
q1.dirs[8] <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/"
# up to N_q1s_collate...

q1 <- vector("list", N_q1s_collate)
# (3) the filename for each q.out
q1[1] <- "1q_1.out"
q1[2] <- "2q_1.out"
q1[3] <- "3q_1.out"
q1[4] <- "4q_1.out"
q1[5] <- "5q_1.out"
q1[6] <- "6q_1.out"
q1[7] <- "7q_1.out"
q1[8] <- "8q_1.out"
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
q1.dir.out <- "c://dropbox/ktp/swat_gof/collect_test/mcmpe2_out/" #will overwrite existing file without asking
q1.name.out <- "q_1.collated.out"
q1.out.filename <- paste(q1.dir.out,q1.name.out,sep="")
#write it somewhere
write(unlist(q1.collate),q1.out.filename)
