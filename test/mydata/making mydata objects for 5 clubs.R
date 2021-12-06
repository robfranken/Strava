# we make RSiena objects for each club; with only one behavior attribute (running frequency)
# we save them
library(RSiena)
rm (list = ls( ))

#load rsiena object for this club (mydata)
load("clubdata.RData")

for (i in (1: length(clubdata))) {
  
  club <- clubdata[[i]] # grab club i
  
  kudonet <- sienaDependent(club$kudo, allowOnly = FALSE)
  freq_run <- sienaDependent(club$freq_run, type = "behavior", allowOnly = FALSE)
  freq_other <- varCovar(club$freq_other[, , ])
  gender <- NA  
  gender <- ifelse(club$male == 1, 1, gender)
  gender <- ifelse(club$female == 1, 2, gender)
  gender <- ifelse(club$other == 1, 2, gender)
  gender <- coCovar(gender)
  mydata <- sienaDataCreate(kudonet, freq_run, freq_other, gender)
  
  # save object
  save(mydata, file=paste("test", "/", "mydata", "/", "mydata_club", i, ".RData", sep = ""))
}

#####

# we do the same thing for the running duration attribute.

rm(list=setdiff(ls(), "clubdata")) #clean environment, keep clubdata list

for (i in (1: length(clubdata))) {
  
  club <- clubdata[[i]] # grab club i
  
  kudonet <- sienaDependent(club$kudo, allowOnly = FALSE)
  time_run <- sienaDependent(club$time_run, type = "behavior", allowOnly = FALSE)
  time_other <- varCovar(club$time_other[, , ])
  gender <- NA  
  gender <- ifelse(club$male == 1, 1, gender)
  gender <- ifelse(club$female == 1, 2, gender)
  gender <- ifelse(club$other == 1, 2, gender)
  gender <- coCovar(gender)
  mydata <- sienaDataCreate(kudonet, time_run, time_other, gender)
  
  # save object
  save(mydata, file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", i, ".RData", sep = ""))
}
