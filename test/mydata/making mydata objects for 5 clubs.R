# we make RSiena objects for each club; with only one behavior attribute (running frequency)
# we save them

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

