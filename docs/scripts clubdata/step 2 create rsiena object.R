####################################################

### Strava paper, PART 2 ### author: ROB FRANKEN ###
### last edited: January 12, 2021 ###

####################################################

# in this script we will use the (anynomized) clubdata retrieved in step 1 ("create club list"),
# to make RSiena objects for each club

# install RSiena
library(RSiena)

# clean the working environment 
# set the working directory to the folder containing the clubdata (clubdata.RData)
rm (list = ls( ))
setwd("C:\\Users\\u244147\\Documents\\dissertatie\\Strava data\\clubs") 
getwd()

# load the clubdata
load("clubdata.RData")
str(clubdata) # inspect structure
# clubdata is a list of 5 lists, 
# with each of these lists containing data of the corresponding club.

# the following script will make a RSiena object.
# again, we start with the first club.
# the script can be re-run after pulling from the list the data of another club (and so forth...)
# eventually, we will combine the output into a list containing the RSiena objects of all the clubs.

####################################################

club <- clubdata[[5]] #### RESTART FROM HERE #####


{ 
  # specify the roles of variables
  names(club)
  
  # A: dependent variables
  kudonet <- sienaDependent(club$kudo) #at least one Kudo
  kudonet2 <- sienaDependent(club$kudo2) #>3 Kudos
  kudonet3 <- sienaDependent(club$kudo3) #>7 Kudos
  time_run <- sienaDependent(club$time_run, type= "behavior")
  freq_run <- sienaDependent(club$freq_run, type= "behavior")
  time_ride <- sienaDependent(club$time_ride, type= "behavior")
  freq_ride <- sienaDependent(club$freq_ride, type= "behavior")
  time_other <- sienaDependent(club$time_other, type= "behavior")
  freq_other <- sienaDependent(club$freq_other, type= "behavior")
  
  # B: explanatory variables
  friendship <- coDyadCovar(club$friendship)
  winter <- varCovar(club$winter)
  male <- coCovar(club$male)
  female <- coCovar(club$female)
  other <- coCovar(club$other)
  
  # now combine the dependent and independent variables in a data object
  mydata <- sienaDataCreate(kudonet, kudonet2, kudonet3,  time_run, freq_run, time_ride, freq_ride, time_other, freq_other,
                            friendship, winter, male, female, other)
  mydata 
  
  # this finishes the data specification
} 

# save the RSiena object and restart the process from line 33.
rsienadataobjectclub_5 <- mydata # adjust number to match the corresponding club number

####################################################

# now make a list containing all the RSiena data objects
clubdata_rsiena <- list(rsienadataobjectclub_1, rsienadataobjectclub_2, rsienadataobjectclub_3, rsienadataobjectclub_4, rsienadataobjectclub_5)

# add netsize
{
  clubdata_rsiena[[1]]$netsize <- clubdata[[1]]$netsize
  clubdata_rsiena[[2]]$netsize <- clubdata[[2]]$netsize
  clubdata_rsiena[[3]]$netsize <- clubdata[[3]]$netsize
  clubdata_rsiena[[4]]$netsize <- clubdata[[4]]$netsize
  clubdata_rsiena[[5]]$netsize <- clubdata[[5]]$netsize
}

# save the output
save(clubdata_rsiena, file="clubdata_rsiena.RData")

# end.
