# Strava SAOM estimation
#additional models
#probing for gender differences in influence dynamics
#last edited 20220807 RF

##################################################

#starting with running frequency models.

# clean the working environment
rm(list = ls())
# load in the R-SIENA objects
load("clubdata_rsiena_freq.Rdata")

#i load the basic for-loop script that runs models (here, with myeff object 5;
#the main model) over all clubs; but, for each club i run 2 models;
#a. the main model + an interaction of gender with kudos indegree
#b. " + an interaction of avAttLower with gender.


###
library(RSiena)
c=5 #5clubs;

#start with c1
#c=1
#then 2-5

for (i in 2:c) { # for club c
  
  i=5
  
  #get rsiena object
  mydata <- clubdata_rsiena_freq[[i]]
  
  #and the list containing myeff objects
  load(file=paste0("test/myeff/myeff_club",i,".RData"))
  myeffL <- myeff
  
  #take 5th element, which is the main model
  myeff <- myeffL[[5]]
  #and now add interaction effects
  myeff1 <- includeInteraction(myeff, effFrom, indeg,
                               name="freq_run", interaction1=c("gender", "kudonet"))
  myeff2 <- includeInteraction(myeff, effFrom, avAttLower,
                               name="freq_run", interaction1=c("gender", "kudonet"))
  myeffL <- list(myeff1,myeff2)#list extra model specficiations
  
  # but first specify algorithm
  myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=5, n3=5000 )
  #mock algo
  myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=3, n3=1000 )
  
  {
  
  for (j in 1:length(myeffL)) {
    
    #reiterate until reaching good convergence
  
    sienaFit <-list()
    #store as jth element in results list
  
    #estimate
    try=1
    #j=2
    sienaFit[[j]] <- siena07( myalgorithm, data = mydata, effects = myeffL[[j]], returnDeps=TRUE, useCluster=TRUE, nbrNodes=10, initC=TRUE, batch=TRUE)

    # re-run until we reach adequate convergence 
    while (TRUE){
      if(sienaFit[[j]]$tconv.max >= .25){
        try <- try + 1
        if (try>20) { # with at max 20 runs.
          print(paste("Now it lasted to long!")) 
                break      
        }
        print(paste("Model did not converge adequately (", sienaFit[[j]]$tconv.max, "); ", "Repeat the estimation (", "try ", try, ")", sep = ""))
        
        sienaFit[[j]] <- siena07( myalgorithm, data = mydata, effects = myeffL[[j]], prevAns= sienaFit[[j]], useCluster=TRUE, nbrNodes=10, initC=TRUE, batch=TRUE)
      }else{
        print(paste("Reached overall maximum convergence ratio of: ", sienaFit[[j]]$tconv.max, sep = ""))
        print("")
        break
      }
    }

    
    }
  # and save the list with RSiena fit objects
  save(sienaFit, file=paste0("test/sienaFit/gender_interaction/sienaFit_club", i, "_g", ".RData"))
  print(paste("All models are estimated for club ", i, ". Model results are stored in gender/sienaFit_club", i, "_g", ".RData", sep=""))
  print("")
  ifelse(i<c, print(paste("Continuing with club ", i+1, sep="")), print("Estimation finished!"))
  
  }
  
}
length(sienaFit)
sienaFit[[1]]
  
