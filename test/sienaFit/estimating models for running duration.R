# with the following script, we estimate, for each club i, all models j
# on the attribute running duration!

require(RSiena)
#temp <- "C:/Users/u244147/Documents/Github/Stravajournal"
#setwd(temp)

# clean the working environment 
rm (list = ls( ))

# we set the no. of clubs and models to estimate
c=5 #clubs
m=6 #models (indeg, avAlt, avAttHigher, avAttLower, avAttHigher+Lower, avSim)

# set the algorithm
myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=5, n3=5000 )

# siena07( myalgorithm, data = mydata, effects = myeff[[j]], prevAns= sienaFit[[j]], returnDeps=TRUE, useCluster=TRUE, nbrNodes=10, initC=TRUE, batch=TRUE)

#####

# i in 1:c

for (i in 2:c) { # for every club


  # we load the RSiena object
  load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", i, ".RData", sep = "")) 
  
  # and the list containing myeff objects
  load(file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club", i, ".RData", sep = "")) 

  # we make a list for storing the RSiena fit objects
  sienaFit <- list()

  # for club i we run models j in 1:m
  for (j in 1:m) {

    # we estimate the model
    try <- 1
    print(paste("Estimating model ", j, " for club ", i, sep=""))
    sienaFit[[j]] <- siena07(myalgorithm, data = mydata, effects = myeff[[j]], returnDeps=TRUE,
                             useCluster=TRUE, nbrNodes=10, initC=TRUE, batch=TRUE) # store it in the list
 
    # re-run until we reach adequate convergence (DONT WE NEED A BREAK!?)
    while (TRUE){
      if(sienaFit[[j]]$tconv.max >= .25){
        try <- try + 1
        if (try>30) {
          print(paste("Now it lasted to long!") 
          break      
        }
        print(paste("Model did not converge adequately (", sienaFit[[j]]$tconv.max, "); ", "Repeat the estimation (", "try ", try, ")", sep = ""))
        sienaFit[[j]] <- siena07( myalgorithm, data = mydata, effects = myeff[[j]], prevAns= sienaFit[[j]], returnDeps=TRUE, useCluster=TRUE, nbrNodes=10, initC=TRUE, batch=TRUE)
      }else{
        print(paste("Reached overall maximum convergence ratio of: ", sienaFit[[j]]$tconv.max, sep = ""))
        print("")
        break
      }
    }
    
  }
  # and save the list with RSiena fit objects
  save(sienaFit, file=paste("test", "/", "sienaFit", "/", "sienaFit_club", i, ".RData", sep = ""))
  print(paste("All models are estimated for club ", i, ". Model results are stored in sienaFit_club", i, ".RData", sep=""))
  print("")
  ifelse(i<c, print(paste("Continuing with club ", i+1, sep="")), print("Estimation finished!"))
  
}

#your objects have the same name, thus I guess you will need something like this. 
sienaFit_clubL <- list()

for (i in 1:5) {
  temp.space <- new.env()
  bar <- load(paste("test/sienaFit/duration/sienaFit_club", i, ".RData", sep=""), temp.space)
  sienaFit_clubL[[i]] <- get(bar, temp.space)
  rm(temp.space)
}


lapply(sienaFit_clubL, '[[', 5)
map(sienaFit_clubL, 6)
}
