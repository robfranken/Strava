require(RSiena)

# clean the working environment 
rm (list = ls( ))


# we set the no. of clubs and models to estimate
c=2 #clubs
m=2 #models (indeg, avAlt, avAttHigher, avAttLower, avAttHigher+Lower, avSim)

# set the algorithm; perhaps tweak a bit?
myalgorithm <- sienaAlgorithmCreate(projname = "test")


# with the following script, we estimate, for each club i, all models j
# we rerun the models until adequate convergence is reached.
# we store the sienaFit objects in a list, which we save later on.


#####


for (i in 1:c) { # for every club

  # we load the RSiena object
  load(file=paste("test", "/", "mydata", "/", "mydata_club", i, ".RData", sep = "")) 
  
  # and the list containing myeff objects
  load(file=paste("test", "/", "myeff", "/", "myeff_club", i, ".RData", sep = "")) 
  
  # we make a list for storing the RSiena fit objects
  sienaFit <- list()
  
  # for club i we run models j in 1:m
  for (j in 1:m) {
 
    # we estimate the model
    try <- 1
    print(paste("Estimating model ", j, " for club ", i, sep=""))
    sienaFit[[j]] <- siena07(myalgorithm, data = mydata, effects = myeff[[j]], returnDeps=TRUE) # store it in the list
    
    # re-run until we reach adequate convergence
    while (TRUE){
      if(sienaFit[[j]]$tconv.max >= .25){
        try <- try + 1
        print(paste("Model did not converge adequately (", sienaFit[[j]]$tconv.max, "); ", "Repeat the estimation (", "try ", try, ")", sep = ""))
        sienaFit[[j]] <- siena07( myalgorithm, data = mydata, effects = myeff[[j]], prevAns= sienaFit[[j]], returnDeps=TRUE)
      }else{
        print(paste("Reached overall maximum convergence ratio of: ", sienaFit[[j]]$tconv.max, sep = ""))
        print("")
        break
      }
    }
    
  }
  # and save the list with RSiena fit objects
  save(sienaFit, file=paste("test", "/", "sienaFit", "/", "sienaFit_club", i, ".RData", sep = ""))
  print(paste("All models are estimated for club ", i, "! Model results are stored in sienaFit_club", i, ".RData", sep=""))
  while (TRUE){
    if(i<c){
      print(paste("Continuing with club ", i+1, sep=""))
    }else{
      print("Estimation finished!")
      break
    }
  }

}





###

