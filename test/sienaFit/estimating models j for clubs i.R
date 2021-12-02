require(RSiena)

# clean the working environment 
rm (list = ls( ))


# we set the no. of clubs and models to estimate
c=5 #clubs
m=6 #models (indeg, avAlt, avAttHigher, avAttLower, avAttHigher+Lower, avSim)

# set the algorithm in advance.
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
    sienaFit[[j]] <- siena07(myalgorithm, data = mydata, effects = myeff[[j]], returnDeps=TRUE) 
    
    # re-run until we reach adequate convergence
    while (TRUE){
      if(sienaFit[[j]]$tconv.max >= .25){
        sienaFit[[j]] <- siena07( myalgorithm, data = mydata, effects = myeff, prevAns= sienaFit[[j]], returnDeps=TRUE)
      }else{
        break
      }
    }
  }
  # and save the list with RSiena fit objects
  save(sienaFit, file=paste("test", "/", "sienaFit", "/", "sienaFit_club", i, ".RData", sep = ""))

}
    


