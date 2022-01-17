# with the following script, we estimate, for each club i,
# additional models with seperate creation effects for behavior dynamics
# main model: avAttHigher + avAttLower
# alternative model: avSim
# with 'evaluation' and 'creation' function; to see whether, in the case of avAttHigher, influence of Higher-alters
# on behavior increase differs from -1* their influence on not decreasing. 

# we rerun the models until adequate convergence is reached.
# we store the sienaFit objects in a list, which we save later on.
require(RSiena)

# clean the working environment 
rm (list = ls( ))

# we set the no. of clubs and models to estimate
c=5 #clubs
m=8 #recall that in our myeff list; these additional models are 7 and 8.

# set the algorithm; perhaps tweak a bit?
#myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=5, n3=5000 )
myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=3, n3=500 )
#####

for (i in 1:c) { # for every club
i=2
    # we load the RSiena object
  load(file=paste("test", "/", "mydata", "/", "mydata_club", i, ".RData", sep = "")) 
  
  # and the list containing myeff objects
  load(file=paste("test", "/", "myeff", "/", "myeff_club", i, ".RData", sep = "")) 

  # we make a list for storing the RSiena fit objects
  sienaFit <- list()
  
  # for club i we run models j in 7:m
  for (j in 7:m) {
  j=7
      # we estimate the model
    try <- 1
    print(paste("Estimating model ", j, " for club ", i, sep=""))

    myeff[[8]]
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
  save(sienaFit, file=paste("test", "/", "sienaFit", "/", "creation effects", "/", "sienaFit_club", i, ".RData", sep = ""))
  

  print(paste("All models are estimated for club ", i, ". Model results are stored in sienaFit_club", i, ".RData ", "in the 'creation effects'-folder", sep=""))
  print("")
  ifelse(i<c, print(paste("Continuing with club ", i+1, sep="")), print("Estimation finished!"))
  
}
