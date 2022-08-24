#Strava SAOM estimation
#additional models
#probing for gender differences in influence dynamics
#last edited 20220807 RF
#to do: make an .Rmd for the online repo website


#explore whether social influence - the indegree and avSim effects - condition on...
#1. running experience (interaction with linear shape effect & constant covariate reflecting years on strava)
#2. gender (ref.=male)

##################################################

#starting with running frequency models.

# clean the working environment
rm(list = ls())

#load rsiena
library(RSiena)

#temp <- "C:\\Users\\u244147\\Documents\\GitHub\\Stravajournal"
#setwd(temp)
#rm(temp)

# load in the R-SIENA objects
load("clubdata_rsiena_freq.Rdata")


# make a list to store our final results in.
results.list <- vector("list", length(clubdata_rsiena_freq)) #"pre-allocate" empty list of length 5

for (i in 1:5) { # for club i
  #get rsiena object
  mydata <- clubdata_rsiena_freq[[i]]
  
  #and the list containing myeff objects
  load(file=paste0("test/myeff/myeff_club",i,".RData"))

  #take 6th element, which is the model with avSim
  myeff <- myeff[[6]]
  
  #include interaction effects, fixed to 0 and test=TRUE
  #here, it is important that in the getEffects command, the behNintn argument is set to a high enough value..
  
  #current behavior (linear shape)
  #myeff1 <- includeInteraction(myeff, linear, indeg, name="freq_run", interaction1=c("","kudonet"))
  #myeff2 <- includeInteraction(myeff, linear, avSim, name="freq_run", interaction1=c("","kudonet"), fix=TRUE, test=TRUE)

  #novice
  myeff3 <- includeInteraction(myeff, effFrom, indeg, name = "freq_run", interaction1 = c("novice", "kudonet"), fix=TRUE, test=TRUE)
  myeff4 <- includeInteraction(myeff, effFrom, avSim, name = "freq_run", interaction1 = c("novice", "kudonet"), fix=TRUE, test=TRUE)
  
  #gender:
  myeff5 <- includeInteraction(myeff, effFrom, indeg, name = "freq_run", interaction1 = c("gender", "kudonet"), fix=TRUE, test=TRUE)
  myeff6 <- includeInteraction(myeff, effFrom, avSim, name = "freq_run", interaction1 = c("gender", "kudonet"), fix=TRUE, test=TRUE)
  
  myeffL<-list(myeff3,myeff4,myeff5,myeff6)
  
  # specify algorithm
 # myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=5, n3=5000 )
  myalgorithm <- sienaAlgorithmCreate(projname = "test", nsub=3, n3=500 )
  
  ansL <- vector("list", length(myeffL)) #"pre-allocate" empty list of length 6

  #estimate
  for (j in 1:length(ansL)) {
    ans <- siena07(myalgorithm, data=mydata, effects=myeffL[[j]], nbrNodes=10, initC=TRUE, batch=TRUE)
    ansL[[j]] <- ans
  }
  
  results.list[[i]] <- ansL
}
  

  
  
  
  
  
  
  
  
  
  ans <- siena07(myalgorithm, data=mydata, effects=myeff, nbrNodes=10, initC=TRUE, batch=TRUE)
  ans
  sans <- summary(ans)
  sans$  
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
  
