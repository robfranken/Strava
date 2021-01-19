rm (list = ls( ))
setwd("C:\\Users\\u244147\\Documents\\dissertatie\\Strava data\\clubs")
library(RSiena)




#################

load("clubdata.Rdata") # load (raw) club list
club <- clubdata[[1]] # grab club 1

# specify variable roles for RSiena object
library(RSiena)

kudonet <- sienaDependent(club$kudo[,, c(1, 6, 12)])
time_run <- sienaDependent(club$time_run[,, c(1, 6, 12)], type= "behavior")
time_other <- sienaDependent(club$time_other[,, c(1, 6, 12)], type= "behavior")

friendship <- coDyadCovar(club$friendship)

gender <- NA
gender <- ifelse(club$male == 1, 1, gender)
gender <- ifelse(club$female == 1, 2, gender)
gender <- ifelse(club$other == 1, 3, gender)
gender <- coCovar(gender)

mydata <- sienaDataCreate(kudonet, time_run, time_other, friendship, gender)

###

# step 2: inspect data
print01Report(mydata)

myeff <- getEffects(mydata)
#effectsDocumentation(myeff)

# Structural effects
myeff1 <- includeEffects(myeff, inPop, transTrip, transRecTrip, name="kudonet")

# Homophily tendencies in Kudo relations with respect to gender
myeff2 <- includeEffects( myeff1, sameX, name="kudonet", interaction1 = "gender" )

# Homophily tendencies in Kudo relations with respect to activity level
myeff2 <- includeEffects(myeff2, egoX, altX, absDiffX, name="kudonet", interaction1 = "time_run")
myeff2 <- includeEffects(myeff2, egoX, altX, absDiffX, name="kudonet", interaction1 = "time_other")

# Kudo-indegree effect on running  
myeff3 <- includeEffects(myeff2, indeg, outdeg, name = "time_run", interaction1 = "kudonet") # we also include outdegree effect
myeff3 <- includeEffects(myeff2, indeg, outdeg, name = "time_other", interaction1 = "kudonet")

# Subset this model
myeff0 <- myeff3
myeff0

# Total alter effect on running 
myeff3a <- includeEffects(myeff0, totAlt, name = "time_run", interaction1 = "kudonet")
myeff3a

# Let's also try the total similarity effect 
myeff3b <- includeEffects(myeff0, totSim, name = "time_run", interaction1 = "kudonet")
myeff3b

# And why not also try the total similarity x reciprocity effect
myeff3c <- includeEffects(myeff0, totSimRecip, name = "time_run", interaction1 = "kudonet")
myeff3c

# create algo
myalgorithm <- sienaAlgorithmCreate(projname = "test")

# estimate


try <- 4


ansM1 <- siena07(myalgorithm, data = mydata, effects = myeff1)
siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "last.html", sep = ""))

#In case we want to run the model as many times as it is necessary until we get a good convergence ratio.
while (TRUE){
  
  if(ansM2$tconv.max > 0.25){
    try <- try + 1
    print(paste("Try:", try, sep=" "))
    ansM1 <- siena07( myalgorithm, data = mydata, effects = myeff1, prevAns= ansM1)
    siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "ansM1_", try, ".html", sep=""))
    
  }else{
    siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "ansM1_", try, ".html", sep=""))
    print(paste("Reached convergence ratio of ", ansM2$tconv.max, sep = ""))
    break
  }
}



# Model 2: include selection effects:
# run the code chunks, and then re-run the code above
{
  # Who sends/receives?
  myeff1 <- includeEffects( myeff1, egoX, altX, name="kudonet", interaction1 = "male" )
  myeff1 <- includeEffects( myeff1, egoX, altX, name="kudonet", interaction1 = "female" )
  myeff1 <- includeEffects( myeff1, egoX, altX, name="kudonet", interaction1 = "other" )
  
  # Homophily tendencies in Kudo relations with respect to gender
  myeff1 <- includeEffects( myeff1, simX, name="kudonet", interaction1 = "male" )
  myeff1 <- includeEffects( myeff1, simX, name="kudonet", interaction1 = "female" )
  myeff1 <- includeEffects( myeff1, simX, name="kudonet", interaction1 = "other" )
  
  # Homophily tendencies in Kudo relations with respect to activity level
  myeff1 <- includeEffects(myeff1, egoX, altX, simX, name="kudonet", interaction1 = "time_run")
  myeff1 <- includeEffects(myeff1, egoX, altX, simX, name="kudonet", interaction1 = "freq_run")
  myeff0 <- myeff1
}

print(myeff0)


# Model 3: include influence effects:
{
  # Kudo-indegree effect on running  
  myeff1 <- includeEffects(myeff0, indeg, outdeg, recip, name = "time_run", interaction1 = "kudonet") # we also include outdegree and reciprocity effect
  myeff1 <- includeEffects(myeff1, indeg, outdeg, recip, name = "freq_run", interaction1 = "kudonet")
  
  # Total alter effect on running 
  myeff1 <- includeEffects(myeff1, totAlt, name = "time_run", interaction1 = "kudonet")
  myeff1 <- includeEffects(myeff1, totAlt, name = "freq_run", interaction1 = "kudonet")
}

# Let's also try the total similarity effect 
{
  myeff1 <- includeEffects(myeff0, totSim, name = "time_run", interaction1 = "kudonet")
  myeff1 <- includeEffects(myeff1, totSim, name = "freq_run", interaction1 = "kudonet")
}

# And why not also try the total similarity x reciprocity effect
{
  myeff1 <- includeEffects(myeff0, totSimRecip, name = "time_run", interaction1 = "kudonet")
  myeff1 <- includeEffects(myeff1, totSimRecip, name = "freq_run", interaction1 = "kudonet")
}





