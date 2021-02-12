library(RSiena)

# clean the working environment 
# set the working directory to the folder containing the clubdata (clubdata.RData)
rm (list = ls( ))
setwd("C:\\Users\\u244147\\Documents\\dissertatie\\Strava data\\clubs") 
getwd()

#load("clubdata_rsiena.Rdata") # load club list
#mydata <- clubdata_rsiena[[1]] # grab rsiena object club 1
#library(RSiena) #and load package

# for fewer waves
load("clubdata.Rdata") # load (raw) club list
club <- clubdata[[1]] # grab club 

# specify variable roles for RSiena object
kudonet <- sienaDependent(club$kudo[,, c(1, 6, 12)])
freq_run <- sienaDependent(club$freq_run[,, c(1, 6, 12)], type= "behavior")
#time_run <- sienaDependent(club$time_run[,, c(1, 6, 12)], type= "behavior")

friendship <- coDyadCovar(club$friendship)

gender <- NA #we dichotomize gender as binary (men vs. women and other)
gender <- ifelse(club$male == 1, 1, gender)
gender <- ifelse(club$female == 1, 2, gender)
gender <- ifelse(club$other == 1, 2, gender)
gender <- coCovar(gender)

winter <- varCovar(club$winter[, c(1, 6, 12)])

#create rsiena data object
mydata <- sienaDataCreate(kudonet, freq_run, friendship, gender, winter)
#mydata <- sienaDataCreate(kudonet, time_run, freq_run, friendship, gender, winter)
mydata

##############################
#######  2. describe  ########
##############################
print01Report(mydata)
# see @2. Change in networks: densities/(average) degree over time, distances/Jaccard coefficient


##############################
### 3. define myeff object ###
##############################

myeff <- getEffects(mydata)
#effectsDocumentation(myeff)

# a. include structural network effects
# guided by: (1) http://www.stats.ox.ac.uk/~snijders/siena/Net_longi7_A_s.pdf
# and:       (2) http://www.stats.ox.ac.uk/~snijders/siena/Siena_ModelSpec_s.pdf

myeff1 <- includeEffects(myeff, gwespFF, #triadic
                         inPop, outAct, name = "kudonet") #degree-related

myeff1 <- includeInteraction(myeff1, recip, gwespFF, parameter = 69) #gwesp x reciprocity

myeff1 <- includeEffects(myeff1, egoX, altX, sameX, name = "kudonet", interaction1 = "gender")


# b. covariate effects of gender (monadic and dyadic)

myeff1 <- includeEffects(myeff1, egoX, altX, sameX, name = "kudonet", interaction1 = "gender")


# attribute effects and homophily tendencies in Kudo relations with respect to gender
myeff2 <- includeEffects( myeff1, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )

# attribute effects and homophily tendencies in Kudo relations with respect to activity level
myeff2 <- includeEffects(myeff2, egoX, altX, simX, name="kudonet", interaction1 = "freq_run")

# Kudo-indegree effect on running  
myeff3 <- includeEffects(myeff2, indeg, name = "freq_run", interaction1 = "kudonet") #popularity-related tendency (i.e. "support")

# we may want to include the square root version! How to specify?
# also: we may want to condition the popularity-related tendency on current behavior!

myeff3 <- includeEffects(myeff3, avAlt, name = "freq_run", interaction1 = "kudonet") #behavior-related average similarity

myeff3 <- includeInteraction(myeff3, quad, effFrom, name = "freq_run", interaction1 = c("", "winter")) #also, we condition the shape effect on winter, to check for seasonal effects.

# additional interactions:
myeff3 <- includeInteraction(myeff3, quad, avAlt, name = "freq_run", interaction1 = c("", "kudonet"))
#check whether social influence ("contagion") is conditional on actors current activity level (see RSiena manual, p.57).


# e. effects on behavior from other variables
myeff3 <- includeEffects(myeff3, effFrom, name = "freq_run", interaction1 = "gender")
myeff3 <- includeEffects(myeff3, effFrom, name = "freq_run", interaction1 = "winter")



# check myeff object
print(myeff3)

##############################
######   4. estimation  ######
##############################

# a. define algorithm

myalgorithm <- sienaAlgorithmCreate(projname = "club1")

# b. estimate the model
# we estimate it until it reaches good convergence

try <- 1

ansM1 <- siena07(myalgorithm, data = mydata, effects = myeff3)
siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "test.html", sep = ""))

# the following script lets the model re-run until we get a good convergence ratio
while (TRUE){
  
  if(ansM1$tconv.max > 0.25){
    try <- try + 1
    print(paste("Try:", try, sep=" "))
    ansM1 <- siena07( myalgorithm, data = mydata, effects = myeff1, prevAns= ansM1)
    siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "ansM1_", "try", try, ".html", sep=""))
    
  }else{
    siena.table(ansM1, type="html", tstat=T, d=2, sig=T, file = paste("files", "/", "ansM1_", try, ".html", sep=""))
    print(paste("Reached convergence ratio of ", ansM1$tconv.max, sep = ""))
    break
  }
}
