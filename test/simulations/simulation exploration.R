# rsiena as abm

# clean the working environment 
#rm (list = ls( ))


# packages:
library(RSiena)


# load data for starting networks

load("clubdata.RData")

meanL_freq <- list()
meanL_vol <- list()

#for (c in 1:length(clubdata)) {
##c=2

# pick club
club <- clubdata[[c]]

# initial running distribution
mean(club$freq_run[,,1], na.rm=TRUE)
sd(club$freq_run[,,1], na.rm=TRUE)

psych::describe(club$freq_run[,,1])
# we reduce the data to only two time points
kudonet <- sienaDependent(club$kudo[,,1:2], allowOnly = FALSE)
freq_run <- sienaDependent(club$freq_run[,,1:2], type = "behavior", allowOnly = FALSE) 
time_run <- sienaDependent(club$time_run[,,1:2], type = "behavior", allowOnly = FALSE) 

# covariates
# changing covariates not possible with only 2 waves, so we make other activity a constant
freq_other <- coCovar(club$freq_other[, ,1])
time_other <- coCovar(club$time_other[, ,1])
gender <- coCovar(ifelse(club$male == 1, 1, 2))

# create a RSiena data object for both models: frequency and volume
mydata <- sienaDataCreate(kudonet, freq_run, freq_other, gender)
mydata2<- sienaDataCreate(kudonet, time_run, time_other, gender)

# load in the sienaFit object list, containing estimated parameters
# for the frequency and volume model
load(paste0("test/sienaFit/sienaFit_club", c, ".RData")) # freq.
ans <- sienaFit[[5]] # get object for main model (m5)
load(paste0("test/sienaFit/duration/sienaFit_club", c, ".RData")) # vol.
ans2 <- sienaFit[[5]]
#rm(sienaFit)

# make effects object
myeff <- getEffects(mydata)
myeff2 <- getEffects(mydata2)
# set initial values of basic effects for simulations based on estimated model
myeff$initialValue[myeff$include==T] <- ans$theta[c(1,12,13,28,39,40)]
myeff2$initialValue[myeff2$include==T] <- ans2$theta[c(1,12,13,28,39,40)]

# include extra effects and set initial value
# frequency model
{
  myeff <- setEffect(myeff, gwespFF, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="gwespFF")])
  myeff <- setEffect(myeff, outActSqrt, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="outActSqrt")])
  myeff <- setEffect(myeff, inPopSqrt, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="inPopSqrt")])
  myeff <- setEffect(myeff, outPopSqrt, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="outPopSqrt")])
  myeff <- setEffect(myeff, reciAct, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="reciAct")])
  myeff <- setEffect(myeff, outIso, name = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="outIso")])
  myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet")
  (eff1 <- myeff[myeff$include, ]$effect1[10])
  (eff2 <- myeff[myeff$include, ]$effect2[10])
  myeff <- setEffect(myeff, unspInt, effect1 = eff1, effect2 = eff2, initialValue = ans$theta[which(ans$effects$shortName=="unspInt")])
  myeff <- setEffect(myeff, higher, name = "kudonet", interaction1 = "freq_run", initialValue = ans$theta[which(ans$effects$shortName=="higher")])
  myeff <- setEffect(myeff, egoX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="egoX" & ans$effects$interaction1=="gender")])
  myeff <- setEffect(myeff, altX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="altX" & ans$effects$interaction1=="gender")])
  myeff <- setEffect(myeff, sameX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="sameX"& ans$effects$interaction1=="gender")])
  myeff <- setEffect(myeff, effFrom, name = "freq_run", interaction1 = "freq_other", initialValue = ans$theta[which(ans$effects$shortName=="effFrom" & ans$effects$interaction1=="freq_other")])
  myeff <- setEffect(myeff, effFrom, name = "freq_run", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="effFrom" & ans$effects$interaction1=="gender")])
  myeff <- setEffect(myeff, indeg, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="indeg")])
  myeff <- setEffect(myeff, avAttHigher, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="avAttHigher")])
  myeff <- setEffect(myeff, avAttLower, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="avAttLower")])
}

# volume model
{
  myeff2 <- setEffect(myeff2, gwespFF, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="gwespFF")])
  myeff2 <- setEffect(myeff2, outActSqrt, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="outActSqrt")])
  myeff2 <- setEffect(myeff2, inPopSqrt, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="inPopSqrt")])
  myeff2 <- setEffect(myeff2, outPopSqrt, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="outPopSqrt")])
  myeff2 <- setEffect(myeff2, reciAct, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="reciAct")])
  myeff2 <- setEffect(myeff2, outIso, name = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="outIso")])
  myeff2 <- includeInteraction(myeff2, recip, gwespFF, parameter = 69, name = "kudonet")
  (eff1 <- myeff2[myeff2$include, ]$effect1[10])
  (eff2 <- myeff2[myeff2$include, ]$effect2[10])
  myeff2 <- setEffect(myeff2, unspInt, effect1 = eff1, effect2 = eff2, initialValue = ans2$theta[which(ans2$effects$shortName=="unspInt")])
  myeff2 <- setEffect(myeff2, higher, name = "kudonet", interaction1 = "time_run", initialValue = ans2$theta[which(ans2$effects$shortName=="higher")])
  myeff2 <- setEffect(myeff2, egoX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="egoX" & ans2$effects$interaction1=="gender")])
  myeff2 <- setEffect(myeff2, altX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="altX" & ans2$effects$interaction1=="gender")])
  myeff2 <- setEffect(myeff2, sameX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="sameX"& ans2$effects$interaction1=="gender")])
  myeff2 <- setEffect(myeff2, effFrom, name = "time_run", interaction1 = "time_other", initialValue = ans2$theta[which(ans2$effects$shortName=="effFrom" & ans2$effects$interaction1=="time_other")])
  myeff2 <- setEffect(myeff2, effFrom, name = "time_run", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="effFrom" & ans2$effects$interaction1=="gender")])
  myeff2 <- setEffect(myeff2, indeg, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="indeg")])
  myeff2 <- setEffect(myeff2, avAttHigher, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="avAttHigher")])
  myeff2 <- setEffect(myeff2, avAttLower, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="avAttLower")])
}

# fix effects at this value
myeff$fix[myeff$include==T] <- TRUE 
myeff2$fix[myeff2$include==T] <- TRUE 

# I also specify models with no social influences, 
myeff_noinf <- setEffect(myeff, avAttHigher, name = "freq_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)
myeff_noinf <- setEffect(myeff_noinf, avAttLower, name = "freq_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)
myeff_noinf <- setEffect(myeff_noinf, indeg, name = "freq_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)

myeff2_noinf <- setEffect(myeff2, avAttHigher, name = "time_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)
myeff2_noinf <- setEffect(myeff2_noinf, avAttLower, name = "time_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)
myeff2_noinf <- setEffect(myeff2_noinf, indeg, name = "time_run", interaction1 = "kudonet", initialValue = 0, fix = TRUE)

# only indegree
myeff_indeg <- setEffect(myeff_noinf, indeg, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="indeg")], fix=TRUE)
myeff2_indeg <- setEffect(myeff2_noinf, indeg, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="indeg")], fix=TRUE)

# only upward assimilation,
myeff_nolow <- setEffect(myeff_noinf, avAttHigher, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="avAttHigher")], fix=TRUE)
myeff2_nolow <- setEffect(myeff2_noinf, avAttHigher, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="avAttHigher")], fix=TRUE)

# and only downward assimilation
myeff_nohigh <- setEffect(myeff_noinf, avAttLower, name = "freq_run", interaction1 = "kudonet", initialValue = ans$theta[which(ans$effects$shortName=="avAttLower")], fix=TRUE)
myeff2_nohigh <- setEffect(myeff2_noinf, avAttLower, name = "time_run", interaction1 = "kudonet", initialValue = ans2$theta[which(ans2$effects$shortName=="avAttLower")], fix=TRUE)

# set up the simulation settings
nIter <- 1000 # number of iterations
sim_model <- sienaAlgorithmCreate(
  projname = 'simulation',
  cond = FALSE,
  useStdInits = FALSE, nsub = 0,
  n3 = nIter, 
  seed=242452, # seed for replication
  simOnly = TRUE)

# I will extract the mean running frequency / volume values from the simulation runs
# make vectors to store means
meanSimO_freq <- meanSimO_vol <- rep(0, nIter)
meanSim_noinf_freq <- meanSim_noinf_vol <- rep(0, nIter)
meanSim_indeg_freq <- meanSim_indeg_vol <- rep(0, nIter)
meanSim_nolow_freq <- meanSim_nolow_vol <- rep(0, nIter)
meanSim_nohigh_freq <- meanSim_nohigh_vol <- rep(0, nIter)

# simulation using estimated parameters
sim_ans <- siena07(sim_model,          # simulation settings
                   data = mydata,      # data
                   effects = myeff,    # defined effects and set parameters
                   returnDeps = TRUE,  # return simulated networks and behaviors
                   returnChains = F,   # return sequences of micro-steps
                   batch = TRUE)
sim_ans2 <- siena07(sim_model,         
                    data = mydata2,      
                    effects = myeff2,    
                    returnDeps = TRUE,  
                    returnChains = F,
                    batch = TRUE)

sim_ans_noinf <- siena07(sim_model,         
                         data = mydata,     
                         effects = myeff_noinf,   
                         returnDeps = TRUE, 
                         returnChains = F,
                         batch = TRUE)
sim_ans2_noinf <- siena07(sim_model,         
                          data = mydata2,     
                          effects = myeff2_noinf,   
                          returnDeps = TRUE, 
                          returnChains = F,
                          batch = TRUE)

sim_ans_indeg <- siena07(sim_model,         
                         data = mydata,     
                         effects = myeff_indeg,   
                         returnDeps = TRUE, 
                         returnChains = F,
                         batch = TRUE)
sim_ans2_indeg <- siena07(sim_model,         
                          data = mydata2,     
                          effects = myeff2_indeg,   
                          returnDeps = TRUE, 
                          returnChains = F,
                          batch = TRUE)

sim_ans_nolow <- siena07(sim_model,         
                         data = mydata,     
                         effects = myeff_nolow,   
                         returnDeps = TRUE, 
                         returnChains = F,
                         batch = TRUE)
sim_ans2_nolow <- siena07(sim_model,         
                          data = mydata2,     
                          effects = myeff2_nolow,   
                          returnDeps = TRUE, 
                          returnChains = F,
                          batch = TRUE)

sim_ans_nohigh <- siena07(sim_model,         
                          data = mydata,     
                          effects = myeff_nohigh,   
                          returnDeps = TRUE, 
                          returnChains = F,
                          batch = TRUE)
sim_ans2_nohigh <- siena07(sim_model,         
                           data = mydata2,     
                           effects = myeff2_nohigh,   
                           returnDeps = TRUE, 
                           returnChains = F,
                           batch = TRUE)

# extract mean behavior values from simulation runs
for (i in 1:nIter) {
  meanSimO_freq[i] <- mean(sim_ans$sims[[i]][[1]]$freq_run[[1]])
  meanSimO_vol[i] <- mean(sim_ans2$sims[[i]][[1]]$time_run[[1]])
  meanSim_noinf_freq[i] <- mean(sim_ans_noinf$sims[[i]][[1]]$freq_run[[1]])
  meanSim_noinf_vol[i] <- mean(sim_ans2_noinf$sims[[i]][[1]]$time_run[[1]])
  meanSim_indeg_freq[i] <- mean(sim_ans_indeg$sims[[i]][[1]]$freq_run[[1]])
  meanSim_indeg_vol[i] <- mean(sim_ans2_indeg$sims[[i]][[1]]$time_run[[1]])
  meanSim_nolow_freq[i] <- mean(sim_ans_nolow$sims[[i]][[1]]$freq_run[[1]])
  meanSim_nolow_vol[i] <- mean(sim_ans2_nolow$sims[[i]][[1]]$time_run[[1]])
  meanSim_nohigh_freq[i] <- mean(sim_ans_nohigh$sims[[i]][[1]]$freq_run[[1]])
  meanSim_nohigh_vol[i] <- mean(sim_ans2_nohigh$sims[[i]][[1]]$time_run[[1]])
}

meanL_freq[[c]] <- data.frame(
  model = c(rep("obs", nIter), rep("no_inf", nIter), rep("indeg", nIter), rep("no_low", nIter), rep("no_high", nIter)),
  sim_means = c(meanSimO_freq, meanSim_noinf_freq, meanSim_indeg_freq, meanSim_nolow_freq, meanSim_nohigh_vol))

meanL_vol[[c]] <- data.frame(
  model = c(rep("obs", nIter), rep("no_inf", nIter), rep("indeg", nIter), rep("no_low", nIter), rep("no_high", nIter)),
  sim_means = c(meanSimO_vol, meanSim_noinf_vol, meanSim_indeg_vol, meanSim_nolow_vol, meanSim_nohigh_vol))


}

# compare means between no_inf model and empircally estimated model
# c=...
t.test(
  sim_means ~ model,
  data = meanL_freq[[c]][which(meanL_freq[[c]]$model=="obs" | meanL_freq[[c]]$model=="no_inf"),],
  var.equal=FALSE, na.rm=TRUE)

# calculate percentage difference
( (mean(meanL_freq[[c]][which(meanL_freq[[c]]$model=="no_inf"),2]) - mean(meanL_freq[[c]][which(meanL_freq[[c]]$model=="obs"),2])) /  mean(meanL_freq[[c]][which(meanL_freq[[c]]$model=="obs"),2]) ) * 100


# and between remaining models and no_inf model
for (m in unique(meanL_freq[[c]]$model)[-c(1,2)]) {
  print(t.test(
    sim_means ~ model, 
    data= meanL_freq[[c]][which(meanL_freq[[c]]$model=="no_inf" | meanL_freq[[c]]$model==m),], 
    var.equal=FALSE, na.rm=TRUE))
}

# calculate percentage difference
# freq model
for (m in unique(meanL_freq[[c]]$model)[-c(1,2)]) {
  print(( (mean(meanL_freq[[c]][which(meanL_freq[[c]]$model==m),2]) - mean(meanL_freq[[c]][which(meanL_freq[[c]]$model=="no_inf"),2])) /  mean(meanL_freq[[c]][which(meanL_freq[[c]]$model=="no_inf"),2]) ) * 100)
}


### same for volume...
t.test(
  sim_means ~ model,
  data = meanL_vol[[c]][which(meanL_vol[[c]]$model=="obs" | meanL_vol[[c]]$model=="no_inf"),],
  var.equal=FALSE, na.rm=TRUE)

# calculate percentage difference
( (mean(meanL_vol[[c]][which(meanL_vol[[c]]$model=="no_inf"),2]) - mean(meanL_vol[[c]][which(meanL_vol[[c]]$model=="obs"),2])) /  mean(meanL_vol[[c]][which(meanL_vol[[c]]$model=="obs"),2]) ) * 100


# and between remaining models and no_inf model
for (m in unique(meanL_vol[[c]]$model)[-c(1,2)]) {
  print(t.test(
    sim_means ~ model, 
    data= meanL_vol[[c]][which(meanL_vol[[c]]$model=="no_inf" | meanL_vol[[c]]$model==m),], 
    var.equal=FALSE, na.rm=TRUE))
}

# calculate percentage difference
# vol model
for (m in unique(meanL_vol[[c]]$model)[-c(1,2)]) {
  print(( (mean(meanL_vol[[c]][which(meanL_vol[[c]]$model==m),2]) - mean(meanL_vol[[c]][which(meanL_vol[[c]]$model=="no_inf"),2])) /  mean(meanL_vol[[c]][which(meanL_vol[[c]]$model=="no_inf"),2]) ) * 100)
}





