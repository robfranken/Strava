
# clean the working environment 
rm (list = ls( ))

# packages:
library(RSiena)
library(sna)
library(lattice)  # for plotting
library(RColorBrewer) # color palettes

load("clubdata.RData")


plotL <- list() # list to store plots in

for (c in 1:length(clubdata)) {
  
  # pick club
  club <- clubdata[[c]]
  
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
  load(paste0("test/sienaFit/volume/sienaFit_club", c, ".RData")) # vol.
  ans2 <- sienaFit[[5]]
  
  # make effects object
  myeff <- getEffects(mydata)
  myeff2 <- getEffects(mydata2)
  # set initial values of basic effects for simulations based on estimated model
  myeff$initialValue[myeff$include==T] <- ans$theta[c(1,12,13,25,36,37)]
  myeff2$initialValue[myeff2$include==T] <- ans2$theta[c(1,12,13,25,36,37)]
  
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
    myeff <- setEffect(myeff, egoX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="egoX")])
    myeff <- setEffect(myeff, altX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="altX")])
    myeff <- setEffect(myeff, sameX, name = "kudonet", interaction1 = "gender", initialValue = ans$theta[which(ans$effects$shortName=="sameX")])
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
    myeff2 <- setEffect(myeff2, egoX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="egoX")])
    myeff2 <- setEffect(myeff2, altX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="altX")])
    myeff2 <- setEffect(myeff2, sameX, name = "kudonet", interaction1 = "gender", initialValue = ans2$theta[which(ans2$effects$shortName=="sameX")])
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
  nIter <- 300 # number of iterations
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
                     returnChains = TRUE,# return sequences of micro-steps
                     batch = TRUE)
  sim_ans2 <- siena07(sim_model,         
                      data = mydata2,      
                      effects = myeff2,    
                      returnDeps = TRUE,  
                      returnChains = TRUE,
                      batch = TRUE)
  
  sim_ans_noinf <- siena07(sim_model,         
                           data = mydata,     
                           effects = myeff_noinf,   
                           returnDeps = TRUE, 
                           returnChains = TRUE,
                           batch = TRUE)
  sim_ans2_noinf <- siena07(sim_model,         
                            data = mydata2,     
                            effects = myeff2_noinf,   
                            returnDeps = TRUE, 
                            returnChains = TRUE,
                            batch = TRUE)
  
  sim_ans_indeg <- siena07(sim_model,         
                           data = mydata,     
                           effects = myeff_indeg,   
                           returnDeps = TRUE, 
                           returnChains = TRUE,
                           batch = TRUE)
  sim_ans2_indeg <- siena07(sim_model,         
                            data = mydata2,     
                            effects = myeff2_indeg,   
                            returnDeps = TRUE, 
                            returnChains = TRUE,
                            batch = TRUE)
  
  sim_ans_nolow <- siena07(sim_model,         
                           data = mydata,     
                           effects = myeff_nolow,   
                           returnDeps = TRUE, 
                           returnChains = TRUE,
                           batch = TRUE)
  sim_ans2_nolow <- siena07(sim_model,         
                            data = mydata2,     
                            effects = myeff2_nolow,   
                            returnDeps = TRUE, 
                            returnChains = TRUE,
                            batch = TRUE)
  
  sim_ans_nohigh <- siena07(sim_model,         
                            data = mydata,     
                            effects = myeff_nohigh,   
                            returnDeps = TRUE, 
                            returnChains = TRUE,
                            batch = TRUE)
  sim_ans2_nohigh <- siena07(sim_model,         
                             data = mydata2,     
                             effects = myeff2_nohigh,   
                             returnDeps = TRUE, 
                             returnChains = TRUE,
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
  #str(sim_ans$sims[[1]]) # numbering is as follows: nIter, group number, DV, period number
  
  # also store observed mean at t2
  meanObs_freq <- mean(club$freq_run[,,2])
  meanObs_vol <- mean(club$time_run[,,2])
  
  #  make a layout for the plots
  dev.off()
  l <- layout(matrix(c(1, 1, 2, 2, 13,  # box plots
                       3, 5, 7, 9, 11, # development of sim_freq means over continuous time
                       4, 6, 8, 10, 12), # sim_vol
                     nrow = 3,
                     ncol = 5,
                     byrow = TRUE))
  #layout.show(l)
  
  # make data for plotting
  plot_data_freq <- rbind(
    data.frame(cond="Observed", mean = meanSimO_freq),
    data.frame(cond="No_inf", mean = meanSim_noinf_freq),
    data.frame(cond="Indeg", mean = meanSim_indeg_freq),
    data.frame(cond="No_avAttL", mean = meanSim_nolow_freq),
    data.frame(cond="No_avAttH", mean = meanSim_nohigh_freq)
  )
  plot_data_vol <- rbind(
    data.frame(cond="Observed", mean = meanSimO_vol),
    data.frame(cond="No_inf", mean = meanSim_noinf_vol),
    data.frame(cond="Indeg", mean = meanSim_indeg_vol),
    data.frame(cond="No_avAttL", mean = meanSim_nolow_vol),
    data.frame(cond="No_avAttH", mean = meanSim_nohigh_vol)
  )
  
  # reorder conditions
  plot_data_freq$cond <- factor(plot_data_freq$cond, levels=c("Observed", "No_inf", "Indeg", "No_avAttL", "No_avAttH"))
  plot_data_vol$cond <- factor(plot_data_vol$cond, levels=c("Observed", "No_inf", "Indeg", "No_avAttL", "No_avAttH"))
  color <- brewer.pal(5, "Set3") # get colors for boxplots
  {
    boxplot(mean ~ cond, data = plot_data_freq, main = paste("Simulation results across", nIter, "iterations"),
            xlab = "Simulation", ylab = "Average running frequency", col = color)
    abline(h=meanObs_freq, col = "brown") # add the observed mean running at t2
  }
  {
    boxplot(mean ~ cond, data = plot_data_vol, main = paste("Simulation results across", nIter, "iterations"),
            xlab = "Simulation", ylab = "Average running volume", col = color)
    abline(h=meanObs_freq, col = "brown") # add the observed mean volume at t2
  }

  
  # I examine the running frequency and volume changes in greater detail
  n_actors <- club$netsize
  
  Zs5_freq <- Zs5_vol <- array(NA, dim=c(n_actors,1))
  Zb5_freq <- Zb5_vol <- array (0, dim=c(nIter,1))
  for (i in 1: nIter) {
    for (j in 1:n_actors) {
      # simulated behavior of actor j in iteration i
      Zs5_freq[j,1] <- sim_ans$sims[[i]][[1]]$freq_run[[1]][[j]]
      Zs5_vol[j,1] <- sim_ans2$sims[[i]][[1]]$time_run[[1]][[j]]
    }
    # mean simulated behavior over actors in iteration i
    Zb5_freq[i] <-colSums(Zs5_freq, na.rm=T)/n_actors
    Zb5_vol[i] <-colSums(Zs5_vol, na.rm=T)/n_actors
  }
  
  #colMeans(cbind(club$freq_run[,,1:2])) # observed means at t1 and t2
  #colMeans(Zb5)  # average mean simulated behavior value
  #mean( sim_ans$sims[[nIter]][[1]]$freq_run[[1]] )# mean of last simulation run
  
  # I plot the mean simulated running behavior over time 
  # extract mean running for all chains, with network change opportunities included
  simChanges_freq <- simChanges_vol <- rep(0, nIter)
  for (i in 1: nIter) {
    simChanges_freq[i] <- length(sim_ans$chain[[i]][[1]][[1]]) 
    simChanges_vol[i] <- length(sim_ans2$chain[[i]][[1]][[1]]) 
    }
  maxChanges_freq <- max(simChanges_freq)
  maxChanges_vol <- max(simChanges_vol)
  
  seqs_freq <- matrix(Zb5_freq, nr= nIter, nc=maxChanges_freq+1)  # fill matrix with final mean
  seqs_vol <- matrix(Zb5_vol, nr= nIter, nc=maxChanges_vol+1)
  seqs_freq[,1] <- mean(club$freq_run[,,1])   # set t1 mean to observed level
  seqs_vol[,1] <- mean(club$time_run[,,1]) 
  
  for (i in 1: nIter) {
    datChain_freq <- t(matrix(unlist(sim_ans$chain[[i]][[1]][[1]]), nc=length(sim_ans$chain[[i]][[1]][[1]]))) 
    datChain_vol <- t(matrix(unlist(sim_ans2$chain[[i]][[1]][[1]]), nc=length(sim_ans2$chain[[i]][[1]][[1]]))) 
    for (j in 1: simChanges_freq[i]) {
      if (datChain_freq[j,2]=="0") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j]
      }
      if (datChain_freq[j,2]=="1") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j] + (as.numeric(datChain_freq[j,6])/n_actors)
      }
    }
    for (k in 1: simChanges_vol[i]) {
      if (datChain_vol[k,2]=="0") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k]
      }
      if (datChain_vol[k,2]=="1") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k] + (as.numeric(datChain_vol[k,6])/n_actors)
      }
    }
    
  }
  
  # plot multiple iterations using loess curves
  micros_freq <- 1:dim(seqs_freq)[2]
  micros_vol <- 1:dim(seqs_vol)[2]
  lo1_freq <- loess(seqs_freq[1,] ~ micros_freq)
  lo1_vol <- loess(seqs_vol[1,] ~ micros_vol)
  l1_freq <- predict(lo1_freq, micros_freq)
  l1_vol <- predict(lo1_vol, micros_vol)
  
  # set ylim; so it is consistent across plots
  ymin_freq <- plyr::round_any(min(seqs_freq), .5, f=floor)
  ymin_vol <- plyr::round_any(min(seqs_vol), .5, f=floor)
  ymax_freq <- plyr::round_any(max(seqs_freq), .5, f=ceiling)
  ymax_vol <- plyr::round_any(max(seqs_vol), .5, f=ceiling)
  
  plot(x=1:dim(seqs_freq)[2], y=seqs_freq[1,], ylim=c(ymin_freq,ymax_freq),type="l", 
       ylab='Average running frequency', xlab='Micro-step', col='white',
       main='Mean frequency over time:
     full model')	
  for (i in 1:25) {
    lines(x=micros_freq, y=predict(loess(seqs_freq[i,] ~ micros_freq), micros_freq), col=colors()[i*10])
  }
  
  plot(x=1:dim(seqs_vol)[2], y=seqs_vol[1,], ylim=c(ymin_vol,ymax_vol),type="l", 
       ylab='Average running volume', xlab='Micro-step', col='white',
       main='Mean volume over time:
     full model')	
  for (i in 1:25) {
    lines(x=micros_vol, y=predict(loess(seqs_vol[i,] ~ micros_vol), micros_vol), col=colors()[i*10])
  }
  
  
  # do the same for the model with no peer influence whatsoever. 
  for (i in 1: nIter) {
    for (j in 1:n_actors) {
      # simulated behavior of actor j in iteration i
      Zs5_freq[j,1] <- sim_ans_noinf$sims[[i]][[1]]$freq_run[[1]][[j]]
      Zs5_vol[j,1] <- sim_ans2_noinf$sims[[i]][[1]]$time_run[[1]][[j]]
    }
    # mean simulated behavior over actors in iteration i
    Zb5_freq[i] <-colSums(Zs5_freq, na.rm=T)/n_actors
    Zb5_vol[i] <-colSums(Zs5_vol, na.rm=T)/n_actors
  }
  
  for (i in 1: nIter) {
    datChain_freq <- t(matrix(unlist(sim_ans_noinf$chain[[i]][[1]][[1]]), nc=length(sim_ans_noinf$chain[[i]][[1]][[1]]))) 
    datChain_vol <- t(matrix(unlist(sim_ans2_noinf$chain[[i]][[1]][[1]]), nc=length(sim_ans2_noinf$chain[[i]][[1]][[1]]))) 
    for (j in 1: simChanges_freq[i]) {
      if (datChain_freq[j,2]=="0") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j]
      }
      if (datChain_freq[j,2]=="1") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j] + (as.numeric(datChain_freq[j,6])/n_actors)
      }
    }
    for (k in 1: simChanges_vol[i]) {
      if (datChain_vol[k,2]=="0") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k]
      }
      if (datChain_vol[k,2]=="1") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k] + (as.numeric(datChain_vol[k,6])/n_actors)
      }
    }
    
  }
  
  micros_freq <- 1:dim(seqs_freq)[2]
  micros_vol <- 1:dim(seqs_vol)[2]
  lo1_freq <- loess(seqs_freq[1,] ~ micros_freq)
  lo1_vol <- loess(seqs_vol[1,] ~ micros_vol)
  l1_freq <- predict(lo1_freq, micros_freq)
  l1_vol <- predict(lo1_vol, micros_vol)
  
  plot(x=1:dim(seqs_freq)[2], y=seqs_freq[1,], ylim=c(ymin_freq,ymax_freq),type="l", 
       ylab='Average running frequency', xlab='Micro-step', col='white',
       main='Mean frequency over time:
     no influence')	
  for (i in 1:25) {
    lines(x=micros_freq, y=predict(loess(seqs_freq[i,] ~ micros_freq), micros_freq), col=colors()[i*10])
  }
  
  plot(x=1:dim(seqs_vol)[2], y=seqs_vol[1,], ylim=c(ymin_vol,ymax_vol),type="l", 
       ylab='Average running volume', xlab='Micro-step', col='white',
       main='Mean volume over time:
     no influence')	
  for (i in 1:25) {
    lines(x=micros_vol, y=predict(loess(seqs_vol[i,] ~ micros_vol), micros_vol), col=colors()[i*10])
  }
  

  # model with only indegree
  for (i in 1: nIter) {
    for (j in 1:n_actors) {
      # simulated behavior of actor j in iteration i
      Zs5_freq[j,1] <- sim_ans_indeg$sims[[i]][[1]]$freq_run[[1]][[j]]
      Zs5_vol[j,1] <- sim_ans2_indeg$sims[[i]][[1]]$time_run[[1]][[j]]
    }
    # mean simulated behavior over actors in iteration i
    Zb5_freq[i] <-colSums(Zs5_freq, na.rm=T)/n_actors
    Zb5_vol[i] <-colSums(Zs5_vol, na.rm=T)/n_actors
  }
  
  for (i in 1: nIter) {
    datChain_freq <- t(matrix(unlist(sim_ans_indeg$chain[[i]][[1]][[1]]), nc=length(sim_ans_indeg$chain[[i]][[1]][[1]]))) 
    datChain_vol <- t(matrix(unlist(sim_ans2_indeg$chain[[i]][[1]][[1]]), nc=length(sim_ans2_indeg$chain[[i]][[1]][[1]]))) 
    for (j in 1: simChanges_freq[i]) {
      if (datChain_freq[j,2]=="0") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j]
      }
      if (datChain_freq[j,2]=="1") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j] + (as.numeric(datChain_freq[j,6])/n_actors)
      }
    }
    for (k in 1: simChanges_vol[i]) {
      if (datChain_vol[k,2]=="0") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k]
      }
      if (datChain_vol[k,2]=="1") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k] + (as.numeric(datChain_vol[k,6])/n_actors)
      }
    }
    
  }
  
  micros_freq <- 1:dim(seqs_freq)[2]
  micros_vol <- 1:dim(seqs_vol)[2]
  lo1_freq <- loess(seqs_freq[1,] ~ micros_freq)
  lo1_vol <- loess(seqs_vol[1,] ~ micros_vol)
  l1_freq <- predict(lo1_freq, micros_freq)
  l1_vol <- predict(lo1_vol, micros_vol)
  
  plot(x=1:dim(seqs_freq)[2], y=seqs_freq[1,], ylim=c(ymin_freq,ymax_freq),type="l", 
       ylab='Average running frequency', xlab='Micro-step', col='white',
       main='Mean frequency over time:
     only indegree')	
  for (i in 1:25) {
    lines(x=micros_freq, y=predict(loess(seqs_freq[i,] ~ micros_freq), micros_freq), col=colors()[i*10])
  }
  
  plot(x=1:dim(seqs_vol)[2], y=seqs_vol[1,], ylim=c(ymin_vol,ymax_vol),type="l", 
       ylab='Average running volume', xlab='Micro-step', col='white',
       main='Mean volume over time:
     only indegree')	
  for (i in 1:25) {
    lines(x=micros_vol, y=predict(loess(seqs_vol[i,] ~ micros_vol), micros_vol), col=colors()[i*10])
  }
  
  # no downward assimilation.
  for (i in 1: nIter) {
    for (j in 1:n_actors) {
      # simulated behavior of actor j in iteration i
      Zs5_freq[j,1] <- sim_ans_nolow$sims[[i]][[1]]$freq_run[[1]][[j]]
      Zs5_vol[j,1] <- sim_ans2_nolow$sims[[i]][[1]]$time_run[[1]][[j]]
    }
    # mean simulated behavior over actors in iteration i
    Zb5_freq[i] <-colSums(Zs5_freq, na.rm=T)/n_actors
    Zb5_vol[i] <-colSums(Zs5_vol, na.rm=T)/n_actors
  }
  
  for (i in 1: nIter) {
    datChain_freq <- t(matrix(unlist(sim_ans_nolow$chain[[i]][[1]][[1]]), nc=length(sim_ans_nolow$chain[[i]][[1]][[1]]))) 
    datChain_vol <- t(matrix(unlist(sim_ans2_nolow$chain[[i]][[1]][[1]]), nc=length(sim_ans2_nolow$chain[[i]][[1]][[1]]))) 
    for (j in 1: simChanges_freq[i]) {
      if (datChain_freq[j,2]=="0") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j]
      }
      if (datChain_freq[j,2]=="1") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j] + (as.numeric(datChain_freq[j,6])/n_actors)
      }
    }
    for (k in 1: simChanges_vol[i]) {
      if (datChain_vol[k,2]=="0") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k]
      }
      if (datChain_vol[k,2]=="1") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k] + (as.numeric(datChain_vol[k,6])/n_actors)
      }
    }
    
  }
  
  micros_freq <- 1:dim(seqs_freq)[2]
  micros_vol <- 1:dim(seqs_vol)[2]
  lo1_freq <- loess(seqs_freq[1,] ~ micros_freq)
  lo1_vol <- loess(seqs_vol[1,] ~ micros_vol)
  l1_freq <- predict(lo1_freq, micros_freq)
  l1_vol <- predict(lo1_vol, micros_vol)
  
  plot(x=1:dim(seqs_freq)[2], y=seqs_freq[1,], ylim=c(ymin_freq,ymax_freq),type="l", 
       ylab='Average running frequency', xlab='Micro-step', col='white',
       main='Mean frequency over time:
     only avAttHigher')	
  for (i in 1:25) {
    lines(x=micros_freq, y=predict(loess(seqs_freq[i,] ~ micros_freq), micros_freq), col=colors()[i*10])
  }
  
  plot(x=1:dim(seqs_vol)[2], y=seqs_vol[1,], ylim=c(ymin_vol,ymax_vol),type="l", 
       ylab='Average running volume', xlab='Micro-step', col='white',
       main='Mean volume over time:
     only avAttHigher')	
  for (i in 1:25) {
    lines(x=micros_vol, y=predict(loess(seqs_vol[i,] ~ micros_vol), micros_vol), col=colors()[i*10])
  }

  # no upward assimilation. 
  for (i in 1: nIter) {
    for (j in 1:n_actors) {
      # simulated behavior of actor j in iteration i
      Zs5_freq[j,1] <- sim_ans_nohigh$sims[[i]][[1]]$freq_run[[1]][[j]]
      Zs5_vol[j,1] <- sim_ans2_nohigh$sims[[i]][[1]]$time_run[[1]][[j]]
    }
    # mean simulated behavior over actors in iteration i
    Zb5_freq[i] <-colSums(Zs5_freq, na.rm=T)/n_actors
    Zb5_vol[i] <-colSums(Zs5_vol, na.rm=T)/n_actors
  }
  
  for (i in 1: nIter) {
    datChain_freq <- t(matrix(unlist(sim_ans_nohigh$chain[[i]][[1]][[1]]), nc=length(sim_ans_nohigh$chain[[i]][[1]][[1]]))) 
    datChain_vol <- t(matrix(unlist(sim_ans2_nohigh$chain[[i]][[1]][[1]]), nc=length(sim_ans2_nohigh$chain[[i]][[1]][[1]]))) 
    for (j in 1: simChanges_freq[i]) {
      if (datChain_freq[j,2]=="0") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j]
      }
      if (datChain_freq[j,2]=="1") { 
        seqs_freq[i,j+1] <- seqs_freq[i,j] + (as.numeric(datChain_freq[j,6])/n_actors)
      }
    }
    for (k in 1: simChanges_vol[i]) {
      if (datChain_vol[k,2]=="0") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k]
      }
      if (datChain_vol[k,2]=="1") { 
        seqs_vol[i,k+1] <- seqs_vol[i,k] + (as.numeric(datChain_vol[k,6])/n_actors)
      }
    }
    
  }
  
  micros_freq <- 1:dim(seqs_freq)[2]
  micros_vol <- 1:dim(seqs_vol)[2]
  lo1_freq <- loess(seqs_freq[1,] ~ micros_freq)
  lo1_vol <- loess(seqs_vol[1,] ~ micros_vol)
  l1_freq <- predict(lo1_freq, micros_freq)
  l1_vol <- predict(lo1_vol, micros_vol)
  
  plot(x=1:dim(seqs_freq)[2], y=seqs_freq[1,], ylim=c(ymin_freq,ymax_freq),type="l", 
       ylab='Average running frequency', xlab='Micro-step', col='white',
       main='Mean frequency over time:
     only avAttLower')	
  for (i in 1:25) {
    lines(x=micros_freq, y=predict(loess(seqs_freq[i,] ~ micros_freq), micros_freq), col=colors()[i*10])
  }
  
  plot(x=1:dim(seqs_vol)[2], y=seqs_vol[1,], ylim=c(ymin_vol,ymax_vol),type="l", 
       ylab='Average running volume', xlab='Micro-step', col='white',
       main='Mean volume over time:
     only avAttLower')	
  for (i in 1:25) {
    lines(x=micros_vol, y=predict(loess(seqs_vol[i,] ~ micros_vol), micros_vol), col=colors()[i*10])
  }
  
  
  #save the plot to the list
  plotL[[c]] <- recordPlot()
  
}

plotL[[1]]
warnings()
  