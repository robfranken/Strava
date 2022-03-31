# we create myeff objects for each club, and save them
# for subsequent meta-analysis, we needed to ensure that the model specification for all our club-networks is identical. 
# in some clubs, some effects were rather important; in clubs where this was not the case, we fixed these to 0.



# Note: the script is adjusted a little bit to make effect objects for the running duration models.

{
  #########################################################################################################################
  # CLUB 1
  {
    #clean wd
    rm (list = ls( ))
    
    #load rsiena object for this club (mydata)
    load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", 1, ".RData", sep = "")) 
    #effectsDocumentation(myeff)
    # define effects
    myeff <- getEffects(mydata)
    
    {
      myeff <- includeEffects(myeff, gwespFF, name = "kudonet") 
      myeff <- includeEffects(myeff, outActSqrt, inPopSqrt, name = "kudonet") 
      myeff <- setEffect( myeff, outIso, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- includeEffects(myeff, egoX, altX, simX, higher, name = "kudonet", interaction1 = "time_run")
      myeff <- includeEffects(myeff, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )
      myeff <- setEffect( myeff, outPopSqrt, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- setEffect( myeff, reciAct, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet")
      (eff1 <- myeff[myeff$include,]$effect1[27])
      (eff2 <- myeff[myeff$include,]$effect2[27])
      myeff <- setEffect(myeff, unspInt, fix=TRUE, test=FALSE, effect1=eff1, effect2=eff2)
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "time_other")
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "gender")
    }
    print(myeff)
    
    {
      myeff1 <- includeEffects(myeff, indeg, name = "time_run", interaction1 = "kudonet")         # model 1: indegree
      myeff2 <- includeEffects(myeff1, avAlt, name = "time_run", interaction1 = "kudonet")        # model 2: avAlt
      myeff3 <- includeEffects(myeff1, avAttHigher, name = "time_run", interaction1 = "kudonet")  # model 3: avAttHigher
      myeff4 <- includeEffects(myeff1, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 4: avAttLower
      myeff5 <- includeEffects(myeff3, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 5: avAttHigher+Lower
      myeff6 <- includeEffects(myeff1, avSim, name = "time_run", interaction1 = "kudonet")        # model 6: avSim
    }

    
    # make a list
    myeff <- list(myeff1, myeff2, myeff3, myeff4, myeff5, myeff6)
    # save
    save(myeff, file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club1", ".RData", sep = ""))

  }
  
  #########################################################################################################################
  
  # CLUB 2
  {
    #clean wd
    rm (list = ls( ))
    
    #load rsiena object for this club (mydata)
    load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", 2, ".RData", sep = "")) 
    #effectsDocumentation(myeff)
    # define effects
    myeff <- getEffects(mydata)
    
    {
      myeff <- includeEffects(myeff, gwespFF, name = "kudonet") 
      myeff <- includeEffects(myeff, outActSqrt, inPopSqrt, name = "kudonet") 
      myeff <- includeEffects( myeff, outIso, name = "kudonet")
      myeff <- includeEffects(myeff, egoX, altX, simX, higher, name = "kudonet", interaction1 = "time_run")
      myeff <- includeEffects(myeff, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )
      myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet")
      myeff <- includeEffects(myeff, reciAct, name = "kudonet")
      myeff <- setEffect( myeff, outPopSqrt, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "time_other")
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "gender")
    }
    print(myeff) # 39 indeed
    
    
    {
      myeff1 <- includeEffects(myeff, indeg, name = "time_run", interaction1 = "kudonet")         # model 1: indegree
      myeff2 <- includeEffects(myeff1, avAlt, name = "time_run", interaction1 = "kudonet")        # model 2: avAlt
      myeff3 <- includeEffects(myeff1, avAttHigher, name = "time_run", interaction1 = "kudonet")  # model 3: avAttHigher
      myeff4 <- includeEffects(myeff1, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 4: avAttLower
      myeff5 <- includeEffects(myeff3, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 5: avAttHigher+Lower
      myeff6 <- includeEffects(myeff1, avSim, name = "time_run", interaction1 = "kudonet")        # model 6: avSim
    }


    
    # make a list
    myeff <- list(myeff1, myeff2, myeff3, myeff4, myeff5, myeff6)
    # save
    save(myeff, file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club2", ".RData", sep = ""))
    
    
  }
  
  #########################################################################################################################
  
  # CLUB 3
  {
    #clean wd
    rm (list = ls( ))
    
    #load rsiena object for this club (mydata)
    load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", 3, ".RData", sep = "")) 
    #effectsDocumentation(myeff)
    # define effects
    myeff <- getEffects(mydata)
    
    {
      myeff <- includeEffects(myeff, gwespFF, name = "kudonet") 
      myeff <- includeEffects(myeff, outActSqrt, inPopSqrt, name = "kudonet") 
      myeff <- includeEffects( myeff, outIso, name = "kudonet")
      myeff <- includeEffects(myeff, egoX, altX, simX, higher, name = "kudonet", interaction1 = "time_run")
      myeff <- includeEffects(myeff, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )
      myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet")
      myeff <- includeEffects(myeff, outPopSqrt, name = "kudonet")
      myeff <- setEffect( myeff, reciAct, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "time_other")
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "gender")
    }
    
    print(myeff) # 39 indeed
    
    {
      myeff1 <- includeEffects(myeff, indeg, name = "time_run", interaction1 = "kudonet")         # model 1: indegree
      myeff2 <- includeEffects(myeff1, avAlt, name = "time_run", interaction1 = "kudonet")        # model 2: avAlt
      myeff3 <- includeEffects(myeff1, avAttHigher, name = "time_run", interaction1 = "kudonet")  # model 3: avAttHigher
      myeff4 <- includeEffects(myeff1, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 4: avAttLower
      myeff5 <- includeEffects(myeff3, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 5: avAttHigher+Lower
      myeff6 <- includeEffects(myeff1, avSim, name = "time_run", interaction1 = "kudonet")        # model 6: avSim
    }
    

    
    # make a list
    myeff <- list(myeff1, myeff2, myeff3, myeff4, myeff5, myeff6)
    # save
    save(myeff, file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club3", ".RData", sep = ""))
    

  }
  
  #########################################################################################################################
  
  # CLUB 4
  {
    #clean wd
    rm (list = ls( ))
    
    #load rsiena object for this club (mydata)
    load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", 4, ".RData", sep = "")) 
    #effectsDocumentation(myeff)
    # define effects
    myeff <- getEffects(mydata)
    
    {
      myeff <- includeEffects(myeff, gwespFF, name = "kudonet") 
      myeff <- includeEffects(myeff, outActSqrt, inPopSqrt, name = "kudonet") 
      myeff <- includeEffects( myeff, outIso, name = "kudonet")
      myeff <- includeEffects(myeff, egoX, altX, simX, higher, name = "kudonet", interaction1 = "time_run")
      myeff <- setEffect( myeff, egoX, name = "kudonet", interaction1 = "gender", fix=TRUE, test=FALSE, initialValue = 0)
      myeff <- setEffect( myeff, altX, name = "kudonet", interaction1 = "gender", fix=TRUE, test=FALSE, initialValue = 0)
      myeff <- setEffect( myeff, sameX, name = "kudonet", interaction1 = "gender", fix=TRUE, test=FALSE, initialValue = 0)
      myeff <- setEffect(myeff, outPopSqrt, name = "kudonet", fix=TRUE, test=FALSE, initialValue = 0)
      myeff <- setEffect(myeff, reciAct, name = "kudonet", fix=TRUE, test=FALSE, initialValue = 0)
      (myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet"))
      (eff1 <- myeff[myeff$include,]$effect1[27])
      (eff2 <- myeff[myeff$include,]$effect2[27])
      myeff <- setEffect(myeff, unspInt, fix=TRUE, test=FALSE, effect1=eff1, effect2=eff2)
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "time_other")
      myeff <- setEffect(myeff, effFrom, name = "time_run", interaction1 = "gender", fix=T, test=F, initialValue=0)
  
    }
    
    print(myeff) # 39 indeed
    
    {
      myeff1 <- includeEffects(myeff, indeg, name = "time_run", interaction1 = "kudonet")         # model 1: indegree
      myeff2 <- includeEffects(myeff1, avAlt, name = "time_run", interaction1 = "kudonet")        # model 2: avAlt
      myeff3 <- includeEffects(myeff1, avAttHigher, name = "time_run", interaction1 = "kudonet")  # model 3: avAttHigher
      myeff4 <- includeEffects(myeff1, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 4: avAttLower
      myeff5 <- includeEffects(myeff3, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 5: avAttHigher+Lower
      myeff6 <- includeEffects(myeff1, avSim, name = "time_run", interaction1 = "kudonet")        # model 6: avSim
    }
    

    
    # make a list
    myeff <- list(myeff1, myeff2, myeff3, myeff4, myeff5, myeff6)
    # save
    save(myeff, file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club4", ".RData", sep = ""))
    
    
  
  #########################################################################################################################
  
  # CLUB 5
  # here, GOF was not reached, yet...
  {
    #clean wd
    rm (list = ls( ))
    
    #load rsiena object for this club (mydata)
    load(file=paste("test", "/", "mydata", "/", "duration", "/", "mydata_club", 5, ".RData", sep = "")) 
    #effectsDocumentation(myeff)
    # define effects
    myeff <- getEffects(mydata)
    
    {
      myeff <- includeEffects(myeff, gwespFF, name = "kudonet") 
      myeff <- includeEffects(myeff, outActSqrt, inPopSqrt, name = "kudonet") 
      myeff <- includeEffects( myeff, outIso, name = "kudonet")
      myeff <- includeEffects(myeff, egoX, altX, simX, higher, name = "kudonet", interaction1 = "time_run")
      myeff <- includeEffects(myeff, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )
      myeff <- includeInteraction(myeff, recip, gwespFF, parameter = 69, name = "kudonet")
      myeff <- setEffect( myeff, outPopSqrt, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- setEffect( myeff, reciAct, name = "kudonet", fix = TRUE, test = FALSE, initialValue = 0)
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "time_other")
      myeff <- includeEffects(myeff, effFrom, name = "time_run", interaction1 = "gender")
    }
    
    print(myeff) # 39 indeed
    
    {
      myeff1 <- includeEffects(myeff, indeg, name = "time_run", interaction1 = "kudonet")         # model 1: indegree
      myeff2 <- includeEffects(myeff1, avAlt, name = "time_run", interaction1 = "kudonet")        # model 2: avAlt
      myeff3 <- includeEffects(myeff1, avAttHigher, name = "time_run", interaction1 = "kudonet")  # model 3: avAttHigher
      myeff4 <- includeEffects(myeff1, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 4: avAttLower
      myeff5 <- includeEffects(myeff3, avAttLower, name = "time_run", interaction1 = "kudonet")   # model 5: avAttHigher+Lower
      myeff6 <- includeEffects(myeff1, avSim, name = "time_run", interaction1 = "kudonet")        # model 6: avSim
    }

    
    # make a list
    myeff <- list(myeff1, myeff2, myeff3, myeff4, myeff5, myeff6)
    # save
    save(myeff, file=paste("test", "/", "myeff", "/", "duration", "/", "myeff_club5", ".RData", sep = ""))
    
    
  }
  
  #########################################################################################################################
  }
}
