###########################################################
############## Influence effect illustration ############## 
##############       Rob Franken             ############## 
##############   Last edited 16-9-2021       ##############
###########################################################


# We illustrate the influence effect (i.c. avSim) using an ego-alter influence table.
# Note: later, we alter the function to reflect the avAttHigher effect,
# or assymetric influence effects are used (e.g., endowment function)

# First, we create an evaluation function for the behavior 
# that includes only relevant effects (i.e., those affected by changes in ego or alter attribute values)

obj_beh <- function (vi, vj) { #relevant effects
  b1*(vi-v_av) + b2*(vi-v_av)^2 + b3*(1 - abs(vi - vj)/r_v - sim_av)
  
  
  b4*
  
  
  # perhaps we need to include the avAttHigher effect above the avSim effect.
}

# Second, we create a function to calculate and then plot predicted ego-alter influence

alterInfluenceTableFunction <- function ( ) {
  #create the tables
  ( beh_tab <- outer(vv, vv, obj_beh) ) #calculate table for the evaluation effect
  
  #visualize the utility of various (or contribution to the objective function) of various decisions conditioned on ego's current behavior and alters' average behavior
  par( mfrow = c(2,5) )
  for (j in 1:5) {       #ego's current behavior
    for (i in 1:5) {     #ego's prospective behavior
      if (i == 1) {
        par( mar = c(5,4,3,1), cex = .75)
        plot( vv, vv, type = "n", 
              xlab = "Alter Average Score", 
              ylab =  "Predicted Utility",
              xlim = c( min(vv) - .5, max(vv) + .5 ),
              ylim = c( round(min(beh_tab) -2.5,0), round(max(beh_tab) +.5,0) ),
              main = paste("Ego Current score:", j) )
        legend( "bottom", c("Increase 1 Step", "No Change", "Decrease 1 Step"), col = c("darkgoldenrod1", "gray50", "cyan3"),
                lwd = c(3,2,2), lty = c(2,1,3), bty = "n" )
      }
      if ( abs (i-j) <= 1 ) {  # change of 0 or 1 steps
        if ( i > j) {          # prospectively increase
          points (vv, beh_tab[,i], type = "l", col = "darkgoldenrod1", lwd = 3, lty = 2)
        }
        else if ( i == j ) {   # prospectively remains the same
          points (vv, beh_tab[,i], type = "l", col = "gray50", lwd = 2, lty = 1)
        }
        else if ( i < j ) {   # prospectively decrease
          points (vv, beh_tab[,i], type = "l", col = "cyan3", lwd = 2, lty = 3)
        }
      }
    }
  }
}

# fill in the values of the parameter estimates and the averages (averages in 'print01Report'-function)
# here, we use hypothetical values
v_av <- 2.99
sim_av <- 0.685
b1 <- .4101  # linear
b2 <- .0574  # quad
b3 <- 4.9738 # avSim
vv <- c( 1:5 ) # behavior scores to be considered
r_v <- max(vv) - min(vv) # range of values

# and use the function to plot predicted utilities based on the above values
alterInfluenceTableFunction()

