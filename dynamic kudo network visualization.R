#################################
#################################
#
# Network visualization: 
# Static and Dynamic networks
# for the STRAVA paper

# see: https://kateto.net/network-visualization
# and: https://cran.r-project.org/web/packages/networkDynamic/vignettes/networkDynamic.pdf

# Rob Franken
# Last edited 7-7-2021
#
#################################
#################################

# clean the working environment 
rm (list = ls( ))

# we set the wd to the folder containing our (raw) club list (in our case, the local GitHub folder)
work_dir <- "C:/Users/u244147/Documents/GitHub/Stravajournal"
setwd(work_dir)

# we load in the data
load("clubdata.Rdata")

#################################

# first, a static network visualization (using the 'igraph'-package)
library(igraph)

# let's take small network (club 4, n=13)
club <- clubdata[[4]]

# and take the kudo network at t1
knet1 <- RSiena::coDyadCovar(data.matrix(club$kudo[,,1])) 

# turn it into an igraph object
G <- igraph::graph_from_adjacency_matrix(knet1, mode = "directed", diag = FALSE) 

# plot it
plot(G)

# let's alter vertice attributes: for now, sex (male vs. not male)
V(G)$color <- ifelse(club$male == 1, "orange", "lightsteelblue2") # vertice color indicating sex (male vs. not male)
plot(G)

# we make a network layout: we use a force-directed layout algorithm (eg, Fruchterman-Reingold, Kamada Kawai, Graphopt)
# we save the layout in object l (this allows us to get the exact same layout every time; which is helpful for plotting the time evolution of a graph).
#l <- layout_with_fr(G)
l <- layout_with_kk(G)
#l <- layout_with_graphopt(G)

# and we alter some plotting parameters
dev.off()
plot(G,
     
     main = "Kudo network of Strava club (N=13) at T1",
     sub = "Note: Tie exists if at least one kudo was awarded by actor i to actor j)",
     
     # === vertex
     vertex.frame.color ="black",          
     vertex.shape=vertex_attr(G)$shape, # adjustable                   
     vertex.size=vertex_attr(G)$size,   # adjustable
     vertex.size2=NA,                            
     
     # === vertex label
     vertex.label=NA,                             
     vertex.label.family="Times",              
     vertex.label.font=2,                     
     vertex.label.cex=1,                           
     vertex.label.dist=0,                        
     vertex.label.degree=0 ,                      
     
     # === Edge
     edge.color="grey",                           
     edge.width=1,                               
     edge.arrow.size=.3,                      
     edge.arrow.width=2,                         
     edge.lty="solid",                             
     edge.curved=.2,
     
     # === Layout
     layout = l
     )

# add a legend
legend(x=-.5, y=-1.1, c("Male", "Not male"), pch=21,
       col="#777777", pt.bg=c("orange", "lightsteelblue2"), pt.cex=2, cex=1, bty="n", ncol=1)


##########################################
##########################################

# now, let's visualize the kudo network dynamically, to see how it evolves over time
# we use the 'ndtv'-package
# this package is part of the Statnet family; it therefore accepts objects from the 'network'-package (which uses a different code than igraph)
library(ndtv)
library(network)

# to create and animate the kudo network dynamically, we need to first load in our kudo network "snap shots" over time
knet <- list()
t = 12

for ( i in 1:t ) {
        net <- as.network(club$kudo[,,i])
        knet[[i]] <- net 
}

# check the structure of this object
length(knet) # 12 networks
is.network(knet[[1]]) # it is really a network
as.sociomatrix(knet[[1]]) # check the sociomatrix
# seems allright!

# now let's generate a dynamic longitudinal network object, using the networkDynamic function
knetDyn <- networkDynamic(network.list=knet)

# note that, when converting panel data in this form, as.networkDynamic assumes that the panels should be assigned times of unit intervals starting at t=0,
# so the first panel is given the spell [0,1], the second [1,2], etc.
# this is important because if you use "at" query syntax the time does not correspond to the panel index:
plot(knetDyn) # this plots all edges that were ever present
plot (network.extract(knetDyn, at=1)) # this plots the edges at time 2 (and not 1!)

# let's generate a quick animation of the dynamic kudo network
render.d3movie(knetDyn)
?render.d3movie
# to embed this animation (Markdown), use the parameter output.mode='inline'

##########################################
##########################################

# Now visualize the SOAM ministeps
# cf. Adams and Schaefer 2018
# so: whereas we have snapshots of networks at different timeponts, the SAOM assumes this dynamic to be unfolding in continuous time between waves.
# with the interval between these snapshots divided into a sequence of microsteps (containing at most one change).

# we pick club 1 (N=30)
club <- clubdata[[1]]

# 1. First, we run the SAOM that will be used
library(RSiena)

# specify variable roles for RSiena object
kudonet <- sienaDependent(club$kudo)
freq_run <- sienaDependent(club$freq_run, type= "behavior")
time_run <- sienaDependent(club$time_run, type = "behavior")
freq_other <- varCovar(club$freq_other[,,])
time_other <- varCovar(club$time_other[,,])
gender <- NA 
gender <- ifelse(club$male == 1, 1, gender)
gender <- ifelse(club$female == 1, 2, gender)
gender <- ifelse(club$other == 1, 2, gender)
gender <- coCovar(gender)
winter <- varCovar(club$winter)

# create rsiena data object
mydata <- sienaDataCreate(kudonet, freq_run, time_run, freq_other, time_other, gender, winter)

# create the effects object and add effects
myeff <- getEffects(mydata)
{
        myeff1 <- includeEffects(myeff, gwespFF, name = "kudonet") 
        myeff1 <- includeEffects(myeff1, outActSqrt, inPopSqrt, name = "kudonet") 
        myeff2 <- includeEffects(myeff1, egoX, altX, simX, name = "kudonet", interaction1 = "freq_run")
        myeff2 <- includeEffects(myeff2, effFrom, name = "time_run", interaction1 = "freq_run")
        myeff2 <- includeEffects(myeff2, effFrom, name = "freq_run", interaction1 = "time_run")
        myeff2 <- includeEffects(myeff2, effFrom, name = "time_run", interaction1 = "time_other")
        myeff2 <- includeEffects(myeff2, effFrom, name = "freq_run", interaction1 = "freq_other")
        myeff2 <- includeEffects(myeff2, effFrom, name = "freq_run", interaction1 = "winter")
        myeff2 <- includeEffects(myeff2, effFrom, name = "time_run", interaction1 = "winter")
        myeff2 <- includeEffects(myeff2, egoX, altX, sameX, name="kudonet", interaction1 = "gender" )
        myeff2 <- includeEffects(myeff2, effFrom, name = "freq_run", interaction1 = "gender")
        myeff2 <- includeEffects(myeff2, effFrom, name = "time_run", interaction1 = "gender")
        myeff3 <- includeEffects(myeff2, indeg, name = "freq_run", interaction1 = "kudonet") 
        myeff3 <- includeEffects(myeff3, indeg, name = "time_run", interaction1 = "kudonet")
        myeff4 <- includeEffects(myeff3, avAttHigher, name = "freq_run", interaction1 = "kudonet") 
        myeff4 <- includeEffects(myeff4, avAttHigher, name = "time_run", interaction1 = "kudonet")
        myeff4 <- includeEffects(myeff4, outdeg, name = "freq_run", interaction1 = "kudonet") 
        myeff4 <- includeEffects(myeff4, outdeg, name = "time_run", interaction1 = "kudonet")
}
print(myeff4) # check the effect object

# define the algorithm
myalgorithm <- sienaAlgorithmCreate(projname = "test")

# and estimate the SAOM
# we turn on returnChains and returnDataFrame, to get an output/dataset of all the simulated ministeps
ansM1 <- siena07(myalgorithm, data = mydata, effects = myeff4, returnChains=TRUE, returnDataFrame=TRUE)
# see: https://www.stats.ox.ac.uk/~snijders/siena/WorkOnChains.r
# we will accept the model with convergence ratio <.30 for now (later on we want to train the model to converge at <.25)

# we save the chain of simulated ministeps
theChain <- ansM1$chain
length(theChain) # list of 1000 simulated runs

# we extract the 50th simulation run for this example
chainNum <- 50
datChain <- t(matrix(unlist(theChain[[chainNum]][[1]][[1]]), 
                     nc=length(theChain[[chainNum]][[1]][[1]]))) 
# From the manual - "The chain has the structure chain[[run]][[depvar]][[period]][[ministep]]."
# So this means, in the above, we are pulling the chains for the first run, from the modeled
# changes between the 1st and 2nd waves in this particular call.
dim(datChain)  # change opportunities (microsteps); 11 values stored for each
df <- as.data.frame(datChain, stringsAsFactors = F)

### Interpretation of the columns on the outputted df - 

# 1 - network or behavior function 
# 2 - same as 1, denoted as 0/1 
# 3 - same as 1/2, denoted using varname 
# 4 - ego ID (starting @0) 
# 5 - if network function - alter ID (starting @0) for changes 
# ego ID for no change 
# 0 if behavior function 
# 6 - 0 if network 
# -1, 0, 1 for change in behavior level behavior 
# Our aims don't make use of columns 7-10


#######################################################
#######################################################

# now that we have created a dataframe of output micro timesteps 
# we initialize a network corresponding to T=0
init <- network( club$kudo[,,1],
                 matrix.type = "adjacency",
                 directed = TRUE)

# and we add a vertex attribute, that is running frequency and time
set.vertex.attribute(init, "freq_run", club$freq_run[,,1])
set.vertex.attribute(init, "time_run", club$time_run[,,1])

# and a PID 
set.vertex.attribute(init, "pid", c(1:length(club)))

# Some object-class conversions to the passed df, because their types got erased.
df$V2 <- as.numeric(df$V2)
df$V4 <- as.numeric(df$V4)
df$V5 <- as.numeric(df$V5)
df$V6 <- as.numeric(df$V6)

# Because the nodes are indexed from 0, changing their IDs (now 1-50)
df$V4 <- df$V4 + 1
df$V5[which(df$V2==0)] <- df$V5[which(df$V2==0)] + 1 
# only change for network steps (retain 0s if behavior)

### Generating a matrix of network edge toggles
df$id <- as.numeric(row.names(df)) # to get micro time step
toggles <- df[,c(12,4,5)][which (df$V2==0 & df$V11==F),] 
# 12 is the time step, (4,5) is head, tail,  
# V2=0 is for network evaluations, V11=F is for CHANGES
colnames(toggles) <- c("time", "tail", "head")

### Generating a matrix of behavior changes
beh <- df[,c(12,4,6)][which(df$V2==1 & df$V11==F),]
colnames(beh) <- c("time", "node", "change")

#######################################################
#######################################################

# Initializing the dynamic network object and some visual options to make that easily spotted.
library(networkDynamic)

dyn <-networkDynamic(base.net=init, edge.toggles=toggles, create.TEAs=T)

# Highlighting the node with the opportunity to make a choice
# Initializing some plotting options for that highlighting 
# (node color currently, also tried thickness and shading of vertex border)
activate.vertex.attribute(dyn, "halo", "white", onset=0, terminus=Inf)
#activate.vertex.attribute(dyn, "growth", 1, onset=0, terminus=Inf)
#activate.vertex.attribute(dyn, "thick", 1, onset=0, terminus=Inf)
for (i in 1:nrow(df)){
        t <- df$id[i] # I'm just trying to keep the replace statement from getting unweildy.
        n <- df$V4[i]
        activate.vertex.attribute(dyn, "halo", "red", onset=t-1, terminus=t, v=n)
        #  activate.vertex.attribute(dyn, "growth", 2, onset=t-1, terminus=t, v=n)
        #  activate.vertex.attribute(dyn, "thick", 5, onset=t-1, terminus=t, v=n)
}

# Adding the behavior changes 
activate.vertex.attribute(dyn, "freq_run", club$freq_run[,,1])
# for some reason the values from "init" aren't sticking w/ the way I assign updates, so re-attaching.
activate.vertex.attribute(dyn, "size", club$freq_run[,,1]/2) #making size relative to running frequency value

# plus 1 for size? otherwise inactive nodes are not visible in the dynamic plot. 


for (i in 1:nrow(beh)){
        t <- beh$time[i] # I'm just trying to keep the replace statement from getting unweildy.
        n <- beh$node[i]
        d <- beh$change[i]
        curr <- get.vertex.attribute.active(dyn,'freq_run',at=(t-1))[n]
        
        # This is the reassignment: current value (curr) + change (d) taking effect at onset (t) for node (n)
        activate.vertex.attribute(dyn, "freq_run", curr+d, onset=t, terminus=Inf, v=n)
        # And re-setting size accordingly
        activate.vertex.attribute(dyn, "size", get.vertex.attribute.active(dyn, "freq_run", at=t)[n]/2, onset=t, terminus=Inf, v=n)
}

#######################################################
#######################################################

# Plotting the network movie
library(ndtv)
palette(c("white", "gray75", "black"))

# This generates the layouts conforming to each interval (be sure to change the end back)
slice.par <- list(interval=1, aggregate.dur=1, start=0, end=nrow(df), rule="all")
compute.animation(dyn, slice.par=slice.par, displayisolates=T)
render.par=list(tween.frames=5, show.time=T)

# This generates the video from that dynamic layout.
# as html
render.d3movie(dyn,filename='SABM.html',
               launchBrowser=FALSE,
               d3.options=list(animationDuration=1000),
               edge.col="gray75",
               vertex.col="halo", vertex.sides=50, vertex.cex="size", 
               displaylabels=T, label.col="gray75", label.cex=.5,
               main="",
               ani.options=list(interval=.1),  
               script.type='embedded', 
               vertex.tooltip = function(slice){paste('ID:',slice%v%'vertex.names','<br>',
                                                      'Choice:', slice%v%'fn','<br>',
                                                      'Decision:', slice%v%'choice' )})

# as mp4
# saveVideo(render.animation(s50d,
#                            edge.col="black", arrowhead.cex=.5,
#                            vertex.border = "gray50", vertex.col="smoke", vertex.sides=50,
#                            displaylabels=T, label.col="gray75", label.cex=.5,
#                            displayisolates=T, ani.options=list(interval=.1),
#                            main="SABM Micro Time Steps", render.cache='none'),
#           video.name="SABM_viz.mp4")


####################
###################
# let's include dynamic actor attributes
# which are called "TEAs" (temporally extended attributes) in the networkDynamics-package
# see paragraph 10.1 (p.39) of the vignette
#?activate.vertex.attribute
#for ( i in 1:length(knet)) {
#        activate.vertex.attribute( knetDyn, "freq", club$freq_run[,,i], onset = i-1, terminus = i )
#}
#list.vertex.attributes(knetDyn) # what are they actually named?
#get.vertex.attribute.active(knetDyn,"freq",at=0)
#club$freq_run[,,1] #should equal...


##########################################
##########################################

# we dynamically visualized the co-evolution of kudo-ties and running behaviors, 
# starting with the initial network corresponding to t=0, and using a chain of ministeps from our SAOM. 

# this method is useful to depict how our influence mechanisms operate:
# we use a simulated network as an initial network to base our network-behavior co-evolution on
# and we assume the objective function for behavior change to comprise only the respective influence effects,
# ie, the indegree effect on behavior and the upward similirity effect.

## data simulation: we simulate 2 networks
generate.network = function(n, p) { # with n members where probability that two members are connected is p;  
  
  # we generate matrix values, sampling 0 or 1 with given probabilities
  matvals = sample(c(0, 1), n * (n - 1)/2, replace = TRUE, prob = c(1 - p, p))
  
  #fFrom the values above, generate a symmetric matrix
  networkmat = matrix(rep(0, n * n), ncol = n)
  mv = 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        networkmat[i, j] = matvals[mv]
        networkmat[j, i] = matvals[mv]
        mv = mv + 1
      }
    }
  }
  return(networkmat)
}
(net1 <- generate.network(6, .7))
(net2 <- generate.network(6, .7)) # we assume ties to be reciprocated

## we simulate behavior attributes for vertices ( no inactives ): at two timepoints, with at most a change of 1
beh1 <- sample(c(1:7), 6, replace = T)

beh2 <- rep (NA, length(beh1) )
for ( i in 1 : length(beh2) ) { 
  ms <- rbinom( length(beh2), 1, .75) * rep (c (-1, 1), length(beh2) ) # ministep (-1, 0 or +1)
  beh2[i] <- beh1[i] + ms[i]
  }

library(RSiena)
knet <- sienaDependent(array ( c ( net1, net2 ), dim = c ( 6, 6, 2) ) )
beh <- sienaDependent(array ( c (beh1, beh2 ), dim = c ( 6, 1, 2) ) )
mydata <- sienaDataCreate( knet, beh )






     