rm (list = ls( ))
load("clubdata_rsiena.RData")

#functions
{
  # density: observed relations divided by possible relations
  fdensity <- function(x) {
    # x is your nomination network make sure diagonal cells are NA
    diag(x) <- NA
    # take care of RSiena structural zeros, set as missing.
    x[x == 10] <- NA
    sum(x == 1, na.rm = T)/(sum(x == 1 | x == 0, na.rm = T))
  }
  
  # calculate intragroup density
  fdensityintra <- function(x, A) {
    # A is matrix indicating whether nodes in dyad have same node attributes
    diag(x) <- NA
    x[x == 10] <- NA
    diag(A) <- NA
    sum(x == 1 & A == 1, na.rm = T)/(sum((x == 1 | x == 0) & A == 1, na.rm = T))
  }
  
  # calculate intragroup density
  fdensityinter <- function(x, A) {
    # A is matrix indicating whether nodes in dyad have same node attributes
    diag(x) <- NA
    x[x == 10] <- NA
    diag(A) <- NA
    sum(x == 1 & A != 1, na.rm = T)/(sum((x == 1 | x == 0) & A != 1, na.rm = T))
  }
  
  # construct dyad characteristic whether nodes are similar/homogenous
  fhomomat <- function(x) {
    # x is a vector of node-covariate
    xmat <- matrix(x, nrow = length(x), ncol = length(x))
    xmatt <- t(xmat)
    xhomo <- xmat == xmatt
    return(xhomo)
  }
  
  # a function to calculate all valid dyads.
  fndyads <- function(x) {
    diag(x) <- NA
    x[x == 10] <- NA
    (sum((x == 1 | x == 0), na.rm = T))
  }
  
  # a function to calculate all valid intragroupdyads.
  fndyads2 <- function(x, A) {
    diag(x) <- NA
    x[x == 10] <- NA
    diag(A) <- NA
    (sum((x == 1 | x == 0) & A == 1, na.rm = T))
  }
  
  
  fscolnet <- function(network, ccovar) {
    # Calculate coleman on network level:
    # https://reader.elsevier.com/reader/sd/pii/S0378873314000239?token=A42F99FF6E2B750436DD2CB0DB7B1F41BDEC16052A45683C02644DAF88215A3379636B2AA197B65941D6373E9E2EE413
    
    fhomomat <- function(x) {
      xmat <- matrix(x, nrow = length(x), ncol = length(x))
      xmatt <- t(xmat)
      xhomo <- xmat == xmatt
      return(xhomo)
    }
    
    fsumintra <- function(x, A) {
      # A is matrix indicating whether nodes constituting dyad have same characteristics
      diag(x) <- NA
      x[x == 10] <- NA
      diag(A) <- NA
      sum(x == 1 & A == 1, na.rm = T)
    }
    
    # expecation w*=sum_g sum_i (ni((ng-1)/(N-1)))
    network[network == 10] <- NA
    ni <- rowSums(network, na.rm = T)
    ng <- NA
    for (i in 1:length(ccovar)) {
      ng[i] <- table(ccovar)[rownames(table(ccovar)) == ccovar[i]]
    }
    N <- length(ccovar)
    wexp <- sum(ni * ((ng - 1)/(N - 1)), na.rm = T)
    
    # wgg1 how many intragroup ties
    w <- fsumintra(network, fhomomat(ccovar))
    
    Scol_net <- ifelse(w >= wexp, (w - wexp)/(sum(ni, na.rm = T) - wexp), (w - wexp)/wexp)
    return(Scol_net)
  }
}

df <- clubdata_rsiena[[1]] # grab club 

# retrieve friendship and kudo network data
fnet <- df$dycCovars$friendship
knet1 <- df$depvars$kudonet[,,1]
knet2 <- df$depvars$kudonet[,,6]
knet3 <- df$depvars$kudonet[,,12]

# retrieve running frequency
freq1 <- df$depvars$freq_run[,,1]
freq2 <- df$depvars$freq_run[,,6]
freq3 <- df$depvars$freq_run[,,12]

# make dyad characterstic indicating whether nodes' run frequency is similar:
# T1:
xmat <- matrix(freq1, nrow = length(freq1), ncol = length(freq1))
xmatt <- t(xmat)
ydif <- abs(xmat - xmatt)
ydif0_t1 <- ydif == 0

# T6:
xmat <- matrix(freq2, nrow = length(freq2), ncol = length(freq2))
xmatt <- t(xmat)
ydif <- abs(xmat - xmatt)
ydif0_t6 <- ydif == 0

# T12:
xmat <- matrix(freq3, nrow = length(freq3), ncol = length(freq3))
xmatt <- t(xmat)
ydif <- abs(xmat - xmatt)
ydif0_t12 <- ydif == 0

# make object to store results
colmat <- matrix(NA, nrow = 1, ncol = 4 )

# coleman homophily index
colmat[1, 1] <- fscolnet(fnet, ydif0_t1)
colmat


desmat[1, 2] <- fscolnet(knet1, ydif0_t1)
desmat[1, 3] <- fscolnet(knet2, ydif0_t6)
desmat[1, 4] <- fscolnet(knet3, ydif0_t12)

# intragroup density
desmat[2, 1] <- fdensityintra(fnet, ydif0_t1)
desmat[2, 2] <- fdensityintra(knet1, ydif0_t1)
desmat[2, 3] <- fdensityintra(knet2, ydif0_t6)
desmat[2, 4] <- fdensityintra(knet3, ydif0_t12)

colnames(desmat) <- c ( "friendship" , "Kudo T1", "Kudo T6", "Kudo T12")
rownames(desmat) <- c ( "Coleman's homophily index")    

print(desmat)   


#########



df <- clubdata_rsiena[[1]] # grab club 

# retrieve friendship and kudo network data
fnet <- df$dycCovars$friendship
knet1 <- df$depvars$kudonet[,,1]
knet2 <- df$depvars$kudonet[,,6]
knet3 <- df$depvars$kudonet[,,12]

# retrieve running frequency
freq1 <- df$depvars$freq_run[,,1]
freq2 <- df$depvars$freq_run[,,6]
freq3 <- df$depvars$freq_run[,,12]

# make dyad characterstic indicating whether nodes' run frequency is similar
xmat <- matrix(freq1, nrow = length(freq1), ncol = length(freq1))
xmatt <- t(xmat)
ydif <- abs(xmat - xmatt)
ydif0 <- ydif == 0
ydif1 <- ydif < 3

# make object to store results
desmat <- matrix(NA, nrow = 3, ncol = 4 )

# use functions, to calculate:
# total density
desmat[1, 1] <- fdensity(fnet)
desmat[1, 2] <- fdensity(knet1)
desmat[1, 3] <- fdensity(knet2)
desmat[1, 4] <- fdensity(knet3)

# intragroup density
desmat[2, 1] <- fdensityintra(fnet, ydif0)
desmat[2, 2] <- fdensityintra(knet1, ydif0)
desmat[2, 3] <- fdensityintra(knet2, ydif0)
desmat[2, 4] <- fdensityintra(knet3, ydif0)

# intergroup density
desmat[3, 1] <- fdensityinter(fnet, ydif0)
desmat[3, 2] <- fdensityinter(knet1, ydif0)
desmat[3, 3] <- fdensityinter(knet2, ydif0)
desmat[3, 4] <- fdensityinter(knet3, ydif0)

colnames(desmat) <- c ( "friendship" , "Kudo T1", "Kudo T6", "Kudo T12")
rownames(desmat) <- c ( "total", "similar run freq.", "dissimilar run freq.")                      

print(desmat)                      

                    
################












df <- clubdata_rsiena[[1]] # grab club 
fnet <- df$dycCovars$friendship # take friendship network
knet1.na <- ifelse((clubdata_rsiena[[1]]$depvars$kudonet[,,1]) == 10, NA, (clubdata_rsiena[[1]]$depvars$kudonet[,,1])) # take kudo network (here I already dealt 10s, but this is not necessary given the functions used later on).


# retrieve node-attribute gender from rsiena object
male <- df$cCovars$male
female <- df$cCovars$female
other <- df$cCovars$other

# de-mean-center
male <- male + attributes(male)$mean
female <- female + attributes(female)$mean
other <- other + attributes(other)$mean

# coleman homophily index
colmat <- matrix(NA, nrow = 2, ncol = 1)
colmat[1, 1] <- fscolnet(knet1.na, male)
colmat[2, 1] <- fscolnet(knet1.na, female)

colnames(colmat) <- "Coleman Homophily Index"
rownames(colmat) <- c("male", "female")
print(colmat)















knet1.na <- ifelse((clubdata_rsiena[[1]]$depvars$kudonet[,,1]) == 10, NA, (clubdata_rsiena[[1]]$depvars$kudonet[,,1])) # take kudo network (here I already dealt 10s, but this is not necessary given the functions used later on).

# retrieve node-attribute gender from rsiena object
male <- df$cCovars$male
female <- df$cCovars$female
other <- df$cCovars$other

# de-mean-center
male <- male + attributes(male)$mean
female <- female + attributes(female)$mean
other <- other + attributes(other)$mean

# construct dyad similarity matrix
male_m <- fhomomat(male)
female_m <- fhomomat(female)

# make object to store results
desmat <- matrix(NA, nrow=5, ncol=1)

# use function
desmat[1, 1] <- fdensity(knet1.na)
desmat[2, 1] <- fdensityintra(knet1.na, male_m)
desmat[3, 1] <- fdensityinter(knet1.na, male_m)
desmat[4, 1] <- fdensityintra(knet1.na, female_m)
desmat[5, 1] <- fdensityinter(knet1.na, female_m)
colnames(desmat) <- "Kudos"
rownames(desmat) <- c("total", "men: same sex", "men: different sex", "women: same sex", "women: different sex")
print(desmat)







df <- clubdata_rsiena[[1]] # grab club 
knet <- df$depvars$kudonet
knet1 <- knet[,,1] # take wave 1 only
knet1.na <- ifelse(knet1 == 10, NA, knet1)

# Let's explore reciprocity in Kudos


### Club 1
```{r }
# make igraph object for the club
G1 <- igraph::graph_from_adjacency_matrix( 
  ifelse((clubdata_rsiena[[1]]$depvars$kudonet[,,1]) == 10, NA, (clubdata_rsiena[[1]]$depvars$kudonet[,,1])),
  mode = "directed", weighted = NULL, diag = TRUE, add.colnames = NA, add.rownames = NA)

# classify dyads
dyadcount <- dyad.census(G1)

# add the total number of dyads to the graph
dyadcount$total <- (vcount(G1)*(vcount(G1)-1))/2
dyadcount

# compare values with a random graph of the same size with the same density
dens <- graph.density(G1)
size <- vcount(G1)
trial <- 1000
recip <- rep(NA, trial)

for ( i in 1:trial ){
  random_graph <- erdos.renyi.game(n = size, p.or.m = dens, directed = TRUE)
  recip[i] <- dyad.census(random_graph)$mut
}

{hist(recip, main="number of mutual dyads in random graph", xlab="", )
abline(v=dyadcount$mut, col="red", lwd=3)}















knet1.na 
knet1
knet1[knet1==10] <- NA # make structural zero's (10) NA
# remember that we made structural zeros (10) between users that have a higher distance of 2 in their connections.

# make a 'graph object'
library(igraph)
G1 <- igraph::graph_from_adjacency_matrix(knet1, mode = "directed", weighted = NULL, diag = TRUE, add.colnames = NA, add.rownames = NA)

# find in- and outdegree for each node
hist(igraph::degree(G1, mode="out"), xlab="outdegree", main="histogram of Kudo outdegree")
hist(igraph::degree(G1, mode="in"), xlab="indegree", main="histogram of Kudo indegree")

# same pareto pattern: some give/receive most of the Kudos, while most give/receive few.