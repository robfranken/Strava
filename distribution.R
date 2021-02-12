####################################################

### Strava paper, PART 1 ### author: ROB FRANKEN ###
### last edited: February 8, 2021 ###

####################################################

# in the following we describe the dependent variable 
# running frequency and time
# to guide our solution for the issue of discretizing
# our continuous dependent behavioral variables
# guided by RSiena manual (p. 27) and Niezink 
# dissertation research (Chapter 6).

####################################################
############### Reading in the data ################
####################################################

# first, clean working environment, and set wd
# to folder containing the scraped clubdata. 
rm (list = ls( ))
setwd("C:\\Users\\u244147\\Documents\\dissertatie\\Strava data\\clubs") 
getwd()

# we read in the data of all clubs, stack the
# repeated measures, and thus assume them to be 
# independent of each other.

# we make a club string to grab the data from the
# corresponding clubs
club_str <- "19037 \n 73971 \n 147521 \n 324488 \n 473939"
club_str_list <- strsplit(club_str, "\n")
club_str <- as.numeric(unlist(club_str_list))

{ # grab clubdata
  club1 <- read.csv(paste(club_str[1], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
  club2 <- read.csv(paste(club_str[2], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
  club3 <- read.csv(paste(club_str[3], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
  club4 <- read.csv(paste(club_str[4], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
  club5 <- read.csv(paste(club_str[5], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
}

# and stack running frequency for each timepoint, hence disregarding club-embededdness
  
y <- array( 
  c(
    y1 <- c(club1$X.run_1.2019, club2$X.run_1.2019, club3$X.run_1.2019, club4$X.run_1.2019, club5$X.run_1.2019),
    y2 <- c(club1$X.run_2.2019, club2$X.run_2.2019, club3$X.run_2.2019, club4$X.run_2.2019, club5$X.run_2.2019),
    y3 <- c(club1$X.run_3.2019, club2$X.run_3.2019, club3$X.run_3.2019, club4$X.run_3.2019, club5$X.run_3.2019),
    y4 <- c(club1$X.run_4.2019, club2$X.run_4.2019, club3$X.run_4.2019, club4$X.run_4.2019, club5$X.run_4.2019),
    y5 <- c(club1$X.run_5.2019, club2$X.run_5.2019, club3$X.run_5.2019, club4$X.run_5.2019, club5$X.run_5.2019),
    y6 <- c(club1$X.run_6.2019, club2$X.run_6.2019, club3$X.run_6.2019, club4$X.run_6.2019, club5$X.run_6.2019),
    y7 <- c(club1$X.run_7.2019, club2$X.run_7.2019, club3$X.run_7.2019, club4$X.run_7.2019, club5$X.run_7.2019),
    y8 <- c(club1$X.run_8.2019, club2$X.run_8.2019, club3$X.run_8.2019, club4$X.run_8.2019, club5$X.run_8.2019),
    y9 <- c(club1$X.run_9.2019, club2$X.run_9.2019, club3$X.run_9.2019, club4$X.run_9.2019, club5$X.run_9.2019),
    y10 <- c(club1$X.run_10.2019, club2$X.run_10.2019, club3$X.run_10.2019, club4$X.run_10.2019, club5$X.run_10.2019),
    y11 <- c(club1$X.run_11.2019, club2$X.run_11.2019, club3$X.run_11.2019, club4$X.run_11.2019, club5$X.run_11.2019),
    y12 <- c(club1$X.run_12.2019, club2$X.run_12.2019, club3$X.run_12.2019, club4$X.run_12.2019, club5$X.run_12.2019)))

freq <- ceiling(y/4) #convert to times per week

# describe distribution with a plot
breaks <- seq(0, 15, by=1) 

freq.cut <- cut(freq, breaks, right=FALSE) 
freq.x <- table(freq.cut)
cumfreq0 = c(0, cumsum(freq.x)) 

plot(breaks, cumfreq0,                       #plot the data 
     main="Weekly running frequency",        #main title 
     xlab="Sessions per week",               #x−axis label 
     ylab="Number of athletes (cumulative)") #y−axis label 
axis(1, at = 1:15)                           #define x-axis
lines(breaks, cumfreq0)                      #join the points

# the elbow of the curve is at 7, which also 
# theoretically makes sense;
# apparently, a significant portion of the sample 
# runs more than once a day!
# let's check the cumulative proportions
df <- data.frame(frequency = 0:15,
                 count = c(sum(freq == 0), sum(freq == 1), sum(freq == 2), sum(freq == 3), sum(freq == 4), sum(freq == 5),  sum(freq == 6),  sum(freq == 7),  sum(freq == 8), sum(freq == 9),  sum(freq == 10),  sum(freq == 11),  sum(freq == 12),  sum(freq == 13), sum(freq == 14), sum(freq == 15)))
df$cum <- cumsum(df$count) / sum(df$count)
print(df)
hist(freq)

# we make >7 times a week a category
freq <- ifelse(freq > 7, 8, freq)
hist(freq) # looks pretty smoothly skewed

####################################################
####################################################

# Now check individual clubs
clubdata <- read.csv(paste(club_str[2], "/", "egoData_extended.csv", sep = ""), row.names = NULL, sep= ",")
y <- array( c( clubdata[, 'X.run_1.2019'], clubdata[, 'X.run_2.2019'], clubdata[, 'X.run_3.2019'], clubdata[, 'X.run_4.2019'], clubdata[, 'X.run_5.2019'], clubdata[, 'X.run_6.2019'], clubdata[, 'X.run_7.2019'], clubdata[, 'X.run_8.2019'], clubdata[, 'X.run_9.2019'], clubdata[, 'X.run_10.2019'], clubdata[, 'X.run_11.2019'], clubdata[, 'X.run_12.2019']))
y <- ceiling(y/4) #times per week
table(y)
freq <- ifelse(y > 7, 8, y)
hist(freq)
