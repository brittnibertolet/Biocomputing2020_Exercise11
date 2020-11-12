#Exercise 11 - modeling growth of populations

#Parameters
K <- 1000000 #carrying capacity
r<- 0.1 #normal growth
rnormND<- -0.1 #normal cells decline
rD<- 0.05 #drug growth

time <- 1:1000 #time values for graphing

#Initialize dataframe for outputs

resevoir <- data.frame(data=NA, nrow=1000, ncol = 3)
resevoir[,1] <- time

