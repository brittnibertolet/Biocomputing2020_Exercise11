# load package
library(ggplot2)

# set initial values
N0=100
M0=1
K=1000000
timesteps=250

# allow r to be variable for drug and no drug conditions

# create vectors to store variables

# simulate
for(t in 1:(timesteps-1)){
  N[t+1] <- N[t]+rN*N[t]*(1-(N[t]+M[t])/K)
  M[t+1] <- M[t]+rM*M[t]*(1-(N[t]+M[t])/K)
}
