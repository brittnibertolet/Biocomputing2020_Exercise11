## Kayla Wood Tutorial Excercise 11: Dynamic Modeling 

### Problem 1: 
  # set parameters w/ the drug
N0=99
M0=1
r=0.1 #growth rate (normal)
rm=0.05*r # mutant growth rate when cancer drug present 
rn=-0.1 # growth rate with cancer treatment 
timesteps=500 #duration of the stimulation
K=1000000 #carrying capacity
  # create a vector to store N
N=numeric(length = timesteps)
N[1]=N0

M=numeric(length = timesteps)
M[1]=M0
  # simulate 
for (t in 1:150) {
  N[t+1] <-N[t]+r*N[t]*(1-(N[t] +M[t])/k)
  M[t+1] <-M[t]+r*M[t]*(1-(N[t] +M[t])/k)
}

for (t in 150:499) {
  N[t+1] <-N[t]+rn*N[t]*(1-(N[t] +M[t])/k)
  M[t+1] <-M[t]+rm*M[t]*(1-(N[t] +M[t])/k)
}

  # plot the data 
# load the ggplot package
library(ggplot2)

simulation= data.frame(time=1: length(N), )