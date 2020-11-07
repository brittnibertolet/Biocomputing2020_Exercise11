# Biocomputing Exercise 11

# Set working directory and load packages
setwd("/Users/aleksandrove/Desktop/r-novice-inflammation/Biocomputing2020_Exercise11")
library(ggplot2)

# Challenge description:
# Create a script that simulates growth of two sub-populations in a tumor to 
# equilibrium followed by drug treatment. 

# Function to simulates growth of wild type cells, with drug and no drug:
simulation1=function(name, r=0.1, K=1000000, timesteps=300, N0=100, M0=100){
  # create vector to store N's and M's, and set initial N and M
  N=numeric(length=timesteps)
  N[1]=N0
  M=numeric(length=timesteps)
  M[1]=M0
  # simulate dynamics 
  for(i in 2:timesteps){
    N[i] <- N[i-1]+r*N[i-1]*(1-(N[i-1]+M[i-1])/K) 
  }
  # put into dataframe
  simWT<-data.frame(name=name, time=1:timesteps, N=N)
  # return dataframe
  return(simWT)
}
# Run two simulations with varying r
sim1=simulation1(name = "WT - No drug")
sim2=simulation1(name="WT - With drug", r = -0.1)

# Mutant with drug and no drug:
simulation2=function(name, r=0.1, K=1000000, timesteps=300, N0=100, M0=100){
  # create vector to store N's and M's, and set initial N and M
  N=numeric(length=timesteps)
  N[1]=N0
  M=numeric(length=timesteps)
  M[1]=M0
  # simulate dynamics 
  for(i in 2:timesteps){
    M[i] <- M[i-1]+r*M[i-1]*(1-(N[i-1]+M[i-1])/K) 
  }
  # put into dataframe
  simM <-data.frame(name=name, time=1:timesteps, N=M)
  # return dataframe
  return(simM)
}
# Run two simulations with varying r
sim3=simulation2(name = "Mut - No drug")
sim4=simulation2(name="Mut - With drug", r = 0.05)

# Combine all 4 simulations into one dataframe
Combined = rbind(sim1,sim2,sim3,sim4)

# plot
ggplot(data=Combined,aes(x=time,y=N))+geom_line(aes(color=name))+theme_classic()


