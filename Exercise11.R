# Exercise 11

#Set up initial parameters
#When drug is absent:
rN<- 0.1 #Rate of normal cell growth
rM<- 0.1 #Rate of mutant cell growth

#When drug is present
rN2<- -0.1 #Rate of normal cell growth
rM2<- 0.05 #Rate of mutant cell growth

#Carrying Capacity
K<- 1000000

#Set timesteps
timesteps<-500

#Set an output to population
N<-numeric(length=length(timesteps))
M<-numeric(length=length(timesteps))

#Set N0 and M0
N0<-99 #Initial normal cell population
M0<-1 #Initial mutant cell population
N[1]<-N0
M[1]<-M0

#Use a for loop to simulate in time
for(i in 1:timesteps){
  if(i<200){ 
    M[i+1] = M[i]+rM*M[i]*(1-(N[i]+M[i])/K)
    N[i+1] = N[i]+rN*N[i]*(1-(N[i]+M[i])/K)
  }else{
    M[i+1] = M[i]+rM2*M[i]*(1-(N[i]+M[i])/K)
    N[i+1] = N[i]+rN2*N[i]*(1-(N[i]+M[i])/K)
  }
}

#Create dataframe for data
TotalCells<-data.frame(time=1:(timesteps+1),N,M)

#Plot data using ggplot
library(ggplot2)
ggplot(TotalCells, aes(x=time))+
  geom_line(aes(y=N), color="red") + #plot the normal strain data
  geom_line(aes(y=M), color="blue") + #plot the mutant strain data
  xlab("Time") + #Label thr x-axis
  ylab("Number of Cells") + #Label the y-axis
  geom_vline(xintercept = c(200), linetype = "dotted") + #Set up a drug treatment event to occur sometime at t around 200
  theme_classic()






















