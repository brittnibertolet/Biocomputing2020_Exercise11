## Kayla Wood Tutorial Excercise 11 (11/12/2020): Dynamic Modeling 

### Problem 1: 
  # set parameters w/ the drug
N0=99
M0=1
r=0.1 #growth rate (normal)
rm=0.05 # mutant growth rate when cancer drug present 
rn=-0.1 # growth rate with cancer treatment 
timesteps=500 #duration of the stimulation
K=1000000 #carrying capacity
  # create a vector to store N and M
Ns=numeric(length = timesteps)
Ns[1]=N0

Ms=numeric(length = timesteps)
Ms[1]=M0

#find equilibrium  
for (t in 1:(timesteps-1)) {
  Ns[t+1] <-Ns[t]+r*Ns[t]*(1-(Ns[t] +Ms[t])/K)
  Ms[t+1] <-Ms[t]+r*Ms[t]*(1-(Ns[t] +Ms[t])/K)
}
equil= data.frame(time= (1:timesteps), Ns, Ms)
k=ggplot(data=equil) +
  geom_line(aes(x=time, y=Ns)) +
  theme_classic()
k
# based on the graph the equilibrium point is 186
  # simulate starting at equilibrium t=186 
for (t in 1:(timesteps-1)) {
  if(t<=186){
  Ns[t+1] <-Ns[t]+r*Ns[t]*(1-(Ns[t] +Ms[t])/K)
  Ms[t+1] <-Ms[t]+r*Ms[t]*(1-(Ns[t] +Ms[t])/K)
}else{
  Ns[t+1] <-Ns[t]+rn*Ns[t]*(1-(Ns[t] +Ms[t])/K)
  Ms[t+1] <-Ms[t]+rm*Ms[t]*(1-(Ns[t] +Ms[t])/K)  
  }
}

# load the ggplot package
library(ggplot2)

# create data frame with data from the stimulation 
simulation= data.frame(time=1: length(Ns), N=Ns, M=Ms)
# plot the data 
ggplot(data= simulation)+
  geom_line(aes(x=time, y=N), col = "purple")+
  geom_line(aes(x=time, y=M), col= "pink") +
  xlab("Time") +
  ylab("Growth")+
  theme_classic()