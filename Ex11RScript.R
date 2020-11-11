# load package
library(ggplot2)

# set initial values
N0=100
M0=1
K=1000000
timesteps=250
rN=0.1
rM=0.05

# allow r to be variable for drug and no drug conditions

# create vectors to store variables
N=numeric(length=timesteps)
N[1]=N0
M=numeric(length=timesteps)
M[1]=M0

# simulate
for(t in 1:(timesteps-1)){
  N[t+1] <- N[t]+rN*N[t]*(1-(N[t]+M[t])/K)
  M[t+1] <- M[t]+rM*M[t]*(1-(N[t]+M[t])/K)
}


for(t in 1:(timesteps-1)){
  if(t==50){
    # 50 additional new individuals due to immigration
    NsEvents[t+1] <- NsEvents[t]+r*NsEvents[t]*(1-NsEvents[t]/K)+50
  }else if(t==150){
    # 90% of individuals die due to illness
    NsEvents[t+1] <- NsEvents[t]*0.1
  }else{
    NsEvents[t+1] <- NsEvents[t]+r*NsEvents[t]*(1-NsEvents[t]/K)
  }
}


# plot simulation
sim1<-data.frame(time=1:length(N))
ggplot(data=sim1)+
  geom_line(aes(x=time,y=N),col='black')+
  geom_line(aes(x=time,y=N),col='red')+
  theme_classic()
sim2<-data.frame(time=1:length(M))
ggplot(data=sim2)+
  geom_line(aes(x=time,y=M),col='black')+
  geom_line(aes(x=time,y=M),col='red')+
  theme_classic()

# combine simulations
dat <- rbind(sim1, sim2)

# plot
ggplot(data=dat,aes(x=time))+geom_line(aes(color=name))+theme_classic()
