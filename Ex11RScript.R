# load package
library(ggplot2)

# set initial values
N0=1
M0=1
K=1000000
timesteps=250
# allow r to be variable

# create vectors to store variables
N=numeric(length=timesteps)
N[1]=N0
M=numeric(length=timesteps)
M[1]=M0


# simulate
for(t in 1:(timesteps-1)){
  if (t<=49){
    rN=0.1
    rM=0.1
    N[t+1] <- N[t]+0.1*N[t]*(1-(N[t]+M[t])/K)
    M[t+1] <- M[t]+0.1*M[t]*(1-(N[t]+M[t])/K)
    } else if (t>49){
      N[t+1] <- N[t]+(-0.1)*N[t]*(1-(N[t]+M[t])/K)
      M[t+1] <- M[t]+0.05*M[t]*(1-(N[t]+M[t])/K)
    }
}


# create data frame
sim1<-data.frame(name="sim1", time=1:length(N), N=N)
sim2<-data.frame(name="sim2", time=1:length(M), M=M)


# plot simulation
ggplot()+
  geom_line(data=sim1, aes(x=time,y=N),col='blue')+
  geom_line(data=sim2, aes(x=time,y=M),col='red')+
  scale_y_continuous(trans="log10")+
  ylab("cell growth")+
  theme_classic()

