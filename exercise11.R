#model for growth: N(t+1)= N(t)+r(N)*N(t)*(1-(N(t)+M(t))/K)
#second model:M(t+1)= M(t)+r(M)*M(t)*(1-(N(t)+M(t))/K)
#when cancer drug is not present: r(N)=r(M)=0.1
#when drug is present, non-mutant cells: r(N)= -0.1
#when drug is present, mutant cells: r(M)=0.05
#assume K=1,000,000
#mutation occurred when there were 100 cells
#generate script that stimulates growth of two subpopulations in the tumor to equilibrium followed
#   drug treatment. Plot results with line graph

#set inital values
K=1000000
r0=0.1
rN= -0.1
rM= 0.05
N0=99
M0=1
timesteps=600

# create vector to store N's and set initial N
N=numeric(length=timesteps)
N[1]=N0
M=numeric(length=timesteps)
M[1]=M0

# simulate
for(i in 1:timesteps){
  if(i<200){
    # drug introduced at i=200, before drug is introduced both pop grow at r=0.1
    N[i+1] = N[i]+r0*N[i]*(1-(N[i]+M[i])/K)
    M[i+1] = M[i]+r0*M[i]*(1-(N[i]+M[i])/K)
  }else{ #after drug introduced, non-mutant declines at r=-0.1, mutant grows at r=0.05
    M[i+1] = M[i]+rM*M[i]*(1-(N[i]+M[i])/K)
    N[i+1] = N[i]+rN*N[i]*(1-(N[i]+M[i])/K)
  }
}

# make dataframe
dat<-data.frame(time=1:(timesteps+1),N=N,M=M)

#call ggplot
library(ggplot2)
# plot simulation
ggplot(dat, aes(x=time))+
  geom_line(aes(y=N), color="steelblue")+
  geom_line(aes(y=M), color="darkred")+
  theme_classic()
