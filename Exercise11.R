#cancer dynamic modeling
#we are looking at 2 subpopulations of cancer cells including non-mutant and mutant populations
#modeling the subpopulations after drug treatment
#N stands for non-mutant cells
#M stands for mutant cells 
#there are 100 total cancer cells
N0=99
M0=1
r=0.1 #normal growth rate
rm=.5 #growth rate of mutant cells under drug-treatment
rn=-.1 #growth rate of non-mutant cells under drug-treatment condition
K=1000000 #carrying capacity is 1 million cells
timesteps=500 #how long we want to run simulation for
#create output
Ns=numeric(length=timesteps)
Ns[1]=N0 #non-mutant cell output
Ms=numeric(length=timesteps)
Ms[1]=M0 #mutant cell output
#finding equilibrium time
for(t in 1:length(timesteps-1)){
  Ns[t+1] = Ns[t] + r * Ns[t]* (1- (Ns[t]/K))
  Ms[t+1] = Ms[t] + r * Ms[t]* (1- (Ms[t]/K))
}
equilibrium=data.frame(time=(1:timesteps), Ns)
View(equilibrium)
library(ggplot2)
ggplot(data=equilibrium, aes())+
  geom_point(aes(x=time, y=Ns))
#equilibrium is where graph levels out
#looking at graph, equilibrium is about at t=190

#for loop to simulate mutant condition and normal condition
for (t in 1:(timesteps-1)) {
    if(t<=190){
      Ns[t+1] <-Ns[t]+r*Ns[t]*(1-(Ns[t] +Ms[t])/K)
      Ms[t+1] <-Ms[t]+r*Ms[t]*(1-(Ns[t] +Ms[t])/K)
    }else{
      Ns[t+1] <-Ns[t]+rn*Ns[t]*(1-(Ns[t] +Ms[t])/K)
      Ms[t+1] <-Ms[t]+rm*Ms[t]*(1-(Ns[t] +Ms[t])/K)
    }
  }
#creating data frame to put non-mutant(N) and mutant(M) conditions in 
dat=data.frame(time=1:length(Ns), N=Ns, M=Ms) 
library(ggplot2)
#plotting N (blue) and M (purple)
ggplot(dat)+ 
  geom_line(aes(x=time, y=N), col="blue")+
  geom_line(aes(x=time, y=M),col="purple")+
  ylab("number of cells")+
  theme_classic()
